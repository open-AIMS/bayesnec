# =====================================================================
# Two design types the default-prior audit does not contain
#
# Provenance. Written for the review of PR #307 (issue #305) and run
# against that branch and against `dev` at bd57f6f3, R 4.6.1,
# brms 2.23.0, 2026-09-10. Priors only; nothing is fitted.
#
# notes/scripts/prior_audit.R fixes the true `bot` at 5, 2 or 0.05 and
# applies zero-inflation at a rate independent of concentration, so it
# contains neither of the designs below and did not detect the two
# defects they expose. Both were found in review of PR #307.
#
#   A. complete effect at the highest concentration, so the top-dose
#      group is entirely zero and the true bot is at or below what the
#      endpoint resolves;
#   B. a hurdle or zero-inflated fit whose survival declines with
#      concentration, so the second block sees a real dose-response
#      rather than a flat rate.
#
# Two measures are reported. The truncated prior CDF at the true value,
# as in the audit; and the prior density at the true value relative to
# the prior's own maximum, which does not penalise a correctly located
# prior whose density is one-sided. Both are needed: a beta with its
# maximum density exactly at 0.014 has almost all its mass above it, so
# the CDF there is small however well the prior is placed.
#
# Run from the repository root.
# =====================================================================

suppressMessages(pkgload::load_all(".", quiet = TRUE))
pp <- function(s) { d <- sub("\\(.*$","",s); a <- as.numeric(strsplit(gsub("^[^(]*\\(|\\)$","",s), ",[[:space:]]*")[[1]]); list(d=d,a=a) }
cdf_at <- function(s, q) { p <- pp(s); switch(p$d, gamma=pgamma(q,p$a[1],p$a[2]),
  normal=pnorm(q,p$a[1],p$a[2]), beta=pbeta(q,p$a[1],p$a[2]), NA_real_) }
get1 <- function(pr, np) { pr <- as.data.frame(pr); s <- pr$prior[pr$nlpar == np]
  if (length(s)) s[1] else NA_character_ }
# The CDF band penalises a correctly located prior whose density is one-sided:
# a beta with its maximum density at 0.014 has almost all its mass above it, so
# the truth sits at the 0.6th percentile of a prior that peaks on it exactly.
# The density at the truth relative to the maximum density is a location measure
# that does not depend on the skew; 0.15 is roughly two standard deviations for
# a normal.
mode_of <- function(s) {
  p <- pp(s)
  switch(p$d, gamma = if (p$a[1] > 1) (p$a[1] - 1) / p$a[2] else 0,
    normal = p$a[1],
    beta = if (p$a[1] > 1 && p$a[2] > 1) (p$a[1] - 1) / (p$a[1] + p$a[2] - 2)
           else if (p$a[1] <= 1) 0 else 1, NA_real_)
}
dens_ratio <- function(s, q) {
  p <- pp(s)
  d <- switch(p$d, gamma = function(z) dgamma(z, p$a[1], p$a[2]),
    normal = function(z) dnorm(z, p$a[1], p$a[2]),
    beta = function(z) dbeta(z, p$a[1], p$a[2]), return(NA_real_))
  lo <- switch(p$d, gamma = 0, beta = 0, q - 10 * p$a[2])
  hi <- switch(p$d, beta = 1, gamma = qgamma(0.99999, p$a[1], p$a[2]),
               q + 10 * p$a[2])
  g <- seq(lo, hi, length.out = 20001)
  d(q) / max(d(g))
}

designs <- list(
  linear = as.numeric(rep(seq(0, 10, length.out = 6), each = 6)),
  log2 = as.numeric(rep(c(0, 0.3125, 0.625, 1.25, 2.5, 10), each = 6)),
  wide = as.numeric(rep(c(0, 0.01, 0.08, 0.63, 2.5, 20), each = 6)))
res <- NULL
for (seed in 1:10) for (dn in names(designs)) {
  x <- designs[[dn]]
  ux <- sort(unique(x))
  set.seed(seed * 100 + which(names(designs) == dn))
  # A. complete effect: mean falls to essentially zero at the top two doses
  prof <- c(1, 1, 0.95, 0.4, 0, 0)[match(x, ux)]
  for (fn in c("poisson", "negbinomial", "Gamma")) {
    top <- if (fn == "Gamma") 10 else 40
    mu <- pmax(top * prof, 1e-6)
    y <- switch(fn,
      poisson = as.numeric(rpois(length(mu), mu)),
      negbinomial = as.numeric(rnbinom(length(mu), mu = mu, size = 5)),
      Gamma = rgamma(length(mu), 25, 25 / mu) * (prof > 0))
    fam <- switch(fn, poisson = poisson(link = "identity"),
      negbinomial = brms::negbinomial(link = "identity"),
      Gamma = Gamma(link = "identity"))
    for (pt in c("uninformative", "regularizing")) {
      pr <- try(define_prior("nec4param", fam, x, y, prior_type = pt), silent = TRUE)
      if (inherits(pr, "try-error")) next
      # the true bot is zero, which the support excludes; scored against the
      # smallest positive observation, the lowest level the endpoint resolves
      truth <- min(y[y > 0]) / 2
      res <- rbind(res, data.frame(case = "A complete effect", design = dn,
        family = fn, prior_type = pt, par = "bot", seed = seed,
        truth = truth, prior = get1(pr, "bot"),
        mode = mode_of(get1(pr, "bot")),
        p_truth = cdf_at(get1(pr, "bot"), truth),
        dr = dens_ratio(get1(pr, "bot"), truth)))
      res <- rbind(res, data.frame(case = "A complete effect", design = dn,
        family = fn, prior_type = pt, par = "top", seed = seed,
        truth = top, prior = get1(pr, "top"),
        mode = mode_of(get1(pr, "top")),
        p_truth = cdf_at(get1(pr, "top"), top),
        dr = dens_ratio(get1(pr, "top"), top)))
    }
  }
  # B. hurdle and zero-inflated with survival declining with concentration
  surv <- c(0.99, 0.99, 0.95, 0.5, 0.05, 0.014)[match(x, ux)]
  for (fn in c("hurdle_gamma", "zero_inflated_beta")) {
    if (fn == "hurdle_gamma") {
      y <- rgamma(length(x), 25, 25 / 8) * rbinom(length(x), 1, surv)
      fam <- brms::hurdle_gamma(link = "identity", link_hu = "identity")
      truths <- c(hutop = 0.99, hubot = 0.014, top = 8, bot = 8)
    } else {
      y <- rbeta(length(x), 0.6 * 20, 0.4 * 20) * rbinom(length(x), 1, surv)
      fam <- brms::zero_inflated_beta(link = "identity", link_zi = "identity")
      truths <- c(zitop = 0.99, zibot = 0.014, top = 0.6, bot = 0.6)
    }
    for (pt in c("uninformative", "regularizing")) {
      pr <- try(define_prior("nec4param", fam, x, y, prior_type = pt), silent = TRUE)
      if (inherits(pr, "try-error")) next
      for (np in names(truths)) {
        s <- get1(pr, np)
        if (is.na(s)) next
        res <- rbind(res, data.frame(case = "B declining survival", design = dn,
          family = fn, prior_type = pt, par = np, seed = seed,
          truth = truths[[np]], prior = s, mode = mode_of(s),
          p_truth = cdf_at(s, truths[[np]]),
          dr = dens_ratio(s, truths[[np]])))
      }
    }
  }
}
res$fail <- !is.finite(res$p_truth) | res$p_truth < 0.025 | res$p_truth > 0.975
cat("== cells outside the central 95% ==\n")
print(with(res, tapply(fail, list(paste(case, par), prior_type), sum)))
cat("\ntotals of", nrow(res) / 2, "cells each:\n")
print(tapply(res$fail, res$prior_type, sum))
res$dfail <- !is.finite(res$dr) | res$dr < 0.15
cat("\n== cells where the density at the truth is below 0.15 of the maximum ==\n")
print(tapply(res$dfail, res$prior_type, sum))
cat("\nfailing regularizing cells, both measures, with the prior and its mode:\n")
f <- subset(res, (fail | dfail) & prior_type == "regularizing")
print(f[, c("case","family","par","seed","truth","prior","mode","p_truth","dr")],
      row.names = FALSE)
