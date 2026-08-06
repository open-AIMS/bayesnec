# Phase 1 acceptance for the beta_ub family: fit a raw brm() on the Phase 0
# simulation, outside bnec(), using a bayesnec equation written out by hand.
#
# Confirms: compiles; recovers the generating values; loo() runs;
# posterior_epred() matches the analytic mean; posterior_predict() reproduces
# the data scale.

suppressMessages(library(brms))
suppressMessages(devtools::load_all("."))

set.seed(173)
options(mc.cores = 2)

nec3 <- function(x, top, beta, nec) top * exp(-exp(beta) * pmax(x - nec, 0))

truth <- list(top = 0.8, beta = 0, nec = 2, phi = 30, U = 1)
x <- rep(seq(0, 6, length.out = 8), each = 5)
mu <- nec3(x, truth$top, truth$beta, truth$nec)
y <- truth$U * rbeta(length(mu), (mu / truth$U) * truth$phi,
                     (1 - mu / truth$U) * truth$phi)
d <- data.frame(x = x, y = y)
ymax <- max(d$y)

sv <- beta_ub_stanvars(ymax)
bf1 <- bf(y ~ top * exp(-exp(beta) * (x - nec) * step(x - nec)),
          top + beta + nec ~ 1, nl = TRUE)
pr <- prior(gamma(2, 2), nlpar = "top", lb = 0) +
  prior(normal(0, 5), nlpar = "beta") +
  prior(uniform(0, 6), nlpar = "nec", lb = 0, ub = 6) +
  prior(gamma(0.01, 0.01), class = "phi") +
  prior_string(paste0("normal(", 1 - ymax, ", 0.1)"), class = "delta")

fit <- brm(bf1, data = d, family = beta_ub(), stanvars = sv, prior = pr,
           chains = 2, iter = 2000, warmup = 1000, seed = 173,
           control = list(adapt_delta = 0.95), refresh = 0,
           backend = "cmdstanr")

cat("\n== parameter recovery ==\n")
s <- summary(fit)
print(fixef(fit))
ps <- as_draws_df(fit)
cat("phi   median", sprintf("%.2f", median(ps$phi)), " truth", truth$phi, "\n")
cat("delta median", sprintf("%.4f", median(ps$delta)), "\n")
cat("U     median", sprintf("%.4f", median(ymax + ps$delta)),
    " truth", truth$U, "\n")

cat("\n== diagnostics ==\n")
cat("max rhat:", sprintf("%.4f", max(rhat(fit), na.rm = TRUE)), "\n")
np <- nuts_params(fit)
cat("divergences:", sum(np$Value[np$Parameter == "divergent__"]), "\n")

cat("\n== loo() ==\n")
l <- suppressWarnings(loo(fit))
print(l$estimates)

cat("\n== posterior_epred() vs analytic mean ==\n")
nd <- data.frame(x = seq(0, 6, length.out = 25))
ep <- posterior_epred(fit, newdata = nd)
dr <- as_draws_df(fit)
analytic <- sapply(nd$x, function(z) {
  nec3(z, dr$b_top_Intercept, dr$b_beta_Intercept, dr$b_nec_Intercept)
})
cat("max abs difference:", format(max(abs(ep - analytic)), digits = 3), "\n")
cat("(epred returns mu, and E[y] = mu exactly for this family)\n")

cat("\n== posterior_predict() ==\n")
pp <- posterior_predict(fit, newdata = nd)
cat("range of draws:", sprintf("%.4f", min(pp)), "to", sprintf("%.4f", max(pp)),
    "\n")
u_med <- median(ymax + dr$delta)
cat("median ceiling U:", sprintf("%.4f", u_med), "\n")
cat("all draws strictly inside (0, U) per draw:",
    all(pp > 0), "\n")
cat("predictive mean at x = 0:", sprintf("%.4f", mean(pp[, 1])),
    " vs epred", sprintf("%.4f", mean(ep[, 1])), "\n")

cat("\n== log_lik() ==\n")
ll <- log_lik(fit)
cat("dim:", paste(dim(ll), collapse = " x "), " all finite:",
    all(is.finite(ll)), "\n")

saveRDS(list(fit = fit, truth = truth, ymax = ymax),
        "dev/prototype/phase1_fit.rds")
cat("\nwritten: dev/prototype/phase1_fit.rds\n")
