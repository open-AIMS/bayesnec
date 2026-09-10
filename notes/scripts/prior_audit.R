# =====================================================================
# Default-prior audit for bayesnec
#
# Provenance. Run against `dev` at eebccdb3 (2026-09-09), R 4.6.1,
# brms 2.23.0. Archived here because it is the only reproduction of the
# measurements cited in #302, #304 and #305.
#
# Updated 2026-09-10 for #305: the prior parser gained a `lognormal`
# case, without which every nec and ec50 cell returns NA and the run
# stops; the output path is now relative and overridable through
# BAYESNEC_PRIOR_AUDIT_OUT; and the summary tables the issue and pull
# request bodies cite are printed at the end.
#
# It no longer reproduces its own nec/ec50 results. Those findings were
# acted on in #302 and PR #304, which replaced the three support-selected
# entries with a normal prior on the log of the predictor, so re-running
# this script against dev at or after 0e8dda3e returns the new prior for
# those two parameters. Run it against eebccdb3 to reproduce the numbers
# as reported.
#
# The top/bot findings in part 1 section 3 and part 2 of
# notes/prior_audit.md are open as #305 and do reproduce.
# =====================================================================
#
# Priors only -- nothing is fitted. Data are simulated from known
# parameters so that each default prior can be compared with the value
# it is meant to locate.
#
# Run from the repository root.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
set.seed(10)

# ---- curve generators (bayesnec's own parameterisation) --------------
mu_nec4 <- function(x, top, bot, nec, beta) {
  bot + (top - bot) * exp(-exp(beta) * (x - nec) * ifelse(x - nec < 0, 0, 1))
}
mu_ecx4 <- function(x, top, bot, ec50, beta) {
  top + (bot - top) / (1 + exp((ec50 - x) * exp(beta)))
}

# ---- designs (predictor on the recorded concentration scale) ---------
# Each design states the series, the true NEC/EC50 on that same scale,
# and a label for the median-to-maximum ratio the issue turns on.
designs <- list(
  linear = list(
    x = seq(0, 10, length.out = 11),
    nec = 4, ec50 = 5),
  linear_unit = list(
    x = seq(0, 1, length.out = 11),
    nec = 0.4, ec50 = 0.5),
  log_2fold = list(
    x = c(0, 0.078, 0.156, 0.3125, 0.625, 1.25, 2.5, 5, 10),
    nec = 1.25, ec50 = 2.5),
  log_unit = list(   # log-spaced but confined to the unit interval
    x = c(0, 0.0039, 0.0078, 0.0156, 0.0312, 0.0625, 0.125, 0.25, 0.5, 1),
    nec = 0.25, ec50 = 0.5),
  log_wide = list(  # nassarius contaminant A series, issue #302
    x = c(0, 0.01, 0.02, 0.04, 0.08, 0.16, 0.31, 0.63, 1.25, 2.5, 20),
    nec = 1.25, ec50 = 2.0)
)
n_rep <- 6

# ---- predictor transforms -------------------------------------------
# log() cannot take the zero control, so the control is replaced by half
# the lowest non-zero dose, which is the usual convention. Recorded so
# the substitution is visible in the output.
transforms <- list(
  identity = list(f = function(x) x, lab = "x", zero_ok = TRUE),
  sqrt     = list(f = sqrt,          lab = "sqrt(x)", zero_ok = TRUE),
  log      = list(f = log,           lab = "log(x)", zero_ok = FALSE)
)

# ---- families: response scale, true top/bot, simulator ---------------
fam_spec <- list(
  gaussian = list(top = 10, bot = 2, alt = "log",
    sim = function(mu) rnorm(length(mu), mu, 0.6)),
  Gamma = list(top = 10, bot = 2, alt = "log",
    sim = function(mu) rgamma(length(mu), shape = 25, rate = 25 / mu)),
  poisson = list(top = 40, bot = 5, alt = "log",
    sim = function(mu) as.integer(rpois(length(mu), mu))),
  negbinomial = list(top = 40, bot = 5, alt = "log",
    sim = function(mu) as.integer(rnbinom(length(mu), mu = mu, size = 5))),
  bernoulli = list(top = 0.95, bot = 0.05, alt = "logit",
    sim = function(mu) as.integer(rbinom(length(mu), 1, mu))),
  binomial = list(top = 0.95, bot = 0.05, alt = "logit", trials = 20,
    sim = function(mu) as.integer(rbinom(length(mu), 20, mu))),
  beta_binomial = list(top = 0.95, bot = 0.05, alt = "logit", trials = 20,
    sim = function(mu) as.integer(rbinom(length(mu), 20, mu))),
  Beta = list(top = 0.9, bot = 0.05, alt = "logit",
    sim = function(mu) rbeta(length(mu), mu * 20, (1 - mu) * 20)),
  hurdle_gamma = list(top = 10, bot = 2, alt = "log",
    sim = function(mu) rgamma(length(mu), 25, 25 / mu) *
            rbinom(length(mu), 1, 0.8)),
  zero_inflated_beta = list(top = 0.9, bot = 0.05, alt = "logit",
    sim = function(mu) rbeta(length(mu), mu * 20, (1 - mu) * 20) *
            rbinom(length(mu), 1, 0.8)),
  zero_inflated_poisson = list(top = 40, bot = 5, alt = "log",
    sim = function(mu) as.integer(rpois(length(mu), mu) *
            rbinom(length(mu), 1, 0.8))),
  zero_inflated_negbinomial = list(top = 40, bot = 5, alt = "log",
    sim = function(mu) as.integer(rnbinom(length(mu), mu = mu, size = 5) *
            rbinom(length(mu), 1, 0.8)))
)

# ---- prior-string parsing and truncated summaries --------------------
parse_prior <- function(s) {
  if (is.na(s) || !nzchar(s)) return(NULL)
  fam <- sub("\\(.*$", "", s)
  args <- as.numeric(strsplit(gsub("^.*\\(|\\)$", "", s), ",")[[1]])
  list(fam = fam, a = args[1], b = args[2])
}
# lognormal was added when #302 replaced the three support-selected nec and
# ec50 entries with one normal on the log of the predictor. Without it every
# nec and ec50 cell -- 960 of the 4,320 -- returns NA and trunc_summary() stops
# the run on the first of them.
pdf_of <- function(p, q) switch(p$fam,
  gamma = dgamma(q, p$a, p$b),
  normal = dnorm(q, p$a, p$b),
  lognormal = dlnorm(q, p$a, p$b),
  beta = dbeta(q, p$a, p$b), rep(NA_real_, length(q)))
cdf_of <- function(p, q) switch(p$fam,
  gamma = pgamma(q, p$a, p$b),
  normal = pnorm(q, p$a, p$b),
  lognormal = plnorm(q, p$a, p$b),
  beta = pbeta(q, p$a, p$b), rep(NA_real_, length(q)))
qf_of <- function(p, q) switch(p$fam,
  gamma = qgamma(q, p$a, p$b),
  normal = qnorm(q, p$a, p$b),
  lognormal = qlnorm(q, p$a, p$b),
  beta = qbeta(q, p$a, p$b), rep(NA_real_, length(q)))

# Summaries of the prior AFTER truncation to [lb, ub], which is the
# density the sampler sees. An untruncated summary of gamma(5, 25) says
# nothing about a prior bounded to the tested range.
trunc_summary <- function(pstr, lb, ub, truth) {
  p <- parse_prior(pstr)
  if (is.null(p)) return(NULL)
  lo <- if (is.na(lb)) -Inf else lb
  hi <- if (is.na(ub)) Inf else ub
  if (p$fam %in% c("gamma", "beta", "lognormal")) lo <- max(lo, 0)
  if (p$fam == "beta") hi <- min(hi, 1)
  Flo <- if (is.infinite(lo) && lo < 0) 0 else cdf_of(p, lo)
  Fhi <- if (is.infinite(hi)) 1 else cdf_of(p, hi)
  mass <- Fhi - Flo
  tq <- function(u) qf_of(p, Flo + u * mass)
  # mode of the truncated density, found on a fine grid inside the bounds
  g <- seq(tq(1e-6), tq(1 - 1e-6), length.out = 4001)
  mode <- g[which.max(pdf_of(p, g))]
  # prior CDF at the truth, on the truncated scale
  pt <- if (is.na(truth)) NA_real_ else {
    if (truth <= lo) 0 else if (truth >= hi) 1 else
      (cdf_of(p, truth) - Flo) / mass
  }
  raw_sd <- switch(p$fam,
    gamma = sqrt(p$a) / p$b,
    normal = p$b,
    lognormal = sqrt((exp(p$b^2) - 1) * exp(2 * p$a + p$b^2)),
    beta = sqrt(p$a * p$b / ((p$a + p$b)^2 * (p$a + p$b + 1))),
    NA_real_)
  list(mode = mode, med = tq(0.5), q025 = tq(0.025), q975 = tq(0.975),
       p_truth = pt, mass_kept = mass, raw_sd = raw_sd)
}

# ---- run -------------------------------------------------------------
rows <- list()
for (dn in names(designs)) {
 dsp <- designs[[dn]]
 for (tn in names(transforms)) {
  tr <- transforms[[tn]]
  xraw <- dsp$x
  sub_note <- ""
  if (!tr$zero_ok && any(xraw == 0)) {
    nz <- min(xraw[xraw > 0])
    xraw[xraw == 0] <- nz / 2
    sub_note <- sprintf("control 0 -> %.4g", nz / 2)
  }
  xt <- tr$f(xraw)                      # predictor as define_prior sees it
  for (fn in names(fam_spec)) {
   fs <- fam_spec[[fn]]
   for (lk in c("identity", fs$alt)) {
    for (ptype in c("uninformative", "regularizing")) {
     for (mod in c("nec4param", "ecx4param")) {
      xp <- if (mod == "nec4param") dsp$nec else dsp$ec50
      # true curve is generated on the natural response scale against the
      # transformed predictor, so the true nec/ec50 on the fitted scale is
      # the transform of the true dose.
      xp_t <- tr$f(xp)
      xx <- rep(xt, each = n_rep)      # transformed: the curve's own scale
      xr <- rep(xraw, each = n_rep)    # recorded: what the formula receives
      # The decay rate is set from the transformed range rather than fixed, so
      # that every design descends from top to bot within the doses tested. A
      # design that never reaches bot leaves bot unidentified by the data, and
      # a prior derived from that response could not locate it whatever rule
      # built it -- which would be a property of the design, not of the prior.
      rate <- 5 / (max(xt) - xp_t)
      mu <- if (mod == "nec4param") {
        mu_nec4(xx, fs$top, fs$bot, xp_t, log(rate))
      } else {
        mu_ecx4(xx, fs$top, fs$bot, xp_t, log(rate))
      }
      mu_span <- range(mu)
      y <- fs$sim(mu)
      dat <- data.frame(x = xr, y = y)
      form <- sprintf("y ~ crf(%s, \"%s\")",
                      sub("x", "x", tr$lab), mod)
      if (!is.null(fs$trials)) {
        dat$trials <- fs$trials
        form <- sprintf("y | trials(trials) ~ crf(%s, \"%s\")", tr$lab, mod)
      }
      # The second block of a hurdle or zero-inflated family must stay on
      # the identity link (validate_family()), so only the mean link varies.
      fargs <- list(link = lk)
      ctor_args <- names(formals(get(fn)))
      for (la in intersect(c("link_hu", "link_zi"), ctor_args)) {
        fargs[[la]] <- "identity"
      }
      fam <- try(do.call(get(fn), fargs), silent = TRUE)
      if (inherits(fam, "try-error")) next
      pr <- try(suppressWarnings(suppressMessages(
        get_priors(as.formula(form), data = dat, family = fam,
                   prior_type = ptype))), silent = TRUE)
      if (inherits(pr, "try-error")) {
        rows[[length(rows) + 1]] <- data.frame(
          design = dn, transform = tn, family = fn, link = lk,
          prior_type = ptype, model = mod, par = "<error>",
          prior = as.character(attr(pr, "condition")$message),
          lb = NA, ub = NA, truth = NA, mode = NA, q025 = NA, q975 = NA,
          p_truth = NA, raw_sd = NA, x_type = NA, mu_min = NA, mu_max = NA,
          y_min = NA, y_q10 = NA, y_q25 = NA, y_q75 = NA, y_q90 = NA,
          y_max = NA, y_sd = NA, y_zero_frac = NA, sub = sub_note,
          stringsAsFactors = FALSE)
        next
      }
      pr <- as.data.frame(pr)
      # The response statistics define_prior() actually reads, obtained by
      # replaying the same path get_priors() takes: model frame, check_data(),
      # rate conversion, then response_link_scale(). Recorded so that each
      # prior string in the export can be traced to the quantity it was built
      # from, which is what a cross-family comparison needs.
      ystat <- tryCatch({
        sf <- single_model_formula(bayesnecformula(as.formula(form)), mod)
        md <- model.frame(sf, data = dat, run_par_checks = FALSE)
        ck <- suppressWarnings(suppressMessages(
          check_data(data = md, family = fam, model = mod)))
        yy <- ck$mod_dat$y
        if (ck$family$family %in% c("binomial", "beta_binomial")) {
          yy <- yy / ck$mod_dat$trials
        }
        if (!is.null(ck$mod_dat$denom)) yy <- yy / ck$mod_dat$denom
        if (is_hurdle_family(ck$family)) {
          # the mu block is primed from the non-zero response only
          yl <- response_link_scale(
            split_hurdle_response(ck$mod_dat$x, yy)$mu$y,
            hurdle_mu_family(ck$family))
        } else {
          yl <- response_link_scale(yy, ck$family)
        }
        qs <- unname(quantile(yl, c(0, 0.1, 0.25, 0.75, 0.9, 1)))
        c(qs, sd(yl), mean(yy == 0))
      }, error = function(e) rep(NA_real_, 8))
      # truth for each parameter, on the scale the prior lives on
      link_f <- fam$linkfun
      truths <- c(top = link_f(fs$top), bot = link_f(fs$bot),
                  nec = xp_t, ec50 = xp_t)
      for (i in seq_len(nrow(pr))) {
        np <- pr$nlpar[i]
        if (!np %in% names(truths)) next
        lb <- suppressWarnings(as.numeric(pr$lb[i]))
        ub <- suppressWarnings(as.numeric(pr$ub[i]))
        ss <- trunc_summary(pr$prior[i], lb, ub, truths[[np]])
        rows[[length(rows) + 1]] <- data.frame(
          design = dn, transform = tn, family = fn, link = lk,
          prior_type = ptype, model = mod, par = np, prior = pr$prior[i],
          lb = lb, ub = ub, truth = truths[[np]],
          mode = if (is.null(ss)) NA else ss$mode,
          q025 = if (is.null(ss)) NA else ss$q025,
          q975 = if (is.null(ss)) NA else ss$q975,
          p_truth = if (is.null(ss)) NA else ss$p_truth,
          raw_sd = if (is.null(ss)) NA else ss$raw_sd,
          x_type = set_distribution(xt, silence_y_msgs = TRUE,
                                    silence_x_msgs = TRUE),
          mu_min = mu_span[1], mu_max = mu_span[2],
          y_min = ystat[1], y_q10 = ystat[2], y_q25 = ystat[3],
          y_q75 = ystat[4], y_q90 = ystat[5], y_max = ystat[6],
          y_sd = ystat[7], y_zero_frac = ystat[8],
          sub = sub_note, stringsAsFactors = FALSE)
      }
     }
    }
   }
  }
 }
}
res <- do.call(rbind, rows)
out <- Sys.getenv("BAYESNEC_PRIOR_AUDIT_OUT", "prior_audit.rds")
saveRDS(res, out)
cat("rows:", nrow(res), "\n")
cat("errors:", sum(res$par == "<error>"), "\n")
print(unique(res$prior[res$par == "<error>"]))
cat("written to", out, "\n")

# ---- the tables the issue and PR bodies cite --------------------------
# top and bot are read from the nec4param cells and nec/ec50 from the
# ecx4param cells, so that each parameter is assessed on the equation
# whose curve identifies it. An ecx4param design does not reach its own
# upper asymptote on the wide series, so scoring `top` there would
# measure the design rather than the prior.
band <- function(d) !is.na(d$p_truth) & (d$p_truth < 0.025 | d$p_truth > 0.975)
tb <- subset(res, par %in% c("top", "bot") & model == "nec4param")
tb$fail <- band(tb)
cat("\n== top and bot: cells outside the central 95% of the truncated prior ==\n")
print(with(tb, tapply(fail, list(paste(family, link), prior_type), sum)))
cat("\ntotals of", sum(tb$prior_type == "regularizing"), "cells each:\n")
print(tapply(tb$fail, tb$prior_type, sum))
cat("\nmean distance of the truncated CDF at the truth from 0.5:\n")
print(round(tapply(abs(tb$p_truth - 0.5), tb$prior_type, mean, na.rm = TRUE), 4))

xp <- subset(res, (par == "nec" & model == "nec4param") |
                    (par == "ec50" & model == "ecx4param"))
xp$fail <- band(xp)
cat("\n== nec and ec50: cells outside the central 95% ==\n")
print(with(xp, tapply(fail, list(par, prior_type), sum)))
cat("truncated CDF at the truth, range by prior type:\n")
for (pt in unique(xp$prior_type)) {
  cat(" ", pt, ":",
      paste(round(range(xp$p_truth[xp$prior_type == pt], na.rm = TRUE), 3),
            collapse = " to "), "\n")
}

# NOTE on ratios. The response is redrawn inside the prior_type loop
# above, so a ratio of prior widths taken across the two types here
# compares priors built from different draws. The coverage tables are
# unaffected, because each prior is scored against its own data, but a
# ratio must be measured with both priors built from one response. That
# is what the ratio figures in PR #307 report and this script does not.
cat("\nRatios across prior types are NOT reported here; see the note in the",
    "source.\n")
