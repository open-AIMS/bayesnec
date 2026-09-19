# =====================================================================
# Default-prior audit for bayesnec
#
# Provenance. Parts 1 to 4 run against `dev` at 441e7464 (2026-09-19),
# R 4.6.1, brms 2.23.0, MASS 7.3.65. MASS is read by part 4 alone, for
# the negative binomial comparison #391 asks for; it is not a bayesnec
# dependency and part 4 reports the comparison as not run where it is
# absent.
#
# Original provenance. Part 1 was first run against `dev` at eebccdb3
# (2026-09-09), R 4.6.1, brms 2.23.0. Archived here because it is the
# only reproduction of the measurements cited in #302, #304 and #305.
#
# Updated 2026-09-10 for #305: the prior parser gained a `lognormal`
# case, without which every nec and ec50 cell returns NA and the run
# stops; the output path is now relative and overridable through
# BAYESNEC_PRIOR_AUDIT_OUT; and the summary tables the issue and pull
# request bodies cite are printed at the end.
#
# Updated 2026-09-19 for #391. Part 1 gained a completeness axis, and a
# proposal count and a flatness result per cell; parts 2 to 4 are new.
#
# Part 1 no longer reproduces its own nec/ec50 results. Those findings
# were acted on in #302 and PR #304, which replaced the three
# support-selected entries with a normal prior on the log of the
# predictor, so re-running this script against dev at or after 0e8dda3e
# returns the new prior for those two parameters. Run it against
# eebccdb3 to reproduce the numbers as reported. The `top`/`bot`
# findings in part 1 section 3 of notes/prior_audit.md were open as #305
# and the complete-design cells no longer reproduce them either; the
# 2026-09-19 run is recorded in that file beside the earlier one.
# =====================================================================
#
# Priors and initial values only -- nothing is fitted, and no Stan
# program is compiled. Data are simulated from known parameters so that
# each default prior can be compared with the value it is meant to
# locate.
#
# Run from the repository root:
#
#   Rscript notes/scripts/prior_audit.R [part ...]
#
# With no argument every part runs. Naming one or more of sweep,
# reproduction, calibration, miss runs those alone.
#
#   1. sweep         the factorial prior sweep, crossed with a
#                    completeness axis, with the proposals the
#                    initial-value search draws and the report of the
#                    flatness rule per cell;
#   2. reproduction  the `bot` and threshold figures #386 quotes, on
#                    #386's own designs;
#   3. calibration   the false-positive rate of the flatness rule on a
#                    flat top, by family and layout, including the four
#                    items #390 disclosed;
#   4. miss          its miss rate on a top that is still declining,
#                    including the negative binomial comparison against
#                    MASS::glm.nb.
#
# Every stochastic part seeds itself, so a part named alone reproduces
# the figures it prints in a full run.
suppressMessages(pkgload::load_all(".", quiet = TRUE))

selected <- commandArgs(trailingOnly = TRUE)
run_this <- function(name) length(selected) == 0 || name %in% selected

# Replicate counts for parts 3 and 4. Overridable so that a structural
# change can be exercised in seconds; the figures recorded in
# notes/prior_audit.md are from the defaults.
N_REP_CAL <- as.integer(Sys.getenv("BAYESNEC_PRIOR_AUDIT_NREP", "4000"))
N_REP_GAUSS <- as.integer(Sys.getenv("BAYESNEC_PRIOR_AUDIT_NREP_GAUSS",
                                     "20000"))
# The shipped initial-value search caps at 1e4 rounds. Part 1 runs one
# search per cell over 5,760 cells, so the cap is lowered here and the
# cells that reach it are counted rather than being left to dominate the
# wall clock. A cell that reaches 200 rounds has not found a full set of
# four chains from 800 proposals, which is the finding; the released cap
# would change how large the recorded number is and not whether the
# search succeeded.
INIT_CAP <- 200L
CHAINS <- 4L

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

# ---- the completeness axis (#391) ------------------------------------
# `completeness` states how far the mean response at the highest
# concentration has travelled from `top` towards `bot`, as a fraction of
# that span. The complete setting is the one the sweep has always used:
# a decay rate of 5 over the distance from the threshold to the highest
# dose leaves exp(-5) of the span untravelled, so the response is within
# 0.7 per cent of `bot` at the highest dose.
#
# The three incomplete settings are #386's, at 0.92, 0.36 and 0.02.
#
# How the setting is reached matters, because two devices are available
# and they measure different things.
#
# Dropping the design's own doses above a cut point -- the literal
# reading of "the series is truncated" -- gives no control over the
# fraction reached on an evenly spaced series: on `linear` the cut for
# 0.02 falls between the fourth and fifth dose, whose realised fraction
# is zero, so the cell would not be the cell that was asked for. It also
# changes the number of concentrations and the ratio of the maximum to
# the median dose, and the gamma branch of the `nec` prior is a function
# of that ratio alone (notes/prior_audit.md part 1 section 1), so the
# completeness contrast would be confounded with a change in the prior's
# own input.
#
# The device used instead holds the series and the decay rate fixed and
# moves the true threshold along the series, which is the situation the
# issue describes: a fixed dilution series against a sample that is more
# or less toxic than the one the series was designed for. Every
# predictor-derived quantity -- the median dose, the maximum, the
# transform, `set_distribution()`'s branch -- is then identical across
# the four completeness settings, and the only thing that differs is how
# much of the curve the design shows.
#
# The threshold that reaches a stated fraction f at the highest dose,
# with the rate held at the complete design's value, is
#
#   nec4param   nec_f  = max(x) + log(1 - f) / rate
#   ecx4param   ec50_f = max(x) + log((1 - f) / f) / rate
#
# For `nec4param` the threshold stays inside the series at every
# setting, so those cells measure what an unidentified lower asymptote
# does to the `bot` prior. For `ecx4param` the threshold rises above the
# highest dose once f falls below a half, so those cells measure what it
# does to the threshold prior's truncation. The two findings #386
# reports are therefore separated by equation rather than mixed.
completeness <- c(complete = 1 - exp(-5), f92 = 0.92, f36 = 0.36,
                  f02 = 0.02)
shifted_threshold <- function(model, x_max, rate, f) {
  if (model == "nec4param") {
    x_max + log(1 - f) / rate
  } else {
    x_max + log((1 - f) / f) / rate
  }
}

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

# ---- the two columns #391 adds ---------------------------------------
# The proposals the initial-value search draws, counted by tracing
# make_inits() rather than by reimplementing the search. make_inits() is
# the only place make_good_inits() draws from, so the count is of the
# released search and cannot drift from it. refine_inits() re-draws one
# parameter at a time for a curve that is finite but out of range and is
# not counted, which is the convention notes/scripts/init_search_audit.R
# already uses for a proposal count.
proposal_counter <- new.env(parent = emptyenv())
proposal_counter$n <- 0L
with_proposal_count <- function(expr) {
  proposal_counter$n <- 0L
  suppressMessages(trace("make_inits", where = asNamespace("bayesnec"),
                         print = FALSE,
                         exit = quote(
                           proposal_counter$n <- proposal_counter$n +
                             length(returnValue()))))
  on.exit(suppressMessages(untrace("make_inits",
                                   where = asNamespace("bayesnec"))),
          add = TRUE)
  value <- force(expr)
  list(value = value, proposals = proposal_counter$n)
}

# The flatness rule of #390, called through the two functions
# check_response_flattened() computes it with, so that the p-value and
# the status are recorded and not only whether a message was emitted.
# The wrapper is checked against them in part 3.
#
# `only` names the blocks to read. A hurdle family returns two, and a
# calibration figure for one of them has to be measured on that one
# alone: the response block of the survival generator below is a flat
# gamma, so reading both would add its own five per cent to the
# survival block's rate and report a tenth where the rule reports a
# twentieth.
flatness_result <- function(x, y, trials, denominator, family,
                            alpha = 0.05, pool_dispersion = TRUE,
                            only = NULL) {
  blocks <- flatness_blocks(x, y, trials, denominator, family)
  if (!is.null(only)) {
    blocks <- blocks[intersect(names(blocks), only)]
  }
  if (length(blocks) == 0) {
    return(list(status = "none", declining = NA, p_value = NA_real_))
  }
  out <- lapply(blocks, flatness_contrast, alpha = alpha,
                pool_dispersion = pool_dispersion)
  tested <- vapply(out, function(z) identical(z$status, "tested"), logical(1))
  status <- if (any(tested)) {
    "tested"
  } else if (any(vapply(out, function(z) identical(z$status, "failed"),
                        logical(1)))) {
    "failed"
  } else {
    "skipped"
  }
  declining <- if (any(tested)) {
    any(vapply(out[tested], function(z) isTRUE(z$declining), logical(1)))
  } else {
    NA
  }
  # The smallest p-value over the blocks, which is the one that decides
  # whether the report is raised.
  p_value <- if (any(tested)) {
    min(vapply(out[tested], function(z) z$p_value, numeric(1)))
  } else {
    NA_real_
  }
  list(status = status, declining = declining, p_value = p_value)
}

# ======================================================================
# Part 1. The factorial sweep
# ======================================================================
if (run_this("sweep")) {
set.seed(10)
rows <- list()
cell_id <- 0L
started <- Sys.time()
for (cn in names(completeness)) {
 f_target <- completeness[[cn]]
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
       cell_id <- cell_id + 1L
       xp <- if (mod == "nec4param") dsp$nec else dsp$ec50
       # true curve is generated on the natural response scale against the
       # transformed predictor, so the true nec/ec50 on the fitted scale is
       # the transform of the true dose.
       xp_t <- tr$f(xp)
       xx <- rep(xt, each = n_rep)      # transformed: the curve's own scale
       xr <- rep(xraw, each = n_rep)    # recorded: what the formula receives
       # The decay rate is set from the transformed range of the complete
       # design and then held across the completeness axis, so that the four
       # settings differ in where the threshold sits and in nothing else.
       rate <- 5 / (max(xt) - xp_t)
       xp_t <- shifted_threshold(mod, max(xt), rate, f_target)
       mu <- if (mod == "nec4param") {
         mu_nec4(xx, fs$top, fs$bot, xp_t, log(rate))
       } else {
         mu_ecx4(xx, fs$top, fs$bot, xp_t, log(rate))
       }
       mu_span <- range(mu)
       # The fraction of the top-to-bot span the mean curve has travelled at
       # the highest dose, and #386's own label for the same design, which is
       # 1 - mean(mu at max) / mean(mu at min). The two are not the same
       # quantity: for #386's top of 0.9 and bot of 0.1 the second is 0.889
       # times the first. Both are recorded so that nothing rests on the label.
       f_reached <- (mu[which.min(xx)][1] - mu[which.max(xx)][1]) /
         (fs$top - fs$bot)
       max_effect <- 1 - mu[which.max(xx)][1] / mu[which.min(xx)][1]
       y <- fs$sim(mu)
       dat <- data.frame(x = xr, y = y)
       form <- sprintf("y ~ crf(%s, \"%s\")", tr$lab, mod)
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
           cell = cell_id, completeness = cn, design = dn, transform = tn,
           family = fn, link = lk,
           prior_type = ptype, model = mod, par = "<error>",
           prior = as.character(attr(pr, "condition")$message),
           lb = NA, ub = NA, truth = NA, mode = NA, q025 = NA, q975 = NA,
           p_truth = NA, raw_sd = NA, x_type = NA, mu_min = NA, mu_max = NA,
           f_reached = f_reached, max_effect = max_effect,
           y_min = NA, y_q10 = NA, y_q25 = NA, y_q75 = NA, y_q90 = NA,
           y_max = NA, y_sd = NA, y_zero_frac = NA,
           n_proposals = NA, init_capped = NA, flat_status = NA,
           flat_report = NA, flat_p = NA,
           sub = sub_note, stringsAsFactors = FALSE)
         next
       }
       pr <- as.data.frame(pr)
       # The response statistics define_prior() actually reads, obtained by
       # replaying the same path get_priors() takes: model frame, check_data(),
       # rate conversion, then response_link_scale(). Recorded so that each
       # prior string in the export can be traced to the quantity it was built
       # from, which is what a cross-family comparison needs. The same replay
       # supplies the predictor and response the initial-value search and the
       # flatness rule are given inside a fit.
       replay <- tryCatch({
         sf <- single_model_formula(bayesnecformula(as.formula(form)), mod)
         md <- model.frame(sf, data = dat, run_par_checks = FALSE)
         ck <- suppressWarnings(suppressMessages(
           check_data(data = md, family = fam, model = mod)))
         yy <- ck$mod_dat$y
         if (ck$family$family %in% c("binomial", "beta_binomial")) {
           yy <- yy / ck$mod_dat$trials
         }
         if (!is.null(ck$mod_dat$denom)) yy <- yy / ck$mod_dat$denom
         list(md = md, ck = ck, yy = yy)
       }, error = function(e) NULL)
       ystat <- rep(NA_real_, 8)
       n_prop <- NA_integer_
       capped <- NA
       flat <- list(status = NA_character_, declining = NA, p_value = NA_real_)
       if (!is.null(replay)) {
         ck <- replay$ck
         yy <- replay$yy
         ystat <- tryCatch({
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
         # The flatness rule, on the blocks and the family the fit would use.
         # pool_dispersion is TRUE because no design here carries a disp()
         # term, which is the case in which bnec() fits a single sigma.
         flat <- tryCatch(
           flatness_result(ck$mod_dat$x, ck$mod_dat$y, ck$mod_dat$trials,
                           ck$mod_dat$denom, ck$family),
           error = function(e) list(status = "error", declining = NA,
                                    p_value = NA_real_))
         # The initial-value search, run as add_brm_defaults() runs it: the
         # curve coefficients only, on the response the fit is given. The
         # seed is the cell index so that a cell reproduces when a part is
         # run alone, and so that no two cells share a proposal stream.
         search <- tryCatch({
           ip <- pr[pr$class == "b" &
                      !pr$nlpar %in% generated_term_names(), , drop = FALSE]
           counted <- if (is_hurdle_family(ck$family)) {
             with_proposal_count(suppressMessages(make_good_hurdle_inits(
               mod, ck$mod_dat$x, yy, priors = ip, chains = CHAINS,
               family = ck$family, dpar = hurdle_dpar(ck$family),
               seed = cell_id, n_trials = INIT_CAP, report_after = Inf)))
           } else {
             with_proposal_count(suppressMessages(make_good_inits(
               mod, ck$mod_dat$x, response_link_scale(yy, ck$family),
               family = ck$family, priors = ip, chains = CHAINS,
               seed = cell_id, n_trials = INIT_CAP, report_after = Inf)))
           }
           fell_back <- length(counted$value) == 1 &&
             "random" %in% names(counted$value)
           list(n = counted$proposals, capped = fell_back)
         }, error = function(e) NULL)
         if (!is.null(search)) {
           n_prop <- as.integer(search$n)
           capped <- search$capped
         }
       }
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
           cell = cell_id, completeness = cn, design = dn, transform = tn,
           family = fn, link = lk,
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
           f_reached = f_reached, max_effect = max_effect,
           y_min = ystat[1], y_q10 = ystat[2], y_q25 = ystat[3],
           y_q75 = ystat[4], y_q90 = ystat[5], y_max = ystat[6],
           y_sd = ystat[7], y_zero_frac = ystat[8],
           n_proposals = n_prop, init_capped = capped,
           flat_status = flat$status, flat_report = flat$declining,
           flat_p = flat$p_value,
           sub = sub_note, stringsAsFactors = FALSE)
       }
      }
     }
    }
   }
  }
  message("sweep: ", cn, " ", dn, " (",
          signif(as.numeric(Sys.time() - started, units = "secs"), 3), "s)")
 }
}
res <- do.call(rbind, rows)
# Written under tempdir() by default, so that a run from the repository root
# does not leave an untracked .rds behind. Set BAYESNEC_PRIOR_AUDIT_OUT to keep
# the result.
out <- Sys.getenv("BAYESNEC_PRIOR_AUDIT_OUT",
                  file.path(tempdir(), "prior_audit.rds"))
saveRDS(res, out)
cat("\n=== 1. the factorial sweep ===\n")
cat("rows:", nrow(res), "  cells:", length(unique(res$cell)), "\n")
cat("errors:", sum(res$par == "<error>"), "\n")
print(subset(res, par == "<error>",
             select = c(completeness, design, transform, family, link,
                        prior_type, model, prior)), row.names = FALSE)
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
cat("\n== top and bot: cells outside the central 95% of the truncated prior,",
    "by completeness ==\n")
print(with(tb, tapply(fail, list(paste(family, link), completeness),
                      sum))[, names(completeness), drop = FALSE])
cat("\ntotals of", sum(tb$prior_type == "regularizing" &
                         tb$completeness == "complete"),
    "cells per prior type and completeness setting:\n")
print(with(tb, tapply(fail, list(prior_type, completeness),
                      sum))[, names(completeness), drop = FALSE])
cat("\n`bot` alone, mean truncated CDF at the truth:\n")
bo <- subset(tb, par == "bot")
print(round(with(bo, tapply(p_truth, list(prior_type, completeness), mean,
                            na.rm = TRUE))[, names(completeness),
                                           drop = FALSE], 4))

xp <- subset(res, (par == "nec" & model == "nec4param") |
                    (par == "ec50" & model == "ecx4param"))
xp$fail <- band(xp)
cat("\n== nec and ec50: cells outside the central 95%, by completeness ==\n")
print(with(xp, tapply(fail, list(paste(par, prior_type), completeness),
                      sum))[, names(completeness), drop = FALSE])
cat("\ncells whose prior support excludes the truth (p_truth exactly 1):\n")
print(with(xp, tapply(p_truth >= 1, list(paste(par, prior_type), completeness),
                      sum, na.rm = TRUE))[, names(completeness), drop = FALSE])

# ---- the two columns #391 adds ---------------------------------------
cells <- res[!duplicated(res$cell), ]
cat("\n== initial-value proposals to a full set of", CHAINS, "chains ==\n")
cat("cap", INIT_CAP, "rounds; cells that fell back to Stan's own",
    "initialisation:", sum(cells$init_capped, na.rm = TRUE), "of",
    sum(!is.na(cells$init_capped)), "\n")
cat("A hurdle or zero-inflated cell primes two blocks in turn, so its count",
    "covers\nboth and its ceiling is twice the", INIT_CAP * CHAINS,
    "of a single-block cell.\n")
print(round(with(cells, tapply(n_proposals, list(model, completeness), median,
                               na.rm = TRUE))[, names(completeness),
                                              drop = FALSE], 1))
cat("\n90th percentile of the proposal count by completeness:\n")
print(round(with(cells, tapply(n_proposals, completeness, quantile,
                               probs = 0.9, na.rm = TRUE))[names(completeness)],
            1))

cat("\n== the flatness rule of #390 over the sweep ==\n")
cat("status of the contrast, by completeness:\n")
print(with(cells, table(flat_status, completeness))[, names(completeness),
                                                    drop = FALSE])
cat("\nreport rate where the contrast was computed, by completeness:\n")
tested <- subset(cells, flat_status == "tested")
print(round(with(tested, tapply(flat_report, list(family, completeness), mean,
                                na.rm = TRUE))[, names(completeness),
                                               drop = FALSE], 3))
cat("\nover all families:\n")
print(round(with(tested, tapply(flat_report, completeness, mean,
                                na.rm = TRUE))[names(completeness)], 3))
cat("\nand by design, which is where the `complete` column comes from:\n")
print(round(with(tested, tapply(flat_report, list(design, completeness), mean,
                                na.rm = TRUE))[, names(completeness),
                                               drop = FALSE], 3))
cat("\nNote. The `complete` column is not the rule's false-positive rate.\n",
    "The rule tests the last interval of the series and not whether the\n",
    "response has reached bot, and on a log-spaced series that interval is\n",
    "the widest one there is: on log_wide the mean is 72 per cent of the\n",
    "way from top to bot at 2.5 and within 1 per cent of bot at 20, so the\n",
    "contrast is large and true. The report rate rises with the spacing of\n",
    "the last step, from 0.09 on the evenly spaced designs to 0.94 on\n",
    "log_wide, and those reports are power and not false positives. Part 3\n",
    "measures the false-positive rate on a top that is flat.\n",
    sep = "")

# NOTE on ratios. The response is redrawn inside the prior_type loop
# above, so a ratio of prior widths taken across the two types here
# compares priors built from different draws. The coverage tables are
# unaffected, because each prior is scored against its own data, but a
# ratio must be measured with both priors built from one response. That
# is what the ratio figures in PR #307 report and this script does not.
cat("\nRatios across prior types are NOT reported here; see the note in the",
    "source.\n")
}

# ======================================================================
# Part 2. The figures #386 quotes
# ======================================================================
# #386's measurements were produced by a script held in a session
# scratch and never committed. Its design is restated from the issue
# text: a nec4param response on a log-spaced series of eight
# concentrations from 0 to 40 by five replicates, with top 0.9 and
# bot 0.1 on the gaussian branch and top 40 and bot 4 on the positive
# branch, under three settings of the threshold and the decay rate that
# differ in the maximum effect observed at the highest concentration.
#
# The seed, the residual standard deviation and the exact threshold and
# rate settings are not recoverable from the issue, so the figures below
# are a reproduction of the design and not of the draw. Two of the three
# were recovered from the three sd(response) values the issue quotes,
# which are 0.384, 0.115 and 0.040.
#
# A residual standard deviation of 0.04 is what leaves sd(response) at
# 0.040 on the flattest design, where the curve contributes almost
# nothing to it. A decay rate of 2.5 is what reaches 0.384 on the
# complete design: the response is at `top` for the five lowest doses
# and at `bot` for the three highest, and a five-three split of a span
# of 0.8 has a standard deviation of 0.387. A slower decay spreads the
# doses through the middle of the span and cannot reach it -- a rate of
# 0.129, which is what the part 1 rule would set here, gives 0.29.
#
# The three settings then differ in the threshold alone, which is one
# of the two things #386 varied. Where the target effect exceeds
# 1 - bot / top the mean curve cannot reach it at any threshold, and the
# design is the complete one; #386's 0.92 against an attainable 0.889 is
# the noise in a mean of five replicates.
if (run_this("reproduction")) {
set.seed(386)
cat("\n\n=== 2. the figures #386 quotes ===\n")

rep_x <- c(0, 0.31, 0.63, 1.25, 2.5, 5, 10, 40)
rep_reps <- 5
rep_rate <- 2.5
# Maximum effect as #386 defines it: 1 - mean(y at 40) / mean(y at 0).
# The threshold that reaches a stated effect E on the mean curve, with
# the rate held, follows from mu(40) = (1 - E) mu(0) = (1 - E) top.
rep_nec <- function(effect, top, bot, rate) {
  share <- ((1 - effect) * top - bot) / (top - bot)
  if (share <= 0) {
    # Unattainable: the complete design, with the threshold between the
    # fifth and the sixth dose.
    return(3)
  }
  max(rep_x) + log(share) / rate
}
rep_row <- function(effect, top, bot, sim, family_tag, link) {
  rate <- rep_rate
  nec <- rep_nec(effect, top, bot, rate)
  x <- rep(rep_x, each = rep_reps)
  mu <- mu_nec4(x, top, bot, nec, log(rate))
  y <- sim(mu)
  dat <- data.frame(x = x, y = y)
  realised <- 1 - mean(y[x == max(rep_x)]) / mean(y[x == 0])
  out <- list()
  for (ptype in c("uninformative", "regularizing")) {
    pr <- as.data.frame(suppressWarnings(suppressMessages(get_priors(
      y ~ crf(x, "nec4param"), data = dat,
      family = do.call(family_tag, list(link = link)),
      prior_type = ptype))))
    row <- pr[pr$nlpar == "bot", ]
    ss <- trunc_summary(row$prior[1],
                        suppressWarnings(as.numeric(row$lb[1])),
                        suppressWarnings(as.numeric(row$ub[1])), bot)
    out[[length(out) + 1]] <- data.frame(
      target_effect = effect, realised_effect = round(realised, 3),
      prior_type = ptype, bot_prior = row$prior[1],
      p_truth = signif(ss$p_truth, 3), sd_response = signif(sd(y), 3),
      stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}

cat("\n-- the gaussian branch: the `bot` prior against a true bot of 0.1 --\n")
gauss <- do.call(rbind, lapply(c(0.92, 0.36, 0.02), rep_row,
                               top = 0.9, bot = 0.1,
                               sim = function(mu) rnorm(length(mu), mu, 0.04),
                               family_tag = "gaussian", link = "identity"))
print(gauss, row.names = FALSE)
cat("\n#386 quotes, on the same three designs:\n")
print(data.frame(
  target_effect = rep(c(0.92, 0.36, 0.02), each = 2),
  prior_type = rep(c("uninformative", "regularizing"), 3),
  bot_prior = c("normal(0.084, 0.960)", "normal(0.076, 0.384)",
                "normal(0.607, 0.287)", "normal(0.579, 0.115)",
                "normal(0.848, 0.101)", "normal(0.913, 0.040)"),
  p_truth = c(0.507, 0.525, 0.0386, 1.45e-05, 6.43e-14, 2.30e-90),
  stringsAsFactors = FALSE), row.names = FALSE)

cat("\n-- the positive branch: prior mass below a true bot of 4 --\n")
pois <- do.call(rbind, lapply(c(0.92, 0.36, 0.02), rep_row,
                              top = 40, bot = 4,
                              sim = function(mu) as.integer(rpois(length(mu),
                                                                  mu)),
                              family_tag = "poisson", link = "identity"))
print(pois, row.names = FALSE)
cat("\n#386 quotes 0.473 and 0.253 on the complete design, 0.0277 and",
    "6.91e-06\nat 40% maximum effect, and 0.0203 and 4.17e-11 at 11%,",
    "for uninformative\nand regularizing respectively. Its poisson designs",
    "realised different\nmaximum effects from its gaussian ones on the same",
    "three settings.\n")

cat("\n-- the threshold priors --\n")
# #386: an ecx4param design with a true ec50 of 45 against a highest
# concentration of 40, and a nec4param design with a true nec of 60.
thr <- list()
for (spec in list(list(model = "ecx4param", par = "ec50", truth = 45),
                  list(model = "nec4param", par = "nec", truth = 60))) {
  x <- rep(rep_x, each = rep_reps)
  # A gentler rate than the reproduction rows above, so that the
  # ecx4param design shows part of its own decline within the series
  # rather than nothing at all: at 0.13 the response at the highest dose
  # is 0.72 against a top of 0.9.
  mu <- if (spec$model == "ecx4param") {
    mu_ecx4(x, 0.9, 0.1, spec$truth, log(0.13))
  } else {
    mu_nec4(x, 0.9, 0.1, spec$truth, log(0.13))
  }
  y <- rnorm(length(mu), mu, 0.04)
  dat <- data.frame(x = x, y = y)
  for (ptype in c("uninformative", "regularizing")) {
    pr <- as.data.frame(suppressWarnings(suppressMessages(get_priors(
      as.formula(sprintf("y ~ crf(x, \"%s\")", spec$model)), data = dat,
      family = gaussian(), prior_type = ptype))))
    row <- pr[pr$nlpar == spec$par, ]
    ss <- trunc_summary(row$prior[1],
                        suppressWarnings(as.numeric(row$lb[1])),
                        suppressWarnings(as.numeric(row$ub[1])), spec$truth)
    thr[[length(thr) + 1]] <- data.frame(
      model = spec$model, par = spec$par, truth = spec$truth,
      prior_type = ptype, prior = row$prior[1],
      lb = row$lb[1], ub = row$ub[1], p_truth = signif(ss$p_truth, 4),
      stringsAsFactors = FALSE)
  }
}
print(do.call(rbind, thr), row.names = FALSE)
cat("\n#386 quotes a truncated prior CDF at the truth of 1.000 under both",
    "prior sets\nfor both.\n")

cat("\n-- the constant entries --\n")
# The bounded families under `uninformative` on the identity link take
# beta(5, 2) and beta(2, 5) and read nothing from the response, so their
# CDF at the truth is the same on all three designs. Evaluated at the
# top of 0.9 and the bot of 0.1 #386's gaussian designs use.
cat("beta(5, 2) at a true top of 0.9:", signif(pbeta(0.9, 5, 2), 3),
    "  beta(2, 5) at a true bot of 0.1:", signif(pbeta(0.1, 2, 5), 3), "\n")
cat("#386 quotes 0.886 and 0.114.\n")
}

# ======================================================================
# Part 3. The false-positive rate of the flatness rule
# ======================================================================
# The rule is tested one-sided at alpha, so on a design whose top is
# flat the true contrast is zero and the report rate is the
# false-positive rate by construction. That is the quantity #390 states
# a level for, and it is not what the `complete` column of part 1
# measures: a complete design there is still declining at the top, by
# exp(-5) of the span.
#
# The generator follows PR #399: five predictor levels whose mean is
# equal at the two highest, four replicates per level unless a layout
# says otherwise. The mean falls linearly from `top` to `bot` over the
# first four levels and is held at `bot` for the fifth.
if (run_this("calibration") || run_this("miss")) {
null_mu <- function(top, bot) top + (bot - top) * c(0, 1 / 3, 2 / 3, 1, 1)
# A declining top: the fourth level is held `step` of the top-to-bot
# span above `bot` and the fifth is at `bot`, so the contrast tested is
# a fall of `step` and the report rate is the rule's power. 1 - power is
# its miss rate.
#
# The decline is written as a raised fourth level rather than as a
# lowered fifth one because a lowered fifth level leaves the support:
# at a `bot` of 5 and a span of 35 a step of 0.4 puts the mean at -9,
# and rpois() and rbinom() return NA there, which reads as a report rate
# of zero and looks like a miss rather than like a broken generator.
alt_mu <- function(top, bot, step) {
  top + (bot - top) * c(0, 1 / 3, 2 / 3, 1 - step, 1)
}

# One replicate of the rule on a simulated block. `trials` and
# `denominator` are passed through to flatness_blocks() unchanged.
rate_of <- function(n_rep_sim, draw, family, alpha = 0.05,
                    pool_dispersion = TRUE, only = NULL, seed) {
  set.seed(seed)
  reported <- logical(n_rep_sim)
  usable <- logical(n_rep_sim)
  for (i in seq_len(n_rep_sim)) {
    d <- draw()
    r <- flatness_result(d$x, d$y, d$trials, d$denominator, family,
                         alpha = alpha, pool_dispersion = pool_dispersion,
                         only = only)
    usable[i] <- identical(r$status, "tested")
    reported[i] <- isTRUE(r$declining)
  }
  # `rate` is over every replicate, which is the rule's operational
  # rate: a block the rule passes over does not report. `tested` records
  # how often the contrast was computable, so the two can be read apart.
  c(rate = mean(reported), tested = mean(usable), n = n_rep_sim)
}
# A Wilson score interval rather than the Wald one. Several cells below
# report at or near zero, where the Wald interval is [0, 0] and asserts
# a precision the run does not have; Wilson stays inside [0, 1] and is
# well behaved there.
ci_of <- function(rate, n) {
  if (!is.finite(rate) || !is.finite(n) || n < 1) {
    return("[NA, NA]")
  }
  z <- 1.959964
  centre <- (rate + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(rate * (1 - rate) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  sprintf("[%.4f, %.4f]", max(0, centre - half), min(1, centre + half))
}
# A block of `reps` rows at each of the five levels.
level_x <- function(reps) rep(1:5, each = reps)
plain_draw <- function(mu, reps, sim) {
  function() list(x = level_x(reps), y = sim(rep(mu, each = reps)),
                  trials = NULL, denominator = NULL)
}
# A beta-binomial count at intra-class correlation rho. rho = 1/(a+b+1),
# so a + b = 1/rho - 1 and the beta mean is p.
rbetabinom <- function(n, size, prob, rho) {
  s <- 1 / rho - 1
  rbinom(n, size, rbeta(n, prob * s, (1 - prob) * s))
}
binom_draw <- function(mu, reps, trials_per_level, rho = NULL) {
  function() {
    x <- level_x(reps)
    tr <- rep(trials_per_level, length.out = length(x))
    p <- rep(mu, each = reps)
    y <- if (is.null(rho)) {
      rbinom(length(x), tr, p)
    } else {
      rbetabinom(length(x), tr, p, rho)
    }
    list(x = x, y = y, trials = tr, denominator = NULL)
  }
}
# The hurdle survival block. A zero response is a non-survivor, so a
# block of `n_ind` individuals per level is `n_ind` rows, which
# flatness_blocks() reduces to one proportion per level with the count
# of individuals behind it. The survivors' own values are drawn from a
# gamma whose mean does not vary with the level, so that any report on
# the response block is the response block's own false positive and not
# a decline leaking across from the survival curve.
surv_draw <- function(p, n_ind) {
  function() {
    x <- rep(1:5, each = n_ind)
    alive <- rbinom(length(x), 1, rep(p, each = n_ind))
    list(x = x, y = ifelse(alive == 1, rgamma(length(x), 25, 25 / 5), 0),
         trials = NULL, denominator = NULL)
  }
}
# An over-dispersed count. theta 2 against a control mean of 40 puts the
# quadratic term of the negative binomial variance, mu^2 / theta, at 800
# against a linear term of 40, so quasipoisson's linear variance departs
# furthest from the truth exactly where the series starts.
nb_draw <- function(mu, reps, theta) {
  function() {
    x <- level_x(reps)
    list(x = x, y = rnbinom(length(x), mu = rep(mu, each = reps),
                            size = theta),
         trials = NULL, denominator = NULL)
  }
}
g_fam <- validate_family("gaussian")
}

if (run_this("calibration")) {
cat("\n\n=== 3. the false-positive rate on a flat top ===\n")
cat("generator: five levels, the mean equal at the two highest;",
    N_REP_CAL, "replicates\nunless stated.\n\n")
cal <- list()
add_cal <- function(label, res, quoted) {
  cal[[length(cal) + 1]] <<- data.frame(
    block = label, replicates = res[["n"]], rate = round(res[["rate"]], 4),
    ci = ci_of(res[["rate"]], res[["n"]]),
    tested = round(res[["tested"]], 3), quoted_390 = quoted,
    stringsAsFactors = FALSE)
}
add_cal("gaussian, dispersion pooled over the series",
        rate_of(N_REP_GAUSS,
                plain_draw(null_mu(10, 2), 4,
                           function(m) rnorm(length(m), m, 0.6)),
                g_fam, pool_dispersion = TRUE, seed = 3001), 0.0499)
add_cal("gaussian, dispersion from the two levels",
        rate_of(N_REP_GAUSS,
                plain_draw(null_mu(10, 2), 4,
                           function(m) rnorm(length(m), m, 0.6)),
                g_fam, pool_dispersion = FALSE, seed = 3002), 0.0508)
add_cal("Gamma",
        rate_of(N_REP_CAL,
                plain_draw(null_mu(10, 2), 4,
                           function(m) rgamma(length(m), 25, 25 / m)),
                validate_family("Gamma"), seed = 3003), 0.061)
add_cal("poisson",
        rate_of(N_REP_CAL,
                plain_draw(null_mu(40, 5), 4,
                           function(m) rpois(length(m), m)),
                validate_family("poisson"), seed = 3004), 0.049)
add_cal("negbinomial, size 5",
        rate_of(N_REP_CAL,
                plain_draw(null_mu(40, 5), 4,
                           function(m) rnbinom(length(m), mu = m, size = 5)),
                validate_family("negbinomial"), seed = 3005), 0.050)
add_cal("binomial, 20 trials",
        rate_of(N_REP_CAL, binom_draw(null_mu(0.9, 0.2), 4, 20),
                validate_family("binomial"), seed = 3006), 0.053)
add_cal("beta_binomial, 20 trials, rho 0.1",
        rate_of(N_REP_CAL, binom_draw(null_mu(0.9, 0.2), 4, 20, rho = 0.1),
                validate_family("beta_binomial"), seed = 3007), 0.053)
add_cal("Beta",
        rate_of(N_REP_CAL,
                plain_draw(null_mu(0.9, 0.2), 4,
                           function(m) rbeta(length(m), m * 20, (1 - m) * 20)),
                validate_family("Beta"), seed = 3008), 0.059)
add_cal("hurdle survival, 20 individuals",
        rate_of(N_REP_CAL, surv_draw(null_mu(0.95, 0.4), 20),
                validate_family("hurdle_gamma"), only = "survival",
                seed = 3009), 0.055)
add_cal("bernoulli, 4 observations per level",
        rate_of(N_REP_CAL,
                plain_draw(null_mu(0.9, 0.3), 4,
                           function(m) rbinom(length(m), 1, m)),
                validate_family("bernoulli"), seed = 3010), 0.088)
cat("-- the rate by block --\n")
print(do.call(rbind, cal), row.names = FALSE)

cat("\n-- item 3 of #390: bernoulli small-sample discreteness --\n")
cat("#390 reports 0.082 at four observations per level, 0.063 at ten and",
    "0.049 at\ntwenty-five, and states the rate is not monotone in that",
    "count.\n\n")
bern <- list()
for (k in c(1, 2, 4, 10, 25, 50)) {
  r <- rate_of(N_REP_GAUSS,
               plain_draw(null_mu(0.9, 0.3), k,
                          function(m) rbinom(length(m), 1, m)),
               validate_family("bernoulli"), seed = 3100 + k)
  bern[[length(bern) + 1]] <- data.frame(
    observations_per_level = k, replicates = r[["n"]],
    rate = round(r[["rate"]], 4), ci = ci_of(r[["rate"]], r[["n"]]),
    tested = round(r[["tested"]], 3), stringsAsFactors = FALSE)
}
print(do.call(rbind, bern), row.names = FALSE)

cat("\n-- item 2 of #390: quasibinomial where the trials vary --\n")
cat("#390 reports 0.088 at trials of 10, 20, 40 and 80 within a level and",
    "0.137 at\n5, 10, 20 and 100, against 0.054 at constant trials, all at",
    "an intra-class\ncorrelation of 0.1.\n\n")
vt <- list()
vt_specs <- list(list(lab = "constant, 20", tr = rep(20, 4)),
                 list(lab = "10, 20, 40, 80", tr = c(10, 20, 40, 80)),
                 list(lab = "5, 10, 20, 100", tr = c(5, 10, 20, 100)))
for (j in seq_along(vt_specs)) {
  spec <- vt_specs[[j]]
  r <- rate_of(N_REP_CAL,
               binom_draw(null_mu(0.9, 0.2), length(spec$tr), spec$tr,
                          rho = 0.1),
               validate_family("beta_binomial"), seed = 3200 + j)
  vt[[length(vt) + 1]] <- data.frame(
    trials_within_a_level = spec$lab, replicates = r[["n"]],
    rate = round(r[["rate"]], 4), ci = ci_of(r[["rate"]], r[["n"]]),
    stringsAsFactors = FALSE)
}
print(do.call(rbind, vt), row.names = FALSE)

cat("\n-- item 4 of #390: one row per level, fixed-dispersion fallback --\n")
cat("#390 reports 0.058 for binomial and 0.184 for beta_binomial at an",
    "intra-class\ncorrelation of 0.1, over 4000 replicates, against the",
    "nominal 0.05.\n\n")
unrep <- list()
unrep_specs <- list(list(lab = "binomial, 20 trials", fam = "binomial",
                         rho = NULL),
                    list(lab = "beta_binomial, 20 trials, rho 0.05",
                         fam = "beta_binomial", rho = 0.05),
                    list(lab = "beta_binomial, 20 trials, rho 0.1",
                         fam = "beta_binomial", rho = 0.1),
                    list(lab = "beta_binomial, 20 trials, rho 0.2",
                         fam = "beta_binomial", rho = 0.2))
for (j in seq_along(unrep_specs)) {
  spec <- unrep_specs[[j]]
  r <- rate_of(N_REP_CAL,
               binom_draw(null_mu(0.9, 0.2), 1, 20, rho = spec$rho),
               validate_family(spec$fam), seed = 3300 + j)
  unrep[[length(unrep) + 1]] <- data.frame(
    block = spec$lab, replicates = r[["n"]], rate = round(r[["rate"]], 4),
    ci = ci_of(r[["rate"]], r[["n"]]), stringsAsFactors = FALSE)
}
print(do.call(rbind, unrep), row.names = FALSE)
cat("\nWith four replicate rows per level the same generator restores the",
    "dispersion\nestimate:\n")
r <- rate_of(N_REP_CAL, binom_draw(null_mu(0.9, 0.2), 4, 20, rho = 0.1),
             validate_family("beta_binomial"), seed = 3399)
cat("  beta_binomial, 4 rows per level, rho 0.1:", round(r[["rate"]], 4),
    ci_of(r[["rate"]], r[["n"]]), "\n")

# The wrapper is what a user meets, and everything above calls the two
# functions it is built from. Checked here so that the rates reported
# are known to be the rates of the message, not of a private path.
cat("\n-- check_response_flattened() against the helpers --\n")
set.seed(3400)
agree <- logical(200)
for (i in seq_len(200)) {
  x <- rep(1:5, each = 4)
  mu <- if (i %% 2 == 0) null_mu(10, 2) else alt_mu(10, 2, 0.4)
  y <- rnorm(length(x), rep(mu, each = 4), 0.6)
  dat <- data.frame(x = x, y = y)
  sf <- single_model_formula(bayesnecformula(y ~ crf(x, "nec4param")),
                             "nec4param")
  md <- model.frame(sf, data = dat, run_par_checks = FALSE)
  msgs <- character(0)
  withCallingHandlers(
    check_response_flattened(md, g_fam),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  wrapper <- any(grepl("still declining at the top", msgs))
  helper <- isTRUE(flatness_result(x, y, NULL, NULL, g_fam)$declining)
  agree[i] <- identical(wrapper, helper)
}
cat("the wrapper and the helpers agree on", sum(agree), "of 200 blocks\n")
}

# ======================================================================
# Part 4. The miss rate on a top that is still declining
# ======================================================================
if (run_this("miss")) {
cat("\n\n=== 4. the miss rate on a declining top ===\n")
cat("generator: the part 3 series with the fourth level held `step` of the",
    "top-to-bot\nspan above `bot` and the fifth at `bot`, so the contrast",
    "tested is a fall of\n`step`.", N_REP_CAL, "replicates.\n\n")
miss <- list()
add_miss <- function(label, step, res) {
  miss[[length(miss) + 1]] <<- data.frame(
    block = label, step = step, replicates = res[["n"]],
    report_rate = round(res[["rate"]], 4),
    miss_rate = round(1 - res[["rate"]], 4),
    ci = ci_of(res[["rate"]], res[["n"]]), stringsAsFactors = FALSE)
}
steps <- c(0.1, 0.2, 0.3)
for (j in seq_along(steps)) {
  step <- steps[j]
  add_miss("gaussian", step,
           rate_of(N_REP_CAL,
                   plain_draw(alt_mu(10, 2, step), 4,
                              function(m) rnorm(length(m), m, 0.6)),
                   g_fam, seed = 4000 + j))
  add_miss("poisson", step,
           rate_of(N_REP_CAL,
                   plain_draw(alt_mu(40, 5, step), 4,
                              function(m) rpois(length(m), m)),
                   validate_family("poisson"), seed = 4100 + j))
  add_miss("binomial, 20 trials", step,
           rate_of(N_REP_CAL, binom_draw(alt_mu(0.9, 0.2, step), 4, 20),
                   validate_family("binomial"), seed = 4200 + j))
  add_miss("hurdle survival, 20 individuals", step,
           rate_of(N_REP_CAL, surv_draw(alt_mu(0.95, 0.4, step), 20),
                   validate_family("hurdle_gamma"), only = "survival",
                   seed = 4300 + j))
}
print(do.call(rbind, miss), row.names = FALSE)

cat("\n-- the three cells the specification asks for --\n")
cat("An over-dispersed count design, and a beta-binomial design with the",
    "trials\nconstant beside the same design with them varying. The second",
    "pair is\nreported together because the difference between them is the",
    "whole of the\nvarying-trials approximation.\n\n")
probe <- list()
for (j in seq_along(steps)) {
  step <- steps[j]
  r <- rate_of(N_REP_CAL, nb_draw(alt_mu(40, 5, step), 4, 2),
               validate_family("negbinomial"), seed = 4400 + j)
  probe[[length(probe) + 1]] <- data.frame(
    block = "negbinomial, theta 2", step = step, replicates = r[["n"]],
    report_rate = round(r[["rate"]], 4), miss_rate = round(1 - r[["rate"]], 4),
    ci = ci_of(r[["rate"]], r[["n"]]), stringsAsFactors = FALSE)
}
probe_specs <- list(list(lab = "beta_binomial, trials constant at 20",
                         tr = rep(20, 4)),
                    list(lab = "beta_binomial, trials 5, 10, 20, 100",
                         tr = c(5, 10, 20, 100)))
for (j in seq_along(steps)) {
  step <- steps[j]
  for (k in seq_along(probe_specs)) {
    spec <- probe_specs[[k]]
    r <- rate_of(N_REP_CAL,
                 binom_draw(alt_mu(0.9, 0.2, step), length(spec$tr), spec$tr,
                            rho = 0.1),
                 validate_family("beta_binomial"),
                 seed = 4500 + 10 * j + k)
    probe[[length(probe) + 1]] <- data.frame(
      block = spec$lab, step = step, replicates = r[["n"]],
      report_rate = round(r[["rate"]], 4),
      miss_rate = round(1 - r[["rate"]], 4),
      ci = ci_of(r[["rate"]], r[["n"]]), stringsAsFactors = FALSE)
  }
}
print(do.call(rbind, probe), row.names = FALSE)

cat("\n-- the count cell against MASS::glm.nb --\n")
# Run once for the comparison and not as a dependency. If the quasi rule
# misses designs glm.nb reports, that is what would justify adding MASS
# to Suggests; if it does not, it is what closes the question.
if (!requireNamespace("MASS", quietly = TRUE)) {
  cat("MASS is not installed; the comparison was not run.\n")
} else {
  cat("MASS", as.character(packageVersion("MASS")),
      "- the same blocks, tested by a likelihood ratio on a",
      "glm.nb\nfit of the two levels, one-sided by the sign of the",
      "contrast.\n\n")
  nb_contrast <- function(x, y, alpha = 0.05) {
    ux <- sort(unique(x))
    keep <- x %in% ux[length(ux) - 1:0]
    d <- data.frame(y = y[keep], level = factor(x[keep]))
    full <- try(suppressWarnings(MASS::glm.nb(y ~ level, data = d)),
                silent = TRUE)
    null <- try(suppressWarnings(MASS::glm.nb(y ~ 1, data = d)), silent = TRUE)
    if (inherits(full, "try-error") || inherits(null, "try-error") ||
          !isTRUE(full$converged) || !isTRUE(null$converged)) {
      return(c(report = NA, ok = 0))
    }
    # The likelihood ratio and not the deviance difference. glm.nb
    # estimates a theta for each fit, so the two deviances are computed
    # under different variance functions and their difference is not a
    # likelihood ratio: on the flat-top null it reported on 0.000 of 200
    # blocks against a nominal 0.05, which is a broken statistic rather
    # than a conservative one.
    stat <- 2 * as.numeric(logLik(full) - logLik(null))
    if (!is.finite(stat)) return(c(report = NA, ok = 0))
    two <- pchisq(max(stat, 0), df = 1, lower.tail = FALSE)
    est <- coef(full)[2]
    p <- if (is.finite(est) && est < 0) two / 2 else 1 - two / 2
    c(report = as.numeric(p < alpha), ok = 1)
  }
  # glm.nb is roughly twenty times the cost of the quasipoisson contrast,
  # so the comparison runs on a tenth of the replicates. The interval is
  # reported with it.
  n_nb <- max(200L, N_REP_CAL %/% 10L)
  nbcmp <- list()
  nb_settings <- list(list(lab = "flat top (null)", mu = null_mu(40, 5)),
                      list(lab = "step 0.1", mu = alt_mu(40, 5, 0.1)),
                      list(lab = "step 0.2", mu = alt_mu(40, 5, 0.2)),
                      list(lab = "step 0.3", mu = alt_mu(40, 5, 0.3)))
  for (j in seq_along(nb_settings)) {
    setting <- nb_settings[[j]]
    set.seed(4600 + j)
    rule <- logical(n_nb)
    nb <- rep(NA_real_, n_nb)
    ok <- logical(n_nb)
    for (i in seq_len(n_nb)) {
      x <- level_x(4)
      y <- rnbinom(length(x), mu = rep(setting$mu, each = 4), size = 2)
      rule[i] <- isTRUE(flatness_result(x, y, NULL, NULL,
                                        validate_family("negbinomial"))$
                          declining)
      z <- nb_contrast(x, y)
      nb[i] <- z[["report"]]
      ok[i] <- z[["ok"]] == 1
    }
    nbcmp[[length(nbcmp) + 1]] <- data.frame(
      setting = setting$lab, replicates = n_nb,
      quasipoisson_rate = round(mean(rule), 4),
      quasipoisson_ci = ci_of(mean(rule), n_nb),
      glm_nb_rate = round(mean(nb[ok]), 4),
      glm_nb_ci = ci_of(mean(nb[ok]), sum(ok)),
      glm_nb_converged = round(mean(ok), 3),
      missed_by_rule_reported_by_nb = round(mean(!rule[ok] & nb[ok] == 1), 4),
      stringsAsFactors = FALSE)
  }
  print(do.call(rbind, nbcmp), row.names = FALSE)
}
}
