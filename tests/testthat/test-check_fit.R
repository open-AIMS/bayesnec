# Check variability by predictor group: a global dispersion statistic can
# miss opposing discrepancies in different regions.

# Cache diagnostic tables only for read-only tests; a test that changes
# one must build its own. Apply skip guards before requesting these
# expensive fixtures, which are built lazily on the first request.
nec4param_checkfit_g4 <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- check_fit(nec4param, group = 4, ndraws = 50)
    }
    cached
  }
})

test_that("check_fit returns a row per group with both statistics", {
  skip_on_cran()
  out <- nec4param_checkfit_g4()
  expect_s3_class(out, "checkfit")
  expect_equal(nrow(out), 4)
  for (nm in c("group", "n", "obs_mean", "sim_mean", "mean_ratio", "ppp_mean",
               "obs_sd", "sim_sd", "sd_ratio", "ppp_sd", "control")) {
    expect_true(nm %in% names(out), info = nm)
  }
  # every observation lands in exactly one group
  expect_equal(sum(out$n), nrow(nec4param$fit$data))
})

test_that("exactly one group is flagged as the control, and it is the lowest", {
  # nsec() reads its reference from the fitted curve at min(x), and the
  # observed control is y[x == min(x)] -- the package's own convention. The
  # flag has to agree with that or it points at the wrong row.
  skip_on_cran()
  out <- nec4param_checkfit_g4()
  expect_equal(sum(out$control), 1)
  expect_true(out$control[1])
})

test_that("posterior predictive p-values are probabilities", {
  skip_on_cran()
  out <- nec4param_checkfit_g4()
  for (nm in c("ppp_mean", "ppp_sd")) {
    expect_true(all(out[[nm]] >= 0 & out[[nm]] <= 1, na.rm = TRUE), info = nm)
  }
})

test_that("ndraws is reduced to what the fit holds rather than erroring", {
  # brms refuses excess ndraws instead of truncating. Cap requests at the
  # 100 draws in this stored fixture.
  skip_on_cran()
  expect_no_error(check_fit(nec4param, group = 3, ndraws = 1e6))
})

test_that("an unreplicated predictor is binned, with a warning", {
  # Unreplicated data need binning, which can hide local structure. The
  # diagnostic must warn that bins are not replicated design points.
  skip_on_cran()
  expect_warning(check_fit(nec4param, ndraws = 50), "not a design point")
})

test_that("a replicated predictor is grouped by its distinct values", {
  # No warning, and one group per level: the design supplies the grouping.
  x <- rep(c(0, 1, 2, 3), each = 5)
  expect_silent(g <- bayesnec:::check_fit_groups(x))
  expect_equal(nlevels(g), 4)
  expect_equal(as.numeric(levels(g)), c(0, 1, 2, 3))
})

test_that("an explicit group vector is used as given", {
  x <- seq_len(12)
  g <- bayesnec:::check_fit_groups(x, rep(c("a", "b", "c"), each = 4))
  expect_equal(nlevels(g), 3)
  expect_equal(as.character(g[1]), "a")
})

test_that("ppp_value is the tail probability, and handles degenerate input", {
  expect_equal(bayesnec:::ppp_value(c(1, 2, 3, 4), 2.5), 0.5)
  expect_equal(bayesnec:::ppp_value(c(1, 2, 3, 4), 0), 1)
  expect_equal(bayesnec:::ppp_value(c(1, 2, 3, 4), 99), 0)
  # a statistic that could not be computed must not become a silent 0 or 1
  expect_true(is.na(bayesnec:::ppp_value(numeric(0), 1)))
  expect_true(is.na(bayesnec:::ppp_value(c(1, 2), NA_real_)))
  expect_equal(bayesnec:::ppp_value(c(1, NA, 3), 2), 0.5)
})

test_that("a bayesmanecfit reports per-model rows carrying their weights", {
  # Global model weights do not guarantee a good control fit. Keep per-model
  # rows so the control diagnostic can be read against each model's weight.
  skip_on_cran()
  out <- check_fit(manec_example, group = 3, ndraws = 50)
  expect_true(all(c("model", "wi") %in% names(out)))
  expect_setequal(unique(out$model), names(manec_example$mod_fits))
  expect_equal(nrow(out), 3 * length(manec_example$mod_fits))
  # the weights must be the object's own, not recomputed
  for (m in unique(out$model)) {
    expect_equal(unique(out$wi[out$model == m]),
                 manec_example$mod_stats[m, "wi"])
  }
})

test_that("check_fit reproduces the local finding a global statistic misses", {
  # This fixture can pass a global dispersion check while overstating
  # control variability. Check that the regional diagnostic detects it.
  skip_on_cran()
  out <- suppressWarnings(check_fit(nec4param, ndraws = 200, seed = 10))
  ctrl <- out[out$control, ]
  expect_lt(ctrl$sd_ratio, 0.95)
  # and the steep tail fails the other way, which a single global number
  # averages away entirely
  expect_gt(max(out$sd_ratio), 1.2)
})

# Keep separate location and scale panels: either discrepancy can occur
# without the other.

# Cache the table for read-only plot tests. Test warning emission through
# a fresh check_fit() call: a cached accessor warns only on its first call.
manec_checkfit <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- suppressWarnings(check_fit(manec_example))
    }
    cached
  }
})

test_that("plot.checkfit returns a ggplot with both statistics panelled", {
  skip_on_cran()
  cf <- manec_checkfit()
  p <- plot(cf)
  expect_s3_class(p, "ggplot")
  # both panels present -- location and scale fail independently, so a single
  # combined panel would hide exactly the case check_fit() exists to catch
  expect_setequal(unique(p$data$statistic),
                  c("location (mean)", "scale (residual SD)"))
  # two rows per group, one per statistic
  expect_equal(nrow(p$data), 2 * nrow(as.data.frame(cf)))
})

test_that("the control is distinguished in the plot data", {
  skip_on_cran()
  cf <- manec_checkfit()
  p <- plot(cf)
  expect_true("control" %in% p$data$role)
  expect_true("exposed" %in% p$data$role)
  # One control group per candidate model, appearing once in each of the two
  # statistic panels. manec_example is a bayesmanecfit, so this is 2 * n_models
  # rather than 2 -- the per-model rows are the point of the manec method.
  n_models <- length(unique(as.data.frame(cf)$model))
  expect_equal(sum(p$data$role == "control"), 2 * n_models)
})

test_that("the simulated intervals are on the object but not printed", {
  skip_on_cran()
  cf <- manec_checkfit()
  expect_true(all(c("sim_mean_lo", "sim_mean_hi", "sim_sd_lo", "sim_sd_hi")
                  %in% names(as.data.frame(cf))))
  # print() drops them: they are for plot(), and including them takes the
  # console table past a readable width
  printed <- capture.output(print(cf))
  expect_false(any(grepl("sim_mean_lo|sim_sd_hi", printed)))
})

test_that("the interval brackets the simulated median", {
  skip_on_cran()
  d <- as.data.frame(manec_checkfit())
  expect_true(all(d$sim_mean_lo <= d$sim_mean & d$sim_mean <= d$sim_mean_hi))
  expect_true(all(d$sim_sd_lo <= d$sim_sd & d$sim_sd <= d$sim_sd_hi))
})

# Compare the combined hurdle response, including zeros. Component checks
# on their own subsets cannot test the observed response distribution.

# Cache this seeded hurdle fit for read-only tests to avoid repeatedly
# compiling its two Stan models.
hurdle_fixture <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      set.seed(17)
      x <- rep(seq(0, 5, length.out = 15), each = 6)
      p_alive <- 1 / (1 + exp(-(2.5 - 0.9 * x)))
      alive <- rbinom(length(x), 1, p_alive)
      g <- rgamma(length(x), shape = 4, rate = 4 / pmax(3 - 0.35 * x, 0.3))
      dat <- data.frame(x = x, y = ifelse(alive == 1, g, 0))
      fit <- suppressWarnings(suppressMessages(
        bnec_hurdle(y ~ crf(x, "nec3param"), data = dat, iter = 400,
                    warmup = 200, chains = 2, seed = 17, refresh = 0)
      ))
      cached <<- list(dat = dat, fit = fit)
    }
    cached
  }
})

test_that("the observed response is reconstructed exactly, zeros included", {
  # The growth response must retain the order of surviving rows so it can be
  # inserted at the survival fit's alive positions.
  skip_on_cran()
  f <- hurdle_fixture()
  y_obs <- bayesnec:::hurdle_observed_response(f$fit)
  expect_length(y_obs, nrow(f$dat))
  expect_equal(sort(y_obs), sort(f$dat$y))
  expect_equal(mean(y_obs == 0), mean(f$dat$y == 0))
  # and the two components really are on different row counts, which is why
  # the growth fit cannot be predicted from its own data here
  expect_gt(nrow(f$fit$survival$fit$data), nrow(f$fit$growth$fit$data))
})

test_that("check_fit on a hurdle fit returns all three components", {
  skip_on_cran()
  res <- suppressWarnings(check_fit(hurdle_fixture()$fit))
  expect_named(res, c("growth", "survival", "combined"))
  expect_s3_class(res$combined, "checkfit")
})

test_that("the combined table reports the zero fraction", {
  # The whole reason the combined view exists: whether the fit reproduces the
  # point mass at zero is not visible in either component alone.
  skip_on_cran()
  res <- suppressWarnings(check_fit(hurdle_fixture()$fit))
  expect_true(all(c("obs_zero", "sim_zero", "ppp_zero") %in%
                    names(as.data.frame(res$combined))))
  d <- as.data.frame(res$combined)
  expect_true(all(d$obs_zero >= 0 & d$obs_zero <= 1))
  expect_true(all(d$sim_zero >= 0 & d$sim_zero <= 1))
})

test_that("combined = FALSE skips it", {
  skip_on_cran()
  res <- suppressWarnings(check_fit(hurdle_fixture()$fit, combined = FALSE))
  expect_named(res, c("growth", "survival"))
})

test_that("the combined simulation is on the full exposed set", {
  # Predicting growth from its own model frame would give a vector the length
  # of the survivors, which cannot be multiplied against the survival draws.
  # Asserted through the table's group sizes summing to every exposed row.
  skip_on_cran()
  f <- hurdle_fixture()
  res <- suppressWarnings(check_fit(f$fit))
  expect_equal(sum(as.data.frame(res$combined)$n), nrow(f$dat))
})

# Threshold the automatic control message on ratios. A posterior
# predictive p-value can fail to flag a control discrepancy.

test_that("summary carries both diagnostic axes and their cutoffs", {
  skip_on_cran()
  s <- suppressWarnings(summary(manec_example))
  expect_true(all(c("rhat_issues", "rhat_cutoff",
                    "fit_issues", "fit_ratio_cutoff") %in% names(s)))
  expect_equal(s$rhat_cutoff, 1.01)
  expect_equal(s$fit_ratio_cutoff, 1.15)
  expect_type(unlist(s$fit_issues), "logical")
})

test_that("the fit axis can be switched off", {
  # Recompute the diagnostic on each call; do not cache prediction matrices
  # on the fitted object.
  skip_on_cran()
  s <- suppressWarnings(summary(manec_example, check_fit = FALSE))
  expect_null(s$fit_issues)
})

test_that("the cutoff is honoured rather than decorative", {
  skip_on_cran()
  # nothing can be beyond a ratio cutoff of infinity
  s <- suppressWarnings(summary(manec_example, fit_ratio_cutoff = Inf))
  expect_false(any(unlist(s$fit_issues)))
  # This fixture has ratios different from one, so at least one is flagged
  # when the cutoff admits only exact agreement.
  s2 <- suppressWarnings(summary(manec_example, fit_ratio_cutoff = 1))
  expect_true(any(unlist(s2$fit_issues)))
})

test_that("print reports both axes in one block, with different advice", {
  # Report sampler failure and control misfit together but give different
  # advice: a converged model's control discrepancy is a modelling result.
  skip_on_cran()
  s <- suppressWarnings(summary(manec_example))
  msg <- tryCatch({ print(s); "" }, warning = function(w) conditionMessage(w))
  expect_match(msg, "Rhat > 1.01")
  expect_match(msg, "control observed/simulated ratio beyond 1.15")
  expect_match(msg, "result, not a fault")
})

test_that("the end-of-fit message never breaks a fit that succeeded", {
  # A diagnostic failure must not invalidate an otherwise usable fit.
  expect_silent(bayesnec:::message_control_fit(list(not = "a fit")))
  expect_null(bayesnec:::message_control_fit(1:10))
})

# Diagnostics must restore the caller's random stream after seeded
# posterior prediction.

test_that("check_fit leaves the caller's RNG stream alone", {
  skip_on_cran()
  # Build the cached table before setting the stream under test.
  nec4param_checkfit_g4()
  set.seed(99)
  expected <- runif(3)
  set.seed(99)
  first <- runif(1)
  check_fit(nec4param, group = 4, ndraws = 50)
  expect_equal(c(first, runif(2)), expected)
})

test_that("check_fit on a model set leaves the caller's RNG stream alone", {
  # The bayesmanecfit method loops over pull_out(), so the restore has to hold
  # for a fit inside a set as well as for a single fit.
  skip_on_cran()
  suppressMessages(check_fit(manec_example, group = 4, ndraws = 50))
  set.seed(99)
  expected <- runif(3)
  set.seed(99)
  first <- runif(1)
  suppressMessages(check_fit(manec_example, group = 4, ndraws = 50))
  expect_equal(c(first, runif(2)), expected)
})

test_that("check_fit still repeats itself under the same seed", {
  # The restore must not be mistaken for the diagnostic having stopped being
  # reproducible: the seed is still applied, only the stream is put back.
  skip_on_cran()
  set.seed(1)
  a <- check_fit(nec4param, group = 4, ndraws = 50)
  runif(5)
  b <- check_fit(nec4param, group = 4, ndraws = 50)
  # The whole table rather than one column: every simulated quantity has to
  # repeat, not only the one the restore was most likely to disturb.
  expect_equal(a, b)
})

test_that("check_fit refuses a seed set.seed() would take silently", {
  # Refuse NULL: set.seed(NULL) would reseed from the clock and make the
  # diagnostic irreproducible.
  skip_on_cran()
  expect_error(check_fit(nec4param, seed = NULL), "seed")
  # NA means no seed on some fitting routes, but this diagnostic requires a
  # number and must refuse it by argument name.
  expect_error(check_fit(nec4param, seed = NA), "must be a number")
  expect_error(check_fit(nec4param, seed = "ten"), "seed")
  expect_error(check_fit(nec4param, seed = c(1, 2)), "seed")
  expect_error(suppressMessages(check_fit(manec_example, seed = NULL)), "seed")
})

test_that("check_fit gives a growth disp(~z) column each row's value (#443)", {
  # check_fit_combined() simulates the growth component over the survival
  # component's fitted data. That frame is built here as bnec_hurdle() builds
  # it, but with brm(empty = TRUE), so nothing is compiled or sampled.
  set.seed(443)
  d <- data.frame(x = rep(c(0.1, 1, 3, 10), each = 6),
                  z = factor(rep(c("a", "b", "c"), 8)))
  d$y <- stats::rgamma(24, 5, 5 / (2 * exp(-0.2 * d$x)))
  d$y[d$x == 10][1:3] <- 0
  f <- bnf(y ~ crf(x, "nec3param") + disp(~z))
  surv_f <- bayesnec:::swap_response(bayesnec:::drop_disp_term(f), ".alive")
  surv_d <- d
  surv_d$.alive <- as.integer(d$y > 0)
  fam <- brms::bernoulli(link = "identity")
  bf <- bayesnec:::wrangle_model_formula(
    "nec3param", surv_f, model.frame(surv_f, data = surv_d), family = fam
  )
  sfit <- suppressMessages(brms::brm(
    bf, data = surv_d, family = fam,
    prior = get_priors(surv_f, data = surv_d, family = fam), empty = TRUE
  ))
  # the frame lacks z, because the survival formula has no disp() term
  expect_false("z" %in% names(sfit$data))
  rows <- bayesnec:::hurdle_growth_rows(list(formula = f, data = d),
                                        sfit$data)
  expect_identical(rows$z, d$z)
  expect_identical(rows$x, sfit$data$x)
  expect_identical(rows$.alive, sfit$data$.alive)
  # unchanged where the growth formula names no such column
  for (g in list(bnf(y ~ crf(x, "nec3param")),
                 bnf(y ~ crf(x, "nec3param") + disp(~x)))) {
    expect_identical(
      bayesnec:::hurdle_growth_rows(list(formula = g, data = d), sfit$data),
      sfit$data
    )
  }
  # rows that cannot be matched are refused rather than misaligned
  expect_error(
    bayesnec:::hurdle_growth_rows(list(formula = f, data = d[-1, ]),
                                  sfit$data),
    "cannot be matched"
  )
  # A level present only among non-survivors has no fitted growth dispersion.
  expect_error(
    bayesnec:::hurdle_growth_rows(list(formula = f, data = d), sfit$data,
                                  growth_data = d[d$z != "c", ]),
    'column "z" at value "c": no surviving response'
  )
})
