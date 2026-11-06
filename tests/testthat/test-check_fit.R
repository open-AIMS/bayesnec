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
  # brms refuses an ndraws above the draws a fit holds, so check_fit() caps
  # it. The packaged fits hold 100 draws; the 1e6 below is deliberate.
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

test_that("a singleton group retains its mean statistics", {
  x <- c(0, 0, 0.03, 0.03, 0.09, 0.29, 0.29)
  y <- c(0.95, 1.38, 1.18, 1.40, 0.81, 0.94, 0.96)
  yrep <- rbind(y, y + 0.1, y - 0.1)
  mu <- rep(1, length(y))
  expect_warning(grp <- bayesnec:::check_fit_groups(x), "cut into 3 bins")
  out <- bayesnec:::check_fit_stats(y, yrep, mu, grp, FALSE)
  one <- out[out$n == 1L, , drop = FALSE]
  expect_equal(nrow(one), 1L)
  expect_true(all(is.na(one[c("obs_sd", "sim_sd", "sd_ratio", "ppp_sd",
                              "sim_sd_lo", "sim_sd_hi")])))
  expect_true(all(is.finite(unlist(one[c("obs_mean", "sim_mean",
                                          "mean_ratio", "ppp_mean",
                                          "sim_mean_lo", "sim_mean_hi")]))))
  sd_cols <- c("obs_sd", "sim_sd", "sd_ratio", "ppp_sd", "sim_sd_lo",
               "sim_sd_hi")
  expect_true(all(is.finite(unlist(out[out$n > 1L, sd_cols]))))

  explicit <- bayesnec:::check_fit_groups(x, x)
  by_concentration <- bayesnec:::check_fit_stats(y, yrep, mu, explicit, FALSE)
  expect_equal(by_concentration$n, c(2L, 2L, 1L, 2L))
  expect_true(is.na(by_concentration$sim_sd[3]))
  expect_true(is.finite(by_concentration$sim_mean[3]))

  out$control <- out$group == out$group[1]
  class(out) <- c("checkfit", "data.frame")
  expect_output(print(out), "NA")
  p <- plot(out)
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 2L * nrow(out) - 1L)
  expect_equal(sum(p$data$statistic == "location (mean)"), nrow(out))
  expect_warning(ggplot2::ggplot_build(p), NA)

  by_model <- rbind(out, out)
  by_model$model <- rep(c("a", "b"), each = nrow(out))
  class(by_model) <- c("checkfit", "data.frame")
  model_plot <- plot(by_model)
  expect_equal(nrow(model_plot$data), 2L * nrow(by_model) - 2L)
  expect_equal(as.integer(table(model_plot$data$model)),
               rep(2L * nrow(out) - 1L, 2L))
  location <- model_plot$data$statistic == "location (mean)"
  expect_equal(model_plot$data$model[location], by_model$model)
  expect_equal(model_plot$data$model[!location],
               by_model$model[is.finite(by_model$obs_sd)])
  expect_warning(ggplot2::ggplot_build(model_plot), NA)
})

test_that("check_fit accepts an explicit singleton group", {
  skip_on_cran()
  group <- rep(0, nrow(nec4param$fit$data))
  group[1] <- 1
  out <- check_fit(nec4param, group = group, ndraws = 50)
  expect_s3_class(out, "checkfit")
  expect_equal(sum(out$n), length(group))
  expect_true(is.na(out$sim_sd[out$n == 1L]))
  expect_true(is.finite(out$sim_mean[out$n == 1L]))
  expect_warning(ggplot2::ggplot_build(plot(out)), NA)
})

test_that("ppp_value splits ties and handles degenerate input", {
  expect_equal(bayesnec:::ppp_value(c(1, 2, 3, 4), 2.5), 0.5)
  expect_equal(bayesnec:::ppp_value(c(1, 2, 3, 4), 0), 1)
  expect_equal(bayesnec:::ppp_value(c(1, 2, 3, 4), 99), 0)
  expect_equal(bayesnec:::ppp_value(rep(0, 4), 0), 0.5)
  expect_equal(bayesnec:::ppp_value(c(0, 0, 1, 1), 0), 0.75)
  expect_equal(bayesnec:::ppp_value(10 + 1e-14, 10), 0.5)
  expect_equal(bayesnec:::ppp_value(10 - 1e-14, 10), 0.5)
  expect_equal(bayesnec:::ppp_value(10 + 1e-10, 10), 1)
  expect_equal(bayesnec:::ppp_value(10 - 1e-10, 10), 0)
  # a statistic that could not be computed must not become a silent 0 or 1
  expect_true(is.na(bayesnec:::ppp_value(numeric(0), 1)))
  expect_true(is.na(bayesnec:::ppp_value(c(1, 2), NA_real_)))
  expect_equal(bayesnec:::ppp_value(c(1, NA, 3), 2), 0.5)
})

test_that("identical binomial counts give a mid-p for mean and SD", {
  y <- rep(10, 3)
  yrep <- matrix(10, nrow = 20, ncol = length(y))
  mu <- rep(10, length(y))
  out <- bayesnec:::check_fit_stats(y, yrep, mu, factor(rep(0, 3)), FALSE)
  expect_equal(out$ppp_mean, 0.5)
  expect_equal(out$ppp_sd, 0.5)
  expect_equal(out$sd_ratio, 1)
})

test_that("identical zero counts do not flag the control", {
  y <- rep(0, 3)
  yrep <- matrix(0, nrow = 20, ncol = length(y))
  out <- bayesnec:::check_fit_stats(y, yrep, y, factor(rep(0, 3)), FALSE)
  expect_equal(c(out$ppp_mean, out$ppp_sd), c(0.5, 0.5))
  expect_equal(c(out$mean_ratio, out$sd_ratio), c(1, 1))
  out$control <- TRUE
  class(out) <- c("checkfit", "data.frame")
  local_mocked_bindings(check_fit = function(x, ...) out)
  expect_identical(control_fit_issues(list(success_models = "ecx4param"),
                                      cutoff = 1.15),
                   list(ecx4param = FALSE))
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

# Cache the table for read-only plot tests. The accessor suppresses
# check_fit()'s warnings, so test warnings through a fresh call.
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

grouped_checkfit_fixture <- function() {
  a <- nec4param
  b <- manec_example
  a_data <- a$fit$data
  b_data <- b$mod_fits[[1L]]$fit$data
  # Only the grouping column is needed from the original input here. The
  # component fits retain their own response and predictor data.
  site <- c(rep("a", nrow(a_data)), rep("b", nrow(b_data)))
  site <- site[order(c(seq_len(nrow(a_data)) * 2L - 1L,
                       seq_len(nrow(b_data)) * 2L))]
  data <- data.frame(site = site)
  structure(list(fits = list(a = a, b = b), levels = c("a", "b"),
                 group_var = "site", data = data),
            class = c("bayesnecgroupfit", "bnecfit"))
}

test_that("grouped checks equal separate component calls", {
  skip_on_cran()
  fit <- grouped_checkfit_fixture()
  # This dispatch fixture has a different dose grid at level b. Its fitted
  # parameters are unchanged, so it is not evidence about model fit.
  fit$fits$b$mod_fits <- lapply(fit$fits$b$mod_fits, function(model) {
    model$fit$data$x <- model$fit$data$x + 0.5
    model
  })
  expect_false(identical(min(fit$fits$a$fit$data$x),
                         min(fit$fits$b$mod_fits[[1L]]$fit$data$x)))
  out <- check_fit(fit, group = 3, ndraws = 20, seed = 19)
  expect_s3_class(out, "checkfit_group")
  expect_named(out$fits, fit$levels)
  expect_identical(out$fits$a,
                   check_fit(fit$fits$a, group = 3, ndraws = 20, seed = 19))
  expect_identical(out$fits$b,
                   check_fit(fit$fits$b, group = 3, ndraws = 20, seed = 19))
  expect_equal(sum(out$fits$a$control), 1)
  expect_true(all(tapply(out$fits$b$control, out$fits$b$model, sum) == 1L))
  expect_true(all(c("model", "wi", "control") %in% names(out$fits$b)))
})

test_that("full-input groups are split in original row order", {
  skip_on_cran()
  fit <- grouped_checkfit_fixture()
  n_a <- nrow(fit$fits$a$fit$data)
  n_b <- nrow(fit$fits$b$mod_fits[[1L]]$fit$data)
  grouping <- character(n_a + n_b)
  grouping[fit$data$site == "a"] <- rep(c("low", "high"), length.out = n_a)
  grouping[fit$data$site == "b"] <- rep(c("early", "late"), length.out = n_b)
  out <- check_fit(fit, group = grouping, ndraws = 20)
  expect_identical(out$fits$a,
                   check_fit(fit$fits$a, group = grouping[fit$data$site == "a"],
                             ndraws = 20))
  expect_identical(out$fits$b,
                   check_fit(fit$fits$b, group = grouping[fit$data$site == "b"],
                             ndraws = 20))
  expect_error(check_fit(fit, group = grouping[-1L], ndraws = 20),
               "original order")
  grouping[1L] <- NA_character_
  expect_error(check_fit(fit, group = grouping, ndraws = 20),
               "non-missing")
})

test_that("grouped results retain component attributes and label displays", {
  skip_on_cran()
  fit <- grouped_checkfit_fixture()
  out <- check_fit(fit, group = 3, ndraws = 20)
  printed <- paste(capture.output(print(out)), collapse = "\n")
  expect_match(printed, "site = a", fixed = TRUE)
  expect_match(printed, "site = b", fixed = TRUE)
  plots <- plot(out)
  expect_named(plots, c("a", "b"))
  expect_s3_class(plots$a, "ggplot")
  expect_identical(plots$a$labels$title, "site: a")
  expect_identical(plots$b$labels$title, "site: b")
  expect_identical(out$fits$b$wi,
                   check_fit(fit$fits$b, group = 3, ndraws = 20)$wi)
})

test_that("grouped dispatch leaves a component's Pareto attribute intact", {
  fit <- grouped_checkfit_fixture()
  seen <- list()
  local_mocked_bindings(
    check_fit = function(x, group, ndraws, seed, ...) {
      seen[[length(seen) + 1L]] <<- list(group = group, ndraws = ndraws,
                                          seed = seed)
      out <- data.frame(group = "control", control = TRUE)
      attr(out, "pareto") <- data.frame(model = "nec4param", max_k = 0.8)
      bayesnec:::allot_class(out, c("checkfit", "data.frame"))
    },
    .package = "bayesnec"
  )
  out <- bayesnec:::check_fit.bayesnecgroupfit(fit, ndraws = 7, seed = 19)
  expect_identical(attr(out$fits$a, "pareto")$max_k, 0.8)
  expect_identical(attr(out$fits$b, "pareto")$max_k, 0.8)
  expect_true(all(vapply(seen, function(z) is.null(z$group), logical(1))))
  expect_true(all(vapply(seen, function(z) z$ndraws == 7 && z$seed == 19,
                         logical(1))))
  seen <- list()
  bayesnec:::check_fit.bayesnecgroupfit(fit, group = 3)
  expect_identical(lapply(seen, `[[`, "group"), list(3, 3))
  seen <- list()
  expect_error(bayesnec:::check_fit.bayesnecgroupfit(fit, group = 1:3),
               "original order")
  expect_length(seen, 0)
  full_group <- rep("g", nrow(fit$data))
  fit$fits$b$mod_fits[[2L]]$fit$data <-
    fit$fits$b$mod_fits[[2L]]$fit$data[-1L, , drop = FALSE]
  expect_error(bayesnec:::check_fit.bayesnecgroupfit(fit, group = full_group),
               "do not match its fitted data")
  expect_length(seen, 0)
})

test_that("grouped checks restore the caller's random stream", {
  skip_on_cran()
  fit <- grouped_checkfit_fixture()
  set.seed(562)
  expected <- runif(3)
  set.seed(562)
  first <- runif(1)
  out <- check_fit(fit, group = 3, ndraws = 20, seed = 17)
  expect_identical(c(first, runif(2)), expected)
  expect_identical(out, check_fit(fit, group = 3, ndraws = 20, seed = 17))
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

test_that("control_fit_issues() compares only the mean of a control of one", {
  ratios <- function(mean_ratio) {
    function(x, ...) {
      data.frame(group = c("0", "1"), n = c(1L, 3L), control = c(TRUE, FALSE),
                 mean_ratio = c(mean_ratio, 1.5), sd_ratio = c(NA, 1.1))
    }
  }
  x <- list(success_models = "nec3param")
  local_mocked_bindings(check_fit = ratios(1.02))
  expect_identical(control_fit_issues(x, cutoff = 1.15),
                   list(nec3param = FALSE))
  local_mocked_bindings(check_fit = ratios(1.30))
  expect_identical(control_fit_issues(x, cutoff = 1.15),
                   list(nec3param = TRUE))
})

# A censored reading is recorded at its bound, while posterior_predict() does
# not censor. Hold both sides at the group's bound before comparing them.

gamma_draws <- function(mu, n, shape = 8) {
  matrix(stats::rgamma(n * length(mu), shape, shape / rep(mu, each = n)),
         nrow = n)
}

test_that("a censored group simulated from the model is not flagged", {
  # Each replicate draws the data from the distribution its simulations come
  # from, then censors it, so a correct check flags about one in ten.
  set.seed(566)
  mu <- rep(c(1, 0.3, 0.12, 0.08, 0.03), each = 12)
  grp <- factor(rep(seq_len(5), each = 12))
  floor <- 0.1
  out <- replicate(200, {
    yrep <- gamma_draws(mu, 100)
    y_true <- gamma_draws(mu, 1)[1, ]
    code <- ifelse(y_true < floor, -1L, 0L)
    y <- pmax(y_true, floor)
    cens <- list(code = code, bound = y, may_be_zero = code == -1L)
    held <- bayesnec:::check_fit_stats(y, yrep, mu, grp, FALSE, cens)
    raw <- bayesnec:::check_fit_stats(y, yrep, mu, grp, FALSE)
    c(held$ppp_mean[3:5], held$ppp_sd[3:5], raw$ppp_mean[4:5],
      held$n_cens[3:5])
  })
  flagged <- rowMeans(out[1:8, ] < 0.05 | out[1:8, ] > 0.95)
  # Groups 3 to 5 are partly, mostly and wholly censored.
  expect_true(all(rowMeans(out[9:11, ]) > c(2, 8, 11.9)))
  expect_true(all(flagged[1:6] < 0.2))
  expect_true(all(abs(rowMeans(out[1:6, ]) - 0.5) < 0.1))
  # Without the hold, the mean of the two most censored groups is flagged in
  # nearly every replicate.
  expect_true(all(flagged[7:8] > 0.9))
})

test_that("values tied at the bound split the p-value evenly", {
  y <- rep(0.1, 4)
  yrep <- matrix(0.01, nrow = 10, ncol = 4)
  mu <- c(0.02, 0.03, 0.02, 0.03)
  grp <- factor(rep("a", 4))
  cens <- list(code = rep(-1L, 4), bound = y, may_be_zero = rep(TRUE, 4))
  out <- bayesnec:::check_fit_stats(y, yrep, mu, grp, FALSE, cens)
  expect_equal(out$n_cens, 4L)
  expect_equal(c(out$obs_mean, out$sim_mean), c(0.1, 0.1))
  expect_equal(c(out$ppp_mean, out$ppp_sd), c(0.5, 0.5))
  # Without the hold, readings recorded at their bound are flagged.
  raw <- bayesnec:::check_fit_stats(y, yrep, mu, grp, FALSE)
  expect_equal(raw$ppp_mean, 0)
  expect_equal(bayesnec:::ppp_value(c(1, 2, 2, 3), 2), 0.5)
})

test_that("a group with no censored reading is unchanged by the cens() term", {
  set.seed(1)
  mu <- rep(c(1, 0.2), each = 6)
  grp <- factor(rep(c("a", "b"), each = 6))
  yrep <- gamma_draws(mu, 50)
  y <- gamma_draws(mu, 1)[1, ]
  code <- c(rep(0L, 6), 0L, -1L, 0L, -1L, 0L, 0L)
  y[code == -1L] <- 0.15
  cens <- list(code = code, bound = y, may_be_zero = code == -1L)
  held <- bayesnec:::check_fit_stats(y, yrep, mu, grp, FALSE, cens)
  raw <- bayesnec:::check_fit_stats(y, yrep, mu, grp, FALSE)
  expect_identical(names(held), append(names(raw), "n_cens", after = 2))
  expect_identical(held$n_cens, c(0L, 2L))
  expect_identical(held[1, names(raw)], raw[1, ])
  expect_false(identical(held[2, names(raw)], raw[2, ]))
})

test_that("right and interval bounds hold values at the group's bounds", {
  # The interval [3, 4] is read as left-censored at 4; the right-censored
  # reading sets a ceiling of 5.
  y <- c(1, 2, 5, 3, 0.5)
  cens <- list(code = c(0L, 0L, 1L, 2L, 0L), bound = c(1, 2, 5, 4, 0.5),
               may_be_zero = rep(FALSE, 5))
  yrep <- rbind(c(0, 3, 6, 10, 4.5), rep(1, 5))
  out <- bayesnec:::check_fit_stats(y, yrep, rep(0, 5), factor(rep("a", 5)),
                                    FALSE, cens)
  expect_equal(out$n_cens, 2L)
  expect_equal(out$obs_mean, mean(c(4, 4, 5, 4, 4)))
  expect_equal(out$sim_mean, median(c(mean(c(4, 4, 5, 5, 4.5)), 4)))
})

test_that("crossing left and right bounds cannot be held together", {
  y <- c(10, 5)
  cens <- list(code = c(-1L, 1L), bound = y,
               may_be_zero = c(TRUE, FALSE))
  expect_error(
    bayesnec:::check_fit_stats(y, matrix(y, nrow = 1), y,
                                factor(c("a", "a")), FALSE, cens),
    "Group a has a left or interval censoring bound above a right"
  )
})

test_that("a structural zero stays at zero unless a censored reading may be one", {
  y <- c(0, 0, 0.1, 0.4, 0.05)
  code <- c(0L, 0L, -1L, 0L, 0L)
  grp <- factor(rep("a", 5))
  yrep <- rbind(c(0, 0.02, 0.3, 0, 0.6), c(0.2, 0, 0, 0.05, 0.5))
  mu <- rep(0.2, 5)
  # A censored survivor of a hurdle fit is known to be alive.
  known <- list(code = code, bound = y, may_be_zero = rep(FALSE, 5))
  out <- bayesnec:::check_fit_stats(y, yrep, mu, grp, TRUE, known)
  expect_equal(out$obs_mean, mean(c(0, 0, 0.1, 0.4, 0.1)))
  expect_equal(c(out$obs_zero, out$sim_zero), c(0.4, 0.4))
  expect_true(is.finite(out$ppp_zero))
  # brms left censoring in a hurdle family includes the zero mass.
  maybe <- list(code = code, bound = y, may_be_zero = code == -1L)
  out <- bayesnec:::check_fit_stats(y, yrep, mu, grp, TRUE, maybe)
  expect_equal(out$obs_mean, mean(c(0.1, 0.1, 0.1, 0.4, 0.1)))
  expect_true(all(is.na(out[c("obs_zero", "sim_zero", "ppp_zero")])))
})

test_that("an identical zero fraction gives a mid-p", {
  y <- c(0, 0, 1, 1)
  yrep <- matrix(rep(y, 20), nrow = 20, byrow = TRUE)
  out <- bayesnec:::check_fit_stats(y, yrep, rep(0.5, 4),
                                    factor(rep("a", 4)), TRUE)
  expect_equal(out$ppp_zero, 0.5)
})

test_that("print, plot and the control check read a censored table", {
  set.seed(3)
  mu <- rep(c(1, 0.5, 0.02), each = 4)
  grp <- factor(rep(c("0", "1", "2"), each = 4))
  yrep <- gamma_draws(mu, 50)
  y <- c(gamma_draws(mu[1:8], 1)[1, ], rep(0.1, 4))
  code <- rep(c(0L, -1L), c(8, 4))
  cens <- list(code = code, bound = y, may_be_zero = code == -1L)
  out <- bayesnec:::check_fit_stats(y, yrep, mu, grp, FALSE, cens)
  out$control <- out$group == "0"
  class(out) <- c("checkfit", "data.frame")
  expect_equal(out$n_cens, c(0L, 0L, 4L))
  expect_true(is.finite(out$ppp_mean[3]))
  expect_output(print(out), "n_cens is the number of censored readings")
  p <- plot(out)
  expect_match(p$labels$caption, "held at their censoring bound")
  expect_warning(ggplot2::ggplot_build(p), NA)
  local_mocked_bindings(check_fit = function(x, ...) out)
  issues <- control_fit_issues(list(success_models = "ecx4param"),
                               cutoff = 1.15)
  expect_named(issues, "ecx4param")
  expect_type(unlist(issues), "logical")
  # A table without the column prints no note.
  raw <- bayesnec:::check_fit_stats(y, yrep, mu, grp, FALSE)
  raw$control <- raw$group == "0"
  class(raw) <- c("checkfit", "data.frame")
  expect_false(any(grepl("n_cens", capture.output(print(raw)))))
  expect_false(grepl("censoring bound", plot(raw)$labels$caption))
})

# brm(empty = TRUE) builds the fitted data without compiling a Stan program.
cens_empty_fit <- function(formula, data) {
  fam <- Gamma(link = "identity")
  bf <- bayesnec:::wrangle_model_formula(
    "ecx4param", formula, model.frame(formula, data = data),
    family = bayesnec:::validate_family(fam)
  )
  suppressMessages(brms::brm(
    bf, data = data, family = bayesnec:::validate_family(fam),
    prior = get_priors(formula, data = data, family = fam), empty = TRUE
  ))
}

cens_empty_data <- data.frame(
  x = rep(c(0.1, 1, 3, 10), each = 3),
  y = c(1.2, 1.1, 0.9, 0.8, 0.7, 0.75, 0.4, 0.3, 0.35, 0.2, 0.2, 0.25),
  censor = c(rep("none", 9), "left", "left", "interval"),
  upper = c(rep(0, 11), 0.3)
)

test_that("check_fit_cens() reads the codes and bounds brms fits", {
  d <- cens_empty_data
  expect_null(bayesnec:::check_fit_cens(
    cens_empty_fit(bnf(y ~ crf(x, "ecx4param")), d)
  ))
  got <- bayesnec:::check_fit_cens(
    cens_empty_fit(bnf(y | cens(censor, upper) ~ crf(x, "ecx4param")), d)
  )
  expect_identical(got$code, c(rep(0L, 9), -1L, -1L, 2L))
  expect_equal(got$bound, c(d$y[1:11], 0.3))
  expect_identical(got$may_be_zero, got$code == -1L)
})

test_that("a hurdle fit's growth censoring is placed on the survivors", {
  d <- cens_empty_data
  gb <- cens_empty_fit(bnf(y | cens(censor, upper) ~ crf(x, "ecx4param")), d)
  y_obs <- c(0, d$y[1:3], 0, 0, d$y[4:12])
  got <- bayesnec:::hurdle_exposed_cens(gb, y_obs)
  expect_identical(got$code, c(0L, 0L, 0L, 0L, 0L, 0L, rep(0L, 6), -1L, -1L,
                               2L))
  expect_equal(got$bound[y_obs > 0], c(d$y[1:11], 0.3))
  expect_false(any(got$may_be_zero))
  expect_error(bayesnec:::hurdle_exposed_cens(gb, y_obs[-2]),
               "cannot be matched")
  expect_null(bayesnec:::hurdle_exposed_cens(
    cens_empty_fit(bnf(y ~ crf(x, "ecx4param")), d), y_obs
  ))
})

cens_fit_fixture <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      set.seed(566)
      x <- rep(c(0, 1, 2, 2.5, 3, 3.5, 5), each = 10)
      mu <- 1 / (1 + exp((x - 2) * 1.5))
      y <- stats::rgamma(length(x), shape = 8, rate = 8 / mu)
      floor <- 0.12
      dat <- data.frame(x = x, y = pmax(y, floor),
                        censor = ifelse(y < floor, "left", "none"))
      fit <- suppressWarnings(suppressMessages(
        bnec(y | cens(censor) ~ crf(x, "ecx4param"), data = dat,
             family = Gamma(), iter = 1000, warmup = 500, chains = 2,
             seed = 566, refresh = 0)
      ))
      cached <<- list(dat = dat, fit = fit)
    }
    cached
  }
})

test_that("check_fit reads the censoring of a fit and holds those groups", {
  skip_on_cran()
  f <- cens_fit_fixture()
  out <- check_fit(f$fit, ndraws = 500, seed = 10)
  expect_identical(out$n_cens,
                   as.integer(tapply(f$dat$censor == "left", f$dat$x, sum)))
  whole <- out$n_cens == out$n
  expect_gte(sum(whole), 2)
  in_span <- function(p) p >= 0.05 & p <= 0.95
  expect_true(all(in_span(c(out$ppp_mean[whole], out$ppp_sd[whole]))))
  # Read without the cens() term, the same groups are flagged.
  local_mocked_bindings(check_fit_cens = function(fit) NULL)
  raw <- check_fit(f$fit, ndraws = 500, seed = 10)
  expect_false("n_cens" %in% names(raw))
  expect_true(all(raw$ppp_mean[whole] < 0.05))
  uncensored <- out$n_cens == 0
  expect_identical(as.data.frame(out)[uncensored, names(raw)],
                   as.data.frame(raw)[uncensored, names(raw)])
})

test_that("grouped and model-set checks carry each fit's censoring", {
  skip_on_cran()
  fit <- cens_fit_fixture()$fit
  n_cens <- nrow(fit$fit$data)
  n_plain <- nrow(nec4param$fit$data)
  grouped <- structure(
    list(fits = list(cens = fit, plain = nec4param),
         levels = c("cens", "plain"), group_var = "site",
         data = data.frame(site = rep(c("cens", "plain"),
                                      c(n_cens, n_plain)))),
    class = c("bayesnecgroupfit", "bnecfit")
  )
  out <- suppressWarnings(check_fit(grouped, ndraws = 50, seed = 4))
  expect_identical(out$fits$cens, check_fit(fit, ndraws = 50, seed = 4))
  expect_true("n_cens" %in% names(out$fits$cens))
  expect_false("n_cens" %in% names(out$fits$plain))
  expect_output(print(out), "n_cens is the number of censored readings")

  # The model-set method delegates to the single-fit method per model.
  set <- structure(
    list(mod_fits = list(ecx4param = fit, copy = fit),
         mod_stats = data.frame(wi = c(0.7, 0.3),
                                row.names = c("ecx4param", "copy"))),
    class = c("bayesmanecfit", "bnecfit")
  )
  local_mocked_bindings(pull_out = function(x, model, ...) x$mod_fits[[model]])
  by_model <- check_fit(set, ndraws = 50, seed = 4)
  expect_identical(names(by_model)[1:5],
                   c("model", "wi", "group", "n", "n_cens"))
  expect_identical(by_model$n_cens[by_model$model == "copy"],
                   out$fits$cens$n_cens)
  expect_identical(by_model$n_cens[by_model$model == "ecx4param"],
                   out$fits$cens$n_cens)
})

test_that("the combined hurdle table marks the control, so it can be plotted", {
  skip_on_cran()
  res <- suppressWarnings(check_fit(hurdle_fixture()$fit))
  d <- as.data.frame(res$combined)
  expect_equal(sum(d$control), 1)
  expect_true(d$control[1])
  p <- plot(res$combined)
  expect_s3_class(p, "ggplot")
  expect_warning(ggplot2::ggplot_build(p), NA)
})

test_that("a hurdle fit with a model set checks every pair of equations", {
  skip_on_cran()
  fit <- hurdle_fixture()$fit
  # The fixture holds 400 draws, so asking for more uses every draw.
  single <- bayesnec:::check_fit_combined(fit, group = 3, ndraws = 1000,
                                          seed = 7)
  g <- fit$growth
  set_fit <- fit
  set_fit$growth <- structure(
    list(mod_fits = setNames(list(g, g), c(g$model, "copy")),
         mod_stats = data.frame(wi = c(0.6, 0.4),
                                row.names = c(g$model, "copy"))),
    class = c("bayesmanecfit", "bnecfit")
  )
  out <- bayesnec:::check_fit_combined(set_fit, group = 3, ndraws = 1000,
                                       seed = 7)
  labels <- paste(c(g$model, "copy"), fit$survival$model, sep = " / ")
  expect_identical(unique(out$model), labels)
  expect_equal(unique(out$wi), c(0.6, 0.4))
  expect_identical(names(out)[1:3], c("model", "wi", "group"))
  first <- as.data.frame(out)[out$model == labels[1], names(single)]
  rownames(first) <- NULL
  expect_equal(first, as.data.frame(single))
  expect_true(all(tapply(out$control, out$model, sum) == 1L))
  expect_s3_class(plot(out), "ggplot")
})

test_that("a tie at the bound survives rounding in the last place", {
  # mean() and rowMeans() can differ in the last place, which on some platforms
  # turns a tie at the bound into a flag.
  x <- 0.1
  up <- x * (1 + 4 * .Machine$double.eps)
  down <- x * (1 - 4 * .Machine$double.eps)
  expect_equal(bayesnec:::ppp_value(rep(up, 4), x), 0.5)
  expect_equal(bayesnec:::ppp_value(rep(down, 4), x), 0.5)
  expect_equal(bayesnec:::ppp_value(c(up, 2 * x), x), 0.75)
  # Half the draws sit just above the bound and half are held at it.
  y <- rep(x, 12)
  yrep <- rbind(matrix(up, 5, 12), matrix(0.05, 5, 12))
  cens <- list(code = rep(-1L, 12), bound = y, may_be_zero = rep(TRUE, 12))
  out <- bayesnec:::check_fit_stats(y, yrep, rep(0.02, 12),
                                    factor(rep("a", 12)), FALSE, cens)
  expect_equal(out$ppp_mean, 0.5)
  expect_equal(out$mean_ratio, 1)
})

test_that("a wholly held control is not read as a discrepancy", {
  y <- rep(0.1, 6)
  yrep <- matrix(0.01, nrow = 20, ncol = 6)
  grp <- factor(rep("0", 6))
  cens <- list(code = rep(-1L, 6), bound = y, may_be_zero = rep(TRUE, 6))
  out <- bayesnec:::check_fit_stats(y, yrep, rep(0.02, 6), grp, FALSE, cens)
  expect_equal(c(out$mean_ratio, out$sd_ratio), c(1, 1))
  expect_equal(c(out$ppp_mean, out$ppp_sd), c(0.5, 0.5))
  out$control <- TRUE
  class(out) <- c("checkfit", "data.frame")
  local_mocked_bindings(check_fit = function(x, ...) out)
  expect_identical(control_fit_issues(list(success_models = "ecx4param"),
                                      cutoff = 1.15),
                   list(ecx4param = FALSE))
})

# The model bnec_joint() refits a censored hurdle fit as: it marks each
# left-censored survivor and passes the formula to bnec().
joint_cens_fixture <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      set.seed(566)
      x <- rep(c(0, 1, 2, 2.5, 3, 3.5, 5), each = 10)
      mu <- 1 / (1 + exp((x - 2) * 1.5))
      y <- stats::rgamma(length(x), shape = 8, rate = 8 / mu)
      floor <- 0.12
      censor <- ifelse(y < floor, "left", "none")
      y <- pmax(y, floor)
      alive <- stats::rbinom(length(x), 1, 1 / (1 + exp((x - 3.5) * 2)))
      y[alive == 0] <- 0
      censor[alive == 0] <- "none"
      dat <- data.frame(x = x, y = y, censor = censor)
      f <- bayesnec:::mark_joint_survivor_cens(
        bnf(y | cens(censor) ~ crf(x, "ecx4param")), dat, "hurdle_gamma"
      )
      fit <- suppressWarnings(suppressMessages(
        bnec(f, data = dat, family = "hurdle_gamma",
             model_survival = "ecx4param", iter = 1000, warmup = 500,
             chains = 2, seed = 568, refresh = 0)
      ))
      cached <<- list(dat = dat, fit = fit)
    }
    cached
  }
})

test_that("a joint refit's censored survivors are read as alive", {
  skip_on_cran()
  f <- joint_cens_fixture()
  fit <- f$fit
  dat <- f$dat
  left <- dat$censor == "left"
  codes <- bayesnec:::check_fit_cens(fit$fit)
  expect_identical(codes$code == 2L, left)
  expect_false(any(codes$may_be_zero))
  expect_equal(codes$bound[left], dat$y[left])
  # The recorded codes call the same rows left-censored, which in a hurdle
  # family includes the zero mass.
  frame <- model.frame(fit$bayesnecformula, data = fit$fit$data)
  recorded <- bayesnec:::retrieve_cens(frame)
  expect_identical(recorded == -1L, left)
  out <- check_fit(fit, ndraws = 400, seed = 9)
  expect_identical(out$n_cens, as.integer(tapply(left, dat$x, sum)))
  expect_equal(out$obs_zero, as.numeric(tapply(dat$y == 0, dat$x, mean)))
  mixed <- as.vector(tapply(left, dat$x, any) & tapply(dat$y == 0, dat$x, any))
  expect_gte(sum(mixed), 1)
  expect_true(all(is.finite(out$ppp_zero[mixed])))
  # Read with the recorded codes, the deaths in those groups would be held at
  # the bound and their zero proportions lost.
  local_mocked_bindings(check_fit_cens = function(fit) {
    list(code = recorded, bound = fit$data$y, may_be_zero = recorded == -1L)
  })
  wrong <- check_fit(fit, ndraws = 400, seed = 9)
  expect_true(all(is.na(wrong$obs_zero[mixed])))
  expect_true(all(wrong$obs_mean[mixed] > out$obs_mean[mixed]))
})
