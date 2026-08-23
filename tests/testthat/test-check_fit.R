# #148. The point of check_fit is that it is LOCAL: a global dispersion
# statistic is structurally incapable of seeing a fit that mis-states
# variability in one region, because a free dispersion parameter absorbs exactly
# the discrepancy the global statistic measures.

test_that("check_fit returns a row per group with both statistics", {
  skip_on_cran()
  n4 <- suppressMessages(pull_out(manec_example, model = "nec4param"))
  out <- check_fit(n4, group = 4, ndraws = 50)
  expect_s3_class(out, "checkfit")
  expect_equal(nrow(out), 4)
  for (nm in c("group", "n", "obs_mean", "sim_mean", "mean_ratio", "ppp_mean",
               "obs_sd", "sim_sd", "sd_ratio", "ppp_sd", "control")) {
    expect_true(nm %in% names(out), info = nm)
  }
  # every observation lands in exactly one group
  expect_equal(sum(out$n), nrow(n4$fit$data))
})

test_that("exactly one group is flagged as the control, and it is the lowest", {
  # nsec() reads its reference from the fitted curve at min(x), and the
  # observed control is y[x == min(x)] -- the package's own convention. The
  # flag has to agree with that or it points at the wrong row.
  skip_on_cran()
  n4 <- suppressMessages(pull_out(manec_example, model = "nec4param"))
  out <- check_fit(n4, group = 4, ndraws = 50)
  expect_equal(sum(out$control), 1)
  expect_true(out$control[1])
})

test_that("posterior predictive p-values are probabilities", {
  skip_on_cran()
  n4 <- suppressMessages(pull_out(manec_example, model = "nec4param"))
  out <- check_fit(n4, group = 4, ndraws = 50)
  for (nm in c("ppp_mean", "ppp_sd")) {
    expect_true(all(out[[nm]] >= 0 & out[[nm]] <= 1, na.rm = TRUE), info = nm)
  }
})

test_that("ndraws is reduced to what the fit holds rather than erroring", {
  # brms errors instead of truncating, and manec_example carries 100 draws, so
  # the default of 1000 would fail on the package's own example object -- which
  # is exactly what someone runs a diagnostic on first.
  skip_on_cran()
  n4 <- suppressMessages(pull_out(manec_example, model = "nec4param"))
  expect_no_error(check_fit(n4, group = 3, ndraws = 1e6))
})

test_that("an unreplicated predictor is binned, with a warning", {
  # Requiring replication is defensible and matches how these designs are run,
  # but it would refuse nec_data, which is the package's own example. Binning
  # always returns something, including where the answer is meaningless, so it
  # has to say so.
  skip_on_cran()
  n4 <- suppressMessages(pull_out(manec_example, model = "nec4param"))
  expect_warning(check_fit(n4, ndraws = 50), "not a design point")
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
  # Stacking weights come from a global elpd, so a candidate can hold high
  # weight while fitting the control badly -- it wins on the bulk of the curve
  # and pays almost nothing for the control. Without per-model rows the table
  # cannot say which model is doing the damage.
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
  # The demonstration #148 rests on: manec_example passes a global dispersion
  # check (1.011 [0.71, 1.44]) while simulating materially more variability
  # than the data show in the control region. If check_fit cannot see that, it
  # does not do the job it was written for.
  skip_on_cran()
  n4 <- suppressMessages(pull_out(manec_example, model = "nec4param"))
  out <- suppressWarnings(check_fit(n4, ndraws = 200, seed = 10))
  ctrl <- out[out$control, ]
  expect_lt(ctrl$sd_ratio, 0.95)
  # and the steep tail fails the other way, which a single global number
  # averages away entirely
  expect_gt(max(out$sd_ratio), 1.2)
})

# #148: the settled scope was "deliver both a numeric test and a plot". The
# table answers whether a group is off; the plot answers by how much and in
# which direction, which is what decides whether it matters.

test_that("plot.checkfit returns a ggplot with both statistics panelled", {
  skip_on_cran()
  cf <- suppressWarnings(check_fit(manec_example))
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
  cf <- suppressWarnings(check_fit(manec_example))
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
  cf <- suppressWarnings(check_fit(manec_example))
  expect_true(all(c("sim_mean_lo", "sim_mean_hi", "sim_sd_lo", "sim_sd_hi")
                  %in% names(as.data.frame(cf))))
  # print() drops them: they are for plot(), and including them takes the
  # console table past a readable width
  printed <- capture.output(print(cf))
  expect_false(any(grepl("sim_mean_lo|sim_sd_hi", printed)))
})

test_that("the interval brackets the simulated median", {
  skip_on_cran()
  d <- as.data.frame(suppressWarnings(check_fit(manec_example)))
  expect_true(all(d$sim_mean_lo <= d$sim_mean & d$sim_mean <= d$sim_mean_hi))
  expect_true(all(d$sim_sd_lo <= d$sim_sd & d$sim_sd <= d$sim_sd_hi))
})

# #148 decision (d): the combined hurdle check. The per-component tables check
# each half against its own subset; neither asks whether the fit reproduces the
# observed response, which contains the zeros and is what was measured.

hurdle_fixture <- function() {
  set.seed(17)
  x <- rep(seq(0, 5, length.out = 15), each = 6)
  p_alive <- 1 / (1 + exp(-(2.5 - 0.9 * x)))
  alive <- rbinom(length(x), 1, p_alive)
  g <- rgamma(length(x), shape = 4, rate = 4 / pmax(3 - 0.35 * x, 0.3))
  dat <- data.frame(x = x, y = ifelse(alive == 1, g, 0))
  fit <- suppressWarnings(suppressMessages(
    bnec_hurdle(y ~ crf(x, "nec3param"), data = dat, iter = 400, warmup = 200,
                chains = 2, seed = 17, refresh = 0, open_progress = FALSE)
  ))
  list(dat = dat, fit = fit)
}

test_that("the observed response is reconstructed exactly, zeros included", {
  # The load-bearing assumption: bnec_hurdle() subsets rather than reorders, so
  # the growth response drops into the alive positions. If that ever stops
  # holding, the combined check silently compares against the wrong vector.
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
