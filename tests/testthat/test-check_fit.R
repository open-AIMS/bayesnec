# #148. The point of check_fit is that it is LOCAL: a global dispersion
# statistic is structurally incapable of seeing a fit that mis-states
# variability in one region, because a free dispersion parameter absorbs exactly
# the discrepancy the global statistic measures.

test_that("check_fit returns a row per group with both statistics", {
  skip_if(Sys.getenv("NOT_CRAN") == "")
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
  skip_if(Sys.getenv("NOT_CRAN") == "")
  n4 <- suppressMessages(pull_out(manec_example, model = "nec4param"))
  out <- check_fit(n4, group = 4, ndraws = 50)
  expect_equal(sum(out$control), 1)
  expect_true(out$control[1])
})

test_that("posterior predictive p-values are probabilities", {
  skip_if(Sys.getenv("NOT_CRAN") == "")
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
  skip_if(Sys.getenv("NOT_CRAN") == "")
  n4 <- suppressMessages(pull_out(manec_example, model = "nec4param"))
  expect_no_error(check_fit(n4, group = 3, ndraws = 1e6))
})

test_that("an unreplicated predictor is binned, with a warning", {
  # Requiring replication is defensible and matches how these designs are run,
  # but it would refuse nec_data, which is the package's own example. Binning
  # always returns something, including where the answer is meaningless, so it
  # has to say so.
  skip_if(Sys.getenv("NOT_CRAN") == "")
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
  skip_if(Sys.getenv("NOT_CRAN") == "")
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
  skip_if(Sys.getenv("NOT_CRAN") == "")
  n4 <- suppressMessages(pull_out(manec_example, model = "nec4param"))
  out <- suppressWarnings(check_fit(n4, ndraws = 200, seed = 10))
  ctrl <- out[out$control, ]
  expect_lt(ctrl$sd_ratio, 0.95)
  # and the steep tail fails the other way, which a single global number
  # averages away entirely
  expect_gt(max(out$sd_ratio), 1.2)
})
