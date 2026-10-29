test_that("prob_vals warnings behave as expected", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  expect_length(
    nsec(manec_example, prob_vals = c(0.6, 0.1, 0.9), resolution = 10), 3
  ) |>
    suppressWarnings()
  expect_error(
    nsec(manec_example, prob_vals = 0.9, resolution = 10)
  ) |>
    suppressWarnings()
  expect_error(
    nsec(manec_example, prob_vals = c(0.6, 0.9, 0.1), resolution = 10)
  ) |>
    suppressWarnings()
  expect_length(
    nsec(nec4param, prob_vals = c(0.6, 0.1, 0.9), resolution = 10), 3
  )
  expect_error(nsec(nec4param, prob_vals = 0.9, resolution = 10))
  expect_error(nsec(nec4param, prob_vals = c(0.6, 0.9, 0.1), resolution = 10))
})

test_that(paste0("nsec returns expected object types and resolution is",
                 " passing correctly"), {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec_summary <- nsec(manec_example, sig_val = 0.01, resolution = 50) |>
    suppressWarnings()
  nsec_summary2 <- nsec(manec_example, sig_val = 0.01, resolution = 50,
                        xform = exp) |>
    suppressWarnings()
  nsec_posterior <- nsec(manec_example, sig_val = 0.01,
                         posterior = TRUE, resolution = 50) |>
    suppressWarnings()
  nsecn_summary <- nsec(nec4param, sig_val = 0.01, resolution = 50) |>
    suppressWarnings()
  nsecn_summary2 <- nsec(nec4param, sig_val = 0.01, resolution = 50,
                         xform = exp) |>
    suppressWarnings()
  nsecn_posterior <- nsec(nec4param, sig_val = 0.01,
                          posterior = TRUE, resolution = 50) |>
    suppressWarnings()
  expect_equal(length(nsec_summary), 3)
  expect_gt(length(nsec_posterior), 3)
  expect_equal(length(nsecn_summary), 3)
  expect_gt(length(nsecn_posterior), 3)
  expect_equal(attributes(nsec_summary)$resolution, 50)
  expect_equal(attributes(nsec_posterior)$resolution, 50)
  expect_equal(attributes(nsecn_summary)$resolution, 50)
  expect_equal(attributes(nsecn_posterior)$resolution, 50)
})

test_that("works for bayesnecfit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec1 <- nsec(ecx4param, resolution = 10)
  expect_equal(length(nsec1), 3)
  expect_equal(names(nsec1), c("Q50", "Q2.5", "Q97.5"))
})

test_that("works for bayesmanecfit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec1 <- nsec(manec_example, resolution = 10) |>
    suppressWarnings()
  expect_equal(length(nsec1), 3)
  expect_equal(names(nsec1), c("Q50", "Q2.5", "Q97.5"))
})

test_that("xform passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec1 <- nsec(ecx4param, resolution = 10)
  nsec2 <- nsec(ecx4param, xform = exp, resolution = 10)
  expect_gt(nsec2[1], nsec1[2])
})

test_that("posterior passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec3 <- nsec(ecx4param, posterior = TRUE, resolution = 10)
  expect_equal(length(nsec3), 100)
})

test_that("prob_vals passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec4 <- nsec(ecx4param, prob_vals = c(0.5, 0.3, 0.7), resolution = 10)
  expect_equal(names(nsec4), c("Q50", "Q30", "Q70"))
})

test_that("sig_val passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec4 <- nsec(ecx4param, prob_vals = c(0.5, 0.3, 0.7), sig_val = 0.05,
                resolution = 10)
  expect_equal(names(nsec4), c("Q50", "Q30", "Q70"))
})


# Control references and removed arguments

test_that("hormesis_def is refused by name on nsec", {
  skip_on_cran()
  expect_error(nsec(nec4param, hormesis_def = "max"),
               "hormesis_def has been removed")
  expect_error(nsec(manec_example, hormesis_def = "control"),
               "hormesis_def has been removed")
})

test_that("ecnsec is the absolute percent effect at the nsec", {
  # ECNSEC uses the same control-to-zero percentage reference as default ECx:
  # (control - reference) / control * 100.
  skip_on_cran()
  out <- nsec(nec4param, resolution = 200, sig_val = 0.01) |>
    suppressWarnings()
  ec <- attr(out, "ecnsec_relativeP")
  expect_length(ec, 3)
  # This fixture gives positive effects below 100 percent. Their magnitude
  # depends on the fitted control distribution and significance level.
  expect_true(all(ec > 0))
  expect_true(all(ec < 100))
  expect_lt(ec[1], 25)
})

test_that("nsec returns NA where the curve never reaches the reference", {
  # A grid confined to the flat head has no crossing. Report it as censored
  # rather than returning its nearest endpoint as an identified NSEC.
  skip_on_cran()
  expect_warning(
    out <- nsec(nec4param, resolution = 50, x_range = c(0, 0.05),
                posterior = TRUE),
    "does not fall below"
  )
  expect_true(any(is.na(out)))
})


# Draws whose NSEC is the control

test_that("a draw at or below the reference at the control returns the control", {
  # For this fixture, sig_val of the draws are already at or below the
  # control reference. Keep them at the observed control so the lower
  # credible bound can reach it.
  skip_on_cran()
  x_control <- min(nec_data$x)
  n_draws <- brms::ndraws(ecx4param$fit)
  for (sig_val in c(0.05, 0.1, 0.2)) {
    out <- nsec(ecx4param, sig_val = sig_val, resolution = 200,
                posterior = TRUE)
    expect_false(anyNA(out))
    expect_equal(sum(out == x_control), sig_val * n_draws)
    expect_equal(unname(quantile(out, 0.025)), x_control)
  }
  # Below the 0.025 quantile the bound sits above the control, which is the
  # other half of the same statement.
  out <- nsec(ecx4param, sig_val = 0.01, resolution = 200, posterior = TRUE)
  expect_equal(sum(out == x_control), 0.01 * n_draws)
  expect_gt(quantile(out, 0.025), x_control)
})

test_that("an x_range above the control does not report the control", {
  # Draws whose control is at or below the reference keep the control even
  # when x_range starts higher. Other draws crossing before the requested
  # grid return NA and are reported as below range.
  skip_on_cran()
  x_control <- min(nec_data$x)
  n_draws <- brms::ndraws(ecx4param$fit)
  # Report crossings below the requested range separately from curves that
  # never reach the reference within it.
  expect_warning(
    out <- nsec(ecx4param, resolution = 200, x_range = c(1, 3),
                posterior = TRUE),
    "falls below the control's 0.01 quantile before 1"
  )
  # The draws whose own control is at or below the reference keep the control,
  # because that is a property of the fit and not of the range asked for. Every
  # other draw already below the reference at the first grid point is NA.
  expect_equal(sum(out == x_control, na.rm = TRUE), 0.01 * n_draws)
  expect_true(sum(is.na(out)) > 0.01 * n_draws)
  expect_true(all(out >= 1 | out == x_control, na.rm = TRUE))
  # Prepending the control preserves these draws when the grid extends below
  # the data or becomes coarser. This checks draw retention; interpolated
  # values can still depend on grid spacing.
  full <- nsec(ecx4param, resolution = 200, sig_val = 0.05, posterior = TRUE)
  for (x_range in list(c(0, max(nec_data$x)), c(0, 100))) {
    wide <- nsec(ecx4param, resolution = 200, sig_val = 0.05,
                 x_range = x_range, posterior = TRUE)
    expect_false(anyNA(wide))
    expect_equal(sum(wide == x_control), sum(full == x_control))
  }
})

test_that("a range with no concentration above the control is refused", {
  # Refuse a grid with no searchable interval above the control; an all-NA
  # result would misdescribe the invalid grid as a fitted-curve failure.
  skip_on_cran()
  expect_error(
    nsec(ecx4param, resolution = 5, x_range = c(0, 0.02)),
    "holds no concentration above"
  )
  # The other way to the same grid. resolution = 1 stays a valid request of
  # bnec_newdata(), which test-bayesmanec_methods.R uses to pin a prediction
  # shape, so it is refused here rather than there.
  expect_error(nsec(ecx4param, resolution = 1), "holds no concentration above")
  # A range that does reach past the control is estimated over what it covers:
  # here only the draws at the control are identified, and the rest are reported
  # as never reaching the reference, which over this range they do not.
  expect_warning(
    out <- nsec(ecx4param, resolution = 5, x_range = c(0, 0.04),
                posterior = TRUE),
    "does not fall below"
  )
  expect_equal(sum(out == min(nec_data$x), na.rm = TRUE),
               0.01 * brms::ndraws(ecx4param$fit))
})

test_that("the draws at the control are not reported", {
  # They are sig_val * n_draws of every fit by construction, so a warning about
  # them restates the definition of the quantile.
  skip_on_cran()
  expect_no_warning(nsec(ecx4param, sig_val = 0.2, resolution = 100))
  expect_no_message(nsec(ecx4param, sig_val = 0.2, resolution = 100))
})

test_that("the stored no-effect estimate of an ecx equation keeps those draws", {
  # pull_out() recomputes the stored NSEC for smooth equations. Check that
  # this path retains control draws even without an explicit nsec() call.
  skip_on_cran()
  ne <- ecx4param$ne_posterior
  expect_false(anyNA(ne))
  expect_equal(sum(ne == min(nec_data$x)), 0.01 * length(ne))
})

test_that("nsec_from_posterior puts the lower sig_val tail at the control", {
  # Use 100 known declining curves: exactly five controls fall at or below
  # the 0.05 quantile, so the expected control count is fixed.
  skip_on_cran()
  x <- seq(1, 10, length.out = 50)
  controls <- seq(0.90, 1.10, length.out = 100)
  post <- t(vapply(controls, function(top) {
    top - (top - 0.05) * (x - min(x)) / diff(range(x))
  }, numeric(length(x))))
  reference <- stats::quantile(post[, 1], 0.05)
  out <- nsec_from_posterior(post, reference, x, x_control = x[1],
                             control = post[, 1])
  expect_false(anyNA(out))
  expect_equal(sum(out == x[1]), 5)
  expect_true(all(out >= x[1]))
  # Ignore grid points below the observed control when searching for NSEC.
  x_low <- c(x[1] / 2, x)
  post_low <- cbind(controls + 0.05, post)
  expect_equal(nsec_from_posterior(post_low, reference, x_low, x_control = x[1],
                                   control = post[, 1]), out)
  # Read the control posterior independently of the grid. Its five low
  # draws stay at the control when the grid begins higher; search other
  # draws only over the requested interval.
  keep_high <- x >= x[10]
  high <- nsec_from_posterior(post[, keep_high, drop = FALSE], reference,
                              x[keep_high], x_control = x[1],
                              control = post[, 1])
  expect_equal(sum(high == x[1], na.rm = TRUE), 5)
  expect_true(all(high >= x[10] | high == x[1], na.rm = TRUE))
  # The draws that are NA there are the ones that reached the reference between
  # the control and the start of the range, which is not identified within it.
  expect_true(anyNA(high))
  # A reference no curve reaches is still NA, and is still the case the warning
  # reports.
  expect_true(all(is.na(nsec_from_posterior(post, 0.01, x, x_control = x[1],
                                            control = post[, 1]))))
})

test_that("NSEC distinguishes a later effect from no effect after the control", {
  x <- 0:3
  post <- rbind(
    away_no_effect = c(0.8, 0.9, 1, 1),
    towards = c(0.8, 0.7, 0.6, 0.5),
    away_then_crosses = c(0.8, 1, 0.7, 0.5),
    flat = rep(0.8, 4)
  )
  out <- nsec_from_posterior(post, 0.9, x, x_control = 0,
                             control = post[, 1])

  expect_true(is.na(out[1]))
  expect_false(attr(out, "below_range")[1])
  expect_equal(unname(out[2]), 0)
  expect_equal(unname(out[3]), 1 + 1 / 3)
  expect_equal(unname(out[4]), 0)
})

test_that("nsec.brmsfit summarises censored draws without dropping them", {
  fit <- structure(list(data = data.frame(x = 0:3)), class = "brmsfit")
  post <- rbind(
    matrix(rep(c(1, 0.9, 0.8, 0.7), 40), nrow = 40, byrow = TRUE),
    matrix(rep(c(1, 1.1, 1.2, 1.3), 60), nrow = 60, byrow = TRUE)
  )
  local_mocked_bindings(posterior_epred = function(...) post)

  draws <- nsec(fit, x_var = "x", resolution = 4, posterior = TRUE)
  estimate <- nsec(fit, x_var = "x", resolution = 4)

  expect_equal(sum(attr(draws, "censored")$above), 60)
  expect_equal(unname(as.numeric(estimate)), c(3, 0, 3))
  expect_identical(attr(estimate, "censored_summary")$bound,
                   c(">=", "", ">="))

  fit$data <- data.frame(x = rep(0:3, 2), g = rep(c("a", "b"), each = 4))
  grouped <- nsec(fit, x_var = "x", group_var = "g", by_group = TRUE,
                  resolution = 4)
  pooled <- nsec(fit, x_var = "x", group_var = "g", resolution = 4)
  expect_equal(grouped$Q50, c(3, 3))
  expect_equal(unname(as.numeric(pooled)), c(3, 0, 3))
  expect_equal(attr(pooled, "censored_summary")$n_above, 120)
})
