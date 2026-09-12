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


# ---- D15: the control anchors nsec too, and hormesis_def is gone -------------

test_that("hormesis_def is refused by name on nsec", {
  skip_on_cran()
  expect_error(nsec(nec4param, hormesis_def = "max"),
               "hormesis_def has been removed")
  expect_error(nsec(manec_example, hormesis_def = "control"),
               "hormesis_def has been removed")
})

test_that("ecnsec is the absolute percent effect at the nsec", {
  # D15 ruling 5, toxval#49 and T8. The attached ecnsec used to be measured
  # against the fitted range, by three formulas that agreed only for a
  # monotonic curve. It is now (control - reference) / control * 100, which is
  # exactly what ecx measures under its default type.
  skip_on_cran()
  out <- nsec(nec4param, resolution = 200, sig_val = 0.01) |>
    suppressWarnings()
  ec <- attr(out, "ecnsec_relativeP")
  expect_length(ec, 3)
  # A percentage, and a small one: the NSEC is by construction the point at
  # which the curve first leaves the control's own lower tail, so the effect
  # there is a few per cent rather than tens of per cent.
  expect_true(all(ec > 0))
  expect_true(all(ec < 100))
  expect_lt(ec[1], 25)
})

test_that("nsec returns NA where the curve never reaches the reference", {
  # D15 ruling 3. Restricting x_range to the flat head of the curve leaves no
  # crossing to find. Up to 2.1.3 every such draw returned max(x_vec), so the
  # NSEC was reported as the top of the range with nothing said.
  skip_on_cran()
  expect_warning(
    out <- nsec(nec4param, resolution = 50, x_range = c(0, 0.05),
                posterior = TRUE),
    "does not fall below"
  )
  expect_true(any(is.na(out)))
})


# ---- #325: the draws whose NSEC is the control -------------------------------

test_that("a draw at or below the reference at the control returns the control", {
  # The reference is the sig_val quantile of the control posterior, so sig_val of
  # the draws sit at or below it and reach it at the control itself. Fisher and
  # Fox (2023) report those draws as the estimate: their Table 3 gives a lower
  # credible bound of zero at every significance level above the 0.025 quantile
  # the bound is read at, and those draws are what produces it. Up to this fix
  # they returned NA and were dropped, so the bound could not reach the control
  # at any sig_val.
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

test_that("the draws at the control are not reported", {
  # They are sig_val * n_draws of every fit by construction, so a warning about
  # them restates the definition of the quantile.
  skip_on_cran()
  expect_no_warning(nsec(ecx4param, sig_val = 0.2, resolution = 100))
  expect_no_message(nsec(ecx4param, sig_val = 0.2, resolution = 100))
})

test_that("the stored no-effect estimate of an ecx equation keeps those draws", {
  # expand_nec() reads the NSEC off the curve for every ecx-class equation, and it
  # is that estimate summary(), the model weights and nec() report, so the defect
  # reached a fit nsec() was never called on. The fixture is built by pull_out(),
  # which recomputes it, so this exercises that path without fitting anything.
  skip_on_cran()
  ne <- ecx4param$ne_posterior
  expect_false(anyNA(ne))
  expect_equal(sum(ne == min(nec_data$x)), 0.01 * length(ne))
})

test_that("nsec_from_posterior puts the lower sig_val tail at the control", {
  # The helper both nsec() and expand_nec() use, on a matrix built so that the
  # answer is known: 100 straight declining curves whose controls run from 0.90
  # to 1.10, so the 0.05 quantile of the first column has exactly five rows at
  # or below it.
  skip_on_cran()
  x <- seq(1, 10, length.out = 50)
  controls <- seq(0.90, 1.10, length.out = 100)
  post <- t(vapply(controls, function(top) {
    top - (top - 0.05) * (x - min(x)) / diff(range(x))
  }, numeric(length(x))))
  reference <- stats::quantile(post[, 1], 0.05)
  out <- nsec_from_posterior(post, reference, x, x_control = x[1])
  expect_false(anyNA(out))
  expect_equal(sum(out == x[1]), 5)
  expect_true(all(out >= x[1]))
  # A grid that x_range has extended below the control is not searched, so the
  # estimate does not change. D15 ruling 2.
  x_low <- c(x[1] / 2, x)
  post_low <- cbind(controls + 0.05, post)
  expect_equal(nsec_from_posterior(post_low, reference, x_low,
                                   x_control = x[1]), out)
  # A reference no curve reaches is still NA, and is still the case the warning
  # reports.
  expect_true(all(is.na(nsec_from_posterior(post, 0.01, x, x_control = x[1]))))
})
