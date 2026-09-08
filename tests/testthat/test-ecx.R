test_that("prob_vals warnings behave as expected", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx(manec_example, prob_vals = c(0.6, 0.1, 0.9), resolution = 10,
      type = "relative") |>
    expect_length(3) |>
    suppressWarnings()
  ecx(manec_example, prob_vals = 0.9, resolution = 10, type = "relative") |>
    expect_error() |>
    suppressWarnings()
  ecx(manec_example, prob_vals = c(0.6, 0.9, 0.1), resolution = 10,
      type = "relative") |>
    expect_error() |>
    suppressWarnings()
  ecx(nec4param, prob_vals = c(0.6, 0.1, 0.9), resolution = 10,
      type = "relative") |>
    expect_length(3) |>
    suppressWarnings()
  expect_error(ecx(nec4param, prob_vals = 0.9, type = "relative",
                   resolution = 10))
  expect_error(ecx(nec4param, prob_vals = c(0.6, 0.9, 0.1), resolution = 10,
                   type = "relative"))
})

test_that("ecx_val warnings behave as expected", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # 0.9 is now accepted: the lower bound is 0, not 1, and there is no upper
  # bound. Under "absolute" the reference is the control and OECD TG 201
  # permits an effect above 100% for a response that can go negative rather
  # than truncating it. See #195 and D15 ruling 7.
  expect_length(
    suppressWarnings(
      ecx(manec_example, ecx_val = 0.9, type = "relative", resolution = 10)
    ), 3
  )
  expect_length(
    suppressWarnings(
      ecx(nec4param, ecx_val = 0.9, type = "relative", resolution = 10)
    ), 3
  )
  expect_length(ecx(nec4param, ecx_val = 150, resolution = 10), 3)
  expect_error(ecx(nec4param, ecx_val = 0, resolution = 10),
               "greater than 0")
  expect_error(ecx(nec4param, ecx_val = -5, resolution = 10),
               "greater than 0")
})

test_that("ecx returns expected object types and arguments pass correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ec50_summary <- ecx(manec_example, ecx_val = 50, type = "relative",
                      resolution = 50)
  ec50_summary2 <- ecx(manec_example, ecx_val = 50, type = "relative",
                       resolution = 50, xform = exp)
  ec50_posterior <- ecx(manec_example, ecx_val = 50,
                        type = "relative", posterior = TRUE, resolution = 50)
  ec50n_summary <- ecx(nec4param, ecx_val = 50, type = "relative",
                       resolution = 50)
  ec50n_summary2 <- ecx(nec4param, ecx_val = 50, type = "relative",
                        resolution = 50, xform = exp)
  ec50n_posterior <- ecx(nec4param, ecx_val = 50, type = "relative",
                         posterior = TRUE, resolution = 50)
  expect_equal(length(ec50_summary), 3)
  expect_gt(length(ec50_posterior), 3)
  expect_equal(length(ec50n_summary), 3)
  expect_gt(length(ec50n_posterior), 3)
  expect_equal(attributes(ec50_summary)$resolution, 50)
  expect_equal(attributes(ec50_posterior)$resolution, 50)
  expect_equal(attributes(ec50n_summary)$resolution, 50)
  expect_equal(attributes(ec50n_posterior)$resolution, 50)
  expect_gt(ec50_summary2[1], ec50_summary[1])
  expect_gt(ec50n_summary2[1], ec50n_summary[1])
})

test_that("works for bayesnecfit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx1 <- ecx(ecx4param, resolution = 10)
  expect_equal(length(ecx1), 3)
  expect_equal(names(ecx1), c("Q50", "Q2.5", "Q97.5"))
})

test_that("works for bayesmanecfit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx1 <- ecx(manec_example, resolution = 10)
  expect_equal(length(ecx1), 3)
  expect_equal(names(ecx1), c("Q50", "Q2.5", "Q97.5"))
})

test_that("xform passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx1 <- ecx(ecx4param, resolution = 10)
  ecx2 <- ecx(ecx4param, xform = exp, resolution = 10)
  expect_gt(ecx2[1], ecx1[2])
})

test_that("posterior passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx3 <- ecx(ecx4param, posterior = TRUE, resolution = 10)
  expect_equal(length(ecx3), 100)
})

test_that("prob_vals passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx4 <- ecx(ecx4param, prob_vals = c(0.5, 0.3, 0.7), resolution = 10)
  expect_equal(names(ecx4), c("Q50", "Q30", "Q70"))
})

test_that("ecx_val passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx4 <- ecx(ecx4param, prob_vals = c(0.5, 0.3, 0.7), ecx_val = 20,
              resolution = 10)
  expect_equal(names(ecx4), c("Q50", "Q30", "Q70"))
})


# ---- D15: the control is the reference, and type is a four-value vocabulary --

test_that("the four types are accepted and give different references", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  a <- ecx(nec4param, ecx_val = 10, type = "absolute", resolution = 200)
  r <- ecx(nec4param, ecx_val = 10, type = "range", resolution = 200)
  rel <- suppressWarnings(
    ecx(nec4param, ecx_val = 10, type = "relative", resolution = 200)
  )
  d <- ecx(nec4param, ecx_val = 1.5, type = "direct", resolution = 200)
  for (v in list(a, r, rel, d)) {
    expect_length(v, 3)
  }
  # Each type measures from the same control towards a different floor, so the
  # ECx values order as the floors do. Measured on this fit: bot is -8.42,
  # the lowest predicted response is -4.13, and the absolute floor is 0. The
  # further the floor, the longer the span, the later a fixed percentage of it
  # is reached, so absolute < range < relative. The predicted minimum is
  # negative here, which is why range sits above absolute rather than below it
  # -- on a response bounded at zero the order of those two reverses.
  expect_lt(a[1], r[1])
  expect_lt(r[1], rel[1])
  expect_error(ecx(nec4param, type = "nonsense"), "type must be one of")
  expect_error(ecx(nec4param, type = c("absolute", "range")),
               "type must be one of")
})

test_that("relative warns that it names a different quantity than in 2.1.3", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # D15 ruling 8. Only when the caller wrote it: the default is "absolute", so
  # a plain ecx() call must stay silent.
  expect_warning(ecx(nec4param, type = "relative", resolution = 100),
                 "now measures from the control")
  expect_silent(ecx(nec4param, resolution = 100))
})

test_that("relative is refused where the bound is infinite", {
  # D15 ruling 6. An equation with no bot declines towards 0, which is a bound
  # under a family bounded below and is not one under gaussian. The refusal
  # names the equation, the family and the two types that do work.
  skip_on_cran()
  # Both packaged fits have a bot parameter, so the draws lookup is mocked
  # rather than a bot-free fit being manufactured: assigning $model alone
  # leaves b_bot_Intercept in the stored draws and the branch is never
  # reached. The suite already mocks this way in test-plot.R.
  local_mocked_bindings(as_draws_df = function(...) data.frame(lp__ = 1),
                        .package = "bayesnec")
  fake <- nec4param
  fake$model <- "nec3param"
  fake$fit$family <- stats::gaussian()
  expect_error(ecx_asymptote(fake, "relative"), "no bot parameter")
  expect_error(ecx_asymptote(fake, "relative"), "not bounded below")
  # Bounded below: the asymptote is 0 and the request is answered.
  fake$fit$family <- stats::Gamma()
  expect_equal(ecx_asymptote(fake, "relative"), 0)
  # Any other type never asks for an asymptote.
  expect_true(is.na(ecx_asymptote(fake, "absolute")))
})

test_that("hormesis_def is refused by name rather than absorbed by dots", {
  skip_on_cran()
  expect_error(ecx(nec4param, hormesis_def = "max"),
               "hormesis_def has been removed")
  expect_error(ecx(manec_example, hormesis_def = "control"),
               "hormesis_def has been removed")
})

test_that("a target the curve never reaches returns NA with a warning", {
  # D15 ruling 3. type = "direct" with a response the curve never attains is
  # the cheapest reproduction: manec_example's response spans roughly 0 to 2.2,
  # so no draw reaches 100. Up to 2.1.3 every draw returned the nearest grid
  # point, and for a target above the whole curve that is x[1] -- the control
  # concentration, returned as the ECx.
  skip_on_cran()
  expect_warning(out <- ecx(nec4param, ecx_val = 100, type = "direct",
                            resolution = 100, posterior = TRUE),
                 "does not reach")
  expect_true(all(is.na(out)))
  # And the summary is NA rather than the lowest concentration tested.
  expect_warning(est <- ecx(nec4param, ecx_val = 100, type = "direct",
                            resolution = 100))
  expect_true(all(is.na(est)))
})

test_that("the ecx reference is the control, not the maximum of the curve", {
  # The change D15 makes, asserted on the quantity rather than on a fitted
  # object: for a hormetic curve the two differ, and the control is the one
  # that gives a single crossing. p_samples is written directly so no fit is
  # needed and the assertion is about the estimator alone.
  x_vec <- seq(0, 10, length.out = 101)
  # Rises from a control of 10 to a peak of 15, then declines to 0.
  curve <- ifelse(x_vec < 2, 10 + 2.5 * x_vec, 15 * (1 - (x_vec - 2) / 8))
  p <- rbind(curve, curve)
  control <- c(10, 10)
  out <- ecx_from_posterior(p, x_vec, 10, "absolute", control, NA_real_)
  # The absolute EC10 target is 9, which the control-anchored curve reaches on
  # the descending limb. Measured from the maximum instead the target would be
  # 13.5, which the curve reaches while still rising and again while falling.
  expect_true(all(out > x_vec[which.max(curve)]))
  expect_equal(out[1], out[2])
  # The same curve read from its maximum crosses 13.5 on the rising limb, at
  # an x below the peak. This is what the estimator used to return.
  from_max <- ecx_from_posterior(p, x_vec, 10, "absolute", c(15, 15), NA_real_)
  expect_true(all(from_max < x_vec[which.max(curve)]))
})


test_that("the relative rename warns once for a set, not once per equation", {
  # sample_ecx() calls ecx() once per equation with type passed explicitly, so
  # without the option guard a model-averaged call warned once for the set and
  # once more for every equation in it. See D15 ruling 8.
  skip_on_cran()
  count_renames <- function(expr) {
    n <- 0
    withCallingHandlers(invisible(expr), warning = function(w) {
      if (grepl("now measures", conditionMessage(w))) n <<- n + 1
      invokeRestart("muffleWarning")
    })
    n
  }
  expect_equal(
    count_renames(ecx(manec_example, type = "relative", resolution = 50)), 1
  )
  expect_equal(
    count_renames(ecx(nec4param, type = "relative", resolution = 50)), 1
  )
  # The option is restored, so a later call in the same session still warns.
  expect_null(getOption("bayesnec.relative_warned"))
})

test_that("the estimate is insensitive to resolution above the default", {
  skip_on_cran()
  # This pins the basis for the default of 200. The crossing is interpolated
  # between the two bracketing grid points rather than snapped to the nearer of
  # them, so precision saturates well below the grid spacing and the default no
  # longer needs to be large. A five-fold increase must not change either
  # estimator materially. See #39.
  # as.numeric() rather than unname(): the estimate has a "resolution"
  # attribute recording what it was computed at, which differs by construction.
  expect_equal(as.numeric(ecx(nec4param, resolution = 200)),
               as.numeric(ecx(nec4param, resolution = 1000)),
               tolerance = 1e-3)
  suppressWarnings({
    n200 <- as.numeric(nsec(nec4param, resolution = 200))
    n1000 <- as.numeric(nsec(nec4param, resolution = 1000))
  })
  expect_equal(n200, n1000, tolerance = 1e-3)
})
