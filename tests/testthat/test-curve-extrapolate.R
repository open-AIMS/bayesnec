test_that("finite extensions retain every original prediction node", {
  fit <- suppressMessages(suppressWarnings(
    pull_out(manec_example, model = "ecx4param")
  ))
  base <- bayesnec:::newdata_eval(fit, 25, c(0.5, 0.9))
  lims <- bayesnec:::curve_extrapolate_limits(
    c(0.3, 2), fit, c(0.5, 0.9), "own", "ECx"
  )
  extended <- bayesnec:::extended_newdata_eval(fit, 25, c(0.5, 0.9), lims)
  positions <- match(base$x_vec, extended$x_vec)
  expect_identical(extended$x_vec[positions], base$x_vec)
  expect_identical(diff(extended$x_vec[positions]), diff(base$x_vec))
  expect_equal(range(extended$x_vec), c(0.3, 2))
  expect_identical(extended$newdata$x[positions], base$newdata$x)
  expect_gt(length(extended$x_vec), length(base$x_vec))
})

test_that("existing positional arguments retain their ECx and ECNSEC meanings", {
  fit <- suppressMessages(suppressWarnings(
    pull_out(manec_example, model = "ecx4param")
  ))
  old_ecx <- suppressWarnings(ecx(
    fit, 20, 30, FALSE, "absolute", c(0.5, 0.9), identity
  ))
  named_ecx <- suppressWarnings(ecx(
    fit, ecx_val = 20, resolution = 30, posterior = FALSE,
    type = "absolute", x_range = c(0.5, 0.9), xform = identity
  ))
  expect_equal(old_ecx, named_ecx)

  old_ecnsec <- ecnsec(fit, 0.7, 30, c(0.5, 0.9), "absolute", identity)
  named_ecnsec <- ecnsec(
    fit, nsec = 0.7, resolution = 30, x_range = c(0.5, 0.9),
    type = "absolute", xform = identity
  )
  expect_equal(old_ecnsec, named_ecnsec)

  part <- nec4param
  hurdle <- structure(list(
    growth = part, survival = part, data = part$fit$data,
    formula = part$bayesnecformula, y_var = "y"
  ), class = c("bayesnechurdlefit", "bnecfit"))
  old_hurdle_ecx <- suppressWarnings(ecx(
    hurdle, 20, 30, FALSE, "absolute", c(0.5, 0.9), identity
  ))
  named_hurdle_ecx <- suppressWarnings(ecx(
    hurdle, ecx_val = 20, resolution = 30, posterior = FALSE,
    type = "absolute", x_range = c(0.5, 0.9), xform = identity
  ))
  expect_equal(old_hurdle_ecx, named_hurdle_ecx)
  expect_equal(
    ecnsec(hurdle, 0.7, 30, c(0.5, 0.9), "absolute", identity),
    ecnsec(hurdle, nsec = 0.7, resolution = 30,
           x_range = c(0.5, 0.9), type = "absolute", xform = identity)
  )
})

test_that("ECx, NSEC and ECNSEC share a finite extended range", {
  fit <- suppressMessages(suppressWarnings(
    pull_out(manec_example, model = "ecx4param")
  ))
  args <- list(resolution = 30, x_range = c(0.5, 0.9))
  expect_error(do.call(ecx, c(list(object = fit, extrapolate = TRUE), args)),
               "samples a NEC")
  expect_error(do.call(ecnsec, c(list(object = fit, nsec = 1.5), args)),
               "outside")
  expect_length(suppressWarnings(do.call(ecx, c(
    list(object = fit, extrapolate = 2), args
  ))), 3)
  expect_length(suppressWarnings(do.call(nsec, c(
    list(object = fit, extrapolate = 2), args
  ))), 3)
  expect_length(suppressWarnings(do.call(ecnsec, c(
    list(object = fit, nsec = 1.5, extrapolate = 2), args
  ))), 3)
  expect_error(do.call(ecnsec, c(
    list(object = fit, nsec = 1.5, extrapolate = 0.8), args
  )), "narrow it with x_range")
})

test_that("ECx preserves interior values and identifies or censors beyond the data", {
  fit <- suppressMessages(suppressWarnings(
    pull_out(manec_example, model = "ecx4param")
  ))
  args <- list(object = fit, resolution = 30, x_range = c(0.5, 0.9))
  estimate <- function(effect, limit = FALSE, posterior = TRUE) {
    suppressWarnings(do.call(ecx, c(args, list(
      ecx_val = effect, extrapolate = limit, posterior = posterior
    ))))
  }

  interior <- estimate(2)
  extended <- estimate(2, 2)
  identified <- is.finite(interior)
  expect_true(any(identified))
  expect_equal(interior[identified], extended[identified], tolerance = 1e-6)

  expect_true(all(is.na(estimate(20))))
  beyond_data <- estimate(20, 2)
  expect_true(all(is.finite(beyond_data)))
  expect_true(all(beyond_data > 0.9 & beyond_data < 2))

  beyond_limit <- estimate(120, 2, posterior = FALSE)
  expect_equal(as.numeric(beyond_limit), rep(2, 3))
  record <- attr(beyond_limit, "censored_summary")
  expect_identical(record$bound, rep(">=", 3))
  expect_equal(record$upper, 2)
  expect_equal(record$n_above, record$n_draws)
})

test_that("an extension exceeding the grid point guard is refused", {
  x <- seq(0, 1, length.out = 500)
  expect_error(bayesnec:::extend_grid_values(
    x, list(lower = 0, upper = 50)
  ), "20,000 grid points")
})

test_that("model sets forward finite extension to their component curves", {
  args <- list(resolution = 30, x_range = c(0.5, 0.9), extrapolate = 2)
  expect_length(suppressMessages(suppressWarnings(do.call(ecx, c(
    list(object = manec_example), args
  )))), 3)
  expect_length(suppressMessages(suppressWarnings(do.call(nsec, c(
    list(object = manec_example), args
  )))), 3)
  expect_length(suppressMessages(suppressWarnings(do.call(ecnsec, c(
    list(object = manec_example, nsec = 1.5), args
  )))), 3)
})

test_that("hurdle components use one retained grid after extension", {
  fit <- nec4param
  hurdle <- structure(list(
    growth = fit, survival = fit, data = fit$fit$data,
    formula = fit$bayesnecformula, y_var = "y"
  ), class = c("bayesnechurdlefit", "bnecfit"))
  base <- bayesnec:::hurdle_component_preds(
    hurdle, resolution = 20, x_range = c(0.5, 0.9)
  )
  extended <- bayesnec:::hurdle_component_preds(
    hurdle, resolution = 20, x_range = c(0.5, 0.9),
    grid_limits = list(lower = 0.3, upper = 2)
  )
  at <- match(base$x, extended$x)
  expect_identical(extended$x[at], base$x)
  expect_equal(range(extended$x), c(0.3, 2))
  expect_equal(extended$growth[, at], base$growth)
  expect_equal(extended$survival[, at], base$survival)
  expect_equal(extended$combined[, at], base$combined)
  expect_length(suppressWarnings(ecx(
    hurdle, resolution = 20, x_range = c(0.5, 0.9),
    extrapolate = 2, which = "growth"
  )), 3)
  expect_length(suppressWarnings(nsec(
    hurdle, resolution = 20, x_range = c(0.5, 0.9),
    extrapolate = 2, which = "growth"
  )), 3)
  expect_length(suppressWarnings(ecnsec(
    hurdle, nsec = 1.5, resolution = 20, x_range = c(0.5, 0.9),
    extrapolate = 2, which = "growth"
  )), 3)
})

test_that("explicit grids match newdata rows for hurdle and observed groups", {
  fit <- nec4param
  x <- c(0.3, 0.5, 0.6, 0.9, 1.1)
  level <- fit
  level$observed_group <- list(plate = "p1")
  level$level_x <- fit$fit$data$x
  level_grid <- bayesnec:::newdata_eval(
    level, length(x), range(x), x_values = x
  )
  expect_equal(nrow(level_grid$newdata), length(x))
  expect_identical(level_grid$newdata$x, x)
  expect_true(all(level_grid$newdata$plate == "p1"))

  hurdle <- structure(list(
    growth = fit, survival = fit, data = fit$fit$data,
    formula = fit$bayesnecformula, y_var = "y"
  ), class = c("bayesnechurdlefit", "bnecfit"))
  hurdle_grid <- bayesnec:::newdata_eval(
    hurdle, length(x), range(x), x_values = x
  )
  expect_equal(nrow(hurdle_grid$newdata), length(x))
  expect_identical(hurdle_grid$newdata$x, x)
})

test_that("inline log predictors retain original recorded grid points", {
  fit <- suppressMessages(suppressWarnings(
    pull_out(manec_example, model = "ecx4param")
  ))
  fit$bayesnecformula <- bnf(y ~ crf(log(x), model = "ecx4param"))
  base <- bayesnec:::newdata_eval(fit, 21, c(0.5, 0.9))
  lims <- bayesnec:::curve_extrapolate_limits(
    2, fit, c(0.5, 0.9), "own", "ECx"
  )
  extended <- bayesnec:::extended_newdata_eval(fit, 21, c(0.5, 0.9), lims)
  at <- match(base$x_vec, extended$x_vec)
  expect_identical(extended$x_vec[at], base$x_vec)
  expect_identical(extended$newdata$x[at], base$newdata$x)
  expect_equal(diff(extended$x_vec[at]), diff(base$x_vec))
})
