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
  expect_identical(as.numeric(extended)[identified],
                   as.numeric(interior)[identified])

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

test_that("the original grid is searched on its own for an ECx", {
  x <- seq(0, 10, by = 1)
  original <- x >= 3 & x <= 7
  # One crossing below the original grid and one inside it.
  dipping <- c(10, 8, 4, 9, 9, 9, 9, 9, 9, 9, 9)
  expect_equal(bayesnec:::crossing_in_segments(dipping, 5, x, original),
               bayesnec:::crossing_x(dipping, 5, x))
  falling <- c(10, 10, 10, 10, 9, 6, 4, 3, 2, 1, 0)
  expect_identical(
    bayesnec:::crossing_in_segments(falling, 5, x, original),
    bayesnec:::crossing_x(falling[original], 5, x[original])
  )
  late <- c(10, 10, 10, 10, 10, 10, 10, 9, 6, 4, 2)
  expect_equal(bayesnec:::crossing_in_segments(late, 5, x, original),
               bayesnec:::crossing_x(late, 5, x))
  expect_true(is.na(bayesnec:::crossing_in_segments(rep(10, 11), 5, x,
                                                    original)))
})

test_that("a type = \"range\" estimate inside the data ignores a far limit", {
  fit <- suppressMessages(suppressWarnings(
    pull_out(manec_example, model = "ecx4param")
  ))
  expect_identical(
    as.numeric(suppressWarnings(ecx(fit, type = "range", posterior = TRUE,
                                    extrapolate = 10))),
    as.numeric(suppressWarnings(ecx(fit, type = "range", posterior = TRUE)))
  )
  expect_identical(
    as.numeric(ecnsec(fit, nsec = 1, type = "range", posterior = TRUE,
                      extrapolate = 10)),
    as.numeric(ecnsec(fit, nsec = 1, type = "range", posterior = TRUE))
  )
})

test_that("a lower limit is reported once, naming where the search begins", {
  notes <- function(expr) {
    out <- character()
    withCallingHandlers(suppressWarnings(expr), message = function(m) {
      if (grepl("cannot begin below", conditionMessage(m))) {
        out <<- c(out, conditionMessage(m))
      }
      invokeRestart("muffleMessage")
    })
    out
  }
  args <- list(resolution = 50, extrapolate = c(0.016, 5))
  for (estimate in list(nsec, ecx)) {
    expect_length(notes(do.call(estimate, c(list(manec_example), args))), 1L)
  }
  fit <- suppressMessages(suppressWarnings(
    pull_out(manec_example, model = "ecx4param")
  ))
  low <- min(fit$fit$data$x)
  expect_match(notes(nsec(fit, resolution = 50, extrapolate = c(0.016, 5))),
               paste0("below ", signif(low, 3),
                      ", the lowest observed concentration"))
  recorded <- fit
  recorded$x_control <- low
  expect_match(notes(nsec(recorded, resolution = 50,
                          extrapolate = c(0.016, 5))),
               "the recorded control concentration")
  # The search and the censoring bound both start at a supplied control.
  expect_length(notes(nsec(fit, resolution = 50, x_control = 0.01,
                           extrapolate = c(0.01, 5))), 0L)
  expect_match(notes(nsec(fit, resolution = 50, x_control = 0.02,
                          extrapolate = c(0.01, 5))),
               "below 0.02, the control concentration set by x_control")
  # An upper limit alone states no lower limit to report.
  expect_length(notes(nsec(fit, resolution = 50, x_range = c(0.01, 3),
                           extrapolate = 5)), 0L)
})
