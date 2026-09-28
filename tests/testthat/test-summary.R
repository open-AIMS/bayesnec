# summary(ecx = TRUE) on a single fit and on a model set passes the rest of
# ... to ecx() for each ECx row (#439), by the rule the hurdle summary applies
# (#416): a supplied x_range replaces the stored grid and reaches each ecx()
# call once, x_range = NULL leaves ecx() its own default, and the no-effect
# rows stay on the record stored at fit time.

# Replaces ecx() with a recorder. Each call's arguments are kept as supplied,
# so an argument passed twice appears twice rather than stopping the call as
# the real generic does.
record_ecx_calls <- function(env = parent.frame()) {
  calls <- new.env()
  calls$ecx <- list()
  estimate <- c(Q50 = 1, Q2.5 = 0.5, Q97.5 = 2)
  local_mocked_bindings(
    ecx = function(object, ...) {
      calls$ecx[[length(calls$ecx) + 1]] <- list(...)
      estimate
    },
    .package = "bayesnec",
    .env = env
  )
  calls
}

n_named <- function(args, name) sum(names(args) == name)

# check_fit is a formal of the model-set method only. Given to the single-fit
# method it would sit in ... and be forwarded to ecx(), where the recorder
# would report it. It is set FALSE to skip the posterior simulation, which
# nothing here reads. The warnings muffled are brms's convergence warnings on
# the packaged fits and the censoring warnings of ecx().
summarise <- function(fit, ...) {
  if (inherits(fit, "bayesmanecfit")) {
    suppressWarnings(suppressMessages(summary(fit, ..., check_fit = FALSE)))
  } else {
    suppressWarnings(suppressMessages(summary(fit, ...)))
  }
}

stored_grid <- function(fit) {
  if (inherits(fit, "bayesmanecfit")) fit$w_pred_vals else fit$pred_vals
}

# The stored grid cut to x <= 1, so that it differs from the range of the
# data, 0.032 to 3.22, over which ecx() builds its own grid when given no
# x_range. Only the grid is cut: summary() reads nothing else of it once ecx()
# is mocked.
narrowed_grid <- function(fit) {
  grid <- stored_grid(fit)
  grid$data <- grid$data[grid$data$x <= 1, , drop = FALSE]
  if (inherits(fit, "bayesmanecfit")) {
    fit$w_pred_vals <- grid
  } else {
    fit$pred_vals <- grid
  }
  fit
}

test_that("a supplied x_range reaches each summary ECx call once (#439)", {
  for (fit in list(nec4param, manec_example)) {
    calls <- record_ecx_calls()
    out <- summarise(fit, ecx = TRUE, ecx_vals = c(10, 50),
                     x_range = c(0.5, 1), xform = exp, resolution = 50)
    expect_length(calls$ecx, 2)
    for (args in calls$ecx) {
      expect_equal(n_named(args, "x_range"), 1)
      expect_equal(args$x_range, c(0.5, 1))
      expect_identical(args$xform, exp)
      expect_equal(args$resolution, 50)
    }
    expect_equal(vapply(calls$ecx, `[[`, 0, "ecx_val"), c(10, 50))
    expect_named(out$ecs, c("ECx (10%) estimate:", "ECx (50%) estimate:"))
  }
})

test_that("without x_range the summary ECx rows use the stored grid", {
  for (fit in list(narrowed_grid(nec4param), narrowed_grid(manec_example))) {
    expected <- range(stored_grid(fit)$data$x)
    expect_lte(expected[2], 1)
    calls <- record_ecx_calls()
    summarise(fit, ecx = TRUE, ecx_vals = 50)
    expect_length(calls$ecx, 1)
    # Only the target and the stored grid, as before #439.
    expect_setequal(names(calls$ecx[[1]]), c("ecx_val", "x_range"))
    expect_equal(calls$ecx[[1]]$x_range, expected)
    # resolution and xform reach ecx() beside the stored grid.
    calls <- record_ecx_calls()
    summarise(fit, ecx = TRUE, ecx_vals = 50, resolution = 50, xform = exp)
    args <- calls$ecx[[1]]
    expect_equal(n_named(args, "x_range"), 1)
    expect_equal(args$x_range, expected)
    expect_equal(args$resolution, 50)
    expect_identical(args$xform, exp)
  }
})

test_that("x_range = NULL leaves ecx() its own default, not the stored grid", {
  # As on the hurdle summary: the call reaches ecx() without an x_range, and
  # ecx() then applies its own default of NA and builds its grid from the
  # data.
  for (fit in list(narrowed_grid(nec4param), narrowed_grid(manec_example))) {
    calls <- record_ecx_calls()
    summarise(fit, ecx = TRUE, ecx_vals = 50, x_range = NULL, resolution = 50)
    expect_length(calls$ecx, 1)
    expect_equal(n_named(calls$ecx[[1]], "x_range"), 0)
    expect_equal(calls$ecx[[1]]$resolution, 50)
  }
})

test_that("an ecx_val given to summary() does not displace its arguments", {
  # ecx_val is ecx()'s name for the target, and summary() takes ecx_vals.
  # Matched by partial name to a formal of the internal helper, it was once
  # taken as ecx_vals, and every positional argument moved along one place:
  # the stored range reached ecx() as resolution and c(10, 50, 90) as the
  # grid. It now reaches ecx() beside the target the summary names, and the
  # real generic stops on the pair. That is asserted before ecx() is mocked,
  # since the mock lasts to the end of the test.
  expect_error(summarise(nec4param, ecx = TRUE, ecx_val = 50),
               "matched by multiple actual arguments")
  for (fit in list(nec4param, manec_example)) {
    calls <- record_ecx_calls()
    summarise(fit, ecx = TRUE, ecx_val = 50)
    expect_length(calls$ecx, 3)
    for (args in calls$ecx) {
      expect_equal(n_named(args, "ecx_val"), 2)
      expect_equal(args$x_range, range(stored_grid(fit)$data$x))
      expect_equal(n_named(args, "resolution"), 0)
      expect_equal(sum(names(args) == ""), 0)
    }
  }
})

test_that("the xform for the scale message is matched as ecx() matches it", {
  # By name, by partial name and by position, with x_range named, supplied
  # or NULL, as ecx_row() passes it. A fourth positional argument is xform
  # where x_range is named in the call and x_range where it is not.
  xf <- bayesnec:::summary_ecx_xform
  expect_identical(xf(list()), identity)
  expect_identical(xf(list(xform = exp)), exp)
  expect_identical(xf(list(xfo = exp)), exp)
  expect_identical(xf(list(20, FALSE, "absolute", exp)), exp)
  expect_identical(xf(list(x_range = c(0.5, 1), 20, FALSE, "absolute", exp)),
                   exp)
  expect_identical(xf(list(x_range = NULL, 20, FALSE, "absolute", NA, exp)),
                   exp)
  expect_identical(xf(list(x_range = NULL, 20, FALSE, "absolute", exp)),
                   identity)
})

test_that("the ECx arguments leave the no-effect rows as stored", {
  # The no-effect estimate is read from the posterior stored at fit time and
  # is censored at the grid stored then, so neither x_range nor xform changes
  # it. Without ecx = TRUE nothing is passed to ecx() at all.
  for (fit in list(nec4param, manec_example)) {
    stored <- if (inherits(fit, "bayesmanecfit")) fit$w_ne else fit$ne
    calls <- record_ecx_calls()
    out <- summarise(fit, ecx = TRUE, ecx_vals = 50, x_range = c(0.5, 1),
                     xform = exp)
    expect_equal(as.numeric(out$nec_vals), as.numeric(stored))
    calls <- record_ecx_calls()
    out <- summarise(fit, x_range = c(0.5, 1), xform = exp)
    expect_length(calls$ecx, 0)
    expect_null(out$ecs)
    expect_equal(as.numeric(out$nec_vals), as.numeric(stored))
  }
})

test_that("each summary ECx row equals a bare ecx() call (#439)", {
  skip_on_cran()
  # Over x_range = c(0.5, 1) every draw of the EC50 lies above 1 on both
  # packaged fits, so the row is censored at the supplied upper limit, and
  # xform = exp puts that limit at 2.72. Before #439 the summary ignored both
  # arguments and printed 1.67, unmarked, the value on the stored grid.
  for (fit in list(nec4param, manec_example)) {
    s <- summarise(fit, ecx = TRUE, ecx_vals = 50, x_range = c(0.5, 1),
                   resolution = 50, xform = exp)
    e <- suppressMessages(suppressWarnings(
      ecx(fit, ecx_val = 50, x_range = c(0.5, 1), resolution = 50,
          xform = exp)
    ))
    expect_identical(s$ecs[[1]], e)
    expect_equal(attr(s$ecs[[1]], "resolution"), 50)
    expect_equal(attr(e, "censored_summary")$bound, rep(">=", 3))
    printed <- utils::capture.output(suppressWarnings(print(s)))
    expect_true(any(grepl("Estimate >= 2.72 >= 2.72 >= 2.72", printed,
                          fixed = TRUE)))
    expect_true(any(grepl("draws of the ECx (50%) lie above 2.72", printed,
                          fixed = TRUE)))
  }
})
