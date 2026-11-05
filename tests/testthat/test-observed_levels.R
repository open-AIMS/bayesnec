test_that("only groupings observed across concentrations enter the curve", {
  d <- data.frame(x = rep(c(0, 1, 2), each = 4), y = 1,
                  plate = rep(c("a", "b"), 6),
                  cell = seq_len(12))
  fit <- nec4param
  fit$fit$data <- d
  fit$fit$formula <- list(
    formula = y ~ mu,
    pforms = list(beta = beta ~ 1 + (1 | plate),
                  ogl = ogl ~ 1 + (1 | cell))
  )
  spec <- bayesnec:::observed_level_spec(fit)
  expect_equal(nrow(spec$combinations), 2L)
  expect_identical(names(spec$combinations), "plate")
  expect_equal(spec$n, c(6L, 6L))
  expect_match(deparse1(spec$re_formula), "plate")
  expect_false(grepl("cell", deparse1(spec$re_formula)))
})

test_that("a grouping with incomplete series still spans the series", {
  d <- data.frame(x = c(0, 1, 0, 2, 1), y = 1,
                  batch = c("a", "a", "b", "b", "c"))
  fit <- nec4param
  fit$fit$data <- d
  fit$fit$formula <- list(formula = y ~ mu,
                          pforms = list(beta = beta ~ 1 + (1 | batch)))
  spec <- bayesnec:::observed_level_spec(fit)
  expect_equal(spec$n, c(2L, 2L, 1L))
  expect_equal(vapply(spec$x, length, integer(1)), spec$n)
})

test_that("a numeric control changes the reference and rejects log zero", {
  skip_if(Sys.getenv("NOT_CRAN") == "")
  fit <- suppressMessages(pull_out(manec_example, model = "ecx4param"))
  low <- bayesnec:::control_x(fit)
  expect_equal(ecx(fit, x_control = low), ecx(fit))
  expect_equal(nsec(fit, x_control = low), nsec(fit))
  expect_warning(nsec(fit, x_control = 0),
                 "below the lowest observed concentration")
  logged <- fit
  logged$bayesnecformula <- bayesnecformula(
    y ~ crf(log(x), model = "ecx4param")
  )
  expect_error(nsec(logged, x_control = 0),
               "not finite under the crf\\(\\) predictor transformation")
})

test_that("a shared control uses the same grid for grouped levels", {
  fit <- suppressMessages(pull_out(manec_example, model = "ecx4param"))
  other <- fit
  other$fit$data$x <- other$fit$data$x + 0.1
  fits <- list(a = fit, b = other)
  expect_warning(
    dots <- bayesnec:::shared_level_control(fits, list(x_control = "shared")),
    "1 level\\(s\\)"
  )
  expect_equal(as.numeric(dots$x_control), bayesnec:::control_x(fit))
  expect_equal(dots$x_range[2], max(other$fit$data$x))
})

test_that("mean_curve reads the average curve before finding ECx", {
  fits <- list(a = list(label = "a"), b = list(label = "b"))
  local_mocked_bindings(
    level_x_bounds = function(object) c(0, 2),
    level_curve_draws = function(object, x_vec, ...) {
      if (object$label == "a") {
        list(curve = rbind(c(10, 9, 8), c(10, 9, 8)),
             control = c(10, 10), asymptote = 0)
      } else {
        list(curve = rbind(c(20, 15, 10), c(20, 15, 10)),
             control = c(20, 20), asymptote = 0)
      }
    },
    returns_recorded = function(...) TRUE,
    .package = "bayesnec"
  )
  out <- bayesnec:::mean_curve_estimate(
    fits, c(1, 1), "ecx",
    list(ecx_val = 10, resolution = 3, posterior = FALSE)
  )
  expect_equal(unname(as.numeric(out["Q50"])), 0.5)
  mean_estimate <- bayesnec:::pool_estimate_draws(
    list(a = c(1, 1), b = c(0.4, 0.4)), c(1, 1), "mean_estimate"
  )
  expect_equal(mean_estimate, c(0.7, 0.7))
})

test_that("saved population displays identify their earlier prediction basis", {
  fit <- nec4param
  fit$pred_vals <- list(data = data.frame(x = c(0, 1)))
  local_mocked_bindings(
    observed_level_spec = function(object) list(combinations = 1),
    .package = "bayesnec"
  )
  expect_warning(bayesnec:::warn_stale_observed_display(fit),
                 "saved before observed-level display values")
  fit$pred_vals$observed_levels <- TRUE
  expect_silent(bayesnec:::warn_stale_observed_display(fit))
})
