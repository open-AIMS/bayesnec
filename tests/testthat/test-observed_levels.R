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
  expect_equal(spec$n, c(2L, 2L))
  expect_equal(vapply(spec$x, length, integer(1)), spec$n)
  expect_equal(spec$excluded, "batch=c")
  fit$fit$data <- d[d$batch == "c", , drop = FALSE]
  expect_null(bayesnec:::observed_level_spec(fit))
})

test_that("excluded-level notes occur once across equations", {
  spec <- list(excluded = "plate=c")
  expect_message(
    bayesnec:::once_level_note({
      bayesnec:::note_excluded_levels(spec)
      bayesnec:::note_excluded_levels(spec)
    }),
    "plate=c"
  )
  messages <- character()
  withCallingHandlers(bayesnec:::once_level_note({
    bayesnec:::note_excluded_levels(spec)
    bayesnec:::note_excluded_levels(spec)
  }), message = function(m) {
    messages <<- c(messages, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  expect_length(messages, 1L)
})

test_that("NULL pool selects separate grouped and joint levels", {
  simple <- function(fit, ...) c(Q50 = fit, Q2.5 = fit, Q97.5 = fit)
  grouped <- list(fits = list(a = 1, b = 2), levels = c("a", "b"))
  g <- bayesnec:::group_estimate_table(grouped, "nec", simple, pool = NULL)
  expect_equal(g$Q50, c(1, 2))
  local_mocked_bindings(
    joint_level_fit = function(object, level) match(level, object$levels),
    .package = "bayesnec"
  )
  joint <- list(levels = c("a", "b"))
  j <- bayesnec:::joint_estimate_table(joint, "nec", simple, pool = NULL)
  expect_equal(j$Q50, c(1, 2))
})

test_that("a mixture gives each posterior draw to one level", {
  idx <- bayesnec:::mixture_draw_indices(c(10, 10, 10), c(1, 1, 2), 10)
  expect_equal(sort(unlist(idx)), 1:10)
  expect_equal(lengths(idx), c(3L, 2L, 5L))
  expect_error(bayesnec:::mixture_draw_indices(c(5, 10), c(1, 1), 8),
               "shortest level")
})

test_that("mixtures over different levels are not paired by level order", {
  share <- function(labels) {
    bayesnec:::mixture_draw_indices(c(500, 500), c(1, 1), 500,
                                    labels = labels)
  }
  expect_identical(share(c("cu1", "cu2")), share(c("cu1", "cu2")))
  first_cu <- seq_len(500) %in% share(c("cu1", "cu2"))[[1]]
  first_zn <- seq_len(500) %in% share(c("zn1", "zn2"))[[1]]
  expect_lt(mean(first_cu == first_zn), 0.9)
})

test_that("a mixture holds draw d of its level at position d", {
  n <- 12L
  out <- bayesnec:::pool_estimate_draws(
    list(a = 100 + seq_len(n), b = 200 + seq_len(n)), c(1, 2), "mixture"
  )
  expect_equal(attr(out, "draw_id"), seq_len(n))
  expect_equal(as.numeric(out) %% 100, seq_len(n))
  expect_equal(as.numeric(out) %/% 100,
               ifelse(attr(out, "level") == "a", 1, 2))
  expect_equal(as.integer(table(attr(out, "level"))), c(4L, 8L))
})

test_that("mixtures over different levels of one posterior pair by draw", {
  # As the copper and zinc plates of one joint refit: every plate of a metal
  # shares that metal's value at draw d.
  n <- 40L
  cu <- stats::setNames(rep(list(seq_len(n)), 3), paste0("cu", 1:3))
  zn <- stats::setNames(rep(list(2 * seq_len(n)), 4), paste0("zn", 1:4))
  mix_cu <- bayesnec:::pool_estimate_draws(cu, c(1, 1, 1), "mixture")
  mix_zn <- bayesnec:::pool_estimate_draws(zn, c(1, 2, 1, 1), "mixture")
  expect_equal(as.numeric(mix_zn) / as.numeric(mix_cu), rep(2, n))
})

test_that("a mixture samples the full posterior when lengths differ", {
  draws <- list(a = 1:4, b = 101:108)
  out <- bayesnec:::pool_estimate_draws(draws, c(1, 3), "mixture", seed = 1)
  level <- attr(out, "level")
  draw_id <- attr(out, "draw_id")
  expect_true(any(draw_id[level == "b"] > 4L))
  expect_equal(as.numeric(out[level == "a"]), draws$a[draw_id[level == "a"]])
  expect_equal(as.numeric(out[level == "b"]), draws$b[draw_id[level == "b"]])
  expect_identical(out, bayesnec:::pool_estimate_draws(
    draws, c(1, 3), "mixture", seed = 1
  ))
})

test_that("a mixture keeps each censoring flag with its draw", {
  a <- c(1, 2, NA_real_, 4)
  attr(a, "censored") <- bayesnec:::censoring_record(
    3, 0, c(FALSE, FALSE, TRUE, FALSE), rep(FALSE, 4))
  b <- c(1.5, 2.5, 2.8, 2.9)
  out <- bayesnec:::pool_estimate_draws(list(a = a, b = b), c(1, 1),
                                        "mixture")
  expect_equal(attr(out, "censored")$above, is.na(out) | out > 3)
  from_a <- attr(out, "level") == "a"
  expect_equal(as.numeric(out)[from_a], a[from_a])
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

test_that("mean_curve NSEC starts at the latest own control", {
  fits <- list(a = list(label = "a"), b = list(label = "b"))
  local_mocked_bindings(
    level_x_bounds = function(object) {
      if (object$label == "a") c(1, 3) else c(2, 3)
    },
    level_curve_draws = function(object, x_vec, ...) {
      list(curve = matrix(rep(c(10, 9, 8), each = 2), nrow = 2),
           control = c(10, 10), asymptote = 0)
    },
    returns_recorded = function(...) TRUE,
    .package = "bayesnec"
  )
  out <- bayesnec:::mean_curve_estimate(
    fits, c(1, 1), "nsec",
    list(x_range = c(0, 3), resolution = 3, posterior = TRUE)
  )
  expect_equal(as.numeric(out), c(2, 2))
  expect_equal(attr(out, "censored")$lower, 2)
})

test_that("mean_curve NSEC censors crossings before the common range", {
  fits <- list(a = list(label = "a"), b = list(label = "b"))
  local_mocked_bindings(
    level_x_bounds = function(object) {
      if (object$label == "a") c(1, 3) else c(2, 3)
    },
    level_curve_draws = function(object, x_vec, ...) {
      second <- if (object$label == "a") c(0, 0, 0) else c(20, 19, 18)
      list(curve = rbind(c(10, 9, 8), second),
           control = c(10, 20), asymptote = 0)
    },
    returns_recorded = function(...) TRUE,
    .package = "bayesnec"
  )
  out <- bayesnec:::mean_curve_estimate(
    fits, c(1, 1), "nsec",
    list(resolution = 3, posterior = TRUE)
  )
  expect_true(is.na(out[2]))
  expect_true(attr(out, "censored")$below[2])
  expect_equal(attr(out, "censored")$lower, 2)
})

test_that("mean_curve NSEC reads a finite extrapolation limit", {
  fits <- list(a = list(), b = list())
  local_mocked_bindings(
    level_x_bounds = function(object) c(0, 3),
    level_curve_draws = function(object, x_vec, ...) {
      list(curve = rbind(10 - 0.5 * x_vec, 12 - 0.5 * x_vec),
           control = c(10, 12), asymptote = 0)
    },
    returns_recorded = function(...) TRUE,
    .package = "bayesnec"
  )
  estimate <- function(extrapolate) bayesnec:::mean_curve_estimate(
    fits, c(1, 1), "nsec",
    list(resolution = 21, posterior = TRUE, extrapolate = extrapolate)
  )
  tested <- estimate(FALSE)
  extended <- estimate(5)
  expect_true(is.na(tested[2]))
  expect_true(is.finite(extended[2]))
  expect_gt(extended[2], 3)
  expect_equal(attr(extended, "censored")$upper, 5)
})

test_that("mean_curve refuses relative effects on a combined hurdle", {
  fits <- list(a = list(fit = list(family = brms::hurdle_poisson())))
  local_mocked_bindings(
    level_x_bounds = function(object) c(0, 3),
    .package = "bayesnec"
  )
  expect_error(
    bayesnec:::mean_curve_estimate(
      fits, 1, "ecx", list(type = "relative", resolution = 3)
    ),
    "combined curve"
  )
  expect_error(
    bayesnec:::mean_curve_estimate(
      fits, 1, "ecnsec",
      list(type = "relative", nsec = 1, resolution = 3)
    ),
    "combined curve"
  )
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

test_that("pooled observed-level ECx retains the value used by plot labels", {
  spec <- list(combinations = data.frame(plate = c("a", "b")),
               n = c(2L, 2L), x = list(c(0, 1), c(0, 1)),
               excluded = character())
  local_mocked_bindings(
    observed_level_fit = function(object, spec, i, with_ne = FALSE) i,
    .package = "bayesnec"
  )
  estimate <- bayesnec:::estimate_observed_levels(
    list(), spec, "ecx", function(level, control, range) rep(0.5, 4),
    pool = "mixture", posterior = FALSE,
    prob_vals = c(0.5, 0.025, 0.975),
    curve_args = list(ecx_val = 10, resolution = 20)
  )
  plotted <- bayesnec:::bind_ecx(data.frame(x = 1:3), estimate)
  expect_equal(plotted$ecx_int[4], 10)
  expect_equal(attr(estimate, "resolution"), 20)
})
