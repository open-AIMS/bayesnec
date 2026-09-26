# Structural tests for the bayesnechurdlefit methods. None require sampling:
# they use a mock built from the pieces each method actually reads.
mock_hurdle <- function() {
  mk <- function(model) structure(list(model = model), class = c("bayesnecfit", "bnecfit"))
  structure(list(growth = mk("nec3param"), survival = mk("nec3param"),
                 data = data.frame(x = 1:4, y = c(2, 1, 0, 0)),
                 formula = bnf(y ~ crf(x, "nec3param")),
                 y_var = "y", n_exposed = 4L, n_dead = 2L),
            class = c("bayesnechurdlefit", "bnecfit"))
}

test_that("which defaults to combined and rejects anything else", {
  expect_equal(bayesnec:::hurdle_check_which("combined"), "combined")
  expect_equal(bayesnec:::hurdle_check_which("growth"), "growth")
  expect_error(bayesnec:::hurdle_check_which("mu"), "should be one of")
  # plots additionally allow "all" for the three-panel form
  expect_equal(bayesnec:::hurdle_check_plot_which("all"), "all")
  expect_error(bayesnec:::hurdle_check_which("all"), "should be one of")
})

test_that("combined is the default for every curve method", {
  for (f in list(posterior_epred.bayesnechurdlefit, fitted.bayesnechurdlefit,
                 predict.bayesnechurdlefit, posterior_predict.bayesnechurdlefit,
                 nsec.bayesnechurdlefit, ecx.bayesnechurdlefit,
                 nec.bayesnechurdlefit)) {
    expect_equal(eval(formals(f)$which), "combined")
  }
  expect_equal(eval(formals(plot.bayesnechurdlefit)$which), "combined")
  expect_equal(eval(formals(autoplot.bayesnechurdlefit)$which), "combined")
})

test_that("hurdle_raw_data recovers the response and a transformed predictor", {
  o <- mock_hurdle()
  d <- bayesnec:::hurdle_raw_data(o)
  expect_equal(d$y, c(2, 1, 0, 0))
  expect_equal(d$x, 1:4)
  o$formula <- bnf(y ~ crf(log(x), "nec3param"))
  expect_equal(bayesnec:::hurdle_raw_data(o)$x, log(1:4))
})

test_that("delegating methods return one element per component", {
  o <- mock_hurdle()
  out <- bayesnec:::hurdle_delegate(o, function(z, ...) z$model)
  expect_equal(out, list(growth = "nec3param", survival = "nec3param"))
})

test_that("hurdle amend validates the full predictor before either part (#317)", {
  object <- mock_hurdle()
  object$data$x <- c(1, 2, -1, -2)
  calls <- 0L
  local_mocked_bindings(
    amend = function(...) {
      calls <<- calls + 1L
      stop("component amend should not start")
    },
    .package = "bayesnec"
  )

  expect_error(
    bayesnec:::amend.bayesnechurdlefit(
      object, add = "ecxexp", predictor_scale = "concentration"
    ),
    "requires a non-negative predictor"
  )
  expect_equal(calls, 0L)
})

test_that("combining refuses objects that are not hurdle fits", {
  o <- mock_hurdle()
  plain <- structure(list(model = "nec3param"), class = c("bayesnecfit", "bnecfit"))
  expect_error(bayesnec:::hurdle_check_pair(list(o, plain)), "class bayesnechurdlefit")
  expect_true(bayesnec:::hurdle_check_pair(list(o, o)))
})

test_that("combining refuses fits on different responses", {
  o <- mock_hurdle(); p <- mock_hurdle(); p$y_var <- "z"
  expect_error(bayesnec:::hurdle_check_pair(list(o, p)), "same response")
})

test_that("update() re-splits newdata rather than passing it through", {
  # Zeros must reach the survival component as the hurdle and be withheld from
  # the growth component, exactly as bnec_hurdle() does when first fitting.
  # Checked on the split itself rather than through a fit.
  nd <- data.frame(x = 1:6, y = c(5, 4, 3, 0, 0, 0))
  y <- nd$y
  growth_rows <- nd[y > 0, , drop = FALSE]
  surv <- nd; surv$.alive <- as.integer(y > 0)
  expect_equal(nrow(growth_rows), 3)
  expect_false(any(growth_rows$y == 0))
  expect_equal(nrow(surv), 6)
  expect_equal(surv$.alive, c(1L, 1L, 1L, 0L, 0L, 0L))
  # and update() carries that split, not the raw frame
  # collapsed, because deparse() wraps mid-expression
  src <- paste(deparse(update.bayesnechurdlefit), collapse = " ")
  expect_match(src, "newdata\\[y >\\s*0")
  expect_match(src, "\\.alive")
})

test_that("update() requires the response column in newdata", {
  o <- mock_hurdle()
  expect_error(update(o, newdata = data.frame(x = 1:3)),
               "must contain the response column")
})

test_that("update() re-checks the zero-vs-censored invariant on newdata", {
  # The invariant is a property of the data, so a refit has to re-check it.
  # Otherwise a row that is both zero and censored is dropped from the growth
  # refit and coded as a death in the survival refit, with no message.
  o <- mock_hurdle()
  o$formula <- bnf(y | cens(cens) ~ crf(x, "nec3param"))
  bad <- data.frame(x = 1:6, y = c(5, 4, 3, 0, 0, 0),
                    cens = c(rep("none", 5), "left"))
  expect_error(update(o, newdata = bad),
               "zero and also carry a censoring code")
  # A censored survivor is a growth observation and must still get past the
  # check. The mock has no real component fit, so update() fails regardless --
  # the point is only that it does not fail on the invariant.
  ok <- bad
  ok$cens <- c("left", rep("none", 5))
  msg <- tryCatch(update(o, newdata = ok), error = conditionMessage)
  expect_false(grepl("zero and also carry", msg))
})

test_that("summary carries its own class and print method", {
  expect_true(!is.null(getS3method("print", "hurdlesummary", optional = TRUE)))
  expect_true(!is.null(getS3method("summary", "bayesnechurdlefit", optional = TRUE)))
})

test_that("every audited method has a bayesnechurdlefit method or a branch", {
  # Guards against a method being added for bayesnecfit later without a
  # matching hurdle path, which is how the silent failures arose originally.
  s3 <- c("summary", "print", "plot", "autoplot", "ggbnec_data", "predict",
          "fitted", "posterior_epred", "posterior_predict", "nsec", "nec",
          "ecx", "ecnsec", "rhat", "check_chains", "check_priors",
          "model.frame", "pull_brmsfit", "bnec_newdata",
          "update", "amend", "c")
  for (g in s3) {
    expect_true(!is.null(getS3method(g, "bayesnechurdlefit", optional = TRUE)),
                label = paste0(g, ".bayesnechurdlefit"))
  }
  # pull_out, dispersion and compare_fitted are plain functions, not generics,
  # so an S3 method for them would be silently inert -- they branch internally.
  for (f in list(bayesnec:::pull_out, bayesnec:::dispersion,
                 bayesnec:::newdata_eval_fitted)) {
    expect_true(any(grepl("is_bayesnechurdlefit", deparse(f))))
  }
  for (f in c("pull_out", "dispersion", "compare_fitted")) {
    expect_false(any(grepl("UseMethod", deparse(get(f)))), label = f)
  }
})


# ---- D15 ruling 2 on the two-block class -------------------------------------

# The three estimator methods took the control from p_samples[, 1], the first
# column of the prediction grid, so x_range moved the reference and therefore
# every estimate. hurdle_component_preds() now returns a control read at the
# lowest observed concentration and pinned to the same concentration on both
# sides. Asserted by mocking that function, because reproducing the defect by
# fitting needs two brms fits and a grid extended below the data.

hurdle_preds_stub <- function(control_at, curve_at_grid_start) {
  # A declining curve on a grid whose first column is NOT the control: this is
  # the situation an extended x_range creates. A method reading p_samples[, 1]
  # gets curve_at_grid_start; one reading preds$control gets control_at.
  x <- seq(1, 10, length.out = 10)
  g <- matrix(rep(seq(curve_at_grid_start, 0.1, length.out = 10), each = 4),
              nrow = 4)
  function(object, resolution = 1000, x_range = NA) {
    list(x = x, growth = g, survival = g, combined = g,
         control = list(growth = rep(control_at, 4),
                        survival = rep(control_at, 4),
                        combined = rep(control_at, 4)))
  }
}

test_that("ecx reads the control from preds$control, not the grid start", {
  obj <- mock_hurdle()
  # Grid starts at 2 but the control is 8. An EC50 measured from the control is
  # reached at 4; measured from the grid start it is reached much earlier.
  local_mocked_bindings(
    hurdle_component_preds = hurdle_preds_stub(control_at = 8,
                                               curve_at_grid_start = 2)
  )
  out <- ecx(obj, ecx_val = 50, posterior = TRUE)
  # The target is 4, which the curve (2 down to 0.1) never reaches, so every
  # draw is NA rather than an estimate read from the wrong reference.
  expect_true(all(is.na(out)))
})

test_that("the estimator methods survive a censored draw", {
  obj <- mock_hurdle()
  local_mocked_bindings(
    hurdle_component_preds = hurdle_preds_stub(control_at = 8,
                                               curve_at_grid_start = 2)
  )
  # quantile() without na.rm stopped with "missing values and NaN's not
  # allowed" here, on a vector the package had just built.
  est <- suppressWarnings(ecx(obj, ecx_val = 50))
  expect_equal(length(est), 3)
  expect_warning(ecx(obj, ecx_val = 50), "not identified for 4 of 4 draws")
})

test_that("ecnsec honours type and refuses the two it cannot compute", {
  obj <- mock_hurdle()
  local_mocked_bindings(
    hurdle_component_preds = hurdle_preds_stub(control_at = 8,
                                               curve_at_grid_start = 2)
  )
  # absolute measures control to 0; range measures control to the curve's
  # lowest predicted response, a shorter span, so the same decline is a larger
  # percentage of it. The version this replaces returned the absolute answer
  # whatever type was given.
  abs_out <- ecnsec(obj, nsec = 5, type = "absolute")
  rng_out <- ecnsec(obj, nsec = 5, type = "range")
  expect_gt(rng_out[1], abs_out[1])
  expect_error(ecnsec(obj, nsec = 5, type = "relative"), "not defined for a")
  expect_error(ecnsec(obj, nsec = 5, type = "direct"), "names a response value")
  expect_error(ecnsec(obj, nsec = 5, type = "nonsense"), "type must be one of")
  expect_error(ecnsec(obj, nsec = 5, hormesis_def = "max"),
               "hormesis_def has been removed")
})

test_that("the summary ECx grid is read from whichever class a block is", {
  # A hurdle component is a bayesnecfit or a bayesmanecfit depending on whether
  # crf() named one equation or a set, and the two store their prediction grid
  # under different names. Reading pred_vals alone returned NULL for the
  # commoner case, range() of nothing is c(Inf, -Inf), and summary(ecx = TRUE)
  # then errored inside seq() (#395).
  mk <- function(grid, slot) {
    out <- list()
    out[[slot]] <- list(data = data.frame(x = grid))
    out
  }
  both_sets <- list(growth = mk(c(1, 4), "w_pred_vals"),
                    survival = mk(c(0.5, 6), "w_pred_vals"))
  # The intersection, which is the range both curves are defined over and the
  # bound combine_censored_min() gives the combined no-effect estimate.
  expect_equal(bayesnec:::hurdle_summary_range(both_sets), c(1, 4))
  mixed <- list(growth = mk(c(1, 4), "pred_vals"),
                survival = mk(c(0.5, 6), "w_pred_vals"))
  expect_equal(bayesnec:::hurdle_summary_range(mixed), c(1, 4))
  # No stored grid on either side: NULL, which leaves ecx() to build its own
  # rather than being handed a range it cannot use.
  expect_null(bayesnec:::hurdle_summary_range(list(growth = list(),
                                                   survival = list())))
  expect_null(bayesnec:::hurdle_summary_range(
    list(growth = mk(c(1, 4), "pred_vals"), survival = list())
  ))
})

# A component carrying only what summary.bayesnechurdlefit() reads once ecx()
# and nec() are mocked: the stored prediction grid, the equation names, the
# no-effect type and the family. grid = NULL stores no grid.
summary_component <- function(grid, averaged = FALSE) {
  fit <- list(family = list(family = "gaussian"))
  stored <- if (is.null(grid)) NULL else list(data = data.frame(x = grid))
  if (averaged) {
    out <- list(mod_fits = list(nec3param = list(fit = fit),
                                ecx4param = list(fit = fit)),
                ne_type = "N(S)EC", w_pred_vals = stored)
    structure(out, class = c("bayesmanecfit", "bnecfit"))
  } else {
    out <- list(model = "nec3param", ne_type = "NEC", fit = fit,
                pred_vals = stored)
    structure(out, class = c("bayesnecfit", "bnecfit"))
  }
}

summary_hurdle <- function(growth, survival) {
  structure(list(growth = growth, survival = survival,
                 n_exposed = 40L, n_dead = 10L),
            class = c("bayesnechurdlefit", "bnecfit"))
}

# Replaces ecx() and nec() with recorders. Each call's arguments are kept as
# supplied, so an argument passed twice appears twice rather than stopping the
# call as the real generic does.
record_summary_calls <- function(env = parent.frame()) {
  calls <- new.env()
  calls$ecx <- list()
  calls$nec <- list()
  estimate <- c(Estimate = 1, Q2.5 = 0.5, Q97.5 = 2)
  local_mocked_bindings(
    ecx = function(object, ...) {
      calls$ecx[[length(calls$ecx) + 1]] <- list(...)
      estimate
    },
    nec = function(object, ...) {
      calls$nec[[length(calls$nec) + 1]] <- list(...)
      estimate
    },
    .package = "bayesnec",
    .env = env
  )
  calls
}

n_named <- function(args, name) sum(names(args) == name)

test_that("an explicit x_range reaches each summary ECx call once (#416)", {
  # Every pairing of a single-equation and a model-averaged component, since
  # the two classes store their grid under different names.
  for (g_avg in c(FALSE, TRUE)) {
    for (s_avg in c(FALSE, TRUE)) {
      o <- summary_hurdle(summary_component(c(0, 10), g_avg),
                          summary_component(c(0, 40), s_avg))
      calls <- record_summary_calls()
      # Both limits differ from the default of c(0, 10), so a range cut back
      # to the intersection of the stored grids would show.
      out <- summary(o, ecx = TRUE, ecx_vals = c(10, 50),
                     x_range = c(0.5, 40), xform = exp, resolution = 50)
      expect_s3_class(out, "hurdlesummary")
      expect_length(calls$ecx, 6)
      for (args in calls$ecx) {
        expect_equal(n_named(args, "x_range"), 1)
        expect_equal(args$x_range, c(0.5, 40))
        expect_identical(args$xform, exp)
        expect_equal(args$resolution, 50)
      }
      expect_setequal(vapply(calls$ecx, `[[`, "", "which"),
                      c("combined", "growth", "survival"))
      expect_setequal(vapply(calls$ecx, `[[`, 0, "ecx_val"), c(10, 50))
      # The no-effect rows are read from the posterior stored at fit time, so
      # x_range is withheld from nec() while the rest of ... still reaches it.
      expect_length(calls$nec, 3)
      for (args in calls$nec) {
        expect_equal(n_named(args, "x_range"), 0)
        expect_identical(args$xform, exp)
        expect_equal(args$resolution, 50)
      }
    }
  }
})

test_that("without x_range the summary ECx rows use the stored grids", {
  for (g_avg in c(FALSE, TRUE)) {
    for (s_avg in c(FALSE, TRUE)) {
      o <- summary_hurdle(summary_component(c(1, 10), g_avg),
                          summary_component(c(0, 40), s_avg))
      calls <- record_summary_calls()
      summary(o, ecx = TRUE, ecx_vals = 50, xform = exp, resolution = 50)
      expect_length(calls$ecx, 3)
      for (args in calls$ecx) {
        expect_equal(n_named(args, "x_range"), 1)
        expect_equal(args$x_range, c(1, 10))
        expect_identical(args$xform, exp)
        expect_equal(args$resolution, 50)
      }
    }
  }
})

test_that("a component with no stored grid leaves ecx() its own default", {
  o <- summary_hurdle(summary_component(NULL),
                      summary_component(c(0, 40), averaged = TRUE))
  calls <- record_summary_calls()
  summary(o, ecx = TRUE, ecx_vals = 50)
  expect_length(calls$ecx, 3)
  for (args in calls$ecx) {
    expect_equal(n_named(args, "x_range"), 0)
  }
  # A supplied range still applies where there is no default to replace.
  calls <- record_summary_calls()
  summary(o, ecx = TRUE, ecx_vals = 50, x_range = c(0.5, 40))
  for (args in calls$ecx) {
    expect_equal(n_named(args, "x_range"), 1)
    expect_equal(args$x_range, c(0.5, 40))
  }
})

test_that("x_range without ecx = TRUE reaches neither estimator", {
  o <- summary_hurdle(summary_component(c(0, 10)),
                      summary_component(c(0, 40)))
  calls <- record_summary_calls()
  expect_no_error(summary(o, x_range = c(0.5, 40)))
  expect_length(calls$ecx, 0)
  expect_length(calls$nec, 3)
  for (args in calls$nec) {
    expect_equal(n_named(args, "x_range"), 0)
  }
})
