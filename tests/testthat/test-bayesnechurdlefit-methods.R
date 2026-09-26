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
  #
  # The growth grid is clipped to growth's observed range, which is built
  # from a fitted component; here each mock carries it as `observed`, equal to
  # its grid unless stated.
  local_mocked_bindings(
    hurdle_estimate_range = function(object, which, x_range) {
      object$growth$observed
    },
    .package = "bayesnec"
  )
  mk <- function(grid, slot) {
    out <- list(observed = range(grid))
    out[[slot]] <- list(data = data.frame(x = grid))
    out
  }
  range_of <- function(o, w) bayesnec:::hurdle_summary_range(o, w)
  both_sets <- list(growth = mk(c(1, 4), "w_pred_vals"),
                    survival = mk(c(0.5, 6), "w_pred_vals"))
  # Growth's own grid for the growth curve, and survival's for the survival
  # and combined curves, which is the rule a bare ecx() applies (#412). The
  # intersection, c(1, 4), cut the survival and combined rows at growth's top.
  expect_equal(range_of(both_sets, "growth"), c(1, 4))
  expect_equal(range_of(both_sets, "survival"), c(0.5, 6))
  expect_equal(range_of(both_sets, "combined"), c(0.5, 6))
  mixed <- list(growth = mk(c(1, 4), "pred_vals"),
                survival = mk(c(0.5, 6), "w_pred_vals"))
  expect_equal(range_of(mixed, "growth"), c(1, 4))
  expect_equal(range_of(mixed, "combined"), c(0.5, 6))
  # No stored grid: NULL, which leaves ecx() to build its own rather than
  # being handed a range it cannot use. Each curve depends on its own
  # component's grid alone.
  for (w in c("combined", "growth", "survival")) {
    expect_null(range_of(list(growth = list(), survival = list()), w))
  }
  growth_only <- list(growth = mk(c(1, 4), "pred_vals"), survival = list())
  expect_equal(range_of(growth_only, "growth"), c(1, 4))
  expect_null(range_of(growth_only, "survival"))
  expect_null(range_of(growth_only, "combined"))
})

test_that("the summary's growth grid is clipped to growth's observed range", {
  # bnec_hurdle() passes one x_range to both components, so a range given at
  # fit time is stored as growth's grid as well, past the highest
  # concentration at which anything survived (#412).
  local_mocked_bindings(
    hurdle_estimate_range = function(object, which, x_range) {
      object$growth$observed
    },
    .package = "bayesnec"
  )
  mk <- function(grid, observed) {
    list(observed = observed, pred_vals = list(data = data.frame(x = grid)))
  }
  range_of <- function(o, w) bayesnec:::hurdle_summary_range(o, w)
  survival <- mk(c(0, 40), c(0, 40))
  # Wider than the growth data: cut to them. The survival grid is kept.
  widened <- list(growth = mk(c(0, 40), c(0, 10)), survival = survival)
  expect_equal(range_of(widened, "growth"), c(0, 10))
  expect_equal(range_of(widened, "survival"), c(0, 40))
  expect_equal(range_of(widened, "combined"), c(0, 40))
  # Narrower than the growth data: the fit was asked for that range, and it is
  # kept.
  narrowed <- list(growth = mk(c(1, 5), c(0, 10)), survival = survival)
  expect_equal(range_of(narrowed, "growth"), c(1, 5))
  # Wholly outside the growth data: nothing to clip to, and ecx() is left to
  # build growth's observed range itself.
  apart <- list(growth = mk(c(20, 40), c(0, 10)), survival = survival)
  expect_null(range_of(apart, "growth"))
})

# A component carrying only what summary.bayesnechurdlefit() reads once ecx()
# and nec() are mocked: the stored prediction grid, the equation names, the
# no-effect type and the family. grid = NULL stores no grid. observed stands
# for the range of the data the component was fitted to, which
# record_summary_calls() reports in place of building it from a fit.
summary_component <- function(grid, averaged = FALSE, observed = grid) {
  fit <- list(family = list(family = "gaussian"))
  stored <- if (is.null(grid)) NULL else list(data = data.frame(x = grid))
  if (averaged) {
    out <- list(mod_fits = list(nec3param = list(fit = fit),
                                ecx4param = list(fit = fit)),
                ne_type = "N(S)EC", w_pred_vals = stored, observed = observed)
    structure(out, class = c("bayesmanecfit", "bnecfit"))
  } else {
    out <- list(model = "nec3param", ne_type = "NEC", fit = fit,
                pred_vals = stored, observed = observed)
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
# call as the real generic does. hurdle_estimate_range() reports the growth
# component's observed range from the mock rather than from a fit.
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
    hurdle_estimate_range = function(object, which, x_range) {
      object$growth$observed
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
      # Both limits differ from the growth rows' default of c(0, 10), and the
      # lower one from the other rows' c(0, 40), so a default left in place of
      # the supplied range would show.
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

test_that("without x_range each summary ECx row uses its own stored grid", {
  # Growth's grid for the growth rows and survival's for the survival and
  # combined rows (#412). Before, all three read the intersection, c(1, 10),
  # which cut the survival and combined rows at growth's top.
  expected <- list(combined = c(0, 40), growth = c(1, 10),
                   survival = c(0, 40))
  for (g_avg in c(FALSE, TRUE)) {
    for (s_avg in c(FALSE, TRUE)) {
      o <- summary_hurdle(summary_component(c(1, 10), g_avg),
                          summary_component(c(0, 40), s_avg))
      calls <- record_summary_calls()
      summary(o, ecx = TRUE, ecx_vals = 50, xform = exp, resolution = 50)
      expect_length(calls$ecx, 3)
      for (args in calls$ecx) {
        expect_equal(n_named(args, "x_range"), 1)
        expect_equal(args$x_range, expected[[args$which]])
        expect_identical(args$xform, exp)
        expect_equal(args$resolution, 50)
      }
      expect_setequal(vapply(calls$ecx, `[[`, "", "which"), names(expected))
    }
  }
  # Both components given a fit-time x_range of c(0, 40), which reaches past
  # growth's data at c(1, 10): the growth rows are cut to the data, and the
  # others keep the stored grid.
  o <- summary_hurdle(summary_component(c(0, 40), observed = c(1, 10)),
                      summary_component(c(0, 40)))
  calls <- record_summary_calls()
  summary(o, ecx = TRUE, ecx_vals = 50)
  expected$growth <- c(1, 10)
  for (args in calls$ecx) {
    expect_equal(args$x_range, expected[[args$which]])
  }
})

test_that("a component with no stored grid leaves ecx() its own default", {
  # Only the rows read over that component's grid lose their x_range; the
  # others keep the grid of the component that has one.
  o <- summary_hurdle(summary_component(NULL),
                      summary_component(c(0, 40), averaged = TRUE))
  calls <- record_summary_calls()
  summary(o, ecx = TRUE, ecx_vals = 50)
  expect_length(calls$ecx, 3)
  for (args in calls$ecx) {
    if (identical(args$which, "growth")) {
      expect_equal(n_named(args, "x_range"), 0)
    } else {
      expect_equal(n_named(args, "x_range"), 1)
      expect_equal(args$x_range, c(0, 40))
    }
  }
  # The survival component without one: the survival and combined rows are
  # left to ecx(), and the growth rows keep growth's grid.
  o <- summary_hurdle(summary_component(c(1, 10)), summary_component(NULL))
  calls <- record_summary_calls()
  summary(o, ecx = TRUE, ecx_vals = 50)
  expect_length(calls$ecx, 3)
  for (args in calls$ecx) {
    if (identical(args$which, "growth")) {
      expect_equal(n_named(args, "x_range"), 1)
      expect_equal(args$x_range, c(1, 10))
    } else {
      expect_equal(n_named(args, "x_range"), 0)
    }
  }
  # A supplied range still applies where there is no default to replace.
  calls <- record_summary_calls()
  summary(o, ecx = TRUE, ecx_vals = 50, x_range = c(0.5, 40))
  for (args in calls$ecx) {
    expect_equal(n_named(args, "x_range"), 1)
    expect_equal(args$x_range, c(0.5, 40))
  }
})

test_that("x_range = NULL leaves ecx() its own default, not the stored grids", {
  # Both components store a grid, so the summary's default would be c(0, 10)
  # for the growth rows and c(0, 40) for the others.
  # An explicit NULL is not that default: the call reaches ecx() without an
  # x_range, and ecx() then applies its own default of NA and builds its grid.
  o <- summary_hurdle(summary_component(c(0, 10)),
                      summary_component(c(0, 40), averaged = TRUE))
  calls <- record_summary_calls()
  summary(o, ecx = TRUE, ecx_vals = 50, x_range = NULL, resolution = 50)
  expect_length(calls$ecx, 3)
  for (args in calls$ecx) {
    expect_equal(n_named(args, "x_range"), 0)
    expect_equal(args$resolution, 50)
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

# ---- The range each hurdle estimate is read over (#412) ----------------------

# A hurdle fit whose growth component stops short of the survival component,
# built from the packaged nec4param fit rather than by sampling. The growth
# component keeps only the rows at or below cut, as a fit to survivors does
# where nothing lived above cut, and stores a grid over those rows, as bnec()
# does. The survival component keeps every row, so its range runs to 3.22.
# Both components share the one set of draws, which is enough here: what is
# asserted is which range each curve is read over.
hurdle_cut_fixture <- function(cut) {
  growth <- nec4param
  growth$fit$data <- growth$fit$data[growth$fit$data$x <= cut, , drop = FALSE]
  xg <- growth$fit$data$x
  growth$pred_vals$data <- data.frame(x = seq(min(xg), max(xg),
                                              length.out = 100))
  structure(list(growth = growth, survival = nec4param,
                 data = nec4param$fit$data,
                 formula = nec4param$bayesnecformula, y_var = "y",
                 n_exposed = nrow(nec4param$fit$data), n_dead = 0L),
            class = c("bayesnechurdlefit", "bnecfit"))
}

test_that("a supplied x_range is kept, and only growth is given its own", {
  # No object is read unless the growth range has to be built.
  expect_true(is.na(bayesnec:::hurdle_estimate_range(list(), "survival", NA)))
  expect_true(is.na(bayesnec:::hurdle_estimate_range(list(), "combined", NA)))
  expect_equal(bayesnec:::hurdle_estimate_range(list(), "growth", c(0, 9)),
               c(0, 9))
})

test_that("summary() and a bare ecx() agree for every curve (#412)", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  obj <- hurdle_cut_fixture(1.6)
  s <- suppressWarnings(summary(obj, ecx = TRUE, ecx_vals = c(10, 50, 90)))
  for (w in c("combined", "growth", "survival")) {
    for (v in c(10, 50, 90)) {
      bare <- suppressWarnings(ecx(obj, ecx_val = v, which = w))
      expect_equal(s$ecs[[w]][[paste0("ec", v)]], bare,
                   label = paste(w, "EC", v))
    }
  }
  # Before #412 the summary read every row over the intersection of the two
  # grids, which ends at growth's top, so its survival EC50 was censored there
  # while a bare ecx() identified it above that.
  g_top <- max(obj$growth$fit$data$x)
  expect_gt(s$ecs$survival$ec50[["Q50"]], g_top)
  expect_null(attr(s$ecs$survival$ec50, "censored_summary"))
})

test_that("a growth ECx beyond growth's range is censored there (#412)", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  obj <- hurdle_cut_fixture(1.6)
  g_top <- max(obj$growth$fit$data$x)
  expect_warning(est <- ecx(obj, ecx_val = 50, which = "growth"),
                 "not identified")
  cs <- attr(est, "censored_summary")
  # The bound is growth's highest concentration, not survival's 3.22.
  expect_equal(cs$upper, g_top)
  expect_equal(cs$bound[1], ">=")
  expect_equal(est[["Q50"]], g_top)
  # Read over the survival range, the same curve gives a number above growth's
  # top, which is what a bare call reported before and what is now withheld.
  s_range <- range(obj$survival$fit$data$x)
  extended <- ecx(obj, ecx_val = 50, which = "growth", x_range = s_range)
  expect_gt(extended[["Q50"]], g_top)
  expect_null(attr(extended, "censored_summary"))
  # An EC10 below growth's top is identified as before.
  inside <- ecx(obj, ecx_val = 10, which = "growth")
  expect_lt(inside[["Q50"]], g_top)
  expect_null(attr(inside, "censored_summary"))
})

test_that("the combined ECx is read beyond growth's range (#412)", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  obj <- hurdle_cut_fixture(1.6)
  g_top <- max(obj$growth$fit$data$x)
  est <- ecx(obj, ecx_val = 90, which = "combined")
  expect_gt(est[["Q2.5"]], g_top)
  expect_null(attr(est, "censored_summary"))
  s_range <- range(obj$survival$fit$data$x)
  expect_equal(est, ecx(obj, ecx_val = 90, which = "combined",
                        x_range = s_range))
  expect_equal(ecx(obj, ecx_val = 50, which = "survival"),
               ecx(obj, ecx_val = 50, which = "survival", x_range = s_range))
})

test_that("a growth NSEC is searched over growth's range (#412)", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # Cut below the NSEC of nec4param, about 1.48, so that most draws have not
  # reached the reference by growth's top.
  obj <- hurdle_cut_fixture(1.4)
  g_top <- max(obj$growth$fit$data$x)
  expect_warning(est <- nsec(obj, which = "growth"), "not identified")
  cs <- attr(est, "censored_summary")
  expect_equal(cs$upper, g_top)
  expect_equal(cs$bound[1], ">=")
  # extrapolate extends the range the call would otherwise search, which for
  # growth is growth's own. A limit between growth's top and survival's is
  # therefore an extension for growth and is accepted, and inside the range
  # for survival and is refused.
  extended <- nsec(obj, which = "growth", extrapolate = 2)
  expect_gt(extended[["Q50"]], g_top)
  expect_equal(extended, nsec(obj, which = "growth", x_range = c(
    min(obj$growth$fit$data$x), 2
  )), ignore_attr = TRUE)
  expect_error(nsec(obj, which = "survival", extrapolate = 2),
               "inside the prediction range")
})

test_that("ecnsec reads the growth curve over growth's range (#412)", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # type = "range" measures towards the lowest response over the grid, so the
  # grid the curve is read over shows in the answer.
  obj <- hurdle_cut_fixture(1.6)
  g_range <- range(obj$growth$fit$data$x)
  s_range <- range(obj$survival$fit$data$x)
  bare <- ecnsec(obj, nsec = 1.5, which = "growth", type = "range")
  expect_equal(bare, ecnsec(obj, nsec = 1.5, which = "growth",
                            type = "range", x_range = g_range))
  expect_false(isTRUE(all.equal(
    bare, ecnsec(obj, nsec = 1.5, which = "growth", type = "range",
                 x_range = s_range)
  )))
})

test_that("the plots and posterior_epred() keep the survival range (#412)", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # A plotted growth curve beyond its data is a prediction, not an estimate,
  # so it is drawn over every concentration tested.
  obj <- hurdle_cut_fixture(1.6)
  s_range <- range(obj$survival$fit$data$x)
  dat <- ggbnec_data(obj, which = "growth", resolution = 20)
  expect_equal(range(dat$curve$x), s_range)
  expect_equal(posterior_epred(obj, which = "growth", resolution = 20),
               posterior_epred(obj, which = "growth", resolution = 20,
                               x_range = s_range))
})

test_that("ecnsec refuses a growth value beyond growth's range (#412)", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # The curve is read at the grid point nearest nsec, so a value above growth's
  # top was read at the top without a message: nsec = 2.5 returned the effect
  # at 1.595. A survival NSEC is the likely such value, since survival's range
  # runs past growth's.
  obj <- hurdle_cut_fixture(1.4)
  g_top <- max(obj$growth$fit$data$x)
  expect_error(ecnsec(obj, nsec = 2.5, which = "growth"),
               "outside the observed range of the growth component")
  s_nsec <- suppressWarnings(nsec(obj, which = "survival"))[["Q50"]]
  expect_gt(s_nsec, g_top)
  expect_error(ecnsec(obj, nsec = s_nsec, which = "growth"),
               paste0("to ", signif(g_top, 4)))
  # A value inside growth's range, at its top included, is read as before.
  expect_length(ecnsec(obj, nsec = g_top, which = "growth"), 3)
  # The survival and combined curves are read over every concentration tested,
  # and a range the caller supplies is theirs to set: an x_range that reaches
  # nsec reads the growth curve extended past its data, as ecx() does.
  expect_length(ecnsec(obj, nsec = 2.5, which = "survival"), 3)
  expect_length(ecnsec(obj, nsec = 2.5, which = "combined"), 3)
  s_range <- range(obj$survival$fit$data$x)
  expect_length(ecnsec(obj, nsec = 2.5, which = "growth", x_range = s_range),
                3)
})

test_that("a fit-time x_range does not carry the growth rows past the data", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # bnec_hurdle(x_range = ) stores the one range on both components. Both
  # stored grids are widened to 4 here, above survival's data as well as
  # growth's. Read over the stored grid, the summary's growth EC50 was an
  # identified 1.674 while a bare ecx() censored it at growth's top (#412).
  obj <- hurdle_cut_fixture(1.6)
  lower <- min(obj$survival$fit$data$x)
  wide <- data.frame(x = seq(lower, 4, length.out = 100))
  obj$growth$pred_vals$data <- wide
  obj$survival$pred_vals$data <- wide
  s <- suppressWarnings(summary(obj, ecx = TRUE, ecx_vals = c(10, 50)))
  for (v in c(10, 50)) {
    bare <- suppressWarnings(ecx(obj, ecx_val = v, which = "growth"))
    expect_equal(s$ecs$growth[[paste0("ec", v)]], bare)
  }
  cs <- attr(s$ecs$growth$ec50, "censored_summary")
  expect_equal(cs$upper, max(obj$growth$fit$data$x))
  # The survival and combined rows keep the stored grid the fit was asked for.
  for (w in c("survival", "combined")) {
    expect_equal(s$ecs[[w]]$ec50,
                 suppressWarnings(ecx(obj, ecx_val = 50, which = w,
                                      x_range = c(lower, 4))))
  }
})

test_that("exceedance() reads a growth estimate on growth's range (#412)", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # exceedance() reads the posterior through ecx() and nsec(), so a growth
  # EC50 beyond growth's top reaches it as censored there. Against a threshold
  # above that top, the censored draws could lie on either side, and the
  # probability is the interval between the two counts rather than a number.
  obj <- hurdle_cut_fixture(1.6)
  g_top <- max(obj$growth$fit$data$x)
  post <- suppressWarnings(ecx(obj, ecx_val = 50, which = "growth",
                               posterior = TRUE))
  n_above <- sum(attr(post, "censored")$above)
  expect_gt(n_above, 0)
  out <- suppressWarnings(exceedance(obj, threshold = 2, estimate = "ecx",
                                     ecx_val = 50, which = "growth"))
  expect_equal(out$n_above, n_above)
  expect_true(is.na(out$prob))
  expect_lt(out$prob_lower, out$prob_upper)
  # Below growth's top every censored draw is known to exceed the threshold,
  # so the probability is identified.
  expect_lt(1.5, g_top)
  below <- suppressWarnings(exceedance(obj, threshold = 1.5, estimate = "ecx",
                                       ecx_val = 50, which = "growth"))
  expect_equal(below$prob_lower, below$prob_upper)
})
