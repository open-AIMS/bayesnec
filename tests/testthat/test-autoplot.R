# autoplot() and ggbnec_data() had no test file of their own in any release.
# The eleven plot-related lines scattered through test-bayesnec_methods.R,
# test-bayesmanec_methods.R and test-expand_classes.R assert that a ggplot
# comes back, and nothing else -- in particular xform, which exists so that a
# fit on a transformed predictor can be drawn on the recorded scale, has never
# been asserted anywhere on the plotting path. It is asserted only on the
# estimators, in test-ecx.R, test-nec.R, test-nsec.R, test-ecnsec.R and
# test-average_estimates.R.
#
# That gap is #268: the decision to apply xform is made with an all-or-nothing
# guard on the formula as a whole, so a transformation on the RESPONSE
# suppresses xform on the PREDICTOR axis. The issue records that it could only
# be read from source, because no packaged fit transforms its response. It can
# in fact be reproduced without fitting anything, by giving a stored fit a
# formula that transforms its response, and that is done below.
#
# Nothing here fits a model. Every assertion runs off manec_example.

ap_fit <- function() {
  data(manec_example, package = "bayesnec")
  suppressMessages(pull_out(manec_example, model = "nec4param"))
}

ap_manec <- function() {
  data(manec_example, package = "bayesnec")
  manec_example
}

# Give a stored fit a formula that transforms its response and nothing else.
# exp() is used rather than log() because manec_example's response reaches
# -6.9, and log() of it is NaN; the point is only that a transformation is
# present on the response, not which one.
ap_transform_response <- function(fit) {
  fit$bayesnecformula <- bayesnecformula(exp(y) ~ crf(x, model = "nec4param"))
  fit
}

ap_x_max <- function(obj, ...) {
  max(suppressMessages(ggbnec_data(obj, ...))$x_e, na.rm = TRUE)
}


# ---- what ggbnec_data returns -----------------------------------------------

test_that("ggbnec_data returns the columns autoplot draws from", {
  d <- suppressMessages(ggbnec_data(ap_fit()))
  expect_true(all(c("x_e", "y_e", "y_ci", "x_r", "y_r") %in% names(d)))
  expect_gt(nrow(d), 0)
})

test_that("ggbnec_data returns the same frame shape for a model set", {
  d <- suppressMessages(ggbnec_data(ap_manec()))
  expect_true(all(c("x_e", "y_e", "y_ci", "x_r", "y_r") %in% names(d)))
})

test_that("the nec annotation is present by default and suppressible", {
  with_nec <- suppressMessages(ggbnec_data(ap_fit()))
  without <- suppressMessages(ggbnec_data(ap_fit(), add_nec = FALSE))
  expect_true("nec_vals" %in% names(with_nec))
  expect_false("nec_vals" %in% names(without))
})

test_that("ggbnec_data takes add_nec, and absorbs nec = FALSE into dots", {
  # autoplot() takes nec = and forwards it as add_nec =. ggbnec_data() is
  # exported and documented separately, and the obvious transfer of the
  # autoplot argument name does nothing at all: nec = FALSE is swallowed by
  # ... and the annotation is still computed and returned. Pinned as current
  # behaviour rather than asserted to be correct.
  swallowed <- suppressMessages(ggbnec_data(ap_fit(), nec = FALSE))
  expect_true("nec_vals" %in% names(swallowed))
})


# ---- xform on the predictor axis --------------------------------------------

test_that("xform is applied to the predictor axis of a single fit", {
  f <- ap_fit()
  expect_equal(ap_x_max(f, xform = function(x) x * 100),
               ap_x_max(f) * 100, tolerance = 1e-8)
})

test_that("xform is applied to the predictor axis of a model set", {
  m <- ap_manec()
  expect_equal(ap_x_max(m, xform = function(x) x * 100),
               ap_x_max(m) * 100, tolerance = 1e-8)
})

test_that("xform reaches the raw data as well as the fitted curve", {
  f <- ap_fit()
  plain <- suppressMessages(ggbnec_data(f))
  scaled <- suppressMessages(ggbnec_data(f, xform = function(x) x * 100))
  expect_equal(max(scaled$x_r, na.rm = TRUE),
               max(plain$x_r, na.rm = TRUE) * 100, tolerance = 1e-8)
})

test_that("the transformation predicate answers per variable", {
  f <- ap_transform_response(ap_fit())
  bdat <- model.frame(f$bayesnecformula, data = f$fit$data,
                      run_par_checks = TRUE)
  # The predictor is not transformed in this formula and the per-variable
  # predicate says so; find_transformations(), which the plotting paths read,
  # answers for the formula as a whole and reports the response.
  expect_false(pop_var_is_transformed(bdat, "x_var"))
  expect_true(pop_var_is_transformed(bdat, "y_var"))
  expect_identical(find_transformations(bdat), "y")
})

test_that("a transformed response suppresses xform on the predictor axis", {
  # PINS THE #268 DEFECT, on the autoplot path (R/autoplot.R:306 and :350).
  # xform is accepted and silently ignored, so the axis is drawn on the fitted
  # scale while the caller asked for the recorded one. The estimates nec() and
  # ecx() return are unaffected; this is the plot alone.
  #
  # INVERT THIS TEST WHEN #268 IS FIXED: the two should then be equal, as they
  # are in "xform is applied to the predictor axis of a single fit" above.
  f <- ap_transform_response(ap_fit())
  expect_equal(ap_x_max(f, xform = function(x) x * 100), ap_x_max(f),
               tolerance = 1e-8)
})

test_that("a transformed response suppresses xform for a model set too", {
  # PINS THE #268 DEFECT on the bayesmanecfit branch. Same inversion applies.
  m <- ap_manec()
  # The model name has to be substituted into the formula text rather than
  # referenced: crf() evaluates its model argument where the formula is used,
  # not where it is written, so a variable reference is out of scope by then.
  mod <- names(m$mod_fits)[1]
  m$mod_fits[[1]]$bayesnecformula <- bayesnecformula(
    stats::as.formula(paste0("exp(y) ~ crf(x, model = \"", mod, "\")"))
  )
  expect_equal(ap_x_max(m, xform = function(x) x * 100), ap_x_max(m),
               tolerance = 1e-8)
})


# ---- autoplot itself --------------------------------------------------------

test_that("autoplot returns a ggplot for both fit classes", {
  expect_s3_class(suppressMessages(autoplot(ap_fit())), "ggplot")
  expect_s3_class(suppressMessages(autoplot(ap_manec())), "ggplot")
})

test_that("autoplot accepts xform without error on both classes", {
  expect_s3_class(
    suppressMessages(autoplot(ap_fit(), xform = function(x) x * 100)), "ggplot")
  expect_s3_class(
    suppressMessages(autoplot(ap_manec(), xform = function(x) x * 100)),
    "ggplot")
})
