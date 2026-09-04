# autoplot() and ggbnec_data() had no test file of their own in any release.
# The eleven plot-related lines scattered through test-bayesnec_methods.R,
# test-bayesmanec_methods.R and test-expand_classes.R assert that a ggplot
# comes back, and nothing else -- in particular xform, which exists so that a
# fit on a transformed predictor can be drawn on the recorded scale, has never
# been asserted anywhere on the plotting path. It is asserted only on the
# estimators, in test-ecx.R, test-nec.R, test-nsec.R, test-ecnsec.R and
# test-average_estimates.R.
#
# That gap is #268, and the defect is not that xform is ignored. The decision
# to apply it is made with an all-or-nothing guard on the formula as a whole
# (R/autoplot.R:308 and :352), so a transformation on the RESPONSE suppresses
# xform on the PREDICTOR axis -- but the nec and ecx annotations are
# transformed unconditionally at R/autoplot.R:313-318, outside that guard. The
# curve and its annotation therefore come back on different scales. Both halves
# are asserted below, because a fix that corrects either one alone leaves the
# figure wrong.
#
# The issue records that this could only be read from source, because no
# packaged fit transforms its response. It is reproduced here with no fitting,
# using transformed_response_fit() from setup.R.
#
# Nothing here fits a model. Every assertion runs off nec4param and
# manec_example, both built in setup.R. gg_x_max() is defined there too,
# because test-plot.R needs it to compare the two paths.


# ---- what ggbnec_data returns -----------------------------------------------

test_that("ggbnec_data returns the columns autoplot draws from", {
  skip_on_cran()
  d <- suppressMessages(ggbnec_data(nec4param))
  expect_true(all(c("x_e", "y_e", "y_ci", "x_r", "y_r") %in% names(d)))
  expect_gt(nrow(d), 0)
})

test_that("ggbnec_data returns the same frame shape for a model set", {
  skip_on_cran()
  d <- suppressMessages(ggbnec_data(manec_example))
  expect_true(all(c("x_e", "y_e", "y_ci", "x_r", "y_r") %in% names(d)))
})

test_that("the nec annotation is present by default and suppressible", {
  skip_on_cran()
  with_nec <- suppressMessages(ggbnec_data(nec4param))
  without <- suppressMessages(ggbnec_data(nec4param, add_nec = FALSE))
  expect_true("nec_vals" %in% names(with_nec))
  expect_false("nec_vals" %in% names(without))
})

test_that("ggbnec_data takes add_nec, and absorbs nec = FALSE into dots", {
  # autoplot() takes nec = and forwards it as add_nec =. ggbnec_data() is
  # exported and documented separately, and the obvious transfer of the
  # autoplot argument name does nothing at all: nec = FALSE is swallowed by
  # ... and the annotation is still computed and returned. Pinned as current
  # behaviour rather than asserted to be correct.
  skip_on_cran()
  swallowed <- suppressMessages(ggbnec_data(nec4param, nec = FALSE))
  expect_true("nec_vals" %in% names(swallowed))
})


# ---- xform on the predictor axis --------------------------------------------

test_that("xform is applied to the predictor axis of a single fit", {
  skip_on_cran()
  expect_equal(gg_x_max(nec4param, xform = function(x) x * 100),
               gg_x_max(nec4param) * 100, tolerance = 1e-8)
})

test_that("xform is applied to the predictor axis of a model set", {
  skip_on_cran()
  expect_equal(gg_x_max(manec_example, xform = function(x) x * 100),
               gg_x_max(manec_example) * 100, tolerance = 1e-8)
})

test_that("xform reaches the raw data as well as the fitted curve", {
  skip_on_cran()
  plain <- suppressMessages(ggbnec_data(nec4param))
  scaled <- suppressMessages(ggbnec_data(nec4param,
                                         xform = function(x) x * 100))
  expect_equal(max(scaled$x_r, na.rm = TRUE),
               max(plain$x_r, na.rm = TRUE) * 100, tolerance = 1e-8)
})


# ---- #268, pinned on both halves of the frame -------------------------------

test_that("find_transformations reports the response for this fixture", {
  # The premise the four pinning tests below rest on: the predictor is not
  # transformed in this formula, and the predicate the plotting paths read
  # answers for the formula as a whole and so reports the response. That is
  # what makes the guard fire on a predictor nobody transformed.
  #
  # Only the fixture property is asserted here. pop_var_is_transformed() itself
  # is specified in test-fit_bayesnec.R.
  skip_on_cran()
  f <- transformed_response_fit(nec4param, "nec4param")
  bdat <- model.frame(f$bayesnecformula, data = f$fit$data,
                      run_par_checks = TRUE)
  expect_identical(find_transformations(bdat), "y")
})

test_that("a transformed response suppresses xform on the predictor axis", {
  # PINS THE #268 DEFECT on the autoplot path for a bayesnecfit (the guard at
  # R/autoplot.R:308). xform is accepted and silently dropped from the axis,
  # so the curve comes back on the fitted scale while the caller asked for the
  # recorded one. The estimates nec() and ecx() return are unaffected; this is
  # the plot alone.
  #
  # INVERT THIS TEST WHEN #268 IS FIXED: the two should then differ by the
  # factor xform applies, as in "xform is applied to the predictor axis of a
  # single fit" above.
  skip_on_cran()
  f <- transformed_response_fit(nec4param, "nec4param")
  expect_equal(gg_x_max(f, xform = function(x) x * 100), gg_x_max(f),
               tolerance = 1e-8)
})

test_that("a transformed response suppresses xform for a model set too", {
  # PINS THE #268 DEFECT on the bayesmanecfit branch (the guard at
  # R/autoplot.R:352). Same inversion applies.
  skip_on_cran()
  m <- transformed_response_manec(manec_example)
  expect_equal(gg_x_max(m, xform = function(x) x * 100), gg_x_max(m),
               tolerance = 1e-8)
})

test_that("the nec annotation is transformed while the axis is not", {
  # PINS THE OTHER HALF OF #268, which the issue does not state.
  # R/autoplot.R:313-318 pass xform to bind_nec() and to ecx() unconditionally,
  # outside the guard, so the annotation column and the curve column come back
  # on scales that differ by whatever xform does. Measured on nec4param with
  # xform = x * 100: max(x_e) is 3.22 with and without xform, while the largest
  # nec_vals -- the upper bound, which is what the assertion below reads --
  # changes from 1.53 to 152.8.
  #
  # INVERT THIS TEST WHEN #268 IS FIXED: the two columns must then agree,
  # whichever scale the fix settles on. A fix that changes only the guard, or
  # only these lines, fails one of the two tests and leaves the other passing,
  # which is the point of asserting both.
  #
  # The axis half of the contrast is asserted in the sibling test above and is
  # not repeated here; this asserts the annotation alone.
  skip_on_cran()
  f <- transformed_response_fit(nec4param, "nec4param")
  plain <- suppressMessages(ggbnec_data(f))
  scaled <- suppressMessages(ggbnec_data(f, xform = function(x) x * 100))
  expect_equal(max(scaled$nec_vals, na.rm = TRUE),
               max(plain$nec_vals, na.rm = TRUE) * 100, tolerance = 1e-8)
})


test_that("the model set returns its nec annotation on the other scale too", {
  # The bayesmanecfit branch has the same split: R/autoplot.R:352 guards the
  # curve and R/autoplot.R:357 passes xform to bind_nec() unconditionally.
  # Asserted separately from the bayesnecfit case above for the same reason:
  # correcting one pair of lines and not the other is the half-fix this file
  # exists to catch.
  #
  # Measured on manec_example with xform = x * 100: max(x_e) is 3.22 with and
  # without xform, while the largest nec_vals changes from 1.53 to 152.7.
  #
  # INVERT THIS TEST WHEN #268 IS FIXED, with the sibling above it.
  skip_on_cran()
  m <- transformed_response_manec(manec_example)
  plain <- suppressMessages(ggbnec_data(m))
  scaled <- suppressMessages(ggbnec_data(m, xform = function(x) x * 100))
  expect_equal(max(scaled$nec_vals, na.rm = TRUE),
               max(plain$nec_vals, na.rm = TRUE) * 100, tolerance = 1e-8)
})


# ---- autoplot itself --------------------------------------------------------

test_that("autoplot returns a ggplot for both fit classes", {
  skip_on_cran()
  expect_s3_class(suppressMessages(autoplot(nec4param)), "ggplot")
  expect_s3_class(suppressMessages(autoplot(manec_example)), "ggplot")
})

test_that("autoplot accepts xform without error on both classes", {
  skip_on_cran()
  expect_s3_class(
    suppressMessages(autoplot(nec4param, xform = function(x) x * 100)),
    "ggplot")
  expect_s3_class(
    suppressMessages(autoplot(manec_example, xform = function(x) x * 100)),
    "ggplot")
})
