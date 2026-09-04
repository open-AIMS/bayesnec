# plot() for the bayesnec classes had no test file of its own in any release.
# What coverage existed asserted that the call did not error; nothing asserted
# what was drawn.
#
# The base-graphics paths cannot be inspected by reading a returned object, so
# the assertions here read the device instead: plotting to a null pdf() device
# and reading par("usr") gives the x-axis limits the call actually produced.
# That is enough to assert the one thing about these paths that has been
# reported wrong -- whether xform reached the predictor axis -- without
# comparing images.
#
# #268 is the defect, and it is not that xform is ignored. The decision to
# apply it is made with an all-or-nothing guard on the formula as a whole
# (R/plot.R:125 and :279), so a transformation on the RESPONSE suppresses xform
# on the PREDICTOR axis -- but the nec and ec10 annotations are transformed
# unconditionally at R/plot.R:130-131, outside that guard. The two halves of
# the figure are therefore drawn on different scales, and the abline() at
# R/plot.R:180 is drawn beyond the axis limit. Both halves are asserted below,
# because a fix that corrects either one alone leaves the figure wrong.
#
# The issue records that this could only be read from source. It is reproduced
# here with no fitting, using transformed_response_fit() from setup.R.
#
# Nothing here fits a model. Every assertion runs off nec4param and
# manec_example, both built in setup.R.

# The x-axis limits the call produced, read off the device rather than from a
# returned object, because these are base-graphics methods that return nothing.
# No suppression: an assertion of silence made through a helper that suppresses
# messages cannot fail, and these methods are silent today.
pl_x_max <- function(obj, ...) {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  plot(obj, ...)
  par("usr")[2]
}

# The factor by which xform changed the axis. A ratio rather than a pair of
# limits: it is scale-free, so it does not encode base graphics' 4% default
# axis expansion the way a comparison against a fixed number would, and it is
# directly comparable with the same ratio taken from the ggplot2 path.
pl_x_ratio <- function(obj) {
  pl_x_max(obj, xform = function(x) x * 100) / pl_x_max(obj)
}


# ---- the argument combinations nothing else covers --------------------------
#
# plot() on a plain bayesnecfit and bayesmanecfit is already asserted silent
# and invisible in test-bayesnec_methods.R and test-bayesmanec_methods.R, so it
# is not repeated. all_models, add_nec and add_ec10 are asserted nowhere else,
# and each is asserted here on what it changes rather than on the call
# completing, which any value of the argument would satisfy.

test_that("all_models draws a panel per candidate, named", {
  # Asserting only that the call is silent would pass just as well with
  # all_models = FALSE, and so would not detect the argument being ignored.
  # R/plot.R:253 labels each panel with the model name, and that legend() call
  # is reached only from the all_models loop, so the names it is given are
  # evidence of which candidates were drawn.
  skip_on_cran()
  labels <- character()
  local_mocked_bindings(
    legend = function(..., legend = NULL) labels <<- c(labels, as.character(legend)),
    .package = "bayesnec"
  )
  pl_x_max(manec_example, all_models = TRUE)
  expect_true(all(names(manec_example$mod_fits) %in% labels))
  # And not otherwise. The model-average plot does call legend(), at
  # R/plot.R:333, but with the estimate string rather than a model name, so no
  # candidate name appears.
  labels <- character()
  pl_x_max(manec_example, all_models = FALSE)
  expect_false(any(names(manec_example$mod_fits) %in% labels))
})

test_that("add_nec and add_ec10 decide what is annotated", {
  # Same reasoning: silence is not evidence that either argument was read.
  # The three branches at R/plot.R:179-192 are mutually exclusive, so the
  # abline() calls are counted AND their values identified. Counting alone does
  # not discriminate: one call is drawn whether the branch taken is the nec or
  # the ec10, so swapping R/plot.R:190 to draw ec10 in the nec branch would
  # pass a count-only assertion.
  skip_on_cran()
  drawn <- list()
  local_mocked_bindings(
    abline = function(v = NULL, ...) drawn[[length(drawn) + 1L]] <<- v,
    .package = "bayesnec"
  )
  # Neither annotation: no vertical line at all.
  pl_x_max(nec4param, add_nec = FALSE)
  expect_length(drawn, 0)
  # The nec alone, drawn at the NEC estimate and its two bounds.
  pl_x_max(nec4param, add_nec = TRUE)
  expect_length(drawn, 1)
  expect_equal(unname(drawn[[1]]), unname(nec4param$ne), tolerance = 1e-8)
  # The ec10 alone. manec_example is gaussian, so R/plot.R:114-115 takes the
  # relative branch (R/plot.R:114-115); the value is not asserted here beyond
  # its being a different one, because ecx() is under test in test-ecx.R.
  pl_x_max(nec4param, add_nec = FALSE, add_ec10 = TRUE)
  expect_length(drawn, 2)
  expect_false(isTRUE(all.equal(unname(drawn[[2]]), unname(nec4param$ne))))
  # Both, in the documented order: the nec in red, then the ec10 in orange.
  pl_x_max(nec4param, add_nec = TRUE, add_ec10 = TRUE)
  expect_length(drawn, 4)
  expect_equal(unname(drawn[[3]]), unname(nec4param$ne), tolerance = 1e-8)
  expect_equal(unname(drawn[[4]]), unname(drawn[[2]]), tolerance = 1e-8)
})


# ---- xform on the predictor axis --------------------------------------------

test_that("xform widens the predictor axis of a single fit", {
  skip_on_cran()
  expect_equal(pl_x_ratio(nec4param), 100, tolerance = 1e-6)
})

test_that("xform widens the predictor axis of a model set", {
  skip_on_cran()
  expect_equal(pl_x_ratio(manec_example), 100, tolerance = 1e-6)
})


# ---- #268, pinned on both halves of the figure ------------------------------

test_that("a transformed response suppresses xform on the predictor axis", {
  # PINS THE #268 DEFECT on the base-plot path for a bayesnecfit
  # (the guard at R/plot.R:125). xform is accepted and silently dropped from
  # the axis, so the data is drawn on the fitted scale while the caller asked
  # for the recorded one.
  #
  # INVERT THIS TEST WHEN #268 IS FIXED: the ratio should then be 100, as in
  # "xform widens the predictor axis of a single fit" above.
  skip_on_cran()
  f <- transformed_response_fit(nec4param, "nec4param")
  expect_equal(pl_x_ratio(f), 1, tolerance = 1e-6)
})

test_that("a transformed response suppresses xform for a model set too", {
  # PINS THE #268 DEFECT on the bayesmanecfit branch (the guard at
  # R/plot.R:279). Same inversion applies.
  skip_on_cran()
  m <- transformed_response_manec(manec_example)
  expect_equal(pl_x_ratio(m), 1, tolerance = 1e-6)
})

test_that("the nec annotation is drawn off the end of the axis", {
  # PINS THE OTHER HALF OF #268, which the issue does not state and the PR
  # that added this file originally described as xform being "silently
  # skipped". It is not skipped. R/plot.R:130-131 apply it to nec and ec10
  # unconditionally, outside the guard, so with a transformed response the
  # abline() at R/plot.R:180 is drawn at the transformed NEC on an axis left
  # untransformed, and the line is not on the figure at all.
  #
  # abline() is intercepted rather than the value recomputed here: what has to
  # be asserted is the number the package passed to it, not a number this file
  # worked out for itself, which would still pass if R/plot.R:130-131 changed.
  # The suite already mocks this way in test-fit_bayesnec.R and
  # test-inits_functions.R.
  #
  # INVERT THIS TEST WHEN #268 IS FIXED: the drawn NEC must then fall inside
  # the axis, whichever scale the fix settles on. A fix that changes only the
  # guard, or only R/plot.R:130-131, fails one of the two tests and leaves the
  # other passing, which is the point of asserting both.
  skip_on_cran()
  f <- transformed_response_fit(nec4param, "nec4param")
  drawn <- NULL
  local_mocked_bindings(abline = function(v = NULL, ...) drawn <<- v,
                        .package = "bayesnec")
  axis_max <- pl_x_max(f, xform = function(x) x * 100)
  # Measured on nec4param: the axis maximum is 3.35 and the NEC line, its lower
  # and its upper bound are drawn at 146, 136 and 153.
  expect_length(drawn, 3)
  expect_gt(min(drawn), axis_max)
})

test_that("plot and autoplot make the xform decision the same way", {
  # The two paths make that decision independently, in four places altogether.
  # This is what catches one being fixed for #268 without the other, so it is
  # asserted on the fixture where the guard actually fires: with an
  # untransformed formula both paths apply xform whatever the guard says, and
  # the comparison is vacuous.
  #
  # Ratios rather than limits, so the base path's axis expansion does not enter
  # the comparison.
  skip_on_cran()
  f <- transformed_response_fit(nec4param, "nec4param")
  gg_ratio <- gg_x_max(f, xform = function(x) x * 100) / gg_x_max(f)
  expect_equal(pl_x_ratio(f), gg_ratio, tolerance = 1e-6)
})
