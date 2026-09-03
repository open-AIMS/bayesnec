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
# #268 is the defect: the decision to apply xform is made with an
# all-or-nothing guard on the formula as a whole (R/plot.R:119 and :259), so a
# transformation on the RESPONSE suppresses xform on the PREDICTOR axis. The
# issue records that it could only be read from source. It is reproduced here
# with no fitting, by giving a stored fit a formula that transforms its
# response.
#
# Nothing here fits a model. Every assertion runs off manec_example.

pl_fit <- function() {
  data(manec_example, package = "bayesnec")
  suppressMessages(pull_out(manec_example, model = "nec4param"))
}

pl_manec <- function() {
  data(manec_example, package = "bayesnec")
  manec_example
}

# exp() rather than log(): manec_example's response reaches -6.9 and log() of
# it is NaN. Only the presence of a transformation on the response matters.
pl_transform_response <- function(fit) {
  fit$bayesnecformula <- bayesnecformula(exp(y) ~ crf(x, model = "nec4param"))
  fit
}

# The x-axis limits the call produced, read off the device rather than from a
# returned object, because these are base-graphics methods that return nothing.
pl_x_limits <- function(obj, ...) {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  suppressMessages(plot(obj, ...))
  par("usr")[1:2]
}


# ---- the call completes for each class and argument combination -------------

test_that("plot draws a single fit and a model set", {
  expect_silent(pl_x_limits(pl_fit()))
  expect_silent(pl_x_limits(pl_manec()))
})

test_that("plot draws every candidate model when asked", {
  expect_silent(pl_x_limits(pl_manec(), all_models = TRUE))
})

test_that("plot accepts the annotation arguments", {
  expect_silent(pl_x_limits(pl_fit(), add_nec = FALSE))
  expect_silent(pl_x_limits(pl_fit(), add_ec10 = TRUE))
})


# ---- xform on the predictor axis --------------------------------------------

test_that("xform widens the predictor axis of a single fit", {
  f <- pl_fit()
  plain <- pl_x_limits(f)
  scaled <- pl_x_limits(f, xform = function(x) x * 100)
  expect_equal(scaled, plain * 100, tolerance = 1e-6)
})

test_that("xform widens the predictor axis of a model set", {
  m <- pl_manec()
  plain <- pl_x_limits(m)
  scaled <- pl_x_limits(m, xform = function(x) x * 100)
  expect_equal(scaled, plain * 100, tolerance = 1e-6)
})

test_that("a transformed response suppresses xform on the predictor axis", {
  # PINS THE #268 DEFECT on the base-plot path for a bayesnecfit
  # (R/plot.R:119). xform is accepted and silently ignored, so the axis is
  # drawn on the fitted scale while the caller asked for the recorded one.
  #
  # INVERT THIS TEST WHEN #268 IS FIXED: the two should then differ by the
  # factor xform applies, as in "xform widens the predictor axis of a single
  # fit" above.
  f <- pl_transform_response(pl_fit())
  expect_equal(pl_x_limits(f, xform = function(x) x * 100), pl_x_limits(f),
               tolerance = 1e-6)
})

test_that("a transformed response suppresses xform for a model set too", {
  # PINS THE #268 DEFECT on the bayesmanecfit branch (R/plot.R:259). Same
  # inversion applies.
  m <- pl_manec()
  # The model name is substituted into the formula text rather than
  # referenced: crf() evaluates its model argument where the formula is used,
  # not where it is written.
  mod <- names(m$mod_fits)[1]
  m$mod_fits[[1]]$bayesnecformula <- bayesnecformula(
    stats::as.formula(paste0("exp(y) ~ crf(x, model = \"", mod, "\")"))
  )
  expect_equal(pl_x_limits(m, xform = function(x) x * 100), pl_x_limits(m),
               tolerance = 1e-6)
})

test_that("plot and autoplot agree on the predictor axis", {
  # The two paths make the xform decision independently, in four places
  # altogether. They agree today; this is what would catch one being fixed for
  # #268 without the other.
  f <- pl_fit()
  gg <- suppressMessages(ggbnec_data(f, xform = function(x) x * 100))
  base_max <- pl_x_limits(f, xform = function(x) x * 100)[2]
  # The device pads the axis beyond the data, so the fitted range must fall
  # inside the drawn range rather than equal it.
  expect_lt(max(gg$x_e, na.rm = TRUE), base_max)
  expect_gt(max(gg$x_e, na.rm = TRUE), base_max * 0.9)
})
