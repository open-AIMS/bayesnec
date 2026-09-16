# autoplot() and ggbnec_data() had no test file of their own in any release.
# Nine plot-related lines are scattered through test-bayesnec_methods.R,
# test-bayesmanec_methods.R and test-expand_classes.R, and only one of them,
# test-expand_classes.R:323, concerns autoplot() at all: it asserts that a
# ggplot comes back, and nothing else. The other eight assert that base plot()
# returns NULL invisibly and silently, which is test-plot.R's subject. In
# particular xform, which exists so that a
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
  # add_nec on the model-set branch, R/autoplot.R:356. Asserted here rather
  # than in a block of its own so the frame above is computed once: making that
  # condition unconditional failed nothing until this line was added, while its
  # bayesnecfit twin at :312 was already observed.
  expect_true("nec_vals" %in% names(d))
  without <- suppressMessages(ggbnec_data(manec_example, add_nec = FALSE))
  expect_false("nec_vals" %in% names(without))
})

test_that("ggbnec_data returns a selected fitted grouping", {
  skip_on_cran()
  fit <- grouped_plot_fit()
  grouped <- suppressMessages(ggbnec_data(fit, group = "plate",
                                          add_nec = FALSE))
  raw <- grouped[!is.na(grouped$y_r), ]
  expect_equal(raw$group, fit$fit$data$plate)
  expect_identical(attr(grouped, "group_var"), "plate")

  plain <- suppressMessages(ggbnec_data(fit, add_nec = FALSE))
  expect_false("group" %in% names(plain))
  expect_null(attr(plain, "group_var"))
})

test_that("model-averaged plotting retains a selected fitted grouping", {
  skip_on_cran()
  object <- manec_example
  object$mod_fits <- lapply(object$mod_fits, grouped_plot_fit)
  grouped <- suppressMessages(ggbnec_data(object, group = "plate",
                                          add_nec = FALSE))
  expect_true("group" %in% names(grouped))
  expect_identical(attr(grouped, "group_var"), "plate")
  plot <- suppressMessages(autoplot(object, group = "plate", nec = FALSE))
  expect_true(any(vapply(
    plot$layers, function(layer) inherits(layer$stat, "StatSummary"),
    logical(1)
  )))
  colour_plot <- suppressMessages(
    autoplot(object, group = "plate", group_aes = "colour", nec = FALSE)
  )
  raw_points <- Filter(function(layer) {
    inherits(layer$geom, "GeomPoint") &&
      !inherits(layer$stat, "StatSummary")
  }, colour_plot$layers)
  expect_identical(rlang::as_label(raw_points[[1]]$mapping$fill), "group")
})

test_that("an unfitted categorical column groups raw observations", {
  skip_on_cran()
  fit <- unfitted_group_plot_fit()
  grouped <- suppressMessages(ggbnec_data(fit, group = "climate",
                                          add_nec = FALSE))
  raw <- grouped[!is.na(grouped$y_r), ]
  expect_equal(raw$group, fit$retained_data$climate)
  expect_identical(attr(grouped, "group_var"), "climate")
  expect_false(attr(grouped, "group_fitted"))

  plot <- suppressMessages(
    autoplot(fit, group = "climate", group_aes = "colour", nec = FALSE)
  )
  expect_identical(plot$labels$fill, "climate (not fitted)")
  expect_identical(plot$labels$colour, "climate (not fitted)")
  expect_false(any(vapply(
    plot$layers, function(layer) inherits(layer$stat, "StatSummary"),
    logical(1)
  )))
  expect_error(
    autoplot(fit, group = "climate", nec = FALSE),
    "only use `group_aes = \\\"colour\\\"`"
  )
})

test_that("retained columns are stored once in fitted-row order", {
  skip_on_cran()
  fit <- unfitted_group_plot_fit()
  expect_named(fit$retained_data, "climate")
  expect_identical(rownames(fit$retained_data), rownames(fit$fit$data))
  expect_false("climate" %in% names(fit$fit$data))
})

test_that("a model set and a pulled fit retain an unfitted grouping", {
  skip_on_cran()
  source <- manec_example$mod_fits[[1]]$fit$data
  source$climate <- factor(rep(c("ambient", "warm", "hot"),
                               length.out = nrow(source)))
  object <- bayesnec:::retain_unused_data(manec_example, source)
  grouped <- suppressMessages(ggbnec_data(object, group = "climate",
                                          add_nec = FALSE))
  expect_equal(grouped$group[!is.na(grouped$y_r)], source$climate)
  one <- suppressMessages(pull_out(object, "nec4param"))
  expect_identical(one$retained_data, object$retained_data)
  expect_no_error(suppressMessages(
    autoplot(one, group = "climate", group_aes = "colour", nec = FALSE)
  ))

  amended <- suppressWarnings(suppressMessages(
    amend(object, drop = "ecx4param")
  ))
  expect_identical(amended$retained_data, object$retained_data)
  other <- suppressMessages(pull_out(object, "ecx4param"))
  combined <- suppressWarnings(suppressMessages(c(one, other)))
  expect_identical(combined$retained_data, object$retained_data)
})

test_that("group names and types are validated against both data sources", {
  skip_on_cran()
  fit <- grouped_plot_fit()
  expect_error(ggbnec_data(fit, group = "missing"),
               "fitted formula or a column retained")
  expect_error(ggbnec_data(nec4param, group = "x"),
               "must be categorical")
  expect_error(ggbnec_data(fit, group = character()),
               "one non-empty column name")

  d <- fit$fit$data
  d$continuous <- seq_len(nrow(d))
  fit <- bayesnec:::retain_unused_data(fit, d)
  expect_error(ggbnec_data(fit, group = "continuous"),
               "must be categorical")
})

test_that("group does not consume an existing positional dots argument", {
  skip_on_cran()
  expect_no_error(suppressMessages(
    ggbnec_data(nec4param, FALSE, FALSE, identity, 50)
  ))
})

test_that("autoplot adds per-level summaries only when grouping is requested", {
  skip_on_cran()
  fit <- grouped_plot_fit()
  grouped <- suppressMessages(autoplot(fit, group = "plate", nec = FALSE))
  plain <- suppressMessages(autoplot(fit, nec = FALSE))
  is_summary <- function(layer) inherits(layer$stat, "StatSummary")
  expect_true(any(vapply(grouped$layers, is_summary, logical(1))))
  expect_false(any(vapply(plain$layers, is_summary, logical(1))))
})

test_that("group colour maps observations and per-level means", {
  skip_on_cran()
  fit <- grouped_plot_fit()
  colour_plot <- suppressMessages(
    autoplot(fit, group = "plate", group_aes = "colour", nec = FALSE)
  )
  line_plot <- suppressMessages(
    autoplot(fit, group = "plate", nec = FALSE)
  )
  is_raw_point <- function(layer) {
    inherits(layer$geom, "GeomPoint") &&
      !inherits(layer$stat, "StatSummary")
  }
  raw_colour <- Filter(is_raw_point, colour_plot$layers)[[1]]
  raw_line <- Filter(is_raw_point, line_plot$layers)[[1]]
  colour_summaries <- Filter(
    function(layer) inherits(layer$stat, "StatSummary"),
    colour_plot$layers
  )

  expect_identical(rlang::as_label(raw_colour$mapping$fill), "group")
  expect_identical(raw_line$aes_params$fill, "grey30")
  expect_true(any(vapply(colour_summaries, function(layer) {
    identical(rlang::as_label(layer$mapping$colour), "group")
  }, logical(1))))
  expect_identical(colour_plot$labels$fill, "plate")
  expect_identical(colour_plot$labels$colour, "plate")
  expect_error(
    autoplot(fit, group = "plate", group_aes = "shape", nec = FALSE),
    "line.*colour"
  )
})

test_that("group colours agree when levels use different mean marks", {
  skip_on_cran()
  fit <- nec4param
  group <- rep(c("b", "c"), length.out = nrow(fit$fit$data))
  group[1] <- "a"
  fit$fit$data$mixed_group <- factor(group, levels = c("a", "b", "c"))
  fit$bayesnecformula <- bayesnecformula(
    y ~ crf(x, model = "nec4param") + ogl(mixed_group)
  )
  plot <- suppressMessages(
    autoplot(fit, group = "mixed_group", group_aes = "colour", nec = FALSE)
  )
  built <- ggplot2::ggplot_build(plot)
  fill_scale <- built$plot$scales$get_scales("fill")
  colour_scale <- built$plot$scales$get_scales("colour")

  expect_equal(fill_scale$map(levels(fit$fit$data$mixed_group)),
               colour_scale$map(levels(fit$fit$data$mixed_group)))
  expect_equal(as.character(fill_scale$get_breaks()), c("a", "b", "c"))
  expect_equal(as.character(colour_scale$get_breaks()), c("a", "b", "c"))
  expect_length(built$plot$guides$guides, 1)
})

test_that("a grouping confined to one predictor value uses mean markers", {
  skip_on_cran()
  fit <- nec4param
  fit$fit$data$dose_group <- factor(fit$fit$data$x)
  fit$bayesnecformula <- bayesnecformula(
    y ~ crf(x, model = "nec4param") + ogl(dose_group)
  )
  plot <- suppressMessages(
    autoplot(fit, group = "dose_group", nec = FALSE)
  )
  summary_layers <- Filter(
    function(layer) inherits(layer$stat, "StatSummary"), plot$layers
  )
  expect_length(summary_layers, 1)
  expect_s3_class(summary_layers[[1]]$geom, "GeomPoint")

  colour_plot <- suppressMessages(
    autoplot(fit, group = "dose_group", group_aes = "colour", nec = FALSE)
  )
  colour_summary <- Filter(
    function(layer) inherits(layer$stat, "StatSummary"),
    colour_plot$layers
  )[[1]]
  expect_identical(rlang::as_label(colour_summary$mapping$fill), "group")
})

test_that("grouped fits return and plot one panel per fitted level", {
  skip_on_cran()
  fit <- grouped_plot_fit()
  object <- structure(
    list(fits = list(a = fit, b = fit), group_var = "site",
         levels = c("a", "b")),
    class = c("bayesnecgroupfit", "bnecfit")
  )
  dat <- suppressMessages(ggbnec_data(object, add_nec = FALSE))
  expect_equal(levels(dat$group), c("a", "b"))
  expect_equal(as.integer(table(dat$group)), rep(nrow(dat) / 2, 2))
  expect_identical(attr(dat, "group_var"), "site")
  selected <- suppressMessages(ggbnec_data(object, group = "plate",
                                            add_nec = FALSE))
  expect_identical(attr(selected, "group_var"), "plate")
  expect_identical(attr(selected, "panel_var"), "site")
  expect_true(attr(selected, "group_fitted"))

  plot <- suppressMessages(autoplot(object, nec = FALSE))
  panels <- levels(plot$layers[[1]]$data$model)
  expect_equal(panels, c("site = a", "site = b"))
})

test_that("grouped fits resolve an unfitted grouping within each level", {
  skip_on_cran()
  fit_a <- unfitted_group_plot_fit()
  fit_b <- unfitted_group_plot_fit(values = rep(
    c("warm", "hot"), length.out = nrow(nec4param$fit$data)
  ))
  object <- structure(
    list(fits = list(a = fit_a, b = fit_b), group_var = "site",
         levels = c("a", "b")),
    class = c("bayesnecgroupfit", "bnecfit")
  )
  dat <- suppressMessages(ggbnec_data(object, group = "climate",
                                      add_nec = FALSE))
  expect_equal(
    as.character(dat$group[dat$panel == "a" & !is.na(dat$y_r)]),
    as.character(fit_a$retained_data$climate)
  )
  expect_equal(
    as.character(dat$group[dat$panel == "b" & !is.na(dat$y_r)]),
    as.character(fit_b$retained_data$climate)
  )
  expect_false(attr(dat, "group_fitted"))

  plot <- suppressMessages(
    autoplot(object, group = "climate", group_aes = "colour", nec = FALSE)
  )
  expect_identical(plot$labels$fill, "climate (not fitted)")
  expect_equal(levels(plot$layers[[1]]$data$model),
               c("site = a", "site = b"))
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


test_that("a non-function xform is refused the same way on both classes", {
  # The bayesnecfit method guarded xform and the bayesmanecfit method did not,
  # so the same user error gave "xform must be a function." for one class and
  # "could not find function \"xform\"" -- raised from inside mutate(), naming
  # neither the argument nor what it should have been -- for the other. The
  # guard was added to the model-set method in #278.
  skip_on_cran()
  expect_error(ggbnec_data(nec4param, xform = "no"),
               "xform must be a function")
  expect_error(ggbnec_data(manec_example, xform = "no"),
               "xform must be a function")
})


# ---- xform on the predictor axis --------------------------------------------

test_that("xform is applied to the predictor axis of a single fit", {
  skip_on_cran()
  expect_equal(gg_x_max(nec4param, xform = function(x) x * 100),
               gg_x_max(nec4param) * 100, tolerance = 1e-8)
})

test_that("xform is applied to the predictor axis of a model set", {
  skip_on_cran()
  plain <- suppressMessages(ggbnec_data(manec_example))
  scaled <- suppressMessages(ggbnec_data(manec_example,
                                         xform = function(x) x * 100))
  expect_equal(max(scaled$x_e, na.rm = TRUE),
               max(plain$x_e, na.rm = TRUE) * 100, tolerance = 1e-8)
  # And the raw data column, R/autoplot.R:354. Dropping x_r from that mutate()
  # failed nothing until this line was added, while its bayesnecfit twin at
  # :310 was already observed.
  expect_equal(max(scaled$x_r, na.rm = TRUE),
               max(plain$x_r, na.rm = TRUE) * 100, tolerance = 1e-8)
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

test_that("a transformed response no longer suppresses xform (#268)", {
  # INVERTED. The guard was the length of find_transformations(), which answers
  # for the formula as a whole, so a transformation on the response suppressed
  # xform on a predictor nobody had transformed.
  #
  # INVERTED: the guard is now per-variable, so the two differ by the factor
  # xform applies, as in "xform is applied to the predictor axis of a single
  # fit" above.
  skip_on_cran()
  f <- transformed_response_fit(nec4param, "nec4param")
  expect_equal(gg_x_max(f, xform = function(x) x * 100), gg_x_max(f) * 100,
               tolerance = 1e-8)
})

test_that("a transformed response no longer suppresses xform for a set", {
  # INVERTED with its sibling above. The bayesmanecfit branch had the same
  # guard and the same defect.
  skip_on_cran()
  m <- transformed_response_manec(manec_example)
  expect_equal(gg_x_max(m, xform = function(x) x * 100), gg_x_max(m) * 100,
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


test_that("the ecx annotation is transformed while the curve is not", {
  # The other annotation. R/autoplot.R:316 passes xform to ecx() outside the
  # guard, exactly as :313 does to bind_nec(), and until this assertion was
  # added the ecx half of the claim made at the top of this file was not read:
  # gating ecx() alone left every assertion in the branch passing.
  #
  # add_ecx is asserted nowhere else in the suite either, so this is also the
  # only observer of R/autoplot.R:315-318 and of bind_ecx().
  #
  # Measured on nec4param with xform = x * 100: max(x_e) is 3.22 with and
  # without xform, while the largest ecx_vals changes from 1.56 to 156.4.
  #
  # INVERT THIS TEST WHEN #268 IS FIXED, with its siblings above.
  skip_on_cran()
  f <- transformed_response_fit(nec4param, "nec4param")
  plain <- suppressMessages(ggbnec_data(f, add_ecx = TRUE))
  scaled <- suppressMessages(ggbnec_data(f, add_ecx = TRUE,
                                         xform = function(x) x * 100))
  expect_equal(max(scaled$ecx_vals, na.rm = TRUE),
               max(plain$ecx_vals, na.rm = TRUE) * 100, tolerance = 1e-8)
})


test_that("the model set returns its ecx annotation on the other scale too", {
  # R/autoplot.R:360 passes xform to ecx() on the bayesmanecfit branch, outside
  # the guard at :352. The single-fit pin above does not observe it.
  #
  # INVERT THIS TEST WHEN #268 IS FIXED, with its siblings above.
  skip_on_cran()
  m <- transformed_response_manec(manec_example)
  plain <- suppressMessages(ggbnec_data(m, add_ecx = TRUE))
  scaled <- suppressMessages(ggbnec_data(m, add_ecx = TRUE,
                                         xform = function(x) x * 100))
  expect_equal(max(scaled$ecx_vals, na.rm = TRUE),
               max(plain$ecx_vals, na.rm = TRUE) * 100, tolerance = 1e-8)
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


# ---- the ECx annotation on an inline-transformed predictor -------------------

# to_axis_scale()'s numerical-inverse branch built a fresh vector, dropping the
# ecx_val attribute that bind_ecx() reads and assigns into the data frame, so
# the assignment received NULL and the call failed with "replacement has length
# zero". The branch runs only when the predictor is transformed inline AND
# xform is left at its default, which is why no existing test reached it: every
# transformed-predictor test here supplies an xform, and every default-xform
# test uses an untransformed predictor. That combination is the one
# vignette("example1") uses.
#
# The formula is rewritten on a stored fit rather than fitted, the same trick
# transformed_response_fit() uses in setup.R, applied to the predictor instead
# of the response. manec_example's x is 0.032 to 3.22, so log() is finite
# throughout.
transformed_predictor_fit <- function(fit, model) {
  fit$bayesnecformula <- bayesnecformula(
    stats::as.formula(paste0("y ~ crf(log(x), model = \"", model, "\")"))
  )
  fit
}

test_that("ggbnec_data annotates an ecx on a transformed predictor", {
  skip_on_cran()
  tf <- transformed_predictor_fit(nec4param, "nec4param")
  out <- suppressMessages(suppressWarnings(
    ggbnec_data(tf, add_ecx = TRUE)
  ))
  expect_s3_class(out, "data.frame")
  # The ecx_val attribute survives the inverse and reaches the data frame; it
  # is what labels the annotation, so a NULL here is the failure above.
  expect_equal(out$ecx_int[!is.na(out$ecx_int)], 10)
})

test_that("autoplot draws that annotation without error", {
  skip_on_cran()
  tf <- transformed_predictor_fit(nec4param, "nec4param")
  expect_s3_class(
    suppressMessages(suppressWarnings(autoplot(tf, ecx = TRUE))),
    "ggplot")
  # An xform supplied takes the other branch, which never lost the attribute.
  expect_s3_class(
    suppressMessages(suppressWarnings(
      autoplot(tf, ecx = TRUE, xform = exp))),
    "ggplot")
})
