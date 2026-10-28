# Use stored fits and changed formula metadata to check plotting only.
# Check both predictor axes and annotations on each class; a correction to
# one path does not establish that the others agree.

# Read axis limits from the device because base plot() returns no plot data.
# Do not suppress messages here: callers use this helper to assert silence.
pl_x_max <- function(obj, ...) {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  plot(obj, ...)
  par("usr")[2]
}

# Compare scale ratios so base graphics' axis expansion cancels. The same
# ratio can then be checked against the ggplot2 path.
pl_x_ratio <- function(obj) {
  pl_x_max(obj, xform = function(x) x * 100) / pl_x_max(obj)
}


# Check drawing calls and values; a silent call alone cannot show that a
# plotting argument took effect.

test_that("add_nec and add_ec10 decide what is annotated for a model set too", {
  # The model-set annotation uses a separate path. Check its values as well
  # as call counts; drawing NEC in the EC10 branch would preserve the count.
  skip_on_cran()
  drawn <- list()
  local_mocked_bindings(
    abline = function(v = NULL, ...) drawn[[length(drawn) + 1L]] <<- v,
    .package = "bayesnec"
  )
  suppressWarnings(pl_x_max(manec_example, add_nec = FALSE))
  expect_length(drawn, 0)
  suppressWarnings(pl_x_max(manec_example, add_nec = TRUE))
  expect_length(drawn, 1)
  expect_equal(unname(drawn[[1]]), unname(manec_example$w_ne), tolerance = 1e-8)
  suppressWarnings(pl_x_max(manec_example, add_nec = FALSE, add_ec10 = TRUE))
  expect_length(drawn, 2)
  expect_false(isTRUE(all.equal(unname(drawn[[2]]),
                                unname(manec_example$w_ne))))
  suppressWarnings(pl_x_max(manec_example, add_nec = TRUE, add_ec10 = TRUE))
  expect_length(drawn, 4)
  expect_equal(unname(drawn[[3]]), unname(manec_example$w_ne), tolerance = 1e-8)
  expect_equal(unname(drawn[[4]]), unname(drawn[[2]]), tolerance = 1e-8)
})

test_that("all_models draws a panel per candidate, named", {
  # Panel legend names identify the candidates actually drawn. A silent call
  # would not detect all_models being ignored.
  skip_on_cran()
  labels <- character()
  local_mocked_bindings(
    legend = function(..., legend = NULL) labels <<- c(labels, as.character(legend)),
    .package = "bayesnec"
  )
  # The deprecated all_models argument must warn once per call.
  old <- collect_warnings(pl_x_max(manec_example, all_models = TRUE))
  expect_length(old$warnings, 1)
  expect_match(old$warnings, "`all_models` is deprecated", fixed = TRUE)
  expect_true(all(names(manec_example$mod_fits) %in% labels))
  # The deprecated all_models = TRUE call draws no model average.
  expect_false("Model averaged predictions" %in% labels)
  # The model-average legend uses an estimate string, not a candidate name.
  labels <- character()
  old <- collect_warnings(pl_x_max(manec_example, all_models = FALSE))
  expect_length(old$warnings, 1)
  expect_false(any(names(manec_example$mod_fits) %in% labels))
})

test_that("all_models draws what its replacement arguments draw", {
  # Compare recorded drawing calls so each deprecated argument combination
  # matches its replacement in both values and layout.
  skip_on_cran()
  drawn <- list()
  local_mocked_bindings(
    legend = function(..., legend = NULL) {
      drawn[[length(drawn) + 1L]] <<- list(legend = legend)
    },
    abline = function(v = NULL, ...) drawn[[length(drawn) + 1L]] <<- list(v = v),
    .package = "bayesnec"
  )
  record <- function(...) {
    drawn <<- list()
    suppressWarnings(pl_x_max(manec_example, ...))
    drawn
  }
  expect_identical(record(all_models = TRUE),
                   record(model = manec_example$success_models,
                          average = FALSE))
  expect_identical(record(all_models = FALSE), record())
  expect_identical(record(all_models = TRUE, add_ec10 = TRUE),
                   record(model = manec_example$success_models,
                          average = FALSE, add_ec10 = TRUE))
})

test_that("model and average choose the panels and their layout", {
  # Read the layout through par(). A lone panel uses the whole device;
  # multiple panels use two columns with the model average first.
  skip_on_cran()
  labels <- character()
  layouts <- list()
  local_mocked_bindings(
    legend = function(..., legend = NULL) labels <<- c(labels, as.character(legend)),
    par = function(...) {
      args <- list(...)
      if (!is.null(args$mfrow)) layouts[[length(layouts) + 1L]] <<- args$mfrow
      graphics::par(...)
    },
    .package = "bayesnec"
  )
  panels <- function(...) {
    labels <<- character()
    layouts <<- list()
    pl_x_max(manec_example, ...)
    list(labels = intersect(labels, c("Model averaged predictions",
                                      manec_example$success_models)),
         layout = layouts)
  }
  one <- panels(model = "nec4param")
  expect_identical(one$labels, c("Model averaged predictions", "nec4param"))
  expect_identical(one$layout, list(c(1, 2)))
  alone <- panels(model = "ecx4param", average = FALSE)
  expect_identical(alone$labels, "ecx4param")
  expect_identical(alone$layout, list(c(1, 1)))
  # The equations follow the model average in the order given.
  all3 <- panels(model = c("ecx4param", "nec4param"))
  expect_identical(all3$labels, c("Model averaged predictions", "ecx4param",
                                  "nec4param"))
  expect_identical(all3$layout, list(c(2, 2)))
  # The default is the model average alone, with no layout set.
  plain <- panels()
  expect_length(plain$labels, 0)
  expect_length(plain$layout, 0)
})

test_that("the panels plot() draws for model are silent and restore par", {
  skip_on_cran()
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  before <- par("mfrow")
  expect_silent(out <- withVisible(plot(manec_example, model = "nec4param")))
  expect_null(out$value)
  expect_false(out$visible)
  expect_identical(par("mfrow"), before)
})

test_that("plot() refuses a model outside the set and an empty selection", {
  skip_on_cran()
  expect_error(pl_x_max(manec_example, model = c("nec4param", "nope")),
               "not in this set: \"nope\"")
  expect_error(pl_x_max(manec_example, model = NULL, average = FALSE),
               "Nothing is selected")
  expect_error(pl_x_max(manec_example, all_models = TRUE,
                        model = "nec4param"),
               "cannot be combined")
  expect_error(pl_x_max(manec_example, all_models = TRUE, average = FALSE),
               "cannot be combined")
})

test_that("add_nec and add_ec10 decide what is annotated", {
  # Count annotation calls and check their values: NEC and EC10 each draw
  # one line set, so a count alone cannot distinguish their branches.
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
  # This Gaussian fixture uses the range EC10. Check that it differs from
  # NEC here; test-ecx.R checks the estimator value.
  pl_x_max(nec4param, add_nec = FALSE, add_ec10 = TRUE)
  expect_length(drawn, 2)
  expect_false(isTRUE(all.equal(unname(drawn[[2]]), unname(nec4param$ne))))
  # NEC precedes EC10. The single-fit estimator reuses stored draws, so the
  # EC10 values must match the earlier call without resampling.
  pl_x_max(nec4param, add_nec = TRUE, add_ec10 = TRUE)
  expect_length(drawn, 4)
  expect_equal(unname(drawn[[3]]), unname(nec4param$ne), tolerance = 1e-8)
  expect_equal(unname(drawn[[4]]), unname(drawn[[2]]), tolerance = 1e-8)
})


# Axis arguments and guards

test_that("the axis is always drawn with at and labels, on either class", {
  # Check both fit classes: each has a separate axis path and argument guard.
  skip_on_cran()
  expect_error(pl_x_max(nec4param, lxform = "not a function"),
               "lxform must be a function")
  expect_error(pl_x_max(manec_example, lxform = "not a function"),
               "lxform must be a function")
  calls <- list()
  local_mocked_bindings(
    axis = function(...) calls[[length(calls) + 1L]] <<- names(list(...)),
    .package = "bayesnec"
  )
  pl_x_max(nec4param)
  pl_x_max(nec4param, xticks = c(0, 1, 2, 3))
  pl_x_max(manec_example)
  pl_x_max(manec_example, xticks = c(0, 1, 2, 3))
  # Check the count before all(): an empty collection would otherwise pass
  # even if no axis were drawn.
  expect_length(calls, 4)
  # Require both tick positions and labels; an axis() call with either
  # argument missing would pass a weaker call-count check.
  expect_true(all(vapply(calls, function(nm) all(c("at", "labels") %in% nm),
                         logical(1))))
})

test_that("position_legend accepts both forms legend() accepts", {
  # Pass numeric legend coordinates as separate x and y arguments. A pair
  # given as x alone describes rectangle corners when y is NULL. Intercept
  # legend(), since its position does not change the axis limits.
  skip_on_cran()
  seen <- list()
  local_mocked_bindings(
    legend = function(x, y = NULL, ...) {
      seen[[length(seen) + 1L]] <<- list(x = x, y = y)
      invisible(NULL)
    },
    .package = "bayesnec"
  )
  pl_x_max(nec4param, position_legend = c(0.5, 0.25))
  pl_x_max(manec_example, position_legend = c(0.5, 0.25))
  pl_x_max(nec4param, position_legend = "bottomleft")
  expect_length(seen, 3)
  expect_identical(seen[[1]], list(x = 0.5, y = 0.25))
  expect_identical(seen[[2]], list(x = 0.5, y = 0.25))
  expect_identical(seen[[3]], list(x = "bottomleft", y = NULL))
})

test_that("position_legend refuses what legend() cannot use, either class", {
  # Check each class and reject mixed valid/invalid keywords before legend()
  # raises an error that does not name position_legend.
  skip_on_cran()
  for (obj in list(nec4param, manec_example)) {
    msg <- tryCatch(pl_x_max(obj, position_legend = c("topright", "bogus")),
                    error = conditionMessage)
    expect_match(msg, "position_legend must be one of")
    expect_false(grepl("'arg' must be of length 1", msg))
    # An unknown keyword on its own, and a numeric of the wrong length: the two
    # forms are each refused by the message that names both.
    expect_error(pl_x_max(obj, position_legend = "bogus"),
                 "position_legend must be one of")
    expect_error(pl_x_max(obj, position_legend = c(0.5, 0.5, 0.5)),
                 "position_legend must be one of")
    # The documented keyword set deliberately excludes legend()'s "center".
    # Widening it changes that contract.
    expect_error(pl_x_max(obj, position_legend = "center"),
                 "position_legend must be one of")
  }
})


# Predictor-axis transformations

test_that("xform widens the predictor axis of a single fit", {
  skip_on_cran()
  expect_equal(pl_x_ratio(nec4param), 100, tolerance = 1e-6)
})

test_that("xform widens the predictor axis of a model set", {
  skip_on_cran()
  expect_equal(pl_x_ratio(manec_example), 100, tolerance = 1e-6)
})


# Response transformations and annotations

test_that("a transformed response no longer suppresses xform (#268)", {
  # A response transformation must not suppress xform on the predictor axis.
  skip_on_cran()
  f <- transformed_response_fit(nec4param, "nec4param")
  expect_equal(pl_x_ratio(f), 100, tolerance = 1e-6)
})

test_that("a transformed response no longer suppresses xform for a set", {
  # The model-set axis has a separate transformation path.
  skip_on_cran()
  m <- transformed_response_manec(manec_example)
  expect_equal(pl_x_ratio(m), 100, tolerance = 1e-6)
})

test_that("the nec annotation is drawn off the end of the axis", {
  # Intercept abline() to check what plot() actually draws. The NEC and its
  # bounds must fit inside the transformed axis, independently of the axis
  # ratio check above.
  skip_on_cran()
  f <- transformed_response_fit(nec4param, "nec4param")
  drawn <- NULL
  local_mocked_bindings(abline = function(v = NULL, ...) drawn <<- v,
                        .package = "bayesnec")
  axis_max <- pl_x_max(f, xform = function(x) x * 100)
  expect_length(drawn, 3)
  expect_lte(max(drawn), axis_max)
})

test_that("the model set draws its nec annotation off the axis too", {
  # Check the model-set NEC annotation separately: it has its own plotting
  # path, so the single-fit assertion cannot detect a mismatch here.
  skip_on_cran()
  m <- transformed_response_manec(manec_example)
  drawn <- NULL
  local_mocked_bindings(abline = function(v = NULL, ...) drawn <<- v,
                        .package = "bayesnec")
  axis_max <- pl_x_max(m, xform = function(x) x * 100)
  expect_length(drawn, 3)
  expect_lte(max(drawn), axis_max)
})

test_that("the ec10 annotation is drawn off the axis as well", {
  # Check EC10 separately from NEC; the two annotations use different paths.
  skip_on_cran()
  f <- transformed_response_fit(nec4param, "nec4param")
  drawn <- NULL
  local_mocked_bindings(abline = function(v = NULL, ...) drawn <<- v,
                        .package = "bayesnec")
  axis_max <- pl_x_max(f, add_nec = FALSE, add_ec10 = TRUE,
                       xform = function(x) x * 100)
  expect_length(drawn, 3)
  expect_lte(max(drawn), axis_max)
})

test_that("the model set draws its ec10 annotation off the axis too", {
  # Check the model-set EC10 path independently of its single-fit counterpart.
  skip_on_cran()
  m <- transformed_response_manec(manec_example)
  drawn <- NULL
  local_mocked_bindings(abline = function(v = NULL, ...) drawn <<- v,
                        .package = "bayesnec")
  axis_max <- suppressWarnings(
    pl_x_max(m, add_nec = FALSE, add_ec10 = TRUE, xform = function(x) x * 100)
  )
  expect_length(drawn, 3)
  expect_lte(max(drawn), axis_max)
})

test_that("plot and autoplot make the xform decision the same way", {
  # Compare the two plotting paths directly as well as their individual
  # expected ratios. Ratios remove base graphics' axis expansion.
  skip_on_cran()
  f <- transformed_response_fit(nec4param, "nec4param")
  gg_ratio <- gg_x_max(f, xform = function(x) x * 100) / gg_x_max(f)
  expect_equal(pl_x_ratio(f), gg_ratio, tolerance = 1e-6)
})
