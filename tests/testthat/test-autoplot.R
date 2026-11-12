skip_unless_full_suite()

# Use stored fits and changed formula metadata to check plotting only.
# Assert both predictor and annotation scales on each class; correcting one
# path does not establish that the others agree.


# Plotting-frame columns

test_that("ggbnec_data returns the columns autoplot draws from", {
  skip_on_cran()
  d <- suppressMessages(ggbnec_data(nec4param))
  expect_true(all(c("x_e", "y_e", "y_ci", "x_r", "y_r") %in% names(d)))
  expect_gt(nrow(d), 0)
})

test_that("ggbnec_data draws the stored prediction curve", {
  skip_on_cran()
  # The stored grid sets a rate() denominator to 1, so drawing it keeps a rate
  # fit's curve on the scale of its plotted observations.
  d <- suppressMessages(ggbnec_data(nec4param, add_nec = FALSE))
  curve <- d$y_e[!is.na(d$y_e)]
  expect_equal(curve, nec4param$pred_vals$data$Estimate)
  expect_equal(d$x_e[seq_along(curve)], nec4param$pred_vals$data$x)
})

test_that("ggbnec_data returns the same frame shape for a model set", {
  skip_on_cran()
  d <- suppressMessages(ggbnec_data(manec_example))
  expect_true(all(c("x_e", "y_e", "y_ci", "x_r", "y_r") %in% names(d)))
  # Check add_nec on the model-set path as well as the single-fit path.
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

test_that("expand_manec retains identical unused columns", {
  skip_on_cran()
  retained <- data.frame(
    climate = factor(rep(c("ambient", "warm", "hot"),
                         length.out = nrow(nec4param$fit$data))),
    row.names = rownames(nec4param$fit$data)
  )
  fits <- list(nec4param, ecx4param)
  fits <- lapply(fits, function(fit) {
    fit$retained_data <- retained
    fit
  })
  recovered <- unlist(lapply(fits, bayesnec:::recover_prebayesnecfit),
                      recursive = FALSE)
  formulas <- lapply(recovered, bayesnec:::extract_formula)
  object <- expand_manec(recovered, formulas) |>
    suppressMessages() |>
    suppressWarnings() |>
    (\(z) bayesnec:::allot_class(z, c("bayesmanecfit", "bnecfit")))()

  expect_identical(object$retained_data, retained)
  expect_true(all(vapply(object$mod_fits, function(fit) {
    is.null(fit$retained_data)
  }, logical(1))))
  expect_no_error(suppressMessages(
    autoplot(object, group = "climate", group_aes = "colour", nec = FALSE)
  ))
})

test_that("expand_manec omits different unused columns", {
  skip_on_cran()
  fits <- list(nec4param, ecx4param)
  fits[[1]]$retained_data <- data.frame(
    climate = factor(rep(c("ambient", "warm"),
                         length.out = nrow(fits[[1]]$fit$data)))
  )
  fits[[2]]$retained_data <- data.frame(
    climate = factor(rep(c("ambient", "hot"),
                         length.out = nrow(fits[[2]]$fit$data)))
  )
  recovered <- unlist(lapply(fits, bayesnec:::recover_prebayesnecfit),
                      recursive = FALSE)
  formulas <- lapply(recovered, bayesnec:::extract_formula)

  expect_message(
    object <- suppressWarnings(expand_manec(recovered, formulas)),
    "retain different unused data columns"
  )
  expect_null(object$retained_data)
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
  expect_true(attr(dat, "group_fitted"))
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
  # ggbnec_data() takes add_nec, while autoplot() takes nec. The former
  # currently swallows nec through dots; this records that behaviour without
  # endorsing the argument mismatch.
  skip_on_cran()
  swallowed <- suppressMessages(ggbnec_data(nec4param, nec = FALSE))
  expect_true("nec_vals" %in% names(swallowed))
})


test_that("a non-function xform is refused the same way on both classes", {
  # Check both method guards so a bad xform fails by argument name before
  # reaching the transformation code.
  skip_on_cran()
  expect_error(ggbnec_data(nec4param, xform = "no"),
               "xform must be a function")
  expect_error(ggbnec_data(manec_example, xform = "no"),
               "xform must be a function")
})


# Predictor-axis transformations

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
  # Check raw-data coordinates separately from the fitted-curve coordinates.
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


# Response transformations and annotations

test_that("find_transformations reports the response for this fixture", {
  # Only the response is transformed in this fixture. The plotting guard
  # must inspect the predictor separately; this assertion checks the fixture,
  # while test-fit_bayesnec.R checks pop_var_is_transformed().
  skip_on_cran()
  f <- transformed_response_fit(nec4param, "nec4param")
  bdat <- model.frame(f$bayesnecformula, data = f$fit$data,
                      run_par_checks = TRUE)
  expect_identical(find_transformations(bdat), "y")
})

test_that("a transformed response no longer suppresses xform (#268)", {
  # The response transformation must not suppress predictor-axis xform.
  skip_on_cran()
  f <- transformed_response_fit(nec4param, "nec4param")
  expect_equal(gg_x_max(f, xform = function(x) x * 100), gg_x_max(f) * 100,
               tolerance = 1e-8)
})

test_that("a transformed response no longer suppresses xform for a set", {
  # The model-set axis has a separate transformation path.
  skip_on_cran()
  m <- transformed_response_manec(manec_example)
  expect_equal(gg_x_max(m, xform = function(x) x * 100), gg_x_max(m) * 100,
               tolerance = 1e-8)
})

test_that("the nec annotation is transformed while the axis is not", {
  # Check the NEC annotation as well as the predictor scale above. Both must
  # receive xform; an annotation-only check cannot detect a mismatched axis.
  skip_on_cran()
  f <- transformed_response_fit(nec4param, "nec4param")
  plain <- suppressMessages(ggbnec_data(f))
  scaled <- suppressMessages(ggbnec_data(f, xform = function(x) x * 100))
  expect_equal(max(scaled$nec_vals, na.rm = TRUE),
               max(plain$nec_vals, na.rm = TRUE) * 100, tolerance = 1e-8)
})


test_that("the model set returns its nec annotation on the other scale too", {
  # The model-set annotation takes a separate path from the single-fit one.
  # Check its scaling independently, alongside the model-set predictor test.
  skip_on_cran()
  m <- transformed_response_manec(manec_example)
  plain <- suppressMessages(ggbnec_data(m))
  scaled <- suppressMessages(ggbnec_data(m, xform = function(x) x * 100))
  expect_equal(max(scaled$nec_vals, na.rm = TRUE),
               max(plain$nec_vals, na.rm = TRUE) * 100, tolerance = 1e-8)
})


test_that("the ecx annotation is transformed while the curve is not", {
  # Check the ECx annotation separately from the NEC annotation and axis.
  # A change to one annotation path must not leave the other untested.
  skip_on_cran()
  f <- transformed_response_fit(nec4param, "nec4param")
  plain <- suppressMessages(ggbnec_data(f, add_ecx = TRUE))
  scaled <- suppressMessages(ggbnec_data(f, add_ecx = TRUE,
                                         xform = function(x) x * 100))
  expect_equal(max(scaled$ecx_vals, na.rm = TRUE),
               max(plain$ecx_vals, na.rm = TRUE) * 100, tolerance = 1e-8)
})


test_that("the model set returns its ecx annotation on the other scale too", {
  # The model-set ECx annotation has its own path; the single-fit assertion
  # does not test it.
  skip_on_cran()
  m <- transformed_response_manec(manec_example)
  plain <- suppressMessages(ggbnec_data(m, add_ecx = TRUE))
  scaled <- suppressMessages(ggbnec_data(m, add_ecx = TRUE,
                                         xform = function(x) x * 100))
  expect_equal(max(scaled$ecx_vals, na.rm = TRUE),
               max(plain$ecx_vals, na.rm = TRUE) * 100, tolerance = 1e-8)
})


# Plot objects

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


# Compare layer data to check the model and average arguments and their
# deprecated all_models mapping.

layers_data <- function(p) lapply(p$layers, function(layer) layer$data)

# The columns ggbnec() reads, for comparing a panel against the plot of the
# same fit drawn on its own; the facet path adds columns joined from the
# summary that ggbnec() does not read.
drawn_columns <- c("x_e", "y_e", "y_ci", "x_r", "y_r", "nec_vals", "nec_labs",
                   "nec_labs_l", "nec_labs_u", "model", "tag")

facet_models <- function(p) {
  layers_data(p)[[1]]$model
}

test_that("all_models = TRUE warns once and draws what model draws", {
  skip_on_cran()
  old <- collect_warnings(
    suppressMessages(autoplot(manec_example, all_models = TRUE))
  )
  expect_length(old$warnings, 1)
  expect_match(old$warnings, "`all_models` is deprecated", fixed = TRUE)
  new <- suppressMessages(
    autoplot(manec_example, model = manec_example$success_models,
             average = FALSE)
  )
  expect_identical(layers_data(old$value), layers_data(new))
  # The deprecated call keeps character model names, so facet_wrap() orders
  # its equation panels alphabetically.
  expect_type(facet_models(new), "character")
  expect_setequal(unique(facet_models(new)), manec_example$success_models)
})

test_that("all_models = FALSE warns once and draws the model average alone", {
  skip_on_cran()
  old <- collect_warnings(
    suppressMessages(autoplot(manec_example, all_models = FALSE))
  )
  expect_length(old$warnings, 1)
  default <- suppressMessages(autoplot(manec_example))
  expect_identical(layers_data(old$value), layers_data(default))
  expect_identical(unique(facet_models(default)),
                   "Model averaged predictions")
})

test_that("model adds a facet per equation after the model average", {
  skip_on_cran()
  p <- suppressMessages(autoplot(manec_example, model = "nec4param"))
  m <- facet_models(p)
  expect_s3_class(m, "factor")
  expect_identical(levels(m), c("Model averaged predictions", "nec4param"))
  expect_setequal(as.character(unique(m)), levels(m))
  # The model-average facet is the plot of the model average alone.
  avg <- layers_data(p)[[1]]
  avg <- avg[avg$model == "Model averaged predictions", drawn_columns]
  alone <- layers_data(suppressMessages(autoplot(manec_example)))[[1]]
  alone <- alone[, drawn_columns]
  rownames(avg) <- rownames(alone) <- NULL
  avg$model <- as.character(avg$model)
  expect_identical(avg, alone)
  # The model average comes first; equation facets follow alphabetically.
  p2 <- suppressMessages(
    autoplot(manec_example, model = c("nec4param", "ecx4param"))
  )
  expect_identical(levels(facet_models(p2)),
                   c("Model averaged predictions", "ecx4param", "nec4param"))
  # One vertical line layer entry per estimate and bound: three per facet.
  nec_rows <- layers_data(p2)[[which(vapply(p2$layers, function(layer) {
    inherits(layer$geom, "GeomVline")
  }, logical(1)))]]
  expect_equal(nrow(nec_rows), 9)
})

test_that("average = FALSE draws a named equation as it is drawn alone", {
  skip_on_cran()
  p <- suppressMessages(
    autoplot(manec_example, model = "nec4param", average = FALSE)
  )
  expect_identical(unique(facet_models(p)), "nec4param")
  single <- suppressMessages(autoplot(nec4param))
  a <- layers_data(p)[[1]][, drawn_columns]
  b <- layers_data(single)[[1]][, drawn_columns]
  rownames(a) <- rownames(b) <- NULL
  expect_identical(a, b)
})

test_that("multi_facet = FALSE returns a plot per panel, the average first", {
  skip_on_cran()
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  pl <- suppressMessages(
    autoplot(manec_example, model = c("nec4param", "ecx4param"),
             multi_facet = FALSE, ask = FALSE)
  )
  expect_length(pl, 3)
  expect_identical(
    vapply(pl, function(p) unique(as.character(facet_models(p))),
           character(1)),
    c("Model averaged predictions", "nec4param", "ecx4param")
  )
  # The deprecated call returns one plot per equation in set order and warns
  # once.
  old <- collect_warnings(suppressMessages(
    autoplot(manec_example, all_models = TRUE, multi_facet = FALSE,
             ask = FALSE)
  ))
  expect_length(old$warnings, 1)
  expect_identical(
    vapply(old$value, function(p) unique(facet_models(p)), character(1)),
    manec_example$success_models
  )
})


# Count pages with grid's before.grid.newpage hook, which runs for each
# page ggplot2 draws with its default newpage = TRUE. This observes drawing
# without requiring a particular device type.

count_pages <- function(expr) {
  pages <- 0L
  old <- getHook("before.grid.newpage")
  setHook("before.grid.newpage", function() pages <<- pages + 1L)
  on.exit(setHook("before.grid.newpage", old, "replace"), add = TRUE)
  value <- expr
  list(value = value, pages = pages)
}

test_that("plot = FALSE draws nothing and leaves the prompt as it was", {
  skip_on_cran()
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  # The prompt is on at entry and ask = FALSE is passed, so a call that
  # reached devAskNewPage(ask = ask) leaves it off and the change is seen.
  grDevices::devAskNewPage(TRUE)
  off <- count_pages(suppressMessages(
    autoplot(manec_example, model = "nec4param", multi_facet = FALSE,
             plot = FALSE, ask = FALSE)
  ))
  expect_identical(off$pages, 0L)
  expect_true(grDevices::devAskNewPage())
  # plot = TRUE turns the prompt off for the first page and applies ask
  # after it; the setting found at entry is back once the call returns. It
  # is set again so that this half does not depend on the half above.
  grDevices::devAskNewPage(TRUE)
  on <- count_pages(suppressMessages(
    autoplot(manec_example, model = "nec4param", multi_facet = FALSE,
             plot = TRUE, ask = FALSE)
  ))
  expect_length(on$value, 2)
  expect_identical(on$pages, 2L)
  expect_true(grDevices::devAskNewPage())
  # Drawing changes nothing in what is returned.
  expect_identical(lapply(off$value, layers_data),
                   lapply(on$value, layers_data))
})

test_that("plot = FALSE opens no device on the deprecated all_models path", {
  skip_on_cran()
  # Without an open device, drawing opens one; with a device already open,
  # only the page count detects drawing. Check both effects.
  before <- grDevices::dev.list()
  res <- count_pages(collect_warnings(suppressMessages(
    autoplot(manec_example, all_models = TRUE, multi_facet = FALSE,
             plot = FALSE, ask = FALSE)
  )))
  expect_identical(grDevices::dev.list(), before)
  expect_identical(res$pages, 0L)
  expect_length(res$value$warnings, 1)
  expect_identical(
    vapply(res$value$value, function(p) unique(facet_models(p)),
           character(1)),
    manec_example$success_models
  )
})

test_that("autoplot refuses a model outside the set and an empty selection", {
  skip_on_cran()
  msg <- tryCatch(autoplot(manec_example, model = c("ecx", "nope")),
                  error = conditionMessage)
  expect_match(msg, "not in this set: \"ecx\", \"nope\"", fixed = TRUE)
  # A group name, which pull_out() would accept, is named as such.
  expect_match(msg, "\"ecx\" names a group of equations", fixed = TRUE)
  expect_error(autoplot(manec_example, average = FALSE),
               "Nothing is selected")
  expect_error(autoplot(manec_example, model = character(0)),
               "must be NULL or a character vector")
  expect_error(autoplot(manec_example, average = NA), "flag")
  expect_error(autoplot(manec_example, all_models = FALSE, model = NULL),
               "cannot be combined")
})


# ECx annotations on transformed predictors

# Preserve ecx_val when inverting an inline predictor transform; bind_ecx()
# fails if the attribute is lost. Relabel the stored fit without refitting.
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
  # The numerical inverse must preserve ecx_val, which labels the annotation.
  expect_equal(out$ecx_int[!is.na(out$ecx_int)], 10)
})

test_that("autoplot draws that annotation without error", {
  skip_on_cran()
  tf <- transformed_predictor_fit(nec4param, "nec4param")
  expect_s3_class(
    suppressMessages(suppressWarnings(autoplot(tf, ecx = TRUE))),
    "ggplot")
  # An explicit xform exercises the separate user-supplied inverse path.
  expect_s3_class(
    suppressMessages(suppressWarnings(
      autoplot(tf, ecx = TRUE, xform = exp))),
    "ggplot")
})

# The layers that draw observations. stat_summary() also draws points, the
# per-level means, which point_alpha must leave opaque.
is_observation_layer <- function(layer) {
  inherits(layer$geom, "GeomPoint") && !inherits(layer$stat, "StatSummary")
}

# The alpha of every observation drawn, read from the built plot. Fail on a
# plot that draws none, so that all() over an empty vector cannot pass.
observation_alpha <- function(p) {
  i <- which(vapply(p$layers, is_observation_layer, logical(1)))
  out <- unlist(lapply(i, function(k) ggplot2::layer_data(p, k)$alpha),
                use.names = FALSE)
  expect_gt(length(out), 0)
  out
}

grouped_object <- function(fit = grouped_plot_fit()) {
  structure(
    list(fits = list(a = fit, b = fit), group_var = "site",
         levels = c("a", "b")),
    class = c("bayesnecgroupfit", "bnecfit")
  )
}

# joint_split() records its levels in `data` only. The plot assigns each
# observation to a panel from the fitted data, so copy the levels there.
joint_with_rows <- function() {
  j <- joint_split()
  j$fit$data$site <- j$data$site
  j
}

test_that("observation points are drawn opaque by default", {
  skip_on_cran()
  p <- suppressMessages(autoplot(nec4param, nec = FALSE))
  obs <- Filter(is_observation_layer, p$layers)
  expect_length(obs, 1)
  expect_identical(obs[[1]]$aes_params$alpha, 1)
  expect_identical(obs[[1]]$aes_params$fill, "grey30")
  expect_identical(obs[[1]]$aes_params$shape, 21)
  expect_equal(observation_alpha(p), rep(1, nrow(nec4param$fit$data)))
  p_set <- suppressMessages(autoplot(manec_example, nec = FALSE))
  expect_true(all(observation_alpha(p_set) == 1))
  p_group <- suppressMessages(autoplot(grouped_object(), nec = FALSE))
  expect_true(all(observation_alpha(p_group) == 1))
  p_joint <- suppressMessages(autoplot(joint_with_rows(), nec = FALSE))
  expect_true(all(observation_alpha(p_joint) == 1))
})

test_that("point_alpha sets the opacity of the observations alone", {
  skip_on_cran()
  p <- suppressMessages(autoplot(nec4param, point_alpha = 0.3))
  expect_equal(observation_alpha(p), rep(0.3, nrow(nec4param$fit$data)))
  band <- Filter(function(layer) inherits(layer$geom, "GeomPolygon"),
                 p$layers)[[1]]
  expect_identical(band$aes_params$alpha, 0.5)
  # The bounds of the range are accepted.
  for (a in c(0, 1)) {
    p_a <- suppressMessages(autoplot(nec4param, nec = FALSE, point_alpha = a))
    expect_true(all(observation_alpha(p_a) == a))
  }
})

test_that("point_alpha reaches every panel of a model set", {
  skip_on_cran()
  avg <- suppressMessages(autoplot(manec_example, nec = FALSE,
                                   point_alpha = 0.3))
  expect_true(all(observation_alpha(avg) == 0.3))
  faceted <- suppressMessages(autoplot(
    manec_example, nec = FALSE, model = c("nec4param", "ecx4param"),
    point_alpha = 0.3
  ))
  n_obs <- nrow(manec_example$mod_fits[[1]]$fit$data)
  expect_equal(observation_alpha(faceted), rep(0.3, 3 * n_obs))
  pages <- suppressMessages(autoplot(
    manec_example, nec = FALSE, model = "nec4param", multi_facet = FALSE,
    plot = FALSE, point_alpha = 0.3
  ))
  expect_length(pages, 2)
  for (page in pages) {
    expect_equal(observation_alpha(page), rep(0.3, n_obs))
  }
})

test_that("point_alpha applies to grouped observations but not their means", {
  skip_on_cran()
  fit <- grouped_plot_fit()
  for (aes_type in c("colour", "line")) {
    p <- suppressMessages(autoplot(fit, group = "plate", group_aes = aes_type,
                                   nec = FALSE, point_alpha = 0.3))
    obs <- Filter(is_observation_layer, p$layers)
    expect_length(obs, 1)
    expect_identical(obs[[1]]$aes_params$alpha, 0.3)
    expect_true(all(observation_alpha(p) == 0.3))
    means <- Filter(function(layer) inherits(layer$stat, "StatSummary"),
                    p$layers)
    expect_gt(length(means), 0)
    for (layer in means) {
      expect_null(layer$aes_params$alpha)
    }
  }
  coloured <- suppressMessages(autoplot(fit, group = "plate",
                                        group_aes = "colour", nec = FALSE,
                                        point_alpha = 0.3))
  obs <- Filter(is_observation_layer, coloured$layers)[[1]]
  expect_identical(rlang::as_label(obs$mapping$fill), "group")
  coloured_data <- ggplot2::layer_data(
    coloured, which(vapply(coloured$layers, is_observation_layer, logical(1)))
  )
  expect_length(unique(coloured_data$fill), 4)
  # A level observed at one concentration is marked by a point at its mean,
  # which is not an observation.
  single <- nec4param
  single$fit$data$dose_group <- factor(single$fit$data$x)
  single$bayesnecformula <- bayesnecformula(
    y ~ crf(x, model = "nec4param") + ogl(dose_group)
  )
  p_single <- suppressMessages(autoplot(single, group = "dose_group",
                                        group_aes = "colour", nec = FALSE,
                                        point_alpha = 0.3))
  mean_marks <- Filter(function(layer) inherits(layer$stat, "StatSummary"),
                       p_single$layers)
  expect_length(mean_marks, 1)
  expect_s3_class(mean_marks[[1]]$geom, "GeomPoint")
  expect_null(mean_marks[[1]]$aes_params$alpha)
  expect_true(all(observation_alpha(p_single) == 0.3))
})

test_that("point_alpha reaches the panels of grouped and joint fits", {
  skip_on_cran()
  object <- grouped_object()
  p <- suppressMessages(autoplot(object, nec = FALSE, point_alpha = 0.3))
  expect_true(all(observation_alpha(p) == 0.3))
  coloured <- suppressMessages(autoplot(object, group = "plate",
                                        group_aes = "colour", nec = FALSE,
                                        point_alpha = 0.3))
  obs <- Filter(is_observation_layer, coloured$layers)
  expect_length(obs, 1)
  expect_identical(rlang::as_label(obs[[1]]$mapping$fill), "group")
  expect_true(all(observation_alpha(coloured) == 0.3))
  joint <- joint_with_rows()
  p_joint <- suppressMessages(autoplot(joint, nec = FALSE, point_alpha = 0.3))
  expect_equal(observation_alpha(p_joint),
               rep(0.3, nrow(joint$fit$data)))
})

test_that("autoplot refuses a point_alpha that is not one number in [0, 1]", {
  skip_on_cran()
  expect_error(autoplot(nec4param, point_alpha = 1.5),
               "`point_alpha` must be between 0 and 1, not 1.5.",
               fixed = TRUE)
  expect_error(autoplot(nec4param, point_alpha = -0.1),
               "`point_alpha` must be between 0 and 1, not -0.1.",
               fixed = TRUE)
  for (bad in list(NA_real_, c(0.2, 0.5), "0.5", TRUE, NULL)) {
    expect_error(autoplot(nec4param, point_alpha = bad),
                 "`point_alpha` must be a number (non-missing numeric scalar).",
                 fixed = TRUE)
  }
  expect_error(autoplot(manec_example, point_alpha = 2),
               "`point_alpha` must be between 0 and 1", fixed = TRUE)
  expect_error(autoplot(grouped_object(), point_alpha = 2),
               "`point_alpha` must be between 0 and 1", fixed = TRUE)
  expect_error(autoplot(joint_split(), point_alpha = 2),
               "`point_alpha` must be between 0 and 1", fixed = TRUE)
})

test_that("a fitted axis spaces an offset-log curve and labels the control", {
  skip_on_cran()
  fit <- nec4param
  fit$fit$data$x[1:4] <- c(0, 0.01, 0.1, 1)
  fit$bayesnecformula <- bayesnecformula(
    y ~ crf(log(x + 0.001), model = "nec4param")
  )
  dat <- suppressMessages(ggbnec_data(fit, add_nec = FALSE,
                                       x_scale = "fitted"))
  curve_x <- dat$x_e[!is.na(dat$y_e)]
  expect_equal(diff(curve_x), rep(diff(curve_x)[1], length(curve_x) - 1))
  expect_equal(dat$x_r[!is.na(dat$y_r)], log(fit$fit$data$x + 0.001))
  axis <- attr(dat, "x_axis")
  expect_true("0" %in% axis$labels)
  expect_equal(axis$breaks[axis$labels == "0"], log(0.001))
  p <- suppressMessages(autoplot(fit, nec = FALSE, x_scale = "fitted"))
  expect_equal(p$scales$get_scales("x")$breaks, axis$breaks)
  expect_error(ggbnec_data(fit, add_nec = FALSE, x_scale = "fitted",
                           xform = exp), "cannot be combined")
})

test_that("a fitted axis labels its estimate lines in recorded units", {
  skip_on_cran()
  fit <- nec4param
  fit$fit$data$x[1:4] <- c(0, 0.01, 0.1, 1)
  fit$bayesnecformula <- bayesnecformula(
    y ~ crf(log(x + 0.001), model = "nec4param")
  )
  dat <- suppressMessages(ggbnec_data(fit, x_scale = "fitted"))
  recorded <- exp(unname(fit$ne)) - 0.001
  expect_equal(dat$nec_labs[!is.na(dat$nec_labs)], rounded(recorded[1], 2))
  expect_equal(dat$nec_labs_l[!is.na(dat$nec_labs_l)], rounded(recorded[2], 2))
  expect_equal(dat$nec_labs_u[!is.na(dat$nec_labs_u)], rounded(recorded[3], 2))
  expect_equal(dat$nec_vals[!is.na(dat$nec_vals)], unname(fit$ne),
               ignore_attr = TRUE)
})

test_that("an untransformed predictor has identical recorded and fitted plots", {
  skip_on_cran()
  plain <- suppressMessages(ggbnec_data(nec4param, add_nec = FALSE))
  fitted <- suppressMessages(ggbnec_data(nec4param, add_nec = FALSE,
                                         x_scale = "fitted"))
  expect_identical(fitted, plain)
  expect_null(attr(fitted, "x_axis"))
})

test_that("a fitted axis retains the stored prediction range", {
  skip_on_cran()
  formula <- bayesnecformula(
    y ~ crf(log(x + 0.001), model = "nec4param")
  )
  fit <- nec4param
  fit$bayesnecformula <- formula
  fit$pred_vals$data$x <- seq(min(fit$pred_vals$data$x), 5,
                              length.out = nrow(fit$pred_vals$data))
  single <- suppressMessages(ggbnec_data(fit, add_nec = FALSE,
                                          x_scale = "fitted"))
  expect_equal(max(single$x_e, na.rm = TRUE), log(5.001))

  average <- manec_example
  average$mod_fits <- lapply(average$mod_fits, function(part) {
    part$bayesnecformula <- formula
    part
  })
  average$w_pred_vals$data$x <- seq(min(average$w_pred_vals$data$x), 5,
                                     length.out = nrow(average$w_pred_vals$data))
  averaged <- suppressMessages(ggbnec_data(average, add_nec = FALSE,
                                            x_scale = "fitted"))
  expect_equal(max(averaged$x_e, na.rm = TRUE), log(5.001))
})

test_that("fitted axes reach model sets, grouped fits and joint refits", {
  skip_on_cran()
  formula <- bayesnecformula(
    y ~ crf(log(x + 0.001), model = "nec4param")
  )
  average <- manec_example
  average$mod_fits <- lapply(average$mod_fits, function(fit) {
    fit$bayesnecformula <- formula
    fit
  })
  avg_data <- suppressMessages(ggbnec_data(average, add_nec = FALSE,
                                           x_scale = "fitted"))
  expect_equal(avg_data$x_r[!is.na(avg_data$y_r)],
               log(average$mod_fits[[1]]$fit$data$x + 0.001))
  expect_true(length(attr(avg_data, "x_axis")$breaks) > 0)

  fit <- nec4param
  fit$bayesnecformula <- formula
  grouped <- structure(
    list(fits = list(a = fit, b = fit), group_var = "site",
         levels = c("a", "b")),
    class = c("bayesnecgroupfit", "bnecfit")
  )
  group_data <- suppressMessages(ggbnec_data(grouped, add_nec = FALSE,
                                             x_scale = "fitted"))
  expect_true(length(attr(group_data, "x_axis")$breaks) > 0)
  expect_equal(length(unique(group_data$panel)), 2)

  joint <- joint_split()
  joint$bayesnecformula <- formula
  joint_data <- suppressMessages(ggbnec_data(joint, add_nec = FALSE,
                                             x_scale = "fitted"))
  expect_true(length(attr(joint_data, "x_axis")$breaks) > 0)
  expect_equal(length(unique(joint_data$panel)), 2)
})

# A joint two-block fit with a smooth response block and a threshold survival
# block, on a log predictor so that the fitted axis applies. Fitted once per
# test process.
joint_hurdle_cache <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      set.seed(564)
      conc <- rep(c(0.1, 0.3, 1, 3, 10, 30), each = 10)
      mu <- 20 / (1 + exp(1.2 * (log(conc) - log(4))))
      alive <- stats::rbinom(length(conc), 1,
                             0.95 * exp(-0.12 * pmax(conc - 5, 0)))
      y <- ifelse(alive == 1, stats::rgamma(length(conc), 15, 15 / mu), 0)
      cached <<- suppressWarnings(suppressMessages(bnec(
        y ~ crf(log(x), "ecx4param"), data = data.frame(x = conc, y = y),
        family = "hurdle_gamma", model_survival = "nec3param",
        chains = 2, iter = 600, warmup = 300, seed = 564, refresh = 0,
        resolution = 50
      )))
    }
    cached
  }
})

# A two-block model set whose survival block is a smooth equation, on an
# untransformed predictor. Fitted once per test process.
joint_set_cache <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      set.seed(565)
      conc <- rep(c(0, 0.5, 1, 2, 4, 8), each = 10)
      mu <- 20 * exp(-0.4 * pmax(conc - 1.5, 0))
      alive <- stats::rbinom(length(conc), 1, 0.95 / (1 + (conc / 4)^3))
      y <- ifelse(alive == 1, stats::rgamma(length(conc), 15, 15 / mu), 0)
      cached <<- suppressWarnings(suppressMessages(bnec(
        y ~ crf(x, c("nec3param", "ecx4param")),
        data = data.frame(x = conc, y = y), family = "hurdle_gamma",
        model_survival = "ecxll3", chains = 2, iter = 600, warmup = 300,
        seed = 565, refresh = 0, resolution = 50
      )))
    }
    cached
  }
})

panel_titles <- function(p) {
  layout <- ggplot2::ggplot_build(p)$layout$layout
  as.character(layout$model[order(layout$PANEL)])
}

joint_titles <- paste0(
  c("Response per individual exposed", "Response of survivors",
    "Probability of survival"),
  "\n", c("ecx4param / nec3param", "ecx4param", "nec3param")
)

test_that("which draws the blocks of a joint two-block fit", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  fit <- joint_hurdle_cache()
  q <- function(expr) suppressWarnings(suppressMessages(expr))

  # One panel, titled by both equations, with the curve on the y axis.
  p <- q(autoplot(fit))
  expect_equal(panel_titles(p), "ecx4param / nec3param")
  expect_equal(p$labels$y, "Response per individual exposed")

  # The panels and titles of autoplot.bayesnechurdlefit(), each naming the
  # equation of its block. The titles name the curves, so the y axis has no
  # label.
  p <- q(autoplot(fit, which = "all"))
  expect_equal(panel_titles(p), joint_titles)
  expect_true("y" %in% names(p$labels))
  expect_null(p$labels$y)
  # One row with the x axis shared, as autoplot.bayesnechurdlefit() draws.
  layout <- ggplot2::ggplot_build(p)$layout$layout
  expect_equal(layout$ROW[order(layout$PANEL)], rep(1L, 3))
  expect_equal(layout$COL[order(layout$PANEL)], 1:3)
  expect_false(p$facet$params$free$x)
  expect_true(p$facet$params$free$y)

  p <- q(autoplot(fit, which = "growth"))
  expect_equal(panel_titles(p), "ecx4param")
  expect_equal(p$labels$y, "Response of survivors")
  p <- q(autoplot(fit, which = "survival", ylab = "Alive"))
  expect_equal(panel_titles(p), "nec3param")
  expect_equal(p$labels$y, "Alive")

  # The no-effect label names the estimate each panel draws: the response
  # block has no nec parameter, the survival block does.
  p <- q(autoplot(fit, which = "all"))
  text <- Filter(function(layer) inherits(layer$geom, "GeomText"),
                 p$layers)[[1]]$data
  expect_equal(text$tag,
               c(rownames(q(summary(fit))$nec_vals)[1], "NSEC", "NEC"))
})

test_that("the block panels of a joint fit match the estimators", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  fit <- joint_hurdle_cache()
  q <- function(expr) suppressWarnings(suppressMessages(expr))
  d <- q(ggbnec_data(fit, which = "all", add_ecx = TRUE))
  expect_equal(levels(d$panel), unname(bayesnec:::hurdle_panel_names))
  panel <- function(name) d[d$panel == name, , drop = FALSE]
  growth <- panel("Response of survivors")
  survival <- panel("Probability of survival")
  combined <- panel("Response per individual exposed")
  obs <- fit$fit$data

  # The observations: every row, the survivors, and the proportion alive at
  # each concentration.
  expect_equal(sum(!is.na(combined$y_r)), nrow(obs))
  expect_equal(sort(growth$y_r[!is.na(growth$y_r)]), sort(obs$y[obs$y > 0]))
  alive <- bayesnec:::survival_by_x(obs$x, obs$y)
  expect_equal(survival$x_r[!is.na(survival$x_r)], alive$x)
  expect_equal(survival$y_r[!is.na(survival$y_r)], alive$y)

  # The bands are those of each block on the stored grid.
  grid <- fit$pred_vals$data$x
  nd <- bnec_newdata(fit, resolution = length(grid), x_range = range(grid))
  band_median <- function(frame) frame$y_e[!is.na(frame$y_e)]
  expect_equal(band_median(growth),
               unname(apply(brms::posterior_epred(fit$fit, newdata = nd,
                                                  dpar = "mu",
                                                  re_formula = NA),
                            2, stats::median)))
  expect_equal(band_median(survival),
               unname(apply(1 - brms::posterior_epred(fit$fit, newdata = nd,
                                                      dpar = "hu",
                                                      re_formula = NA),
                            2, stats::median)))
  expect_equal(band_median(combined), fit$pred_vals$data$Estimate)

  # The response block is smooth, so its line is the NSEC of its curve; the
  # survival block's is its nec parameter, on the recorded scale.
  lines <- function(frame, column) frame[[column]][!is.na(frame[[column]])]
  expect_equal(lines(growth, "nec_vals"),
               as.numeric(q(nsec(fit, dpar = "mu", resolution = 50))))
  hunec <- exp(as.numeric(
    brms::as_draws_df(fit$fit, variable = "b_hunec_Intercept")$b_hunec_Intercept
  ))
  expect_equal(lines(survival, "nec_vals"),
               unname(c(stats::median(hunec),
                        stats::quantile(hunec, c(0.025, 0.975)))))

  # The ECx lines are those ecx() returns for each block.
  expect_equal(lines(growth, "ecx_vals"),
               as.numeric(q(ecx(fit, dpar = "mu"))))
  expect_equal(lines(survival, "ecx_vals"),
               as.numeric(q(ecx(fit, dpar = "hu"))))
  expect_equal(lines(combined, "ecx_vals"), as.numeric(q(ecx(fit))))
})

test_that("the block panels of a joint fit follow the fitted axis", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  fit <- joint_hurdle_cache()
  q <- function(expr) suppressWarnings(suppressMessages(expr))
  d <- q(ggbnec_data(fit, which = "all", add_ecx = TRUE,
                     x_scale = "fitted"))
  survival <- d[d$panel == "Probability of survival", , drop = FALSE]
  growth <- d[d$panel == "Response of survivors", , drop = FALSE]
  expect_equal(range(growth$x_e, na.rm = TRUE), log(c(0.1, 30)))
  expect_equal(sort(unique(survival$x_r[!is.na(survival$x_r)])),
               log(c(0.1, 0.3, 1, 3, 10, 30)))
  # Lines sit on the fitted scale, and their labels give recorded units.
  ne <- survival$nec_vals[!is.na(survival$nec_vals)]
  expect_equal(as.numeric(survival$nec_labs[!is.na(survival$nec_labs)]),
               round(exp(ne[1]), 2))
  ec <- growth$ecx_vals[!is.na(growth$ecx_vals)]
  # Quantiles interpolate between draws, so the two scales agree closely
  # rather than exactly.
  expect_equal(exp(ec), as.numeric(q(ecx(fit, dpar = "mu"))),
               tolerance = 0.01)
  expect_equal(as.numeric(growth$ecx_labs[!is.na(growth$ecx_labs)]),
               round(exp(ec[1]), 2))

  p <- q(autoplot(fit, which = "all", ecx = TRUE, x_scale = "fitted"))
  expect_equal(panel_titles(p), joint_titles)
  scale <- p$scales$get_scales("x")
  expect_equal(scale$breaks, attr(d, "x_axis")$breaks)
})

test_that("xlab and ylab label every class autoplot() draws", {
  skip_on_cran()
  q <- function(expr) suppressWarnings(suppressMessages(expr))
  labels_of <- function(p) c(p$labels$x, p$labels$y)
  defaults <- c("Predictor", "Response")
  supplied <- c("Dose", "Length")
  for (object in list(nec4param, manec_example, grouped_object(),
                      joint_with_rows())) {
    expect_equal(labels_of(q(autoplot(object))), defaults)
    expect_equal(labels_of(q(autoplot(object, xlab = "Dose",
                                      ylab = "Length"))), supplied)
  }
  expect_equal(labels_of(q(autoplot(manec_example, model = "nec4param",
                                    xlab = "Dose", ylab = "Length"))),
               supplied)
  pages <- q(autoplot(manec_example, model = "nec4param",
                      multi_facet = FALSE, plot = FALSE, xlab = "Dose",
                      ylab = "Length"))
  for (page in pages) expect_equal(labels_of(page), supplied)
})

test_that("plots refuse a curve they cannot draw", {
  skip_on_cran()
  q <- function(expr) suppressWarnings(suppressMessages(expr))
  # A one-block family has no block to select, whatever the class.
  for (object in list(nec4param, manec_example, grouped_object(),
                      joint_with_rows())) {
    expect_error(q(autoplot(object, which = "growth")), "one curve")
    expect_error(q(ggbnec_data(object, which = "all")), "one curve")
    expect_error(q(autoplot(object, dpar = "mu")), "one curve")
    # The default is accepted when named.
    expect_s3_class(q(autoplot(object, which = "combined")), "ggplot")
  }
  # The ECx line would otherwise read the block while the curve did not.
  expect_error(q(autoplot(nec4param, ecx = TRUE, dpar = "mu")), "one curve")
  expect_error(q(autoplot(nec4param, which = "nonsense")), "should be one of")

  # A model set of two-block fits names pull_out(); a grouped fit or joint
  # refit names the estimators.
  hurdle_part <- list(fit = list(family = brms::hurdle_gamma()))
  set <- structure(list(mod_fits = list(ecx4param = hurdle_part)),
                   class = c("bayesmanecfit", "bnecfit"))
  expect_error(bayesnec:::check_plot_single_curve("all", set),
               "pull_out(x, model = \"ecx4param\"), which = \"all\")",
               fixed = TRUE)
  expect_error(bayesnec:::check_plot_dpar(list(dpar = "hu"), set),
               "pull_out", fixed = TRUE)
  expect_null(bayesnec:::check_plot_single_curve("combined", set))
  expect_null(bayesnec:::check_plot_single_curve(NULL, set))
  joint <- structure(hurdle_part, class = c("bayesnecjointfit", "bnecfit"))
  expect_error(bayesnec:::check_plot_single_curve("growth", joint),
               "ecx() or nsec() and `dpar`", fixed = TRUE)
})

test_that("a smooth survival block draws its NSEC and its ECx", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  q <- function(expr) suppressWarnings(suppressMessages(expr))
  fit <- q(pull_out(joint_set_cache(), model = "nec3param"))
  p <- q(autoplot(fit, which = "all", ecx = TRUE))
  expect_equal(panel_titles(p), paste0(
    c("Response per individual exposed", "Response of survivors",
      "Probability of survival"),
    "\n", c("nec3param / ecxll3", "nec3param", "ecxll3")
  ))
  text <- Filter(function(layer) inherits(layer$geom, "GeomText"),
                 p$layers)[[1]]$data
  expect_equal(text$tag,
               c(rownames(q(summary(fit))$nec_vals)[1], "NEC", "NSEC"))

  d <- q(ggbnec_data(fit, which = "all", add_ecx = TRUE))
  growth <- d[d$panel == "Response of survivors", , drop = FALSE]
  survival <- d[d$panel == "Probability of survival", , drop = FALSE]
  lines <- function(frame, column) frame[[column]][!is.na(frame[[column]])]
  nec <- as.numeric(
    brms::as_draws_df(fit$fit, variable = "b_nec_Intercept")$b_nec_Intercept
  )
  expect_equal(lines(growth, "nec_vals"),
               unname(c(stats::median(nec),
                        stats::quantile(nec, c(0.025, 0.975)))))
  args <- bayesnec:::recorded_expand_args(list(fit))
  expect_equal(lines(survival, "nec_vals"),
               as.numeric(q(nsec(fit, dpar = "hu", sig_val = args$sig_val,
                                 resolution = args$resolution))))
  expect_equal(lines(growth, "ecx_vals"),
               as.numeric(q(ecx(fit, dpar = "mu"))))
  expect_equal(lines(survival, "ecx_vals"),
               as.numeric(q(ecx(fit, dpar = "hu"))))

  # A relative ECx is defined for a block, not for the combined curve, and
  # the refusal names the plot's argument rather than ecx()'s.
  expect_error(q(autoplot(fit, which = "all", ecx = TRUE,
                          type = "relative")),
               "`which = \"growth\"`", fixed = TRUE)
  expect_s3_class(q(autoplot(fit, which = "survival", ecx = TRUE,
                             type = "relative")), "ggplot")
})

test_that("a two-block model set titles each equation as pull_out() does", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  q <- function(expr) suppressWarnings(suppressMessages(expr))
  set <- joint_set_cache()
  models <- set$success_models
  pulled <- vapply(models, function(m) {
    panel_titles(q(autoplot(q(pull_out(set, model = m)))))
  }, character(1))
  expect_equal(unname(pulled), paste(models, "/ ecxll3"))
  by_name <- unname(pulled[sort(models)])
  p <- q(autoplot(set, model = models))
  expect_equal(panel_titles(p), c("Model averaged predictions", by_name))
  p <- q(autoplot(set, model = models, average = FALSE))
  expect_equal(panel_titles(p), by_name)
  pages <- q(autoplot(set, model = models, multi_facet = FALSE,
                      plot = FALSE))
  expect_equal(vapply(pages, panel_titles, character(1)),
               c("Model averaged predictions", unname(pulled)))
  expect_equal(p$labels$y, "Response per individual exposed")

  expect_error(q(autoplot(set, which = "all")), "pull_out", fixed = TRUE)
  expect_error(q(autoplot(set, dpar = "mu")), "pull_out", fixed = TRUE)
  expect_error(q(autoplot(set, ecx = TRUE, type = "relative")),
               "type = \"absolute\"", fixed = TRUE)
})

test_that("a joint two-block fit refuses dpar and names which", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  fit <- joint_hurdle_cache()
  expect_error(suppressMessages(autoplot(fit, dpar = "mu")),
               "`which = \"growth\"`", fixed = TRUE)
  expect_error(suppressMessages(ggbnec_data(fit, add_ecx = TRUE,
                                            dpar = "hu")),
               "not `dpar`", fixed = TRUE)
})
