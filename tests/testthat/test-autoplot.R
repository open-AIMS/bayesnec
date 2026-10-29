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
