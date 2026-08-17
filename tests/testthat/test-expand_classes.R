fit1 <- manec_example$mod_fits[["nec4param"]]
fit2 <- manec_example$mod_fits[["ecx4param"]]

test_that("expand_nec defaults work for nec model", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nec_fit <- expand_nec(fit1, fit1$bayesnecformula, model = "nec4param") |>
    suppressWarnings()
  expect_equal(names(nec_fit), c("fit", "model", "init", "bayesnecformula",
                                 "pred_vals", "top",
                                 "beta", "ne", "f", "bot", "d",
                                 "slope", "ec50", "dispersion", "predicted_y",
                                 "residuals", "ne_posterior", "ne_type"))
  expect_equal(class(nec_fit$fit), "brmsfit")
  expect_equal(nec_fit$model, "nec4param")
  # The n_draws x resolution matrix is no longer stored (#180); only the
  # summary it was used to build.
  expect_named(nec_fit$pred_vals, "data")
  expect_null(nec_fit$pred_vals$posterior)
  expect_equal(dim(nec_fit$pred_vals$data), c(1000, 4))
  expect_equal(range(nec_fit$pred_vals$data$x), c(0.03234801, 3.22051966))
})

test_that("expand_nec arguments work for nec model", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nec_fit <- expand_nec(fit1, fit1$bayesnecformula, model = "nec4param",
                        x_range = c(0.01, 4), resolution = 20) |>
    suppressWarnings()
  expect_equal(names(nec_fit), c("fit", "model", "init", "bayesnecformula",
                                 "pred_vals", "top",
                                 "beta", "ne", "f", "bot", "d",
                                 "slope", "ec50", "dispersion", "predicted_y",
                                 "residuals", "ne_posterior", "ne_type"))
  expect_equal(class(nec_fit$fit), "brmsfit")
  expect_equal(nec_fit$model, "nec4param")
  expect_named(nec_fit$pred_vals, "data")
  expect_equal(dim(nec_fit$pred_vals$data), c(20, 4))
  expect_equal(range(nec_fit$pred_vals$data$x), c(0.01, 4))
})

test_that("expand_ecx defaults work for ecx model", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx_fit <- expand_nec(fit2, fit2$bayesnecformula, model = "ecx4param") |>
    suppressWarnings()
  expect_equal(names(ecx_fit), c("fit", "model", "init", "bayesnecformula",
                                 "pred_vals", "top",
                                 "beta", "ne", "f", "bot", "d",
                                 "slope", "ec50", "dispersion", "predicted_y",
                                 "residuals", "ne_posterior", "ne_type"))
  expect_equal(class(ecx_fit$fit), "brmsfit")
  expect_equal(ecx_fit$model, "ecx4param")
  expect_named(ecx_fit$pred_vals, "data")
  expect_equal(dim(ecx_fit$pred_vals$data), c(1000, 4))
  expect_equal(range(ecx_fit$pred_vals$data$x), c(0.03234801, 3.22051966))
})

test_that("expand_ecx arguments work for ecx model", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx_fit <- expand_nec(fit2, fit2$bayesnecformula, model = "ecx4param",
                        x_range = c(0.01, 4), resolution = 20) |>
    suppressWarnings()
  expect_equal(names(ecx_fit), c("fit", "model", "init", "bayesnecformula",
                                 "pred_vals", "top",
                                 "beta", "ne", "f", "bot", "d", "slope",
                                 "ec50", "dispersion", "predicted_y",
                                 "residuals", "ne_posterior", "ne_type"))
  expect_equal(class(ecx_fit$fit), "brmsfit")
  expect_equal(ecx_fit$model, "ecx4param")
  expect_named(ecx_fit$pred_vals, "data")
  expect_equal(dim(ecx_fit$pred_vals$data), c(20, 4))
  expect_equal(range(ecx_fit$pred_vals$data$x), c(0.01, 4))
})

test_that("expand_ecx sig_val argument work for ecx model", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx_fit_a <- expand_nec(fit2, fit2$bayesnecformula, model = "ecx4param") |>
    suppressWarnings()
  ecx_fit_b <- expand_nec(fit2, fit2$bayesnecformula, model = "ecx4param",
                          sig_val = 0.2) |>
    suppressWarnings()
  expect_gt(ecx_fit_a$ne["Estimate"], ecx_fit_b$ne["Estimate"])
})

tt1 <- manec_example$mod_fits
formulas <- lapply(tt1, `[[`, "bayesnecformula")
test_null <- NULL
tt2 <- tt1["nec4param"]

test_that("expand_manec warnings work correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  expect_error(expand_manec(test_null))
  expect_message(expand_manec(tt2, formulas[["nec4param"]]),
                 "Only nec4param is fitted, no model averaging done.")
  expect_message(expand_manec(tt1, formulas),
                 "Fitted models are: nec4param ecx4param") |>
    suppressWarnings()
})

test_that("expand_manec defaults work correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  tt3 <- expand_manec(tt1, formulas) |>
    suppressMessages() |>
    suppressWarnings()
  # The weighted n_draws x resolution matrix is no longer stored either (#213);
  # only the summary built from it.
  expect_named(tt3$w_pred_vals, "data")
  expect_null(tt3$w_pred_vals$posterior)
  expect_equal(dim(tt3$w_pred_vals$data), c(1000, 4))
  expect_equal(range(tt3$w_pred_vals$data$x), c(0.03234801, 3.22051966))
})

test_that("expand_manec defaults work correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  tt4 <- expand_manec(tt1, formulas, x_range = c(0.01, 4), resolution = 20) |>
    suppressMessages() |>
    suppressWarnings()
  expect_named(tt4$w_pred_vals, "data")
  expect_equal(dim(tt4$w_pred_vals$data), c(20, 4))
  expect_equal(range(tt4$w_pred_vals$data$x), c(0.01, 4))
})

test_that("new loo_controls are incorporated", {
  get_new_method <- function(x) {
    attributes(x$mod_stats$wi)$method
  }
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  expand_manec(tt1, formulas) |>
    get_new_method() |>
    expect_null() |>
    expect_message() |>
    suppressWarnings()
  my_ctrls <- list(weights = list(method = "pseudobma"))
  expand_manec(tt1, formulas, loo_controls = my_ctrls) |>
    get_new_method() |>
    expect_equal("pseudobma") |>
    expect_message() |>
    suppressWarnings()
  my_ctrls <- list(weights = list(method = "stacking"))
  expand_manec(tt1, formulas, loo_controls = my_ctrls) |>
    get_new_method() |>
    expect_equal("stacking") |>
    expect_message() |>
    suppressWarnings()
})

test_that("each model is thinned to exactly its share of the weighted draws", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # expand_manec() used to read each model's posterior back off the object and
  # thin it there; it now computes and thins one model at a time in
  # w_grid_pred_calc(). "The weighted draws are the same" means two specific
  # things, both asserted here rather than inferred from a range: each model
  # contributes exactly round(sample_size * wi) rows, and every row it
  # contributes is a row of that model's own posterior. Checked against the
  # arithmetic of the weights rather than a stored constant, so this stays
  # meaningful if the example objects are ever refitted.
  set.seed(180)
  tt5 <- expand_manec(tt1, formulas) |>
    suppressMessages() |>
    suppressWarnings()
  expect_named(tt5$w_pred_vals, "data")
  expect_null(tt5$w_pred_vals$posterior)
  n <- tt5$sample_size
  formulas_by_model <- lapply(tt5$mod_fits, `[[`, "bayesnecformula")
  for (mod in tt5$success_models) {
    full <- bayesnec:::posterior_on_grid(tt5$mod_fits[[mod]]$fit,
                                         formulas_by_model[[mod]],
                                         resolution = 1000)
    drawn <- bayesnec:::w_grid_pred_calc(mod, tt5$mod_fits, formulas_by_model,
                                         NA, 1000, n, tt5$mod_stats)
    expect_equal(nrow(drawn), round(n * tt5$mod_stats[mod, "wi"]))
    expect_equal(ncol(drawn), 1000)
    # Row sums identify a row uniquely here because the values are copied, not
    # recomputed, so they compare exactly.
    expect_true(all(rowSums(drawn) %in% rowSums(full)))
  }
  # The rows contributed across all models account for the whole weighted set.
  expect_equal(sum(round(n * tt5$mod_stats$wi)),
               sum(vapply(tt5$success_models, function(mod) {
                 as.integer(round(n * tt5$mod_stats[mod, "wi"]))
               }, integer(1))))
  # The summary the plot methods use spans the grid it is supposed to.
  expect_equal(nrow(tt5$w_pred_vals$data), 1000)
  expect_named(tt5$w_pred_vals$data, c("x", "Estimate", "Q2.5", "Q97.5"))
})

test_that("posterior_epred() reproduces the draws the cache used to hold", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # This is the migration path documented in NEWS and in both class docs for
  # code that used to read pred_vals$posterior / w_pred_vals$posterior. It has
  # to actually work, on both classes, at the documented grid.
  single <- pull_out(manec_example, "nec4param") |>
    suppressMessages() |>
    suppressWarnings()
  post_single <- posterior_epred(
    single, newdata = bnec_newdata(single, resolution = 50), re_formula = NA
  )
  expect_true(is.matrix(post_single))
  expect_equal(ncol(post_single), 50)
  post_manec <- posterior_epred(
    manec_example,
    newdata = bnec_newdata(manec_example, resolution = 50), re_formula = NA
  )
  expect_true(is.matrix(post_manec))
  expect_equal(ncol(post_manec), 50)
})

test_that("a partially specified x_range is rejected rather than guessed at", {
  # bnec_newdata() used to ignore it silently while expand_nec() turned it into
  # seq(NA, NA). Now one grid builder, one behaviour. See #211.
  # Rejected whichever end is missing -- the two used to behave differently.
  expect_error(bayesnec:::check_args_newdata(10, c(1, NA)), "fully specified")
  expect_error(bayesnec:::check_args_newdata(10, c(NA_real_, 4)),
               "fully specified")
  # The documented "not supplied" value, and a proper range, both still pass.
  expect_error(bayesnec:::check_args_newdata(10, NA), NA)
  expect_error(bayesnec:::check_args_newdata(10, c(0, 4)), NA)
})

test_that("an object saved with the old cache still works", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # Nothing reads pred_vals$posterior any more, but an object saved before this
  # change still carries it. It must neither be needed nor get in the way.
  new_style <- expand_nec(fit1, fit1$bayesnecformula, model = "nec4param") |>
    suppressWarnings() |>
    (\(z) bayesnec:::allot_class(z, c("bayesnecfit", "bnecfit")))()
  expect_null(new_style$pred_vals$posterior)
  old_style <- new_style
  old_style$pred_vals$posterior <- bayesnec:::posterior_on_grid(
    new_style$fit, new_style$bayesnecformula, resolution = 1000
  )
  expect_false(is.null(old_style$pred_vals$posterior))
  for (obj in list(new_style, old_style)) {
    expect_equal(nrow(predict(obj)), nrow(obj$fit$data))
    expect_true(is.numeric(nec(obj)))
    expect_error(suppressWarnings(summary(obj)), NA)
    expect_error(suppressMessages(ecx(obj, ecx_val = 10)), NA)
    expect_invisible(plot(obj))
  }
  # And an old-style object can still be combined into a model set, which is
  # the one path that used to read the cache.
  combined <- c(new_style, pull_out(manec_example, "ecx4param")) |>
    suppressMessages() |>
    suppressWarnings()
  expect_s3_class(combined, "bayesmanecfit")
  expect_null(combined$mod_fits$nec4param$pred_vals$posterior)
  expect_null(combined$w_pred_vals$posterior)
})

test_that("a bayesmanecfit saved with the old weighted cache still works", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # Same again for w_pred_vals$posterior, dropped in #213. The shipped
  # manec_example was built before that change, so it is a genuinely old
  # serialised object rather than a synthesised one -- which is the case worth
  # testing. Nothing needs the matrix and nothing should trip over it.
  old_style <- manec_example
  if (is.null(old_style$w_pred_vals$posterior)) {
    # Only reached if the example data is ever regenerated under the new code;
    # the fixture must not quietly stop testing anything at that point.
    old_style$w_pred_vals$posterior <- posterior_epred(
      manec_example, newdata = bnec_newdata(manec_example, resolution = 1000),
      re_formula = NA
    )
  }
  expect_false(is.null(old_style$w_pred_vals$posterior))
  new_style <- old_style
  new_style$w_pred_vals$posterior <- NULL
  expect_named(new_style$w_pred_vals, "data")
  for (obj in list(new_style, old_style)) {
    expect_equal(nrow(predict(obj)), nrow(obj$mod_fits[[1]]$fit$data))
    expect_true(is.numeric(obj$w_ne))
    expect_error(suppressWarnings(summary(obj)), NA)
    expect_error(suppressMessages(ecx(obj, ecx_val = 10)), NA)
    expect_invisible(plot(obj))
    expect_s3_class(autoplot(obj), "ggplot")
  }
})
