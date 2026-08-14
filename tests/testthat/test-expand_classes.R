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
  expect_equal(dim(tt3$w_pred_vals$posterior), c(100, 1000))
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
  expect_equal(dim(tt4$w_pred_vals$posterior), c(100, 20))
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

test_that("the model-averaged draws are unchanged by dropping the cache", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # expand_manec() used to read each model's posterior back off the object,
  # where expand_nec() had stored it; it now builds the same matrices itself and
  # discards them. The weighted draws must be the same in distribution, and are
  # drawn in the same proportions. Compared against the priors-free arithmetic
  # rather than against a stored constant, so this stays meaningful if the
  # example object is ever refitted.
  set.seed(180)
  tt5 <- expand_manec(tt1, formulas) |>
    suppressMessages() |>
    suppressWarnings()
  n <- tt5$sample_size
  expected_rows <- sum(round(n * tt5$mod_stats$wi))
  expect_equal(nrow(tt5$w_pred_vals$posterior), expected_rows)
  expect_equal(ncol(tt5$w_pred_vals$posterior), 1000)
  # Every draw came from one of the component posteriors, so the weighted set
  # spans the same range as the models it was drawn from.
  each <- lapply(tt5$mod_fits, function(z) {
    bayesnec:::posterior_on_grid(z$fit, z$bayesnecformula, resolution = 1000)
  })
  expect_gte(min(tt5$w_pred_vals$posterior),
             min(vapply(each, min, numeric(1))))
  expect_lte(max(tt5$w_pred_vals$posterior),
             max(vapply(each, max, numeric(1))))
  # The summary the plot methods use is still built from those draws.
  expect_equal(tt5$w_pred_vals$data$Estimate,
               unname(apply(tt5$w_pred_vals$posterior, 2,
                            bayesnec:::estimates_summary)["Estimate", ]))
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
})
