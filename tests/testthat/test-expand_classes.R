library(bayesnec)

data(manec_example)
fit1 <- manec_example$mod_fits[["nec4param"]]
fit2 <- manec_example$mod_fits[["ecx4param"]]

test_that("expand_nec defaults work for nec model", {
  nec_fit <- expand_nec(fit1, fit1$bayesnecformula, model = "nec4param") %>%
    suppressWarnings
  expect_equal(names(nec_fit), c("fit", "model", "init", "bayesnecformula",
                                 "pred_vals", "top",
                                 "beta", "nec", "f", "bot", "d",
                                 "slope", "ec50", "dispersion", "predicted_y",
                                 "residuals", "nec_posterior"))
  expect_equal(class(nec_fit$fit), "brmsfit")
  expect_equal(nec_fit$model, "nec4param")
  expect_equal(dim(nec_fit$pred_vals$posterior), c(100, 1000))
  expect_equal(dim(nec_fit$pred_vals$data), c(1000, 4))
  expect_equal(range(nec_fit$pred_vals$data$x), c(0.03234801, 3.22051966))
})

test_that("expand_nec arguments work for nec model", {
  nec_fit <- expand_nec(fit1, fit1$bayesnecformula, model = "nec4param",
                        x_range = c(0.01, 4), precision = 20) %>%
    suppressWarnings
  expect_equal(names(nec_fit), c("fit", "model", "init", "bayesnecformula",
                                 "pred_vals", "top",
                                 "beta", "nec", "f", "bot", "d",
                                 "slope", "ec50", "dispersion", "predicted_y",
                                 "residuals", "nec_posterior"))
  expect_equal(class(nec_fit$fit), "brmsfit")
  expect_equal(nec_fit$model, "nec4param")
  expect_equal(dim(nec_fit$pred_vals$posterior), c(100, 20))
  expect_equal(dim(nec_fit$pred_vals$data), c(20, 4))
  expect_equal(range(nec_fit$pred_vals$data$x), c(0.01, 4))
})

test_that("expand_ecx defaults work for ecx model", {
  ecx_fit <- expand_nec(fit2, fit2$bayesnecformula, model = "ecx4param") %>%
    suppressWarnings
  expect_equal(names(ecx_fit), c("fit", "model", "init", "bayesnecformula",
                                 "pred_vals", "top",
                                 "beta", "nec", "f", "bot", "d",
                                 "slope", "ec50", "dispersion", "predicted_y",
                                 "residuals", "nec_posterior"))
  expect_equal(class(ecx_fit$fit), "brmsfit")
  expect_equal(ecx_fit$model, "ecx4param")
  expect_equal(dim(ecx_fit$pred_vals$posterior), c(100, 1000))
  expect_equal(dim(ecx_fit$pred_vals$data), c(1000, 4))
  expect_equal(range(ecx_fit$pred_vals$data$x), c(0.03234801, 3.22051966))
})

test_that("expand_ecx arguments work for ecx model", {
  ecx_fit <- expand_nec(fit2, fit2$bayesnecformula, model = "ecx4param",
                        x_range = c(0.01, 4), precision = 20) %>%
    suppressWarnings
  expect_equal(names(ecx_fit), c("fit", "model", "init", "bayesnecformula",
                                 "pred_vals", "top",
                                 "beta", "nec", "f", "bot", "d", "slope",
                                 "ec50", "dispersion", "predicted_y",
                                 "residuals", "nec_posterior"))
  expect_equal(class(ecx_fit$fit), "brmsfit")
  expect_equal(ecx_fit$model, "ecx4param")
  expect_equal(dim(ecx_fit$pred_vals$posterior), c(100, 20))
  expect_equal(dim(ecx_fit$pred_vals$data), c(20, 4))
  expect_equal(range(ecx_fit$pred_vals$data$x), c(0.01, 4))
})

test_that("expand_ecx sig_val argument work for ecx model", {
  ecx_fit_a <- expand_nec(fit2, fit2$bayesnecformula, model = "ecx4param") %>%
    suppressWarnings
  ecx_fit_b <- expand_nec(fit2, fit2$bayesnecformula, model = "ecx4param",
                          sig_val = 0.2) %>%
    suppressWarnings
  expect_gt(ecx_fit_a$nec["Estimate"], ecx_fit_b$nec["Estimate"])
})

tt1 <- manec_example$mod_fits
formulas <- lapply(tt1, `[[`, "bayesnecformula")
test_null <- NULL
tt2 <- tt1["nec4param"]

test_that("expand_manec warnings work correctly", {
  expect_error(expand_manec(test_null))
  expect_message(expand_manec(tt2, formulas[["nec4param"]]),
                 "Only nec4param is fitted, no model averaging done.")
  expect_message(expand_manec(tt1, formulas),
                 "Fitted models are: nec4param ecx4param") %>%
    suppressWarnings
})

test_that("expand_manec defaults work correctly", {
  tt3 <- expand_manec(tt1, formulas) %>%
    suppressMessages %>%
    suppressWarnings
  expect_equal(dim(tt3$w_pred_vals$posterior), c(100, 1000))
  expect_equal(dim(tt3$w_pred_vals$data), c(1000, 4))
  expect_equal(range(tt3$w_pred_vals$data$x), c(0.03234801, 3.22051966))
})

test_that("expand_manec defaults work correctly", {
  tt4 <- expand_manec(tt1, formulas, x_range = c(0.01, 4), precision = 20) %>%
    suppressMessages %>%
    suppressWarnings
  expect_equal(dim(tt4$w_pred_vals$posterior), c(100, 20))
  expect_equal(dim(tt4$w_pred_vals$data), c(20, 4))
  expect_equal(range(tt4$w_pred_vals$data$x), c(0.01, 4))
})

test_that("new loo_controls are incorporated", {
  get_new_method <- function(x) {
    attributes(x$mod_stats$wi)$method
  }
  expand_manec(tt1, formulas) %>%
    get_new_method %>%
    expect_null %>%
    expect_message %>%
    suppressWarnings
  my_ctrls <- list(weights = list(method = "pseudobma"))
  expand_manec(tt1, formulas, loo_controls = my_ctrls) %>%
    get_new_method %>%
    expect_equal("pseudobma") %>%
    expect_message %>%
    suppressWarnings
  my_ctrls <- list(weights = list(method = "stacking"))
  expand_manec(tt1, formulas, loo_controls = my_ctrls) %>%
    get_new_method %>%
    expect_equal("stacking") %>%
    expect_message %>%
    suppressWarnings
})
