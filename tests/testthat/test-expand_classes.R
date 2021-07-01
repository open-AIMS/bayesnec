library(bayesnec)

data(manec_example)
ecx4param <- pull_out(manec_example, model = "ecx4param")
nec4param <- pull_out(manec_example, model = "nec4param")

fit1 <- nec4param$fit
fit2 <- ecx4param$fit

fit1$loo <- suppressWarnings(loo(fit1))
fit2$loo <- suppressWarnings(loo(fit2))

fit1$waic <- suppressWarnings(waic(fit1, pointwise = FALSE))
fit2$waic <- suppressWarnings(waic(fit2, pointwise = FALSE))

fit1 <- list(fit = fit1, model = "nec4param", inits = NA)
fit2 <- list(fit = fit2, model = "ecx4param", inits = NA)
fit1 <- allot_class(fit1, "prebayesnecfit")
fit2 <- allot_class(fit2, "prebayesnecfit")

test_that("expand_nec defaults work for nec model", {
  nec_fit <- expand_nec(fit1)
  expect_equal(names(nec_fit), c("fit", "model", "inits", "pred_vals", "top",
                                 "beta", "nec", "alpha", "bot", "d",
                                 "slope", "ec50", "dispersion", "predicted_y",
                                 "residuals", "nec_posterior"))
  expect_equal(class(nec_fit$fit), "brmsfit")
  expect_equal(nec_fit$model, "nec4param")
  expect_equal(dim(nec_fit$pred_vals$posterior), c(10, 1000))
  expect_equal(dim(nec_fit$pred_vals$data), c(1000, 4))
  expect_equal(range(nec_fit$pred_vals$data$x), c(0.03234801, 3.22051966))
})

test_that("expand_nec arguments work for nec model", {
  nec_fit <- expand_nec(fit1, x_range = c(0.01, 4), precision = 20)
  expect_equal(names(nec_fit), c("fit", "model", "inits", "pred_vals", "top",
                                 "beta", "nec", "alpha", "bot", "d",
                                 "slope", "ec50", "dispersion", "predicted_y",
                                 "residuals", "nec_posterior"))
  expect_equal(class(nec_fit$fit), "brmsfit")
  expect_equal(nec_fit$model, "nec4param")
  expect_equal(dim(nec_fit$pred_vals$posterior), c(10, 20))
  expect_equal(dim(nec_fit$pred_vals$data), c(20, 4))
  expect_equal(range(nec_fit$pred_vals$data$x), c(0.01, 4))
})

test_that("expand_ecx defaults work for ecx model", {
  ecx_fit <- expand_nec(fit2)
  expect_equal(names(ecx_fit), c("fit", "model", "inits", "pred_vals", "top",
                                 "beta", "nec", "alpha", "bot", "d",
                                 "slope", "ec50", "dispersion", "predicted_y",
                                 "residuals", "nec_posterior"))
  expect_equal(class(ecx_fit$fit), "brmsfit")
  expect_equal(ecx_fit$model, "ecx4param")
  expect_equal(dim(ecx_fit$pred_vals$posterior), c(10, 1000))
  expect_equal(dim(ecx_fit$pred_vals$data), c(1000, 4))
  expect_equal(range(ecx_fit$pred_vals$data$x), c(0.03234801, 3.22051966))
})

test_that("expand_ecx arguments work for ecx model", {
  ecx_fit <- expand_nec(fit2, x_range = c(0.01, 4), precision = 20)
  expect_equal(names(ecx_fit), c("fit", "model", "inits", "pred_vals", "top",
                                 "beta", "nec", "alpha", "bot", "d", "slope",
                                 "ec50", "dispersion", "predicted_y",
                                 "residuals", "nec_posterior"))
  expect_equal(class(ecx_fit$fit), "brmsfit")
  expect_equal(ecx_fit$model, "ecx4param")
  expect_equal(dim(ecx_fit$pred_vals$posterior), c(10, 20))
  expect_equal(dim(ecx_fit$pred_vals$data), c(20, 4))
  expect_equal(range(ecx_fit$pred_vals$data$x), c(0.01, 4))
})

test_that("expand_ecx sig_val argument work for ecx model", {
  ecx_fit_a <- expand_nec(fit2)
  ecx_fit_b <- expand_nec(fit2, sig_val = 0.2)
  expect_gt(ecx_fit_a$nec["Estimate"], ecx_fit_b$nec["Estimate"])
})

tt1 <- manec_example$mod_fits
test_null <- NULL
tt2 <- tt1["nec4param"]

test_that("expand_manec warnings work correctly", {
  expect_error(expand_manec(test_null))
  expect_message(expand_manec(tt2),
                 "Only nec4param is fitted, no model averaging done.")
  expect_message(expand_manec(tt1), "Fitted models are:  nec4param ecx4param")
})

test_that("expand_manec defaults work correctly", {
  tt3 <- expand_manec(tt1)
  expect_equal(dim(tt3$w_pred_vals$posterior), c(10, 1000))
  expect_equal(dim(tt3$w_pred_vals$data), c(1000, 4))
  expect_equal(range(tt3$w_pred_vals$data$x), c(0.03234801, 3.22051966))
})

test_that("expand_manec defaults work correctly", {
  tt4 <- expand_manec(tt1, x_range = c(0.01, 4), precision = 20)
  expect_equal(dim(tt4$w_pred_vals$posterior), c(10, 20))
  expect_equal(dim(tt4$w_pred_vals$data), c(20, 4))
  expect_equal(range(tt4$w_pred_vals$data$x), c(0.01, 4))
})
