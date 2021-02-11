library(bayesnec)

nec_gausian_identity <- pull_out(manec_gausian_identity, "nec4param")
fit1 <- manec_gausian_identity$mod_fits$nec4param$fit
fit2 <- manec_gausian_identity$mod_fits$ecx4param$fit
ec10 <- ecx(manec_gausian_identity, type = "relative")
ec90 <- ecx(manec_gausian_identity, ecx_val = 90, type = "relative")

test_that("gaussian model with identity works correctly", {
  expect_equal(summary(manec_gausian_identity)$family, "gaussian")  
  expect_equal(fit1$family$link, "identity")  
  expect_range(ec10[1], 1.38, 1.64)
  expect_range(ec90[1], 2.93, 2.97)
  
  expect_range(fixef(fit1)["bot_Intercept", 1], -12, -5)
  expect_range(fixef(fit1)["top_Intercept", 1], 2.1, 2.2)
  expect_range(fixef(fit1)["beta_Intercept", 1], -0.9, -0.1)
  expect_range(fixef(fit1)["nec_Intercept", 1], 1.3, 1.6)
  expect_warning(expect_range(waic(fit1)$estimates[3, 1], 167, 180))
  
  expect_range(fixef(fit2)["bot_Intercept", 1], -6.6, -5.0)  
  expect_range(fixef(fit2)["ec50_Intercept", 1], 2.3, 2.5)
  expect_range(fixef(fit2)["top_Intercept", 1], 2.1, 2.4)
  expect_range(fixef(fit2)["beta_Intercept", 1], 0.8, 1.0)
  expect_warning(expect_range(waic(fit2)$estimates[3, 1], 175, 215))  
})

test_that("all model families return expected ecx values", {
 lapply(manec_fits, FUN = function(x) expect_range(ecx(x, type = "relative")[1], 1.3, 1.64))
 lapply(manec_fits[-1], FUN = function(x) expect_range(ecx(x, ecx_val = 90, type = "relative")[1], 1.9, 2.97)) 
})

test_that("all model families return expected nec values", {
  lapply(manec_fits, FUN = function(x) expect_range(x$w_nec["Estimate"], 1.4, 1.6)) 
})

test_that("all model families return expected weights", {
 lapply(manec_fits, FUN = function(x) expect_range(x$mod_stats[1,"wi"], 0.7, 1)) 
})

test_that("all model families return expected class", {
  lapply(nec_fits, FUN = function(x) expect_s3_class(x, "bayesnecfit"))
  lapply(manec_fits, FUN = function(x) expect_s3_class(x, "bayesmanecfit"))
})

test_that("beta model returns expected family and link", {
  expect_equal(summary(manec_beta_identity)$family, "beta")
  expect_equal(summary(manec_beta_logit)$family, "beta")  
  expect_equal(manec_beta_identity$mod_fits$nec4param$fit$family$link, "identity")  
  expect_equal(manec_beta_logit$mod_fits$nec4param$fit$family$link, "logit")  
})

test_that("binomial model returns expected family and link", {
  expect_equal(summary(manec_binomial_identity)$family, "binomial")
  expect_equal(summary(manec_binomial_logit)$family, "binomial")  
  expect_equal(manec_binomial_identity$mod_fits$nec4param$fit$family$link, "identity")  
  expect_equal(manec_binomial_logit$mod_fits$nec4param$fit$family$link, "logit")  
})
  
test_that("poisson model returns expected family and link", {
  expect_equal(summary(manec_poisson_identity)$family, "poisson")
  expect_equal(summary(manec_poisson_log)$family, "poisson")  
  expect_equal(manec_poisson_identity$mod_fits$nec4param$fit$family$link, "identity")  
  expect_equal(manec_poisson_log$mod_fits$nec4param$fit$family$link, "log")  
})

test_that("negbinomial model returns expected family and link", {
  expect_equal(summary(manec_negbinomial_identity)$family, "negbinomial")
  expect_equal(summary(manec_negbinomial_log)$family, "negbinomial")  
  expect_equal(manec_negbinomial_identity$mod_fits$nec4param$fit$family$link, "identity")  
  expect_equal(manec_negbinomial_log$mod_fits$nec4param$fit$family$link, "log")  
})

test_that("gamma model returns expected family and link", {
  expect_equal(summary(manec_gamma_identity)$family, "gamma")
  expect_equal(summary(manec_gamma_log)$family, "gamma")  
  expect_equal(manec_gamma_identity$mod_fits$nec4param$fit$family$link, "identity")  
  expect_equal(manec_gamma_log$mod_fits$nec4param$fit$family$link, "log")  
})