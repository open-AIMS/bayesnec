library(bayesnec)

nec_gausian_identity <- pull_out(manec_gausian_identity, "nec4param")
fit1 <- manec_gausian_identity$mod_fits$nec4param$fit
fit2 <- manec_gausian_identity$mod_fits$ecx4param$fit
ec10 <- ecx(manec_gausian_identity, type = "relative")
ec90 <- ecx(manec_gausian_identity, ecx_val = 90, type = "relative")

test_that("gaussian model with identity works correctly", {
  expect_s3_class(manec_gausian_identity, "bayesmanecfit")
  expect_s3_class(nec_gausian_identity, "bayesnecfit")
  expect_equal(summary(manec_gausian_identity)$family, "gaussian")  
  expect_range(manec_gausian_identity$mod_stats[1,"wi"], 0.7, 0.95)
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

nec_beta_identity <- pull_out(manec_beta_identity, "nec4param")
test_that("beta model with identity works correctly", {
  expect_s3_class(manec_beta_identity, "bayesmanecfit")
  expect_s3_class(nec_beta_identity, "bayesnecfit")
  expect_equal(summary(manec_beta_identity)$family, "beta")  
  expect_range(manec_beta_identity$mod_stats[1,"wi"], 0.7, 0.99)
  expect_range(manec_beta_identity$w_nec[1,"Estimate"], 1.4, 1.6)
  expect_range(ecx(manec_beta_identity)[1], 1.38, 1.64)
  expect_range(ecx(manec_beta_identity, ecx_val = 90, type = "relative")[1], 2.5, 2.97)
  
})
