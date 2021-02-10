library(bayesnec)

nec_gausian_identity <- pull_out(manec_gausian_identity, "nec4param")
fit1 <- manec_gausian_identity$mod_fits$nec4param$fit
fit2 <- manec_gausian_identity$mod_fits$ecx4param$fit

test_that("gaussian model with identity works correctly", {
  expect_s3_class(manec_gausian_identity, "bayesmanecfit")
  expect_s3_class(nec_gausian_identity, "bayesnecfit")
  expect_equal(summary(manec_gausian_identity)$family, "gaussian")  
  expect_range(manec_gausian_identity$mod_stats[1,"wi"], 0.7, 0.9)
  
  expect_range(fixef(fit1)["nec_Intercept", 1], 1.3, 1.6)
  expect_range(fixef(fit1)["beta_Intercept", 1], -0.8, -0.6)
  expect_range(fixef(fit1)["top_Intercept", 1], 2.1, 2.2)
  expect_range(fixef(fit1)["bot_Intercept", 1], -9.0, -8.8)
  expect_range(waic(fit1)$estimates[3, 1], 167, 180)
  
  expect_range(fixef(fit2)["ec50_Intercept", 1], 2.3, 2.5)
  expect_range(fixef(fit2)["beta_Intercept", 1], 0.8, 1.0)
  expect_range(fixef(fit2)["top_Intercept", 1], 2.3, 2.4)
  expect_range(fixef(fit2)["bot_Intercept", 1], -6.6, -5.0)
  expect_range(waic(fit2)$estimates[3, 1], 175, 195)  
  
})
