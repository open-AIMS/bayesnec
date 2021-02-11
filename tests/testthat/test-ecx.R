library(bayesnec)
library(brms)

test_that("gaussian absolute ecx values are not returned", {
 expect_error(ecx(manec_gausian_identity))
})

test_that("prob_vals warnings behave as expected", {
  expect_length(ecx(manec_gausian_identity, prob_vals = c(0.6, 0.1, 0.9), type = "relative"), 3)
  expect_error(ecx(manec_gausian_identity, prob_vals = 0.9, type = "relative"))
  expect_error(ecx(manec_gausian_identity, prob_vals = c(0.6, 0.9, 0.1), type = "relative"))
})