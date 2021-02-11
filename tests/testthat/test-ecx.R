library(bayesnec)
library(brms)

ec50_summary <- ecx(manec_gausian_identity, ecx_val = 50, type = "relative", precision = 50)
ec50_summary2 <- ecx(manec_gausian_identity, ecx_val = 50, type = "relative", precision = 50, xform=exp)
ec50_posterior <- ecx(manec_gausian_identity, ecx_val = 50, type = "relative", posterior = TRUE, precision = 50)

ec50n_summary <- ecx(nec_gausian_identity, ecx_val = 50, type = "relative", precision = 50)
ec50n_summary2 <- ecx(nec_gausian_identity, ecx_val = 50, type = "relative", precision = 50, xform=exp)
ec50n_posterior <- ecx(nec_gausian_identity, ecx_val = 50, type = "relative", posterior = TRUE, precision = 50)

test_that("gaussian absolute ecx values are not returned", {
 expect_error(ecx(manec_gausian_identity))
  expect_error(ecx(nec_gausian_identity))
})

test_that("prob_vals warnings behave as expected", {
  expect_length(ecx(manec_gausian_identity, prob_vals = c(0.6, 0.1, 0.9), type = "relative"), 3)
  expect_error(ecx(manec_gausian_identity, prob_vals = 0.9, type = "relative"))
  expect_error(ecx(manec_gausian_identity, prob_vals = c(0.6, 0.9, 0.1), type = "relative"))
  expect_length(ecx(nec_gausian_identity, prob_vals = c(0.6, 0.1, 0.9), type = "relative"), 3)
  expect_error(ecx(nec_gausian_identity, prob_vals = 0.9, type = "relative"))
  expect_error(ecx(nec_gausian_identity, prob_vals = c(0.6, 0.9, 0.1), type = "relative"))
})

test_that("ecx_val warnings behave as expected", {
  expect_error(ecx(manec_gausian_identity, ecx_val = 0.9, type = "relative"))  
  expect_error(ecx(nec_gausian_identity, ecx_val = 0.9, type = "relative"))  
})

test_that("ecx returns expected object types", {
 expect_equal(length(ec50_summary), 3)
 expect_gt(length(ec50_posterior), 3)  
 expect_equal(length(ec50n_summary), 3)
 expect_gt(length(ec50n_posterior), 3) 
})

test_that("precision is passing correctly", {
  expect_equal(attributes(ec50_summary)$precision, 50)
  expect_equal(attributes(ec50_posterior)$precision, 50)
  expect_equal(attributes(ec50n_summary)$precision, 50)
  expect_equal(attributes(ec50n_posterior)$precision, 50)
})

test_that("xform operates corrected", {
  expect_equal(exp(ec50_summary)[1], ec50_summary2[1])
  expect_equal(exp(ec50n_summary)[1], ec50n_summary2[1]) 
  
})
