library(bayesnec)
library(brms)

nsec_summary <- nsec(manec_gausian_identity, sig_val = 0.01, precision = 50)
nsec_summary2 <- nsec(manec_gausian_identity, sig_val = 0.01, precision = 50, xform=exp)
nsec_posterior <- nsec(manec_gausian_identity, sig_val = 0.01, posterior = TRUE, precision = 50)

nsecn_summary <- nsec(nec_gausian_identity, sig_val = 0.01, precision = 50)
nsecn_summary2 <- nsec(nec_gausian_identity, sig_val = 0.01, precision = 50, xform=exp)
nsecn_posterior <- nsec(nec_gausian_identity, sig_val = 0.01, posterior = TRUE, precision = 50)

test_that("prob_vals warnings behave as expected", {
  expect_length(nsec(manec_gausian_identity, prob_vals = c(0.6, 0.1, 0.9)), 3)
  expect_error(nsec(manec_gausian_identity, prob_vals = 0.9))
  expect_error(nsec(manec_gausian_identity, prob_vals = c(0.6, 0.9, 0.1)))
  expect_length(nsec(nec_gausian_identity, prob_vals = c(0.6, 0.1, 0.9)), 3)
  expect_error(nsec(nec_gausian_identity, prob_vals = 0.9))
  expect_error(nsec(nec_gausian_identity, prob_vals = c(0.6, 0.9, 0.1)))
})

test_that("ecx_val warnings behave as expected", {
  expect_error(nsec(manec_gausian_identity, ecx_val = 0.9))  
  expect_error(nsec(nec_gausian_identity, ecx_val = 0.9))  
})

test_that("nsec returns expected object types", {
  expect_equal(length(nsec_summary), 3)
  expect_gt(length(nsec_posterior), 3)  
  expect_equal(length(nsecn_summary), 3)
  expect_gt(length(nsecn_posterior), 3) 
})

test_that("precision is passing correctly", {
  expect_equal(attributes(nsec_summary)$precision, 50)
  expect_equal(attributes(nsec_posterior)$precision, 50)
  expect_equal(attributes(nsecn_summary)$precision, 50)
  expect_equal(attributes(nsecn_posterior)$precision, 50)
})

