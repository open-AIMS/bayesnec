library(bayesnec)
library(brms)

data(manec_example)

test_that("prob_vals warnings behave as expected", {
  expect_length(nsec(manec_example, prob_vals = c(0.6, 0.1, 0.9)), 3) %>%
    suppressWarnings
  expect_error(nsec(manec_example, prob_vals = 0.9)) %>%
    suppressWarnings
  expect_error(nsec(manec_example, prob_vals = c(0.6, 0.9, 0.1))) %>%
    suppressWarnings
  expect_length(nsec(nec4param, prob_vals = c(0.6, 0.1, 0.9)), 3)
  expect_error(nsec(nec4param, prob_vals = 0.9))
  expect_error(nsec(nec4param, prob_vals = c(0.6, 0.9, 0.1)))
})

test_that("ecx_val warnings behave as expected", {
  expect_error(nsec(manec_example, ecx_val = 0.9))
  expect_error(nsec(nec4param, ecx_val = 0.9))
})

test_that(paste0("nsec returns expected object types and precision is",
                 " passing correctly"), {
  nsec_summary <- nsec(manec_example, sig_val = 0.01, precision = 50) %>%
    suppressWarnings
  nsec_summary2 <- nsec(manec_example, sig_val = 0.01, precision = 50,
                        xform = exp) %>%
    suppressWarnings
  nsec_posterior <- nsec(manec_example, sig_val = 0.01,
                         posterior = TRUE, precision = 50) %>%
    suppressWarnings
  nsecn_summary <- nsec(nec4param, sig_val = 0.01, precision = 50) %>%
    suppressWarnings
  nsecn_summary2 <- nsec(nec4param, sig_val = 0.01, precision = 50,
                         xform = exp) %>%
    suppressWarnings
  nsecn_posterior <- nsec(nec4param, sig_val = 0.01,
                          posterior = TRUE, precision = 50) %>%
    suppressWarnings
  expect_equal(length(nsec_summary), 3)
  expect_gt(length(nsec_posterior), 3)
  expect_equal(length(nsecn_summary), 3)
  expect_gt(length(nsecn_posterior), 3)
  expect_equal(attributes(nsec_summary)$precision, 50)
  expect_equal(attributes(nsec_posterior)$precision, 50)
  expect_equal(attributes(nsecn_summary)$precision, 50)
  expect_equal(attributes(nsecn_posterior)$precision, 50)
})

test_that("works for bayesnecfit", {
  nsec1 <- nsec(ecx4param)
  expect_equal(length(nsec1), 3)
  expect_equal(names(nsec1), c("ec_0.01_Q50", "ec_0.01_Q2.5", "ec_0.01_Q97.5"))
})

test_that("works for bayesmanecfit", {
  nsec1 <- nsec(manec_example) %>%
    suppressWarnings
  expect_equal(length(nsec1), 3)
  expect_equal(names(nsec1), c("ec_0.01", "ec_0.01_lw", "ec_0.01_up"))
})

test_that("xform passes correctly", {
  nsec1 <- nsec(ecx4param)
  nsec2 <- nsec(ecx4param, xform = exp)
  expect_gt(nsec2[1], nsec1[2])
})

test_that("posterior passes correctly", {
  nsec3 <- nsec(ecx4param, posterior = TRUE)
  expect_equal(length(nsec3), 100)
})

test_that("prob_vals passes correctly", {
  nsec4 <- nsec(ecx4param, prob_vals = c(0.3, 0.5, 0.7))
  expect_equal(names(nsec4), c("ec_0.01_Q30", "ec_0.01_Q50", "ec_0.01_Q70"))
})

test_that("sig_val passes correctly", {
  nsec4 <- nsec(ecx4param, prob_vals = c(0.3, 0.5, 0.7), sig_val = 0.05)
  expect_equal(names(nsec4), c("ec_0.05_Q30", "ec_0.05_Q50", "ec_0.05_Q70"))
})
