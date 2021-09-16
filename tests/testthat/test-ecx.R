library(bayesnec)
library(dplyr)
library(brms)

data(manec_example)

test_that("prob_vals warnings behave as expected", {
  ecx(manec_example, prob_vals = c(0.6, 0.1, 0.9),
      type = "relative") %>%
    expect_length(3) %>%
    suppressWarnings
  ecx(manec_example, prob_vals = 0.9, type = "relative") %>%
    expect_error %>%
    suppressWarnings
  ecx(manec_example, prob_vals = c(0.6, 0.9, 0.1),
      type = "relative") %>%
    expect_error %>%
    suppressWarnings
  ecx(nec4param, prob_vals = c(0.6, 0.1, 0.9),
      type = "relative") %>%
    expect_length(3) %>%
    suppressWarnings
  expect_error(ecx(nec4param, prob_vals = 0.9, type = "relative"))
  expect_error(ecx(nec4param, prob_vals = c(0.6, 0.9, 0.1),
                   type = "relative"))
})

test_that("ecx_val warnings behave as expected", {
  expect_error(ecx(manec_example, ecx_val = 0.9, type = "relative")) %>%
    suppressWarnings
  expect_error(ecx(nec4param, ecx_val = 0.9, type = "relative"))
})

test_that("ecx returns expected object types and arguments pass correctly", {
  ec50_summary <- ecx(manec_example, ecx_val = 50, type = "relative",
                      precision = 50)
  ec50_summary2 <- ecx(manec_example, ecx_val = 50, type = "relative",
                       precision = 50, xform = exp)
  ec50_posterior <- ecx(manec_example, ecx_val = 50,
                        type = "relative", posterior = TRUE, precision = 50)
  ec50n_summary <- ecx(nec4param, ecx_val = 50, type = "relative",
                       precision = 50)
  ec50n_summary2 <- ecx(nec4param, ecx_val = 50, type = "relative",
                        precision = 50, xform = exp)
  ec50n_posterior <- ecx(nec4param, ecx_val = 50, type = "relative",
                         posterior = TRUE, precision = 50)
  expect_equal(length(ec50_summary), 3)
  expect_gt(length(ec50_posterior), 3)
  expect_equal(length(ec50n_summary), 3)
  expect_gt(length(ec50n_posterior), 3)
  expect_equal(attributes(ec50_summary)$precision, 50)
  expect_equal(attributes(ec50_posterior)$precision, 50)
  expect_equal(attributes(ec50n_summary)$precision, 50)
  expect_equal(attributes(ec50n_posterior)$precision, 50)
  expect_gt(ec50_summary2[1], ec50_summary[1])
  expect_gt(ec50n_summary2[1], ec50n_summary[1])
})

test_that("works for bayesnecfit", {
  ecx1 <- ecx(ecx4param)
  expect_equal(length(ecx1), 3)
  expect_equal(names(ecx1), c("ec_10_Q50", "ec_10_Q2.5", "ec_10_Q97.5"))
})

test_that("works for bayesmanecfit", {
  ecx1 <- ecx(manec_example)
  expect_equal(length(ecx1), 3)
  expect_equal(names(ecx1), c("ec_10", "ec_10_lw", "ec_10_up"))
})


test_that("xform passes correctly", {
  ecx1 <- ecx(ecx4param)
  ecx2 <- ecx(ecx4param, xform = exp)
  expect_gt(ecx2[1], ecx1[2])
})

test_that("posterior passes correctly", {
  ecx3 <- ecx(ecx4param, posterior = TRUE)
  expect_equal(length(ecx3), 100)
})

test_that("prob_vals passes correctly", {
  ecx4 <- ecx(ecx4param, prob_vals = c(0.3, 0.5, 0.7))
  expect_equal(names(ecx4), c("ec_10_Q30",   "ec_10_Q50",  "ec_10_Q70"))
})

test_that("ecx_val passes correctly", {
  ecx4 <- ecx(ecx4param, prob_vals = c(0.3, 0.5, 0.7), ecx_val = 20)
  expect_equal(names(ecx4), c("ec_20_Q30", "ec_20_Q50", "ec_20_Q70"))
})
