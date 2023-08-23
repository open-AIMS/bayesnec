library(bayesnec)
library(brms)

r_pois <- rpois(1000, lambda = 10)
r_norm <- rnorm(1000)
r_gamm <- rgamma(1000, 2)
r_beta <- rbeta(1000, 1, 2)
r_binm <- rbinom(1000, 10, 0.5)

test_that("expect correct distribution", {
  expect_identical(set_distribution(r_pois, support_integer = TRUE), "poisson")
  expect_identical(set_distribution(r_norm), "gaussian")
  expect_error(expect_identical(set_distribution(r_gamm), "gamma"))
  expect_identical(set_distribution(r_gamm), "Gamma")
  expect_identical(set_distribution(r_beta), "Beta")
  expect_identical(set_distribution(r_binm, support_integer = TRUE, 10),
                   "binomial")
  expect_error(set_distribution(r_norm, trials = TRUE))
  expect_identical(set_distribution(r_pois, support_integer = TRUE, 10),
                   "binomial")
})

r_pois_b <- add_na(r_pois, n = 10)
r_norm_b <- add_na(r_norm, n = 10)
test_that("does not support NA", {
  expect_error(set_distribution(r_pois_b, support_integer = TRUE))
  expect_error(set_distribution(r_norm_b))
})
