library(bayesnec)

test_that("beta_binomial2 works", {
  out_lpmf <- beta_binomial2_lpmf(y = c(2, 4, 6), mu = c(0.25, 0.5, 0.75),
                                  phi = 1 / 3, trials = 12)
  out_rng <- beta_binomial2_rng(mu = c(0.25, 0.5, 0.75), phi = 1 / 3,
                                trials = 12)
  prep <- list(dpars = list(mu = matrix(rep(c(0.25, 0.5, 0.75), 3), nrow = 3),
               phi = 1 / 3), data = list(Y = c(2, 4, 6), trials = rep(12, 3)))
  out_log_lik <- log_lik_beta_binomial2(1, prep)
  out_pp <- posterior_predict_beta_binomial2(1, prep)
  out_ep <- posterior_epred_beta_binomial2(prep)
  expect_equal(class(beta_binomial2_lpmf), "function")
  expect_equal(class(out_lpmf), "numeric")
  expect_length(out_lpmf, 3)
  expect_length(out_rng, 3)
  expect_length(out_log_lik, 3)
  expect_length(out_pp, 3)
  expect_length(out_ep, 9)
})
