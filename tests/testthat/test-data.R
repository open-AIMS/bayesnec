library(bayesnec)
data("nec_data")

test_that("nec_data are as expected", {
  expect_equal(class(nec_data), "data.frame")
})

test_that("beta_binomial2 family behaves as expected", {
  expect_equal(class(beta_binomial2), c("customfamily", "brmsfamily", "family"))
  expect_equal(beta_binomial2$link, "logit")
  expect_equal(beta_binomial2$link_phi, "log")
  expect_equal(beta_binomial2$dpars, c("mu", "phi"))
})
