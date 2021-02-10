library(bayesnec)

necfit <- pull_out(bayesnec:::model_fits$manec_gausian_identity, "nec4param")

test_that("returns null", {
  skip_on_cran()
  skip_on_ci()
  expect_equal(check_chains.default(necfit), NULL)
})

