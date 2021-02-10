library(bayesnec)

test_that("returns null", {
  expect_equal(check_chains.default(nec_gausian_identity), NULL)
})
