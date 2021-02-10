library(bayesnec)

test_that("returns null", {
  expect_null(check_chains.default(nec_gausian_identity))
})
