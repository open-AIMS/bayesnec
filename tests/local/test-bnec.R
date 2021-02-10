library(bayesnec)

nec_gausian_identity <- pull_out(manec_gausian_identity, "nec4param")

test_that("gaussian model with identity works correctly", {
  expect_s3_class(manec_gausian_identity, "bayesmanecfit")
  expect_s3_class(nec_gausian_identity, "bayesnecfit")
})
