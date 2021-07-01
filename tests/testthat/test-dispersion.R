library(bayesnec)

data(manec_example)
nec4param <- pull_out(manec_example, model = "nec4param")

test_fam <- nec4param$fit
test_fam$family$family <- "other"

test_that("dispersion works", {
  expect_gt(length(bayesnec:::dispersion(nec4param$fit)), 3)
  expect_length(bayesnec:::dispersion(nec4param$fit, summary = TRUE), 3)
  expect_null(bayesnec:::dispersion(test_fam))
  expect_type(bayesnec:::dispersion(nec4param$fit), "double")
  expect_type(bayesnec:::dispersion(nec4param$fit, summary = TRUE), "double")
})
