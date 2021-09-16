library(bayesnec)
data(manec_example)

test_that("dispersion fails because family is gaussian", {
  expect_length(dispersion(ecx4param), 0)
  expect_length(dispersion(ecx4param, summary = TRUE), 0)
})
