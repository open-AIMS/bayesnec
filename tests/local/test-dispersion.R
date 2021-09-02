library(bayesnec)

nec4param <- pull_out(manec_binomial_identity, model = "nec4param") %>%
  suppressMessages %>%
  suppressWarnings

test_that("dispersion works", {
  expect_length(dispersion(nec4param), nrow(as.matrix(nec4param$fit)))
  expect_length(dispersion(nec4param, summary = TRUE), 3)
  expect_type(dispersion(nec4param), "double")
  expect_type(dispersion(nec4param, summary = TRUE), "double")
})
