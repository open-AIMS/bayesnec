library(bayesnec)

data(manec_example)
nec4param <- pull_out(manec_example, model = "nec4param") %>%
  suppressMessages %>%
  suppressWarnings

test_that("dispersion fails because family is gaussian", {
  expect_length(dispersion(nec4param$fit), 0)
  expect_length(dispersion(nec4param$fit, summary = TRUE), 0)
})

test_fam <- nec4param$fit
test_fam$family <- binomial()
test_fam$data$trials <- nrow(test_fam$data)
test_fam$data$y <- rbinom(nrow(test_fam$data), test_fam$data$trials, 0.5)

test_that("dispersion works in fake binomial", {
  expect_length(dispersion(test_fam), 100)
  expect_length(dispersion(test_fam, summary = TRUE), 3)
  expect_type(dispersion(test_fam), "double")
  expect_type(dispersion(test_fam, summary = TRUE), "double")
})
