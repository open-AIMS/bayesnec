# Regression tests for ecnsec on models whose prediction newdata has more than
# one column (binomial / beta-binomial models carry a `trials` column). The
# previous implementation built the nsec prediction newdata by hand and called
# `colnames(newdat_nsec) <- x_name`, which errored with
# "'names' attribute [2] must be the same length as the vector [1]" whenever
# newdata had >1 column. The fix routes this through newdata_eval(). These
# fixtures (manec_binomial_*, manec_betabinomial) are defined in setup.R.

test_that("ecnsec works for a binomial (trials) bayesmanecfit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ns <- nsec(manec_binomial_identity, resolution = 10) |>
    suppressWarnings()
  res <- ecnsec(manec_binomial_identity, nsec = unname(ns["Q50"]),
                resolution = 10) |>
    suppressWarnings()
  expect_length(res, 3)
  expect_equal(names(res), c("50%", "2.5%", "97.5%"))
  expect_true(is.numeric(res))
})

test_that("ecnsec works for a logit-link binomial bayesmanecfit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ns <- nsec(manec_binomial_logit, resolution = 10) |>
    suppressWarnings()
  res <- ecnsec(manec_binomial_logit, nsec = unname(ns["Q50"]),
                resolution = 10) |>
    suppressWarnings()
  expect_length(res, 3)
  expect_equal(names(res), c("50%", "2.5%", "97.5%"))
})

test_that("ecnsec works for a beta_binomial bayesmanecfit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ns <- nsec(manec_betabinomial, resolution = 10) |>
    suppressWarnings()
  res <- ecnsec(manec_betabinomial, nsec = unname(ns["Q50"]),
                resolution = 10) |>
    suppressWarnings()
  expect_length(res, 3)
  expect_equal(names(res), c("50%", "2.5%", "97.5%"))
})

test_that("ecnsec posterior = TRUE returns the full posterior for a trials model", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ns <- nsec(manec_binomial_identity, resolution = 10) |>
    suppressWarnings()
  res <- ecnsec(manec_binomial_identity, nsec = unname(ns["Q50"]),
                posterior = TRUE, resolution = 10) |>
    suppressWarnings()
  expect_true(is.numeric(res))
  expect_gt(length(res), 3)
})
