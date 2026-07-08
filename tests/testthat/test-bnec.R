test_that("Check for data when using formula syntax", {
  expect_error(bnec(y ~ crf(x, "ecxlin")), "argument \"data\" is missing")
})

test_that("user-supplied `prior` is not captured by partial matching to `prior_type`", {
  # Regression test: `prior_type` was added as a formal that `prior` (a common
  # brms argument) partial-matches, so `prior = <brmsprior>` was silently bound
  # to `prior_type`, tripping its match.arg() before any data checks. With an
  # explicit `prior` formal, `prior =` is matched exactly and reaches the normal
  # missing-data error instead of the spurious match.arg() error.
  mp <- brms::prior_string("beta(5, 1)", nlpar = "top")
  expect_true("prior" %in% names(formals(bnec)))
  expect_error(bnec(y ~ crf(x, "nec3param"), prior = mp),
               "argument \"data\" is missing")
})

test_that("Check models inappropriate for negative x are dropped", {
 bnec(y ~ crf(log_x, "nechorme4pwr"), data = nec_data) |>
    expect_message("Dropping the model\\(s\\) nechorme4pwr as they are not valid for data with negative predictor \\(x\\) values\\.") |>
    expect_error("No valid models have been supplied for this data type.")
})
