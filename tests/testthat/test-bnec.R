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

test_that("a model set states the missing-value refusal once, from bnec()", {
  # check_data() runs once per model inside fit_bayesnec(), and bnec() wraps
  # that call in try() for a model set, so a refusal raised from there was
  # printed once per model and the call then ended on the generic
  # all-models-failed advice, which names neither the missing values nor the
  # remedy. The default model argument is a set, so that is the common path.
  # The check is raised in bnec() instead, immediately after the model frame is
  # built. See #278.
  d <- nec_data
  d$y[3] <- NA
  msg <- tryCatch(bnec(y ~ crf(x, model = c("nec3param", "nec4param")),
                       data = d, family = Beta(link = "identity")),
                  error = conditionMessage)
  expect_match(msg, "1 row\\(s\\) with missing values")
  expect_false(grepl("None of the models fit successfully", msg))
})

test_that("the refusal precedes the family choice, so nothing is read off a subset", {
  # Placed immediately after the model frame is built rather than beside
  # check_normalisation(), so that nothing downstream, retrieve_valid_family()
  # included, is decided from a smaller sample than was supplied. Asserted by
  # the absence of the family-selection message, which is the only observable
  # thing that runs between the two positions on this input. See #278.
  d <- nec_data
  d$y[3] <- NA
  msgs <- capture.output(
    msg <- tryCatch(bnec(y ~ crf(x, model = "nec3param"), data = d),
                    error = conditionMessage),
    type = "message"
  )
  expect_match(msg, "1 row\\(s\\) with missing values")
  expect_length(msgs, 0)
})

test_that("Check models inappropriate for negative x are dropped", {
  # The family is given explicitly because nec_data's response is 0-1 bounded,
  # for which nechorme4pwr is now excluded up front (#177) -- the negative-x
  # rule would never be reached. Gamma leaves it in the set so that this test
  # still exercises the rule it is about.
  bnec(y ~ crf(log_x, "nechorme4pwr"), data = nec_data,
       family = Gamma(link = "identity")) |>
    expect_message("Dropping the model\\(s\\) nechorme4pwr as they are not valid for data with negative predictor \\(x\\) values\\.") |>
    expect_error("No valid models have been supplied for this data type.")
})
