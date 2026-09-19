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

test_that("predictor_scale is an explicit bnec argument (#317)", {
  expect_true("predictor_scale" %in% names(formals(bnec)))
  expect_error(
    bnec(y ~ crf(x, "nec3param"), data = nec_data,
         predictor_scale = "unknown"),
    "arg.*one of"
  )
  expect_error(
    bnec(y ~ crf(log_x, c("nec3param", "nec4param")), data = nec_data,
         predictor_scale = "concentration"),
    "requires a non-negative predictor"
  )
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

test_that("an incomplete observed response is reported before fitting", {
  d <- data.frame(
    x = rep(c(0, 1, 2, 4), each = 3),
    y = rep(c(1, 0.95, 0.9, 0.8), each = 3)
  )
  bdat <- model.frame(
    bayesnecformula(y ~ crf(x, c("nec3param", "nec4param"))),
    data = d, run_par_checks = TRUE
  )
  family <- brms::Beta(link = "identity")
  expect_warning(check_response_range(bdat, family),
                 "observed decline is.*20%")

  d$y <- rep(c(1, 0.7, 0.3, 0.1), each = 3)
  bdat <- model.frame(
    bayesnecformula(y ~ crf(x, c("nec3param", "nec4param"))),
    data = d, run_par_checks = TRUE
  )
  expect_silent(check_response_range(bdat, family))
})

test_that("bnec reports an incomplete range while affected defaults remain", {
  d <- data.frame(
    x = rep(c(0, 1, 2, 4), each = 3),
    y = rep(c(1, 0.95, 0.9, 0.8), each = 3)
  )
  f <- y ~ crf(x, c("nec3param", "nec4param"))
  local_mocked_bindings(
    fit_bayesnec = function(...) stop("mock fit"),
    .package = "bayesnec"
  )
  warnings <- character(0)
  suppressMessages(
    withCallingHandlers(
      expect_error(
        bnec(f, d, family = Beta(link = "identity")),
        "None of the model"
      ),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  )
  expect_length(grep("may not identify the lower asymptote", warnings), 1)

  partial <- brms::prior_string("beta(5, 2)", nlpar = "top")
  expect_warning(
    suppressMessages(
      expect_error(
        bnec(f, d, family = Beta(link = "identity"), prior = partial),
        "None of the model"
      )
    ),
    "may not identify the lower asymptote"
  )

  nec_prior <- brms::prior_string("normal(1, 1)", nlpar = "nec")
  affected <- brms::prior_string("normal(0, 1)", nlpar = "bot") +
    nec_prior
  expect_warning(
    suppressMessages(
      expect_error(
        bnec(y ~ crf(x, "nec4param"), d,
             family = Beta(link = "identity"), prior = affected),
        "mock fit"
      )
    ),
    NA
  )

  complete <- list(nec3param = nec_prior, nec4param = affected)
  expect_warning(
    suppressMessages(
      expect_error(
        bnec(f, d, family = Beta(link = "identity"), prior = complete),
        "None of the model"
      )
    ),
    NA
  )
})

test_that("the response-range diagnostic is not inferred on an invalid scale", {
  d <- data.frame(
    x = rep(0:3, each = 2),
    y = rep(c(0, -0.1, -0.2, -0.3), each = 2)
  )
  bdat <- model.frame(
    bayesnecformula(y ~ crf(x, "nec4param")), data = d,
    run_par_checks = TRUE
  )
  expect_silent(check_response_range(bdat, gaussian(link = "identity")))
})

test_that("the response-range check uses binomial proportions and rates", {
  binomial_data <- data.frame(
    x = rep(c(0, 1), each = 2),
    successes = c(20, 20, 8, 8),
    trials = c(100, 100, 10, 10)
  )
  binomial_frame <- model.frame(
    bayesnecformula(successes | trials(trials) ~ crf(x, "nec3param")),
    data = binomial_data, run_par_checks = TRUE
  )
  expect_warning(
    check_response_range(binomial_frame, binomial(link = "identity")),
    "decline is -300%"
  )

  rate_data <- data.frame(
    x = rep(c(0, 1), each = 2),
    count = c(100, 100, 40, 40),
    exposure = c(100, 100, 50, 50)
  )
  rate_frame <- model.frame(
    bayesnecformula(count | rate(exposure) ~ crf(x, "nec3param")),
    data = rate_data, run_par_checks = TRUE
  )
  expect_warning(
    check_response_range(rate_frame, poisson(link = "identity")),
    "decline is.*20%"
  )
})

test_that("the response-range check assesses hurdle blocks separately", {
  d <- data.frame(
    x = rep(c(0, 1), each = 10),
    y = c(rep(1, 10), rep(1, 2), rep(0, 8))
  )
  bdat <- model.frame(
    bayesnecformula(y ~ crf(x, "nec3param")), d,
    run_par_checks = TRUE
  )
  expect_warning(
    check_response_range(
      bdat, brms::hurdle_gamma(link = "identity", link_hu = "identity")
    ),
    "response.*0%"
  )

  response_prior <- brms::prior_string("normal(1, 1)", nlpar = "nec")
  survival_prior <- brms::prior_string("normal(1, 1)", nlpar = "hunec")
  family <- brms::hurdle_gamma(link = "identity", link_hu = "identity")

  response_custom <- uses_response_range_defaults(
    response_prior, "nec3param", family
  )
  expect_identical(response_custom,
                   c(response = FALSE, survival = TRUE))
  expect_silent(
    check_response_range(bdat, family,
                         blocks = names(response_custom)[response_custom])
  )

  survival_custom <- uses_response_range_defaults(
    survival_prior, "nec3param", family
  )
  expect_identical(survival_custom,
                   c(response = TRUE, survival = FALSE))
  expect_warning(
    check_response_range(bdat, family,
                         blocks = names(survival_custom)[survival_custom]),
    "response.*0%"
  )
})

test_that("equations without affected defaults do not trigger the check", {
  sensitivity <- uses_response_range_defaults(
    NULL, c("ecxlin", "ecxexp", "ecxsigm"),
    gaussian(link = "identity")
  )
  expect_identical(sensitivity, c(response = FALSE))
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


test_that("bnec refuses a resolution below 2 before fitting anything", {
  # A property of the call, fixed before any model is fitted. Left to arrive
  # from expand_nec(), where the no-effect estimate of a smooth equation is read
  # off the grid, it arrives only after every model in the set has compiled and
  # sampled. See #325.
  expect_error(
    bnec(y ~ crf(x, model = "nec3param"), data = nec_data, resolution = 1),
    "must be at least 2"
  )
})
