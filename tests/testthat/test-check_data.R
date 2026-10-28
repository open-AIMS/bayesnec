# Test the guards, corrections, reports and caller write-back directly.
# Build model frames through the public construction path so unreachable
# branches cannot pass on unrealistic hand-built inputs.

# Build the same model frame that callers supply to check_data().
cd_bdat <- function(formula, data) {
  model.frame(bayesnecformula(formula), data = data, run_par_checks = TRUE)
}

cd_run <- function(formula, data, family, model = "nec3param") {
  check_data(cd_bdat(formula, data), family, model)
}

# The missing-value refusal, as a regex, so the count and the rows can be
# asserted around it without repeating the fixed part.
cd_missing_msg <- "row\\(s\\) with missing values \\(NA or NaN\\), at row\\(s\\)"

# Capture correction messages so both their presence and absence are tested.
cd_messages <- function(formula, data, family, model = "nec3param") {
  capture.output(invisible(cd_run(formula, data, family, model)),
                 type = "message")
}

# A declining Gamma-shaped response whose top concentration is all zeros.
cd_data_zero <- function() {
  data.frame(x = rep(c(0.1, 1, 10, 100), each = 5),
             y = c(rep(c(8, 6, 3), each = 5), rep(0, 5)))
}

# A proportion response touching both boundaries of the Beta support.
cd_data_bounded <- function() {
  data.frame(x = rep(c(0.1, 1, 10, 100), each = 5),
             y = c(rep(1, 5), rep(0.6, 5), rep(0.3, 5), rep(0, 5)))
}


# ---- guards -----------------------------------------------------------------

test_that("a non-finite predictor is refused, naming the predictor", {
  d <- cd_data_zero()
  d$x[1] <- Inf
  expect_error(cd_run(y ~ crf(x, model = "nec3param"), d, gaussian()),
               "predictor column contains values that are not finite")
})

test_that("a non-finite response is refused, naming the response", {
  d <- cd_data_zero()
  d$y[1] <- Inf
  expect_error(cd_run(y ~ crf(x, model = "nec3param"), d, gaussian()),
               "response column contains values that are not finite")
})

test_that("an NA or NaN row is refused, and the row is named", {
  # model.frame() removes incomplete cases. Read na.action to refuse and
  # name those rows instead of silently fitting the reduced data.
  d <- cd_data_zero()
  d$y[1] <- NaN
  expect_error(cd_run(y ~ crf(x, model = "nec3param"), d, gaussian()),
               paste("1", cd_missing_msg, "1"))
  # NA in the predictor is refused on the same route, and more than one row is
  # counted and named rather than only the first.
  d2 <- cd_data_zero()
  d2$x[c(2, 7)] <- NA
  expect_error(cd_run(y ~ crf(x, model = "nec3param"), d2, gaussian()),
               paste("2", cd_missing_msg, "2, 7"))
  # The remedy is named, since the user has to act on it: dropping the rows is
  # no longer done for them.
  msg <- tryCatch(cd_run(y ~ crf(x, model = "nec3param"), d, gaussian()),
                  error = conditionMessage)
  expect_match(msg, "remove or impute those rows")
  # And no function is named, because three of them reach this check: bnec()
  # and bnec_group() before they fit anything, and get_priors() per model.
  expect_false(grepl("The function bnec", msg))
})

test_that("the rows are reported by name, not by position", {
  # Report na.action names, which identify original rows. Its values index
  # the current subset and can misidentify rows of grouped input.
  d <- cd_data_zero()
  rownames(d) <- paste0("s", seq_len(nrow(d)) + 100L)
  d$y[3] <- NA
  expect_error(cd_run(y ~ crf(x, model = "nec3param"), d, gaussian()),
               paste("1", cd_missing_msg, "s103"))
})

test_that("an NA reaching the guard directly is named by column", {
  # With na.pass there is no na.action record. The elementwise finiteness
  # check must still identify missing predictor or response values.
  old <- options(na.action = "na.pass")
  on.exit(options(old), add = TRUE)
  d <- cd_data_zero()
  d$x[1] <- NA
  expect_error(cd_run(y ~ crf(x, model = "nec3param"), d, gaussian()),
               "predictor column contains values that are not finite")
  d2 <- cd_data_zero()
  d2$y[1] <- NaN
  expect_error(cd_run(y ~ crf(x, model = "nec3param"), d2, gaussian()),
               "response column contains values that are not finite")
})

test_that("a response that increases with the predictor warns", {
  d <- cd_data_zero()
  d$y <- rev(d$y)
  expect_warning(cd_run(y ~ crf(x, model = "nec3param"), d, gaussian()),
                 "only allows for response values to decline")
})

test_that("a hormesis model is exempt from the decline warning", {
  d <- cd_data_zero()
  d$y <- rev(d$y)
  expect_no_warning(cd_run(y ~ crf(x, model = "nechorme"), d, gaussian(),
                           model = "nechorme"))
})


# Earlier checks already refuse these inputs; retain their messages rather
# than adding unreachable duplicate guards to check_data().

test_that("a character predictor errors from retrieve_var", {
  d <- cd_data_zero()
  d$x <- as.character(d$x)
  msg <- tryCatch(cd_run(y ~ crf(x, model = "nec3param"), d, gaussian()),
                  error = conditionMessage)
  expect_match(msg, "is not numeric")
})

test_that("a numeric group-level column errors from model.frame", {
  d <- cd_data_zero()
  d$grp <- rep(1:2, 10)
  msg <- tryCatch(cd_run(y ~ crf(x, model = "nec3param") + ogl(grp), d,
                         gaussian()),
                  error = conditionMessage)
  expect_match(msg, "Group-level variables cannot be numeric")
})


test_that("check_data does not call check_custom_name", {
  # Make the unnecessary check_custom_name() call fail: ignoring its return
  # value would otherwise hide the call from output comparisons.
  d <- cd_data_zero()
  local_mocked_bindings(
    check_custom_name = function(...) stop("check_custom_name was called"),
    .package = "bayesnec"
  )
  expect_error(cd_run(y ~ crf(x, model = "nec3param"), d, gaussian()),
               NA)
})


# Check response arithmetic separately from the entry-point reports.

test_that("a Gamma zero is shifted by one tenth of the smallest non-zero value", {
  res <- suppressMessages(cd_run(y ~ crf(x, model = "nec3param"),
                                 cd_data_zero(), Gamma(link = "identity")))
  # min(y[y > 0]) is 3, so the zeros become 0.3 and nothing else changes.
  expect_identical(sort(unique(res$mod_dat$y)), c(0.3, 3, 6, 8))
})

test_that("the Gamma zero shift is reported, and names the remedy", {
  # Keep check_data() silent; entry points report substitutions once per call.
  expect_length(cd_messages(y ~ crf(x, model = "nec3param"), cd_data_zero(),
                            Gamma(link = "identity")), 0)
  rec <- cd_run(y ~ crf(x, model = "nec3param"), cd_data_zero(),
                Gamma(link = "identity"))$substitutions
  msg <- paste(capture.output(report_substitutions(rec), type = "message"),
               collapse = " ")
  expect_match(msg, "shifted to 0\\.3")
  expect_match(msg, "hurdle_gamma")
})

test_that("a beta zero is shifted by the same rule", {
  res <- cd_run(y ~ crf(x, model = "nec3param"), cd_data_bounded(),
                Beta(link = "identity"))
  # min(y[y > 0]) is 0.3, so the zeros become 0.03.
  expect_true(0.03 %in% round(res$mod_dat$y, 10))
  expect_false(any(res$mod_dat$y == 0))
})

test_that("a beta one is reduced by exactly 0.001", {
  res <- cd_run(y ~ crf(x, model = "nec3param"), cd_data_bounded(),
                Beta(link = "identity"))
  expect_true(0.999 %in% round(res$mod_dat$y, 10))
  expect_false(any(res$mod_dat$y == 1))
})

test_that("all three corrections are reported, none from check_data (#93)", {
  # Both Beta corrections are reported by entry points, not by check_data(),
  # which runs once per model.
  expect_length(cd_messages(y ~ crf(x, model = "nec3param"),
                            cd_data_bounded(), Beta(link = "identity")), 0)
  rec <- cd_run(y ~ crf(x, model = "nec3param"), cd_data_bounded(),
                Beta(link = "identity"))$substitutions
  expect_equal(nrow(rec), 2)
  msgs <- capture.output(report_substitutions(rec), type = "message")
  expect_length(msgs, 2)
  expect_match(paste(msgs, collapse = " "), "at 0,")
  expect_match(paste(msgs, collapse = " "), "at 1,")
})

test_that("zero_inflated_beta keeps its zeros and loses its ones", {
  res <- cd_run(y ~ crf(x, model = "nec3param"), cd_data_bounded(),
                brms::zero_inflated_beta(link = "identity"))
  # The zeros are the signal the zi block identifies itself from; the ones are
  # still outside Beta's open support and must go.
  expect_true(any(res$mod_dat$y == 0))
  expect_false(any(res$mod_dat$y == 1))
})

test_that("a family with no excluded boundary leaves the response alone", {
  d <- cd_data_zero()
  res <- cd_run(y ~ crf(x, model = "nec3param"), d, gaussian())
  expect_identical(res$mod_dat$y, d$y)
  expect_length(cd_messages(y ~ crf(x, model = "nec3param"), d, gaussian()), 0)
})

test_that("the predictor reaches the fit exactly as recorded", {
  # A zero control is valid: a response family does not constrain predictor
  # support.
  d <- cd_data_zero()
  d$x <- rep(c(0, 1, 10, 100), each = 5)
  res <- suppressMessages(cd_run(y ~ crf(x, model = "nec3param"), d,
                                 Gamma(link = "identity")))
  expect_identical(res$mod_dat$x, d$x)
})

test_that("a 0-1 bounded predictor keeps both of its bounds", {
  # Keep bounded predictor values unchanged; response-family boundary
  # corrections do not apply to the predictor.
  d <- data.frame(x = rep(c(0, 0.25, 0.5, 1), each = 5),
                  y = rep(c(8, 6, 3, 1), each = 5))
  res <- cd_run(y ~ crf(x, model = "nec3param"), d, Gamma(link = "identity"))
  expect_identical(range(res$mod_dat$x), c(0, 1))
})


# ---- the shape of what is returned ------------------------------------------

test_that("check_data returns mod_dat, the family and the substitutions", {
  res <- cd_run(y ~ crf(x, model = "nec3param"), cd_data_zero(), gaussian())
  # Return substitutions so callers can report them once and store the record.
  expect_named(res, c("mod_dat", "family", "substitutions"))
  expect_named(res$mod_dat, c("x", "y", "trials"))
  expect_s3_class(res$family, "family")
})

test_that("trials come from the trials variable for a binomial family", {
  d <- data.frame(x = rep(c(0.1, 1, 10, 100), each = 5),
                  suc = as.integer(rep(c(9, 7, 4, 1), each = 5)),
                  tot = as.integer(rep(10, 20)))
  res <- cd_run(suc | trials(tot) ~ crf(x, model = "nec3param"), d,
                binomial(link = "identity"))
  expect_identical(res$mod_dat$trials, d$tot)
})

test_that("a rate() term adds the denominator under its own name", {
  d <- data.frame(x = rep(c(0.1, 1, 10, 100), each = 5),
                  y = as.integer(rep(c(20, 15, 8, 2), each = 5)),
                  n = rep(2, 20))
  res <- cd_run(y | rate(n) ~ crf(x, model = "nec3param"), d,
                poisson(link = "identity"))
  # Named denom rather than rate: it is the denominator of the rate, not the
  # rate. See R/check_data.R.
  expect_true("denom" %in% names(res$mod_dat))
  expect_identical(unique(res$mod_dat$denom), 2)
})

test_that("is_censored returns a scalar FALSE that recycles", {
  # The contract the boundary corrections depend on: with no cens() term the
  # result must recycle harmlessly against a response of any length.
  expect_identical(is_censored(NULL), FALSE)
  expect_length(is_censored(NULL) & rep(TRUE, 3), 3)
  expect_identical(is_censored(c(0, -1, 1, NA)), c(FALSE, TRUE, TRUE, FALSE))
})


# Check that callers use the corrected frame returned by check_data().

test_that("get_priors builds its priors from the corrected response", {
  # Compare priors with a manually shifted response; finiteness alone cannot
  # distinguish priors built from corrected and uncorrected data.
  d <- cd_data_zero()
  d_shifted <- d
  # min(y[y > 0]) is 3, so check_data() shifts the zeros to 0.3.
  d_shifted$y[d_shifted$y == 0] <- 0.3
  pr <- suppressMessages(
    get_priors(y ~ crf(x, model = "nec3param"), data = d,
               family = Gamma(link = "identity"))
  )
  pr_shifted <- get_priors(y ~ crf(x, model = "nec3param"), data = d_shifted,
                           family = Gamma(link = "identity"))
  expect_s3_class(pr, "brmsprior")
  # Compare the prior column. Parameter names are unchanged by the shifted
  # values, so comparing those names would not detect a bypassed correction.
  expect_identical(pr$prior, pr_shifted$prior)
})

test_that("the update route writes back the correction it reports (#274)", {
  # check_update_data() must return the corrected frame as well as its family
  # check, so update() fits the response described by its correction message.
  skip_on_cran()
  f <- nec4param
  d <- f$fit$data
  d$y <- abs(d$y)
  d$y[1:3] <- 0
  res <- NULL
  msgs <- capture.output(
    res <- check_update_data(list(f), d, Gamma(link = "identity")),
    type = "message"
  )
  expect_true(any(grepl("shifted", msgs)))
  expect_type(res, "list")
  expect_named(res, c("changed_family", "data", "substitutions"))
  expect_type(res$changed_family, "logical")
  expect_s3_class(res$substitutions, "data.frame")
  # The zeros the message says were shifted are shifted in the frame returned.
  expect_equal(sum(d$y == 0), 3)
  expect_equal(sum(res$data$y == 0), 0)
  expect_true(all(res$data$y > 0))
  # Every other row is untouched.
  expect_equal(res$data$y[-(1:3)], d$y[-(1:3)])
})

test_that("the update route reports the substitution for a model set too", {
  # Keep the first model's substitution record: later iterations see the
  # corrected data and return an empty record.
  skip_on_cran()
  f <- nec4param
  d <- f$fit$data
  d$y <- abs(d$y)
  d$y[1:3] <- 0
  res <- NULL
  msgs <- capture.output(
    res <- check_update_data(list(f, f), d, Gamma(link = "identity")),
    type = "message"
  )
  expect_true(any(grepl("shifted", msgs)))
  expect_s3_class(res$substitutions, "data.frame")
  expect_equal(res$substitutions$n_rows, 3)
  # Reported once for the set, not once per member.
  expect_equal(sum(grepl("have been shifted", msgs)), 1)
  # And the correction still reaches the frame, from whichever iteration made
  # it.
  expect_equal(sum(res$data$y == 0), 0)
})


# Finiteness of a dispersion sub-model.

test_that("a disp() sub-model that is not finite is refused, naming the term", {
  # Check disp() expressions separately: their variables are intentionally
  # absent from the concentration-response model frame.
  d <- data.frame(x = rep(c(0, 1, 10, 100), each = 5),
                  y = rep(c(8, 6, 3, 1), each = 5))
  expect_error(
    check_disp_finite(bnf(y ~ crf(x, model = "nec3param") + disp(~log(x))), d),
    "log\\(x\\)"
  )
  # Distinguish missing values from infinities. The ordinary complete-case
  # check cannot inspect columns excluded from its model frame.
  d_na <- d
  d_na$w <- c(NA, rep(1, nrow(d_na) - 1))
  expect_error(
    check_disp_finite(bnf(y ~ crf(x, model = "nec3param") + disp(~w)), d_na),
    "missing values"
  )
  expect_error(
    check_disp_finite(bnf(y ~ crf(x, model = "nec3param") + disp(~w)), d_na),
    "\"w\""
  )
  # Raise malformed disp() terms here, before the per-model fitting loop
  # can repeat the same refusal for every equation.
  expect_error(
    check_disp_finite(
      bnf(y ~ crf(x, model = "nec3param") + disp(~x) + disp(~log(x))), d
    ),
    "more than one"
  )
  expect_error(
    check_disp_finite(bnf(y ~ crf(x, model = "nec3param") + disp(~log(x))), d),
    "not finite"
  )
  # Not confined to zeros: any expression that is not finite on the recorded
  # data reaches Stan the same way.
  expect_error(
    check_disp_finite(
      bnf(y ~ crf(x, model = "nec3param") + disp(~I(1 / (x - 1)))), d
    ),
    "not finite"
  )
})

test_that("a finite disp() sub-model, a variance function and no disp pass", {
  d <- data.frame(x = rep(c(0, 1, 10, 100), each = 5),
                  y = rep(c(8, 6, 3, 1), each = 5))
  expect_null(
    check_disp_finite(bnf(y ~ crf(x, model = "nec3param") + disp(~x)), d)
  )
  # Route B names a variance function rather than a formula, so there is no
  # expression to evaluate.
  expect_null(
    check_disp_finite(bnf(y ~ crf(x, model = "nec3param") + disp("power")), d)
  )
  expect_null(check_disp_finite(bnf(y ~ crf(x, model = "nec3param")), d))
  # The same term on data without the zero is fine, which is what makes this a
  # data-and-formula check rather than a rule about log().
  d2 <- d
  d2$x[d2$x == 0] <- 0.5
  expect_null(
    check_disp_finite(bnf(y ~ crf(x, model = "nec3param") + disp(~log(x))), d2)
  )
})

# Response substitution reports and records.

test_that("both boundary nudges are reported, with counts", {
  # Report each boundary change so comparisons with other fitting engines
  # can reproduce the actual response used.
  y <- c(rep(0.9, 6), rep(0.6, 6), rep(0.2, 6), rep(0, 3), rep(0.05, 3))
  rec <- substitution_record(y, NULL, validate_family("Beta"))
  expect_s3_class(rec, "data.frame")
  expect_equal(nrow(rec), 1)
  expect_equal(rec$n_rows, 3)
  expect_equal(rec$from, 0)
  expect_message(report_substitutions(rec), "3 value\\(s\\) at 0,")

  y1 <- y
  y1[1:2] <- 1
  rec1 <- substitution_record(y1, NULL, validate_family("Beta"))
  expect_equal(nrow(rec1), 2)
  expect_equal(rec1$n_rows, c(3, 2))
  expect_message(report_substitutions(rec1), "at 1,")

  # Nothing on a boundary, nothing to report.
  expect_null(substitution_record(c(0.2, 0.5, 0.8), NULL,
                                  validate_family("Beta")))
  expect_silent(report_substitutions(NULL))
})

test_that("the substitution message ends as its caller requires (#486)", {
  # bnec() stores the record, get_priors() prints priors built from the
  # shifted response, and update() on a fit with no record does neither.
  y <- c(rep(0.9, 6), rep(0.6, 6), rep(0.2, 6), rep(0, 3), rep(0.05, 3))
  rec <- substitution_record(y, NULL, validate_family("Beta"))
  ending <- function(...) {
    paste(capture.output(report_substitutions(rec, ...), type = "message"),
          collapse = " ")
  }
  on_fit <- ending()
  expect_match(on_fit, "see ?bnec_record.", fixed = TRUE)
  expect_no_match(on_fit, "prior")
  priors <- ending(on_fit = FALSE)
  expect_match(priors, "The priors below are derived", fixed = TRUE)
  expect_no_match(priors, "bnec_record")
  neither <- ending(on_fit = FALSE, prints_priors = FALSE)
  expect_match(neither, "shifted to 0.005. If those zeros", fixed = TRUE)
  expect_no_match(neither, "prior|bnec_record")
})

test_that("a hurdle family keeps its zeros and reports no substitution", {
  # The zeros are the hurdle signal, not a boundary artefact.
  y <- c(rep(3, 6), rep(2, 6), rep(0, 6))
  expect_null(substitution_record(y, NULL, validate_family("hurdle_gamma")))
  # And a censored zero is exempt for the same reason: the value is a declared
  # bound, not an artefact.
  cens <- c(rep(0, 12), rep(-1, 6))
  expect_null(substitution_record(y, cens, validate_family("Gamma")))
})


test_that("a data column named after a generated term is refused (#257)", {
  # brms resolves formula terms against the user's data first, so a column
  # named bnecmu or ogl would be used in place of the term bayesnec generates
  # and the fit would silently be a different model.
  d <- data.frame(x = rep(c(1, 10), each = 5), y = rep(c(0.8, 0.2), each = 5))
  expect_null(check_reserved_names(d))
  d$bnecmu <- 1
  expect_error(check_reserved_names(d), "bnecmu")
  expect_error(check_reserved_names(d), "silently be a different model")
  d$ogl <- 1
  expect_error(check_reserved_names(d), "ogl")
  # Both are named at once rather than one per call.
  expect_error(check_reserved_names(d), "bnecmu.*ogl")
})

test_that("the parameter-level generated terms are reserved too (#294)", {
  # Reserve all generated parameter-term names regardless of the current
  # family or group structure, which can change between calls.
  d <- data.frame(x = rep(c(1, 10), each = 5), y = rep(c(0.8, 0.2), each = 5))
  for (nm in c("botgl", "topgl", "bnecbot", "bnectop")) {
    dd <- d
    dd[[nm]] <- 1
    expect_error(check_reserved_names(dd), nm)
  }
  # Leave ordinary parameter names outside the generated-name refusal.
  # Collisions with predictor or response symbols are a separate check.
  for (nm in c("top", "bot", "nec", "beta")) {
    dd <- d
    dd[[nm]] <- 1
    expect_null(check_reserved_names(dd))
  }
})


# Resolve disp() terms in the formula environment.

test_that("a disp term written with a local function is still checked", {
  # Resolve local functions from the formula environment; an evaluation
  # error must not silently skip the finiteness check.
  d <- nec_data
  d$z <- c(0, d$x[-1])
  build <- function() {
    lg <- function(v) log(v)
    check_disp_finite(bnf(y ~ crf(x, "nec3param") + disp(~lg(z))), d)
  }
  expect_error(build(), "not finite")
})


# A bounded response at one boundary in every observation.

# Six concentrations of five, so a level of bnec_group() holds 30 rows.
cd_at_bound_x <- function() rep(c(0.1, 0.5, 1, 3, 10, 30), each = 5)

test_that("response_bound_reached names the bound every observation is at", {
  expect_identical(response_bound_reached(rep(0, 6)), 0)
  expect_identical(response_bound_reached(rep(1, 6)), 1)
  # Counts are compared with their own trials, which may differ by row.
  expect_identical(response_bound_reached(c(5, 8, 10), c(5, 8, 10)), 1)
  expect_identical(response_bound_reached(c(0, 0, 0), c(5, 8, 10)), 0)
  # One observation away from the bound is a response that varies.
  expect_identical(response_bound_reached(c(rep(1, 5), 0)), NA_real_)
  expect_identical(response_bound_reached(c(5, 8, 9), c(5, 8, 10)), NA_real_)
  expect_identical(response_bound_reached(c(rep(0, 5), 0.2)), NA_real_)
  # Both bounds at once is a response that varies, not one at a bound.
  expect_identical(response_bound_reached(c(0, 1, 0, 1)), NA_real_)
  expect_identical(response_bound_reached(numeric(0)), NA_real_)
})

test_that("the update route tests the family the refit uses (#400)", {
  # Check the stored refit family: an all-one response can suggest a different
  # family from the Bernoulli family actually being refitted.
  bern <- nec4param
  bern$fit$family <- brms::bernoulli(link = "identity")
  d <- bern$fit$data
  d$y <- rep(1L, nrow(d))
  err <- expect_error(check_update_data(list(bern), d))
  expect_match(conditionMessage(err),
               "The response \"y\" is at the upper bound of a bernoulli",
               fixed = TRUE)
  # A Gaussian refit stays Gaussian even when all new responses equal one;
  # the data must not select a replacement family.
  d1 <- nec4param$fit$data
  d1$y <- 1
  res <- suppressMessages(check_update_data(list(nec4param), d1))
  expect_false(res$changed_family)
  expect_true(all(res$data$y == 1))
  # A supplied family is the one the refit uses, so it is the one tested.
  expect_error(
    check_update_data(list(nec4param), d1, Beta(link = "identity")),
    "at the upper bound of a beta response", fixed = TRUE
  )
  # Check zero_inflated_beta at both bounds.
  expect_error(
    check_update_data(list(nec4param), d1, "zero_inflated_beta"),
    "at the upper bound of a zero_inflated_beta response", fixed = TRUE
  )
  d0 <- d1
  d0$y <- 0
  expect_error(
    check_update_data(list(nec4param), d0, "zero_inflated_beta"),
    "so the beta block has no data", fixed = TRUE
  )
})

test_that("the update route lets an ecxflat fit through at a bound (#419)", {
  # bnec() fits ecxflat alone to such a response, so an ecxflat fit given new
  # data at a bound is refitted; the fit's own equation is what is tested.
  flat <- ecx4param
  flat$model <- "ecxflat"
  flat$fit$family <- brms::bernoulli(link = "identity")
  flat$bayesnecformula <- bayesnecformula(y ~ crf(x, model = "ecxflat"))
  d <- flat$fit$data
  d$y <- rep(1L, nrow(d))
  res <- tryCatch(suppressMessages(check_update_data(list(flat), d)),
                  error = conditionMessage)
  expect_false(any(grepl("upper bound", unlist(res), fixed = TRUE)))
  # The same data under a curve equation is refused, naming ecxflat.
  bern <- nec4param
  bern$fit$family <- brms::bernoulli(link = "identity")
  expect_error(check_update_data(list(bern), d),
               "The constant equation ecxflat", fixed = TRUE)
})

test_that("update() refuses before the refit, and refits a gaussian (#400)", {
  # Mock the imported update binding to halt brms. Call stats::update()
  # explicitly so the real method, including its guards, still runs.
  calls <- 0L
  local_mocked_bindings(
    update = function(...) {
      calls <<- calls + 1L
      stop("refit reached")
    },
    .package = "bayesnec"
  )
  bern <- nec4param
  bern$fit$family <- brms::bernoulli(link = "identity")
  d <- bern$fit$data
  d$y <- rep(1L, nrow(d))
  expect_error(stats::update(bern, newdata = d),
               "at the upper bound of a bernoulli response", fixed = TRUE)
  expect_identical(calls, 0L)
  d1 <- nec4param$fit$data
  d1$y <- 1
  err <- NULL
  capture.output(
    err <- tryCatch(
      suppressMessages(stats::update(nec4param, newdata = d1,
                                     force_fit = TRUE)),
      error = conditionMessage
    ),
    type = "message"
  )
  expect_false(grepl("upper bound", err))
  expect_identical(calls, 1L)
})

test_that("an interval-censored response is judged on its recorded values (#400)", {
  # Refuse recorded values all at a family bound even when intervals extend
  # inside support: default priors use the recorded values alone.
  x <- cd_at_bound_x()
  d <- data.frame(x = x, cover = 0, cens = "interval",
                  upper = seq(0.05, 0.5, length.out = length(x)))
  err <- expect_error(suppressMessages(
    get_priors(cover | cens(cens, upper) ~ crf(x, model = "nec3param"),
               data = d, family = "Beta")
  ))
  expect_match(conditionMessage(err),
               "The response \"cover\" is at the lower bound of a beta",
               fixed = TRUE)
  expect_false(grepl("na.rm", conditionMessage(err), fixed = TRUE))
  # The same for a count at its trials with an interval below it.
  n <- data.frame(x = x, alive = 10L, exposed = 10L, cens = "interval",
                  lower = 8L)
  err <- expect_error(suppressMessages(
    get_priors(alive | trials(exposed) + cens(cens, lower) ~
                 crf(x, model = "nec3param"), data = n, family = "binomial")
  ))
  expect_match(conditionMessage(err), "every count equals its number of trials",
               fixed = TRUE)
})

test_that("check_response_at_bound leaves every other response alone", {
  x <- cd_at_bound_x()
  ones <- cd_bdat(y ~ crf(x, model = "nec3param"), data.frame(x = x, y = 1))
  zeros <- cd_bdat(y ~ crf(x, model = "nec3param"),
                   data.frame(x = x, y = 0L))
  # Not a bounded family: a constant gaussian or count response is a matter
  # for the prior construction of those families, not for this refusal.
  expect_silent(check_response_at_bound(ones, gaussian()))
  expect_silent(check_response_at_bound(zeros, poisson()))
  # zero_inflated_beta is included in the bound check at both endpoints.
  expect_error(check_response_at_bound(ones, "zero_inflated_beta"),
               "upper bound of a zero_inflated_beta response", fixed = TRUE)
  # Its zeros are inside its support, so a response with any value above zero
  # varies and is left alone.
  zib_near <- cd_bdat(y ~ crf(x, model = "nec3param"),
                      data.frame(x = x, y = c(rep(0, 29), 0.4)))
  expect_silent(check_response_at_bound(zib_near, "zero_inflated_beta"))
  near <- cd_bdat(y ~ crf(x, model = "nec3param"),
                  data.frame(x = x, y = c(rep(1L, 29), 0L)))
  expect_silent(check_response_at_bound(near, bernoulli()))
  # Without trials(), this helper cannot locate a binomial bound. Other
  # routes refuse the missing term before reaching this check.
  expect_silent(check_response_at_bound(zeros, binomial()))
})

test_that("a grouped response names every level at a bound, and only those", {
  x <- cd_at_bound_x()
  d <- data.frame(x = rep(x, 3), y = c(rep(1L, 30), rep(0L, 30),
                                       rep(c(1L, 0L), 15)),
                  site = rep(c("north", "south", "reef"), each = 30))
  bdat <- cd_bdat(y ~ crf(x, model = "nec3param"), d)
  err <- expect_error(check_response_at_bound(
    bdat, bernoulli(), group = factor(d$site), group_name = "site"
  ))
  msg <- conditionMessage(err)
  expect_match(msg, "2 level(s) of \"site\"", fixed = TRUE)
  expect_match(msg, "\"north\", where every value is 1 (the upper bound)",
               fixed = TRUE)
  expect_match(msg, "\"south\", where every value is 0 (the lower bound)",
               fixed = TRUE)
  expect_false(grepl("reef", msg))
  expect_match(msg, "Remove those levels from `data`", fixed = TRUE)
  # The level that varies passes on its own as well.
  reef <- cd_bdat(y ~ crf(x, model = "nec3param"), d[d$site == "reef", ])
  expect_silent(check_response_at_bound(reef, bernoulli()))
})

# The constant equation at a response bound.

test_that("ecxflat alone is let through at a bound, except at 0 of beta (#419)", {
  # Callers supply the equations they will fit. A set containing a curve
  # equation is refused at a bound; ecxflat alone passes where it is valid.
  cases <- at_bound_cases()
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    bdat <- cd_bdat(cs$formula, cs$data)
    fam <- validate_family(cs$family)
    err <- expect_error(
      check_response_at_bound(bdat, fam, model = c("ecxflat", "nec3param"))
    )
    if (nm %in% at_bound_refused) {
      expect_match(conditionMessage(err),
                   "no equation can be fitted to it, the constant equation",
                   fixed = TRUE, info = nm)
      # Use each Beta family's own refusal: zero_inflated_beta permits zeros
      # but still needs observations above zero for its Beta block.
      reason <- if (nm == "zib_zero") {
        "no observation of this response is above zero, so the beta block"
      } else {
        "A beta distribution cannot represent a zero"
      }
      expect_error(check_response_at_bound(bdat, fam, model = "ecxflat"),
                   reason, fixed = TRUE, info = nm)
      next
    }
    expect_match(conditionMessage(err),
                 "The constant equation ecxflat, whose mean does not change",
                 fixed = TRUE, info = nm)
    expect_silent(check_response_at_bound(bdat, fam, model = "ecxflat"))
  }
})

test_that("constant_fallback replaces the set and records why (#419)", {
  cs <- at_bound_cases()$binomial_trials
  bdat <- cd_bdat(cs$formula, cs$data)
  fam <- validate_family(cs$family)
  msgs <- character(0)
  out <- withCallingHandlers(
    constant_fallback(bdat, fam, c("nec3param", "ecx4param")),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_identical(out$model, "ecxflat")
  expect_identical(out$excluded$model, c("nec3param", "ecx4param"))
  expect_match(out$excluded$reason, "upper bound of a binomial response",
               fixed = TRUE)
  expect_length(msgs, 1)
  expect_match(msgs, "the 2 other equation(s) requested are not fitted",
               fixed = TRUE)
  # A name that is no equation is refused, not recorded as set aside.
  expect_error(constant_fallback(bdat, fam, c("nec3param", "bogus")),
               "bogus; is not a valid model entry", fixed = TRUE)
  # Said once by bnec_group() for every level, so the level's call is silent.
  expect_silent(constant_fallback(bdat, fam, "nec3param", report = FALSE))
  # A response one observation off the bound is fitted with the set asked for.
  near <- cd_bdat(cs$formula, cs$near)
  expect_null(constant_fallback(near, fam, "nec3param"))
  # So is a family with no bound to be at.
  ones <- cd_bdat(y ~ crf(x, model = "nec3param"),
                  data.frame(x = cd_at_bound_x(), y = 1))
  expect_null(constant_fallback(ones, gaussian(), "nec3param"))
  # Beta at 0 cannot be fitted with ecxflat either, and is refused.
  bz <- at_bound_cases()$beta_zero
  expect_error(
    constant_fallback(cd_bdat(bz$formula, bz$data),
                      validate_family(bz$family), "nec3param"),
    "A beta distribution cannot represent a zero", fixed = TRUE
  )
  # zero_inflated_beta also refuses an all-zero response and substitutes
  # ecxflat at an all-one response.
  cases <- at_bound_cases()
  zz <- cases$zib_zero
  expect_error(
    constant_fallback(cd_bdat(zz$formula, zz$data),
                      validate_family(zz$family), "nec3param"),
    "so the beta block has no data", fixed = TRUE
  )
  zo <- cases$zib_one
  out <- suppressMessages(
    constant_fallback(cd_bdat(zo$formula, zo$data), validate_family(zo$family),
                      c("nec3param", "ecx4param"))
  )
  expect_identical(out$model, "ecxflat")
  expect_match(out$excluded$reason,
               "upper bound of a zero_inflated_beta response", fixed = TRUE)
  expect_null(constant_fallback(cd_bdat(zo$formula, zo$near),
                                validate_family(zo$family), "nec3param"))
  expect_null(constant_fallback(cd_bdat(zz$formula, zz$near),
                                validate_family(zz$family), "nec3param"))
})

test_that("a grouped call names the levels fitted with ecxflat (#419)", {
  x <- cd_at_bound_x()
  d <- data.frame(x = rep(x, 3), y = c(rep(1L, 30), rep(0L, 30),
                                       rep(c(1L, 0L), 15)),
                  site = rep(c("north", "south", "reef"), each = 30))
  bdat <- cd_bdat(y ~ crf(x, model = "nec3param"), d)
  msgs <- character(0)
  levs <- withCallingHandlers(
    constant_fallback_levels(bdat, bernoulli(), factor(d$site), "site"),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_setequal(levs, c("north", "south"))
  expect_length(msgs, 1)
  # Nothing is set aside where ecxflat alone was requested, so nothing is said,
  # as bnec() says nothing then; the levels are still returned.
  expect_silent(quiet <- constant_fallback_levels(
    bdat, bernoulli(), factor(d$site), "site", model = "ecxflat"
  ))
  expect_setequal(quiet, c("north", "south"))
  # A name that is no equation is refused before any level is fitted.
  expect_error(
    constant_fallback_levels(bdat, bernoulli(), factor(d$site), "site",
                             model = c("nec3param", "bogus")),
    "bogus; is not a valid model entry", fixed = TRUE
  )
  expect_match(msgs, "2 level(s) of \"site\"", fixed = TRUE)
  expect_match(msgs, "\"north\" (the upper bound)", fixed = TRUE)
  expect_false(grepl("reef", msgs))
  # A beta level at 0 cannot be fitted with ecxflat, so the whole call is
  # refused before any level is fitted, naming that level alone.
  b <- data.frame(x = rep(x, 3), cover = c(rep(1, 30), rep(0, 30),
                                           rep(c(0.9, 0.2), 15)),
                  site = rep(c("north", "south", "reef"), each = 30))
  bb <- cd_bdat(cover ~ crf(x, model = "nec3param"), b)
  err <- expect_error(constant_fallback_levels(
    bb, validate_family("Beta"), factor(b$site), "site"
  ))
  msg <- conditionMessage(err)
  expect_match(msg, "1 level(s) of \"site\": \"south\"", fixed = TRUE)
  expect_match(msg, "these levels do not have, so no equation can be fitted",
               fixed = TRUE)
  expect_false(grepl("north", msg))
  # For zero_inflated_beta, refuse an all-zero level but fit an all-one
  # level with ecxflat.
  zmsgs <- character(0)
  zerr <- tryCatch(
    withCallingHandlers(
      constant_fallback_levels(bb, validate_family("zero_inflated_beta"),
                               factor(b$site), "site"),
      message = function(m) {
        zmsgs <<- c(zmsgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    ),
    error = conditionMessage
  )
  expect_match(zerr, "1 level(s) of \"site\": \"south\"", fixed = TRUE)
  expect_match(zerr, paste("no observation of these levels is above zero,",
                           "so the beta block has no data"), fixed = TRUE)
  expect_false(grepl("north", zerr))
  expect_length(zmsgs, 0)
  b1 <- b[b$site != "south", ]
  zlevs <- suppressMessages(constant_fallback_levels(
    cd_bdat(cover ~ crf(x, model = "nec3param"), b1),
    validate_family("zero_inflated_beta"), factor(b1$site), "site"
  ))
  expect_identical(zlevs, "north")
})

test_that("the asymptote report does not count ecxflat on either side (#419)", {
  # Exclude ecxflat from both sides of the asymptote-declaration report: it
  # has no bot, and a constant can be distinguished from a declining curve
  # without observing that curve's lower asymptote.
  bdat <- cd_bdat(y ~ crf(x, model = "nec4param"), nec_data)
  report <- function(models) {
    msgs <- character(0)
    withCallingHandlers(
      try(check_asymptote_declaration(bdat, validate_family("Beta"), models,
                                      asymptote_observed = FALSE),
          silent = TRUE),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    any(grepl("This set mixes equations", msgs, fixed = TRUE))
  }
  expect_false(report(c("ecxflat", "nec3param")))
  expect_false(report(c("ecxflat", "nec4param")))
  expect_true(report(c("nec3param", "nec4param")))
})

# A binomial formula without trials().

test_that("check_trials_term refuses binomial and beta_binomial alone (#442)", {
  # nec_data's count and trials columns are added in setup.R. The predictor is
  # transformed so that the example in the message is seen to write it as the
  # formula did.
  no_trials <- cd_bdat(count ~ crf(log(x), model = "nec3param"), nec_data)
  with_trials <- cd_bdat(count | trials(trials) ~ crf(x, model = "nec3param"),
                         nec_data)
  err <- expect_error(check_trials_term(no_trials, validate_family("binomial")))
  msg <- conditionMessage(err)
  expect_match(msg, paste("The binomial family needs the number of trials for",
                          "each observation, and the formula has no trials()",
                          "term"), fixed = TRUE)
  expect_match(msg, "count | trials(n) ~ crf(log(x), \"nec3param\")",
               fixed = TRUE)
  expect_error(check_trials_term(no_trials, "beta_binomial"),
               "The beta_binomial family needs", fixed = TRUE)
  expect_silent(check_trials_term(with_trials, validate_family("binomial")))
  expect_silent(check_trials_term(with_trials, "beta_binomial"))
  # No other family reads a number of trials.
  expect_silent(check_trials_term(no_trials, poisson()))
  expect_silent(check_trials_term(no_trials, "negbinomial"))
  expect_silent(check_trials_term(no_trials, bernoulli()))
})

test_that("check_data() keeps the trials() refusal as the backstop (#442)", {
  # The update route still needs check_data() to refuse missing trials by
  # name before an indexed lookup fails.
  err <- expect_error(cd_run(count ~ crf(x, model = "nec3param"), nec_data,
                             validate_family("beta_binomial")))
  expect_match(conditionMessage(err), "has no trials() term", fixed = TRUE)
  expect_false(grepl("subscript", conditionMessage(err), fixed = TRUE))
  # With the term, the trials reach the checked frame as recorded.
  res <- cd_run(count | trials(trials) ~ crf(x, model = "nec3param"),
                nec_data, validate_family("binomial"))
  expect_identical(res$mod_dat$trials, nec_data$trials)
})
