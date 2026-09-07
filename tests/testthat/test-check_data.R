# check_data() had no test file of its own until this one, in any release. It
# was exercised only from the side, as a step on the way to testing
# fit_bayesnec(), get_priors(), inits_functions(), cens and hurdle_family, and
# five defects have been found in it in two weeks: #258, #265, #269, #271 and
# #274. Four of the five are the same shape -- a branch that cannot be reached,
# or a correction that reaches one caller and not another -- which is what a
# test written from the caller's side cannot see.
#
# So this file specifies check_data() as a function rather than as a step: its
# guards, the arithmetic and the messaging of each correction it makes, the
# shape of what it returns, and what each of its callers does with the result.
#
# What is deliberately NOT duplicated here, and where it lives:
#   the write-back into brm()'s data frame  tests/testthat/test-fit_bayesnec.R
#   the integer predictor guard             tests/testthat/test-fit_bayesnec.R
#   pop_var_is_transformed()                tests/testthat/test-fit_bayesnec.R
#   the censoring exemptions                tests/testthat/test-cens.R
#   zeros preserved for a hurdle family     tests/testthat/test-hurdle_family.R
#   check_normalisation()                   tests/testthat/test-check_normalisation.R

# Build the model frame the way bnec() does, which is the only route any caller
# uses. Asserting through it rather than on a hand-built data frame is what
# makes the two unreachable-branch tests below meaningful.
cd_bdat <- function(formula, data) {
  model.frame(bayesnecformula(formula), data = data, run_par_checks = TRUE)
}

cd_run <- function(formula, data, family, model = "nec3param") {
  check_data(cd_bdat(formula, data), family, model)
}

# The missing-value refusal, as a regex, so the count and the rows can be
# asserted around it without repeating the fixed part.
cd_missing_msg <- "row\\(s\\) with missing values \\(NA or NaN\\), at row\\(s\\)"

# The messages check_data() emits, as a character vector. Several corrections
# are silent, and asserting the absence of a message is half of what this file
# is for, so the helper returns them rather than swallowing them.
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
  # model.frame() removes incomplete cases before check_data() is given the
  # data, so until #278 an NA or NaN left the fit running on fewer rows than
  # were supplied with nothing said, while Inf was refused. Both are now
  # refused. The rows model.frame() removed are read off the na.action
  # attribute, which is the only remaining evidence that they existed, so the
  # message can name them.
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
  # names(attr(data, "na.action")) holds the row names and its values hold the
  # positions. The name is what the user sees in their own data frame, and
  # where bnec() has been handed a subset -- one level of a bnec_group() call
  # -- a position indexes the subset and names no row of the data supplied.
  d <- cd_data_zero()
  rownames(d) <- paste0("s", seq_len(nrow(d)) + 100L)
  d$y[3] <- NA
  expect_error(cd_run(y ~ crf(x, model = "nec3param"), d, gaussian()),
               paste("1", cd_missing_msg, "s103"))
})

test_that("an NA reaching the guard directly is named by column", {
  # The na.action attribute is absent when the user has set
  # options(na.action = "na.pass"), so the finiteness guard is the only thing
  # that sees the missing value. It reads is.finite() elementwise rather than
  # is.finite(mean(x)) so that this case is refused too, naming which of the
  # two columns holds it.
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


# ---- the two guards check_data() no longer duplicates -----------------------
#
# check_data() used to compose its own message for each of the two inputs
# below, and neither branch could fire: an earlier call raised first on the same
# input. Both were removed in #278. The tests remain, asserting the error that
# does fire, so that a reader who reinstates either branch is told what already
# refuses the input.

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
  # check_data() used to assign custom_name <- check_custom_name(family) and
  # read it nowhere. check_custom_name() is pure, so the call had no effect and
  # was removed in #278, along with the same dead assignment at three further
  # sites in plot() and prep_raw_data().
  #
  # Asserted by making the call raise. A discarded result cannot be observed
  # from the return value -- which is why the assignment survived -- so the
  # absence of the call is what is asserted instead.
  d <- cd_data_zero()
  local_mocked_bindings(
    check_custom_name = function(...) stop("check_custom_name was called"),
    .package = "bayesnec"
  )
  expect_error(cd_run(y ~ crf(x, model = "nec3param"), d, gaussian()),
               NA)
})


# ---- the response boundary corrections --------------------------------------
#
# The arithmetic and the messaging are asserted separately, because they do not
# agree: one of the three corrections is announced and two are not. That asymmetry
# is what #93 was reopened on, so it is pinned here as a value rather than left
# as a claim in an issue comment.

test_that("a Gamma zero is shifted by one tenth of the smallest non-zero value", {
  res <- suppressMessages(cd_run(y ~ crf(x, model = "nec3param"),
                                 cd_data_zero(), Gamma(link = "identity")))
  # min(y[y > 0]) is 3, so the zeros become 0.3 and nothing else changes.
  expect_identical(sort(unique(res$mod_dat$y)), c(0.3, 3, 6, 8))
})

test_that("the Gamma zero shift is reported, and names the remedy", {
  # The report moved out of check_data() and into report_substitutions(), which
  # the user-facing entry points call once per call rather than once per model.
  # check_data() is silent; the message and its remedy are unchanged. See #93.
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
  # INVERTED. This pinned the #93 measurement that the two beta corrections
  # were silent while the Gamma one messaged. All three are now reported, and
  # none of them from check_data(), which runs once per model and would repeat
  # the message for every member of a model set.
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
  # #269 removed all three predictor corrections. A zero control is an ordinary
  # control: no family constrains the support of a predictor.
  d <- cd_data_zero()
  d$x <- rep(c(0, 1, 10, 100), each = 5)
  res <- suppressMessages(cd_run(y ~ crf(x, model = "nec3param"), d,
                                 Gamma(link = "identity")))
  expect_identical(res$mod_dat$x, d$x)
})

test_that("a 0-1 bounded predictor keeps both of its bounds", {
  # #265: check_data() tested x_type == "beta" while set_distribution() returns
  # "Beta", so these two branches never fired in any release. #269 removed them
  # rather than repairing them, and this pins that they stay removed.
  d <- data.frame(x = rep(c(0, 0.25, 0.5, 1), each = 5),
                  y = rep(c(8, 6, 3, 1), each = 5))
  res <- cd_run(y ~ crf(x, model = "nec3param"), d, Gamma(link = "identity"))
  expect_identical(range(res$mod_dat$x), c(0, 1))
})


# ---- the shape of what is returned ------------------------------------------

test_that("check_data returns mod_dat, the family and the substitutions", {
  res <- cd_run(y ~ crf(x, model = "nec3param"), cd_data_zero(), gaussian())
  # substitutions joined the return with #93: the corrections check_data()
  # makes have to be recoverable by the caller, which reports them once and
  # stores them on the fit.
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


# ---- what each caller does with the result ----------------------------------
#
# check_data() has three call sites in R/ and they do not agree on what to do
# with the corrected data frame it returns. #258 was one caller getting it
# wrong; #274 is a second, found four days later. Enumerating them here is what
# makes the next one visible without another user report.

test_that("get_priors builds its priors from the corrected response", {
  # R/get_priors.R:163. The Gamma zero shift must reach the prior, or top and
  # nec are derived from a response the fit will never see.
  #
  # Asserted by equality against the same data with the shift applied by hand,
  # which is the only form of the assertion that discriminates. A finiteness
  # check does not: the prior is finite either way. Measured, the top prior is
  # gamma(2, 0.3077) from the corrected response and gamma(2, 0.25) from the
  # uncorrected one, so the two frames differ if and only if the correction is
  # bypassed.
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
  # The prior column alone. nlpar is decided by the model and the family, not
  # by the five shifted values, so comparing it passes under the mutation this
  # block exists to catch and asserts nothing the prior comparison does not.
  expect_identical(pr$prior, pr_shifted$prior)
})

test_that("the update route writes back the correction it reports (#274)", {
  # INVERTED. This pinned the #274 defect: update() with newdata ran
  # check_data() through has_family_changed(), which kept only the family and
  # dropped the corrected data frame, so the user was told their data had been
  # repaired and the fit then failed on the condition reported repaired -- the
  # #258 symptom on a route #258 did not cover.
  #
  # The return is no longer a bare logical: it carries the corrected frame,
  # which is what makes the correction reachable by the caller.
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
  # The write-back corrects `data` inside the loop, so iteration 2 rebuilds the
  # model frame from a response that no longer sits on the boundary and its own
  # record is empty. Reading the last iteration's record therefore reported
  # nothing at all for a model set -- which is what update.bnecfit() passes for
  # any bayesmanecfit -- while reporting normally for a single model. The
  # record is now taken from the first iteration. See #93.
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


# ---- #271, the dispersion sub-model finiteness check -------------------------

test_that("a disp() sub-model that is not finite is refused, naming the term", {
  # check_data() tests the predictor and the response for finiteness, but it
  # inspects only the population variables crf() declares. A disp(~...) term is
  # an arbitrary brms formula whose variables are deliberately kept out of the
  # model frame, so nothing tested it and an infinite value reached Stan: the
  # fit did not run, reporting a brms warning about the data in general with
  # nothing naming the term responsible. See #271.
  d <- data.frame(x = rep(c(0, 1, 10, 100), each = 5),
                  y = rep(c(8, 6, 3, 1), each = 5))
  expect_error(
    check_disp_finite(bnf(y ~ crf(x, model = "nec3param") + disp(~log(x))), d),
    "log\\(x\\)"
  )
  # A missing value is not an infinite one, and naming it as one pointed at
  # log() of a zero, which is the wrong cause. check_data()'s complete-cases
  # check cannot report it either: it is given the model frame, which by design
  # does not contain a disp() term's columns.
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
  # parse_disp_term()'s own refusals are raised from here rather than swallowed
  # by a try(). Suppressing them deferred the error to add_brm_defaults(), which
  # runs inside the per-model try() in bnec(), so one malformed formula printed
  # the refusal once for every member of the model set.
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

# ---- #93, the response substitutions are reported and recorded ---------------

test_that("both boundary nudges are reported, with counts", {
  # Two of the three were silent, so a user comparing bayesnec against another
  # engine had no way to see that the data had been altered. See #93 and D16.
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
