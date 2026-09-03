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

test_that("an NA or NaN row is dropped by model.frame, not caught by the check", {
  # The finiteness guard sees only what model.frame() passes it, and
  # model.frame() drops incomplete cases first. So Inf reaches the guard and is
  # refused, while NA and NaN are removed silently and the fit proceeds on
  # fewer rows than the user supplied. Pinned because the two behave
  # differently and neither is announced; it is the same class of gap as #271,
  # where a disp() sub-model is not checked for finiteness at all.
  d <- cd_data_zero()
  d$y[1] <- NaN
  res <- cd_run(y ~ crf(x, model = "nec3param"), d, gaussian())
  expect_identical(nrow(res$mod_dat), nrow(d) - 1L)
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

test_that("an integer predictor is refused from the data check", {
  d <- cd_data_zero()
  d$x <- as.integer(d$x * 10)
  expect_error(cd_run(y ~ crf(x, model = "nec3param"), d, gaussian()),
               "does not currently support integer concentration")
})


# ---- two guards in check_data() that nothing can reach -----------------------
#
# Both branches below are written in check_data() and neither can fire, because
# an earlier call raises first on the same input. They are pinned rather than
# removed: a test that names the reachable error is what tells the next reader
# that the branch underneath it is dead, and #265 is the precedent for a
# condition that was written, never fired, and went unnoticed for five years.

test_that("a character predictor errors from retrieve_var, not check_data", {
  d <- cd_data_zero()
  d$x <- as.character(d$x)
  # check_data():112-118 composes "Your indicated predictor column ... requires
  # the predictor column to be numeric". retrieve_var(error = TRUE) at :108
  # raises first, so that message can never be produced.
  msg <- tryCatch(cd_run(y ~ crf(x, model = "nec3param"), d, gaussian()),
                  error = conditionMessage)
  expect_match(msg, "is not numeric")
  expect_false(grepl("indicated predictor column", msg))
})

test_that("a numeric group-level column errors from model.frame, not check_data", {
  d <- cd_data_zero()
  d$grp <- rep(1:2, 10)
  # check_data():203-209 composes "Your group-level column(s) ... must be either
  # a character or a factor". model.frame() refuses first, so that message can
  # never be produced either.
  msg <- tryCatch(cd_run(y ~ crf(x, model = "nec3param") + ogl(grp), d,
                         gaussian()),
                  error = conditionMessage)
  expect_match(msg, "Group-level variables cannot be numeric")
  expect_false(grepl("must be either a character or a factor", msg))
})


# ---- the response boundary corrections --------------------------------------
#
# The arithmetic and the messaging are asserted separately, because they do not
# agree: one of the three corrections speaks and two are silent. That asymmetry
# is what #93 was reopened on, so it is pinned here as a value rather than left
# as a claim in an issue comment.

test_that("a Gamma zero is shifted by one tenth of the smallest non-zero value", {
  res <- suppressMessages(cd_run(y ~ crf(x, model = "nec3param"),
                                 cd_data_zero(), Gamma(link = "identity")))
  # min(y[y > 0]) is 3, so the zeros become 0.3 and nothing else changes.
  expect_identical(sort(unique(res$mod_dat$y)), c(0.3, 3, 6, 8))
})

test_that("the Gamma zero shift is reported, and names the remedy", {
  msg <- paste(cd_messages(y ~ crf(x, model = "nec3param"), cd_data_zero(),
                           Gamma(link = "identity")), collapse = " ")
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

test_that("both beta corrections are silent, unlike the Gamma one", {
  # Pins the #93 measurement. If either correction is given a message, this
  # assertion fails and #93 is the issue to read before changing it.
  expect_length(cd_messages(y ~ crf(x, model = "nec3param"),
                            cd_data_bounded(), Beta(link = "identity")), 0)
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

test_that("check_data returns mod_dat and the family, and nothing else", {
  res <- cd_run(y ~ crf(x, model = "nec3param"), cd_data_zero(), gaussian())
  expect_named(res, c("mod_dat", "family"))
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
  # bot are derived from a response the fit will never see.
  d <- cd_data_zero()
  pr <- suppressMessages(
    get_priors(y ~ crf(x, model = "nec3param"), data = d,
               family = Gamma(link = "identity"))
  )
  expect_s3_class(pr, "brmsprior")
  expect_true(all(is.finite(as.numeric(
    gsub(".*\\(|,.*|\\)", "", pr$prior[pr$nlpar == "bot"])
  ))))
})

test_that("has_family_changed reports a correction it then discards", {
  # PINS THE #274 DEFECT. update() with newdata runs check_data() through
  # has_family_changed(), which keeps only the family and drops the corrected
  # data frame, so the user is told their data was repaired and the fit then
  # fails on the condition that was reported repaired -- the #258 symptom on a
  # route #258 did not cover.
  #
  # INVERT THIS TEST WHEN #274 IS FIXED: the message should no longer be
  # emitted from this route, or the correction should reach the caller.
  skip_on_cran()
  data(manec_example, package = "bayesnec")
  f <- suppressMessages(pull_out(manec_example, model = "nec4param"))
  d <- f$fit$data
  d$y <- abs(d$y)
  d$y[1:3] <- 0
  msgs <- capture.output(
    invisible(has_family_changed(list(f), d, Gamma(link = "identity"))),
    type = "message"
  )
  expect_true(any(grepl("shifted", msgs)))
  expect_identical(sum(d$y == 0), 3L)
})
