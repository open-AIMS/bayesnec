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
  missing_msg <- "row\\(s\\) with missing values \\(NA or NaN\\), at row\\(s\\)"
  expect_error(cd_run(y ~ crf(x, model = "nec3param"), d, gaussian()),
               paste("1", missing_msg, "1"))
  # NA in the predictor is refused on the same route, and more than one row is
  # counted and named rather than only the first.
  d2 <- cd_data_zero()
  d2$x[c(2, 7)] <- NA
  expect_error(cd_run(y ~ crf(x, model = "nec3param"), d2, gaussian()),
               paste("2", missing_msg, "2, 7"))
  # The remedy is named, since the user has to act on it: dropping the rows is
  # no longer done for them.
  msg <- tryCatch(cd_run(y ~ crf(x, model = "nec3param"), d, gaussian()),
                  error = conditionMessage)
  expect_match(msg, "remove or impute those rows")
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

test_that("has_family_changed reports a correction it then discards", {
  # PINS THE #274 DEFECT. update() with newdata runs check_data() through
  # has_family_changed() (R/bnecfit-methods.R:171), which keeps only the family
  # and drops the corrected data frame, so the user is told their data was
  # repaired and the fit then fails on the condition that was reported
  # repaired -- the #258 symptom on a route #258 did not cover.
  #
  # Two assertions, both of which can fail. The first is that the message is
  # emitted from this route at all. The second is that the whole of what comes
  # back is a length-one logical, which is what makes the correction
  # unreachable by the caller: there is nowhere for the repaired data frame to
  # go. Asserting the state of the local d instead would be a tautology --
  # has_family_changed() takes d by value and cannot alter it.
  #
  # INVERT THIS TEST WHEN #274 IS FIXED: either the message is no longer
  # emitted from this route, or the return includes the corrected data frame
  # and is no longer a bare logical.
  skip_on_cran()
  f <- nec4param
  d <- f$fit$data
  d$y <- abs(d$y)
  d$y[1:3] <- 0
  res <- NULL
  msgs <- capture.output(
    res <- has_family_changed(list(f), d, Gamma(link = "identity")),
    type = "message"
  )
  expect_true(any(grepl("shifted", msgs)))
  expect_type(res, "logical")
  expect_length(res, 1)
})
