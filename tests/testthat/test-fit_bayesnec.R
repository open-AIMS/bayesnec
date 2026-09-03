# #258: check_data() corrects a response sitting on a boundary its family
# cannot represent, and fit_bayesnec() writes that correction into the data
# frame brm() is handed. The write-back was decided for the formula as a whole,
# so a transformation written inline in crf() -- on any population variable --
# discarded every correction, and brm() then failed naming the condition the
# package had just reported it had repaired.
#
# #269: the predictor is no longer corrected at all, so the assertions here
# about the predictor are that it reaches brm() exactly as recorded.
#
# Three levels of assertion are made. fit_data() drives fit_bayesnec() itself
# with brm() mocked, which is what proves the write-back reaches the fit; one
# case drives bnec() over a model set, which is where the conflict is raised;
# and brm_data() runs check_data() and write_back_checks() in the order
# fit_bayesnec() calls them, which is cheap enough to cover the boundary cases
# exhaustively.

gamma_boundary_data <- function() {
  d <- data.frame(x = rep(c(0.1, 1, 10, 100), each = 5))
  d$y <- c(rep(c(8, 6, 3), each = 5), rep(0, 5))
  d
}

# The data frame brm() is actually given. brm() is mocked rather than run: the
# assertion is about the object it receives and not about anything the sampler
# does with it. `init` is supplied so that add_brm_defaults() skips the
# initial-value search, which is stochastic and can take minutes (#266), and
# are_chains_correct() is mocked because the fake fit has no chains to count.
fit_data <- function(formula, data, family, brm_args = list()) {
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    brm = function(formula, data, ...) {
      seen$data <- data
      structure(list(), class = "brmsfit")
    },
    are_chains_correct = function(...) TRUE,
    .package = "bayesnec"
  )
  suppressMessages(suppressWarnings(
    bayesnec:::fit_bayesnec(
      formula = bnf(formula), data = data, model = "nec3param",
      brm_args = c(list(family = family, init = list(list()), chains = 1,
                        iter = 10), brm_args))
  ))
  seen$data
}

# The same object, reached without driving fit_bayesnec(). Used for the cases
# that only exercise check_data() and the write-back.
brm_data <- function(formula, data, family) {
  bdat <- model.frame(bnf(formula), data = data)
  checked <- suppressMessages(bayesnec:::check_data(bdat, family, "nec3param"))
  bayesnec:::write_back_checks(data, bdat, "y_var", checked$mod_dat$y)
}

test_that("a Gamma zero is repaired when the predictor is transformed inline", {
  d <- gamma_boundary_data()
  gam <- Gamma(link = "identity")
  inline <- brm_data(y ~ crf(log(x), model = "nec3param"), d, gam)
  d_pre <- d
  d_pre$log_x <- log(d_pre$x)
  pre <- brm_data(y ~ crf(log_x, model = "nec3param"), d_pre, gam)
  expect_false(any(inline$y == 0))
  expect_equal(min(inline$y), 0.3)
  # the correction is the one the equivalent pre-transformed call receives
  expect_equal(inline$y, pre$y)
  # the predictor itself is untouched: brm() re-evaluates log(x) from it
  expect_identical(inline$x, d$x)
})

test_that("a beta zero is repaired when the predictor is transformed inline", {
  # The reproduction reported in the issue, on packaged data. herbicide records
  # one exact zero in fvfm for hexazinone, which check_data() shifts to 0.001;
  # before the fix brm() received the zero and refused it.
  hex <- herbicide[herbicide$herbicide == "hexazinone", ]
  bet <- Beta(link = "identity")
  inline <- brm_data(fvfm ~ crf(log(concentration), model = "nec3param"),
                     hex, bet)
  hex_pre <- hex
  hex_pre$concentration <- log(hex_pre$concentration)
  pre <- brm_data(fvfm ~ crf(concentration, model = "nec3param"),
                  hex_pre, bet)
  expect_false(any(inline$fvfm == 0))
  expect_equal(min(inline$fvfm), 0.001)
  expect_equal(inline$fvfm, pre$fvfm)
})

test_that("a beta one is repaired when the predictor is transformed inline", {
  d <- data.frame(x = rep(c(0.1, 1, 10, 100), each = 5),
                  y = rep(c(0.9, 0.6, 0.3, 0.1), each = 5))
  d$y[1] <- 1
  inline <- brm_data(y ~ crf(log(x), model = "nec3param"), d,
                     Beta(link = "identity"))
  expect_false(any(inline$y == 1))
  expect_equal(max(inline$y), 0.999)
})

test_that("the write-back still reaches brm() with no transformation at all", {
  gam <- Gamma(link = "identity")
  out <- brm_data(y ~ crf(x, model = "nec3param"), gamma_boundary_data(), gam)
  expect_equal(min(out$y), 0.3)
  # The predictor is never corrected, so a zero control reaches brm() as
  # recorded whether or not the response beside it was corrected (#269).
  d0 <- data.frame(x = rep(c(0, 1, 10, 100), each = 5),
                   y = rep(c(8, 6, 3, 1), each = 5))
  out0 <- brm_data(y ~ crf(x, model = "nec3param"), d0, gam)
  expect_identical(out0$x, d0$x)
})

test_that("the write-back aligns rows when incomplete cases were dropped", {
  d <- gamma_boundary_data()
  d$y[2] <- NA
  out <- brm_data(y ~ crf(x, model = "nec3param"), d, Gamma(link = "identity"))
  expect_equal(nrow(out), nrow(d))
  expect_true(is.na(out$y[2]))
  expect_equal(min(out$y, na.rm = TRUE), 0.3)
})

test_that("the write-back finds the right rows when row names are not 1:n", {
  d <- gamma_boundary_data()
  rownames(d) <- paste0("r", seq_len(nrow(d)))
  out <- brm_data(y ~ crf(x, model = "nec3param"), d, Gamma(link = "identity"))
  expect_equal(min(out$y), 0.3)
  expect_equal(unname(out$y[1:5]), rep(8, 5))
})

test_that("a boundary value on a transformed response is refused, not reported", {
  d <- gamma_boundary_data()
  d$y[d$y == 0] <- 1
  bdat <- model.frame(bnf(log(y) ~ crf(x, model = "nec3param")), data = d)
  expect_error(bayesnec:::check_data(bdat, Gamma(link = "identity"), "nec3param"),
               "a transformation written inside the model formula")
  # a Beta response carrying a one, the other boundary
  p <- data.frame(x = rep(c(0.1, 1, 10, 100), each = 5),
                  y = c(rep(c(0.81, 0.36, 0.09), each = 5), rep(0.04, 5)))
  p$y[1] <- 1
  bdat2 <- model.frame(bnf(sqrt(y) ~ crf(x, model = "nec3param")), data = p)
  expect_error(bayesnec:::check_data(bdat2, Beta(link = "identity"), "nec3param"),
               "a transformation written inside the model formula")
})

test_that("check_data() leaves the predictor alone, transformed or not", {
  d <- data.frame(x = rep(c(0, 1, 10, 100), each = 5),
                  y = rep(c(8, 6, 3, 1), each = 5))
  checked_x <- function(formula) {
    bdat <- model.frame(bnf(formula), data = d)
    bayesnec:::check_data(bdat, Gamma(link = "identity"),
                          "nec3param")$mod_dat$x
  }
  # A zero control is an ordinary observation: no family constrains the support
  # of a predictor, so there is nothing for check_data() to repair (#269).
  expect_identical(checked_x(y ~ crf(x, model = "nec3param")), d$x)
  expect_identical(checked_x(y ~ crf(sqrt(x), model = "nec3param")), sqrt(d$x))
})

test_that("a 0-1 bounded predictor reaches brm() with its bounds intact", {
  # The corrections #265 records as unreachable are removed rather than
  # repaired, so a proportion predictor keeps both a zero and a one (#269).
  d <- data.frame(x = rep(c(0, 0.25, 0.5, 1), each = 5),
                  y = rep(c(8, 6, 3, 1), each = 5))
  seen <- fit_data(y ~ crf(x, model = "nec3param"), d,
                   Gamma(link = "identity"))
  expect_identical(seen$x, d$x)
})

test_that("an integer predictor is still refused", {
  # check_data() no longer reads a distribution off the predictor, but the
  # set_distribution() call is kept for this error, which nothing else raises.
  d <- data.frame(x = rep(c(0L, 1L, 10L, 100L), each = 5),
                  y = rep(c(8, 6, 3, 1), each = 5))
  bdat <- model.frame(bnf(y ~ crf(x, model = "nec3param")), data = d)
  expect_error(bayesnec:::check_data(bdat, Gamma(link = "identity"),
                                     "nec3param"),
               "does not currently support integer concentration")
})

test_that("pop_var_is_transformed answers per variable", {
  d <- data.frame(x = rep(c(1, 10, 100), each = 4),
                  y = as.integer(rep(c(9, 5, 1), each = 4)),
                  n = as.integer(10))
  bdat <- model.frame(bnf(y | trials(n) ~ crf(log(x), model = "nec3param")),
                      data = d)
  expect_true(bayesnec:::pop_var_is_transformed(bdat, "x_var"))
  expect_false(bayesnec:::pop_var_is_transformed(bdat, "y_var"))
  # trials() wraps a variable without transforming it
  expect_false(bayesnec:::pop_var_is_transformed(bdat, "trials_var"))
  # a variable the formula does not carry is not transformed either
  expect_false(bayesnec:::pop_var_is_transformed(bdat, "rate_var"))
})

test_that("fit_bayesnec() hands brm() the repaired response", {
  # The integration point: the write_back_checks() calls in fit_bayesnec().
  # Removing the y_var call makes this fail and leaves every check_data()-level
  # assertion above passing.
  d <- gamma_boundary_data()
  seen <- fit_data(y ~ crf(log(x), model = "nec3param"), d,
                   Gamma(link = "identity"))
  expect_equal(min(seen$y), 0.3)
  expect_identical(seen$x, d$x)
})

test_that("fit_bayesnec() hands brm() the predictor as recorded", {
  d <- data.frame(x = rep(c(0, 1, 10, 100), each = 5),
                  y = rep(c(8, 6, 3, 1), each = 5))
  seen <- fit_data(y ~ crf(x, model = "nec3param"), d,
                   Gamma(link = "identity"))
  expect_identical(seen$x, d$x)
})

test_that("a binomial fit reaches brm() with its trials column untouched", {
  # There is no trials write-back, because check_data() never corrects trials.
  # The assertion is that the column arrives as recorded even where the model
  # frame has dropped an incomplete case, which is where the write-back this
  # replaced would have failed outright.
  d <- data.frame(x = rep(c(1, 10, 100), each = 4),
                  y = as.integer(rep(c(9, 5, 1), each = 4)),
                  n = as.integer(rep(10, 12)))
  d$y[2] <- NA
  seen <- fit_data(y | trials(n) ~ crf(log(x), model = "nec3param"), d,
                   binomial(link = "identity"))
  expect_identical(seen$n, d$n)
  expect_equal(nrow(seen), nrow(d))
  expect_true(is.na(seen$y[2]))
})

test_that("a model set stops once on the conflict, not once per model", {
  # check_data() runs per model, so leaving the check there alone printed the
  # paragraph for every member of the set and then ended on the generic
  # all-models-failed advice, long after the cause. bnec() raises it
  # before the model loop instead.
  d <- gamma_boundary_data()
  d$y[d$y == 0] <- 1
  msgs <- character(0)
  err <- tryCatch(
    withCallingHandlers(
      bnec(log(y) ~ crf(x, model = "decline"), data = d,
           family = Gamma(link = "identity")),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "a transformation written inside the model formula")
  # The all-models-failed paragraph is raised by stop(), not message(), so it
  # has to be asserted on the error rather than on what was collected above.
  expect_false(grepl("None of the models fit successfully", err))
  expect_length(msgs, 0)
})

test_that("an arithmetic trials term is not written over", {
  # clean_aterms() maps `trials(n * 2)` back to `n`, so the write-back this
  # change removes overwrote the user's `n` with the doubled values, and brm()
  # then evaluated `trials(n * 2)` against those: a recorded 10 became 40.
  d <- data.frame(x = rep(c(1, 10, 100, 1000), each = 4),
                  y = as.integer(rep(c(18, 12, 6, 2), each = 4)),
                  n = as.integer(rep(10, 16)))
  seen <- fit_data(y | trials(n * 2) ~ crf(x, model = "nec3param"), d,
                   binomial(link = "identity"))
  expect_identical(seen$n, d$n)
})
