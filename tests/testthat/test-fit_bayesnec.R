# #258: check_data() corrects a response or predictor sitting on a boundary
# its family cannot represent, and fit_bayesnec() writes that correction into
# the data frame brm() is handed. The write-back was decided for the formula as
# a whole, so a transformation written inline in crf() -- on any population
# variable -- discarded every correction, and brm() then failed naming the
# condition the package had just reported it had repaired.
#
# The assertions below run check_data() and write_back_checks() in the order
# fit_bayesnec() runs them, which is the whole of the decision under test.
# fit_bayesnec() itself is not driven, because everything it does after the
# write-back -- building priors, drawing initial values -- is stochastic and
# slow, and none of it bears on which values reach brm().

gamma_boundary_data <- function() {
  d <- data.frame(x = rep(c(0.1, 1, 10, 100), each = 5))
  d$y <- c(rep(c(8, 6, 3), each = 5), rep(0, 5))
  d
}

# The data frame brm() would be given: the user's own data frame carrying
# whatever check_data() repaired. fit_bayesnec() hands brm() exactly this
# object.
brm_data <- function(formula, data, family) {
  bdat <- model.frame(bnf(formula), data = data)
  checked <- suppressMessages(bayesnec:::check_data(bdat, family, "nec3param"))
  data <- bayesnec:::write_back_checks(data, bdat, "y_var", checked$mod_dat$y)
  bayesnec:::write_back_checks(data, bdat, "x_var", checked$mod_dat$x)
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
  # the predictor zero shift is unchanged where the predictor is a bare column
  d0 <- data.frame(x = rep(c(0, 1, 10, 100), each = 5),
                   y = rep(c(8, 6, 3, 1), each = 5))
  out0 <- brm_data(y ~ crf(x, model = "nec3param"), d0, gam)
  expect_equal(min(out0$x), 0.1)
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

test_that("a zero on a transformed predictor is left where it is", {
  d <- data.frame(x = rep(c(0, 1, 10, 100), each = 5),
                  y = rep(c(8, 6, 3, 1), each = 5))
  min_x <- function(formula) {
    bdat <- model.frame(bnf(formula), data = d)
    min(bayesnec:::check_data(bdat, Gamma(link = "identity"), "nec3param")$mod_dat$x)
  }
  # sqrt(0) is zero, but so is log(1): a zero on the transformed scale is not
  # evidence of a boundary artefact on the recorded scale, and the shift could
  # not reach the fit in any case.
  expect_equal(min_x(y ~ crf(sqrt(x), model = "nec3param")), 0)
  expect_equal(min_x(y ~ crf(x, model = "nec3param")), 0.1)
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
