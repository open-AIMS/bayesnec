# Check boundary-correction write-back at the mocked brm() call, through
# a model set, and through the data helpers. Predictor transformations
# must not discard response corrections or change recorded predictors.

gamma_boundary_data <- function() {
  d <- data.frame(x = rep(c(0.1, 1, 10, 100), each = 5))
  d$y <- c(rep(c(8, 6, 3), each = 5), rep(0, 5))
  d
}

# Mock brm() to inspect its input, not sampler behaviour. Supply init to
# bypass a stochastic search that can take minutes; mock chain validation
# because the fake fit has no chains.
fit_call <- function(formula, data, family, brm_args = list(),
                     predictor_scale = "auto") {
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    brm = function(formula, data, ...) {
      seen$data <- data
      seen$formula <- formula
      seen$prior <- list(...)$prior
      structure(list(), class = "brmsfit")
    },
    are_chains_correct = function(...) TRUE,
    .package = "bayesnec"
  )
  suppressMessages(suppressWarnings(
    bayesnec:::fit_bayesnec(
      formula = bnf(formula), data = data, model = "nec3param",
      brm_args = c(list(family = family, init = list(list()), chains = 1,
                        iter = 10), brm_args),
      predictor_scale = predictor_scale)
  ))
  seen
}

test_that("fit_bayesnec passes an explicit predictor scale to its prior (#317)", {
  x <- rep(log(c(1, 3, 10, 30, 100)), each = 4)
  d <- data.frame(x = x, y = seq(8, 2, length.out = length(x)))
  seen <- fit_call(y ~ crf(x, model = "nec3param"), d, gaussian(),
                   predictor_scale = "log")
  prior <- as.data.frame(seen$prior)

  expect_match(prior$prior[prior$nlpar == "nec"], "^normal\\(")
  # The declared scale decides the distribution. The bounds are the tested
  # series on the scale supplied.
  expect_equal(as.numeric(prior$lb[prior$nlpar == "nec"]), 0)
  expect_equal(as.numeric(prior$ub[prior$nlpar == "nec"]), log(100))
})

# The data frame alone, which is what most of the cases below assert on.
fit_data <- function(formula, data, family, brm_args = list()) {
  fit_call(formula, data, family, brm_args)$data
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
  # The herbicide fixture has one exact zero in hexazinone fvfm; check that
  # the Beta boundary correction reaches brm() as 0.001.
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
  # This helper checks response write-back only. Its predictor stays as
  # supplied; separate tests exercise predictor checking and fit_bayesnec().
})

test_that("an incomplete case is refused before the write-back", {
  # Reject missing rows before write-back. The separate non-default
  # row-name case checks alignment when no row has been dropped.
  d <- gamma_boundary_data()
  d$y[2] <- NA
  expect_error(brm_data(y ~ crf(x, model = "nec3param"), d,
                        Gamma(link = "identity")),
               "1 row\\(s\\) with missing values")
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
  # A zero predictor is valid data; response-family support does not
  # authorise altering predictor values.
  expect_identical(checked_x(y ~ crf(x, model = "nec3param")), d$x)
  expect_identical(checked_x(y ~ crf(sqrt(x), model = "nec3param")), sqrt(d$x))
})

test_that("a 0-1 bounded predictor keeps both of its bounds", {
  # Proportion predictors retain both boundary values.
  d <- data.frame(x = rep(c(0, 0.25, 0.5, 1), each = 5),
                  y = rep(c(8, 6, 3, 1), each = 5))
  bdat <- model.frame(bnf(y ~ crf(x, model = "nec3param")), data = d)
  checked <- bayesnec:::check_data(bdat, Gamma(link = "identity"), "nec3param")
  expect_identical(checked$mod_dat$x, d$x)
})

test_that("an integer predictor is still refused by check_data()", {
  # Keep set_distribution() here to reject integer predictors before prior
  # construction. Testing check_data() specifically protects that ordering.
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

test_that("a disp() sub-model is built from the predictor as recorded", {
  # disp(~log(x)) reaches brm() with the recorded zero still present.
  # Inspect both formula and data; this mock does not test Stan behaviour.
  d <- data.frame(x = rep(c(0, 1, 10, 100), each = 5),
                  y = rep(c(8, 6, 3, 1), each = 5))
  seen <- fit_call(y ~ crf(x, model = "nec3param") + disp(~log(x)), d,
                   Gamma(link = "identity"))
  # Both halves are asserted: that the sub-model is present as written, and
  # that the column it will be evaluated against still holds the zero. The
  # second alone would pass with the disp term dropped.
  expect_true(any(grepl("shape ~ log(x)",
                        vapply(seen$formula$pforms, deparse1, character(1)),
                        fixed = TRUE)))
  expect_identical(seen$data$x, d$x)
})

test_that("a binomial fit reaches brm() with its trials column untouched", {
  # Trials are never boundary-corrected. Preserve their recorded column
  # rather than writing an evaluated additional term back into it.
  d <- data.frame(x = rep(c(1, 10, 100), each = 4),
                  y = as.integer(rep(c(9, 5, 1), each = 4)),
                  n = as.integer(rep(10, 12)))
  seen <- fit_data(y | trials(n) ~ crf(log(x), model = "nec3param"), d,
                   binomial(link = "identity"))
  expect_identical(seen$n, d$n)
  expect_equal(nrow(seen), nrow(d))
  expect_identical(seen$y, d$y)
})

test_that("an incomplete case is refused for a binomial fit too", {
  # Missing trials are refused just like missing responses; either would
  # otherwise cause model.frame() to drop a population-data row.
  d <- data.frame(x = rep(c(1, 10, 100), each = 4),
                  y = as.integer(rep(c(9, 5, 1), each = 4)),
                  n = as.integer(rep(10, 12)))
  d$n[3] <- NA
  expect_error(
    fit_data(y | trials(n) ~ crf(log(x), model = "nec3param"), d,
             binomial(link = "identity")),
    "1 row\\(s\\) with missing values"
  )
})

test_that("a model set stops once on the conflict, not once per model", {
  # Reject the model-set conflict before looping, so callers receive one
  # specific error rather than repeated messages and an all-models-failed error.
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
  # Writing evaluated trials(n * 2) back to n would double twice:
  # a recorded 10 would reach brm() as 40.
  d <- data.frame(x = rep(c(1, 10, 100, 1000), each = 4),
                  y = as.integer(rep(c(18, 12, 6, 2), each = 4)),
                  n = as.integer(rep(10, 16)))
  seen <- fit_data(y | trials(n * 2) ~ crf(x, model = "nec3param"), d,
                   binomial(link = "identity"))
  expect_identical(seen$n, d$n)
})


# Group-level terms

grouped_data <- function() {
  set.seed(1)
  d <- data.frame(x = rep(c(0.1, 1, 3, 10), each = 6),
                  g = factor(rep(c("a", "b", "c"), 8)))
  d$y <- 5 * exp(-0.3 * d$x) * stats::rgamma(24, shape = 20, rate = 20)
  d
}

test_that("fit_bayesnec() refuses an equation with no grouping term (#470)", {
  # The backstop for bnec_joint() on a bayesnecgroupfit, which fits through
  # fit_bayesnec() directly. Refused before brm() is reached.
  calls <- 0L
  local_mocked_bindings(
    brm = function(...) {
      calls <<- calls + 1L
      stop("brm reached")
    },
    .package = "bayesnec"
  )
  expect_error(
    suppressMessages(bayesnec:::fit_bayesnec(
      formula = bnf(y ~ crf(x, "ecxexp") + (nec | g) + (ec50 | g)),
      data = grouped_data(), model = "ecxexp",
      brm_args = list(family = Gamma(link = "identity"), init = list(list()),
                      chains = 1, iter = 10)
    )),
    "No equation requested can be fitted", fixed = TRUE
  )
  expect_identical(calls, 0L)
})

test_that("an equation the rule admits stores every grouping variable (#470)", {
  # empty = TRUE builds a brmsfit without compilation or sampling. Keep
  # the grouping column in its data so the written formula builds the grid.
  local_mocked_bindings(
    brm = function(...) {
      args <- list(...)
      args$empty <- TRUE
      do.call(brms::brm, args)
    },
    are_chains_correct = function(...) TRUE,
    .package = "bayesnec"
  )
  f <- y ~ crf(x, "all") + (nec | g) + (ec50 | g)
  for (m in c("nec3param", "ecxll3")) {
    out <- suppressMessages(suppressWarnings(bayesnec:::fit_bayesnec(
      formula = bnf(f), data = grouped_data(), model = m,
      brm_args = list(family = Gamma(link = "identity"), init = list(list()),
                      chains = 1, iter = 10)
    )))
    expect_true("g" %in% names(out$fit$data), info = m)
    expect_setequal(labels(terms(out$bayesnecformula)),
                    c("nec | g", "ec50 | g",
                      paste0("crf(x, model = \"", m, "\")")))
    grid <- prediction_grid(out$fit, out$bayesnecformula)
    expect_identical(dim(grid$newdata), c(1000L, 1L), info = m)
  }
})
