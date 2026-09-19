# check_response_flattened() reports, before anything is fitted, that the
# response is still declining between the two highest concentrations tested.
# The rule replaces the decline-from-the-control ratio of PR #387, which
# measured the size of the effect rather than whether the series had flattened.
# See #390 and notes/tasks/386-incomplete-designs-claude.md section 2.2.
#
# Nothing here fits a model, so no Stan program is compiled.

set.seed(390)

# Helper: build the model frame the check expects, as bnec() does.
flat_bdat <- function(formula, data) {
  model.frame(bayesnecformula(formula), data = data, run_par_checks = TRUE)
}

flat_x <- rep(c(0, 1, 2, 4), each = 4)
# The two highest levels differ: the response has not flattened.
declining_y <- c(1.02, 0.98, 1.01, 0.99,
                 0.90, 0.92, 0.88, 0.91,
                 0.80, 0.83, 0.78, 0.81,
                 0.60, 0.62, 0.58, 0.61)
# The two highest levels agree: the response has flattened.
flattened_y <- c(1.02, 0.98, 1.01, 0.99,
                 0.90, 0.92, 0.88, 0.91,
                 0.80, 0.83, 0.78, 0.81,
                 0.79, 0.82, 0.80, 0.81)

declining_counts <- c(40, 42, 38, 41,
                      30, 32, 28, 31,
                      20, 22, 19, 21,
                      8, 10, 7, 9)
flattened_counts <- c(40, 42, 38, 41,
                      30, 32, 28, 31,
                      20, 22, 19, 21,
                      21, 19, 22, 20)

declining_successes <- c(19, 20, 19, 20,
                         16, 15, 17, 16,
                         11, 10, 12, 11,
                         4, 5, 3, 4)
flattened_successes <- c(19, 20, 19, 20,
                         16, 15, 17, 16,
                         11, 10, 12, 11,
                         12, 10, 11, 11)

still_declining <- "is still declining at the top of the series"

test_that("a gaussian response is reported where its top two levels differ", {
  bdat <- flat_bdat(y ~ crf(x, "nec4param"),
                    data.frame(x = flat_x, y = declining_y))
  expect_message(
    check_response_flattened(bdat, gaussian(link = "identity")),
    still_declining
  )
  # The means and the levels compared are named, so that the report can be
  # checked against the data without recomputing it.
  expect_message(
    check_response_flattened(bdat, gaussian(link = "identity")),
    "falls from 0.805 at 2 to 0.602 at 4"
  )
  flat <- flat_bdat(y ~ crf(x, "nec4param"),
                    data.frame(x = flat_x, y = flattened_y))
  expect_silent(check_response_flattened(flat, gaussian(link = "identity")))
})

test_that("a high lower asymptote is not reported", {
  # The case PR #387's decline ratio warned on and this rule does not: a
  # sublethal endpoint whose response has fallen by a fifth and then flattened
  # is a complete design.
  y <- c(1.02, 0.98, 1.01, 0.99,
         0.90, 0.92, 0.88, 0.91,
         0.81, 0.83, 0.80, 0.82,
         0.80, 0.82, 0.81, 0.81)
  bdat <- flat_bdat(y ~ crf(x, "nec4param"), data.frame(x = flat_x, y = y))
  expect_silent(check_response_flattened(bdat, gaussian(link = "identity")))
})

test_that("a response that rises at the top is not reported", {
  y <- rev(declining_y)
  bdat <- flat_bdat(y ~ crf(x, "nec4param"), data.frame(x = flat_x, y = y))
  expect_silent(check_response_flattened(bdat, gaussian(link = "identity")))
})

test_that("the Gamma row is fitted on the log link", {
  # stats::Gamma() defaults to the inverse link, which reverses the sign of the
  # contrast and would report a declining response as a rising one.
  expect_identical(flatness_spec("Gamma")$family$link, "log")
  expect_identical(flatness_spec("poisson")$family$link, "log")
  expect_identical(flatness_spec("negbinomial")$family$family, "quasipoisson")
  bdat <- flat_bdat(y ~ crf(x, "nec3param"),
                    data.frame(x = flat_x, y = declining_y * 10))
  expect_message(
    check_response_flattened(bdat, Gamma(link = "identity")),
    still_declining
  )
  flat <- flat_bdat(y ~ crf(x, "nec3param"),
                    data.frame(x = flat_x, y = flattened_y * 10))
  expect_silent(check_response_flattened(flat, Gamma(link = "identity")))
})

test_that("the count families are assessed under a quasipoisson variance", {
  bdat <- flat_bdat(y ~ crf(x, "nec3param"),
                    data.frame(x = flat_x, y = declining_counts))
  flat <- flat_bdat(y ~ crf(x, "nec3param"),
                    data.frame(x = flat_x, y = flattened_counts))
  for (family in list(poisson(link = "identity"),
                      brms::negbinomial(link = "identity"))) {
    expect_message(check_response_flattened(bdat, family), still_declining)
    expect_silent(check_response_flattened(flat, family))
  }
})

test_that("the zero-inflated count families are assessed whole", {
  # They are not two-block families here: zi is held constant and no curve is
  # fitted on it, so the check runs on the whole response.
  zi_y <- declining_counts
  zi_y[c(2, 7)] <- 0
  bdat <- flat_bdat(y ~ crf(x, "nec3param"),
                    data.frame(x = flat_x, y = zi_y))
  blocks <- flatness_blocks(flat_x, zi_y, NULL, NULL,
                            "zero_inflated_poisson")
  expect_named(blocks, "response")
  expect_message(
    check_response_flattened(
      bdat, brms::zero_inflated_poisson(link = "identity")
    ),
    still_declining
  )
})

test_that("a binomial response is assessed on its counts", {
  d <- data.frame(x = flat_x, succ = declining_successes, tr = 20)
  bdat <- flat_bdat(succ | trials(tr) ~ crf(x, "nec3param"), d)
  d_flat <- data.frame(x = flat_x, succ = flattened_successes, tr = 20)
  flat <- flat_bdat(succ | trials(tr) ~ crf(x, "nec3param"), d_flat)
  for (family in list(binomial(link = "identity"),
                      brms::beta_binomial(link = "identity"))) {
    expect_message(check_response_flattened(bdat, family), still_declining)
    expect_silent(check_response_flattened(flat, family))
  }
  # The proportion is reported, not the count.
  expect_message(
    check_response_flattened(bdat, binomial(link = "identity")),
    "falls from 0.55 at 2 to 0.2 at 4"
  )
})

test_that("a bernoulli response is assessed with the dispersion fixed", {
  # One trial per observation leaves no dispersion to estimate.
  expect_identical(flatness_spec("bernoulli")$family$family, "binomial")
  x <- rep(c(0, 1, 2, 4), each = 10)
  y <- c(rep(1, 10), c(rep(1, 8), 0, 0), c(rep(1, 5), rep(0, 5)),
         c(1, rep(0, 9)))
  bdat <- flat_bdat(y ~ crf(x, "nec3param"), data.frame(x = x, y = y))
  expect_message(
    check_response_flattened(bdat, brms::bernoulli(link = "identity")),
    still_declining
  )
  y_flat <- c(rep(1, 10), c(rep(1, 8), 0, 0), c(rep(1, 5), rep(0, 5)),
              c(rep(1, 5), rep(0, 5)))
  flat <- flat_bdat(y ~ crf(x, "nec3param"), data.frame(x = x, y = y_flat))
  expect_silent(
    check_response_flattened(flat, brms::bernoulli(link = "identity"))
  )
})

test_that("a beta response is assessed under a quasibinomial variance", {
  # Var = mu (1 - mu) / (1 + phi) is mu (1 - mu) times a constant, which is
  # what quasibinomial fits, so no beta regression is needed.
  expect_identical(flatness_spec("beta")$family$family, "quasibinomial")
  expect_identical(flatness_spec("beta")$kind, "plain")
  bdat <- flat_bdat(y ~ crf(x, "nec3param"),
                    data.frame(x = flat_x, y = declining_y * 0.9))
  expect_message(
    check_response_flattened(bdat, brms::Beta(link = "identity")),
    still_declining
  )
  flat <- flat_bdat(y ~ crf(x, "nec3param"),
                    data.frame(x = flat_x, y = flattened_y * 0.9))
  expect_silent(
    check_response_flattened(flat, brms::Beta(link = "identity"))
  )
})

test_that("a rate() contrast is taken on the rate scale", {
  # The counts rise at the highest concentration and the rate falls, because
  # the exposure doubled. The denominator enters as offset(log(denominator)),
  # so the contrast follows the rate.
  d <- data.frame(
    x = flat_x,
    count = c(100, 110, 95, 105, 80, 85, 75, 82, 30, 28, 32, 30,
              30, 34, 28, 32),
    expo = c(rep(10, 12), rep(20, 4))
  )
  bdat <- flat_bdat(count | rate(expo) ~ crf(x, "nec3param"), d)
  expect_message(
    check_response_flattened(bdat, poisson(link = "identity")),
    "falls from 3 at 2 to 1.55 at 4"
  )
  # The same counts with the denominator held at 1 are rising, so the contrast
  # would report nothing. The difference is the offset and nothing else.
  d_flat <- d
  d_flat$expo <- 1
  bdat_flat <- flat_bdat(count | rate(expo) ~ crf(x, "nec3param"), d_flat)
  expect_silent(
    check_response_flattened(bdat_flat, poisson(link = "identity"))
  )
})

test_that("a hormesis design is assessed against the top of its own series", {
  # The response rises above the control before it falls. The rule never reads
  # the control, so the peak cannot be mistaken for it.
  y <- c(1.00, 1.02, 0.98, 1.01,
         1.38, 1.42, 1.40, 1.39,
         0.30, 0.32, 0.28, 0.31,
         0.29, 0.31, 0.30, 0.30)
  bdat <- flat_bdat(y ~ crf(x, "nechorme"), data.frame(x = flat_x, y = y))
  expect_silent(check_response_flattened(bdat, gaussian(link = "identity")))
  y[13:16] <- c(0.18, 0.20, 0.17, 0.19)
  bdat <- flat_bdat(y ~ crf(x, "nechorme"), data.frame(x = flat_x, y = y))
  expect_message(
    check_response_flattened(bdat, gaussian(link = "identity")),
    still_declining
  )
})

test_that("a response with a non-positive control mean is still assessed", {
  # A log-ratio or centred response, for which the PR #387 decline ratio was
  # undefined and the check was skipped silently (#229).
  y <- declining_y - 1
  bdat <- flat_bdat(y ~ crf(x, "nec4param"), data.frame(x = flat_x, y = y))
  expect_message(
    check_response_flattened(bdat, gaussian(link = "identity")),
    still_declining
  )
})

# ---- the two blocks of a hurdle fit ----------------------------------------

hurdle_frame <- function(x, y) {
  flat_bdat(y ~ crf(x, "nec3param"), data.frame(x = x, y = y))
}

hurdle_family <- brms::hurdle_gamma(link = "identity", link_hu = "identity")

test_that("a hurdle survival block still falling is reported by name", {
  # The commonest incomplete design there is: a dilution series whose survival
  # is still falling at the undiluted end. There is one proportion per
  # concentration, so no rule built on replication within a level applies.
  x <- rep(c(0, 1, 2, 4), each = 12)
  alive <- c(rep(1, 12), c(rep(1, 11), 0), c(rep(1, 8), rep(0, 4)),
             c(rep(1, 2), rep(0, 10)))
  y <- ifelse(alive == 1, rep(c(5.0, 5.2, 4.9, 5.1), length.out = 48), 0)
  bdat <- hurdle_frame(x, y)
  msg <- capture_messages(check_response_flattened(bdat, hurdle_family))
  expect_length(grep("The survival block is still declining", msg), 1)
  expect_length(grep("The response block is still declining", msg), 0)
})

test_that("a hurdle response block still falling is reported by name", {
  x <- rep(c(0, 1, 2, 4), each = 12)
  alive <- rep(c(rep(1, 8), rep(0, 4)), 4)
  y <- numeric(length(x))
  y[alive == 1] <- rep(c(10.0, 10.4, 9.8, 10.2, 8.0, 8.3, 7.8, 8.1,
                         6.0, 6.2, 5.8, 6.1, 3.0, 3.2, 2.8, 3.1),
                       length.out = sum(alive))
  bdat <- hurdle_frame(x, y)
  msg <- capture_messages(check_response_flattened(bdat, hurdle_family))
  expect_length(grep("The response block is still declining", msg), 1)
  expect_length(grep("The survival block is still declining", msg), 0)
})

test_that("a hurdle response block is assessed against its own two levels", {
  # Nothing survived the highest concentration, so the response block has no
  # observations there and its two highest levels are not the design's.
  x <- rep(c(0, 1, 2, 4), each = 10)
  alive <- c(rep(1, 30), rep(0, 10))
  y <- numeric(length(x))
  y[1:10] <- c(10.0, 10.4, 9.8, 10.2, 10.1, 9.9, 10.3, 9.7, 10.0, 10.2)
  y[11:20] <- c(8.0, 8.3, 7.8, 8.1, 8.2, 7.9, 8.4, 7.7, 8.0, 8.1)
  y[21:30] <- c(3.0, 3.2, 2.8, 3.1, 3.3, 2.9, 3.4, 2.7, 3.0, 3.1)
  bdat <- hurdle_frame(x, y)
  msg <- capture_messages(check_response_flattened(bdat, hurdle_family))
  # The levels named for the response block are 1 and 2, not 2 and 4.
  expect_length(grep("The response block .*at 1 to .*at 2", msg), 1)
  expect_length(grep("The survival block .*at 2 to .*at 4", msg), 1)
})

test_that("the hurdle survival block reads the raw counts", {
  # survival_by_x() clamps its proportions to [eps, 1 - eps] so that Stan can
  # fit them under an identity link, and those values are not the observed
  # proportions where any were 0 or 1.
  x <- rep(c(0, 1, 2, 4), each = 10)
  y <- c(rep(2, 10), rep(2, 10), rep(2, 10), rep(0, 10))
  blocks <- flatness_blocks(x, y, NULL, NULL, hurdle_family)
  expect_identical(blocks$survival$successes, c(10, 10, 10, 0))
  expect_identical(blocks$survival$trials, c(10, 10, 10, 10))
  observed <- blocks$survival$successes / blocks$survival$trials
  expect_false(any(observed %in% survival_by_x(x, y)$y))
  # Complete separation, which is where the Wald statistic summary.glm() prints
  # reports p near a half and the analysis of deviance does not.
  result <- flatness_contrast(blocks$survival)
  expect_identical(result$status, "tested")
  expect_true(result$declining)
  expect_lt(result$p_value, 1e-4)
})

# ---- edge cases -------------------------------------------------------------

test_that("fewer than two predictor levels is passed over in silence", {
  bdat <- flat_bdat(y ~ crf(x, "nec4param"),
                    data.frame(x = rep(1, 8), y = declining_y[1:8]))
  expect_silent(check_response_flattened(bdat, gaussian(link = "identity")))
})

test_that("an unreplicated design reports that it could not be assessed", {
  bdat <- flat_bdat(y ~ crf(x, "nec4param"),
                    data.frame(x = c(0, 1, 2, 4),
                               y = c(1, 0.9, 0.8, 0.6)))
  expect_message(
    check_response_flattened(bdat, gaussian(link = "identity")),
    "could not"
  )
})

test_that("a response with no variation reports that it could not be ", {
  y <- rep(c(1, 0.9, 0.8, 0.8), each = 4)
  bdat <- flat_bdat(y ~ crf(x, "nec4param"),
                    data.frame(x = flat_x, y = y))
  expect_message(
    check_response_flattened(bdat, gaussian(link = "identity"),
                             pool_dispersion = FALSE),
    "could not"
  )
})

test_that("a gaussian dispersion is pooled or local as the fit's own is", {
  # bnec() fits a single sigma across the series unless a disp() term is
  # supplied, so pooling is the assumption the fit itself makes. With a disp()
  # term the two levels supply their own.
  y <- declining_y
  y[1:8] <- c(1.30, 0.70, 1.25, 0.75, 1.20, 0.60, 1.15, 0.65)
  bdat <- flat_bdat(y ~ crf(x, "nec4param"), data.frame(x = flat_x, y = y))
  blocks <- flatness_blocks(flat_x, y, NULL, NULL, gaussian())
  pooled <- flatness_contrast(blocks$response, pool_dispersion = TRUE)
  local <- flatness_contrast(blocks$response, pool_dispersion = FALSE)
  # The low levels are noisy and the top two are not, so pooling their variance
  # gives the larger p-value of the two.
  expect_gt(pooled$p_value, local$p_value)
  expect_silent(check_response_flattened(bdat, gaussian(link = "identity")))
  expect_message(
    check_response_flattened(bdat, gaussian(link = "identity"),
                             pool_dispersion = FALSE),
    still_declining
  )
})

test_that("a family with no row is passed over", {
  expect_null(flatness_spec("weibull"))
  expect_identical(flatness_blocks(flat_x, declining_y, NULL, NULL, "weibull"),
                   list())
})

test_that("a binomial family with no trials term is passed over", {
  # check_data() refuses it, and does so after this report, so nothing is said
  # here and the refusal is what the user sees.
  expect_identical(
    flatness_blocks(flat_x, declining_successes, NULL, NULL,
                    binomial(link = "identity")),
    list()
  )
})

# ---- the grouped route ------------------------------------------------------

test_that("each level of a grouped call is assessed on its own", {
  d <- data.frame(
    x = rep(flat_x, 2),
    y = c(declining_y, flattened_y),
    site = rep(c("incomplete", "complete"), each = 16)
  )
  bdat <- flat_bdat(y ~ crf(x, "nec4param"), d)
  msg <- capture_messages(
    check_response_flattened(bdat, gaussian(link = "identity"),
                             group = d$site)
  )
  expect_length(grep("level \"incomplete\"", msg), 1)
  expect_length(grep("level \"complete\"", msg), 0)
})

test_that("a per-level block list is honoured", {
  d <- data.frame(
    x = rep(flat_x, 2),
    y = c(declining_y, declining_y),
    site = rep(c("a", "b"), each = 16)
  )
  bdat <- flat_bdat(y ~ crf(x, "nec4param"), d)
  msg <- capture_messages(
    check_response_flattened(
      bdat, gaussian(link = "identity"), group = d$site,
      blocks = list(a = "response", b = character(0))
    )
  )
  expect_length(grep("level \"a\"", msg), 1)
  expect_length(grep("level \"b\"", msg), 0)
})

# ---- the priors that make the report relevant -------------------------------

test_that("a complete prior for every sensitive row silences the report", {
  nec_prior <- brms::prior_string("normal(1, 1)", nlpar = "nec")
  bot_prior <- brms::prior_string("normal(0, 1)", nlpar = "bot")
  expect_identical(
    uses_response_range_defaults(NULL, "nec4param",
                                 gaussian(link = "identity")),
    c(response = TRUE)
  )
  expect_identical(
    uses_response_range_defaults(nec_prior + bot_prior, "nec4param",
                                 gaussian(link = "identity")),
    c(response = FALSE)
  )
  # A partial prior does not silence it: fill_missing_priors() fills the
  # omitted rows from the same defaults.
  expect_identical(
    uses_response_range_defaults(nec_prior, "nec4param",
                                 gaussian(link = "identity")),
    c(response = TRUE)
  )
  # An equation estimating none of bot, nec or ec50 is not sensitive.
  expect_identical(
    uses_response_range_defaults(NULL, c("ecxlin", "ecxexp", "ecxsigm"),
                                 gaussian(link = "identity")),
    c(response = FALSE)
  )
})

test_that("the two blocks of a hurdle fit are silenced separately", {
  response_prior <- brms::prior_string("normal(1, 1)", nlpar = "nec")
  survival_prior <- brms::prior_string("normal(1, 1)", nlpar = "hunec")
  expect_identical(
    uses_response_range_defaults(response_prior, "nec3param", hurdle_family),
    c(response = FALSE, survival = TRUE)
  )
  expect_identical(
    uses_response_range_defaults(survival_prior, "nec3param", hurdle_family),
    c(response = TRUE, survival = FALSE)
  )
})

# ---- placement --------------------------------------------------------------

test_that("the report is raised once per bnec() call, not once per equation", {
  d <- data.frame(x = flat_x, y = declining_y)
  local_mocked_bindings(
    fit_bayesnec = function(...) stop("mock fit"),
    .package = "bayesnec"
  )
  msg <- capture_messages(
    expect_error(
      bnec(y ~ crf(x, c("nec3param", "nec4param", "necsigm")), d,
           family = gaussian(link = "identity")),
      "None of the model"
    )
  )
  expect_length(grep(still_declining, msg), 1)
})

test_that("a complete prior for every equation silences the bnec() report", {
  d <- data.frame(x = flat_x, y = declining_y)
  nec_prior <- brms::prior_string("normal(1, 1)", nlpar = "nec")
  bot_prior <- brms::prior_string("normal(0, 1)", nlpar = "bot")
  local_mocked_bindings(
    fit_bayesnec = function(...) stop("mock fit"),
    .package = "bayesnec"
  )
  msg <- capture_messages(
    expect_error(
      bnec(y ~ crf(x, "nec4param"), d, family = gaussian(link = "identity"),
           prior = nec_prior + bot_prior),
      "mock fit"
    )
  )
  expect_length(grep(still_declining, msg), 0)
})

test_that("bnec_group reports every level once and marks the inner calls", {
  d <- data.frame(
    x = rep(flat_x, 2),
    y = c(declining_y, declining_y),
    site = rep(c("a", "b"), each = 16)
  )
  marked <- logical(0)
  local_mocked_bindings(
    bnec = function(...) {
      marked <<- c(marked, isTRUE(list(...)[[".bayesnec_flatness_checked"]]))
      list()
    },
    .package = "bayesnec"
  )
  msg <- capture_messages(
    bnec_group(y ~ crf(x, "nec4param"), d, group_var = "site",
               family = gaussian(link = "identity"))
  )
  # Both levels are named, in one message raised before either was fitted.
  expect_length(grep(still_declining, msg), 1)
  expect_length(grep("level \"a\".*\n.*level \"b\"", msg), 1)
  expect_identical(marked, c(TRUE, TRUE))
})

# ---- calibration ------------------------------------------------------------

test_that("the report rate on a flat top is near the stated alpha", {
  # On a design whose top is flat the true contrast is zero, so the rule
  # reports at alpha by construction. This is a small self-contained check that
  # the realised rate is of that order; the audit over the incomplete-design
  # cells is #391.
  skip_on_cran()
  set.seed(3901)
  reps <- 500
  x <- rep(c(0, 1, 2, 4, 8), each = 4)
  mu <- rep(c(1, 0.7, 0.4, 0.25, 0.25), each = 4)
  spec <- flatness_spec("gaussian")
  rate <- function(generate, pool) {
    mean(vapply(seq_len(reps), function(i) {
      result <- flatness_contrast(generate(), alpha = 0.05,
                                  pool_dispersion = pool)
      isTRUE(result$declining)
    }, logical(1)))
  }
  gaussian_block <- function() {
    list(x = x, y = rnorm(length(x), mu, 0.1), spec = spec)
  }
  # Two standard errors of a 500-replicate proportion at 0.05 is 0.019.
  expect_lt(rate(gaussian_block, TRUE), 0.1)
  expect_lt(rate(gaussian_block, FALSE), 0.1)
  binomial_spec <- flatness_spec("binomial")
  p <- c(1, 0.7, 0.4, 0.25, 0.25)
  survival_block <- function() {
    list(x = c(0, 1, 2, 4, 8), successes = rbinom(5, 20, p),
         trials = rep(20, 5),
         spec = list(family = binomial(), kind = "matrix", offset = FALSE))
  }
  expect_lt(rate(survival_block, TRUE), 0.1)
  # And the rule has power where the top is genuinely still falling.
  mu_declining <- rep(c(1, 0.7, 0.5, 0.35, 0.2), each = 4)
  declining_block <- function() {
    list(x = x, y = rnorm(length(x), mu_declining, 0.1), spec = spec)
  }
  expect_gt(rate(declining_block, TRUE), 0.5)
})
