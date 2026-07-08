# Regression tests for GitHub issue #162
# beta_binomial + identity link: initial values must stay within (0, 1)

# -- Shared test data (issue #162 reprex) ------------------------------------

dat_real <- structure(
  list(
    mgL = c(
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      4.094250037, 4.094250037, 4.094250037, 4.094250037, 4.094250037,
      6.896245775, 6.896245775, 6.896245775, 6.896245775, 6.896245775,
      12.75631805, 12.75631805, 12.75631805, 12.75631805, 12.75631805,
      23.23869006, 23.23869006, 23.23869006, 23.23869006, 23.23869006,
      35.13868903, 35.13868903, 35.13868903, 35.13868903, 35.13868903,
      57.21608294, 57.21608294, 57.21608294, 57.21608294, 57.21608294,
      80.27771977, 80.27771977, 80.27771977, 80.27771977, 80.27771977,
      153.6317617, 153.6317617, 153.6317617, 153.6317617, 153.6317617,
      232.3389074, 232.3389074, 232.3389074, 232.3389074, 232.3389074,
      311.490013,  311.490013,  311.490013,  311.490013,  311.490013
    ),
    prop = c(
      0.911, 0.948, 0.967, 0.99, 0.953,
      0.913, 0.879, 0.926, 0.969, 0.927,
      0.973, 0.927, 0.957, 0.936, 0.97,
      0.946, 0.906, 0.927, 0.919, 0.829,
      0.964, 0.95,  0.918, 0.907, 0.971,
      0.933, 0.943, 0.971, 0.943, 0.934,
      0.831, 0.96,  0.935, 0.926, 0.844,
      0.968, 0.982, 0.945, 0.939, 0.964,
      0.856, 0.901, 0.851, 0.747, 0.917,
      0.626, 0.678, 0.832, 0.83,  0.596,
      0,     0.059, 0.064, 0,     0.056,
      0.011, 0,     0.011, 0,     0
    ),
    trials = c(
      90L, 96L, 91L, 105L, 86L, 103L, 99L, 95L, 98L, 110L,
      113L, 96L, 92L, 78L, 99L, 93L, 96L, 96L, 86L, 82L,
      111L, 101L, 85L, 118L, 102L, 104L, 105L, 105L, 106L, 91L,
      83L, 101L, 93L, 95L, 90L, 95L, 109L, 110L, 114L, 112L,
      90L, 91L, 101L, 95L, 96L, 91L, 90L, 95L, 106L, 94L,
      94L, 68L, 94L, 94L, 107L, 94L, 93L, 89L, 105L, 99L
    )
  ),
  class = "data.frame",
  row.names = c(NA, -60L)
)
dat_real$y <- as.integer(round(dat_real$prop * dat_real$trials))
dat_real$log.x <- log(dat_real$mgL + 0.1)

bb_family    <- beta_binomial(link = "identity")
bb_response  <- dat_real$y / dat_real$trials
bb_predictor <- dat_real$log.x

# -- response_link_scale unit tests ------------------------------------------

test_that("response_link_scale clamps identity-link beta_binomial away from 0 and 1", {
  response <- c(0, 0, 0.05, 0.5, 0.9, 0.95, 0.97, 1.0)
  family <- beta_binomial(link = "identity")
  result <- bayesnec:::response_link_scale(response, family)

  expect_true(all(result > 0),
              info = "All values should be strictly > 0 after clamping")
  expect_true(all(result < 1),
              info = "All values should be strictly < 1 after clamping")
})

test_that("response_link_scale clamps identity-link binomial away from 0 and 1", {
  response <- c(0, 0.1, 0.5, 0.9, 1.0)
  family <- binomial(link = "identity")
  result <- bayesnec:::response_link_scale(response, family)

  expect_true(all(result > 0))
  expect_true(all(result < 1))
})

test_that("response_link_scale clamps identity-link beta away from 0 and 1", {
  response <- c(0, 0.3, 0.7, 1.0)
  family <- Beta(link = "identity")
  result <- bayesnec:::response_link_scale(response, family)

  expect_true(all(result > 0))
  expect_true(all(result < 1))
})

test_that("response_link_scale is identity for identity-link families without boundary values", {
  response <- c(0.2, 0.4, 0.6, 0.8)
  family <- beta_binomial(link = "identity")
  result <- bayesnec:::response_link_scale(response, family)

  expect_equal(result, response,
               info = "No clamping needed when response is already in (0, 1)")
})

test_that("response_link_scale only-zeros edge case stays in (0, 1)", {
  response <- c(0, 0, 0, 0.01, 0.02)
  family <- beta_binomial(link = "identity")
  result <- bayesnec:::response_link_scale(response, family)

  expect_true(all(result > 0))
  expect_true(all(result < 1))
})

test_that("response_link_scale only-ones edge case stays in (0, 1)", {
  response <- c(0.98, 0.99, 1.0, 1.0)
  family <- beta_binomial(link = "identity")
  result <- bayesnec:::response_link_scale(response, family)

  expect_true(all(result > 0))
  expect_true(all(result < 1))
})

# -- Helper to run the make_good_inits pipeline for a given model/prior ------

run_init_test <- function(model, prior_type = "uninformative") {
  response_link <- bayesnec:::response_link_scale(bb_response, bb_family)
  priors <- bayesnec:::define_prior(model, bb_family, bb_predictor,
                                    response_link,
                                    prior_type = prior_type)
  set.seed(42)
  inits <- bayesnec:::make_good_inits(
    model, bb_predictor, response_link,
    priors = priors, chains = 4, seed = 42
  )
  inits
}

get_pred_fct_args <- function(model) {
  pred_fct <- get(paste0("pred_", model),
                  envir = asNamespace("bayesnec"))
  fct_args <- names(unlist(as.list(args(pred_fct))))
  setdiff(fct_args, "x")
}

# -- make_good_inits across models and prior types ---------------------------
# Models valid for beta_binomial + identity (excludes neclin, neclinhorme,
# ecxlin which are dropped by check_models for this family/link combo).
#
# Some model/prior combos cannot find good inits within the default 10k
# trials for this particular dataset --- their prediction curves exceed
# the (0, 1) bounds due to hormesis slopes or sigmoidal shapes. These
# are pre-existing limitations, not caused by the identity-link fix.
# We split models into those expected to succeed and those that may
# fall back to Stan's random initialisation.

# Models that reliably find good inits for both prior types. nechorme is
# rescued by refine_inits() (its hormesis slope term exp(b_slope) * x has no
# fractional power, so re-drawing b_slope/b_beta can bring predictions into
# range).
bb_models_expected_pass <- c(
  "nec3param", "nec4param",
  "nechorme", "nechorme4", "necsigm",
  "nechormepwr01",
  "ecxexp",
  "ecx4param", "ecxwb1", "ecxwb2",
  "ecxwb1p3", "ecxwb2p3",
  "ecxll5", "ecxll4", "ecxll3",
  "ecxhormebc4", "ecxhormebc5"
)

# Models that are structurally invalid for this dataset: they raise the
# predictor x to a fractional power (nechormepwr and nechorme4pwr use
# x^(1/(1+exp(b_slope))); ecxsigm uses x^exp(b_d)). The predictor here is
# log(mgL + 0.1), which is negative for low concentrations, and a negative
# base with a non-integer exponent is NaN in R for every parameter draw --- no
# init could fix this because the same term is evaluated in Stan. bnec() never
# reaches init-finding for them: check_models() drops them upstream whenever
# the predictor contains negative values. The test below asserts that
# behaviour rather than exercising make_good_inits() on a path bnec() prevents.
bb_models_negative_x <- c(
  "nechormepwr", "nechorme4pwr", "ecxsigm"
)

for (mod in bb_models_expected_pass) {
  for (pt in c("uninformative", "regularizing")) {
    test_that(
      paste0("make_good_inits succeeds for ", mod,
             " with ", pt, " priors (issue #162 data)"), {
      inits <- run_init_test(mod, prior_type = pt)

      # Should NOT fall back to random init
      expect_false(
        identical(inits, list(random = "random")),
        info = paste("make_good_inits should find valid inits for",
                     mod, "with", pt, "priors")
      )
      expect_type(inits, "list")
      expect_length(inits, 4)

      # Each chain's init predictions should be in (0, 1)
      pred_fct <- get(paste0("pred_", mod),
                      envir = asNamespace("bayesnec"))
      fct_args <- get_pred_fct_args(mod)
      for (i in seq_along(inits)) {
        preds <- bayesnec:::get_init_predictions(
          inits[[i]], sort(bb_predictor), pred_fct, fct_args
        )
        expect_true(
          all(preds > 0 & preds < 1),
          info = paste("Chain", i, "predictions out of (0,1) for",
                       mod, pt)
        )
      }
    })
  }
}

# The fractional-power models never reach init-finding for this dataset:
# check_models() drops them upstream because the predictor contains negative
# values. Assert that upstream guard directly (this is the behaviour that
# protects make_good_inits() from ever seeing them via bnec()).
test_that("check_models drops fractional-power models for negative predictors", {
  form <- bnf(y | trials(trials) ~ crf(log.x, "nec3param"))
  bdat <- model.frame(form, dat_real)

  requested <- c(bb_models_negative_x, "nec3param", "ecxexp")
  kept <- suppressMessages(
    bayesnec:::check_models(requested, bb_family, bdat)
  )

  # The three fractional-power models are dropped ...
  expect_false(any(bb_models_negative_x %in% kept),
               info = "fractional-power models must be dropped for negative x")
  # ... while models valid for negative predictors are retained.
  expect_true(all(c("nec3param", "ecxexp") %in% kept))
})
