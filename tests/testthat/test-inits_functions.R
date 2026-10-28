# Initial curves for identity-link beta-binomial fits must stay inside (0, 1).


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

test_that("response_link_scale shifts a response at one bound inside it (#419)", {
  # For ecxflat at a response bound, use the 1 / (2n) correction to obtain
  # a finite value strictly inside the unit interval.
  for (fam in list(bernoulli(link = "identity"), binomial(link = "identity"),
                   beta_binomial(link = "identity"), Beta(link = "identity"))) {
    ones <- bayesnec:::response_link_scale(rep(1, 20), fam)
    zeros <- bayesnec:::response_link_scale(rep(0, 20), fam)
    expect_equal(ones, rep(1 - 1 / 40, 20), info = fam$family)
    expect_equal(zeros, rep(1 / 40, 20), info = fam$family)
  }
  # Rescale a varying response using its interior observations, preserving
  # distinct values instead of applying the all-bound correction.
  mixed <- bayesnec:::response_link_scale(c(0, 0.5, 1),
                                          bernoulli(link = "identity"))
  expect_length(unique(mixed), 3)
  expect_equal(mixed[1], 0.005)
  expect_identical(
    bayesnec:::response_link_scale(rep(1, 5), gaussian()), rep(1, 5)
  )
})

test_that("the init search accepts a constant inside the support (#419)", {
  # A constant cannot pass a declining-curve check. Check ecxflat against
  # mean support so it can initialise even when the response band has no width.
  x <- rep(c(0.1, 0.5, 1, 3, 10, 30), each = 5)
  fam <- bernoulli(link = "identity")
  pr <- brms::prior_string("beta(5, 2)", nlpar = "top", lb = 0, ub = 1)
  y <- bayesnec:::response_link_scale(rep(1, length(x)), fam)
  inits <- make_good_inits("ecxflat", x, y, family = fam, priors = pr,
                           chains = 3, seed = 419, n_trials = 5)
  expect_length(inits, 3)
  tops <- vapply(inits, function(i) as.numeric(i$b_top), numeric(1))
  expect_true(all(tops > 0 & tops < 1))
  g <- make_good_inits("ecxflat", nec_data$x, nec_data$y, family = gaussian(),
                       priors = brms::prior_string("normal(0.5, 1)",
                                                   nlpar = "top"),
                       chains = 2, seed = 419, n_trials = 5)
  expect_length(g, 2)
  expect_false("random" %in% names(g))
})

test_that("response_link_scale only-ones edge case stays in (0, 1)", {
  response <- c(0.98, 0.99, 1.0, 1.0)
  family <- beta_binomial(link = "identity")
  result <- bayesnec:::response_link_scale(response, family)

  expect_true(all(result > 0))
  expect_true(all(result < 1))
})


run_init_test <- function(model, prior_type = "uninformative") {
  response_link <- bayesnec:::response_link_scale(bb_response, bb_family)
  priors <- bayesnec:::define_prior(model, bb_family, bb_predictor,
                                    response_link,
                                    prior_type = prior_type)
  set.seed(42)
  # Pass the family so the search enforces the mean’s support.
  inits <- bayesnec:::make_good_inits(
    model, bb_predictor, response_link, family = bb_family,
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

# This fixture tests admissible beta-binomial equations under an identity
# link. Initial-value success is specific to these data and prior choices.

# refine_inits() can rescue nechorme by redrawing its slope and beta; its
# hormetic term has no fractional power of the predictor.
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

# The logged predictor is negative at low doses, making fractional powers
# undefined. check_models() must exclude these equations before the search;
# initial-value retries cannot repair an invalid predictor domain.
bb_models_negative_x <- c(
  "nechormepwr", "nechorme4pwr", "ecxsigm"
)

for (mod in bb_models_expected_pass) {
  for (pt in c("uninformative", "regularizing")) {
    test_that(
      paste0("make_good_inits succeeds for ", mod,
             " with ", pt, " priors (issue #162 data)"), {
      inits <- run_init_test(mod, prior_type = pt)

      expect_false(
        identical(inits, list(random = "random")),
        info = paste("make_good_inits should find valid inits for",
                     mod, "with", pt, "priors")
      )
      expect_type(inits, "list")
      expect_length(inits, 4)

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

# Check the upstream exclusion directly: invalid fractional-power equations
# must not reach initial-value search on these negative predictors.
test_that("check_models drops fractional-power models for negative predictors", {
  form <- bnf(y | trials(trials) ~ crf(log.x, "nec3param"))
  bdat <- model.frame(form, dat_real)

  requested <- c(bb_models_negative_x, "nec3param", "ecxexp")
  kept <- suppressMessages(
    bayesnec:::check_models(requested, bb_family, bdat)
  )

  expect_false(any(bb_models_negative_x %in% kept),
               info = "fractional-power models must be dropped for negative x")
  expect_true(all(c("nec3param", "ecxexp") %in% kept))
})

# Dispersion-prior rows must not enter the curve-parameter name check.

disp_prior_df <- function(class = "sigma") {
  data.frame(prior = c("normal(1,1)", "normal(0,5)", "normal(0.5,1)",
                       "gamma(5,2)", "student_t(3,0,2.5)"),
             class = c(rep("b", 4), class), coef = "", group = "",
             resp = "", dpar = "",
             nlpar = c("top", "beta", "bot", "nec", ""),
             lb = "", ub = "", stringsAsFactors = FALSE)
}

test_that("make_inits accepts a prior on the dispersion parameter", {
  fct_args <- c("b_top", "b_beta", "b_bot", "b_nec")
  for (cl in c("sigma", "shape", "phi")) {
    out <- bayesnec:::make_inits("nec4param", fct_args,
                                 priors = disp_prior_df(cl), chains = 2)
    expect_length(out, 2)
    expect_setequal(names(out[[1]]), fct_args)
  }
})

test_that("no initial value is generated for the dispersion parameter", {
  # Stan initialises dispersion parameters absent from the init list. Their
  # priors still reach brm(); only the curve search ignores them.
  out <- bayesnec:::make_inits("nec4param",
                               c("b_top", "b_beta", "b_bot", "b_nec"),
                               priors = disp_prior_df(), chains = 2)
  expect_false(any(grepl("sigma", names(out[[1]]))))
  expect_length(out[[1]], 4)
})

test_that("a prior naming a parameter the curve does not have still errors", {
  # The name check must keep doing its job. Only dispersion classes are exempt.
  bad <- disp_prior_df()
  bad <- bad[bad$class == "b", ]
  bad$nlpar[1] <- "notaparameter"
  expect_error(
    bayesnec:::make_inits("nec4param", c("b_top", "b_beta", "b_bot", "b_nec"),
                          priors = bad, chains = 2),
    "do not match expectation"
  )
})

# A constant prior supplies its fixed value without a distribution draw.

const_prior_df <- function(prior_bot = "constant(0)", lb = "", ub = "") {
  data.frame(prior = c("normal(1,1)", "normal(0,5)", prior_bot,
                       "gamma(5,2)"),
             class = "b", coef = "", group = "", resp = "", dpar = "",
             nlpar = c("top", "beta", "bot", "nec"),
             lb = c("", "", lb, ""), ub = c("", "", ub, ""),
             stringsAsFactors = FALSE)
}

test_that("make_inits assigns a constant prior rather than sampling it", {
  fct_args <- c("b_top", "b_beta", "b_bot", "b_nec")
  out <- bayesnec:::make_inits("nec4param", fct_args,
                               priors = const_prior_df(), chains = 3)
  expect_length(out, 3)
  for (chain in out) {
    expect_setequal(names(chain), fct_args)
    expect_equal(as.numeric(chain$b_bot), 0)
  }
  # Preserve a nonzero constant exactly.
  out2 <- bayesnec:::make_inits("nec4param", fct_args,
                               priors = const_prior_df("constant(0.5)"),
                               chains = 2)
  expect_equal(as.numeric(out2[[1]]$b_bot), 0.5)
})

test_that("a constant outside its own bounds does not hang", {
  # A constant cannot be redrawn into its bounds. Skip rejection sampling
  # for fixed values to avoid an infinite loop.
  out <- bayesnec:::make_inits(
    "nec4param", c("b_top", "b_beta", "b_bot", "b_nec"),
    priors = const_prior_df("constant(0)", lb = "1", ub = "10"), chains = 2
  )
  expect_equal(as.numeric(out[[1]]$b_bot), 0)
})

test_that("the fixed value is kept for the curve check, not dropped", {
  # Keep fixed values during curve checks: dropping bot = 0 leaves the
  # prediction function without a required parameter.
  x <- as.numeric(rep(1:10, each = 5))
  set.seed(42)
  y <- 3 * exp(-exp(-0.5) * pmax(x - 4, 0)) + rnorm(length(x), 0, 0.1)
  priors <- bayesnec:::define_prior("nec4param",
                                    validate_family("gaussian"), x, y)
  priors$prior[priors$nlpar == "bot"] <- "constant(0)"
  inits <- bayesnec:::make_good_inits("nec4param", x, y,
                                     family = validate_family("Beta"),
                                     priors = priors,
                                      chains = 2, seed = 42)
  expect_false(is.character(inits))   # i.e. not the "random" fallback
  expect_true("b_bot" %in% names(inits[[1]]))
  expect_equal(as.numeric(inits[[1]]$b_bot), 0)
})

test_that("refine_inits skips a parameter that is fixed", {
  # Refinement must also skip a parameter fixed by constant().
  x <- as.numeric(rep(1:10, each = 5))
  priors <- const_prior_df()
  priors$prior[priors$nlpar == "beta"] <- "constant(-0.5)"
  fct_args <- c("b_top", "b_beta", "b_bot", "b_nec")
  init <- list(b_top = as.array(1e6), b_beta = as.array(-0.5),
               b_bot = as.array(0), b_nec = as.array(5))
  expect_silent(
    out <- bayesnec:::refine_inits(init, sort(x),
                                   bayesnec:::pred_nec4param, fct_args,
                                   limits = c(0, 3), priors = priors,
                                   n_sub = 5)
  )
  expect_equal(as.numeric(out$b_beta), -0.5)
})

test_that("a constant prior that fixes no readable value errors", {
  expect_error(
    bayesnec:::make_inits("nec4param", c("b_top", "b_beta", "b_bot", "b_nec"),
                          priors = const_prior_df("constant(a)"), chains = 2),
    "must fix a single numeric value"
  )
})

test_that("constant() is read as brms writes it, not as a bare number", {
  # Both of these are legal brms priors that as.numeric() on the bracket
  # contents cannot read: the value is an R expression rather than a literal,
  # and constant() takes a second `broadcast` argument.
  v <- bayesnec:::constant_prior_value
  expect_equal(v("constant(0.5)"), 0.5)
  expect_equal(v("constant( 0.5 )"), 0.5)
  expect_equal(v("constant(-1e-3)"), -0.001)
  expect_equal(v("constant(1/2)"), 0.5)
  expect_equal(v("constant(0.5, broadcast = FALSE)"), 0.5)
  expect_equal(v(c("constant(1/4)", "constant(2)")), c(0.25, 2))
  expect_error(v("constant(a)"), "must fix a single numeric value")
  expect_error(v("constant(c(1, 2))"), "must fix a single numeric value")
})

test_that("a fixed nec still reads as a nec, not silently as an NSEC", {
  # Keep a fixed nec in the extracted coefficients. Otherwise expand_nec()
  # can misclassify the equation and report an NSEC instead of its NEC.
  fef <- matrix(c(4, 4, 4, 3.04, 2.9, 3.2), nrow = 2, byrow = TRUE,
                dimnames = list(c("nec_Intercept", "top_Intercept"),
                                c("Estimate", "Q2.5", "Q97.5")))
  local_mocked_bindings(fixef = function(...) fef, .package = "bayesnec")
  out <- bayesnec:::extract_pars("nec", structure(list(), class = "brmsfit"))
  expect_false(identical(out, NA))
  expect_equal(unname(out["Estimate"]), 4)
  # A fixed parameter’s zero-width interval is not missing.
  expect_equal(unname(out["Q2.5"]), 4)
})

test_that("a constant prior value is not evaluated against the caller's data", {
  # A prior is a specification, not a hook for arbitrary code from elsewhere in
  # the session, so the expression is evaluated in baseenv().
  secret_value_244 <- 99
  expect_error(bayesnec:::constant_prior_value("constant(secret_value_244)"),
               "must fix a single numeric value")
})

test_that("the fixed parameter is dropped before the inits reach brm", {
  # Keep constants through curve checks, then remove their initial values
  # before brm(): Stan does not declare them in its parameters block.
  x <- as.numeric(rep(1:10, each = 5))
  set.seed(42)
  y <- 3 * exp(-exp(-0.5) * pmax(x - 4, 0)) + rnorm(length(x), 0, 0.1)
  priors <- bayesnec:::define_prior("nec4param",
                                    validate_family("gaussian"), x, y)
  priors$prior[priors$nlpar == "bot"] <- "constant(0)"
  out <- suppressMessages(
    bayesnec:::add_brm_defaults(list(prior = priors, chains = 2, seed = 42),
                               "nec4param", validate_family("gaussian"), x, y,
                               skip_check = FALSE, custom_name = NULL)
  )
  expect_false("b_bot" %in% names(out$init[[1]]))
  expect_setequal(names(out$init[[1]]), c("b_top", "b_beta", "b_nec"))
  expect_true("constant(0)" %in% out$prior$prior)
})

test_that("a supplied init is honoured whether or not the data check is run", {
  # A supplied init must bypass the search even under skip_check = TRUE.
  # Skipping data checks does not request replacement initial values.
  x <- as.numeric(rep(1:10, each = 5))
  set.seed(42)
  y <- 3 * exp(-exp(-0.5) * pmax(x - 4, 0)) + rnorm(length(x), 0, 0.1)
  searched <- FALSE
  local_mocked_bindings(
    make_good_inits = function(...) {
      searched <<- TRUE
      list(random = "random")
    },
    .package = "bayesnec"
  )
  for (sc in c(TRUE, FALSE)) {
    searched <- FALSE
    out <- suppressMessages(
      bayesnec:::add_brm_defaults(list(init = "random"), "nec4param",
                                 validate_family("gaussian"), x, y,
                                 skip_check = sc, custom_name = NULL)
    )
    expect_false(searched)
    expect_identical(out$init, "random")
  }
  # Search when init is absent on either skip_check route.
  for (sc in c(TRUE, FALSE)) {
    searched <- FALSE
    suppressMessages(
      bayesnec:::add_brm_defaults(list(), "nec4param",
                                 validate_family("gaussian"), x, y,
                                 skip_check = sc, custom_name = NULL)
    )
    expect_true(searched)
  }
})
# Fixed parameters produce zero-variance draws and unavailable diagnostics.
# Exclude those parameters from screening so they do not cause an NA verdict
# or an error when summarising a model set.

test_that("a fixed parameter does not break rhat on a multi-model fit", {
  skip_on_cran()
  set.seed(244)
  x <- rep(seq(0, 5, length.out = 20), 3)
  y <- 3 * exp(-exp(-0.5) * pmax(x - 2, 0)) + rnorm(length(x), 0, 0.1)
  d <- data.frame(x = x, y = y)
  # Use two candidate equations so this reaches the model-set rhat method.
  f <- y ~ crf(x, model = c("nec4param", "ecx4param"))
  p <- lapply(get_priors(f, data = d, family = gaussian()), function(z) {
    z$prior[z$nlpar == "top"] <- "constant(3)"
    z
  })
  fit <- suppressWarnings(suppressMessages(
    bnec(f, data = d, family = gaussian(), prior = p, chains = 2, iter = 600,
         warmup = 300, seed = 244, refresh = 0)
  ))
  skip_if_not(is_bayesmanecfit(fit), "both candidates were needed for this test")

  r <- expect_silent(rhat(fit, rhat_cutoff = 99))
  verdicts <- vapply(r, "[[", logical(1), "failed")
  expect_false(anyNA(verdicts))
  # Exclude the fixed parameter rather than retaining an NA diagnostic.
  expect_false(anyNA(r[[1]]$rhat_vals))
  expect_false("top" %in% names(r[[1]]$rhat_vals))

  tab <- check_sampling(fit, rhat_cutoff = 99, ess_cutoff = 0,
                        divergence_cutoff = 1e6)
  expect_false(anyNA(tab$failed))
  expect_false(anyNA(tab$max_rhat))
  expect_false(any(tab$failed))
  # Check that a reported no-op leaves the model set unchanged.
  expect_message(
    out <- screen_models(fit, rhat_cutoff = 99, ess_cutoff = 0,
                         divergence_cutoff = 1e6),
    "candidate models passed"
  )
  expect_equal(length(out$mod_fits), length(fit$mod_fits))
})

# The curve search must exclude auxiliary ogl and dispersion priors while
# retaining them for brm(). The ogl row has class b, so class filtering
# alone does not remove it.

test_that("a group-level prior does not make the init search reject the set", {
  x <- as.numeric(rep(1:10, each = 5))
  set.seed(245)
  y <- plogis(3 * exp(-exp(-0.5) * pmax(x - 4, 0)) + rnorm(length(x), 0, 0.1))
  priors <- bayesnec:::define_prior(
    "nec4param", validate_family("Beta"), x, y,
    group_spec = list(nlpars = "ogl", ogl = TRUE)
  )
  expect_true(any(priors$class == "sd" & priors$nlpar == "ogl"))
  expect_true(any(priors$class == "b" & priors$nlpar == "ogl"))
  out <- suppressMessages(
    bayesnec:::add_brm_defaults(list(prior = priors, chains = 2, seed = 245),
                                "nec4param", validate_family("Beta"), x, y,
                                skip_check = FALSE, custom_name = NULL)
  )
  expect_setequal(names(out$init[[1]]), c("b_top", "b_bot", "b_beta", "b_nec"))
  expect_true(any(out$prior$class == "sd" & out$prior$nlpar == "ogl"))
  expect_true(any(out$prior$class == "b" & out$prior$nlpar == "ogl"))
})

test_that("a group-level sd on a curve parameter leaves its own init alone", {
  # pgl and (par | group) put the sd on the curve's own nlpar names, so the
  # filter has to be by class rather than by name -- dropping "top" by name
  # would take the curve's own prior with it.
  x <- as.numeric(rep(1:10, each = 5))
  set.seed(245)
  y <- plogis(3 * exp(-exp(-0.5) * pmax(x - 4, 0)) + rnorm(length(x), 0, 0.1))
  priors <- bayesnec:::define_prior(
    "nec4param", validate_family("Beta"), x, y,
    group_spec = list(nlpars = c("top", "nec"), ogl = FALSE)
  )
  out <- suppressMessages(
    bayesnec:::add_brm_defaults(list(prior = priors, chains = 2, seed = 245),
                                "nec4param", validate_family("Beta"), x, y,
                                skip_check = FALSE, custom_name = NULL)
  )
  expect_setequal(names(out$init[[1]]), c("b_top", "b_bot", "b_beta", "b_nec"))
  expect_equal(sum(out$prior$class == "sd"), 2)
})

test_that("a grouped fit on a bounded family initialises and samples", {
  # A group prior alone does not initialise the group terms. Supply their
  # initial values too, and check that the fit actually contains draws.
  skip_on_cran()
  set.seed(245)
  n_tank <- 12
  x <- rep(seq(0, 4, length.out = 15), each = 4)
  tank <- factor(rep(seq_len(n_tank), length.out = length(x)))
  # Generate an actual tank effect so the group-level term is estimated.
  offset <- rnorm(n_tank, 0, 0.04)[as.integer(tank)]
  mu <- 0.05 + (0.9 - 0.05) * exp(-exp(-0.4) * pmax(x - 2, 0)) + offset
  mu <- pmin(pmax(mu, 0.01), 0.99)
  y <- rbeta(length(mu), mu * 40, (1 - mu) * 40)
  d <- data.frame(x = x, y = y, tank = tank)
  fit <- suppressMessages(suppressWarnings(
    bnec(y ~ crf(x, "nec4param") + ogl(tank), data = d,
         family = Beta(link = "identity"), iter = 600, warmup = 300,
         chains = 2, seed = 245, refresh = 0)
  ))
  expect_s3_class(fit, "bayesnecfit")
  # A fit that fails initialisation has no draws.
  expect_gt(brms::ndraws(fit$fit), 0)
  expect_true(any(grepl("^sd_tank", brms::variables(fit$fit))))
})

test_that("group_inits reads its indices from the model brms will build", {
  # Read group-term indices and dimensions from make_standata(): a pgl term
  # over several parameters creates several separately indexed terms.
  set.seed(245)
  d <- data.frame(x = runif(60, 0, 4), y = runif(60, 0.1, 0.9),
                  tank = factor(rep(1:12, 5)))
  bf_one <- brms::bf(
    y ~ ogl + bot + (top - bot) * exp(-exp(beta) * (x - nec) * step(x - nec)),
    ogl ~ 1 + (1 | tank), bot ~ 1, top ~ 1, beta ~ 1, nec ~ 1, nl = TRUE
  )
  pr <- brms::prior_string("student_t(3, 0, 0.08)", class = "sd",
                           nlpar = "ogl")
  # Pass the group specification so the helper can identify intercepts
  # that need zero initial values.
  gi <- bayesnec:::group_inits(bf_one, d, Beta(link = "identity"), pr,
                               group_spec = list(nlpars = "ogl", ogl = TRUE))
  expect_setequal(names(gi), c("sd_1", "z_1", "b_ogl"))
  expect_equal(dim(gi$z_1), c(1L, 12L))
  expect_true(all(gi$z_1 == 0))
  expect_equal(as.numeric(gi$b_ogl), 0)
  expect_equal(as.numeric(gi$sd_1), 0.08)

  bf_two <- brms::bf(
    y ~ bot + (top - bot) * exp(-exp(beta) * (x - nec) * step(x - nec)),
    bot ~ 1 + (1 | tank), top ~ 1 + (1 | tank), beta ~ 1, nec ~ 1, nl = TRUE
  )
  gi2 <- bayesnec:::group_inits(bf_two, d, Beta(link = "identity"), pr)
  expect_setequal(names(gi2), c("sd_1", "z_1", "sd_2", "z_2"))
  expect_false("b_ogl" %in% names(gi2))
})

test_that("sd_prior_scales reads the scale out of a generated prior", {
  pr <- brms::prior_string("student_t(3, 0, 0.0979)", class = "sd",
                           nlpar = "ogl") +
    brms::prior_string("student_t(3, 0, 0.5)", class = "sd", nlpar = "beta") +
    brms::prior_string("normal(0, 5)", nlpar = "beta")
  expect_equal(bayesnec:::sd_prior_scales(pr), c(0.0979, 0.5))
  expect_length(bayesnec:::sd_prior_scales(
    brms::prior_string("normal(0, 5)", nlpar = "beta")), 0)
  expect_length(bayesnec:::sd_prior_scales(NULL), 0)
  # Read only distributions whose final argument is a scale. Gamma and
  # exponential priors use rates, and constant() supplies a value.
  mixed <- data.frame(
    prior = c("exponential(25)", "student_t(3, 0, 0.09)", "constant(0)",
              "normal(0, 0.5)", "gamma(2, 100)", "cauchy(0, 2)"),
    class = rep("sd", 6), stringsAsFactors = FALSE
  )
  expect_equal(bayesnec:::sd_prior_scales(mixed), c(0.09, 0.5, 2))
})

# mu_support() and mu_is_constrained() are tested directly in
# test-mu_support.R; what is tested here is the gate that consumes them.
test_that("the adapt_delta raise is gated on the support of mu", {
  # For an equation with unprotected group deviations, decide the raise from
  # the mean support and inverse-link range together. A Beta log link is
  # unsafe because exp() can exceed one.
  x <- as.numeric(rep(1:10, each = 5))
  set.seed(245)
  y <- plogis(rnorm(50, 1, 1))
  defaults <- function(family, group) {
    suppressMessages(bayesnec:::add_brm_defaults(
      list(chains = 2), "nec3param", family, x, y,
      skip_check = TRUE, custom_name = NULL, group_spec = group
    ))
  }
  # Use neclin to isolate the support gate: its unbounded-below mean cannot
  # use a support-preserving deviation transform. Test equation-specific
  # transform gates separately in test-check_priors.R.
  grouped <- list(nlpars = c("top", "slope", "nec"), ogl = FALSE)
  defaults_lin <- function(family, group) {
    suppressMessages(bayesnec:::add_brm_defaults(
      list(chains = 2), "neclin", family, x, y,
      skip_check = TRUE, custom_name = NULL, group_spec = group
    ))
  }

  expect_equal(defaults_lin(validate_family("Beta"), grouped)$control$adapt_delta,
               0.99)
  expect_equal(defaults_lin(validate_family("Gamma"), grouped)$control$adapt_delta,
               0.99)
  expect_equal(
    defaults_lin(validate_family("poisson"), grouped)$control$adapt_delta, 0.99
  )
  expect_null(defaults_lin(validate_family("gaussian"), grouped)$control)
  expect_null(defaults_lin(gaussian(link = "log"), grouped)$control)
  expect_null(defaults_lin(Beta(link = "logit"), grouped)$control)
  # A support-preserving ogl transform needs no excursion-specific raise.
  expect_null(
    defaults(validate_family("Beta"), list(nlpars = "ogl", ogl = TRUE))$control
  )
  # The parameter transforms likewise protect this bounded curve.
  expect_null(defaults(validate_family("Beta"), grouped)$control)
  # constrained mean, ungrouped: left alone, since there is no unconstrained
  # deviation to carry the mean out of range
  expect_null(defaults(validate_family("Beta"), NULL)$control)

  both <- suppressMessages(bayesnec:::add_brm_defaults(
    list(chains = 2, control = list(max_treedepth = 12)), "neclin",
    validate_family("Beta"), x, y, skip_check = TRUE, custom_name = NULL,
    group_spec = grouped))
  expect_equal(both$control$max_treedepth, 12)
  expect_equal(both$control$adapt_delta, 0.99)
  # Preserve an explicit adapt_delta, including a lower value.
  own <- suppressMessages(bayesnec:::add_brm_defaults(
    list(chains = 2, control = list(adapt_delta = 0.8)), "neclin",
    validate_family("Beta"), x, y, skip_check = TRUE, custom_name = NULL,
    group_spec = grouped))
  expect_equal(own$control$adapt_delta, 0.8)
})

test_that("a constant ogl intercept gets no initial value", {
  # A constant ogl intercept has no Stan parameter to initialise. Strip its
  # init after group_inits() appends it, while preserving the prior.
  set.seed(245)
  d <- data.frame(x = rep(seq(0, 4, length.out = 15), 4),
                  y = runif(60, 0.1, 0.9), tank = factor(rep(1:12, 5)))
  f <- y ~ crf(x, "nec4param") + ogl(tank)
  pr <- suppressMessages(suppressWarnings(
    get_priors(f, data = d, family = Beta(link = "identity"))
  ))
  pr$prior[pr$class == "b" & pr$nlpar == "ogl"] <- "constant(0)"
  bnf <- bayesnecformula(f)
  bdat <- suppressMessages(model.frame(bnf, data = d))
  bb <- suppressMessages(suppressWarnings(
    bayesnec:::wrangle_model_formula("nec4param", bnf, bdat,
                                     validate_family("Beta"))
  ))
  gi <- bayesnec:::group_inits(bb, d, Beta(link = "identity"), pr,
                               group_spec = list(nlpars = "ogl", ogl = TRUE))
  # group_inits itself is not prior-aware; the strip happens in fit_bayesnec
  expect_true("b_ogl" %in% names(gi))
  const <- as.data.frame(pr)
  keep <- !names(gi) %in% paste0("b_", const$nlpar[
    bayesnec:::is_constant_prior(const$prior) & const$class == "b" &
      nzchar(const$nlpar)])
  expect_false("b_ogl" %in% names(gi[keep]))
  expect_true(all(c("sd_1", "z_1") %in% names(gi[keep])))
})

test_that("group_inits works when the response is invalid for the fit's family", {
  # Group-term dimensions must remain available for exact boundary responses.
  # The fit-family standata query can reject them; the Gaussian fallback
  # obtains the dimensions without that response-support refusal.
  set.seed(245)
  d <- data.frame(x = rep(seq(0, 4, length.out = 15), 4),
                  colony = factor(rep(1:5, 12)))
  # Include exact zeros and ones to exercise the support-validation fallback.
  d$y <- c(rep(1, 20), runif(20, 0.1, 0.9), rep(0, 20))
  bb <- brms::bf(
    y ~ bot + (top - bot) * exp(-exp(beta) * (x - nec) * step(x - nec)),
    bot ~ 1 + (1 | colony), top ~ 1 + (1 | colony),
    beta ~ 1 + (1 | colony), nec ~ 1 + (1 | colony), nl = TRUE
  )
  pr <- brms::prior_string("student_t(3, 0, 0.1)", class = "sd", nlpar = "top")
  gi <- expect_no_warning(
    bayesnec:::group_inits(bb, d, Beta(link = "identity"), pr)
  )
  expect_setequal(names(gi), c("sd_1", "z_1", "sd_2", "z_2",
                               "sd_3", "z_3", "sd_4", "z_4"))
  expect_true(all(vapply(gi[grep("^z_", names(gi))],
                         function(z) all(z == 0), logical(1))))
  expect_equal(unname(vapply(gi[grep("^z_", names(gi))],
                             function(z) ncol(z), integer(1))),
               rep(5L, 4))
})

test_that("pgl on a bounded family initialises and samples", {
  # Exercise pgl initialisation end to end with exact boundary responses.
  skip_on_cran()
  set.seed(245)
  n_col <- 5
  x <- rep(seq(0, 4, length.out = 20), 4)
  colony <- factor(rep(seq_len(n_col), length.out = length(x)))
  mu <- 0.05 + (0.92 - 0.05) * exp(-exp(-0.3) * pmax(x - 2, 0))
  mu <- pmin(pmax(mu + rnorm(n_col, 0, 0.03)[as.integer(colony)], 0.01), 0.99)
  y <- rbeta(length(mu), mu * 30, (1 - mu) * 30)
  # push a handful onto the boundaries, which is what check_data() has to nudge
  y[sample(seq_along(y), 6)] <- 1
  y[sample(which(x > 3), 4)] <- 0
  d <- data.frame(x = x, y = y, colony = colony)
  fit <- suppressMessages(suppressWarnings(
    bnec(y ~ crf(x, "nec4param") + pgl(colony), data = d,
         family = Beta(link = "identity"), iter = 600, warmup = 300,
         chains = 2, seed = 245, refresh = 0)
  ))
  expect_s3_class(fit, "bayesnecfit")
  expect_gt(brms::ndraws(fit$fit), 0)
  expect_true(any(grepl("^sd_colony", brms::variables(fit$fit))))
})

test_that("group_inits works for a family whose formula carries trials()", {
  # Ask make_standata() with the fit’s own family before the Gaussian
  # fallback: Gaussian does not accept trials(), so it cannot query these
  # binomial group-term dimensions.
  set.seed(245)
  d <- data.frame(y = rbinom(60, 20, 0.5), tr = rep(20, 60),
                  x = rep(seq(0, 4, length.out = 15), 4),
                  tank = factor(rep(1:12, 5)))
  f <- bayesnecformula(y | trials(tr) ~ crf(x, "nec3param") + ogl(tank))
  bdat <- suppressMessages(model.frame(f, data = d))
  bb <- suppressMessages(suppressWarnings(
    bayesnec:::wrangle_model_formula("nec3param", f, bdat,
                                     validate_family("binomial"))
  ))
  pr <- brms::prior_string("student_t(3, 0, 0.08)", class = "sd", nlpar = "ogl")
  gi <- expect_no_warning(
    bayesnec:::group_inits(bb, d, binomial(link = "identity"), pr,
                           group_spec = list(nlpars = "ogl", ogl = TRUE))
  )
  expect_setequal(names(gi), c("sd_1", "z_1", "b_ogl"))
  expect_equal(dim(gi$z_1), c(1L, 12L))
  expect_true(all(gi$z_1 == 0))
  expect_equal(as.numeric(gi$b_ogl), 0)
})

test_that("a binomial grouped fit initialises and samples", {
  # Exercise the trials() route through a complete grouped fit.
  skip_on_cran()
  set.seed(245)
  n_tank <- 12
  x <- rep(seq(0, 4, length.out = 15), each = 4)
  tank <- factor(rep(seq_len(n_tank), length.out = length(x)))
  offset <- rnorm(n_tank, 0, 0.04)[as.integer(tank)]
  mu <- 0.05 + (0.9 - 0.05) * exp(-exp(-0.4) * pmax(x - 2, 0)) + offset
  mu <- pmin(pmax(mu, 0.01), 0.99)
  tr <- rep(20, length(mu))
  y <- rbinom(length(mu), tr, mu)
  d <- data.frame(x = x, y = y, tr = tr, tank = tank)
  fit <- suppressMessages(suppressWarnings(
    bnec(y | trials(tr) ~ crf(x, "nec3param") + ogl(tank), data = d,
         family = binomial(link = "identity"), iter = 600, warmup = 300,
         chains = 2, seed = 245, refresh = 0)
  ))
  expect_s3_class(fit, "bayesnecfit")
  expect_gt(brms::ndraws(fit$fit), 0)
  expect_true(any(grepl("^sd_tank", brms::variables(fit$fit))))
})

test_that("a hurdle grouped fit initialises and samples", {
  # The hurdle branch constructs priors and group inits separately from
  # link-scaled survivors. Exercise it through a fitted model.
  skip_on_cran()
  set.seed(245)
  n_site <- 6
  x <- rep(seq(0, 4, length.out = 15), each = 4)
  site <- factor(rep(seq_len(n_site), length.out = length(x)))
  mu <- 8 * exp(-exp(-0.4) * pmax(x - 2, 0)) +
    rnorm(n_site, 0, 0.5)[as.integer(site)]
  y <- rgamma(length(mu), shape = 5, rate = 5 / pmax(mu, 0.1))
  y[x > 3 & runif(length(x)) < 0.5] <- 0
  d <- data.frame(x = x, y = y, site = site)
  fit <- suppressMessages(suppressWarnings(
    bnec(y ~ crf(x, "nec3param") + ogl(site), data = d,
         family = "hurdle_gamma", iter = 600, warmup = 300,
         chains = 2, seed = 245, refresh = 0)
  ))
  expect_s3_class(fit, "bayesnecfit")
  expect_gt(brms::ndraws(fit$fit), 0)
  expect_true(any(grepl("^sd_site", brms::variables(fit$fit))))
})

test_that("group_inits works for a formula carrying a rate() aterm", {
  # rate() also excludes a Gaussian-only standata query. Trying the fit’s
  # own family first supports family-specific terms without special cases.
  set.seed(245)
  d <- data.frame(y = rpois(60, 40), expo = rep(2, 60),
                  x = rep(seq(0, 4, length.out = 15), 4),
                  site = factor(rep(1:12, 5)))
  f <- bayesnecformula(y | rate(expo) ~ crf(x, "nec3param") + ogl(site))
  bdat <- suppressMessages(model.frame(f, data = d))
  bb <- suppressMessages(suppressWarnings(
    bayesnec:::wrangle_model_formula("nec3param", f, bdat,
                                     validate_family("poisson"))
  ))
  expect_error(
    suppressMessages(brms::make_standata(bb, data = d, family = gaussian()))
  )
  pr <- brms::prior_string("student_t(3, 0, 2)", class = "sd", nlpar = "ogl")
  gi <- expect_no_warning(
    bayesnec:::group_inits(bb, d, poisson(link = "identity"), pr,
                           group_spec = list(nlpars = "ogl", ogl = TRUE))
  )
  expect_setequal(names(gi), c("sd_1", "z_1", "b_ogl"))
  expect_equal(dim(gi$z_1), c(1L, 12L))
  expect_true(all(gi$z_1 == 0))
})


test_that("the search is bounded by attempts alone, and the cap is 1e4", {
  # Bound search by attempts, not elapsed time, so machine load cannot
  # change the initial values for a fixed seed.
  expect_false("max_seconds" %in% names(formals(make_good_inits)))
  expect_equal(formals(make_good_inits)$n_trials, quote(1e4))
  # report_after says the search is still running; it must not end it.
  expect_true("report_after" %in% names(formals(make_good_inits)))
})

test_that("the search is deterministic given a seed", {
  # Compare actual results under one seed; checking only the absence of a
  # time-limit argument would not establish deterministic search behaviour.
  skip_on_cran()
  x <- rep(c(1, 5, 20, 100), each = 5)
  y <- rep(c(0.9, 0.6, 0.3, 0.1), each = 5)
  pr <- suppressMessages(
    define_prior("nec4param", validate_family("Beta"), x, y)
  )
  run <- function() {
    suppressMessages(
      make_good_inits("nec4param", x, y, family = validate_family("Beta"),
                      n_trials = 20, seed = 42, priors = pr, chains = 2)
    )
  }
  expect_equal(run(), run())
})

# A session may lack .Random.seed. Preserve that absence as well as any
# existing seed, and register restore_stream() in the test’s own frame
# so its on.exit() handler runs when the test ends.
saved_stream <- function() {
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  }
}

restore_stream <- function(old) {
  if (is.null(old)) {
    suppressWarnings(rm(".Random.seed", envir = globalenv()))
  } else {
    assign(".Random.seed", old, envir = globalenv())
  }
}

test_that("with no seed the search follows the caller's stream", {
  # With no explicit seed, follow the ambient stream. Check reproducibility,
  # sensitivity to a changed seed and the resulting stream state together.
  skip_on_cran()
  x <- rep(c(1, 5, 20, 100), each = 5)
  y <- rep(c(0.9, 0.6, 0.3, 0.1), each = 5)
  fam <- validate_family("Beta")
  pr <- suppressMessages(define_prior("nec4param", fam, x, y))
  # Restore the ambient stream so this test does not affect later tests.
  old_seed <- saved_stream()
  on.exit(restore_stream(old_seed), add = TRUE)
  run <- function(ambient) {
    set.seed(ambient)
    inits <- suppressMessages(
      make_good_inits("nec4param", x, y, family = fam, n_trials = 200,
                      priors = pr, chains = 2)
    )
    list(inits = inits, after = .Random.seed)
  }
  a <- run(333)
  b <- run(333)
  c <- run(334)
  expect_equal(a$inits, b$inits)
  expect_true(identical(a$after, b$after))
  # A different ambient seed must change the initial values.
  expect_false(isTRUE(all.equal(a$inits, c$inits)))
})

test_that("seed = NA is read as no seed rather than erroring", {
  # Treat brms seed = NA as no seed; set.seed(NA) would error.
  skip_on_cran()
  x <- rep(c(1, 5, 20, 100), each = 5)
  y <- rep(c(0.9, 0.6, 0.3, 0.1), each = 5)
  fam <- validate_family("Beta")
  pr <- suppressMessages(define_prior("nec4param", fam, x, y))
  old_seed <- saved_stream()
  on.exit(restore_stream(old_seed), add = TRUE)
  run <- function() {
    set.seed(11)
    suppressMessages(
      make_good_inits("nec4param", x, y, family = fam, n_trials = 200,
                      priors = pr, chains = 2, seed = NA)
    )
  }
  expect_error(run(), NA)
  expect_equal(run(), run())
  # A seed of any other shape is rejected at this boundary. integer(0) is the
  # case all(is.na()) got wrong; set.seed(c(1, 2)) silently uses 1, so the
  # length-two case must be checked here rather than delegated to base R.
  expect_error(
    suppressMessages(
      make_good_inits("nec4param", x, y, family = fam, n_trials = 5,
                      priors = pr, chains = 2, seed = integer(0))
    ),
    "numeric vector of length 1"
  )
  expect_error(
    suppressMessages(
      make_good_inits("nec4param", x, y, family = fam, n_trials = 5,
                      priors = pr, chains = 2, seed = c(1, 2))
    ),
    "numeric vector of length 1"
  )
})

test_that("the seed reaches the search from brm_args, NA included", {
  # Check forwarding from brm_args without compiling Stan, including NA and
  # an absent seed.
  skip_on_cran()
  x <- rep(c(1, 5, 20, 100), each = 5)
  y <- rep(c(0.9, 0.6, 0.3, 0.1), each = 5)
  fam <- validate_family("Beta")
  old_seed <- saved_stream()
  on.exit(restore_stream(old_seed), add = TRUE)
  go <- function(args) {
    set.seed(101)
    suppressMessages(
      add_brm_defaults(args, "nec4param", fam, x, y,
                       skip_check = TRUE, custom_name = NULL)
    )$init
  }
  expect_error(go(list(chains = 2, seed = NA)), NA)
  expect_equal(go(list(chains = 2)), go(list(chains = 2)))
})

test_that("both hurdle blocks follow the caller's stream", {
  # Check both hurdle blocks: this caller forwards the seed twice.
  skip_on_cran()
  conc <- rep(c(0, 1, 5, 20, 100), each = 6)
  set.seed(1)
  y <- c(rep(1, 12), stats::rbinom(18, 1, 0.4)) *
    stats::rgamma(30, shape = 4, rate = 2)
  pr <- brms::prior_string("normal(2, 2)", nlpar = "top") +
    brms::prior_string("normal(0, 2)", nlpar = "beta") +
    brms::prior_string("normal(20, 20)", nlpar = "nec", lb = 0) +
    brms::prior_string("normal(1, 1)", nlpar = "hutop") +
    brms::prior_string("normal(0, 2)", nlpar = "hubeta") +
    brms::prior_string("normal(20, 20)", nlpar = "hunec", lb = 0)
  old_seed <- saved_stream()
  on.exit(restore_stream(old_seed), add = TRUE)
  run <- function(ambient) {
    set.seed(ambient)
    suppressMessages(
      make_good_hurdle_inits("nec3param", conc, y, priors = pr, chains = 2,
                             family = brms::hurdle_gamma(link = "identity"),
                             n_trials = 200)
    )
  }
  expect_equal(run(7), run(7))
  expect_false(isTRUE(all.equal(run(7), run(8))))
})

test_that("a long search says it is still running", {
  # Use report_after = 0 to test the progress notice without a timing
  # assertion that depends on machine load.
  skip_on_cran()
  priors <- brms::prior_string("normal(1e6, 1)", nlpar = "top") +
    brms::prior_string("normal(1e6, 1)", nlpar = "beta") +
    brms::prior_string("normal(1e6, 1)", nlpar = "nec")
  msg <- capture.output(
    make_good_inits("nec3param", x = c(1, 5, 20, 100),
                    y = c(0.9, 0.6, 0.3, 0.1),
                    family = validate_family("gaussian"),
                    priors = priors, chains = 2,
                    n_trials = 3, report_after = 0),
    type = "message"
  )
  msg <- paste(msg, collapse = " ")
  expect_match(msg, "Still searching")
  expect_match(msg, "nec3param")
  expect_match(msg, "failed to find initial values")
})

# Exercise a user-supplied lognormal prior with explicit bounds. The
# default threshold prior is not truncated to the tested range.

lognormal_prior_df <- function(nec_prior = "lognormal(0, 2.3496)") {
  data.frame(prior = c("normal(1,1)", "normal(0,5)", "normal(0.5,1)",
                       nec_prior),
             class = "b", coef = "", group = "", resp = "", dpar = "",
             nlpar = c("top", "beta", "bot", "nec"),
             lb = c("", "", "", "0"), ub = c("", "", "", "100"),
             stringsAsFactors = FALSE)
}

test_that("make_inits draws a truncated lognormal for nec", {
  set.seed(302)
  fct_args <- c("b_top", "b_beta", "b_bot", "b_nec")
  out <- bayesnec:::make_inits("nec4param", fct_args,
                               priors = lognormal_prior_df(), chains = 3)
  expect_length(out, 3)
  for (chain in out) {
    expect_setequal(names(chain), fct_args)
    # the rejection loop against the bounds terminates and respects them
    expect_gt(as.numeric(chain$b_nec), 0)
    expect_lt(as.numeric(chain$b_nec), 100)
  }
})

test_that("make_inits names a distribution it cannot draw from", {
  # Name the unrecognised distribution instead of calling a missing sampler.
  expect_error(
    bayesnec:::make_inits("nec4param",
                          c("b_top", "b_beta", "b_bot", "b_nec"),
                          priors = lognormal_prior_df("cauchy(0, 1)"),
                          chains = 2),
    "cauchy"
  )
  expect_error(
    bayesnec:::make_inits("nec4param",
                          c("b_top", "b_beta", "b_bot", "b_nec"),
                          priors = lognormal_prior_df("cauchy(0, 1)"),
                          chains = 2),
    "gamma, normal, beta, uniform, lognormal"
  )
})

test_that("surrounding whitespace in a prior string is tolerated", {
  # A user-built prior table can contain whitespace that prior_string()
  # would not write.
  set.seed(302)
  out <- bayesnec:::make_inits("nec4param",
                               c("b_top", "b_beta", "b_bot", "b_nec"),
                               priors = lognormal_prior_df(" lognormal(0, 1)"),
                               chains = 1)
  expect_gt(as.numeric(out[[1]]$b_nec), 0)
})


alga_a <- function() {
  d <- alga[alga$species == "c_proliferum" & alga$contaminant == "A", ]
  list(x = d$dose, y = d$sgr)
}

test_that("group_spread pools within groups and stands in where none repeat", {
  x <- rep(c(0, 1, 2), each = 5)
  set.seed(11)
  y <- c(rnorm(5, 1, 0.1), rnorm(5, 2, 0.1), rnorm(5, 3, 0.1))
  v <- vapply(split(y, factor(x)), var, numeric(1))
  expect_equal(group_spread(x, y), sqrt(mean(v)))
  # No replication leaves no within-group variation to pool, and the spread of
  # the whole response stands in.
  expect_equal(group_spread(1:6, c(1, 2, 3, 4, 5, 6)), sd(1:6))
})

test_that("replicated_group_means ignores predictor values seen once", {
  # An unreplicated value's "mean" is that observation, and admitting those
  # would put the band back on the extrema it exists to leave.
  x <- c(0, 0, 0, 1, 2, 2)
  y <- c(1, 2, 3, 99, 4, 6)
  expect_equal(sort(replicated_group_means(x, y)), c(2, 5))
})

test_that("the upper reference is a group mean, not a single observation", {
  # A spiked control affects its mean and spread differently from max(y).
  # Check the mean reference separately from the widened band.
  x <- rep(c(0, 1, 5, 20), each = 6)
  set.seed(309)
  y <- c(rnorm(6, 1, 0.02), rnorm(6, 0.8, 0.02), rnorm(6, 0.4, 0.02),
         rnorm(6, 0.1, 0.02))
  y2 <- y
  y2[1] <- 3
  ref <- function(z) regularizing_location(x, z, "top")[["location"]]
  expect_lt(ref(y2) - ref(y), (3 - y[1]) / 6 + 1e-8)
  expect_gt(max(y2) - max(y), 1.9)
  # The outlier also widens the spread. Check containment so every curve
  # accepted by the clean band remains accepted.
  clean <- init_limits(x, y)
  spiked <- init_limits(x, y2)
  expect_lte(spiked[1], clean[1])
  expect_gte(spiked[2], clean[2])
})

test_that("the band does not get looser as replicates are added", {
  # Compare bands under replication of the same generating curve and noise.
  # Raw extrema can expand with sample size even when that noise is unchanged.
  set.seed(310)
  gen <- function(reps) {
    x <- rep(c(0, 1, 5, 20), each = reps)
    y <- rnorm(length(x), rep(c(1, 0.8, 0.4, 0.1), each = reps), 0.1)
    list(x = x, y = y)
  }
  small <- gen(5)
  large <- gen(200)
  band_growth <- diff(init_limits(large$x, large$y)) /
    diff(init_limits(small$x, small$y))
  range_growth <- diff(range(large$y)) / diff(range(small$y))
  expect_lt(band_growth, range_growth)
  expect_lt(band_growth, 1.3)
})

test_that("the band contains every group mean of the packaged alga series", {
  # Include all level means in the band: this heteroscedastic series is
  # not monotone at its lower end, so endpoint anchors alone omit levels.
  d <- alga_a()
  gm <- vapply(split(d$y, factor(d$x)), mean, numeric(1))
  lim <- init_limits(d$x, d$y)
  expect_true(all(gm >= lim[1] & gm <= lim[2]))
  expect_true(which.min(gm) != length(gm))
})

test_that("a degenerate response falls back to the observed range", {
  x <- rep(c(0, 1), each = 3)
  y <- rep(2, 6)
  expect_equal(init_limits(x, y), range(y))
})

test_that("zero_bounded_family names the families define_prior scales on zero", {
  # Read at two places that must agree: the gamma-scaled branch of
  # define_prior() and the band the init search uses.
  expect_true(zero_bounded_family(validate_family("Gamma")))
  expect_true(zero_bounded_family(validate_family("poisson")))
  expect_true(zero_bounded_family(validate_family("negbinomial")))
  expect_false(zero_bounded_family(validate_family("gaussian")))
  expect_false(zero_bounded_family(validate_family("Beta")))
})

test_that("a chain is accepted on its own, so a low per-chain rate still succeeds", {
  # For independent chains with per-chain acceptance p, the all-at-once
  # rule accepts a round with probability p^4. Compare the search structures
  # under the same acceptance band.
  skip_on_cran()
  d <- alga_a()
  pr <- suppressMessages(
    define_prior("nec3param", validate_family("gaussian"), d$x, d$y)
  )
  limits <- init_limits(d$x, d$y)
  pf <- pred_nec3param
  fa <- setdiff(names(unlist(as.list(args(pf)))), "x")
  ok <- function(init) {
    check_init_predictions(
      get_init_predictions(init, sort(d$x), pf, fa), limits)
  }
  # Compare the all-chains-at-once rule under the same band. Use 20,000
  # rounds so the three fixed seeds complete with the untruncated prior.
  released_drawn <- function(seed, cap = 20000) {
    set.seed(seed)
    n <- 0
    passed <- FALSE
    while (!passed && n < cap) {
      n <- n + 1
      passed <- all(vapply(make_inits("nec3param", fa, pr, 4), ok, logical(1)))
    }
    # Return success with the count so a capped run cannot be mistaken
    # for a measured completion count.
    c(drawn = 4 * n, ok = passed)
  }
  changed_drawn <- function(seed, cap = 2000) {
    set.seed(seed)
    filled <- rep(FALSE, 4)
    drawn <- 0
    n <- 0
    while (any(!filled) && n < cap) {
      need <- sum(!filled)
      got <- vapply(make_inits("nec3param", fa, pr, need), ok, logical(1))
      filled[which(!filled)[got]] <- TRUE
      drawn <- drawn + need
      n <- n + 1
    }
    if (any(!filled)) NA_integer_ else drawn
  }
  seeds <- c(11, 22, 33)
  a <- vapply(seeds, released_drawn, numeric(2))
  b <- vapply(seeds, changed_drawn, numeric(1))
  expect_false(anyNA(b))
  expect_lt(max(b), 200)
  expect_gt(median(a["drawn", ]) / median(b), 20)
  # Require successful runs so capped counts are not treated as measured
  # completion counts. The ecxlin test below separately checks exhaustion.
  expect_true(all(a["ok", ] == 1))
})

test_that("only the empty chain slots are re-drawn", {
  # Keep accepted chains and redraw only empty slots to avoid requiring
  # all chains to pass in the same round.
  skip_on_cran()
  d <- alga_a()
  pr <- suppressMessages(
    define_prior("nec4param", validate_family("gaussian"), d$x, d$y)
  )
  drawn <- 0
  # The original is captured before the binding is replaced; calling
  # bayesnec:::make_inits() from inside the mock would resolve to the mock.
  real_make_inits <- bayesnec:::make_inits
  with_mocked_bindings(
    make_inits = function(model, fct_args, priors, chains) {
      drawn <<- drawn + chains
      real_make_inits(model, fct_args, priors, chains)
    },
    {
      inits <- suppressMessages(
        make_good_inits("nec4param", d$x, d$y,
                        family = validate_family("gaussian"),
                        priors = pr, chains = 4, n_trials = 500, seed = 99)
      )
    }
  )
  expect_length(inits, 4)
  # Check that retaining accepted chains uses a modest number of proposals
  # on this fixture.
  expect_lt(drawn, 400)
})

test_that("every chain the search returns satisfies the check", {
  skip_on_cran()
  d <- alga_a()
  for (m in c("nec3param", "ecx4param", "ecxlin")) {
    pr <- suppressMessages(
      define_prior(m, validate_family("gaussian"), d$x, d$y)
    )
    lim <- init_limits(d$x, d$y)
    pf <- get(paste0("pred_", m))
    fa <- setdiff(names(unlist(as.list(args(pf)))), "x")
    inits <- suppressMessages(
      make_good_inits(m, d$x, d$y, family = validate_family("gaussian"),
                      priors = pr, chains = 4, seed = 7)
    )
    expect_false("random" %in% names(inits), info = m)
    expect_true(
      all(vapply(inits, function(i) check_init_predictions(
        get_init_predictions(i, sort(d$x), pf, fa), lim), logical(1))),
      info = m
    )
  }
})

test_that("the band is bounded by the support of the mean", {
  # Clamp the location-and-spread band to mean support under identity links;
  # unlike the observed response range, that band can extend outside support.
  x <- rep(c(0, 1, 5, 20), each = 5)
  set.seed(162)
  y <- pmin(pmax(rep(c(0.98, 0.8, 0.4, 0.1), each = 5) + rnorm(20, 0, 0.05),
                 0.001), 0.999)
  unconstrained <- init_limits(x, y)
  bounded <- init_limits(x, y, support = mu_support(validate_family("Beta")))
  expect_gt(unconstrained[2], 1)
  # Keep the band strictly inside the finite support boundaries.
  expect_lt(bounded[2], 1)
  expect_gt(bounded[1], 0)
  # The inset remains no stricter than the nearest observed interior value.
  expect_gte(bounded[2], max(y))
  expect_lte(bounded[1], min(y))
  expect_equal(init_limits(x, y, support = mu_support(validate_family("gaussian"))),
               unconstrained)
})

test_that("make_good_inits keeps a bounded family's curve inside its support", {
  skip_on_cran()
  x <- rep(c(0, 1, 5, 20), each = 5)
  set.seed(163)
  y <- pmin(pmax(rep(c(0.98, 0.8, 0.4, 0.1), each = 5) + rnorm(20, 0, 0.05),
                 0.001), 0.999)
  fam <- validate_family("Beta")
  pr <- suppressMessages(define_prior("nec4param", fam, x, y))
  inits <- suppressMessages(
    make_good_inits("nec4param", x, y, family = fam, priors = pr,
                    chains = 4, seed = 5)
  )
  expect_false("random" %in% names(inits))
  fa <- setdiff(names(unlist(as.list(args(pred_nec4param)))), "x")
  for (i in seq_along(inits)) {
    preds <- get_init_predictions(inits[[i]], sort(x), pred_nec4param, fa)
    expect_true(all(preds > 0 & preds < 1))
  }
})

test_that("a case that exhausted the cap now succeeds well inside it", {
  # Compare both searches on the alga c_proliferum contaminant A series,
  # each with its own acceptance criterion.
  skip_on_cran()
  d <- alga_a()
  pr <- suppressMessages(
    define_prior("ecxlin", validate_family("gaussian"), d$x, d$y)
  )
  fa <- setdiff(names(unlist(as.list(args(pred_ecxlin)))), "x")
  ok <- function(init, limits) {
    check_init_predictions(
      get_init_predictions(init, sort(d$x), pred_ecxlin, fa), limits)
  }
  cap <- 2000
  for (s in c(11, 22, 33)) {
    set.seed(s)
    rounds <- 0
    passed <- FALSE
    while (!all(passed) && rounds < cap) {
      rounds <- rounds + 1
      passed <- vapply(make_inits("ecxlin", fa, pr, 4), ok, logical(1),
                       range(d$y))
    }
    expect_gte(rounds, cap)
    # Distinguish exhausting the cap from success on the final round.
    expect_false(all(passed))
    inits <- suppressMessages(
      make_good_inits("ecxlin", d$x, d$y,
                      family = validate_family("gaussian"),
                      priors = pr, chains = 4, n_trials = cap, seed = s)
    )
    expect_false("random" %in% names(inits), info = paste("seed", s))
    expect_length(inits, 4)
  }
})

test_that("prior_family_tag applies both rewrites define_prior depends on", {
  # Use the same family-tag rewrites for prior construction and init search,
  # including zero-inflated counts and non-identity links.
  expect_equal(prior_family_tag(validate_family("zero_inflated_poisson")),
               "poisson")
  expect_equal(prior_family_tag(validate_family("zero_inflated_negbinomial")),
               "negbinomial")
  expect_equal(prior_family_tag(stats::Gamma(link = "log")), "gaussian")
  expect_equal(prior_family_tag(brms::Beta(link = "logit")), "gaussian")
  expect_equal(prior_family_tag(validate_family("Gamma")), "Gamma")
  expect_true(zero_bounded_family(validate_family("zero_inflated_poisson")))
  expect_false(zero_bounded_family(stats::Gamma(link = "log")))
})

test_that("a zero-inflated count still gets its response-scaled priors", {
  # Check the rewritten count-family tags through complete fits.
  skip_on_cran()
  set.seed(309)
  d <- data.frame(x = rep(c(0, 1, 5, 20), each = 5),
                  y = rpois(20, rep(c(20, 15, 6, 2), each = 5)))
  for (pt in c("uninformative", "regularizing")) {
    pr <- suppressMessages(
      get_priors(y ~ crf(x, "nec3param"), data = d,
                 family = "zero_inflated_poisson", prior_type = pt)
    )
    expect_true(all(nzchar(pr$prior[pr$nlpar %in% c("top", "bot")])), info = pt)
  }
})

test_that("the support is mapped onto the scale the curve is on", {
  # Predictions and the band use the linear-predictor scale. Map support
  # through the link before clamping; clamping a logit predictor to [0, 1]
  # would restrict response means to about [0.5, 0.73].
  expect_equal(init_support(validate_family("Beta")), c(0, 1))
  expect_equal(init_support(brms::Beta(link = "logit")), c(-Inf, Inf))
  expect_equal(init_support(stats::Gamma(link = "log")), c(-Inf, Inf))
  expect_equal(init_support(validate_family("Gamma")), c(0, Inf))
  expect_equal(init_support(validate_family("gaussian")), c(-Inf, Inf))
})

test_that("an unreplicated design uses the successive differences, not sd(y)", {
  # Avoid sd(y), which includes the decline. For independent equal-variance
  # noise, the noise part of a successive difference has twice that variance;
  # changes in the curve also contribute to the observed differences.
  x <- c(0, 0.5, 1, 2, 4, 8, 16, 32)
  y <- c(0.98, 0.95, 0.88, 0.70, 0.42, 0.20, 0.08, 0.04)
  expect_equal(group_spread(x, y), successive_difference_spread(x, y))
  expect_lt(group_spread(x, y), sd(y) / 3)
  # Check the band width relative to this fixture’s response range.
  expect_lt(diff(init_limits(x, y)) / diff(range(y)), 2)
  # fewer than three observations leave too few differences to read
  expect_equal(group_spread(c(1, 2), c(1, 2)), sd(c(1, 2)))
})

test_that("make_good_inits requires the family", {
  # Require a family so the search cannot silently omit its support clamp.
  expect_error(
    suppressMessages(
      make_good_inits("nec3param", c(1, 5, 20), c(0.9, 0.5, 0.1),
                      priors = define_prior("nec3param",
                                            validate_family("gaussian"),
                                            c(1, 5, 20), c(0.9, 0.5, 0.1)),
                      chains = 2, n_trials = 2)
    ),
    "family"
  )
})

test_that("ecxsigm succeeds on the alga c_proliferum series", {
  skip_on_cran()
  d <- alga_a()
  fam <- validate_family("gaussian")
  pr <- suppressMessages(define_prior("ecxsigm", fam, d$x, d$y))
  inits <- suppressMessages(
    make_good_inits("ecxsigm", d$x, d$y, family = fam, priors = pr,
                    chains = 4, n_trials = 500, seed = 8)
  )
  expect_false("random" %in% names(inits))
  expect_length(inits, 4)
  fa <- setdiff(names(unlist(as.list(args(pred_ecxsigm)))), "x")
  lim <- init_limits(d$x, d$y, support = init_support(fam))
  expect_true(all(vapply(inits, function(i) check_init_predictions(
    get_init_predictions(i, sort(d$x), pred_ecxsigm, fa), lim), logical(1))))
})

test_that("the band stops short of the support boundary", {
  # Inset a finite support boundary using level means and spread, capped
  # by the nearest interior observation; do not place the edge at that
  # observation by default.
  x <- rep(c(0, 1, 5, 20), each = 5)
  set.seed(164)
  y <- pmin(pmax(rep(c(0.98, 0.6, 0.2, 0.02), each = 5) + rnorm(20, 0, 0.05),
                 0.002), 0.998)
  lim <- init_limits(x, y, support = c(0, 1))
  expect_gt(lim[1], 0)
  expect_lt(lim[2], 1)
  # Keep every observed level mean inside the band.
  gm <- vapply(split(y, factor(x)), mean, numeric(1))
  expect_true(all(gm >= lim[1] & gm <= lim[2]))
  expect_equal(init_limits(x, y), init_limits(x, y, support = c(-Inf, Inf)))
})

test_that("the second block of a hurdle fit is not started at zero survival", {
  # The hurdle survival block uses one proportion per concentration. Its
  # initial-value band must exclude near-zero curves that can give a
  # non-finite joint log likelihood.
  skip_on_cran()
  set.seed(11)
  nec3 <- function(x, top, beta, nec) {
    top * exp(-exp(beta) * (x - nec) * (x > nec))
  }
  conc <- rep(c(0, 0.5, 1, 2, 3, 4, 5), each = 12)
  mu <- nec3(conc, 25, log(0.55), 1)
  pa <- nec3(conc, 0.97, log(0.9), 2)
  y <- ifelse(rbinom(length(conc), 1, pa) == 1,
              rgamma(length(conc), 12, 12 / mu), 0)
  fam <- validate_family("hurdle_gamma")
  parts <- split_hurdle_response(conc, y)
  hu_band <- init_limits(parts$hu$x, parts$hu$y, support = c(0, 1))
  # Exclude near-zero survival while keeping the smallest observed
  # survival proportion inside the band.
  expect_gt(hu_band[1], 1e-5)
  expect_lte(hu_band[1], min(parts$hu$y))
  pr <- suppressMessages(define_prior("nec3param", fam, conc, y))
  inits <- suppressMessages(
    make_good_hurdle_inits("nec3param", conc, y, priors = pr, chains = 2,
                           seed = 7, family = fam)
  )
  skip_if(length(inits) == 1 && "random" %in% names(inits),
          "init search fell back to random")
  fa <- setdiff(names(unlist(as.list(args(pred_nec3param)))), "x")
  for (i in seq_along(inits)) {
    hu <- inits[[i]][c("b_hutop", "b_hubeta", "b_hunec")]
    names(hu) <- sub("^b_hu", "b_", names(hu))
    surv <- get_init_predictions(hu, sort(parts$hu$x), pred_nec3param, fa)
    expect_gt(min(surv), min(parts$hu$y) / 2)
  }
})

test_that("the boundary inset does not exclude a zero-bounded asymptote", {
  # An observation-based floor can exceed a count asymptote. Check that
  # the inset stays below the generating bot on this zero-inflated design.
  set.seed(2)
  x <- rep(c(0, 1, 2, 4, 8, 16), each = 8)
  mu <- c(20, 18, 12, 5, 1, 0.3)[match(x, c(0, 1, 2, 4, 8, 16))]
  y <- rpois(length(x), mu) * rbinom(length(x), 1, 0.8)
  lim <- init_limits(x, y, zero_bounded = TRUE, support = c(0, Inf))
  expect_gt(lim[1], 0)
  expect_lt(lim[1], 0.3)
  expect_lt(lim[1], min(y[y > 0]))
})

test_that("the spread is not decided by a single repeated observation", {
  # One repeated pair contributes only one variance degree of freedom.
  # Do not let it replace the differences available across the full design.
  set.seed(7)
  x <- 1:24
  y <- 1 - 0.03 * x + rnorm(24, 0, 0.03)
  x2 <- c(x, 12)
  y2 <- c(y, y[12])
  expect_lt(abs(group_spread(x2, y2) / group_spread(x, y) - 1), 0.2)
  xr <- rep(c(0, 1, 5, 20), each = 5)
  yr <- rnorm(20, rep(c(1, 0.8, 0.4, 0.1), each = 5), 0.05)
  v <- vapply(split(yr, factor(xr)), var, numeric(1))
  expect_equal(group_spread(xr, yr), sqrt(mean(v)))
})

test_that("a band with nothing to anchor on rejects every draw", {
  # Return an unusable band when no observations anchor it. An inverted
  # range can be reordered by min()/max() and wrongly accept every draw.
  lim <- init_limits(rep(c(0, 1), each = 3), rep(NA_real_, 6))
  expect_true(all(is.na(lim)))
  expect_false(check_init_predictions(c(1, 0.5, 0.2, 0.1), lim))
  expect_false(check_init_predictions(c(1e9, 1e5, -1e5, -1e9), lim))
})

test_that("the count floor tracks the level means, not the integer scale", {
  # Use level means to obtain a floor below integer granularity. On this
  # design, replication concentrates the floor, so compare its spread as
  # well as its level.
  floor_at <- function(reps, seed) {
    set.seed(seed)
    x <- rep(c(0, 1, 2, 4, 8, 16), each = reps)
    mu <- c(20, 18, 12, 5, 1, 0.3)[match(x, c(0, 1, 2, 4, 8, 16))]
    y <- rpois(length(x), mu) * rbinom(length(x), 1, 0.8)
    init_limits(x, y, zero_bounded = TRUE, support = c(0, Inf))[1]
  }
  seeds <- 1:40
  small <- vapply(seeds, function(s) floor_at(8, s), numeric(1))
  large <- vapply(seeds, function(s) floor_at(80, s), numeric(1))
  expect_lt(median(small), 0.1)
  expect_lt(median(large), 0.1)
  expect_lt(median(large), 0.3)
  # Compare spread as well as level: replication concentrates the floor
  # and reduces all-zero groups without systematically lowering it.
  expect_lt(abs(median(large) - median(small)), 0.02)
  expect_lt(IQR(large), IQR(small))
  expect_lt(sum(large == 0), sum(small == 0))
})

test_that("the inset is no stricter than range(y) where the response reaches in", {
  # The gap is capped at the distance to the closest value the response takes
  # strictly inside the boundary, so on a response that does not touch the
  # boundary the criterion is not tightened past the released one.
  set.seed(22)
  x <- rep(c(0, 1, 5, 20), each = 5)
  y <- pmin(pmax(rep(c(0.9, 0.6, 0.25, 0.05), each = 5) + rnorm(20, 0, 0.03),
                 0.002), 0.998)
  lim <- init_limits(x, y, support = c(0, 1))
  expect_lte(lim[1], min(y))
  expect_gte(lim[2], max(y))
})

test_that("the spread threshold prefers the differences at two degrees of freedom", {
  set.seed(23)
  x <- 1:24
  y <- 1 - 0.03 * x + rnorm(24, 0, 0.03)
  x2 <- c(x, 5, 12)
  y2 <- c(y, y[5] + 0.01, y[12] - 0.01)
  expect_equal(group_spread(x2, y2), successive_difference_spread(x2, y2))
  x3 <- c(x, 5, 12, 18)
  y3 <- c(y, y[5] + 0.01, y[12] - 0.01, y[18] + 0.02)
  expect_false(isTRUE(all.equal(group_spread(x3, y3),
                                successive_difference_spread(x3, y3))))
})

# Initial-value bands for a declared unobserved asymptote.

# Vary only the fraction of the Gaussian nec4param decline observed: hold
# the dose series, replication and rate fixed while placing the threshold.
flat_design <- function(reached, seed = 394, top = 0.9, bot = 0.1,
                        sigma = 0.02) {
  x <- rep(c(0, 0.31, 0.63, 1.25, 2.5, 5, 10, 40), each = 5)
  rate <- 2.5
  nec <- max(x) + log(1 - reached) / rate
  mu <- bot + (top - bot) * exp(-rate * (x - nec) * ifelse(x - nec < 0, 0, 1))
  set.seed(seed)
  list(x = x, y = stats::rnorm(length(mu), mu, sigma), nec = nec)
}

test_that("the band extends to the floor when declared (#394)", {
  d <- flat_design(0.02)
  fam <- validate_family("gaussian")
  yl <- response_link_scale(d$y, fam)
  released <- bayesnec:::init_limits(d$x, yl, zero_bounded = FALSE,
                                     support = bayesnec:::init_support(fam))
  declared <- bayesnec:::init_limits(
    d$x, yl, zero_bounded = FALSE, support = bayesnec:::init_support(fam),
    floor_value = bayesnec:::asymptote_floor(fam, yl))
  expect_gt(released[1], 0)
  expect_lte(declared[1], 0)
  # The upper limit is untouched: the declaration says the asymptote may be
  # lower than the response shows and never that it is higher.
  expect_identical(released[2], declared[2])
})

test_that("a floor never narrows the band (#394)", {
  # A declared floor may extend the band downward but must never raise an
  # existing lower limit.
  d <- flat_design(1 - exp(-5), sigma = 0.15)
  fam <- validate_family("gaussian")
  yl <- response_link_scale(d$y, fam)
  released <- bayesnec:::init_limits(d$x, yl, zero_bounded = FALSE,
                                     support = bayesnec:::init_support(fam))
  declared <- bayesnec:::init_limits(
    d$x, yl, zero_bounded = FALSE, support = bayesnec:::init_support(fam),
    floor_value = 0)
  expect_lt(released[1], 0)
  expect_identical(released, declared)
})

test_that("the band stops short of a bounded family's floor (#394)", {
  # Keep bounded-family initial curves inside support while extending the
  # band below the observed response.
  d <- flat_design(0.02)
  fam <- validate_family("Beta")
  y <- pmin(pmax(d$y, 1e-3), 1 - 1e-3)
  yl <- response_link_scale(y, fam)
  released <- bayesnec:::init_limits(d$x, yl, zero_bounded = FALSE,
                                     support = bayesnec:::init_support(fam))
  declared <- bayesnec:::init_limits(
    d$x, yl, zero_bounded = FALSE, support = bayesnec:::init_support(fam),
    floor_value = bayesnec:::asymptote_floor(fam, yl))
  expect_gt(declared[1], 0)
  expect_lt(declared[1], 0.01)
  expect_lt(declared[1], released[1])
})

test_that("the band admits the curve the declared prior draws (#394)", {
  # Check a fixed curve at the declared floor directly. This isolates whether
  # the band admits such a curve from stochastic initial-value search.
  d <- flat_design(0.02)
  fam <- validate_family("gaussian")
  yl <- response_link_scale(d$y, fam)
  released <- bayesnec:::init_limits(d$x, yl, zero_bounded = FALSE,
                                     support = bayesnec:::init_support(fam))
  declared <- bayesnec:::init_limits(
    d$x, yl, zero_bounded = FALSE, support = bayesnec:::init_support(fam),
    floor_value = bayesnec:::asymptote_floor(fam, yl))
  init <- list(b_top = max(yl), b_bot = 0, b_nec = stats::median(d$x),
               b_beta = 0)
  pars <- c("b_beta", "b_bot", "b_nec", "b_top")
  preds <- bayesnec:::get_init_predictions(
    init, sort(d$x), bayesnec:::pred_nec4param, pars)
  expect_false(bayesnec:::check_init_predictions(preds, released))
  expect_true(bayesnec:::check_init_predictions(preds, declared))
})

test_that("the declared band accepts more of the prior's draws (#394)", {
  # Score one fixed proposal set against both bands. Separate search runs
  # consume different random draws after the first differing acceptance,
  # so their iteration counts are not a paired comparison of band width.
  d <- flat_design(0.02)
  fam <- validate_family("gaussian")
  yl <- response_link_scale(d$y, fam)
  pr <- as.data.frame(suppressWarnings(suppressMessages(
    define_prior("nec4param", fam, d$x, d$y, asymptote_observed = FALSE)
  )))
  pr <- pr[pr$class == "b", ]
  released <- bayesnec:::init_limits(d$x, yl, zero_bounded = FALSE,
                                     support = bayesnec:::init_support(fam))
  declared <- bayesnec:::init_limits(
    d$x, yl, zero_bounded = FALSE, support = bayesnec:::init_support(fam),
    floor_value = bayesnec:::asymptote_floor(fam, yl))
  pars <- c("b_beta", "b_bot", "b_nec", "b_top")
  set.seed(394)
  draws <- bayesnec:::make_inits("nec4param", pars, priors = pr, chains = 200)
  accepted <- function(limits) {
    sum(vapply(draws, function(init) {
      bayesnec:::check_init_predictions(
        bayesnec:::get_init_predictions(init, sort(d$x),
                                        bayesnec:::pred_nec4param, pars),
        limits)
    }, logical(1)))
  }
  expect_gt(accepted(declared), accepted(released))
})

test_that("add_brm_defaults passes the declaration to the search (#394)", {
  # Capture the search argument to check that prior and band construction
  # receive the same asymptote declaration without running a stochastic search.
  d <- flat_design(0.02)
  fam <- validate_family("gaussian")
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    make_good_inits = function(..., asymptote_observed = TRUE) {
      seen$declared <- asymptote_observed
      list(random = "random")
    },
    .package = "bayesnec"
  )
  for (declared in c(TRUE, FALSE)) {
    seen$declared <- NULL
    suppressMessages(suppressWarnings(bayesnec:::add_brm_defaults(
      brm_args = list(chains = 2, iter = 10), model = "nec4param",
      family = fam, predictor = d$x, response = d$y, skip_check = TRUE,
      custom_name = NULL, asymptote_observed = declared)))
    expect_identical(seen$declared, declared)
  }
})
