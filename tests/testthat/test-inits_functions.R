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

# #207 part 1: a prior on the family's own dispersion parameter -- sigma, shape,
# phi -- used to kill make_inits() outright, because the name check compared the
# prior's parameter names against the *curve's* arguments as an exact set. A
# user therefore could not regularise dispersion at all.

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
  # Deliberate: Stan random-initialises any parameter absent from an init list,
  # and bayesnec has never given sigma an init. The prior still reaches brm();
  # only the init search ignores it.
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

# #244: a constant() prior fixes a parameter, but has no entry in the sampling
# table make_inits() looks distributions up in, so the whole init search died
# with "attempt to apply non-function". Fixing a parameter then required the
# user to hand-write an `init` list for every *other* parameter in order to
# skip the search, which is what example7 (#193) had to do.

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
  # a non-zero constant is carried through as itself, not coerced
  out2 <- bayesnec:::make_inits("nec4param", fct_args,
                               priors = const_prior_df("constant(0.5)"),
                               chains = 2)
  expect_equal(as.numeric(out2[[1]]$b_bot), 0.5)
})

test_that("a constant outside its own bounds does not hang", {
  # The bound-respecting redraw loops until the value falls inside lb/ub. A
  # constant cannot be redrawn, so without the branch this spins forever --
  # a hang rather than an error, which is why it is tested explicitly.
  out <- bayesnec:::make_inits(
    "nec4param", c("b_top", "b_beta", "b_bot", "b_nec"),
    priors = const_prior_df("constant(0)", lb = "1", ub = "10"), chains = 2
  )
  expect_equal(as.numeric(out[[1]]$b_bot), 0)
})

test_that("the fixed value is kept for the curve check, not dropped", {
  # make_good_inits() evaluates the candidate curve, and a parameter fixed at
  # bot = 0 is genuinely part of that curve. Dropping the constant here -- the
  # obvious reading of "skip constant priors" -- makes every candidate fail the
  # range check and sends the search to Stan's defaults after 10,000 trials.
  x <- as.numeric(rep(1:10, each = 5))
  set.seed(42)
  y <- 3 * exp(-exp(-0.5) * pmax(x - 4, 0)) + rnorm(length(x), 0, 0.1)
  priors <- bayesnec:::define_prior("nec4param",
                                    validate_family("gaussian"), x, y)
  priors$prior[priors$nlpar == "bot"] <- "constant(0)"
  inits <- bayesnec:::make_good_inits("nec4param", x, y, priors = priors,
                                      chains = 2, seed = 42)
  expect_false(is.character(inits))   # i.e. not the "random" fallback
  expect_true("b_bot" %in% names(inits[[1]]))
  expect_equal(as.numeric(inits[[1]]$b_bot), 0)
})

test_that("refine_inits skips a parameter that is fixed", {
  # refine_inits() re-draws slope/d/beta from its own copy of the sampling
  # table, so a constant on one of those hit the identical error.
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
  # brms carries a constant parameter into the draws as a zero-variance column
  # and fixef() reports it, so extract_pars() finds it and expand_nec() keeps
  # the model in the nec class. If that ever changed, extract_pars() would
  # return NA, expand_nec() would fall through to mod_class <- "ecx", and the
  # reported NEC would silently become an NSEC -- a wrong answer with nothing
  # to signal it. Pinned here because #244 makes fixing `nec` a one-liner.
  fef <- matrix(c(4, 4, 4, 3.04, 2.9, 3.2), nrow = 2, byrow = TRUE,
                dimnames = list(c("nec_Intercept", "top_Intercept"),
                                c("Estimate", "Q2.5", "Q97.5")))
  local_mocked_bindings(fixef = function(...) fef, .package = "bayesnec")
  out <- bayesnec:::extract_pars("nec", structure(list(), class = "brmsfit"))
  expect_false(identical(out, NA))
  expect_equal(unname(out["Estimate"]), 4)
  # and the zero-width interval a fixed parameter has is not read as missing
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
  # Stan moves a constant parameter out of its `parameters` block, so an init
  # for it has nothing to initialise. Both backends currently accept such an
  # init and ignore it, so this pins a deliberate choice rather than a
  # constraint that binds: bayesnec does not send brm() an init for a parameter
  # Stan does not declare. The value is carried through the search and removed
  # only here, in add_brm_defaults().
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
  # the prior itself must still reach brm(); only the init is dropped
  expect_true("constant(0)" %in% out$prior$prior)
})
# --- #244 x #148: the two halves of the constant-prior NA ---------------------
# brms carries a parameter fixed by constant() into the draws as a zero-variance
# column, and posterior returns NA for it. Before #148 Part D that NA reached
# `if (all(failed))` in rhat.bayesmanecfit and errored outright, and reached
# `failed` in check_sampling as an NA that made screen_models report a drop it
# had not performed. Part D excludes zero-variance parameters from the screen;
# this is the end-to-end case, which needs both halves to exist -- the fit needs
# the constant() support added here, the pass needs Part D's exclusion.

test_that("a fixed parameter does not break rhat on a multi-model fit", {
  skip_on_cran()
  set.seed(244)
  x <- rep(seq(0, 5, length.out = 20), 3)
  y <- 3 * exp(-exp(-0.5) * pmax(x - 2, 0)) + rnorm(length(x), 0, 0.1)
  d <- data.frame(x = x, y = y)
  # nec4param and ecx4param, not nec3param: nec3param is dropped for a Gaussian
  # response, and this needs two candidates to reach rhat.bayesmanecfit.
  f <- y ~ crf(x, model = c("nec4param", "ecx4param"))
  p <- lapply(get_priors(f, data = d, family = gaussian()), function(z) {
    z$prior[z$nlpar == "top"] <- "constant(3)"
    z
  })
  fit <- suppressWarnings(suppressMessages(
    bnec(f, data = d, family = gaussian(), prior = p, chains = 2, iter = 600,
         warmup = 300, seed = 244, open_progress = FALSE, refresh = 0)
  ))
  skip_if_not(is_bayesmanecfit(fit), "both candidates were needed for this test")

  # the error this used to raise was `if (all(failed))` on an NA
  r <- expect_silent(rhat(fit, rhat_cutoff = 99))
  verdicts <- vapply(r, "[[", logical(1), "failed")
  expect_false(anyNA(verdicts))
  # the fixed parameter is out of the screen rather than in it as an NA
  expect_false(anyNA(r[[1]]$rhat_vals))
  expect_false("top" %in% names(r[[1]]$rhat_vals))

  # and the same on the check_sampling side
  tab <- check_sampling(fit, rhat_cutoff = 99, ess_cutoff = 0,
                        divergence_cutoff = 1e6)
  expect_false(anyNA(tab$failed))
  expect_false(anyNA(tab$max_rhat))
  expect_false(any(tab$failed))
  # screen_models must be a genuine no-op here, not a silent one
  expect_message(
    out <- screen_models(fit, rhat_cutoff = 99, ess_cutoff = 0,
                         divergence_cutoff = 1e6),
    "candidate models passed"
  )
  expect_equal(length(out$mod_fits), length(fit$mod_fits))
})

# --- #245: the ogl offset must not reach the init name check ------------------
# make_inits() tests exact set equality between the prior's parameter names and
# the curve's own arguments. The class filter added by #207/#231 already drops
# an sd row, but the ogl offset carries class "b" and survives it, so the check
# rejected the whole set -- which is what stopped a user supplying by hand the
# group-level prior that was never generated. Filtered in add_brm_defaults(),
# alongside the dispersion parameters, for the same reason: neither plays any
# part in getting the mean curve inside the response range.

test_that("a group-level prior does not make the init search reject the set", {
  x <- as.numeric(rep(1:10, each = 5))
  set.seed(245)
  y <- plogis(3 * exp(-exp(-0.5) * pmax(x - 4, 0)) + rnorm(length(x), 0, 0.1))
  priors <- bayesnec:::define_prior(
    "nec4param", validate_family("Beta"), x, y,
    group_spec = list(nlpars = "ogl", ogl = TRUE)
  )
  # both rows are present in what reaches brm()
  expect_true(any(priors$class == "sd" & priors$nlpar == "ogl"))
  expect_true(any(priors$class == "b" & priors$nlpar == "ogl"))
  out <- suppressMessages(
    bayesnec:::add_brm_defaults(list(prior = priors, chains = 2, seed = 245),
                                "nec4param", validate_family("Beta"), x, y,
                                skip_check = FALSE, custom_name = NULL)
  )
  # the init search ran and produced values for the curve parameters only
  expect_setequal(names(out$init[[1]]), c("b_top", "b_bot", "b_beta", "b_nec"))
  # and the group-level priors still reach brm() untouched
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
  # The end-to-end case for #245, and the one that matters: the prior fix alone
  # does NOT get this far. Stan initialises a lower-bounded sd as
  # exp(uniform(-2, 2)) whatever prior is declared, and the ogl offset is
  # unbounded, so without initial values of their own the mean starts outside a
  # (0, 1) response's support and brm() returns a fit with no draws. Verified
  # by running it both ways: priors only failed with "Initialization failed",
  # priors plus inits sampled.
  skip_on_cran()
  set.seed(245)
  n_tank <- 12
  x <- rep(seq(0, 4, length.out = 15), each = 4)
  tank <- factor(rep(seq_len(n_tank), length.out = length(x)))
  # a real tank effect, so this tests a grouped model rather than a funnel
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
  # a fit that failed to initialise carries no draws at all
  expect_gt(brms::ndraws(fit$fit), 0)
  # the group-level standard deviation was estimated, not merely declared
  expect_true(any(grepl("^sd_tank", brms::variables(fit$fit))))
})

test_that("group_inits reads its indices from the model brms will build", {
  # brms numbers group-level terms by its own internal ordering: one pgl term
  # over four parameters becomes four separately indexed terms, not one.
  # Guessing that ordering would silently mismatch the initial values, so the
  # dimensions come from make_standata().
  set.seed(245)
  d <- data.frame(x = runif(60, 0, 4), y = runif(60, 0.1, 0.9),
                  tank = factor(rep(1:12, 5)))
  bf_one <- brms::bf(
    y ~ ogl + bot + (top - bot) * exp(-exp(beta) * (x - nec) * step(x - nec)),
    ogl ~ 1 + (1 | tank), bot ~ 1, top ~ 1, beta ~ 1, nec ~ 1, nl = TRUE
  )
  pr <- brms::prior_string("student_t(3, 0, 0.08)", class = "sd",
                           nlpar = "ogl")
  gi <- bayesnec:::group_inits(bf_one, d, Beta(link = "identity"), pr,
                               ogl = TRUE)
  expect_setequal(names(gi), c("sd_1", "z_1", "b_ogl"))
  expect_equal(dim(gi$z_1), c(1L, 12L))
  expect_true(all(gi$z_1 == 0))
  expect_equal(as.numeric(gi$b_ogl), 0)
  expect_equal(as.numeric(gi$sd_1), 0.08)

  # a group-level term on two parameters gets two indices, not one
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
  # no sd rows, and a set with no class column at all, both give nothing
  expect_length(bayesnec:::sd_prior_scales(
    brms::prior_string("normal(0, 5)", nlpar = "beta")), 0)
  expect_length(bayesnec:::sd_prior_scales(NULL), 0)
})

test_that("a constant ogl intercept gets no initial value", {
  # Fixing the ogl intercept at zero is the clean way to remove its confounding
  # with top and bot. Stan then does not declare b_ogl, so an init for it has
  # nothing to initialise. add_brm_defaults() strips inits for constant
  # parameters before the group inits are appended, so fit_bayesnec() repeats
  # the strip. Same hygiene as #244.
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
  gi <- bayesnec:::group_inits(bb, d, Beta(link = "identity"), pr, ogl = TRUE)
  # group_inits itself is not prior-aware; the strip happens in fit_bayesnec
  expect_true("b_ogl" %in% names(gi))
  const <- as.data.frame(pr)
  keep <- !names(gi) %in% paste0("b_", const$nlpar[
    bayesnec:::is_constant_prior(const$prior) & const$class == "b" &
      nzchar(const$nlpar)])
  expect_false("b_ogl" %in% names(gi[keep]))
  # and the group-level terms themselves still get theirs
  expect_true(all(c("sd_1", "z_1") %in% names(gi[keep])))
})

test_that("group_inits works when the response is invalid for the fit's family", {
  # The regression for the bug that hid inside this function's own error
  # handling. group_inits() asks make_standata() for the group-level
  # dimensions, and make_standata() validates the response against the family's
  # support. A Beta response still carrying exact zeros and ones -- which is
  # what reaches here before check_data() has nudged them -- made that call
  # error, the try() turned it into an empty init list, and the fit then failed
  # to initialise for a reason nothing reported. The dimensions do not depend
  # on the family at all, so the query uses gaussian() and cannot fail this way.
  set.seed(245)
  d <- data.frame(x = rep(seq(0, 4, length.out = 15), 4),
                  colony = factor(rep(1:5, 12)))
  # exact 0 and 1 present, as the coral live-tissue response has
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
  # pgl over four parameters gives four separately indexed group-level terms
  expect_setequal(names(gi), c("sd_1", "z_1", "sd_2", "z_2",
                               "sd_3", "z_3", "sd_4", "z_4"))
  expect_true(all(vapply(gi[grep("^z_", names(gi))],
                         function(z) all(z == 0), logical(1))))
  expect_equal(unname(vapply(gi[grep("^z_", names(gi))],
                             function(z) ncol(z), integer(1))),
               rep(5L, 4))
})

test_that("pgl on a bounded family initialises and samples", {
  # The end-to-end counterpart: pgl() is the case the silent-empty-inits bug
  # actually broke, and it broke on a response carrying exact zeros and ones.
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
