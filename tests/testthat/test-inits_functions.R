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
  # The family is passed because fit_bayesnec() passes it: it is what tells the
  # search that the mean of this response lives in (0, 1). See #309.
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
  inits <- bayesnec:::make_good_inits("nec4param", x, y,
                                     family = validate_family("Beta"),
                                     priors = priors,
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

test_that("a supplied init is honoured whether or not the data check is run", {
  # add_brm_defaults() used to run the search when `init` was absent OR when
  # skip_check was TRUE, so a caller who supplied initial values under
  # skip_check = TRUE paid for a search -- measured at 577 s on a fixture that
  # cannot be initialised -- and then had what they supplied overwritten by its
  # result. The two conditions answer different questions: whether anyone needs
  # initial values, and whether the data has been checked. See #290.
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
  # Absent, the search still runs on both routes.
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
  # a genuine tank effect, so this exercises an estimated group-level term
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
  # group_spec rather than the old ogl flag: #294 generalised the zero-started
  # intercepts from `ogl` alone to every deviation intercept a term introduces.
  gi <- bayesnec:::group_inits(bf_one, d, Beta(link = "identity"), pr,
                               group_spec = list(nlpars = "ogl", ogl = TRUE))
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
  # Only the distributions whose last argument is a scale are read. gamma() and
  # exponential() carry a rate there and constant() a value, so taking the last
  # number from them returns something that is not a scale. A user-supplied
  # gamma(2, 100) previously produced a starting value of 100.
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
  # An unconstrained mean has no boundary for a group-level offset to cross, so
  # a grouped gaussian fit is left at the brms default; every other family under
  # an identity link restricts mu and gets the raise. A non-identity link is
  # decided by whether the range of its inverse lies inside that support, which
  # is not the same as "any link other than identity is safe": Beta(link =
  # "log") is not, because exp() is unbounded above. See mu_is_constrained(),
  # #245 and #256.
  x <- as.numeric(rep(1:10, each = 5))
  set.seed(245)
  y <- plogis(rnorm(50, 1, 1))
  defaults <- function(family, group) {
    suppressMessages(bayesnec:::add_brm_defaults(
      list(chains = 2), "nec3param", family, x, y,
      skip_check = TRUE, custom_name = NULL, group_spec = group
    ))
  }
  # neclin, deliberately. #257 and #294 apply the deviation multiplicatively
  # wherever they can, and on an equation whose mean is confined by its own
  # parameters that removes the excursion the raise exists to mitigate, and the
  # raise with it. neclin's mean is unbounded below, so no transform is defined
  # for it and the mu-support gate this test is about is still the thing
  # deciding. The equation gate is asserted separately in test-check_priors.R.
  grouped <- list(nlpars = c("top", "slope", "nec"), ogl = FALSE)
  defaults_lin <- function(family, group) {
    suppressMessages(bayesnec:::add_brm_defaults(
      list(chains = 2), "neclin", family, x, y,
      skip_check = TRUE, custom_name = NULL, group_spec = group
    ))
  }

  # constrained mean, grouped: raised
  expect_equal(defaults_lin(validate_family("Beta"), grouped)$control$adapt_delta,
               0.99)
  expect_equal(defaults_lin(validate_family("Gamma"), grouped)$control$adapt_delta,
               0.99)
  expect_equal(
    defaults_lin(validate_family("poisson"), grouped)$control$adapt_delta, 0.99
  )
  # unconstrained mean, grouped: left alone
  expect_null(defaults_lin(validate_family("gaussian"), grouped)$control)
  expect_null(defaults_lin(gaussian(link = "log"), grouped)$control)
  expect_null(defaults_lin(Beta(link = "logit"), grouped)$control)
  # and the ogl case, which #257 transforms, is not raised at all
  expect_null(
    defaults(validate_family("Beta"), list(nlpars = "ogl", ogl = TRUE))$control
  )
  # nor a term on top, bot, nec or beta on an equation whose mean lies between
  # bot and top, which is what #294 adds
  expect_null(defaults(validate_family("Beta"), grouped)$control)
  # constrained mean, ungrouped: left alone, since there is no unconstrained
  # deviation to carry the mean out of range
  expect_null(defaults(validate_family("Beta"), NULL)$control)

  # a control list supplied for another reason keeps its own entries
  both <- suppressMessages(bayesnec:::add_brm_defaults(
    list(chains = 2, control = list(max_treedepth = 12)), "neclin",
    validate_family("Beta"), x, y, skip_check = TRUE, custom_name = NULL,
    group_spec = grouped))
  expect_equal(both$control$max_treedepth, 12)
  expect_equal(both$control$adapt_delta, 0.99)
  # and an adapt_delta the caller chose is theirs, including a lower one
  own <- suppressMessages(bayesnec:::add_brm_defaults(
    list(chains = 2, control = list(adapt_delta = 0.8)), "neclin",
    validate_family("Beta"), x, y, skip_check = TRUE, custom_name = NULL,
    group_spec = grouped))
  expect_equal(own$control$adapt_delta, 0.8)
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
  gi <- bayesnec:::group_inits(bb, d, Beta(link = "identity"), pr,
                               group_spec = list(nlpars = "ogl", ogl = TRUE))
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

test_that("group_inits works for a family whose formula carries trials()", {
  # The regression for the binomial half of the make_standata() query. The
  # dimensions do not depend on the family, so the call was made with
  # gaussian() to survive a Beta response carrying exact zeros and ones -- but
  # `trials` is not a valid aterm for gaussian, so every binomial and
  # beta_binomial fit took the error path instead, got an empty init list, and
  # kept the whole of #245 for two of the three bounded families the fix is
  # for. The fit's own family is asked first now, gaussian() only as a
  # fallback, and neither case can take the other's failure.
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
  # The end-to-end counterpart of the test above, and the case NEWS names but
  # the first version of the fix did not actually reach.
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
  # define_prior() returns early for a hurdle family, so the group priors are
  # added on a separate branch. Pinned end to end rather than at prior level
  # only: the branch has its own response (survivors, link-scaled) and its own
  # route through group_inits(), and neither is exercised by the unit tests.
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
  # The companion to the trials() case, and the reason the fix is a fallback
  # rather than a special case for trials. rate() arrived on dev under #136,
  # after the group-level work was written, and is valid for poisson and
  # negbinomial only -- so a gaussian()-only query to make_standata() fails on
  # it in exactly the same way. Asking the fit's own family first covers it
  # without knowing it exists. Confirmed that the gaussian()-only form does
  # error here, so this is a live case rather than a hypothetical one.
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
  # A wall-clock bound was tried for #266 and removed: it made the number of
  # attempts, and so the initial values, and so the fit, a function of machine
  # load. Two of these assertions exist to stop it coming back.
  expect_false("max_seconds" %in% names(formals(make_good_inits)))
  expect_equal(formals(make_good_inits)$n_trials, quote(1e4))
  # report_after says the search is still running; it must not end it.
  expect_true("report_after" %in% names(formals(make_good_inits)))
})

test_that("the search is deterministic given a seed", {
  # The property the wall-clock bound broke, and the reason it was removed: two
  # runs with the same seed must agree whatever else the machine is doing. A
  # time-bounded search does fewer attempts under load, so a busy machine got
  # different initial values -- and therefore a different fit -- from an idle
  # one. Asserted on the result rather than on the absence of the argument,
  # which the sibling test above covers, because it is the behaviour that
  # matters.
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

test_that("a long search says it is still running", {
  # The actual complaint in #266: 561 seconds with no output, which a user
  # cannot tell from a hang. report_after = 0 makes the notice fire on the
  # first pass, so the assertion is on the mechanism rather than on a
  # wall-clock reading, which would be load-sensitive.
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
  # and it still falls back when the cap is reached
  expect_match(msg, "failed to find initial values")
})

# #302: the sampling table was written out at each of the three places that
# needed it, and lognormal was in none of them. lognormal is now the default
# prior for nec on a predictor supplied on the dose scale, so every fit draws
# initial values from one, and it is drawn under truncation to the tested range.

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
  # Previously fcts[[dist]] was NULL and the call failed with "attempt to apply
  # non-function", naming neither the prior nor the distribution.
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
  # prior_string() never writes one, but a user assembling a data frame by hand
  # can, and the parsed name reached the lookup untrimmed.
  set.seed(302)
  out <- bayesnec:::make_inits("nec4param",
                               c("b_top", "b_beta", "b_bot", "b_nec"),
                               priors = lognormal_prior_df(" lognormal(0, 1)"),
                               chains = 1)
  expect_gt(as.numeric(out[[1]]$b_nec), 0)
})

# #309: the initial-value search required all chains to pass at the same time,
# and tested the initial curve against range(y).

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
  # One aberrant control observation raises max(y) by the whole of it and the
  # control mean by a sixth of it. The band widens as well, because a variance
  # estimate is not robust either and the observation is evidence that the
  # response varies more than the other groups suggested -- but that is the
  # spread responding, not the reference.
  x <- rep(c(0, 1, 5, 20), each = 6)
  set.seed(309)
  y <- c(rnorm(6, 1, 0.02), rnorm(6, 0.8, 0.02), rnorm(6, 0.4, 0.02),
         rnorm(6, 0.1, 0.02))
  y2 <- y
  y2[1] <- 3
  ref <- function(z) regularizing_location(x, z, "top")[["location"]]
  expect_lt(ref(y2) - ref(y), (3 - y[1]) / 6 + 1e-8)
  expect_gt(max(y2) - max(y), 1.9)
  # The band widens, because a variance estimate follows an outlier too. That
  # is the permissive direction -- the spiked band contains the clean one, so
  # every starting value the clean band accepted is still accepted -- and it is
  # the reason the assertion is on containment rather than on width.
  clean <- init_limits(x, y)
  spiked <- init_limits(x, y2)
  expect_lte(spiked[1], clean[1])
  expect_gte(spiked[2], clean[2])
})

test_that("the band does not get looser as replicates are added", {
  # range(y) drifts outward with sample size, so the released check was more
  # permissive on the larger design. The band is a mean plus a spread and is
  # not a function of n in that way.
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
  # A curve whose asymptote sits at a level the design recorded is not a
  # starting value the search should reject. That series is heteroscedastic and
  # is not monotone at its lower end, which is why every level mean is a centre
  # of the band and not only the two anchors read at the ends of the series.
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
  # The released rule required four chains to pass at the same time and re-drew
  # the complete set when any one failed, which raises the number of proposals
  # to the fourth power of the per-chain rate. Both rules are run here under
  # the same band so the comparison is of the structure alone.
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
  # The released rule, reimplemented here so the comparison does not depend on
  # a second working tree.
  released_drawn <- function(seed, cap = 2000) {
    set.seed(seed)
    n <- 0
    passed <- FALSE
    while (!passed && n < cap) {
      n <- n + 1
      passed <- all(vapply(make_inits("nec3param", fa, pr, 4), ok, logical(1)))
    }
    # The success flag is returned with the count, so a capped run is not read
    # as a measured one: at this per-chain rate the released rule usually does
    # not succeed inside the cap at all.
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
  # Both counts are measurements rather than floors: the released rule does
  # succeed inside the cap here, so the ratio is a like-for-like comparison of
  # the two structures. The sibling test below uses ecxlin, where it does not.
  expect_true(all(a["ok", ] == 1))
})

test_that("only the empty chain slots are re-drawn", {
  # The property that makes the change free: an accepted chain is kept, so the
  # search draws the number of proposals a per-chain rate implies rather than
  # its fourth power.
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
  # Four chains at a per-chain rate near a fifth need of the order of tens of
  # proposals. The released rule drew four per round and needed thousands of
  # rounds on this series.
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
  # Under the identity link bnec() assigns, an initial curve outside the
  # interval the likelihood permits is invalid rather than merely poor: Stan
  # rejects it and the fit ends on "Initialization failed". range(y) kept the
  # curve inside the support by accident, because a response is inside its own
  # support; a band built from a location and a spread has no such guarantee.
  x <- rep(c(0, 1, 5, 20), each = 5)
  set.seed(162)
  y <- pmin(pmax(rep(c(0.98, 0.8, 0.4, 0.1), each = 5) + rnorm(20, 0, 0.05),
                 0.001), 0.999)
  unconstrained <- init_limits(x, y)
  bounded <- init_limits(x, y, support = mu_support(validate_family("Beta")))
  expect_gt(unconstrained[2], 1)
  # Inside the support, and inside it strictly: where the band reaches the
  # boundary it stops at the nearest value the response takes.
  expect_lt(bounded[2], 1)
  expect_gt(bounded[1], 0)
  # a tenth of the way from the boundary towards the nearest observed value, so
  # outside the observed range and inside the support
  expect_gte(bounded[2], max(y))
  expect_lte(bounded[1], min(y))
  # and gaussian is not bounded, so the band is left alone
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
  # ecxlin on the alga c_proliferum contaminant A series is one of the searches
  # that reached n_trials under the released rule and fell through to Stan's
  # own initialisation. Both rules are run here, each under its own criterion,
  # because that is the pair of states the change moves between.
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
    # Reached the cap without succeeding, rather than succeeding on the last
    # round, which expect_gte() alone would also admit.
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
  # Two rewrites stand between the family name and the tag the response-scaled
  # priors are keyed on, and both were inline in define_prior() where the init
  # search could not read them. Reading family$family directly instead broke
  # the zero-inflated counts outright and reintroduced the #229 failure for a
  # log link, so each is asserted here rather than left to the caller.
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
  # The failure the tag rewrite prevents: with no gamma-scaled entry the NA
  # reaches brms as "Cannot coerce 'prior' to a single character value", and
  # neither zero-inflated count family can be fitted at all.
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
  # pred_<model>() returns the linear predictor and the band is built from the
  # response on the link scale, so clamping to the support of the mean is only
  # correct under the identity link. Under logit it would admit means between
  # 0.5 and 0.73 alone.
  expect_equal(init_support(validate_family("Beta")), c(0, 1))
  expect_equal(init_support(brms::Beta(link = "logit")), c(-Inf, Inf))
  expect_equal(init_support(stats::Gamma(link = "log")), c(-Inf, Inf))
  expect_equal(init_support(validate_family("Gamma")), c(0, Inf))
  expect_equal(init_support(validate_family("gaussian")), c(-Inf, Inf))
})

test_that("an unreplicated design uses the successive differences, not sd(y)", {
  # sd(y) grows with the size of the effect, which is the reason it is not the
  # spread on a replicated design either. The successive differences of a
  # smooth curve have twice the noise variance.
  x <- c(0, 0.5, 1, 2, 4, 8, 16, 32)
  y <- c(0.98, 0.95, 0.88, 0.70, 0.42, 0.20, 0.08, 0.04)
  expect_equal(group_spread(x, y), successive_difference_spread(x, y))
  expect_lt(group_spread(x, y), sd(y) / 3)
  # and the band it gives is a small multiple of the response range, where
  # sd(y) gave more than four times it
  expect_lt(diff(init_limits(x, y)) / diff(range(y)), 2)
  # fewer than three observations leave too few differences to read
  expect_equal(group_spread(c(1, 2), c(1, 2)), sd(c(1, 2)))
})

test_that("make_good_inits requires the family", {
  # A default would silently drop the support clamp, which is the constraint
  # range(y) supplied for free.
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
  # Named in #309 as one of the equations that exhausted the cap and fell
  # through to Stan's initialisation on this series.
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
  # A mean at the boundary is one the likelihood cannot evaluate. Where the
  # band reaches it the band stops at the nearest value the response takes, so
  # that a bounded response cannot be started at a mean no design resolves.
  x <- rep(c(0, 1, 5, 20), each = 5)
  set.seed(164)
  y <- pmin(pmax(rep(c(0.98, 0.6, 0.2, 0.02), each = 5) + rnorm(20, 0, 0.05),
                 0.002), 0.998)
  lim <- init_limits(x, y, support = c(0, 1))
  expect_gt(lim[1], 0)
  expect_lt(lim[2], 1)
  # it never crosses a level mean, so every level the design measured is still
  # inside the band
  gm <- vapply(split(y, factor(x)), mean, numeric(1))
  expect_true(all(gm >= lim[1] & gm <= lim[2]))
  # and an unbounded support is left alone
  expect_equal(init_limits(x, y), init_limits(x, y, support = c(-Inf, Inf)))
})

test_that("the second block of a hurdle fit is not started at zero survival", {
  # The second block is primed from one survival proportion per concentration,
  # which is an unreplicated series of a few points, so the spread read from its
  # successive differences is a third of the response range and the band spans
  # the whole of (0, 1). A curve accepted at a survival of 1e-66 makes the joint
  # log likelihood -Inf and the fit ends on "Initialization failed".
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
  # Two orders of magnitude above the 1e-66 that failed, and below the smallest
  # proportion the design records, so the band still contains every level.
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
  # Stopping the band at the nearest observed value was measured and is too
  # strict on a count response with structural zeros: it put the floor above the
  # generating bot, which is the error the width exists to avoid. A tenth of the
  # distance from the boundary keeps the floor below anything the design
  # resolves.
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
  # One repeated predictor value contributes a variance on one degree of
  # freedom. Preferring it to the differences the rest of the design offers made
  # the band a function of that one pair.
  set.seed(7)
  x <- 1:24
  y <- 1 - 0.03 * x + rnorm(24, 0, 0.03)
  x2 <- c(x, 12)
  y2 <- c(y, y[12])
  expect_lt(abs(group_spread(x2, y2) / group_spread(x, y) - 1), 0.2)
  # and with enough replication the pooled estimate is used
  xr <- rep(c(0, 1, 5, 20), each = 5)
  yr <- rnorm(20, rep(c(1, 0.8, 0.4, 0.1), each = 5), 0.05)
  v <- vapply(split(yr, factor(xr)), var, numeric(1))
  expect_equal(group_spread(xr, yr), sqrt(mean(v)))
})

test_that("a band with nothing to anchor on rejects every draw", {
  # check_init_predictions() reads the band through min() and max(), which
  # reorder it, so an inverted pair is read as the whole line and accepts
  # everything. range(y) on an all-missing response did exactly that.
  lim <- init_limits(rep(c(0, 1), each = 3), rep(NA_real_, 6))
  expect_true(all(is.na(lim)))
  expect_false(check_init_predictions(c(1, 0.5, 0.2, 0.1), lim))
  expect_false(check_init_predictions(c(1e9, 1e5, -1e5, -1e9), lim))
})

test_that("the boundary floor falls as a count design gains replicates", {
  # Read from the level means and not from the observations. min(y[y > 0]) is 1
  # for any integer response, so an observation-based floor is the same for
  # eight replicates as for eight hundred and says nothing about the design.
  gen <- function(reps) {
    set.seed(21)
    x <- rep(c(0, 1, 2, 4, 8, 16), each = reps)
    mu <- c(20, 18, 12, 5, 1, 0.3)[match(x, c(0, 1, 2, 4, 8, 16))]
    y <- rpois(length(x), mu) * rbinom(length(x), 1, 0.8)
    init_limits(x, y, zero_bounded = TRUE, support = c(0, Inf))[1]
  }
  small <- gen(8)
  large <- gen(80)
  expect_gt(small, 0)
  expect_lt(large, small)
  # and it stays below a generating asymptote the released criterion admitted
  expect_lt(small, 0.3)
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
  # two tied pairs give two degrees of freedom, which is below the threshold
  x2 <- c(x, 5, 12)
  y2 <- c(y, y[5] + 0.01, y[12] - 0.01)
  expect_equal(group_spread(x2, y2), successive_difference_spread(x2, y2))
  # a third takes it to three, and the pooled estimate is used
  x3 <- c(x, 5, 12, 18)
  y3 <- c(y, y[5] + 0.01, y[12] - 0.01, y[18] + 0.02)
  expect_false(isTRUE(all.equal(group_spread(x3, y3),
                                successive_difference_spread(x3, y3))))
})
