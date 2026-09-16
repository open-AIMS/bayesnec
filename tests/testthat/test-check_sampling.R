# Part D of #148 — sampler diagnostics and screening. The spec is
# notes/tasks/148-model-fit-diagnostics.md; the decisions there are settled and
# these tests pin the ones that are easy to regress.

test_that("check_sampling returns one row per candidate with all columns", {
  skip_on_cran()
  tab <- check_sampling(manec_example)
  expect_s3_class(tab, "data.frame")
  expect_setequal(tab$model, manec_example$success_models)
  expect_named(tab, c("model", "max_rhat", "min_ess", "min_ess_ratio",
                      "n_divergent", "failed"))
  expect_type(tab$failed, "logical")
})

test_that("min_ess is neff_ratio times ndraws, not a ratio", {
  # D2, settled: report the absolute so that `ess_cutoff = 400` is directly
  # Vehtari's "both bulk and tail exceed 100 per chain" at four chains, with no
  # arithmetic left to the user. The ratio is reported beside it so that
  # "passed because we drew 8000" can be told from "passed efficiently".
  skip_on_cran()
  tab <- check_sampling(manec_example)
  fit <- bayesnec:::pull_brmsfit(
    suppressMessages(pull_out(manec_example, model = "nec4param"))
  )
  # The screened vector, not the raw one -- prior_* draws are excluded (see the
  # exclusion tests below). They are iid draws, so their ESS is close to ndraws
  # and they are never the minimum in practice; asserted here so that a change
  # to the exclusion set shows up as a failure rather than passing by luck.
  ratio <- min(bayesnec:::screenable_pars(brms::neff_ratio(fit)))
  expect_equal(ratio, min(brms::neff_ratio(fit)))
  expect_equal(tab$min_ess[tab$model == "nec4param"],
               ratio * brms::ndraws(fit))
  expect_equal(tab$min_ess_ratio[tab$model == "nec4param"], ratio)
  # and the two are consistent with each other
  expect_equal(tab$min_ess / tab$min_ess_ratio,
               rep(brms::ndraws(fit), nrow(tab)))
})

test_that("divergences come from nuts_params, which is backend-agnostic", {
  # Deliberately not rstan::get_num_divergent(x$fit): rstan is Suggests-only
  # and that route reaches past brms into the stanfit slot, so it fails under
  # cmdstanr. Asserted because it is the obvious thing to reach for.
  skip_on_cran()
  tab <- check_sampling(manec_example)
  fit <- bayesnec:::pull_brmsfit(
    suppressMessages(pull_out(manec_example, model = "nec4param"))
  )
  np <- brms::nuts_params(fit, pars = "divergent__")
  expect_equal(tab$n_divergent[tab$model == "nec4param"], sum(np$Value))
})

test_that("every threshold is an argument and each can fail a model alone", {
  skip_on_cran()
  # manec_example fails Rhat and ESS at the defaults
  expect_true(all(check_sampling(manec_example)$failed))
  # relax everything: nothing fails
  loose <- check_sampling(manec_example, rhat_cutoff = 99, ess_cutoff = 0,
                          divergence_cutoff = 1e6)
  expect_false(any(loose$failed))
  # each threshold in isolation
  expect_true(any(check_sampling(manec_example, rhat_cutoff = 1.01,
                                 ess_cutoff = 0,
                                 divergence_cutoff = 1e6)$failed))
  expect_true(any(check_sampling(manec_example, rhat_cutoff = 99,
                                 ess_cutoff = 400,
                                 divergence_cutoff = 1e6)$failed))
})

test_that("check_sampling works on a single bayesnecfit", {
  skip_on_cran()
  one <- suppressMessages(pull_out(manec_example, model = "nec4param"))
  tab <- check_sampling(one)
  expect_equal(nrow(tab), 1)
  expect_equal(tab$model, "nec4param")
})

test_that("check_sampling refuses an object it cannot diagnose", {
  expect_error(check_sampling(1:10), "bayesnecfit, a bayesmanecfit")
})

test_that("the cutoffs must be scalars", {
  # chk_number, not chk_numeric: the latter admits a vector of any length, and
  # a length-2 cutoff silently recycles into the comparison.
  skip_on_cran()
  expect_error(check_sampling(manec_example, rhat_cutoff = c(1.01, 1.05)))
  expect_error(check_sampling(manec_example, ess_cutoff = c(400, 500)))
  expect_error(summary(manec_example, rhat_cutoff = c(1.01, 1.05)))
})

# --- what the screen reduces over --------------------------------------------

test_that("screenable_pars drops prior_* draws and keeps lp__ and lprior", {
  # bnec() forces sample_prior = "yes", so every fit carries an independent
  # draw from the prior for every parameter. Their Rhat is Monte Carlo noise
  # about a distribution the sampler never explored, and at the 1.01 cutoff
  # they are enough to fail a model on a variable that is not in the model.
  x <- c(b_top_Intercept = 1.002, b_nec_Intercept = 1.004,
         prior_b_top = 1.023, prior_sigma = 1.031,
         lprior = 1.006, lp__ = 1.001)
  out <- bayesnec:::screenable_pars(x)
  expect_named(out, c("b_top_Intercept", "b_nec_Intercept", "lprior", "lp__"))
  expect_equal(max(out), 1.006)
})

test_that("screenable_pars drops parameters with no variance", {
  # posterior::rhat() and ess_bulk() both return NA for a zero-variance column,
  # which is what a constant() prior produces (#244). A parameter fixed at a
  # known value has nothing to converge to, so it leaves the screen rather than
  # entering it as an NA for every caller downstream to guard against.
  x <- c(b_top_Intercept = NA_real_, b_nec_Intercept = 1.004, lp__ = 1.001)
  out <- bayesnec:::screenable_pars(x)
  expect_named(out, c("b_nec_Intercept", "lp__"))
  expect_false(anyNA(out))
})

test_that("failed is a logical, never NA, even with nothing to assess", {
  # The whole point of the screen is that silence means a pass. An NA verdict
  # skips the "all passed" branch, drops nothing, and still reports a drop.
  skip_on_cran()
  tab <- check_sampling(manec_example)
  expect_type(tab$failed, "logical")
  expect_false(anyNA(tab$failed))
  # and the degenerate case: nothing assessable is not a failure, but it is
  # visible in the diagnostics rather than folded into the verdict
  expect_true(is.na(bayesnec:::max_or_na(numeric(0))))
  expect_true(is.na(bayesnec:::min_or_na(numeric(0))))
})

test_that("max_rhat excludes the prior draws on a real fit", {
  skip_on_cran()
  fit <- manec_example$mod_fits[["ecx4param"]]$fit
  raw <- brms::rhat(fit)
  expect_true(any(grepl("^prior_", names(raw))))
  tab <- check_sampling(manec_example)
  expect_equal(tab$max_rhat[tab$model == "ecx4param"],
               max(raw[!grepl("^prior_", names(raw))]))
})

test_that("check_sampling delegates over both components of a hurdle fit", {
  skip_on_cran()
  h <- structure(list(growth = manec_example, survival = manec_example),
                 class = c("bayesnechurdlefit", "bnecfit"))
  out <- check_sampling(h)
  expect_named(out, c("growth", "survival"))
  expect_s3_class(out$growth, "data.frame")
  expect_equal(out$growth, check_sampling(manec_example))
})

# --- screen_models: the three cases the spec enumerates -----------------------

test_that("case 1 — nothing failed: unchanged, amend not called", {
  skip_on_cran()
  # Assigned inside expect_message(), not around it: expect_message() returns
  # the message condition, not the value of the expression.
  expect_message(
    out <- screen_models(manec_example, rhat_cutoff = 99, ess_cutoff = 0,
                         divergence_cutoff = 1e6),
    "All 2 candidate models passed"
  )
  expect_s3_class(out, "bayesmanecfit")
  expect_equal(length(out$mod_fits), length(manec_example$mod_fits))
})

test_that("case 2 — some failed: the failures are dropped", {
  # No fixture in the package has a mixed pass/fail set, so the cutoff is put
  # between the two models' Rhats (1.213 and 1.088) rather than building one.
  skip_on_cran()
  suppressWarnings(expect_message(
    out <- screen_models(manec_example, rhat_cutoff = 1.15, ess_cutoff = 0,
                         divergence_cutoff = 1e6),
    "Dropping 1 of 2"
  ))
  # a two-model set reduced to one becomes a bayesnecfit, as amend() does
  expect_s3_class(out, "bayesnecfit")
  expect_equal(out$model, "ecx4param")
})

test_that("case 3 — everything failed: a useful error, not amend's", {
  skip_on_cran()
  expect_error(
    suppressMessages(screen_models(manec_example)),
    "nothing left to return"
  )
  # and specifically NOT amend()'s bare message
  msg <- tryCatch(suppressMessages(screen_models(manec_example)),
                  error = function(e) conditionMessage(e))
  expect_false(grepl("All models removed, nothing to return", msg, fixed = TRUE))
})

test_that("a failing bayesnecfit gets the screen's error, not amend's", {
  # The all-failed case is decided from the table before amend() is called, so
  # amend.bayesnecfit's deliberate "Cannot drop models from a bayesnecfit" is
  # no longer relabelled as a convergence result.
  skip_on_cran()
  one <- suppressMessages(pull_out(manec_example, model = "nec4param"))
  msg <- tryCatch(suppressMessages(screen_models(one)),
                  error = function(e) conditionMessage(e))
  expect_match(msg, "nothing left to return")
  expect_false(grepl("Cannot drop models from a bayesnecfit", msg, fixed = TRUE))
})

test_that("a passing bayesnecfit comes back unchanged", {
  skip_on_cran()
  one <- suppressMessages(pull_out(manec_example, model = "nec4param"))
  expect_message(
    out <- screen_models(one, rhat_cutoff = 99, ess_cutoff = 0,
                         divergence_cutoff = 1e6),
    "All 1 candidate models passed"
  )
  expect_s3_class(out, "bayesnecfit")
  expect_equal(out$model, "nec4param")
})

test_that("screen_models rewraps a hurdle fit as a hurdle fit", {
  skip_on_cran()
  h <- structure(list(growth = manec_example, survival = manec_example),
                 class = c("bayesnechurdlefit", "bnecfit"))
  out <- suppressMessages(
    screen_models(h, rhat_cutoff = 99, ess_cutoff = 0, divergence_cutoff = 1e6)
  )
  expect_s3_class(out, "bayesnechurdlefit")
  expect_s3_class(out$growth, "bayesmanecfit")
  expect_s3_class(out$survival, "bayesmanecfit")
})

test_that("the reasons name every failing criterion, not just the first", {
  # A model that failed on two counts is a different situation from one that
  # scraped past on one, and the reasons are what a methods section cites.
  # manec_example fails on every count at the defaults, so this comes through
  # the all-failed error, which carries the same reason list as the message.
  skip_on_cran()
  msg <- tryCatch({
    withCallingHandlers(screen_models(manec_example),
                        message = function(m) stop(conditionMessage(m)))
  }, error = function(e) conditionMessage(e))
  expect_match(msg, "nec4param: Rhat [0-9.]+, ESS")
  # and through the drop message when only some failed
  msg2 <- tryCatch({
    withCallingHandlers(
      screen_models(manec_example, rhat_cutoff = 1.15, ess_cutoff = 0,
                    divergence_cutoff = 1e6),
      message = function(m) stop(conditionMessage(m))
    )
  }, error = function(e) conditionMessage(e))
  expect_match(msg2, "nec4param: Rhat [0-9.]+")
})

test_that("quiet suppresses the message but not the screening", {
  skip_on_cran()
  expect_silent(
    out <- screen_models(manec_example, rhat_cutoff = 99, ess_cutoff = 0,
                         divergence_cutoff = 1e6, quiet = TRUE)
  )
  expect_s3_class(out, "bayesmanecfit")
})
