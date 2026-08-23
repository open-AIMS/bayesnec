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
  expect_equal(tab$min_ess[tab$model == "nec4param"],
               min(brms::neff_ratio(fit)) * brms::ndraws(fit))
  expect_equal(tab$min_ess_ratio[tab$model == "nec4param"],
               min(brms::neff_ratio(fit)))
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
  expect_error(check_sampling(1:10), "bayesnecfit or a bayesmanecfit")
})

# --- screen_models: the three cases the spec enumerates -----------------------

test_that("case 1 — nothing failed: unchanged, amend not called", {
  skip_on_cran()
  out <- expect_message(
    screen_models(manec_example, rhat_cutoff = 99, ess_cutoff = 0,
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
  out <- suppressWarnings(expect_message(
    screen_models(manec_example, rhat_cutoff = 1.15, ess_cutoff = 0,
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
    "nothing left to average"
  )
  # and specifically NOT amend()'s bare message
  msg <- tryCatch(suppressMessages(screen_models(manec_example)),
                  error = function(e) conditionMessage(e))
  expect_false(grepl("All models removed, nothing to return", msg, fixed = TRUE))
})

test_that("the drop message names every failing criterion, not just the first", {
  # A model that failed on two counts is a different situation from one that
  # scraped past on one, and the message is what a methods section cites.
  skip_on_cran()
  msg <- tryCatch({
    withCallingHandlers(screen_models(manec_example),
                        message = function(m) stop(conditionMessage(m)))
  }, error = function(e) conditionMessage(e))
  expect_match(msg, "nec4param: Rhat [0-9.]+, ESS")
})

test_that("quiet suppresses the message but not the screening", {
  skip_on_cran()
  expect_silent(
    out <- screen_models(manec_example, rhat_cutoff = 99, ess_cutoff = 0,
                         divergence_cutoff = 1e6, quiet = TRUE)
  )
  expect_s3_class(out, "bayesmanecfit")
})
