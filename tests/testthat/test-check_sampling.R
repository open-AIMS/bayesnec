skip_unless_full_suite()

# Sampler diagnostics must report absolute and relative efficiency,
# exclude unscreenable parameters and distinguish partial from total failure.

# The packaged fit passes the screen, and these tests need one that fails it.
manec_example <- unconverged_manec(manec_example)

test_that("check_sampling returns one row per candidate with all columns", {
  skip_on_cran()
  tab <- check_sampling(manec_example)
  expect_s3_class(tab, "data.frame")
  expect_setequal(tab$model, manec_example$success_models)
  expect_named(tab, c("model", "max_rhat", "min_ess", "min_ess_ratio",
                      "n_divergent", "divergent_ratio", "failed"))
  expect_type(tab$failed, "logical")
})

test_that("min_ess is neff_ratio times ndraws, not a ratio", {
  # Report absolute ESS beside its draw-count ratio. At four chains, an
  # ESS cutoff of 400 expresses the 100-per-chain diagnostic guideline;
  # the ratio distinguishes more draws from more efficient sampling.
  skip_on_cran()
  tab <- check_sampling(manec_example)
  fit <- bayesnec:::pull_brmsfit(
    suppressMessages(pull_out(manec_example, model = "nec4param"))
  )
  # Use the screened parameters. Prior draws are excluded; on this fixture
  # they do not determine the minimum ESS, but that is not guaranteed for
  # every finite sample.
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
  # Read through brms, a required dependency; bayesnec lists rstan only
  # in Suggests. This checks the interface used by count_divergent().
  skip_on_cran()
  tab <- check_sampling(manec_example)
  fit <- bayesnec:::pull_brmsfit(
    suppressMessages(pull_out(manec_example, model = "nec4param"))
  )
  np <- brms::nuts_params(fit, pars = "divergent__")
  expect_equal(tab$n_divergent[tab$model == "nec4param"], sum(np$Value))
})

# Replaces nuts_params() so that every fit reports `k` divergent transitions,
# and, where `draws` is given, ndraws() so that every fit retains `draws`.
local_divergent_count <- function(k, draws = NULL, env = parent.frame()) {
  local_mocked_bindings(
    nuts_params = function(object, pars = NULL, ...) {
      data.frame(Chain = 1L, Iteration = seq_len(k + 1),
                 Parameter = factor("divergent__"), Value = c(rep(1, k), 0))
    },
    .package = "bayesnec",
    .env = env
  )
  if (!is.null(draws)) {
    local_mocked_bindings(ndraws = function(x, ...) draws,
                          .package = "bayesnec", .env = env)
  }
}

# The divergence decision of check_sampling() alone: the Rhat and ESS
# criteria are switched off.
divergence_verdict <- function(k, draws, ...) {
  local_divergent_count(k, draws)
  tab <- check_sampling(manec_example, rhat_cutoff = 99, ess_cutoff = 0, ...)
  tab$failed[tab$model == "nec4param"]
}

test_that("divergent_ratio is n_divergent divided by the retained draws", {
  skip_on_cran()
  local_divergent_count(7)
  tab <- check_sampling(manec_example)
  fit <- manec_example$mod_fits$nec4param$fit
  expect_equal(tab$n_divergent, c(7, 7))
  expect_equal(tab$divergent_ratio, tab$n_divergent / brms::ndraws(fit))
})

test_that("the default cutoff passes 10 of 8000 draws and fails 11", {
  # 0.00125 is 10 of the 8,000 draws bnec() retains by default.
  failed <- function(n, draws) {
    bayesnec:::divergence_failed(bayesnec:::divergent_ratio(n, draws),
                                 0.00125)
  }
  expect_false(failed(10, 8000))
  expect_true(failed(11, 8000))
  expect_false(failed(5, 4000))
  expect_true(failed(6, 4000))
  skip_on_cran()
  expect_false(divergence_verdict(10, 8000))
  expect_true(divergence_verdict(11, 8000))
  expect_false(divergence_verdict(5, 4000))
  expect_true(divergence_verdict(6, 4000))
})

test_that("a rate of divergence gets one decision at any number of draws", {
  # Thinning by 2 halves the draws and, in expectation, the count.
  skip_on_cran()
  expect_identical(divergence_verdict(12, 8000), divergence_verdict(6, 4000))
  expect_true(divergence_verdict(6, 4000))
  expect_identical(divergence_verdict(10, 8000), divergence_verdict(5, 4000))
  expect_false(divergence_verdict(5, 4000))
})

test_that("no retained draws gives an NA ratio and no divergence failure", {
  expect_true(is.na(bayesnec:::divergent_ratio(3, 0)))
  expect_true(is.na(bayesnec:::divergent_ratio(3, NA)))
  expect_false(bayesnec:::divergence_failed(NA_real_, 0.00125))
  skip_on_cran()
  for (draws in list(0, NA_real_)) {
    local_divergent_count(3, draws)
    tab <- check_sampling(manec_example, rhat_cutoff = 99, ess_cutoff = 0)
    expect_true(all(is.na(tab$divergent_ratio)))
    expect_identical(tab$failed, c(FALSE, FALSE))
  }
})

test_that("Inf switches the divergence criterion off", {
  skip_on_cran()
  expect_true(divergence_verdict(99, 100))
  expect_false(divergence_verdict(99, 100, divergence_cutoff = Inf))
  expect_true(divergence_verdict(1, 100, divergence_cutoff = 0))
  local_divergent_count(99, 100)
  expect_message(
    screen_models(manec_example, rhat_cutoff = 99, ess_cutoff = 0,
                  divergence_cutoff = Inf),
    "divergences not screened", fixed = TRUE
  )
})

test_that("a cutoff written as a count stops with the conversion", {
  skip_on_cran()
  conversion <- "10 divergent transitions of 8,000 draws is 0.00125"
  messages <- vapply(list(
    function() check_sampling(manec_example, divergence_cutoff = 10),
    function() screen_models(manec_example, divergence_cutoff = 10),
    function() summary(manec_example, divergence_cutoff = 10),
    function() summary(nec4param, divergence_cutoff = 10)
  ), function(f) tryCatch(f(), error = conditionMessage), character(1))
  expect_match(messages, "proportion of the retained draws", fixed = TRUE)
  expect_match(messages, conversion, fixed = TRUE)
  expect_length(unique(messages), 1)
  expect_error(check_sampling(manec_example, divergence_cutoff = 1),
               conversion, fixed = TRUE)
  expect_error(check_sampling(manec_example, divergence_cutoff = -0.001),
               "must not be negative", fixed = TRUE)
  expect_error(check_sampling(manec_example, divergence_cutoff = NA_real_))
})

test_that("the screen states the cutoff and the share of draws as percentages", {
  skip_on_cran()
  local_divergent_count(0)
  expect_message(
    screen_models(manec_example, rhat_cutoff = 99, ess_cutoff = 0),
    "(Rhat <= 99, ESS >= 0, divergent draws <= 0.125%)", fixed = TRUE
  )
  local_divergent_count(17, 8000)
  msg <- tryCatch(
    screen_models(manec_example, rhat_cutoff = 99, ess_cutoff = 0),
    error = conditionMessage
  )
  expect_match(msg, "nothing left to return", fixed = TRUE)
  expect_match(msg,
               "nec4param: 17 divergent transitions (0.21% of 8000 draws)",
               fixed = TRUE)
})

test_that("every threshold is an argument and each can fail a model alone", {
  skip_on_cran()
  # manec_example fails Rhat and ESS at the defaults
  expect_true(all(check_sampling(manec_example)$failed))
  # relax everything: nothing fails
  loose <- check_sampling(manec_example, rhat_cutoff = 99, ess_cutoff = 0,
                          divergence_cutoff = Inf)
  expect_false(any(loose$failed))
  # each threshold in isolation
  expect_true(any(check_sampling(manec_example, rhat_cutoff = 1.01,
                                 ess_cutoff = 0,
                                 divergence_cutoff = Inf)$failed))
  expect_true(any(check_sampling(manec_example, rhat_cutoff = 99,
                                 ess_cutoff = 400,
                                 divergence_cutoff = Inf)$failed))
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
  # Independent prior_* draws are not sampled posterior parameters. Their
  # Monte Carlo Rhat variation must not cause a convergence-screen failure.
  x <- c(b_top_Intercept = 1.002, b_nec_Intercept = 1.004,
         prior_b_top = 1.023, prior_sigma = 1.031,
         lprior = 1.006, lp__ = 1.001)
  out <- bayesnec:::screenable_pars(x)
  expect_named(out, c("b_top_Intercept", "b_nec_Intercept", "lprior", "lp__"))
  expect_equal(max(out), 1.006)
})

test_that("screenable_pars drops parameters with no variance", {
  # Constant-prior parameters have zero variance and undefined Rhat/ESS.
  # Exclude them from the screen rather than propagating NA verdicts.
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

group_sampling_fixture <- function() {
  passing <- manec_example
  # Give nec4param the diagnostics of the passing equation at level b. The
  # model name stays fixed so the same equation fails at a and passes at b.
  passing$mod_fits[["nec4param"]]$fit <-
    passing$mod_fits[["ecx4param"]]$fit
  structure(
    list(
      fits = list(a = manec_example, b = passing),
      levels = c("a", "b")
    ),
    class = c("bayesnecgroupfit", "bnecfit")
  )
}

test_that("check_sampling returns one level-labelled table for a grouped fit", {
  skip_on_cran()
  grouped <- group_sampling_fixture()
  tab <- check_sampling(grouped, rhat_cutoff = 1.15, ess_cutoff = 0,
                        divergence_cutoff = Inf)
  expect_s3_class(tab, "data.frame")
  expect_named(tab, c("level", "model", "max_rhat", "min_ess",
                      "min_ess_ratio", "n_divergent", "divergent_ratio",
                      "failed"))
  expect_equal(nrow(tab), 4)
  nec_rows <- tab[tab$model == "nec4param", ]
  expect_equal(nec_rows$level, c("a", "b"))
  expect_equal(nec_rows$failed, c(TRUE, FALSE))
})

# --- screen_models: the three cases the spec enumerates -----------------------

test_that("case 1 — nothing failed: unchanged, amend not called", {
  skip_on_cran()
  # Assigned inside expect_message(), not around it: expect_message() returns
  # the message condition, not the value of the expression.
  expect_message(
    out <- screen_models(manec_example, rhat_cutoff = 99, ess_cutoff = 0,
                         divergence_cutoff = Inf),
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
                         divergence_cutoff = Inf),
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
                         divergence_cutoff = Inf),
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
    screen_models(h, rhat_cutoff = 99, ess_cutoff = 0, divergence_cutoff = Inf)
  )
  expect_s3_class(out, "bayesnechurdlefit")
  expect_s3_class(out$growth, "bayesmanecfit")
  expect_s3_class(out$survival, "bayesmanecfit")
})

test_that("grouped screening drops failures independently by default", {
  skip_on_cran()
  grouped <- group_sampling_fixture()
  suppressWarnings(expect_message(
    out <- screen_models(grouped, rhat_cutoff = 1.15, ess_cutoff = 0,
                         divergence_cutoff = Inf),
    "Screening level"
  ))
  expect_s3_class(out, "bayesnecgroupfit")
  expect_s3_class(out$fits$a, "bayesnecfit")
  expect_equal(out$fits$a$model, "ecx4param")
  expect_s3_class(out$fits$b, "bayesmanecfit")
  expect_setequal(names(out$fits$b$mod_fits),
                  c("nec4param", "ecx4param"))
})

test_that("common grouped screening removes an equation from every level", {
  skip_on_cran()
  grouped <- group_sampling_fixture()
  suppressWarnings(expect_message(
    out <- screen_models(grouped, rhat_cutoff = 1.15, ess_cutoff = 0,
                         divergence_cutoff = Inf, group_action = "common"),
    "from every level"
  ))
  expect_s3_class(out, "bayesnecgroupfit")
  expect_true(all(vapply(out$fits, inherits, logical(1), "bayesnecfit")))
  expect_equal(vapply(out$fits, `[[`, character(1), "model"),
               c(a = "ecx4param", b = "ecx4param"))
  cmp <- suppressWarnings(suppressMessages(
    compare_posterior(out, comparison = "n(s)ec")
  ))
  expect_named(cmp, c("posterior_list", "posterior_data", "diff_list",
                      "diff_data", "prob_diff"))
})

test_that("grouped screening names a level from which it would drop all fits", {
  skip_on_cran()
  grouped <- group_sampling_fixture()
  msg <- tryCatch(
    suppressMessages(screen_models(grouped, group_action = "common")),
    error = function(e) conditionMessage(e)
  )
  expect_match(msg, 'level.*"a"')
  expect_match(msg, "nec4param: Rhat")
})

test_that("the reasons name every failing criterion, not just the first", {
  # Keep every failure reason in both the all-failed error and partial-drop
  # message. This fixture reaches the error at the default cutoffs.
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
                    divergence_cutoff = Inf),
      message = function(m) stop(conditionMessage(m))
    )
  }, error = function(e) conditionMessage(e))
  expect_match(msg2, "nec4param: Rhat [0-9.]+")
})

test_that("quiet suppresses the message but not the screening", {
  skip_on_cran()
  expect_silent(
    out <- screen_models(manec_example, rhat_cutoff = 99, ess_cutoff = 0,
                         divergence_cutoff = Inf, quiet = TRUE)
  )
  expect_s3_class(out, "bayesmanecfit")
})
test_that("sampler diagnostics dispatch on each concrete fit class", {
  classes <- c("bayesnecfit", "bayesmanecfit", "bayesnechurdlefit",
               "bayesnecgroupfit", "bayesnecjointfit")
  for (class in classes) {
    expect_true(is.function(getS3method("check_sampling", class)))
  }
})
