# Run `code` under a multisession plan, restoring the caller's plan after.
#
# multisession rather than multicore: it is the only backend available on every
# platform bayesnec is checked on, and it is the one that serialises globals
# and results, so it exercises the round trip that a forking backend skips.
with_parallel_plan <- function(code, workers = 2) {
  # setup.R sets mc.cores = 1 so that the suite does not oversubscribe, and
  # parallelly reads that as the core budget and warns about every worker past
  # the first. Raised for the duration of the call rather than suppressed at
  # each site, so the warning is still available where it means something.
  old_limit <- options(parallelly.maxWorkers.localhost = Inf)
  on.exit(options(old_limit), add = TRUE)
  old <- future::plan(future::multisession, workers = workers)
  on.exit(future::plan(old), add = TRUE)
  code
}

skip_unless_future <- function() {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
}

# A multisession worker loads the INSTALLED bayesnec, not one loaded with
# pkgload::load_all(), so under devtools::test() against an older installation
# the internals these tests exercise are simply absent and every parallel
# assertion fails for a reason that has nothing to do with the code under test.
# R CMD check installs the package first, so this skip does not fire there.
# Measured 2026-09-11, R 4.6.1, future 1.70.0: a load_all() session reports
# "object 'bnec_model_lapply' not found" from a multisession worker and works
# under multicore, which forks and so inherits the loaded namespace.
skip_unless_worker_sees_internals <- function() {
  ok <- tryCatch(
    future.apply::future_lapply(1, function(i) {
      exists("bnec_model_lapply", envir = asNamespace("bayesnec"),
             inherits = FALSE)
    }, future.seed = TRUE)[[1]],
    error = function(e) FALSE
  )
  skip_if(!isTRUE(ok),
          "worker cannot reach the bayesnec internals under test")
}

test_that("a sequential plan is not a parallel one, and a multisession is", {
  skip_unless_future()
  old <- future::plan(future::sequential)
  on.exit(future::plan(old), add = TRUE)
  expect_false(bnec_plan_is_parallel())
  expect_true(with_parallel_plan(bnec_plan_is_parallel()))
})

test_that("fewer than two models to fit is never parallel", {
  skip_unless_future()
  # A parallel plan provides no benefit over one brm() call, and treating it as
  # parallel would set cores = 1 and so make a lone fit slower than it is
  # today. amend() reaches this whenever the amended set needs no new fit.
  out <- with_parallel_plan(plan_model_set(list(chains = 4), 1))
  expect_false(out$parallel)
  expect_identical(out$brm_args, list(chains = 4))
  out0 <- with_parallel_plan(plan_model_set(list(), 0))
  expect_false(out0$parallel)
})

test_that("a parallel set samples its chains in sequence unless told not to", {
  skip_unless_future()
  out <- suppressMessages(
    with_parallel_plan(plan_model_set(list(chains = 4), 5))
  )
  expect_true(out$parallel)
  expect_identical(out$brm_args$cores, 1)
  # A cores the user supplied is left alone: nesting the two levels is a
  # legitimate thing to want, and this argument is how it is asked for.
  kept <- suppressMessages(
    with_parallel_plan(plan_model_set(list(chains = 4, cores = 2), 5))
  )
  expect_true(kept$parallel)
  expect_identical(kept$brm_args$cores, 2)
  expect_message(with_parallel_plan(plan_model_set(list(), 5)),
                 "Fitting 5 models in parallel")
  # amend() has no `...` to pass brms arguments through, so the advice bnec()
  # gives about nesting the two levels does not apply there and is not given.
  expect_message(
    with_parallel_plan(plan_model_set(list(), 5, caller = "amend")),
    "refit the set with bnec"
  )
})

test_that("no plan and no future leaves brm_args untouched", {
  old <- if (requireNamespace("future", quietly = TRUE)) {
    future::plan(future::sequential)
  } else {
    NULL
  }
  if (!is.null(old)) {
    on.exit(future::plan(old), add = TRUE)
  }
  out <- plan_model_set(list(chains = 4), 23)
  expect_false(out$parallel)
  # No cores is added on the sequential path: adding one would change what
  # every existing call passes to brm().
  expect_identical(out$brm_args, list(chains = 4))
})

test_that("the sequential path is lapply() and leaves the RNG alone", {
  seed_before <- {
    set.seed(1)
    get(".Random.seed", envir = globalenv())
  }
  out <- bnec_model_lapply(1:3, function(i) i^2, parallel = FALSE)
  expect_identical(out, lapply(1:3, function(i) i^2))
  expect_identical(get(".Random.seed", envir = globalenv()), seed_before)
})

test_that("a parallel run draws the same initial values as a sequential one", {
  skip_unless_future()
  # The acceptance criterion of #184, reduced to the mechanism it depends on.
  # make_good_inits() opens with set.seed(brm_args$seed), so what has to hold
  # is that the same seed produces the same draws in a worker as in the parent.
  draw <- function(i) {
    set.seed(100 + i)
    stats::runif(3)
  }
  sequential <- bnec_model_lapply(1:4, draw, parallel = FALSE)
  parallel <- with_parallel_plan({
    skip_unless_worker_sees_internals()
    bnec_model_lapply(1:4, draw, parallel = TRUE)
  })
  expect_identical(parallel, sequential)
})

test_that("the initial-value search draws the same values in a worker", {
  skip_unless_future()
  # The same assertion as above at the call site that matters, and without
  # sampling anything. make_good_inits() is where bayesnec's reproducibility
  # comes from: it opens with set.seed(seed), and the L'Ecuyer generator a
  # worker is given would have changed every value it returns.
  draw_inits <- function(model) {
    fam <- brms::Beta(link = "identity")
    pr <- define_prior(model, fam, nec_data$x, nec_data$y)
    make_good_inits(model, nec_data$x, nec_data$y, family = fam,
                    priors = pr, chains = 2, seed = 184)
  }
  models <- c("nec3param", "nec4param")
  sequential <- suppressMessages(
    bnec_model_lapply(models, draw_inits, parallel = FALSE)
  )
  parallel <- with_parallel_plan(suppressMessages({
    skip_unless_worker_sees_internals()
    bnec_model_lapply(models, draw_inits, parallel = TRUE)
  }))
  expect_identical(parallel, sequential)
})

test_that("future's own seeding would have changed those draws", {
  skip_unless_future()
  # The evidence for restoring RNGkind inside the worker, kept as a test
  # because the failure it prevents is silent. future.seed = TRUE installs an
  # L'Ecuyer-CMRG stream, set.seed() with the default kind = NULL does not put
  # the generator back, and the same seed then draws from a different
  # generator. Measured 2026-09-11 under future 1.70.0 on R 4.6.1: set.seed(43)
  # then runif(1) gives 0.48503768 in the parent and 0.47899959 in a worker,
  # under plan(sequential) as much as under plan(multisession).
  #
  # A failure here means future has changed how future.seed = TRUE seeds a
  # worker, not that bayesnec is broken; the restore would then be redundant
  # rather than wrong.
  draw <- function(i) {
    set.seed(100 + i)
    stats::runif(3)
  }
  sequential <- lapply(1:4, draw)
  unrestored <- with_parallel_plan({
    skip_unless_worker_sees_internals()
    future.apply::future_lapply(1:4, draw, future.seed = TRUE)
  })
  expect_false(isTRUE(all.equal(unrestored, sequential)))
  kind <- with_parallel_plan(
    future.apply::future_lapply(1, function(i) RNGkind()[1],
                                future.seed = TRUE)[[1]]
  )
  expect_identical(kind, "L'Ecuyer-CMRG")
})

test_that("a failing element yields its condition and the rest still run", {
  skip_unless_future()
  # The NA-on-failure contract several downstream functions read. try() is
  # inside the applied function, so an error becomes one element rather than
  # aborting the batch -- which under a parallel plan would lose every model.
  body <- function(i) {
    try(
      if (i == 2) {
        stop(fit_failure_condition("nec3param", "boom", NULL, NULL))
      } else {
        i
      },
      silent = TRUE
    )
  }
  check <- function(out) {
    expect_true(inherits(out[[2]], "try-error"))
    expect_identical(out[[1]], 1L)
    expect_identical(out[[3]], 3L)
    # failure_record() reads the condition off the try-error, and the custom
    # fields fit_failure_condition() attaches have to survive the trip back
    # from a worker or ?failed_models loses the priors and inits.
    cnd <- attr(out[[2]], "condition")
    expect_true(inherits(cnd, "bnec_fit_failure"))
    expect_match(conditionMessage(cnd), "boom")
  }
  check(bnec_model_lapply(1:3, body, parallel = FALSE))
  check(with_parallel_plan({
    skip_unless_worker_sees_internals()
    bnec_model_lapply(1:3, body, parallel = TRUE)
  }))
})

test_that("a model set fitted in parallel reproduces the sequential fit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  skip_unless_future()
  # The acceptance criterion end to end, on the smallest set that exercises it:
  # two models, two chains, 200 iterations, one seed. Asserted on the posterior
  # draws rather than on the whole object, which also stores the call.
  #
  # Both equations have to be valid for the family bnec() guesses, or the set
  # collapses to one model, plan_model_set() declines to parallelise it, and
  # the test passes while exercising nothing. nec_data is fitted as Beta with
  # an identity link, for which ecxlin is dropped; nec3param and nec4param are
  # both retained.
  # Arguments for brm() are passed through bnec()'s `...`, not a list.
  set_call <- function() {
    suppressMessages(suppressWarnings(
      bnec(y ~ crf(x, model = c("nec3param", "nec4param")), data = nec_data,
           seed = 184, chains = 2, iter = 200, refresh = 0)
    ))
  }
  sequential <- set_call()
  # The skip reason names what came back. A set that collapsed to one model
  # reports as a bayesnecfit here, which is a defect in this test rather than
  # an environment that cannot sample, and the two are not otherwise
  # distinguishable from a skipped run.
  skip_if(!inherits(sequential, "bayesmanecfit"),
          paste("expected a bayesmanecfit from the two-model set, got",
                paste(class(sequential), collapse = "/")))
  parallel <- with_parallel_plan({
    skip_unless_worker_sees_internals()
    set_call()
  })
  expect_s3_class(parallel, "bayesmanecfit")
  expect_identical(names(parallel$mod_fits), names(sequential$mod_fits))
  for (m in names(sequential$mod_fits)) {
    expect_equal(
      brms::as_draws_matrix(parallel$mod_fits[[m]]$fit),
      brms::as_draws_matrix(sequential$mod_fits[[m]]$fit),
      info = m
    )
  }
})

test_that("a timeout ends a parallel run exactly as it ends a sequential one", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  skip_unless_future()
  skip_if_not_installed("R.utils")
  # timeout is the only way to make a model fail on demand without inventing
  # data that fails for some other reason. What is asserted is agreement
  # between the two plans rather than any particular outcome: whatever
  # R.utils::withTimeout does to a fit, moving the fit into a worker must not
  # change it. A timeout this short fires before brm() reaches the sampler, so
  # nothing is compiled and the test spends no sampling time.
  run <- function() {
    suppressMessages(suppressWarnings(tryCatch(
      bnec(y ~ crf(x, model = c("nec3param", "nec4param")), data = nec_data,
           timeout = 0.001, seed = 184, chains = 2, iter = 200, refresh = 0),
      error = conditionMessage
    )))
  }
  sequential <- run()
  parallel <- with_parallel_plan({
    skip_unless_worker_sees_internals()
    run()
  })
  expect_identical(parallel, sequential)
})
