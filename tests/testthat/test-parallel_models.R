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

# Run `code` under a forking plan. Skips where forking is unavailable, which
# is Windows, and RStudio or Positron everywhere.
#
# multisession is the backend the rest of the file uses because it is available
# on every platform and serialises the round trip. Forking takes a different
# path through all three of the things this file is about: the worker inherits
# the parent's memory rather than being sent globals, it inherits the RNG state
# at the fork rather than being given a stream, and it shares the parent's
# stderr. None of that is exercised by multisession, so on the platforms where
# forking works it is worth checking that the same guarantees hold.
with_fork_plan <- function(code, workers = 2) {
  old_limit <- options(parallelly.maxWorkers.localhost = Inf)
  on.exit(options(old_limit), add = TRUE)
  old <- suppressWarnings(future::plan(future::multicore, workers = workers))
  on.exit(future::plan(old), add = TRUE)
  # future falls back to evaluating in the parent where forking is
  # unavailable, and reports one worker when it does, so the worker count is
  # the test for whether this platform actually forks. Asked of future rather
  # than of supportsMulticore() in parallelly, a namespaced call to which is an
  # undeclared import and fails R CMD check under error_on = "warning".
  skip_if(future::nbrOfWorkers() < workers,
          "forking is not available on this platform")
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
# "object 'bnec_parallel_lapply' not found" from a multisession worker and works
# under multicore, which forks and so inherits the loaded namespace.
skip_unless_worker_sees_internals <- function() {
  ok <- tryCatch(
    future.apply::future_lapply(1, function(i) {
      exists("bnec_parallel_lapply", envir = asNamespace("bayesnec"),
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

test_that("RNGkind derives the state, it does not reseed from the clock", {
  # The reason bnec_parallel_lapply() recorded for restoring the kind was that
  # RNGkind() re-initialises .Random.seed from the clock. It does that only
  # where no seed exists yet. Pinned because the whole argument about what a
  # parallel run reproduces rests on which of the two it is. See #310.
  old_kind <- RNGkind()
  old_seed <- if (exists(".Random.seed", envir = globalenv(),
                         inherits = FALSE)) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  }
  on.exit({
    # The kind first, then the seed, and the kind restored even where there
    # was no seed: removing .Random.seed does not put the kind back, and this
    # block changes it. R/helpers.R:136 records the same order for the same
    # reason.
    suppressWarnings(do.call(RNGkind, as.list(old_kind)))
    if (is.null(old_seed)) {
      suppressWarnings(rm(".Random.seed", envir = globalenv()))
    } else {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)
  once <- function(from, to) {
    set.seed(42, kind = from)
    suppressWarnings(do.call(RNGkind, as.list(to)))
    stats::runif(1)
  }
  mt <- c("Mersenne-Twister", "Inversion", "Rejection")
  # The same-kind call, which is what bnec_parallel_lapply() makes in the parent's
  # own kind, and the L'Ecuyer-CMRG to Mersenne-Twister change, which is what
  # it makes inside a worker. Two independent pairs per arm rather than one:
  # a single pair of clock-seeded draws could in principle collide.
  expect_identical(once(mt[1], mt), once(mt[1], mt))
  expect_identical(once(mt[1], mt), once(mt[1], mt))
  expect_identical(once("L'Ecuyer-CMRG", mt), once("L'Ecuyer-CMRG", mt))
  expect_identical(once("L'Ecuyer-CMRG", mt), once("L'Ecuyer-CMRG", mt))
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
  out <- bnec_parallel_lapply(1:3, function(i) i^2, parallel = FALSE)
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
  sequential <- bnec_parallel_lapply(1:4, draw, parallel = FALSE)
  parallel <- with_parallel_plan({
    skip_unless_worker_sees_internals()
    bnec_parallel_lapply(1:4, draw, parallel = TRUE)
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
    bnec_parallel_lapply(models, draw_inits, parallel = FALSE)
  )
  parallel <- with_parallel_plan(suppressMessages({
    skip_unless_worker_sees_internals()
    bnec_parallel_lapply(models, draw_inits, parallel = TRUE)
  }))
  expect_identical(parallel, sequential)
})

test_that("a forking plan gives the same guarantees as a socket one", {
  skip_unless_future()
  # The three assertions this file turns on, run through the fork path. A
  # forked worker inherits the parent's RNG state at the fork instead of being
  # given a stream, so the kind restore has to hold there for a different
  # reason than it does under multisession, and it is the backend on which
  # sharing the parent's memory could hide a missing binding in
  # narrow_environment().
  draw <- function(i) {
    set.seed(100 + i)
    stats::runif(3)
  }
  sequential <- bnec_parallel_lapply(1:4, draw, parallel = FALSE)
  res <- with_fork_plan({
    set.seed(9)
    before <- get(".Random.seed", envir = globalenv())
    out <- bnec_parallel_lapply(1:4, draw, parallel = TRUE)
    list(out = out, before = before,
         after = get(".Random.seed", envir = globalenv()))
  })
  expect_identical(res$out, sequential)
  expect_identical(res$after, res$before)
  # And the failure contract, which under forking returns the try-error
  # through shared memory rather than through serialisation.
  out <- with_fork_plan(bnec_parallel_lapply(1:3, function(i) {
    try(
      if (i == 2) {
        stop(fit_failure_condition("nec3param", "boom", NULL, NULL))
      } else {
        i
      },
      silent = TRUE
    )
  }, parallel = TRUE))
  expect_true(inherits(out[[2]], "try-error"))
  expect_identical(out[[3]], 3L)
  expect_true(inherits(attr(out[[2]], "condition"), "bnec_fit_failure"))
})

test_that("a parallel run leaves the caller's RNG stream where it was", {
  skip_unless_future()
  # expand_manec() draws w_draw_seed from the ambient stream immediately after
  # the model loop, and through it the index deciding which draws each equation
  # contributes to the model-averaged estimate. A parallel loop advances that
  # stream by generating its per-element seeds, so without this restore two
  # parallel runs of one call would not agree with each other. It does not make
  # the draw match the sequential run's, which advances the stream by running
  # every init search in the parent; see ?bnec.
  #
  # The baseline is taken inside the plan and after the skip check, not before
  # it. skip_unless_worker_sees_internals() calls future_lapply() itself, which
  # advances the parent's stream; a baseline taken ahead of it is a state the
  # loop was never given, and the comparison then fails for that reason alone.
  # It did, on macOS, at 2026-09-11.
  res <- with_parallel_plan({
    skip_unless_worker_sees_internals()
    set.seed(9)
    before <- get(".Random.seed", envir = globalenv())
    out <- bnec_parallel_lapply(1:4, function(i) i, parallel = TRUE)
    list(out = out, before = before,
         after = get(".Random.seed", envir = globalenv()))
  })
  expect_length(res$out, 4)
  expect_identical(res$after, res$before)
  # And with no stream to begin with, none is left behind. The removal comes
  # after the skip check for the same reason.
  left_behind <- with_parallel_plan({
    skip_unless_worker_sees_internals()
    suppressWarnings(rm(".Random.seed", envir = globalenv()))
    bnec_parallel_lapply(1:2, function(i) i, parallel = TRUE)
    exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  })
  expect_false(left_behind)
})

test_that("a one-worker parallel plan does not take chains away from brms", {
  skip_unless_future()
  # plan(multicore) resolves to a single worker wherever forking is
  # unavailable -- Windows, and RStudio or Positron on Linux and macOS. Setting
  # cores = 1 there would fit one model at a time with its chains in sequence,
  # which for a user who has set mc.cores is slower than fitting sequentially,
  # while the message claimed models were being fitted in parallel.
  old_limit <- options(parallelly.maxWorkers.localhost = Inf)
  on.exit(options(old_limit), add = TRUE)
  old <- future::plan(future::multisession, workers = 1)
  on.exit(future::plan(old), add = TRUE)
  out <- suppressMessages(plan_model_set(list(chains = 4), 5))
  expect_null(out$brm_args$cores)
  expect_message(plan_model_set(list(chains = 4), 5),
                 "resolves to a single worker")
  # More than one worker still gets the clamp.
  many <- suppressMessages(with_parallel_plan(plan_model_set(list(), 5)))
  expect_identical(many$brm_args$cores, 1)
})

test_that("only what the applied function names travels to a worker", {
  # future exports the applied function with its enclosing environment, so an
  # ordinary closure written inside bnec() or amend_model_set() would ship
  # every object those functions have built -- for amend(), every fit already
  # in the set. narrow_environment() is what stops that, and it is applied on
  # the sequential path too so that a name left out of its list fails on the
  # first ordinary call rather than only for whoever sets a plan.
  bulky <- matrix(0, 500, 500)
  build <- function() {
    hidden <- bulky
    wanted <- 1:3
    narrow_environment(function(m) wanted[m], list(wanted = wanted))
  }
  fn <- build()
  expect_identical(fn(2), 2L)
  expect_identical(ls(environment(fn)), "wanted")
  expect_false(exists("hidden", envir = environment(fn)))
  # Package internals still resolve, because the namespace is the parent.
  expect_identical(parent.env(environment(fn)), asNamespace("bayesnec"))
})

test_that("brm_args does not export a family object's calling environment", {
  skip_unless_future()
  # This is the second route reported in #329. The formula was already narrowed,
  # but a family constructed beside an unrelated object retained that object
  # through its closures and reached the model worker as brm_args$family.
  make_family <- function() {
    big <- numeric(1e6)
    Gamma(link = "identity")
  }
  supplied <- make_family()
  family <- bayesnec:::validate_family(supplied, link_source = "symbol")
  brm_args <- list(family = bayesnec:::unmark_family(family))
  # A finite maxSize asks future to calculate total_size even where the calling
  # session has disabled its export limit with future.globals.maxSize = Inf.
  # Without it the macOS check returned NA for both totals and tested nothing.
  max_size <- 100 * 1024^2
  globals <- future::getGlobalsAndPackages(
    quote(brm_args$family$family),
    envir = list2env(list(brm_args = brm_args), parent = baseenv()),
    maxSize = max_size
  )$globals
  supplied_globals <- future::getGlobalsAndPackages(
    quote(brm_args$family$family),
    envir = list2env(
      list(brm_args = list(family = supplied)), parent = baseenv()
    ),
    maxSize = max_size
  )$globals
  expect_gt(attr(supplied_globals, "total_size"), 7 * 1024^2)
  expect_lt(
    attr(globals, "total_size"),
    attr(supplied_globals, "total_size") / 10
  )
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
  check(bnec_parallel_lapply(1:3, body, parallel = FALSE))
  check(with_parallel_plan({
    skip_unless_worker_sees_internals()
    bnec_parallel_lapply(1:3, body, parallel = TRUE)
  }))
})

test_that("a model set fitted in parallel reproduces the sequential fit", {
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
  # The fitted models are what reproduces. w_draw_seed and w_draw_index do not,
  # and are deliberately not asserted here: expand_manec() draws them from the
  # ambient RNG stream, which the parallel loop cannot leave in the state the
  # sequential loop leaves it in. The test above pins what is true instead --
  # that the loop does not disturb the caller's stream at all.
  for (m in names(sequential$mod_fits)) {
    expect_equal(
      brms::as_draws_matrix(parallel$mod_fits[[m]]$fit),
      brms::as_draws_matrix(sequential$mod_fits[[m]]$fit),
      info = m
    )
  }
})

test_that("a timeout ends a parallel run exactly as it ends a sequential one", {
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

# --- The level loop of a grouped call (#338) -------------------------------

test_that("the two arrangements of one plan are counted in rounds of one fit", {
  # The unit is one fit sampling its chains in sequence, which is what happens
  # inside a worker on either arrangement. ceiling() on both sides: a round is
  # set by its slowest member, so a part-filled round still takes a whole one.
  rounds <- bayesnec:::group_loop_rounds
  # The shape #338 was measured on. Four cores, eleven equations, seven levels:
  # the models are the smaller count, so the release arrangement stands.
  expect_equal(rounds(7, 11, 4)$models, 21)
  expect_equal(rounds(7, 11, 4)$levels, 22)
  # The same shape at eight workers, where the levels become the smaller.
  expect_equal(rounds(7, 11, 8)$models, 14)
  expect_equal(rounds(7, 11, 8)$levels, 11)
  # Two levels of fifteen equations, the other call in that vignette. The
  # The levels are never the smaller count here, because a level arrangement
  # cannot go below M rounds and the model arrangement reaches L.
  expect_equal(rounds(2, 15, 4)$models, 8)
  expect_equal(rounds(2, 15, 4)$levels, 15)
  # One worker is parallel in name only and gives a tie, which the caller
  # resolves in favour of the models.
  expect_equal(rounds(7, 11, 1)$models, rounds(7, 11, 1)$levels)
  # An unbounded worker count is a scheduler backend -- nbrOfWorkers() reports
  # Inf for future.batchtools -- and is counted rather than refused: one round
  # of whichever loop is dispatched. ceiling(L / Inf) is 0, so dividing would
  # have reported an arrangement that takes no time at all.
  expect_equal(rounds(7, 11, Inf)$levels, 11)
  expect_equal(rounds(7, 11, Inf)$models, 7)
  # A count that genuinely could not be read is the NA case, and the caller
  # leaves the levels in sequence.
  expect_true(is.na(rounds(7, 11, NA)$levels))
  expect_true(is.na(rounds(7, NA_integer_, 8)$levels))
})

test_that("fewer than two levels, or no plan, is never a parallel level loop", {
  skip_unless_future()
  old <- future::plan(future::sequential)
  on.exit(future::plan(old), add = TRUE)
  expect_false(bayesnec:::plan_group_levels(5, 11)$dispatch)
  expect_false(
    with_parallel_plan(bayesnec:::plan_group_levels(1, 11))$dispatch
  )
})

test_that("the smaller count decides which loop an ordinary plan drives", {
  skip_unless_future()
  # Two workers, three levels, two equations: the models take 3 rounds and the
  # levels 4, so the levels stay in sequence and the message says so with both
  # counts in it.
  expect_message(
    out <- with_parallel_plan(bayesnec:::plan_group_levels(3, 2)),
    "3 rounds of one fit, against 4"
  )
  expect_false(out$dispatch)
  expect_false(out$concurrent)
  # Two workers, four levels, three equations: the levels take 6 rounds and
  # the models 8, so the levels take the workers.
  expect_message(
    out <- with_parallel_plan(bayesnec:::plan_group_levels(4, 3)),
    "Fitting 4 levels in parallel over 2 workers"
  )
  expect_true(out$dispatch)
  expect_true(out$concurrent)
})

test_that("an unreadable model count leaves the levels in sequence", {
  skip_unless_future()
  expect_message(
    out <- with_parallel_plan(bayesnec:::plan_group_levels(4, NA_integer_)),
    "equations the formula asks for could not be read"
  )
  expect_false(out$dispatch)
})

test_that("a nested plan is honoured without being counted", {
  skip_unless_future()
  old_limit <- options(parallelly.maxWorkers.localhost = Inf)
  on.exit(options(old_limit), add = TRUE)
  old <- future::plan(list(future::tweak(future::multisession, workers = 2),
                           future::tweak(future::multisession, workers = 2)))
  on.exit(future::plan(old), add = TRUE)
  # Three levels of two equations over two outer workers is the shape the
  # count refuses -- 4 rounds against 3 -- and the nested plan takes it
  # anyway, because there the user has divided the workers deliberately.
  expect_message(out <- bayesnec:::plan_group_levels(3, 2),
                 "plan is a list")
  expect_true(out$dispatch)
  expect_true(out$concurrent)
})

test_that("each worker compiles into its own directory, under the user's root", {
  # Every level fits the same equations, so parallel levels compile the same
  # Stan programs at once. cmdstanr does not lock its cache, and a forked
  # worker shares the parent's tempdir, so the directory has to differ by
  # process. Keyed on the process rather than on the level so that a worker
  # which draws three levels compiles each equation once and not three times.
  # Paths are compared after normalisation. dirname() returns forward slashes
  # and tempdir() returns the platform separator, so on Windows the two differ
  # by separator alone: measured on the CI runner,
  # "C:/Users/.../RtmpYnmHNK" against "C:\\Users\\...\\RtmpYnmHNK".
  same_path <- function(a, b) {
    norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
    expect_identical(norm(a), norm(b))
  }
  own <- bayesnec:::worker_stan_cache_dir()
  expect_match(basename(own), paste0("^bayesnec-stan-", Sys.getpid(), "$"))
  expect_true(dir.exists(own))
  same_path(dirname(own), tempdir())
  # A directory the user chose is shared by every worker on every backend, so
  # it is separated the same way.
  root <- file.path(tempdir(), "bayesnec-cache-root-test")
  dir.create(root, showWarnings = FALSE)
  same_path(dirname(bayesnec:::worker_stan_cache_dir(root)), root)
  # The root is an argument rather than read from the options here, because
  # future exports globals and not options and a multisession worker would
  # otherwise never see one the caller set.
  old <- options(cmdstanr_write_stan_file_dir = root)
  on.exit(options(old), add = TRUE)
  same_path(dirname(bayesnec:::worker_stan_cache_dir()), tempdir())
  # Two workers do not share a directory, which is the property the race
  # needs. Asserted through a future rather than by construction, since the
  # process id is what separates them.
  skip_unless_future()
  skip_unless_worker_sees_internals()
  dirs <- with_parallel_plan(
    bayesnec:::bnec_parallel_lapply(1:2, function(i) {
      Sys.sleep(0.2)
      bayesnec:::worker_stan_cache_dir()
    }, parallel = TRUE)
  )
  expect_length(unique(unlist(dirs)), 2)
})

test_that("a value passed by do.call keeps the environment of its formula", {
  # bnec_group() reaches bnec() through do.call() rather than by forwarding
  # `...`, because `...` cannot be put in the environment narrow_environment()
  # builds. A formula records the environment it was created in, and #319 needs
  # that environment intact for crf() to resolve a symbol from it, so what is
  # pinned here is that do.call() transports rather than rebuilds it. Measured
  # on R 4.6.1.
  e <- new.env()
  f <- y ~ x
  environment(f) <- e
  seen <- do.call(function(z, ...) environment(z), list(f, family = "beta"))
  expect_identical(seen, e)
})

test_that("a grouped call fits each level on its own rows", {
  skip_on_cran()
  # The level loop end to end on the real function: the subsets are built in
  # the parent and dispatched as the elements, so what is asserted is that each
  # level was fitted on its own rows, that the fits come back named and ordered
  # by level, and that the seeds the levels were fitted under are recorded.
  #
  # No plan is used for this real fit. Since #338 every level is seeded from the
  # calling session, so the two arrangements give the same estimates by
  # construction, and that is pinned directly and without fitting by "a seeded
  # body gives one answer on both arrangements" below. The following test calls
  # bnec_group() under a forking plan with bnec() mocked, which covers the outer
  # orchestration without another Stan compilation. What a second real call
  # under a plan would add is a Stan compilation per worker.
  #
  # It would also be unreliable here. Measured on R 4.6.1 with future 1.70.0,
  # on a WSL2 host: this call takes 173 s with no plan, while the same call
  # under plan(multisession, workers = 2) did not return in 300 s from inside
  # testthat, on three runs and with the fitting removed -- although it returns
  # in 3 s outside testthat, and bnec_parallel_lapply() over a body that loads
  # brms, raises, or calls bnec() returns in under 3 s inside testthat and out.
  # The interaction was not identified. It is recorded here so that the next
  # person to consider adding such a test knows what happened, and in the
  # pull request for #338.
  #
  # Bound in the frame the formula is written in, so that the stored fits also
  # answer #329: without narrow_formula_environment() this 7.6 MiB vector
  # reaches every stored fit.
  big <- rnorm(1e6)
  d <- nec_data
  d$site <- rep(c("a", "b"), length.out = nrow(d))
  fit <- suppressMessages(suppressWarnings(
    bnec_group(y ~ crf(x, model = "nec3param"), data = d, group_var = "site",
               seed = 338, chains = 2, iter = 200, refresh = 0)
  ))
  expect_identical(names(fit$fits), c("a", "b"))
  expect_identical(fit$levels, c("a", "b"))
  expect_identical(fit$n, c(50L, 50L))
  expect_identical(
    unname(vapply(fit$fits, function(f) nrow(f$fit$data), integer(1))),
    c(50L, 50L)
  )
  # What each level was fitted under, recorded rather than regenerated.
  expect_length(fit$level_seeds, 2L)
  expect_true(is.numeric(fit$level_seeds))
  # The stored formula holds the narrowed environment, not the 7.6 MiB vector
  # bound beside it above (#329). Measured against the same formula written
  # here and left alone, so the comparison does not depend on what else the
  # session holds.
  unnarrowed <- local({
    keep <- big
    bayesnecformula(y ~ crf(x, model = "nec3param"))
  })
  for (lev in c("a", "b")) {
    expect_lt(
      length(serialize(fit$fits[[lev]]$bayesnecformula, NULL)),
      length(serialize(unnarrowed, NULL)) / 50
    )
  }
})

test_that("a grouped call dispatches its levels under a plan", {
  skip_unless_future()
  # The preceding test runs the real bnec() fitting contract. Repeating those
  # Stan fits under a socket plan did not return in 300 s on three runs on the
  # development host, and it adds one Stan compilation per worker. The mock here
  # leaves the rest of bnec_group() intact and is inherited by forked workers,
  # so this test runs the level split, plan decision, concurrent dispatch,
  # worker cache and result assembly without another Stan compile. The socket
  # dispatch and serialisation contract is tested separately above.
  d <- nec_data
  d$site <- rep(c("a", "b"), length.out = nrow(d))
  big <- numeric(1e6)
  parent_pid <- Sys.getpid()
  mock_bnec <- function(formula, data, family, ...) {
    env <- environment(formula)
    has_big <- FALSE
    while (is.environment(env) && !bayesnec:::env_by_reference(env)) {
      if (exists("big", envir = env, inherits = FALSE)) {
        has_big <- TRUE
        break
      }
      env <- parent.env(env)
    }
    list(
      pid = Sys.getpid(),
      rows = row.names(data),
      cache = getOption("cmdstanr_write_stan_file_dir"),
      has_big = has_big
    )
  }
  local_mocked_bindings(bnec = mock_bnec, .package = "bayesnec")
  expect_message(
    out <- suppressWarnings(with_fork_plan(
      bnec_group(y ~ crf(x, model = "nec3param"), data = d,
                 group_var = "site", seed = 338)
    )),
    "Fitting 2 levels in parallel"
  )
  expect_s3_class(out, "bayesnecgroupfit")
  expect_identical(names(out$fits), c("a", "b"))
  expect_identical(out$n, c(50L, 50L))
  expect_identical(
    unname(lapply(out$fits, function(f) f$rows)),
    unname(split(row.names(d), d$site))
  )
  pids <- vapply(out$fits, function(f) f$pid, integer(1))
  expect_length(unique(pids), 2L)
  expect_false(any(pids == parent_pid))
  caches <- vapply(out$fits, function(f) f$cache, character(1))
  expect_length(unique(caches), 2L)
  expect_match(basename(caches), "^bayesnec-stan-[0-9]+$")
  expect_false(any(vapply(out$fits, function(f) f$has_big, logical(1))))
})

test_that("a formula stops holding the environment it was written in", {
  # A formula records where it was created, and serialize() writes that
  # environment out in full. narrow_environment() does not reach it: it
  # replaces the environment of the applied function, and the formula inside
  # travels with its own. Measured rather than asserted loosely, because the
  # size is the whole point.
  mib <- function(x) length(serialize(x, NULL)) / 1024^2
  make <- function() {
    big <- rnorm(1e6)
    bayesnecformula(y ~ crf(x, model = c("nec3param", "ecx4param")))
  }
  f <- make()
  narrowed <- bayesnec:::narrow_formula_environment(f, nec_data)
  # The class survives, and so does the formula itself.
  expect_s3_class(narrowed, "bayesnecformula")
  expect_true(identical(narrowed[[2]], f[[2]]))
  # The 7.6 MiB vector is no longer reachable from the formula, which is the
  # property; the sizes below are its consequence. Asserted on the binding
  # rather than on bytes alone, because what a serialised environment weighs
  # depends on everything else in the chain and so differs between a console
  # session and a check runner.
  reachable <- function(e, what) {
    while (is.environment(e) && !bayesnec:::env_by_reference(e)) {
      if (exists(what, envir = e, inherits = FALSE)) return(TRUE)
      e <- parent.env(e)
    }
    FALSE
  }
  expect_true(reachable(environment(f), "big"))
  expect_false(reachable(environment(narrowed), "big"))
  # The model frame holds the same environment, through the .Environment of
  # its terms attribute, and amend() exports one to every worker.
  before <- mib(f)
  after <- mib(narrowed)
  frame_before <- model.frame(f, data = nec_data)
  frame_after <- model.frame(narrowed, data = nec_data)
  terms_env <- function(frame) attr(attr(frame, "terms"), ".Environment")
  expect_gt(before, 7)
  expect_lt(after, before / 50)
  expect_true(reachable(terms_env(frame_before), "big"))
  expect_false(reachable(terms_env(frame_after), "big"))
  # Do not assert the frame's byte size. model.frame.bayesnecformula() binds
  # trials() in this terms environment; with R_KEEP_PKG_SOURCE=yes, serialising
  # that function includes source references which grew to 1.20 MiB after the
  # CI suite had forced the namespace. That size is unrelated to the caller's
  # environment, whose absence is asserted directly above.
})

test_that("a name the formula uses and the data does not supply is kept", {
  # model.frame() resolves a term against data first and the formula's
  # environment second, so blanking the environment would be wrong. The model
  # argument of crf() is the case that matters: #319 resolves it there.
  make <- function() {
    big <- rnorm(1e6)
    mods <- c("nec3param", "ecx4param")
    bayesnecformula(y ~ crf(x, model = mods))
  }
  f <- make()
  narrowed <- bayesnec:::narrow_formula_environment(f, nec_data)
  expect_identical(get("mods", envir = environment(narrowed)),
                   c("nec3param", "ecx4param"))
  # The model set is kept and the 7.6 MiB vector beside it is not. Compared
  # against the formula it came from rather than against a byte count, because
  # what a serialised environment weighs depends on the rest of its chain.
  expect_lt(length(serialize(narrowed, NULL)),
            length(serialize(f, NULL)) / 50)
  # A column of the data is not copied: the model frame resolves it from
  # there, and copying it would send the data twice.
  expect_false(exists("y", envir = environment(narrowed), inherits = FALSE))
})

test_that("an environment R sends by reference is left alone", {
  # The global environment, a namespace and an attached package are written as
  # a reference, so a formula holding one adds nothing and rebuilding it would
  # only drop names. The walk up the parent chain stops at the first of them
  # for the same reason.
  # Set rather than inherited: a formula written inside a test_that() block
  # records the test's own frame, which is an ordinary environment.
  f <- bayesnecformula(y ~ crf(x, model = "nec3param"))
  environment(f) <- globalenv()
  expect_identical(
    environment(bayesnec:::narrow_formula_environment(f, nec_data)),
    globalenv()
  )
  # And where it is narrowed, the replacement is parented at the point the walk
  # stopped, so the rest of the lookup chain is the one the formula had. A
  # frame whose parent is baseenv() therefore still resolves through baseenv()
  # and not through the bayesnec namespace, which is what the trials() test in
  # test-bayesnecformula.R rests on.
  outer <- new.env(parent = baseenv())
  inner <- new.env(parent = outer)
  g <- bayesnecformula(y ~ crf(x, model = "nec3param"))
  environment(g) <- inner
  expect_identical(
    parent.env(environment(bayesnec:::narrow_formula_environment(g, nec_data))),
    baseenv()
  )
  expect_true(bayesnec:::env_by_reference(globalenv()))
  expect_true(bayesnec:::env_by_reference(asNamespace("stats")))
  expect_true(bayesnec:::env_by_reference(baseenv()))
  expect_false(bayesnec:::env_by_reference(new.env()))
})

test_that("a function the formula names comes with its own environment", {
  # The limit of what this can do, recorded rather than hidden. A user
  # transformation defined beside the formula is a closure over the same
  # environment, so the name is copied with everything it closed over. The
  # closure is needed; the vector beside it is not, and there is no way to tell
  # them apart from the formula. A helper defined in a knitr chunk therefore
  # still gives a large fit.
  make <- function() {
    big <- rnorm(1e6)
    halve <- function(z) z / 2
    bayesnecformula(y ~ crf(halve(x), model = "nec3param"))
  }
  narrowed <- bayesnec:::narrow_formula_environment(make(), nec_data)
  expect_true(exists("halve", envir = environment(narrowed), inherits = FALSE))
  expect_gt(length(serialize(narrowed, NULL)) / 1024^2, 7)
})

test_that("a list plan whose first strategy is sequential asks for the models", {
  skip_unless_future()
  old_limit <- options(parallelly.maxWorkers.localhost = Inf)
  on.exit(options(old_limit), add = TRUE)
  # Entering a future is what moves the plan on to the next strategy, so the
  # levels are still dispatched; a sequential strategy runs them one at a time
  # in the parent, and each level's model loop then sees the inner plan.
  # Measured on R 4.6.1 with future 1.70.0: nbrOfWorkers() is 1 in the parent
  # and 2 inside the level future. Without this branch the same plan fitted
  # both loops in sequence and said nothing, because bnec_plan_is_parallel()
  # reads only the first strategy.
  old <- future::plan(list(future::sequential,
                           future::tweak(future::multisession,
                                         workers = I(2))))
  on.exit(future::plan(old), add = TRUE)
  expect_message(out <- bayesnec:::plan_group_levels(3, 11),
                 "first strategy is sequential")
  expect_true(out$dispatch)
  expect_false(out$concurrent)
  skip_unless_worker_sees_internals()
  # suppressWarnings for future's own check on connections opened inside a
  # future: starting the inner multisession cluster opens two of them and they
  # stay open, which is what makes the cluster reusable. Every nested plan of
  # multisession strategies emits it, once per level, and ?bnec_group says so.
  seen <- suppressWarnings(bayesnec:::bnec_parallel_lapply(1:2, function(i) {
    future::nbrOfWorkers()
  }, parallel = TRUE))
  expect_equal(unlist(seen), c(2, 2))
})

test_that("the nested plan the message recommends asserts its worker count", {
  skip_unless_future()
  # future sets mc.cores to 1 inside a worker, parallelly reads that as the
  # core budget, and its hard limit is 300 per cent, so a bare inner worker
  # count is refused where I() is not. Measured on R 4.6.1 with future 1.70.0
  # and parallelly 1.48.0, one run each: under
  # plan(list(tweak(multisession, workers = 2), tweak(multisession,
  # workers = 4))) an inner future returned "Attempting to set up 4 localhost
  # parallel workers with only 1 CPU cores available ... The hard limit is set
  # to 300%", and the same plan with workers = I(4) returned 4. The refusal
  # depends on the outer count as well: one outer worker accepted the bare 4.
  #
  # Building that cluster is not asserted here. Two measurements of it
  # disagree -- 1.7 s on one run of a bare future_lapply, and no completion in
  # 280 s on two runs of the same plan inside this file -- and repeated
  # multisession cluster creation in one session stalls on the development
  # host often enough that a test which builds one is a flake rather than a
  # check. What is asserted is that the advice bayesnec prints carries the
  # I(), without which the call it recommends is the one refused above.
  expect_match(
    suppressMessages(
      capture_messages(with_parallel_plan(bayesnec:::plan_group_levels(3, 2)))
    ),
    "workers = I(", fixed = TRUE, all = FALSE
  )
})

test_that("a NULL binding is kept rather than deleted", {
  # vals[[nm]] <- x deletes the element when x is NULL, which left the name out
  # of the replacement environment and let it resolve against the parent chain
  # instead. A decoy of the same name there then answered for it, so a model
  # set that get_model_from_formula() would have refused became a fit of
  # something else.
  make <- function() {
    mods <- NULL
    bayesnecformula(y ~ crf(x, model = mods))
  }
  narrowed <- bayesnec:::narrow_formula_environment(make(), nec_data)
  expect_true(exists("mods", envir = environment(narrowed), inherits = FALSE))
  expect_null(get("mods", envir = environment(narrowed), inherits = FALSE))
})

test_that("the levels of a grouped call are seeded from the caller", {
  # Without this the arrangement decides the answer: expand_manec() realises
  # the weighted-draw seed from whatever stream bnec() is running in, so the
  # levels in a worker draw from the worker's and the levels in the parent from
  # the session's -- and plan_group_levels() reads the arrangement off the
  # worker count, so the machine would change the estimates.
  seeds <- bayesnec:::group_level_seeds
  # A supplied seed fixes them outright, so a grouped call repeats with no
  # set.seed() in the session.
  expect_identical(seeds(4, 17), seeds(4, 17))
  expect_false(identical(seeds(4, 17), seeds(4, 18)))
  expect_length(seeds(4, 17), 4L)
  # With none supplied they come from the session's stream, so set.seed()
  # before the call fixes them.
  set.seed(338)
  a <- seeds(3)
  set.seed(338)
  expect_identical(seeds(3), a)
  # The caller's stream is left where it was found, either way.
  set.seed(338)
  before <- get(".Random.seed", envir = globalenv())
  invisible(seeds(5, 17))
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  invisible(seeds(5))
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  # And so is the generator kind, which the seeded path pins.
  kind <- RNGkind()
  on.exit(suppressWarnings(do.call(RNGkind, as.list(kind))), add = TRUE)
  suppressWarnings(RNGkind(sample.kind = "Rounding"))
  invisible(seeds(3, 17))
  expect_identical(RNGkind()[3], "Rounding")
})

test_that("a seeded body gives one answer on both arrangements", {
  skip_unless_future()
  skip_unless_worker_sees_internals()
  # The mechanism behind the claim that the arrangement does not change a
  # grouped call's estimates, pinned without fitting anything. The body stands
  # in for one level: it seeds itself from the level seed the parent realised,
  # then makes the two draws a level makes -- the initial-value search, and the
  # single sample.int() expand_manec() uses to pick the weighted draw.
  #
  # It fails if the set.seed() in bnec_group()'s fit_level is removed, and it
  # fails if bnec_parallel_lapply() stops restoring the parent's RNG kind in
  # the worker, because the same seed then draws from L'Ecuyer-CMRG in a worker
  # and from Mersenne-Twister in the parent.
  set.seed(1)
  seeds <- bayesnec:::group_level_seeds(4, 17)
  seeded <- function(i) {
    set.seed(seeds[i])
    c(runif(1), sample.int(.Machine$integer.max, 1))
  }
  unseeded <- function(i) c(runif(1), sample.int(.Machine$integer.max, 1))
  expect_identical(
    bayesnec:::bnec_parallel_lapply(1:4, seeded, parallel = FALSE),
    with_parallel_plan(
      bayesnec:::bnec_parallel_lapply(1:4, seeded, parallel = TRUE)
    )
  )
  # The same comparison without the seed, which is what earlier versions did
  # and what made the arrangement decide the answer.
  set.seed(2)
  a <- bayesnec:::bnec_parallel_lapply(1:4, unseeded, parallel = FALSE)
  set.seed(2)
  b <- with_parallel_plan(
    bayesnec:::bnec_parallel_lapply(1:4, unseeded, parallel = TRUE)
  )
  expect_false(identical(a, b))
})

test_that("a level seed means one thing whatever sampler the session is in", {
  # sample.int()'s algorithm changed in R 3.6.0 and set.seed() with
  # kind = NULL leaves whichever is in force, so without pinning the sampler
  # the same `seed` realised different level seeds in a session set to the
  # pre-3.6.0 one -- and a grouped call would not repeat across two sessions
  # configured differently.
  kind <- RNGkind()
  on.exit(suppressWarnings(do.call(RNGkind, as.list(kind))), add = TRUE)
  suppressWarnings(RNGkind(sample.kind = "Rejection"))
  a <- bayesnec:::group_level_seeds(4, 17)
  suppressWarnings(RNGkind(sample.kind = "Rounding"))
  b <- bayesnec:::group_level_seeds(4, 17)
  expect_identical(a, b)
  # And the generator as well as the sampler: "L'Ecuyer-CMRG" is what a user
  # doing their own parallel work sets, and it is a generator rather than a
  # sampler, so pinning only the sampler left the same seed realising different
  # level seeds there.
  suppressWarnings(RNGkind(kind = "L'Ecuyer-CMRG", sample.kind = "Rejection"))
  expect_identical(bayesnec:::group_level_seeds(4, 17), a)
  # The session is left in whatever it was in, both times.
  expect_identical(RNGkind()[1], "L'Ecuyer-CMRG")
  suppressWarnings(RNGkind(kind = "Mersenne-Twister", sample.kind = "Rounding"))
  invisible(bayesnec:::group_level_seeds(4, 17))
  expect_identical(RNGkind()[3], "Rounding")
})

test_that("a grouped call leaves the caller's stream where it found it", {
  skip_on_cran()
  skip_if_not_installed("R.utils")
  # fit_level() calls set.seed() on every path, so without a restore around the
  # whole loop a grouped call fitted with the levels in sequence left the
  # session at the last level's seed -- and, where `seed` was supplied, at a
  # constant, so set.seed(i); bnec_group(..., seed = 1); rnorm(1) returned one
  # number for every i. Asserted on the real call, because the restore is in
  # bnec_group() and not in the dispatcher.
  #
  # timeout is how the loop is entered without fitting anything: it fires
  # before brm() reaches the sampler, so nothing is compiled, the call ends in
  # an error, and what is under test is the on.exit restore -- which an error
  # exit has to honour as much as a return does.
  d <- nec_data
  d$site <- rep(c("a", "b"), length.out = nrow(d))
  after <- function(i) {
    set.seed(i)
    suppressMessages(suppressWarnings(try(
      bnec_group(y ~ crf(x, model = "nec3param"), data = d,
                 group_var = "site", seed = 1, timeout = 0.001, chains = 2,
                 iter = 200, refresh = 0),
      silent = TRUE
    )))
    runif(1)
  }
  expect_false(identical(after(1), after(2)))
  # And the same session seed gives the same next draw, which is what a
  # restored stream means.
  expect_identical(after(3), after(3))
})
