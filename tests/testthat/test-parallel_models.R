# Use multisession to exercise serialisation on every supported platform.
# Restore the caller's plan after the test.
with_parallel_plan <- function(code, workers = 2) {
  # Temporarily lift the worker limit: setup.R sets mc.cores to one, which
  # parallelly otherwise treats as the budget for these explicit worker tests.
  old_limit <- options(parallelly.maxWorkers.localhost = Inf)
  on.exit(options(old_limit), add = TRUE)
  old <- future::plan(future::multisession, workers = workers)
  on.exit(future::plan(old), add = TRUE)
  code
}

# Exercise forked workers separately: they inherit memory and RNG state and
# share stderr, so multisession alone does not cover this backend.
# Skip when future cannot provide the requested forked workers.
with_fork_plan <- function(code, workers = 2) {
  old_limit <- options(parallelly.maxWorkers.localhost = Inf)
  on.exit(options(old_limit), add = TRUE)
  old <- suppressWarnings(future::plan(future::multicore, workers = workers))
  on.exit(future::plan(old), add = TRUE)
  # Ask future whether this plan actually forks; unavailable backends use one
  # worker. Avoid a direct call to the undeclared parallelly dependency.
  skip_if(future::nbrOfWorkers() < workers,
          "forking is not available on this platform")
  code
}

skip_unless_future <- function() {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
}

# Multisession workers load the installed package, not load_all() internals.
# Skip when that installation lacks the required helpers; R CMD check
# installs the package before running these tests.
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
  # A lone fit keeps its chain-level parallelism; clamping cores would slow it.
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
  # With an existing seed, RNGkind() deterministically converts the state.
  # This dependency check distinguishes that from clock-seeding a new state.
  old_kind <- RNGkind()
  old_seed <- if (exists(".Random.seed", envir = globalenv(),
                         inherits = FALSE)) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  }
  on.exit({
    # Restore the kind before the seed, including when no seed existed.
    # Removing .Random.seed alone does not restore the generator kind.
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
  # Check both the parent's generator and a worker's L'Ecuyer conversion.
  # Repeat each pair to reduce the chance of a clock-seeded collision.
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
  # An explicit seed must produce the same draws in a worker and the parent.
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
  # Compare the actual initial-value search without Stan sampling.
  # A worker's L'Ecuyer generator must not change the seeded result.
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
  # Forked workers inherit memory and RNG state rather than receive them.
  # Check the seeded draws, the caller's stream and the failure record here.
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
  # The parallel loop must preserve the caller's stream for model averaging.
  # Take the baseline after the skip helper, which itself advances that stream.
  # This does not require equality with the sequential loop's final stream.
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
  # Remove the seed after the skip helper, which can create one itself.
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
  # A one-worker fallback must preserve chain parallelism and avoid claiming
  # that models will run concurrently.
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
  # Narrow the closure on both paths so missing bindings fail even without
  # a parallel plan; otherwise it exports unrelated fits and data to workers.
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
  # A family constructed beside a large object can retain it through closures,
  # even when the formula and applied function have narrow environments.
  make_family <- function() {
    big <- numeric(1e6)
    Gamma(link = "identity")
  }
  supplied <- make_family()
  family <- bayesnec:::validate_family(supplied, link_source = "symbol")
  brm_args <- list(family = bayesnec:::unmark_family(family))
  # Set a finite maxSize so future computes total_size even when the session
  # has disabled its export limit; NA totals would test nothing.
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
  supplied_size <- attr(supplied_globals, "total_size")
  rebuilt_size <- attr(globals, "total_size")
  expect_gt(supplied_size, 7 * 1024^2)
  # Compare the removed bytes rather than a ratio so a platform-specific fixed
  # baseline in the discovered globals cannot obscure removal of the sentinel.
  expect_gt(supplied_size - rebuilt_size, 7 * 1024^2)
})

test_that("future's own seeding would have changed those draws", {
  skip_unless_future()
  # future.seed = TRUE installs L'Ecuyer; set.seed() alone keeps that kind.
  # This characterises future: failure can mean its seeding changed, making
  # the restore redundant rather than showing a bayesnec regression.
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
  # Both equations must survive family screening or the set becomes one fit
  # and never runs in parallel. Compare posterior draws, excluding stored calls.
  # Pass brm() arguments through bnec()'s dots, not as a list.
  set_call <- function() {
    suppressMessages(suppressWarnings(
      bnec(y ~ crf(x, model = c("nec3param", "nec4param")), data = nec_data,
           seed = 184, chains = 2, iter = 200, refresh = 0)
    ))
  }
  sequential <- set_call()
  # Name the returned class: a collapsed one-model set means this fixture
  # failed to exercise parallel fitting.
  skip_if(!inherits(sequential, "bayesmanecfit"),
          paste("expected a bayesmanecfit from the two-model set, got",
                paste(class(sequential), collapse = "/")))
  parallel <- with_parallel_plan({
    skip_unless_worker_sees_internals()
    set_call()
  })
  expect_s3_class(parallel, "bayesmanecfit")
  expect_identical(names(parallel$mod_fits), names(sequential$mod_fits))
  # Compare fitted-model draws only. The model-averaging seed and indices
  # come from ambient streams that differ between the two arrangements.
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
  # Use a short timeout to compare failure handling without Stan sampling.
  # Assert agreement between plans, not a particular timeout result.
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

# The level loop of a grouped call.

test_that("the two arrangements of one plan are counted in rounds of one fit", {
  # The unit is one fit sampling its chains in sequence, which is what happens
  # inside a worker on either arrangement. ceiling() on both sides: a round is
  # set by its slowest member, so a part-filled round still takes a whole one.
  rounds <- bayesnec:::group_loop_rounds
  # Seven levels of eleven equations at four workers: the models take fewer.
  expect_equal(rounds(7, 11, 4)$models, 21)
  expect_equal(rounds(7, 11, 4)$levels, 22)
  # The same seven by eleven shape at eight workers: the levels take fewer.
  expect_equal(rounds(7, 11, 8)$models, 14)
  expect_equal(rounds(7, 11, 8)$levels, 11)
  # At four workers, two levels of fifteen equations favour the models: the
  # level arrangement cannot take fewer than fifteen rounds.
  expect_equal(rounds(2, 15, 4)$models, 8)
  expect_equal(rounds(2, 15, 4)$levels, 15)
  # One worker is parallel in name only and gives a tie, which the caller
  # resolves in favour of the models.
  expect_equal(rounds(7, 11, 1)$models, rounds(7, 11, 1)$levels)
  # An unbounded scheduler can dispatch one whole loop at once. Dividing by
  # Inf would incorrectly count zero rounds.
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
  # An explicit nested plan overrides the rounds comparison because the caller
  # has chosen how to divide workers between levels and models.
  expect_message(out <- bayesnec:::plan_group_levels(3, 2),
                 "plan is a list")
  expect_true(out$dispatch)
  expect_true(out$concurrent)
})

test_that("only the next strategy can drive the nested model loop", {
  skip_unless_future()
  old <- future::plan(list(
    future::sequential,
    future::sequential,
    future::tweak(future::multisession, workers = I(2))
  ))
  on.exit(future::plan(old), add = TRUE)
  # There are only two fitting loops. A parallel third strategy would require a
  # third nested future, so it cannot make the sequential second strategy drive
  # the model loop.
  out <- bayesnec:::plan_group_levels(3, 11)
  expect_false(out$dispatch)
  expect_false(out$concurrent)
})

test_that("each active host-process pair has its own compile directory", {
  # Separate caches by host and process to avoid concurrent Stan compilation
  # writing to one directory. Normalise paths before comparing Windows
  # separators with dirname() output.
  same_path <- function(a, b) {
    norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
    expect_identical(norm(a), norm(b))
  }
  own <- bayesnec:::worker_stan_cache_dir(host = "node/a", pid = 42)
  expect_identical(basename(own), "bayesnec-stan-node_a-42")
  expect_true(dir.exists(own))
  same_path(dirname(own), tempdir())
  # A directory the user chose is shared by every worker on every backend, so
  # it is separated the same way.
  root <- file.path(tempdir(), "bayesnec-cache-root-test")
  dir.create(root, showWarnings = FALSE)
  same_path(dirname(bayesnec:::worker_stan_cache_dir(root)), root)
  node_a <- bayesnec:::worker_stan_cache_dir(root, host = "node-a", pid = 17)
  node_b <- bayesnec:::worker_stan_cache_dir(root, host = "node-b", pid = 17)
  expect_false(identical(node_a, node_b))
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
  # do.call() must retain the formula's environment so crf() can resolve local
  # symbols; narrowing the applied function must not erase those bindings.
  e <- new.env()
  f <- y ~ x
  environment(f) <- e
  seen <- do.call(function(z, ...) environment(z), list(f, family = "beta"))
  expect_identical(seen, e)
})

test_that("a grouped call fits each level on its own rows", {
  skip_on_cran()
  # Run the real grouped fit sequentially and test parallel orchestration
  # separately with a mock. The large local vector checks formula narrowing.
  # Repeated real socket-plan calls stalled in the recorded development runs.
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
  # Compare with the same unnarrowed formula so unrelated session bindings
  # cannot determine the size reduction.
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
  # Mock bnec() under a forked plan to test splitting, dispatch, worker caches
  # and assembly without extra Stan compilation. This does not test a real
  # grouped fit under multisession; socket serialisation is checked separately.
  d <- nec_data
  d$site <- rep(c("a", "b"), length.out = nrow(d))
  big <- numeric(1e6)
  parent_pid <- Sys.getpid()
  mock_bnec <- function(formula, data, family, ...) {
    dots <- list(...)
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
      has_big = has_big,
      seed = dots[["seed"]],
      group_seed = dots[[".bayesnec_group_seed"]]
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
  expect_match(basename(caches), "^bayesnec-stan-.+-[0-9]+$")
  expect_false(any(vapply(out$fits, function(f) f$has_big, logical(1))))
  expect_identical(
    unname(vapply(out$fits, function(f) f$seed, integer(1))),
    out$level_seeds
  )
  expect_identical(
    unname(vapply(out$fits, function(f) f$group_seed, integer(1))),
    out$level_seeds
  )
})

test_that("a formula stops holding the environment it was written in", {
  # A formula serialises its own environment even when its enclosing applied
  # function has been narrowed.
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
  # Test reachability directly: serialised size also depends on unrelated
  # objects in the parent chain and differs between console and check runners.
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
  # Do not assert the model-frame byte size: serialised trials() source
  # references can inflate it under R_KEEP_PKG_SOURCE=yes. Check that the
  # caller's environment is absent instead.
})

test_that("a name the formula uses and the data does not supply is kept", {
  # Keep local symbols used by the formula: model.frame() resolves data first
  # and then the formula environment, including crf()'s model set.
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
  # Reference environments need no narrowing. Set them explicitly because a
  # formula created inside test_that() otherwise uses the test's ordinary frame.
  f <- bayesnecformula(y ~ crf(x, model = "nec3param"))
  environment(f) <- globalenv()
  expect_identical(
    environment(bayesnec:::narrow_formula_environment(f, nec_data)),
    globalenv()
  )
  # Parent the narrowed environment at the original stopping point so name
  # lookup still reaches baseenv() rather than the package namespace.
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

test_that("an arbitrary attached environment is narrowed by value", {
  attach(list(review_big = numeric(1e6)), name = "review_env")
  on.exit(detach("review_env"), add = TRUE)
  attached <- as.environment("review_env")
  expect_false(bayesnec:::env_by_reference(attached))
  f <- bayesnecformula(y ~ crf(x, model = "nec3param"))
  environment(f) <- new.env(parent = attached)
  narrowed <- bayesnec:::narrow_formula_environment(f, nec_data)
  expect_false(exists("review_big", envir = environment(narrowed),
                      inherits = TRUE))
})

test_that("a function the formula names comes with its own environment", {
  # A required local transformation can retain unrelated objects through its
  # closure. Formula narrowing cannot safely discard that closure environment.
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
  # Dispatch levels through the outer sequential future to expose the inner
  # parallel strategy to each model loop.
  old <- future::plan(list(future::sequential,
                           future::tweak(future::multisession,
                                         workers = I(2))))
  on.exit(future::plan(old), add = TRUE)
  expect_message(out <- bayesnec:::plan_group_levels(3, 11),
                 "first strategy is sequential")
  expect_true(out$dispatch)
  expect_false(out$concurrent)
  skip_unless_worker_sees_internals()
  # Suppress future's connection warning when an inner multisession cluster
  # stays open for reuse.
  seen <- suppressWarnings(bayesnec:::bnec_parallel_lapply(1:2, function(i) {
    future::nbrOfWorkers()
  }, parallel = TRUE))
  expect_equal(unlist(seen), c(2, 2))
})

test_that("the nested plan the message recommends asserts its worker count", {
  skip_unless_future()
  # Check the printed I(workers) advice only. Nested socket-cluster creation
  # stalled in repeated development runs, so this test does not verify the
  # cluster itself or its performance.
  expect_match(
    suppressMessages(
      capture_messages(with_parallel_plan(bayesnec:::plan_group_levels(3, 2)))
    ),
    "workers = I(", fixed = TRUE, all = FALSE
  )
})

test_that("a NULL binding is kept rather than deleted", {
  # Retain NULL bindings explicitly: deleting one lets a parent's same-named
  # value answer in its place and can change the requested model set.
  make <- function() {
    mods <- NULL
    bayesnecformula(y ~ crf(x, model = mods))
  }
  narrowed <- bayesnec:::narrow_formula_environment(make(), nec_data)
  expect_true(exists("mods", envir = environment(narrowed), inherits = FALSE))
  expect_null(get("mods", envir = environment(narrowed), inherits = FALSE))
})

test_that("the levels of a grouped call are seeded from the caller", {
  # Draw each level's seed in the caller before choosing an arrangement, so
  # model averaging does not depend on the worker count or on which process
  # fits a level.
  seeds <- bayesnec:::group_level_seeds
  # A supplied seed fixes them outright, so a grouped call repeats with no
  # set.seed() in the session.
  expect_identical(seeds(4, 17), seeds(4, 17))
  expect_false(identical(seeds(4, 17), seeds(4, 18)))
  expect_length(seeds(4, 17), 4L)
  # Reject malformed seeds; scalar NA requests an unseeded call. Retain
  # set.seed()'s convention of using the first element of a vector.
  expect_error(seeds(2, "bad"), "supplied seed")
  expect_error(seeds(2, Inf), "supplied seed")
  expect_identical(seeds(2, 1:2), seeds(2, 1))
  expect_length(seeds(2, NA_integer_), 2L)
  d <- nec_data
  d$site <- rep(c("a", "b"), length.out = nrow(d))
  expect_error(
    suppressMessages(bnec_group(
      y ~ crf(x, model = "nec3param"), data = d, group_var = "site",
      family = "Beta", seed = "bad"
    )),
    "supplied seed"
  )
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

test_that("a seeded level gives one answer with either model arrangement", {
  skip_unless_future()
  skip_unless_worker_sees_internals()
  # Mock both nested loops without Stan. with_group_seed() must make the
  # weighted draw independent of the ambient stream left by the model loop.
  level_seed <- bayesnec:::group_level_seeds(1, 17)
  run_level <- function(parallel) {
    set.seed(level_seed)
    fits <- bayesnec:::bnec_parallel_lapply(1:3, function(i) {
      set.seed(level_seed)
      runif(2)
    }, parallel = parallel)
    draw_seed <- bayesnec:::with_group_seed(
      level_seed, sample.int(.Machine$integer.max, 1)
    )
    list(fits = fits, draw_seed = draw_seed)
  }
  expect_identical(
    run_level(FALSE),
    with_parallel_plan(run_level(TRUE))
  )
  # A standalone bnec() call supplies NULL and retains the ambient-stream
  # behaviour documented for its model-averaging draw.
  set.seed(2)
  expected <- runif(1)
  set.seed(2)
  expect_identical(bayesnec:::with_group_seed(NULL, runif(1)), expected)
})

test_that("a level seed means one thing whatever sampler the session is in", {
  # group_level_seeds() pins the sampler: set.seed(sample.kind = NULL) would
  # keep the session's sample.int() choice and realise different level seeds.
  kind <- RNGkind()
  on.exit(suppressWarnings(do.call(RNGkind, as.list(kind))), add = TRUE)
  suppressWarnings(RNGkind(sample.kind = "Rejection"))
  a <- bayesnec:::group_level_seeds(4, 17)
  suppressWarnings(RNGkind(sample.kind = "Rounding"))
  b <- bayesnec:::group_level_seeds(4, 17)
  expect_identical(a, b)
  # Pin the generator too; fixing only the sampling algorithm is insufficient.
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
  # Test restoration around the real grouped loop, including its error exit.
  # A short timeout enters the loop without Stan sampling; level seeds must
  # not replace the caller's ambient stream.
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
