#' Is the user's \pkg{future} plan a parallel one?
#'
#' The decision is read from the plan rather than from an argument to
#' \code{\link{bnec}}. A \code{cores} or \code{parallel} argument would
#' duplicate state the plan already holds, and the two become ambiguous the
#' moment a user sets both. See \code{?bnec} under \emph{Fitting a model set in
#' parallel}.
#'
#' Tested on the strategy rather than on \code{future::nbrOfWorkers()}. A
#' \code{cluster} plan pointed at a single remote node reports one worker but
#' is still a parallel plan, and the question here is whether the user asked
#' for one, not how many workers it has.
#'
#' Returns \code{FALSE} where either package is absent, which is what keeps the
#' sequential path free of the dependency.
#'
#' @return A \code{\link[base]{logical}} vector of length 1.
#'
#' @noRd
bnec_plan_is_parallel <- function() {
  if (!requireNamespace("future", quietly = TRUE) ||
        !requireNamespace("future.apply", quietly = TRUE)) {
    return(FALSE)
  }
  strategy <- try(future::plan("list")[[1]], silent = TRUE)
  if (inherits(strategy, "try-error")) {
    return(FALSE)
  }
  !inherits(strategy, "sequential")
}

#' Decide how a model set will be fitted, and say so
#'
#' Called once, before the loop, by \code{\link{bnec}} and \code{\link{amend}}.
#' Returns the decision and the \code{\link[brms]{brm}} arguments that go with
#' it, so the two are made in one place and cannot disagree.
#'
#' \bold{Fewer than two models to fit is never parallel.} A parallel plan
#' provides no benefit over a single \code{\link[brms]{brm}} call, and treating
#' it as parallel would apply the \code{cores} limit below and so make a lone
#' fit slower than it is today. \code{\link{amend}} reaches this case whenever
#' every model in the amended set is carried over from the object.
#'
#' \bold{Under a parallel plan of more than one worker, each model samples its
#' chains in sequence.} \code{\link[brms]{brm}} already parallelises across
#' chains, so fitting models in parallel on top of that requests
#' \code{workers x chains} processes, and bayesnec passes no \code{cores}
#' argument of its own.
#'
#' It is worth being exact about what this prevents, because the obvious
#' account of it is wrong. \pkg{future} sets \code{mc.cores} to 1 inside a
#' worker itself, so \pkg{brms} does not in fact reach a value set in the
#' user's profile: measured 2026-09-11 with \code{options(mc.cores = 7)} in
#' \code{R_PROFILE_USER}, \code{getOption("mc.cores")} inside a future is 1
#' under \code{multisession} at two and four workers, under \code{multicore},
#' and under a two-node \code{cluster}. Passing \code{cores = 1} is therefore
#' not what stands between the user and sixteen processes; \pkg{future}
#' already does that. It is kept because it makes what \pkg{brms} is asked to
#' do a property of this package rather than of an implementation detail of
#' another. A
#' \code{cores} the user passed to \code{\link{bnec}}, which arrives here in
#' \code{brm_args} by way of its \code{...}, is left alone: nesting the two
#' levels is a legitimate thing to want where there are cores to spare, and
#' that argument is how it is asked for. \code{\link{amend}} has no such
#' argument -- it rebuilds \code{brm_args} from the stored \code{simdat} --
#' so \code{caller} decides whether the message offers that advice or says it
#' is unavailable.
#'
#' The decision is reported because a parallel run emits its per-model messages
#' out of order or not at all, depending on the backend, so the user otherwise
#' has no way to tell which path was taken.
#'
#' @param brm_args A named \code{\link[base]{list}} of arguments for
#' \code{\link[brms]{brm}}.
#' @param n_models A \code{\link[base]{numeric}} vector of length 1 giving the
#' number of models that will actually be fitted.
#' @param caller A \code{\link[base]{character}} vector of length 1, either
#' \code{"bnec"} or \code{"amend"}.
#'
#' @return A \code{\link[base]{list}} with elements \code{parallel}, a
#' \code{\link[base]{logical}} vector of length 1, and \code{brm_args}.
#'
#' @noRd
plan_model_set <- function(brm_args, n_models, caller = "bnec") {
  if (n_models < 2 || !bnec_plan_is_parallel()) {
    return(list(parallel = FALSE, brm_args = brm_args))
  }
  workers <- try(future::nbrOfWorkers(), silent = TRUE)
  if (inherits(workers, "try-error") || !is.numeric(workers)) {
    workers <- NA_integer_
  }
  # The clamp is on the worker count, not on the strategy. A plan can name a
  # parallel strategy and still resolve to one worker -- plan(multicore) does
  # exactly that on Windows, and on Linux and macOS inside RStudio or Positron,
  # where parallelly::supportsMulticore() is FALSE and future falls back to
  # evaluating in the parent. Clamping there would fit one model at a time with
  # its chains in sequence, which for a user who has set mc.cores is slower
  # than the release, while the message said the opposite. Unknown reads as
  # more than one, since the risk being guarded is oversubscription.
  one_worker <- isTRUE(workers == 1)
  supplied_cores <- "cores" %in% names(brm_args)
  if (!supplied_cores && !one_worker) {
    brm_args$cores <- 1
  }
  message(
    if (one_worker) {
      # No advice about which plan to use instead. A single-node cluster plan
      # reports one worker too, and there the fitting genuinely does move to
      # another process, so telling that user to switch to multisession would
      # be beside the point. What is true of every one-worker plan is that the
      # models are fitted one at a time and that nothing here is clamped.
      paste0("The future plan in effect resolves to a single worker, so the ",
             n_models, " models are fitted one at a time and bayesnec applies",
             " no cores limit here. plan(multicore) resolves this way",
             " wherever forking is unavailable, which includes Windows,",
             " RStudio and Positron.")
    } else {
      paste0(
        "Fitting ", n_models, " models in parallel over ", workers,
        " workers, under the future plan already set.\n",
        if (supplied_cores) {
          paste0("Each model samples its chains over cores = ",
                 brm_args$cores, ", as you supplied, so up to ", workers,
                 " x ", brm_args$cores, " chains may sample at once.")
        } else {
          paste0("Each model samples its chains in sequence (cores = 1);",
                 if (identical(caller, "amend")) {
                   paste0(" amend() takes no brms arguments, so to nest the",
                          " two levels of parallelism refit the set with",
                          " bnec().")
                 } else {
                   paste0(" pass `cores` to bnec() to nest the two levels of",
                          " parallelism.")
                 })
        },
        "\nPer-model messages from a worker may arrive out of order, or not",
        " at all. A model that fails is recorded either way; see",
        " ?failed_models."
      )
    }
  )
  list(parallel = TRUE, brm_args = brm_args)
}

#' A worker count with its noun in the right number
#'
#' Written out because the same count reads as "1 workers" often enough to be
#' worth one function rather than a conditional at each site.
#'
#' @param n A \code{\link[base]{numeric}} vector of length 1.
#'
#' @return A \code{\link[base]{character}} vector of length 1.
#'
#' @noRd
n_workers <- function(n) {
  paste0(n, " worker", if (!isTRUE(n == 1)) "s")
}

#' The number of rounds each arrangement of one plan takes
#'
#' A grouped call has two loops that could use the workers: the levels, and the
#' models within a level. One \pkg{future} plan of \emph{W} workers can drive
#' one of them, so the choice is between two arrangements, and this counts what
#' each takes.
#'
#' The unit is one fit whose chains sample one after another, and it is the same
#' unit on both arrangements. Whichever loop is dispatched, the fitting happens
#' inside a worker, and \pkg{future} sets \code{mc.cores} to 1 there; on the
#' model arrangement \code{plan_model_set()} additionally passes
#' \code{cores = 1}. A \code{cores} the user supplied is honoured on both. So
#' the two counts are comparable and their ratio is the ratio of wall clock,
#' under the assumption that every fit takes the same time.
#'
#' That assumption is the weak point, and it is wrong in a known direction. The
#' 23 equations differ in fitting time by an order of magnitude, so a round is
#' set by its slowest member and a count of rounds understates the arrangement
#' that packs fewer fits into each round. The level arrangement puts \emph{M}
#' fits in a round and the model arrangement \emph{W}, so where \emph{M} is the
#' larger the count favours the level arrangement more than the clock would.
#' Ties therefore go to the model arrangement, which is also what the release
#' does.
#'
#' \code{n_models} is the set the formula asks for, not the set that will be
#' fitted: \code{check_models()} runs inside each level's \code{\link{bnec}}
#' call and may drop an equation the family or the data rules out. It is an
#' upper bound on both counts, so this is a scheduling estimate rather than a
#' measurement.
#'
#' An unbounded worker count is counted rather than refused. A scheduler backend
#' reports \code{Inf} from \code{\link[future]{nbrOfWorkers}} ---
#' \pkg{future.batchtools} is the usual one, and it is the setting a grouped
#' call of this size belongs in --- and there each arrangement takes one round
#' of the loop it dispatches: \emph{M} for the levels and \emph{L} for the
#' models. \code{NA} is reserved for a count that genuinely could not be read.
#'
#' @param n_levels A \code{\link[base]{numeric}} vector of length 1.
#' @param n_models A \code{\link[base]{numeric}} vector of length 1, or
#' \code{NA}.
#' @param workers A \code{\link[base]{numeric}} vector of length 1, or
#' \code{NA}.
#'
#' @return A \code{\link[base]{list}} with elements \code{levels} and
#' \code{models}, each a \code{\link[base]{numeric}} vector of length 1.
#'
#' @noRd
group_loop_rounds <- function(n_levels, n_models, workers) {
  if (length(workers) != 1 || is.na(workers) || !is.numeric(workers) ||
        length(n_models) != 1 || is.na(n_models)) {
    return(list(levels = NA_real_, models = NA_real_))
  }
  if (is.infinite(workers)) {
    # ceiling(L / Inf) is 0, which would report an arrangement that takes no
    # time at all, so the unbounded case is written out rather than divided.
    return(list(levels = as.numeric(n_models), models = as.numeric(n_levels)))
  }
  list(levels = ceiling(n_levels / workers) * n_models,
       models = n_levels * ceiling(n_models / workers))
}

#' A Stan compile directory that no two workers share
#'
#' Every level of a grouped call fits the same equations, so parallel levels
#' compile the same Stan programs at the same time. \pkg{cmdstanr} does not lock
#' its compile cache: two levels on a cold cache would write the same
#' \code{.stan} file and run \code{make} on the same executable path at once.
#' \code{hpc/precompile-hpc.sh} serialises its job array for exactly this
#' reason. The model loop inside one \code{\link{bnec}} call is not exposed to
#' it, because there the equations differ and so do the programs.
#'
#' \bold{Keyed on the process, not on the level.} A key per level would make a
#' worker that draws three levels compile every equation three times, so a
#' grouped call of seven levels and eleven equations would compile 77 programs
#' where it needs at most one per worker. A key per process gives the same
#' separation --- no two futures run in one process at one time --- at one
#' compilation per equation per worker.
#'
#' It is applied on both backends although only one needs it. Under
#' \code{multisession} each worker is a separate process with its own
#' \code{\link[base]{tempdir}}, so the default cache location already differs;
#' under \code{multicore} the child inherits the parent's, so it does not. A
#' directory per process is the same arrangement on both, and it is the case
#' where the user has set \code{cmdstanr_write_stan_file_dir} that makes it
#' necessary on both: a directory the user chose is shared by every worker
#' whatever the backend.
#'
#' Rooted at \code{cmdstanr_write_stan_file_dir} where the user has set one and
#' at \code{\link[base]{tempdir}} otherwise, which is where \pkg{cmdstanr}
#' writes by default.
#'
#' \bold{What this adds is compilation on a cold cache.} A parallel grouped run
#' compiles each equation once per worker rather than once, and a process id is
#' not the same on the next run, so a persistent cache the user has warmed is
#' not read by a parallel grouped call and the programs that call writes are not
#' read by the next one. Against a fit measured at 18 minutes on the AIMS HPC
#' that is small; against a short fit on a cold cache it need not be, and
#' \code{group_loop_rounds()} counts sampling only. Fitting the levels in
#' sequence --- which a plan of \code{list(sequential, ...)} asks for --- uses
#' the cache as it always did.
#'
#' \code{root} is read in the parent and passed in rather than read here.
#' \pkg{future} exports the globals a future needs and not the session's
#' options, so a \code{multisession} worker starts with the option unset: read
#' here, a deliberate persistent cache would be honoured under a forking plan,
#' which inherits options, and silently ignored under every other one.
#'
#' This addresses \pkg{cmdstanr} only. \pkg{rstan} caches a compiled program
#' under \code{rstan_options(auto_write = TRUE)}, in \code{\link[base]{tempdir}}
#' and keyed by the program text, which a forked worker shares with its parent.
#' That option is \code{FALSE} by default and no argument changes that
#' location, so it is documented in \code{?bnec_group} rather than worked
#' around here.
#'
#' @param root A \code{\link[base]{character}} vector of length 1 giving the
#' directory to write under, or \code{NULL} for \code{\link[base]{tempdir}}.
#'
#' @return A \code{\link[base]{character}} vector of length 1, the directory,
#' which has been created.
#'
#' @noRd
worker_stan_cache_dir <- function(root = NULL) {
  base <- root
  if (is.null(base) || !is.character(base) || length(base) != 1 ||
        is.na(base)) {
    base <- tempdir()
  }
  path <- file.path(base, paste0("bayesnec-stan-", Sys.getpid()))
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  path
}

#' One seed per level, realised in the caller's session
#'
#' \code{\link{bnec_group}} applies these inside each level, so that a level's
#' whole random procedure --- the initial-value search where no \code{seed} was
#' supplied, and the weighted-draw seed \code{expand_manec()} realises for the
#' model-averaged estimates --- answers to the caller's session and not to
#' whichever process the level was fitted in.
#'
#' Without it the arrangement decides the answer. \code{expand_manec()} draws
#' \code{w_draw_seed} from the ambient stream (#216), so a level fitted in a
#' worker draws from that worker's L'Ecuyer-CMRG stream and a level fitted in
#' the parent from the session's. \code{plan_group_levels()} chooses the
#' arrangement from the worker count, so one script at one seed would report
#' different model-averaged estimates on a four-core and an eight-core machine.
#' This is the one place a grouped call can close that gap without changing
#' \code{expand_manec()}, which \code{bnec_parallel_lapply()} records as the
#' wider fix and as #216's deliberate decision.
#'
#' \bold{Derived from \code{seed} where the user supplied one.} A grouped call
#' then repeats itself with no \code{\link[base]{set.seed}} in the session,
#' which is more than \code{\link{bnec}} offers for a single set. Where none was
#' supplied the seeds come from the session's stream, so
#' \code{\link[base]{set.seed}} before the call fixes them.
#'
#' \bold{The caller's stream and generator kind are both put back.}
#' \code{\link{bnec_group}} is not entitled to move either, which is the rule
#' \code{bnec_parallel_lapply()} and \code{weighted_draw_index()} follow, and
#' for the same reason: a fit must not reset a user's simulation seed. The
#' generator as well as the stream, because the line below pins it.
#'
#' \bold{The sampler kind is pinned where a \code{seed} was supplied.}
#' \code{\link[base]{sample.int}}'s algorithm changed in R 3.6.0 and
#' \code{\link[base]{set.seed}} with \code{kind = NULL} leaves whichever is in
#' force, so the same \code{seed} would otherwise realise different level seeds
#' in a session set to the pre-3.6.0 sampler. \code{"Rejection"} is named so
#' that \code{seed} means one thing. \code{\link{bnec_group}} records what was
#' realised on the returned object as well, for the reason
#' \code{expand_manec()} stores \code{w_draw_index} beside
#' \code{w_draw_seed}: these objects are archived and reopened years later, and
#' a stored realisation cannot drift the way a regenerated one can.
#'
#' \bold{The level seed deliberately supersedes the stream
#' \code{future.seed = TRUE} installs.} That stream is an L'Ecuyer-CMRG
#' substream, chosen by \code{bnec_parallel_lapply()} so that a body which uses
#' the RNG is parallel-safe whatever it does; seeding over it replaces a
#' guaranteed non-overlapping substream with an ordinary Mersenne-Twister one.
#' That is safe for this body and not in general: the only draws a level makes
#' are the initial-value search and the single \code{sample.int()} of
#' \code{expand_manec()}, and the seeds themselves are drawn from one stream in
#' the parent, so two levels start from two draws of \code{sample.int()} rather
#' than from two points in one substream. Reproducibility across the two
#' arrangements is judged worth that, and nothing else dispatched by
#' \code{bnec_parallel_lapply()} reseeds.
#'
#' One consequence to state rather than hide: the realised estimates of a
#' grouped call differ from those of earlier versions, because the levels are
#' now seeded rather than drawing from the stream as they are reached. They are
#' another realisation of the same weighting, and they are the same on every
#' arrangement, which they were not.
#'
#' @param n_levels A \code{\link[base]{numeric}} vector of length 1.
#' @param seed The \code{seed} the caller passed through to
#' \code{\link[brms]{brm}}, or \code{NULL}.
#'
#' @return An \code{\link[base]{integer}} vector of length \code{n_levels}.
#'
#' @noRd
group_level_seeds <- function(n_levels, seed = NULL) {
  rng_kind <- RNGkind()
  has_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (has_seed) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    # The kind first, because the generator is encoded in .Random.seed[1].
    suppressWarnings(do.call(RNGkind, as.list(rng_kind)))
    if (is.null(old_seed)) {
      suppressWarnings(rm(".Random.seed", envir = globalenv()))
    } else {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)
  if (!is.null(seed) && length(seed) == 1 && is.numeric(seed) &&
        is.finite(seed)) {
    suppressWarnings(set.seed(seed, sample.kind = "Rejection"))
  }
  sample.int(.Machine$integer.max, n_levels)
}

#' Decide how the levels of a grouped call will be fitted, and say so
#'
#' Called once, before the level loop, by \code{\link{bnec_group}}. The
#' counterpart of \code{plan_model_set()}, and the decision is read from the
#' plan for the same reason: an argument beside it would duplicate state the
#' plan already holds. See \code{?bnec_group} under \emph{Fitting the levels in
#' parallel}.
#'
#' \bold{A plan of one strategy drives one loop.} \pkg{future} evaluates a
#' nested future sequentially unless the plan is a list, so a level fitted in a
#' worker fits its own models one at a time: measured on R 4.6.1 with
#' \pkg{future} 1.70.0, \code{plan(multisession, workers = 2)} reports the
#' strategy inside a worker as \code{sequential} and \code{nbrOfWorkers()} there
#' as 1. Dispatching the levels therefore takes the workers away from the model
#' loop rather than adding to it, and the two arrangements are counted against
#' each other by \code{group_loop_rounds()}. The larger count is not taken; a
#' tie leaves the levels in sequence, which is what the release does.
#'
#' \bold{A plan that is a list is honoured without being counted}, because there
#' the user has divided the workers between the two loops deliberately. The
#' outer element governs the level loop and the next one the model loop:
#' measured on the same versions, inside a level worker of
#' \code{plan(list(tweak(multisession, workers = 2), tweak(multisession,
#' workers = I(3))))}, \code{nbrOfWorkers()} reports 3 and the model loop
#' parallelises over them.
#'
#' \bold{A list whose outer element is \code{sequential} is how the release
#' arrangement is asked for.} The levels are still dispatched through
#' \pkg{future}, because entering a future is what moves the plan on to the next
#' strategy, but a sequential strategy evaluates them one at a time in the
#' parent, so the model loop inside each gets the whole of the inner plan.
#' Measured on the same versions: under
#' \code{plan(list(sequential, tweak(multisession, workers = I(3))))} the parent
#' reports one worker and \code{nbrOfWorkers()} inside the level future reports
#' 3. Without this branch that plan fitted both loops in sequence and said
#' nothing, since \code{bnec_plan_is_parallel()} reads only the first strategy.
#'
#' \code{concurrent} is therefore not \code{dispatch}: the levels go through
#' \pkg{future} in both cases, and only in the first can two of them be fitted
#' at one time. It is what decides whether the compile cache has to be
#' separated and whether a per-level message can still be printed in order.
#'
#' \bold{The levels are the last of the three claims on a plan.} A core given to
#' the chains of one fit adds no export and no second fit in memory; a worker
#' fitting one model holds one fit; a worker fitting a level holds that level's
#' whole model-averaged set. That ordering is why the comparison above is made
#' rather than assumed: on the four cores the grouped call of #338 was measured
#' over, with eleven equations and seven levels, the model arrangement is the
#' smaller count (\code{7 x ceiling(11 / 4) = 21} against
#' \code{ceiling(7 / 4) x 11 = 22}) and the levels are left in sequence. The
#' level loop takes the workers only from about eight of them upwards.
#'
#' The decision is reported for the reason \code{plan_model_set()} gives: a
#' parallel run emits its per-level messages out of order or not at all, so the
#' user otherwise has no way to tell which path was taken.
#'
#' @param n_levels A \code{\link[base]{numeric}} vector of length 1 giving the
#' number of levels to be fitted.
#' @param n_models A \code{\link[base]{numeric}} vector of length 1 giving the
#' number of equations the formula asks for, or \code{NA} where it could not be
#' read.
#'
#' @return A \code{\link[base]{list}} with \code{dispatch} and \code{concurrent},
#' each a \code{\link[base]{logical}} vector of length 1.
#'
#' @noRd
plan_group_levels <- function(n_levels, n_models) {
  none <- list(dispatch = FALSE, concurrent = FALSE)
  if (n_levels < 2 ||
        !requireNamespace("future", quietly = TRUE) ||
        !requireNamespace("future.apply", quietly = TRUE)) {
    return(none)
  }
  strategies <- try(future::plan("list"), silent = TRUE)
  if (inherits(strategies, "try-error")) {
    return(none)
  }
  nested <- length(strategies) > 1
  if (!nested && !bnec_plan_is_parallel()) {
    return(none)
  }
  # A list whose remaining strategies are all sequential promises nothing, so it
  # is not read as a request: dispatching the levels through future would cost
  # a round trip to reach the lapply they would have reached anyway, and the
  # message below would name a strategy that does no more than the first.
  nested_inner <- nested &&
    any(!vapply(strategies[-1], inherits, logical(1), "sequential"))
  if (nested && inherits(strategies[[1]], "sequential")) {
    if (!nested_inner) {
      return(none)
    }
    message(
      "The plan is a list whose first strategy is sequential, so the ",
      n_levels, " levels are fitted one at a time and each level's model set",
      " uses the next strategy in the list. This is the arrangement earlier",
      " versions took, asked for explicitly."
    )
    return(list(dispatch = TRUE, concurrent = FALSE))
  }
  workers <- try(future::nbrOfWorkers(), silent = TRUE)
  if (inherits(workers, "try-error") || !is.numeric(workers) ||
        length(workers) != 1) {
    workers <- NA_integer_
  }
  rounds <- group_loop_rounds(n_levels, n_models, workers)
  # A one-worker plan is parallel in name only, and the same case
  # plan_model_set() reports separately: plan(multicore) resolves this way
  # wherever forking is unavailable. Counting it gives equal rounds on both
  # arrangements, so the tie already leaves the levels in sequence, and the
  # message below says which.
  concurrent <- nested || isTRUE(rounds$levels < rounds$models)
  if (!concurrent) {
    message(
      if (is.na(rounds$levels)) {
        paste0(
          "Fitting ", n_levels, " levels one at a time. ",
          if (is.na(n_models)) {
            paste0("The equations the formula asks for could not be read")
          } else {
            paste0("The number of workers the plan resolves to could not be",
                   " read")
          },
          ", so the two arrangements cannot be counted against each other and",
          " the levels are left in sequence. Nest the plan to fit them in",
          " parallel."
        )
      } else {
        paste0(
          "Fitting ", n_levels, " levels one at a time, with the equations of",
          " each level over ", n_workers(workers), ": ", rounds$models,
          " rounds of one fit, against ", rounds$levels,
          " with the levels in parallel instead, counted over the ", n_models,
          " equations the formula asks for before any is ruled out.\nTo divide",
          " the workers between the two loops, nest the plan, for example",
          " plan(list(tweak(multisession, workers = 2),",
          " tweak(multisession, workers = I(2))))."
        )
      }
    )
    return(none)
  }
  held <- if (is.na(workers) || is.infinite(workers)) {
    n_levels
  } else {
    min(workers, n_levels)
  }
  message(
    "Fitting ", n_levels, " levels in parallel over ",
    if (is.na(workers) || is.infinite(workers)) {
      "the plan's workers"
    } else {
      n_workers(workers)
    },
    ", under the future plan already set.\n",
    if (nested) {
      paste0("The plan is a list, so each level fits its own model set under",
             " the next strategy in it.")
    } else {
      paste0("Each level fits its model set one model at a time: the plan",
             " inside a worker is sequential unless the plan is a list.")
    },
    "\nPer-level messages from a worker may arrive out of order, or not at",
    " all. A level that fails ends the call either way.",
    "\nEach worker writes its Stan programs to its own directory, so on a cold",
    " cache every equation is compiled once per worker rather than once.",
    "\nUp to ", held, " fitted model sets are held at once, against one when",
    " the levels are fitted in sequence."
  )
  list(dispatch = TRUE, concurrent = TRUE)
}

#' Apply a function over a set of fits, in parallel where asked
#'
#' Serves both of the loops that fit more than one thing: the model set inside
#' \code{\link{bnec}} and \code{amend()}, and the level loop inside
#' \code{\link{bnec_group}}. Everything recorded below is a property of the
#' dispatch rather than of what is being dispatched, and holds for either.
#'
#' \code{\link[base]{lapply}} when \code{parallel} is \code{FALSE}, which is
#' every call made today. That path is what the acceptance criterion
#' "sequential behaviour is identical to current output for the same seed"
#' rests on, and it touches neither \pkg{future} nor the RNG.
#'
#' \bold{The RNG kind is restored inside the worker}, and this is the part that
#' fails silently if it is left out. \code{future.seed = TRUE} installs an
#' L'Ecuyer-CMRG stream in each worker so that draws are parallel-safe. Where
#' a \code{seed} was supplied, bayesnec then seeds its own initial-value search
#' with \code{set.seed(brm_args$seed)} in \code{make_good_inits()}, and
#' \code{\link[base]{set.seed}} called with \code{kind = NULL} -- the default --
#' leaves the current generator kind in place. The same seed therefore draws
#' from L'Ecuyer-CMRG in a worker and from the session's kind, normally
#' Mersenne-Twister, in the parent, and the two give different initial values.
#' Nothing errors and nothing warns: the fits simply differ from the sequential
#' run. Restoring the parent's kind first makes the seed mean the same thing in
#' both places. \code{weighted_draw_index()} records the same trap on the
#' post-fit side.
#'
#' \bold{The caller's RNG stream is left where it was found.} A parallel loop
#' advances the parent's \code{.Random.seed} by generating the per-element
#' streams, and initialises it from entropy where the session had not yet used
#' the RNG, so what \code{expand_manec()} draws next -- \code{w_draw_seed},
#' and through it which draws each equation contributes to the model-averaged
#' estimate -- would otherwise differ between two parallel runs of the same
#' call. Restoring makes that draw answer to a \code{set.seed()} in the
#' caller's session, which is what \code{expand_manec()} says it is for, and
#' stops a fit resetting a user's simulation seed. The restore itself is
#' \code{with_preserved_rng_state()}, which \code{weighted_draw_index()},
#' \code{check_fit_table()}, \code{check_fit_combined()} and
#' \code{dispersion()} also use; it puts the kind back first, because the
#' generator is encoded in \code{.Random.seed[1]}.
#'
#' It does not make the model-averaging draw match the sequential run's. Run in
#' sequence the loop advances the parent's stream, because every model's
#' initial-value search --- and, where a \code{seed} was supplied, its
#' \code{set.seed(brm_args$seed)} --- happens there; run in parallel they
#' happen in a worker and nothing can replay them.
#'
#' Whether the \emph{fits} match a sequential run is a question of the seed,
#' and #310 did not change the answer. Where a \code{seed} is supplied they
#' match exactly, because \code{make_good_inits()} seeds itself with it
#' wherever it runs; \code{w_draw_seed} and \code{w_draw_index} still do not.
#' Where none is supplied they do not match either, because the search draws
#' from the stream it is handed and a worker's is not the parent's. What #310
#' changed is that each of the two runs now repeats itself, where before
#' neither did. Measured on R 4.6.1 under \code{plan(multicore, workers = 3)},
#' three equations, the body being \code{add_brm_defaults()} and so the search
#' itself, \code{set.seed(777)} before each run, against the released code and
#' against this one: sequential agreed with parallel under a seed and not
#' without one, on both, while two runs of either kind agreed only here. So
#' \code{seed} is what a user comparing the two needs, and
#' \code{\link{bnec}} says so.
#'
#' Closing the \code{w_draw_seed} gap means deriving the draw from
#' \code{brm_args$seed} rather than from the ambient stream, which is a change
#' to \code{expand_manec()} and to what \#216 decided deliberately.
#'
#' \code{future.seed = TRUE} is kept rather than dropped to \code{NULL}.
#' \code{NULL} leaves the
#' worker's RNG state to the backend, which makes correctness a property of
#' every code path inside \code{fit_bayesnec()} seeding itself rather than of
#' the dispatcher, and that is not something anyone maintains. \code{TRUE}
#' gives each element a well-defined independent stream whatever the body does.
#' (\code{future.seed = FALSE}, the \code{future_lapply} default, is not an
#' option at all: it reports \code{UNRELIABLE VALUE} for a body that uses the
#' RNG, which every fit does. \code{NULL} is silent.)
#'
#' Restoring the kind does not leave the workers drawing in step, and the
#' reason recorded here before #310 was wrong. It said
#' \code{\link[base]{RNGkind}} re-initialises \code{.Random.seed} from the
#' clock and the process id. It does so only where no seed exists yet, which is
#' what \code{?Random} documents and what makes \code{set.seed(NULL)} the
#' cause of #310; where a seed is already present, which inside a
#' \code{future.seed = TRUE} worker it always is, the new state is derived
#' from the current one. Measured on R 4.6.1 over repeated calls in one process
#' and again in a second process, on both arms: \code{set.seed(42)} then
#' \code{RNGkind()} at the kind already in force gives a first draw of
#' 0.8311705 every time, and \code{set.seed(42, kind = "L'Ecuyer-CMRG")} then
#' \code{RNGkind()} back to Mersenne-Twister --- the change a worker makes ---
#' gives 0.2046757 every time. What makes the
#' workers differ is therefore the per-element L'Ecuyer-CMRG stream
#' \code{future.seed = TRUE} installs, not the clock. The 2026-09-11
#' measurement under \code{plan(multicore, workers = 3)} --- three of three
#' draws distinct, with the restore in place as without it --- holds either
#' way and so did not distinguish them.
#'
#' \bold{A run under a plan reproduces under the caller's seed.} That follows:
#' each element's stream is derived from the parent's, the restore is
#' deterministic, and since #310 a search given no seed draws from the stream
#' it is handed. Measured on R 4.6.1 under \code{plan(multicore, workers = 3)},
#' three equations, the body being \code{add_brm_defaults()} and so the search
#' itself, with no \code{seed} supplied: two runs at one
#' \code{\link[base]{set.seed}} gave identical initial values and a third at
#' another seed gave different ones. One backend and one R version, so it is
#' a measurement rather than a guarantee, and \code{seed} remains the way to
#' fix a run that has to repeat across either.
#'
#' What a plan still does not reproduce is the \emph{sequential} run's
#' model-averaging draw, for the reason given above under
#' \code{w_draw_seed}: that draw is made in the parent from a stream the two
#' runs advance differently.
#'
#' \bold{One element per chunk.} \code{future_lapply()} otherwise divides the
#' set into one chunk per worker and runs each chunk in sequence, which for
#' bayesnec is wrong twice over. The 23 equations differ in fitting time by an
#' order of magnitude, so a worker that draws the slow ones sets the wall clock
#' while the others idle; and a worker holds every fit in its chunk until the
#' chunk ends, which is the memory multiplication \#184 warns about in its
#' worst form. One element per future gives the assignment dynamically and
#' holds one fit at a time.
#'
#' The trade is that the data, the formula and the priors are sent once per
#' model rather than once per worker, which for 23 models over four workers is
#' about six times the transfer. \code{narrow_environment()} does not offset
#' that -- those are the objects a fit genuinely needs -- it removes everything
#' else. Load balancing and holding one fit rather than six are judged the
#' larger effects on a set whose equations differ in fitting time by an order
#' of magnitude, but no timing has been taken either way.
#'
#' @param X A \code{\link[base]{vector}} to apply over.
#' @param FUN A \code{\link[base]{function}} taking one element of \code{X}.
#' @param parallel A \code{\link[base]{logical}} vector of length 1, as
#' returned by \code{plan_model_set()}.
#'
#' @return A \code{\link[base]{list}} of the same length as \code{X}.
#'
#' @noRd
bnec_parallel_lapply <- function(X, FUN, parallel = FALSE) {
  if (!parallel) {
    return(lapply(X, FUN))
  }
  rng_kind <- RNGkind()
  with_preserved_rng_state(future.apply::future_lapply(
    X,
    function(x) {
      # suppressWarnings for the sample.kind = "Rounding" notice, which a
      # session set to the pre-3.6.0 sampler would otherwise have relayed once
      # per model. with_preserved_rng_state() suppresses it at its own restore.
      suppressWarnings(do.call(RNGkind, as.list(rng_kind)))
      FUN(x)
    },
    future.seed = TRUE,
    future.chunk.size = 1
  ))
}

#' Is this environment serialised by reference rather than by value?
#'
#' \code{\link[base]{serialize}} writes the global environment, the base
#' environment, the empty environment and any package namespace or attached
#' package environment as a reference, so a formula holding one of those adds
#' nothing to the serialised size. Every other environment is written out in
#' full, together with everything bound in it.
#'
#' Tested on the name rather than on identity with each of them in turn:
#' \code{\link[base]{environmentName}} returns a non-empty string for exactly
#' the environments R serialises by reference and the empty string for an
#' ordinary local one.
#'
#' @param env An \code{\link[base]{environment}}.
#'
#' @return A \code{\link[base]{logical}} vector of length 1.
#'
#' @noRd
env_by_reference <- function(env) {
  !is.environment(env) || nzchar(environmentName(env))
}

#' Rebuild a formula's environment to hold only the names it uses
#'
#' A formula records the environment it was created in, and
#' \code{\link[base]{serialize}} writes that environment out in full. Created at
#' the top level of a script it is the global environment and adds nothing;
#' created in a \pkg{knitr} chunk it is the chunk environment, which holds every
#' object the document has built so far. \code{narrow_environment()} does not
#' reach it: it replaces the environment of the applied \emph{function}, and the
#' formula inside travels with its own.
#'
#' Measured on R 4.6.1 with a 76 MiB vector bound beside the formula in the
#' environment that created it: the formula alone serialises to 76.29 MiB, the
#' narrowed closure that reads it to 76.30 MiB, and the model frame built from
#' it to 76.30 MiB, that last by way of the \code{.Environment} of its
#' \code{terms} attribute. So a fit sends the calling session to every worker,
#' and stores it in every saved fit. See #329.
#'
#' \bold{The environment is narrowed rather than removed.}
#' \code{\link[stats]{model.frame}} resolves a term against \code{data} first
#' and the formula's environment second, so a formula naming anything outside
#' \code{data} needs it --- a transformation the user wrote, and the model
#' argument of \code{crf()} where that is a variable rather than a string.
#' Since #319 that resolution is deliberate rather than incidental:
#' \code{formula_eval_env()} evaluates in a frame whose parent is
#' \code{formula_env()}. What is built here holds exactly the names the formula
#' mentions and \code{data} does not supply.
#'
#' \bold{The walk stops at the first environment R serialises by reference, and
#' the replacement is parented there.} A name bound in the global environment,
#' in an attached package or in a namespace is left where it is and resolved
#' through the parent chain, because copying it would reintroduce the size this
#' function exists to remove. Parenting the replacement to the environment the walk
#' stopped at rather than to the package namespace keeps the lookup chain the
#' user's formula had: a formula written in a frame whose parent is
#' \code{\link[base]{baseenv}} still resolves through \code{baseenv()} and
#' not through the \pkg{bayesnec} namespace, which is what
#' \code{?bayesnecformula}'s own test of \code{trials()} rests on.
#'
#' One consequence is unchanged rather than introduced: a worker's global
#' environment is not the caller's, so a name that only the calling session's
#' workspace supplies is already out of reach under a plan, with or without
#' this.
#'
#' \bold{Applied on the sequential path as well}, for the reason
#' \code{narrow_environment()} gives: a name left behind then fails on the first
#' ordinary call rather than only for whoever sets a plan. It is applied before
#' the model frame is built, so that the frame, the \pkg{brms} formula and the
#' stored fit are all narrowed by the one call.
#'
#' \bold{A function the formula names is the case this does nothing for.} A
#' transformation the user defined beside the formula is a closure over the same
#' environment, so the name is copied with everything that environment holds,
#' and the fit is the size it was: measured at 76.294 MiB before and after. The
#' saving is real only where every name the formula mentions is either a column
#' of \code{data} or small. A helper defined in a \pkg{knitr} chunk and used in
#' a formula is therefore still a large fit, and moving it to a package or to
#' the global environment is what makes it small.
#'
#' @param formula A \code{\link{bayesnecformula}}, or any formula.
#' @param data A \code{\link[base]{data.frame}}, whose names are the terms the
#' formula does not need its environment for.
#'
#' @return \code{formula}, with its environment replaced, or unchanged where
#' its environment is one R serialises by reference.
#'
#' @noRd
narrow_formula_environment <- function(formula, data) {
  env <- environment(formula)
  if (env_by_reference(env)) {
    return(formula)
  }
  wanted <- setdiff(all.names(formula), names(data))
  vals <- list()
  while (!env_by_reference(env)) {
    for (nm in setdiff(wanted, names(vals))) {
      if (exists(nm, envir = env, inherits = FALSE)) {
        # vals[nm] <- list(x), not vals[[nm]] <- x: the second deletes the
        # element when x is NULL, so a name bound to NULL would be left out of
        # the replacement and then resolve against the parent chain, where an
        # unrelated object of the same name would answer for it. A NULL model
        # set went from a refusal by get_model_from_formula() to a fit of
        # whatever the global environment happened to hold.
        vals[nm] <- list(get(nm, envir = env, inherits = FALSE))
      }
    }
    env <- parent.env(env)
  }
  # `env` is now the environment the walk stopped at, which is the first one R
  # sends by reference, and parenting there preserves the rest of the chain
  # exactly as the formula had it.
  environment(formula) <- list2env(vals, parent = env)
  formula
}

#' Rebuild a function so that only what it names travels to a worker
#'
#' \pkg{future} exports the applied function together with its enclosing
#' environment, and that environment is by default the frame of
#' \code{\link{bnec}} or \code{amend_model_set()}. Those frames hold
#' everything the function has computed by that point -- for \code{amend()},
#' every fit already in the set -- so a closure that reads a handful of small
#' objects serialises all of it, once per future. Measured on the two-equation
#' \code{manec_example}, 2026-09-11: the applied function reported 14.1 MiB
#' against 75 bytes for the arguments it actually reads, and the same closure
#' over a 32 MB object was refused outright at
#' \code{future.globals.maxSize = 10 MiB}, naming \code{FUN}, which tells the
#' user nothing about the cause.
#'
#' The replacement environment's parent is the package namespace, so package
#' internals resolve as before and are exported by reference rather than by
#' value.
#'
#' Applied on the sequential path as well as the parallel one, deliberately. A
#' name left out of \code{vars} then fails on the first ordinary call rather
#' than only for whoever sets a plan.
#'
#' @param fn A \code{\link[base]{function}}.
#' @param vars A named \code{\link[base]{list}} of everything \code{fn}
#' reads from its enclosing scope.
#'
#' @return \code{fn}, with its environment replaced.
#'
#' @noRd
narrow_environment <- function(fn, vars) {
  environment(fn) <- list2env(vars, parent = asNamespace("bayesnec"))
  fn
}
