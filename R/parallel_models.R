#' Is the user's \pkg{future} plan a parallel one?
#'
#' Reads the plan rather than an argument to \code{\link{bnec}}, which would
#' duplicate state the plan already holds. Tests the strategy, not
#' \code{future::nbrOfWorkers()}: a \code{cluster} plan on one remote node
#' reports one worker but is still a parallel plan.
#'
#' Returns \code{FALSE} where \pkg{future} or \pkg{future.apply} is absent, so
#' that the sequential path needs neither package.
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
#' Called once, before the model loop, by \code{\link{bnec}} and
#' \code{\link{amend}}. Returns the decision and the \code{\link[brms]{brm}}
#' arguments together, so that the two cannot disagree.
#'
#' Fewer than two models is never parallel: the \code{cores = 1} limit below
#' would only slow a lone fit.
#'
#' Under a parallel plan of more than one worker each model samples its chains
#' in sequence: \code{cores = 1} is passed unless the user supplied
#' \code{cores}, which asks for both levels of parallelism. \pkg{future}
#' already sets \code{mc.cores} to 1 inside a worker; passing \code{cores}
#' makes the request here rather than relying on that.
#'
#' The decision is reported because messages from a worker arrive out of order
#' or not at all, so the user could not otherwise tell which path was taken.
#'
#' @param brm_args A named \code{\link[base]{list}} of arguments for
#' \code{\link[brms]{brm}}.
#' @param n_models A \code{\link[base]{numeric}} vector of length 1 giving the
#' number of models that will actually be fitted.
#' @param caller A \code{\link[base]{character}} vector of length 1, either
#' \code{"bnec"} or \code{"amend"}. \code{amend()} takes no \pkg{brms}
#' arguments, so its message cannot suggest passing \code{cores}.
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
  # Decide cores = 1 on the worker count, not the strategy. plan(multicore)
  # resolves to one worker where forking is unavailable (Windows, RStudio,
  # Positron), and cores = 1 there would sample every chain of every model in
  # sequence. An unknown count is treated as more than one.
  one_worker <- isTRUE(workers == 1)
  supplied_cores <- "cores" %in% names(brm_args)
  if (!supplied_cores && !one_worker) {
    brm_args$cores <- 1
  }
  message(
    if (one_worker) {
      # No advice on which plan to use: a single-node cluster plan also
      # reports one worker, and there the fits do run in another process.
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
#' A grouped call has two loops that could use the workers: the levels, and
#' the models within a level. One plan of \emph{W} workers drives one of them.
#' This counts the rounds of one fit that each arrangement takes:
#' \code{ceiling(L / W) * M} for the levels and \code{L * ceiling(M / W)} for
#' the models.
#'
#' The count assumes every fit takes the same time. The equations differ by an
#' order of magnitude and a round lasts as long as its slowest fit, so the
#' count favours the level arrangement where \emph{M} exceeds \emph{W}. The
#' caller gives a tie to the models for that reason. \code{n_models} is the set
#' the formula asks for, an upper bound on what each level fits.
#'
#' A scheduler backend such as \pkg{future.batchtools} reports \code{Inf}
#' workers. The levels then take \emph{M} rounds and the models \emph{L}.
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
#' Parallel levels of a grouped call compile the same Stan programs at the same
#' time, and \pkg{cmdstanr} does not lock its compile cache, so two workers
#' would write the same file and executable at once. The model loop inside one
#' \code{\link{bnec}} call compiles different programs and is not exposed.
#'
#' The directory is keyed on host and process ID, because PIDs are unique only
#' within one host and cluster nodes can share \code{root}. Every backend needs
#' it, since a \code{root} the user has set is shared by every worker. A
#' persistent worker (\code{multisession}, \code{cluster}) reuses its
#' directory; under \code{multicore} each level is a new process and compiles
#' again. A cache the user has warmed is not read, because each worker writes
#' below its own subdirectory.
#'
#' \code{root} must be read in the parent and passed in: \pkg{future} does not
#' export options, so a \code{multisession} worker sees
#' \code{cmdstanr_write_stan_file_dir} unset.
#'
#' Only \pkg{cmdstanr} is handled. \pkg{rstan} caches only under
#' \code{rstan_options(auto_write = TRUE)}, which \code{?bnec_group} documents.
#'
#' @param root A \code{\link[base]{character}} vector of length 1 giving the
#' directory to write under, or \code{NULL} for \code{\link[base]{tempdir}}.
#' @param host The worker host name.
#' @param pid The worker process identifier.
#'
#' @return A \code{\link[base]{character}} vector of length 1, the directory,
#' which has been created.
#'
#' @noRd
worker_stan_cache_dir <- function(root = NULL,
                                  host = Sys.info()[["nodename"]],
                                  pid = Sys.getpid()) {
  base <- root
  if (is.null(base) || !is.character(base) || length(base) != 1 ||
        is.na(base)) {
    base <- tempdir()
  }
  # Keep only portable filename characters: the operating system supplies the
  # node name.
  host <- if (length(host) == 1 && !is.na(host) && nzchar(host)) {
    gsub("[^[:alnum:]_.-]", "_", host)
  } else {
    "unknown-host"
  }
  path <- file.path(base, paste0("bayesnec-stan-", host, "-", pid))
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  path
}

#' Evaluate an expression under a grouped call's realised level seed
#'
#' \code{expand_manec()} draws its model-averaging seed after the equation
#' loop, which advances R's stream differently under \code{lapply()} and under
#' \pkg{future}. Seeding the expansion with the level seed makes the weighted
#' posterior independent of the arrangement; the stream is restored afterwards.
#' A \code{NULL} seed evaluates \code{expr} unchanged, as a standalone
#' \code{\link{bnec}} call does.
#'
#' @param seed A realised seed, or \code{NULL}.
#' @param expr An expression to evaluate.
#'
#' @return The value of \code{expr}.
#'
#' @noRd
with_group_seed <- function(seed, expr) {
  if (is.null(seed)) {
    return(expr)
  }
  with_preserved_rng_state({
    suppressWarnings(set.seed(seed, kind = "Mersenne-Twister",
                              sample.kind = "Rejection"))
    expr
  })
}

#' One seed per level, realised in the caller's session
#'
#' \code{\link{bnec_group}} applies these inside each level, so that the
#' initial-value search and the model-averaging draw of \code{expand_manec()}
#' answer to the caller's session rather than to the process that fits the
#' level. Without them the estimates would depend on the number of workers.
#'
#' The seeds derive from \code{seed} where the caller supplied one, and from
#' the session's stream otherwise. The caller's stream and generator kind are
#' restored on exit: a fit must not reset a user's simulation seed.
#'
#' Where \code{seed} is supplied the generator and sampler are named
#' (\code{"Mersenne-Twister"}, \code{"Rejection"}), so that one \code{seed}
#' gives the same level seeds whatever \code{RNGkind()} the session has set.
#'
#' A level seed replaces the L'Ecuyer-CMRG substream that
#' \code{future.seed = TRUE} installs in a worker. That is safe only because a
#' level draws for nothing but the initial-value search and one
#' \code{sample.int()} in \code{expand_manec()}. Nothing else dispatched by
#' \code{bnec_parallel_lapply()} reseeds.
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
  # As in make_good_inits(), NULL and a single NA mean no seed. Pass every
  # other value to set.seed(), so that a malformed seed is refused rather than
  # ignored.
  if (!is.null(seed) && !(length(seed) == 1 && is.na(seed))) {
    suppressWarnings(set.seed(seed, kind = "Mersenne-Twister",
                              sample.kind = "Rejection"))
  }
  sample.int(.Machine$integer.max, n_levels)
}

#' Decide how the levels of a grouped call will be fitted, and say so
#'
#' Called once, before the level loop, by \code{\link{bnec_group}}. Like
#' \code{plan_model_set()}, it reads the decision from the plan.
#'
#' \pkg{future} evaluates a nested future sequentially unless the plan is a
#' list, so a plan of one strategy drives one loop: dispatching the levels
#' takes the workers from the model loop. \code{group_loop_rounds()} compares
#' the two arrangements, and a tie leaves the levels in sequence.
#'
#' A list plan is honoured without counting: its first strategy drives the
#' levels and its second the models. A list whose first strategy is
#' \code{sequential} still dispatches each level through \pkg{future}, because
#' entering a future is what hands the second strategy to the model loop; the
#' levels then run one at a time. \code{bnec_plan_is_parallel()} reads only
#' the first strategy, so it must not decide for a list plan.
#'
#' \code{concurrent} is therefore not \code{dispatch}. Only a concurrent
#' arrangement fits two levels at once, which decides whether the compile
#' cache must be separated and whether a per-level message prints in order.
#'
#' The decision is reported for the reason \code{plan_model_set()} gives.
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
  # Only the second strategy can drive the model loop, since bnec_group() has
  # two nested loops. Where the first and second are both sequential the list
  # asks for nothing, and the levels are not dispatched.
  nested_inner <- nested && !inherits(strategies[[2]], "sequential")
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
  # A one-worker plan gives equal rounds on both arrangements, so the tie
  # leaves the levels in sequence without a case of its own.
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
    if (nested && nested_inner) {
      paste0("The plan is a list, so each level fits its own model set under",
             " the next strategy in it.")
    } else if (nested) {
      paste0("Each level fits its model set one model at a time: the next",
             " strategy in the list is sequential.")
    } else {
      paste0("Each level fits its model set one model at a time: the plan",
             " inside a worker is sequential unless the plan is a list.")
    },
    "\nPer-level messages from a worker may arrive out of order, or not at",
    " all. A level that fails ends the call either way.",
    "\nEach active host-process pair writes its Stan programs to its own",
    " directory. On a cold cache this can compile every equation once per",
    " persistent worker, and once per level under a multicore plan.",
    "\nUp to ", held, " fitted model sets are held at once, against one when",
    " the levels are fitted in sequence."
  )
  list(dispatch = TRUE, concurrent = TRUE)
}

#' Apply a function over a set of fits, in parallel where asked
#'
#' Used by the model loop of \code{\link{bnec}} and \code{amend()} and by the
#' level loop of \code{\link{bnec_group}}. With \code{parallel = FALSE} it is
#' \code{\link[base]{lapply}} and touches neither \pkg{future} nor the random
#' number stream.
#'
#' The worker restores the caller's \code{RNGkind()} before \code{FUN} runs.
#' \code{future.seed = TRUE} installs L'Ecuyer-CMRG in each worker, and
#' \code{set.seed(seed)} in \code{make_good_inits()} keeps the current kind, so
#' without the restore one \code{seed} would give different initial values in
#' a worker and in the parent, with no error or warning.
#'
#' \code{with_preserved_rng_state()} restores the caller's stream after
#' \pkg{future} generates per-element streams. This leaves the following
#' model-averaging draw in \code{expand_manec()} to use the caller's stream at
#' entry. With a supplied \code{seed}, initial values match a sequential run.
#' Its model-averaging draw can still differ: the sequential loop advances
#' the parent's stream while fitting each model, but the parallel loop
#' makes those draws in workers.
#'
#' Keep \code{future.seed = TRUE}. \code{NULL} leaves each worker's stream to
#' the backend, and \code{FALSE} warns \code{UNRELIABLE VALUE} for any body
#' that draws random numbers, as every fit does.
#'
#' \code{future.chunk.size = 1} sends one element per future, so that workers
#' take the slow equations as they come and each holds one fit at a time. The
#' data, formula and priors are then sent once per element rather than once
#' per worker.
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
      # suppressWarnings() hides the notice a session on the "Rounding" sampler
      # would otherwise relay once per model.
      suppressWarnings(do.call(RNGkind, as.list(rng_kind)))
      FUN(x)
    },
    future.seed = TRUE,
    future.chunk.size = 1
  ))
}

#' Is this environment serialised by reference rather than by value?
#'
#' \code{\link[base]{serialize}} writes the global, base and empty
#' environments, namespaces and attached package environments as references,
#' and every other environment in full. Each case is tested explicitly:
#' \code{\link[base]{environmentName}} alone is not enough, since an arbitrary
#' environment attached under a name is serialised by value.
#'
#' @param env An \code{\link[base]{environment}}.
#'
#' @return A \code{\link[base]{logical}} vector of length 1.
#'
#' @noRd
env_by_reference <- function(env) {
  if (!is.environment(env)) {
    return(TRUE)
  }
  identical(env, globalenv()) || identical(env, baseenv()) ||
    identical(env, emptyenv()) || isNamespace(env) ||
    startsWith(environmentName(env), "package:")
}

#' Rebuild a formula's environment to hold only the names it uses
#'
#' A formula records the environment it was created in, and
#' \code{\link[base]{serialize}} writes that environment out in full. In a
#' \pkg{knitr} chunk it holds every object the document has built, so each fit
#' would send it to every worker and store it in every saved fit.
#' \code{narrow_environment()} does not reach it: the formula keeps its own
#' environment inside the narrowed function.
#'
#' The environment is narrowed, not removed. \code{\link[stats]{model.frame}}
#' and \code{formula_eval_env()} resolve a name missing from \code{data} in the
#' formula's environment, such as a user's transformation or a \code{crf()}
#' model argument held in a variable. The replacement holds the names the
#' formula mentions that \code{data} does not supply.
#'
#' The walk stops at the first environment R serialises by reference, and the
#' replacement is parented there, so the formula's lookup chain is unchanged:
#' a formula written in a frame whose parent is \code{\link[base]{baseenv}}
#' does not start resolving through the \pkg{bayesnec} namespace.
#'
#' It is applied on the sequential path too, so that a name left behind fails
#' on an ordinary call, and before the model frame is built, so that the frame,
#' the \pkg{brms} formula and the stored fit all hold the narrowed environment.
#'
#' A function the formula names is copied with the environment it closes over,
#' so a transformation defined in a \pkg{knitr} chunk still makes a large fit.
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
        # vals[nm] <- list(x), not vals[[nm]] <- x, which drops a NULL value
        # and lets an unrelated object of that name further up the chain
        # answer for it.
        vals[nm] <- list(get(nm, envir = env, inherits = FALSE))
      }
    }
    env <- parent.env(env)
  }
  # `env` is now the first environment R serialises by reference.
  environment(formula) <- list2env(vals, parent = env)
  formula
}

#' Rebuild a function so that only what it names travels to a worker
#'
#' \pkg{future} exports the applied function with its enclosing environment,
#' by default the frame of \code{\link{bnec}} or \code{amend_model_set()}. That
#' frame holds everything computed so far, for \code{amend()} every fit in the
#' set, and would be serialised once per future. A large frame can also exceed
#' \code{future.globals.maxSize}, with an error that names only \code{FUN}.
#'
#' The replacement's parent is the package namespace, so package internals
#' resolve as before and are exported by reference. It is applied on the
#' sequential path as well, so that a name missing from \code{vars} fails on
#' an ordinary call rather than only under a plan.
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
