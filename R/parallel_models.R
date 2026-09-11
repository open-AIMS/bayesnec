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
#' another, and because it is what a supplied \code{cores} then overrides. A
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
             n_models, " models are fitted one at a time and nothing is",
             " changed about how brms samples its chains. plan(multicore)",
             " resolves this way wherever forking is unavailable, which",
             " includes Windows and most IDEs.")
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

#' Apply a function over a model set, in parallel where asked
#'
#' \code{\link[base]{lapply}} when \code{parallel} is \code{FALSE}, which is
#' every call made today. That path is what the acceptance criterion
#' "sequential behaviour is identical to current output for the same seed"
#' rests on, and it touches neither \pkg{future} nor the RNG.
#'
#' \bold{The RNG kind is restored inside the worker}, and this is the part that
#' fails silently if it is left out. \code{future.seed = TRUE} installs an
#' L'Ecuyer-CMRG stream in each worker so that draws are parallel-safe.
#' bayesnec then seeds its own initial-value search with
#' \code{set.seed(brm_args$seed)} in \code{make_good_inits()}, and
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
#' stops a fit resetting a user's simulation seed. \code{weighted_draw_index()}
#' restores for the same reason and in the same order: the kind first, because
#' the generator is encoded in \code{.Random.seed[1]}.
#'
#' It does not make the model-averaging draw match the sequential run's. Run in
#' sequence the loop advances the parent's stream, because every model's
#' \code{set.seed(brm_args$seed)} and initial-value search happen there; run in
#' parallel they happen in a worker and nothing can replay them. The fitted
#' models reproduce exactly; \code{w_draw_seed} and \code{w_draw_index} do not.
#' Closing that gap means deriving the draw from \code{brm_args$seed} rather
#' than from the ambient stream, which is a change to \code{expand_manec()} and
#' to what \#216 decided deliberately.
#'
#' \code{future.seed = TRUE} is kept rather than dropped to \code{NULL}, even
#' though the stream it installs is then discarded. \code{NULL} leaves the
#' worker's RNG state to the backend, which makes correctness a property of
#' every code path inside \code{fit_bayesnec()} seeding itself rather than of
#' the dispatcher, and that is not something anyone maintains. \code{TRUE}
#' gives each element a well-defined independent stream whatever the body does.
#' (\code{future.seed = FALSE}, the \code{future_lapply} default, is not an
#' option at all: it reports \code{UNRELIABLE VALUE} for a body that uses the
#' RNG, which every fit does. \code{NULL} is silent.)
#'
#' Discarding the stream does not leave the workers drawing in step.
#' \code{\link[base]{RNGkind}} re-initialises \code{.Random.seed} from the
#' clock and the process id, so each worker starts somewhere different, and
#' bayesnec then seeds the search itself. Measured 2026-09-11 under
#' \code{plan(multicore, workers = 3)}: three of three draws distinct with the
#' restore in place, as without it.
#'
#' Where no \code{seed} is supplied the search calls \code{set.seed(NULL)},
#' which reseeds from entropy, so the run is not reproducible under either plan
#' -- which is what it does today, with or without a plan set.
#'
#' \bold{One model per chunk.} \code{future_lapply()} otherwise divides the set
#' into one chunk per worker and runs each chunk in sequence, which for
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
bnec_model_lapply <- function(X, FUN, parallel = FALSE) {
  if (!parallel) {
    return(lapply(X, FUN))
  }
  rng_kind <- RNGkind()
  has_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (has_seed) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    suppressWarnings(do.call(RNGkind, as.list(rng_kind)))
    if (is.null(old_seed)) {
      suppressWarnings(rm(".Random.seed", envir = globalenv()))
    } else {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)
  future.apply::future_lapply(
    X,
    function(x) {
      # suppressWarnings for the sample.kind = "Rounding" notice, which a
      # session set to the pre-3.6.0 sampler would otherwise have relayed once
      # per model. weighted_draw_index() suppresses it at its own restore.
      suppressWarnings(do.call(RNGkind, as.list(rng_kind)))
      FUN(x)
    },
    future.seed = TRUE,
    future.chunk.size = 1
  )
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
