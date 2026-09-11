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
#' \bold{Under a parallel plan each model samples its chains in sequence.}
#' \code{\link[brms]{brm}} already parallelises across chains, so fitting
#' models in parallel on top of that requests \code{workers x chains}
#' processes. bayesnec passes no \code{cores} argument of its own, so
#' \pkg{brms} falls back to \code{getOption("mc.cores")}; a user who set that
#' in their profile would get four chains per worker without having asked for
#' them, and a four-worker plan with the default \code{chains = 4} would then
#' require sixteen processes on a machine that may not have them. A
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
  if (inherits(workers, "try-error")) {
    workers <- NA_integer_
  }
  supplied_cores <- "cores" %in% names(brm_args)
  if (!supplied_cores) {
    brm_args$cores <- 1
  }
  message(
    "Fitting ", n_models, " models in parallel over ", workers,
    " workers, under the future plan already set.\n",
    if (supplied_cores) {
      paste0("Each model samples its chains over cores = ", brm_args$cores,
             ", as you supplied, so up to ", workers, " x ", brm_args$cores,
             " processes may run at once.")
    } else {
      paste0("Each model samples its chains in sequence (cores = 1);",
             if (identical(caller, "amend")) {
               paste0(" amend() takes no brms arguments, so to nest the two",
                      " levels of parallelism refit the set with bnec().")
             } else {
               " pass `cores` to bnec() to nest the two levels of parallelism."
             })
    },
    "\nPer-model messages from a worker may arrive out of order, or not at",
    " all. A model that fails is recorded either way; see ?failed_models."
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
  future.apply::future_lapply(
    X,
    function(x) {
      do.call(RNGkind, as.list(rng_kind))
      FUN(x)
    },
    future.seed = TRUE
  )
}
