#' Fit a concentration-response model separately at each level of a factor
#'
#' Fits the model set independently within each level of a grouping factor,
#' model-averaging within each level, and returns a
#' \code{\link{bayesnecgroupfit}}. Crossed model weights across levels are then
#' available from the per-level weights alone.
#'
#' @param formula A \code{\link{bayesnecformula}}, as passed to
#' \code{\link{bnec}}.
#' @param data A \code{\link[base]{data.frame}} containing the data.
#' @param group_var A \code{\link[base]{character}} string naming the column of
#' \code{data} that carries the factor.
#' @param family A \code{\link[stats]{family}} function, string, or \code{NULL}.
#' If \code{NULL} it is chosen once from the \emph{whole} response and passed
#' down --- see Details.
#' @param predictor_scale The predictor-scale declaration passed to
#' \code{\link{bnec}} for every level. It is checked against the whole predictor
#' before any level is fitted.
#' @param ... Further arguments passed to \code{\link{bnec}} for every level.
#'
#' @details \bold{The basis for fitting the levels separately}
#'
#' Factor levels partition the data disjointly and share no parameters, so the
#' log likelihood is a sum over levels and the expected log predictive density
#' is additive. Under pseudo-BMA --- the package default --- the crossed model
#' weights are then exactly the outer product of the per-level weight vectors,
#' which is the same identity \code{\link{crossed_weights}} rests on for the two
#' blocks of a hurdle fit. See \code{\link{crossed_group_weights}}.
#'
#' \bold{The family is chosen once}
#'
#' The family chosen from each subset separately could differ between
#' levels, which would make the levels incomparable ---
#' their \code{elpd} contributions would not be on the same scale and the
#' crossed weights would be meaningless. So it is chosen from the whole response
#' and passed down.
#'
#' \bold{Dispersion per level}
#'
#' Separate fits give each level its own \code{sigma}, \code{shape} or
#' \code{phi}, deliberately: a shared dispersion parameter would break the
#' factorisation the crossed weights depend on. A model with structure spanning
#' levels --- shared dispersion, a group-level effect crossing levels, an
#' explicit contrast --- is a different model, and is not what this function
#' fits. Neither does it refit the favoured combination jointly.
#'
#' \bold{Fitting the levels in parallel}
#'
#' A grouped call has two loops that could use a \pkg{future} plan: the levels,
#' and the models within a level. One plan drives one of them, because
#' \pkg{future} evaluates a nested future sequentially unless the plan is a
#' list, so \code{bnec_group()} decides which and says which.
#'
#' The two arrangements are counted in rounds of one fit and the smaller is
#' taken, a tie going to the models. Writing \emph{L} for the levels, \emph{M}
#' for the equations the formula asks for and \emph{W} for the workers, the
#' levels take \code{ceiling(L / W) * M} rounds and the models
#' \code{L * ceiling(M / W)}. Seven levels of eleven equations over four workers
#' is 22 against 21, so the levels stay in sequence; over eight workers it is 11
#' against 14, so they do not. A nested plan ---
#' \code{plan(list(tweak(multisession, workers = 2), tweak(multisession,
#' workers = I(4))))} --- drives both loops and is honoured without being
#' counted, and \code{plan(list(sequential, ...))} asks for the levels in
#' sequence explicitly.
#'
#' No argument turns this on or off: the plan already holds that state, as it
#' does for \code{\link{bnec}}.
#'
#' The worked treatment --- why the inner worker count needs \code{I()}, what a
#' parallel grouped call does to compilation and to memory, and the warning a
#' nested plan emits --- is in \code{vignette("example2")} under \emph{The
#' levels of a grouped call}.
#'
#' \bold{Reproducing a grouped fit under a plan}
#'
#' Each level is given its own seed, realised in the calling session before the
#' levels are dispatched and derived from \code{seed} where one was supplied. The
#' realised seed is passed to every equation fitted for that level and is
#' reapplied when the model-averaging draw is realised. A grouped call therefore
#' gives the same estimates whichever arrangement it took, and the worker count
#' does not change an answer. With a \code{seed} it repeats with no
#' \code{\link[base]{set.seed}} in the session; without one,
#' \code{\link[base]{set.seed}} before the call fixes it.
#'
#' What was realised is kept: the returned object records \code{level_seeds},
#' one per level in the order of \code{levels}. A regenerated seed cannot be
#' relied on to be the same one years later, since \code{sample.int()}'s
#' algorithm has changed before, and these objects are archived and reopened, so
#' the realisation is stored rather than described. \code{\link{bnec}} describes
#' the rest of what a plan does to a fit, and none of that changes here.
#'
#' @return An object of class \code{\link{bayesnecgroupfit}}.
#'
#' @seealso \code{\link{bnec}}, \code{\link{crossed_group_weights}},
#' \code{\link{bayesnecgroupfit}}
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' data(nec_data)
#' nec_data$site <- rep(c("a", "b"), length.out = nrow(nec_data))
#' fit <- bnec_group(y ~ crf(x, "nec3param"), data = nec_data,
#'                   group_var = "site")
#' nec(fit)
#' crossed_group_weights(fit)
#' }
#'
#' @export
bnec_group <- function(formula, data, group_var, family = NULL,
                       predictor_scale = "auto", ...) {
  # Captured before anything can rebind it; see family_link_source() and #256.
  link_source <- family_link_source(substitute(family), env = parent.frame())
  predictor_scale <- validate_predictor_scale(predictor_scale)
  if (!is.character(group_var) || length(group_var) != 1) {
    stop("`group_var` must be a single column name.", call. = FALSE)
  }
  if (!group_var %in% names(data)) {
    stop("`group_var` \"", group_var, "\" is not a column of `data`.",
         call. = FALSE)
  }
  # Coerced up front, as bnec() does. Without it `model.frame(formula, data)`
  # below dispatches to stats::model.frame, which evaluates crf(x, "nec3param")
  # as an ordinary call returning a length-1 string -- and reports "variable
  # lengths differ", which points nowhere near the actual cause.
  formula <- bayesnecformula(formula, env = parent.frame())
  # Narrowed before anything is built from it, as bnec() does. A grouped call
  # exports the formula once per level as well as once per model, so the
  # environment it would otherwise hold is sent L x M times. See #329.
  formula <- narrow_formula_environment(formula, data)
  grp <- data[[group_var]]
  if (is.numeric(grp)) {
    stop("The grouping column \"", group_var, "\" is numeric. A factor",
         " covariate must be a character or a factor, so that its levels are",
         " unambiguous; a numeric column is almost always a predictor that",
         " belongs in crf() instead.", call. = FALSE)
  }
  # Refused rather than dropped. factor() removes NA from levels() and table()
  # ignores it, so an NA group would pass every check below -- but
  # data[grp == lev, ] is logical indexing with NA present, which puts an
  # all-NA row into *every* level's subset. model.frame() would then quietly
  # absorb them, so the fit would proceed and nothing would say why the level
  # sizes reported here did not match what was fitted.
  if (anyNA(grp)) {
    stop("The grouping column \"", group_var, "\" has ", sum(is.na(grp)),
         " missing value(s). Every observation must belong to a known level,",
         " because each level is fitted as a separate model; drop or impute",
         " those rows before calling bnec_group.", call. = FALSE)
  }
  grp <- factor(grp)
  levs <- levels(grp)
  if (length(levs) < 2) {
    stop("The grouping column \"", group_var, "\" has ", length(levs),
         " level; there is nothing to compare. Use bnec() directly.",
         call. = FALSE)
  }
  counts <- table(grp)
  if (any(counts < 4)) {
    thin <- names(counts)[counts < 4]
    stop("Level(s) ", paste0("\"", thin, "\"", collapse = ", "),
         " have fewer than 4 observations. Each level is fitted as a complete",
         " concentration-response model in its own right, so it needs enough",
         " data to support one.", call. = FALSE)
  }
  # Refused before the loop, not left to the bnec() call for the level that
  # holds it. Each level is a complete fit, so a missing value in level k would
  # otherwise be reached only after levels 1 to k-1 had sampled -- measured on
  # two levels of twelve, level "a" compiled and sampled to completion before
  # level "b" raised. The whole data frame is checked here, so the rows are
  # named as the user recorded them rather than by their position within a
  # subset. See #278.
  mod_dat <- model.frame(formula, data = data)
  check_complete_cases(mod_dat)
  validate_predictor_scale(
    predictor_scale, retrieve_var(mod_dat, "x_var", error = TRUE)
  )
  # Before the loop for the same reason as the line above it: bnec_group()
  # fits each level with bnec() in sequence, so a refusal reached at level k
  # arrives only after levels 1 to k-1 have compiled and sampled. See #271.
  check_disp_finite(formula, data)
  check_reserved_names(data)
  # The response substitutions are not reported here. bnec_group() fits each
  # level on its own subset, so the values substituted differ between levels
  # and the per-level bnec() call is where the report belongs. See #93.
  # Chosen once, from the whole response, for the reason in Details.
  if (is.null(family)) {
    y <- retrieve_var(mod_dat, "y_var", error = TRUE)
    tr <- retrieve_var(mod_dat, "trials_var")
    family <- set_distribution(y, support_integer = TRUE, trials = tr)
    message("Family chosen once from the whole response: ", family,
            ". Pass `family` to override.")
  }
  family <- validate_family(family, link_source = link_source)
  # The crossed weights are an outer product of the per-level weight vectors,
  # and that identity holds for pseudo-BMA only, so the method is checked in
  # crossed_group_weights() rather than merely documented -- multiplying
  # stacking weights gives a wrong crossed table with nothing to signal it.
  #
  # Captured here as well as read off the fits, because neither route is
  # reliable alone. expand_manec() does record the method, as
  # attr(mod_stats$wi, "method") (R/expand_classes.R), but that attribute
  # survives cbind and is dropped by row-subsetting, so it is present on a
  # fresh bayesmanecfit and absent after any reordering -- and a level that
  # fitted a single model is a bayesnecfit with no mod_stats at all. The
  # request is therefore recorded here, and crossed_group_weights() prefers
  # whatever the fits themselves still carry, since that is what actually
  # happened. See #33.
  dots <- list(...)
  wt_method <- if (!is.null(dots$loo_controls$weights$method)) {
    dots$loo_controls$weights$method
  } else {
    "pseudobma"
  }
  # The set the formula asks for, which is what plan_group_levels() counts the
  # two arrangements over. Read here rather than taken from an argument because
  # bnec() itself reads it from the formula; check_models() may then drop an
  # equation per level, so this is an upper bound. A formula this cannot be read
  # from is one bnec() is about to refuse, so the failure is left to arrive from
  # there and the levels stay in sequence meanwhile.
  models <- try(get_model_from_formula(formula), silent = TRUE)
  n_models <- if (inherits(models, "try-error")) NA_integer_ else length(models)
  level_plan <- plan_group_levels(length(levs), n_models)
  # Read in the parent and passed into the worker. future exports globals and
  # not the session's options, so a multisession worker starts with this one
  # unset; read inside the worker instead, a deliberate persistent cache would
  # be honoured under a forking plan and silently ignored under every other.
  cache_root <- getOption("cmdstanr_write_stan_file_dir")
  # One seed per level, realised here and applied to that level's model fits and
  # model-averaging draw. Without both uses the arrangement decides the answer:
  # the sequential and parallel model loops advance different streams before
  # expand_manec() draws its seed. Since plan_group_levels() reads the
  # arrangement from the worker count, one script at one seed would otherwise
  # report different model-averaged estimates on different machines. See #216
  # and the private .bayesnec_group_seed consumed by bnec().
  #
  # Derived from `seed` where the user supplied one, so that a grouped call
  # repeats without a set.seed() in the session as well. The caller's stream is
  # put back either way: bnec_group() is not entitled to move it, which is the
  # same rule bnec_parallel_lapply() follows.
  level_seeds <- group_level_seeds(length(levs), dots[["seed"]])
  if (level_plan$concurrent) {
    # Announced together, before the dispatch. The per-level message the
    # sequential path emits is dropped here: it would be emitted by the worker
    # that fits the level, and from there it arrives out of order or not at
    # all, so the sizes are reported once from the parent instead.
    message("Levels: ",
            paste0("\"", levs, "\" (", counts[levs], " observations)",
                   collapse = ", "), ".")
  }
  # The subsets are the elements dispatched, not indices into `data`. future
  # serialises the globals of the applied function once per future, so holding
  # the whole data frame there would send all of it to every level -- L times
  # what the levels between them need, and the transfer #329 exists to remove
  # reintroduced in the loop #338 adds. Measured on a 200,000-row frame over
  # seven levels: 5.53 MiB per future against 5.53 MiB for the seven subsets
  # together.
  #
  # split() rather than data[grp == lev, ]: it is one pass, and it orders the
  # pieces by levels(grp), which is the order `levs` is in. An empty level
  # cannot arise -- one under four observations was refused above.
  parts <- Map(
    function(i, d) list(i = i, data = d),
    seq_along(levs), unname(split(data, grp))
  )
  concurrent <- level_plan$concurrent
  # narrow_environment(), for the reason bnec() gives at its own call: future
  # exports the applied function with its enclosing environment, and that would
  # be this frame. Only the seven names below need to reach a worker, and neither
  # `data` nor `grp` is one of them.
  #
  # do.call() rather than forwarding `...`, because `...` cannot be put in the
  # replacement environment. Two things this changes were checked rather than
  # assumed. The formula reaches bnec() with its own environment intact, which
  # #319 needs for crf() to resolve a symbol, measured on R 4.6.1 against an
  # environment set by hand. And the family arrives as a value rather than as
  # the symbol it was, which family_link_source() already treats as "symbol" --
  # R/validate_family.R names do.call() as the case -- so the link is read the
  # same way on both paths. The family has in any event been validated and
  # marked above, so validate_family() leaves it alone downstream.
  #
  # "bnec" by name rather than the function object: do.call() records the value
  # it is given as the head of the call, so passing the object left every error
  # raised inside a level reading `Error in (function (formula, data, ...`.
  fit_level <- narrow_environment(
    function(part) {
      i <- part$i
      set.seed(level_seeds[i])
      if (concurrent) {
        # Set inside the worker, where it is the worker's own option, and
        # restored so that a plan evaluating in the parent leaves nothing
        # behind. See worker_stan_cache_dir().
        old <- options(
          cmdstanr_write_stan_file_dir = worker_stan_cache_dir(cache_root)
        )
        on.exit(options(old), add = TRUE)
      }
      # Every random part of one level answers to the same realised level seed.
      # Passing it to brms fixes model initialisation and sampling when the
      # model loop changes arrangement. The private companion is removed by
      # bnec() before brms sees it and fixes expand_manec()'s weighted draw,
      # which otherwise follows the different streams advanced by sequential
      # and parallel model loops.
      level_dots <- dots
      level_dots[["seed"]] <- level_seeds[i]
      level_dots[[".bayesnec_group_seed"]] <- level_seeds[i]
      do.call("bnec", c(list(formula, data = part$data, family = family,
                             predictor_scale = predictor_scale),
                        level_dots))
    },
    list(formula = formula, family = family, dots = dots,
         predictor_scale = predictor_scale,
         concurrent = concurrent, cache_root = cache_root,
         level_seeds = level_seeds)
  )
  # The caller's stream is restored around the whole loop, not only inside
  # bnec_parallel_lapply()'s parallel path. fit_level() calls set.seed() on
  # every path, so without this a grouped call fitted with the levels in
  # sequence left the session at the last level's seed -- and, where `seed` was
  # supplied, at a constant, so `set.seed(i); bnec_group(..., seed = 1);
  # rnorm(1)` returned one number for every i. The state after the call would
  # then depend on the arrangement, which is what level_seeds exists to stop.
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
  fits <- if (level_plan$concurrent) {
    bnec_parallel_lapply(parts, fit_level, parallel = TRUE)
  } else {
    # One level per dispatch, announced before it starts. future_lapply()
    # creates every future and then collects, and a sequential future relays
    # its conditions at collection, so dispatching all the levels at once put
    # every per-level message at the end of the run -- measured at 1.5 s a
    # level, all three messages arrived at 3.0 s. On a seven-level call at
    # eighteen minutes a fit that is the only progress signal there is.
    # Dispatching one at a time still enters a future, which is what moves the
    # plan on to the inner strategy, so the model loop inside each level still
    # gets the whole of it.
    lapply(parts, function(part) {
      message("Fitting level \"", levs[part$i], "\" (",
              counts[[levs[part$i]]], " observations).")
      if (level_plan$dispatch) {
        bnec_parallel_lapply(list(part), fit_level, parallel = TRUE)[[1]]
      } else {
        fit_level(part)
      }
    })
  }
  names(fits) <- levs
  out <- list(fits = fits, group_var = group_var, levels = levs,
              formula = formula, data = data, family = unmark_family(family),
              n = as.integer(counts[levs]), weights_method = wt_method,
              # Recorded rather than regenerated, for the reason
              # expand_manec() stores w_draw_index beside w_draw_seed: these
              # objects are archived and reopened years later, and what was
              # realised cannot drift the way a fresh sample.int() can.
              level_seeds = level_seeds)
  allot_class(out, c("bayesnecgroupfit", "bnecfit"))
}

#' The weighting method a fit was built with
#'
#' \code{expand_manec()} stamps it on the weight vector. The attribute does not
#' survive row-subsetting of \code{mod_stats}, and a single-model fit is a
#' bayesnecfit with no \code{mod_stats}, so an absent attribute means "unknown"
#' rather than "pseudo-BMA" and contributes nothing.
#'
#' Read per level by \code{crossed_group_weights()}, and on the whole object by
#' \code{\link{amend}}, \code{\link{pull_out}}, \code{\link[base]{c}} and
#' \code{update()}, each of which preserves the method of the set it operates
#' on. See #320.
#'
#' @param x A fitted object, or one level of a
#' \code{\link{bayesnecgroupfit}}.
#'
#' @return A length-1 \code{\link[base]{character}}, or \code{NULL}.
#'
#' @noRd
fit_weights_method <- function(x) {
  if (!inherits(x, "bayesmanecfit")) {
    return(NULL)
  }
  attr(x$mod_stats$wi, "method")
}

#' Crossed model weights across the levels of a factor
#'
#' The weight of every combination of per-level models, and the two readings of
#' that table which answer different questions.
#'
#' @param object An object of class \code{\link{bayesnecgroupfit}}.
#' @param pooled Optionally, a fit of the same model set to the \emph{whole}
#' data set, ignoring the factor, as returned by \code{\link{bnec}}. Used to
#' answer whether the factor matters at all --- see Details.
#'
#' @details Levels partition the data disjointly and share no parameters, so
#' \code{elpd} is additive across them:
#' \code{elpd(m_1, ..., m_G) = sum_g elpd_g(m_g)}. Under pseudo-BMA the crossed
#' weight of a combination is proportional to the product of its per-level
#' weights, so the whole table follows from the per-level vectors and never has
#' to be materialised --- which matters, because with 23 models and \emph{G}
#' levels it has \code{23^G} cells.
#'
#' \bold{This identity is specific to pseudo-BMA}, the package default, and
#' \code{crossed_group_weights()} enforces it rather than assuming it: a fit
#' built with any other weighting method is refused, because there is no
#' correct crossed table to return for one.
#' Stacking optimises a different objective whose solution is not generally an
#' outer product; stacked crossed weights would require the full pointwise
#' matrix and are not computed here. The same caveat applies to
#' \code{\link{crossed_weights}} for hurdle fits, for the same reason.
#'
#' \bold{Two readings, both useful}
#'
#' The \strong{unrestricted} maximum picks the best model for each level
#' independently, and will typically assign different equations to different
#' levels. That is the direct answer to the question this function exists for:
#' whether the functional form of the response changes across levels.
#'
#' The \strong{diagonal} maximum, \code{w_m proportional to prod_g w_gm}, is a
#' comparison of \emph{common-form} models: which single equation best describes
#' every level, which is often the question a reader of the analysis has.
#'
#' \bold{The contribution of the factor itself}
#'
#' Pass \code{pooled} --- a \code{\link{bnec}} fit of the same model set to the
#' whole data set, with the factor ignored --- and the same additivity gives the
#' third reading. A pooled fit is scored on exactly the same observations as the
#' levels together are, so their information criteria are directly comparable:
#' the grouped WAIC is the sum over levels of the best model's WAIC, and the
#' difference against the pooled fit's is a like-for-like comparison. A negative
#' \code{diff} favours the pooled fit, a positive one the grouped fit.
#'
#' The comparison is on WAIC because that is what \code{bayesnec} stores on
#' every fit. A standard error for the difference needs the \emph{pointwise}
#' values, and those are kept only on a single-model fit. A
#' \code{\link{bayesmanecfit}} stores its component fits as they were before
#' \code{expand_nec()} attached their criteria, so it carries each model's WAIC
#' point estimate in \code{mod_stats} and none of the pointwise values.
#'
#' So \code{se_diff} is reported when every level, and the pooled fit, settled
#' on a single model, and is \code{NA} whenever any of them is
#' model-averaged --- in which case the difference is a point estimate with no
#' uncertainty attached, which is worth remembering before reading much into a
#' small one. Where the values are present the observations are additionally
#' checked to line up before anything is computed.
#'
#' Note this compares the \emph{best} model per level against the \emph{best}
#' pooled model, not the model-averaged predictions of either. Model averaging
#' is within level by construction, and the averaged predictive density is not
#' the weighted sum of the components'.
#'
#' @return A \code{\link[base]{list}} with the per-level weight vectors, the
#' unrestricted best combination and its weight, the diagonal weights over the
#' models common to every level, and --- if \code{pooled} was given --- the
#' grouped-versus-pooled WAIC comparison.
#'
#' @seealso \code{\link{bnec_group}}, \code{\link{crossed_weights}}
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' crossed_group_weights(fit)
#' }
#'
#' @importFrom stats setNames sd
#'
#' @export
crossed_group_weights <- function(object, pooled = NULL) {
  if (!is_bayesnecgroupfit(object)) {
    stop("crossed_group_weights requires an object of class",
         " bayesnecgroupfit.", call. = FALSE)
  }
  # Refused rather than warned: under stacking there is no correct crossed
  # table to return, so returning one that looks right is worse than returning
  # nothing. The per-level weights remain valid and the message says so.
  #
  # The fits are asked first: attr(wi, "method") is what the fit actually did,
  # where object$weights_method is only what was requested of bnec_group(), and
  # the two can part company if a fit was amended afterwards.
  observed <- unique(unlist(lapply(object$fits, fit_weights_method)))
  method <- if (length(observed) > 0) {
    observed[[1]]
  } else if (!is.null(object$weights_method)) {
    object$weights_method
  } else {
    "pseudobma"
  }
  if (length(observed) > 1) {
    stop("The levels of this fit were weighted by different methods (",
         paste0("\"", observed, "\"", collapse = ", "), "). Their weights",
         " are not on a common footing, so there is no crossed table to",
         " return. Refit every level the same way.", call. = FALSE)
  }
  if (!identical(method, "pseudobma")) {
    stop("crossed_group_weights is defined for pseudo-BMA weights only, and",
         " this fit used \"", method, "\". The crossed weight of a",
         " combination is the product of its per-level weights because a",
         " pseudo-BMA weight is a deterministic function of that model's own",
         " elpd, which is additive over levels. Stacking optimises a",
         " different objective over the whole set at once and its solution is",
         " not an outer product, so multiplying stacked weights gives a table",
         " that looks right and is not. Refit with the default weighting, or",
         " read the per-level weights off the fits directly.", call. = FALSE)
  }
  wt <- function(x) {
    if (inherits(x, "bayesmanecfit")) {
      setNames(x$mod_stats$wi, rownames(x$mod_stats))
    } else {
      setNames(1, x$model)
    }
  }
  per_level <- lapply(object$fits, wt)
  best <- vapply(per_level, function(w) names(w)[which.max(w)], character(1))
  best_weight <- prod(vapply(per_level, max, numeric(1)))
  # The diagonal is only defined over models every level actually fitted; a
  # model dropped from one level by check_models() cannot be the common form.
  common <- Reduce(intersect, lapply(per_level, names))
  diagonal <- if (length(common) > 0) {
    d <- vapply(common, function(m) {
      prod(vapply(per_level, function(w) w[[m]], numeric(1)))
    }, numeric(1))
    if (sum(d) > 0) d / sum(d) else d
  } else {
    numeric(0)
  }
  out <- list(per_level = per_level,
              best_combination = best,
              best_weight = best_weight,
              common_models = common,
              diagonal = sort(diagonal, decreasing = TRUE))
  if (!is.null(pooled)) {
    out$pooled <- compare_pooled(object, pooled, best)
  }
  out
}

#' The best model's WAIC on one level, with its pointwise values if kept
#'
#' @param x A fit for one level.
#' @param model The model to read, or NULL for the best-weighted one.
#'
#' @return A \code{\link[base]{list}} of \code{model}, \code{waic} and
#' \code{pointwise}.
#'
#' @noRd
fit_best_waic <- function(x, model = NULL) {
  if (inherits(x, "bayesmanecfit")) {
    if (is.null(model)) {
      model <- rownames(x$mod_stats)[which.max(x$mod_stats$wi)]
    }
    waic <- x$mod_stats$waic[match(model, rownames(x$mod_stats))]
    brmfit <- x$mod_fits[[model]]$fit
  } else {
    model <- x$model
    brmfit <- x$fit
    waic <- try(extract_waic_estimate(x), silent = TRUE)
    if (inherits(waic, "try-error")) {
      waic <- NA_real_
    }
  }
  # NULL for every component of a bayesmanecfit: expand_manec() takes its
  # mod_fits snapshot *before* the expand_nec() loop that calls add_criteria(),
  # so the stored component fits never carry criteria. mod_stats$waic has the
  # point estimate, which is why the WAIC comparison still works and only the
  # standard error is lost. See R/expand_classes.R.
  pw <- brmfit$criteria$waic$pointwise
  list(model = model,
       waic = as.numeric(waic),
       pointwise = if (is.null(pw)) NULL else as.numeric(pw[, "waic"]))
}

#' Grouped versus pooled, on the same observations
#'
#' @param object An object of class \code{\link{bayesnecgroupfit}}.
#' @param pooled A fit of the same model set to the whole data.
#' @param best The best model per level, from \code{crossed_group_weights}.
#'
#' @return A \code{\link[base]{list}}.
#'
#' @noRd
compare_pooled <- function(object, pooled, best) {
  if (!inherits(pooled, c("bayesnecfit", "bayesmanecfit"))) {
    stop("`pooled` must be a bayesnecfit or bayesmanecfit, as returned by",
         " bnec() on the whole data set with the factor ignored.",
         call. = FALSE)
  }
  # The WAIC point estimates are always comparable, because bayesnec stores one
  # per model on every fit. The *standard error* needs the pointwise values, and
  # needs them to line up observation for observation -- so the checks below
  # guard the SE only, and a mismatch leaves it NA rather than blocking the
  # comparison or, worse, pairing values that do not correspond.
  per_level <- lapply(seq_along(object$levels), function(i) {
    fit_best_waic(object$fits[[i]], best[[object$levels[i]]])
  })
  names(per_level) <- object$levels
  pooled_w <- fit_best_waic(pooled)
  waic_grouped <- sum(vapply(per_level, function(z) z$waic, numeric(1)))
  diff <- pooled_w$waic - waic_grouped
  se_diff <- NA_real_
  n_obs <- NA_integer_
  grouped_pw <- lapply(per_level, function(z) z$pointwise)
  if (!any(vapply(grouped_pw, is.null, logical(1))) &&
        !is.null(pooled_w$pointwise)) {
    n_level <- vapply(grouped_pw, length, integer(1))
    if (identical(as.integer(n_level), as.integer(object$n)) &&
          length(pooled_w$pointwise) == sum(n_level) &&
          nrow(object$data) == sum(n_level)) {
      # Assembled in level order, which is the order the pooled fit's rows are
      # in only if the data were already sorted by level. The paired difference
      # below is therefore taken on the level-ordered pooled values, recovered
      # by the same split as the fits themselves used.
      grp <- factor(object$data[[object$group_var]], levels = object$levels)
      pooled_ordered <- pooled_w$pointwise[order(grp)]
      d <- pooled_ordered - unlist(grouped_pw, use.names = FALSE)
      n_obs <- length(d)
      se_diff <- sqrt(n_obs) * sd(d)
    }
  }
  list(grouped_models = best,
       pooled_model = pooled_w$model,
       waic_grouped = waic_grouped,
       waic_pooled = pooled_w$waic,
       diff = diff,
       se_diff = se_diff,
       n_obs = n_obs)
}
