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
#' @details \bold{Why the levels can be fitted separately}
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
#' \bold{Dispersion is per level, deliberately}
#'
#' Separate fits give each level its own \code{sigma}/\code{shape}/\code{phi}.
#' That is a feature, not an oversight: a shared dispersion parameter would
#' break the factorisation the crossed weights depend on. A model with structure
#' spanning levels --- shared dispersion, a group-level effect crossing levels,
#' an explicit contrast --- is a different model, and is not what this function
#' fits.
#'
#' \bold{What this does not do}
#'
#' It does not refit the favoured combination jointly. That is stage 2 of #33
#' and lands in the post-processing that the \code{toxval} migration moves, so
#' it is deliberately out of scope here.
#'
#' \bold{Fitting the levels in parallel}
#'
#' A grouped call has two loops that could use a \pkg{future} plan: the levels,
#' and the models within a level. One plan drives one of them, because
#' \pkg{future} evaluates a nested future sequentially unless the plan is a
#' list, so \code{bnec_group()} decides which and says which.
#'
#' The two arrangements are counted in rounds of one fit and the smaller is
#' taken, a tie going to the models, which is what earlier versions did. Writing
#' \emph{L} for the levels, \emph{M} for the equations the formula asks for and
#' \emph{W} for the workers, the levels take \code{ceiling(L / W) * M} rounds
#' and the models \code{L * ceiling(M / W)}. Seven levels of eleven equations
#' over four workers is 22 against 21, so the levels stay in sequence; over
#' eight workers it is 11 against 14, so they do not. A nested plan ---
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
#' \bold{What a plan does and does not reproduce}
#'
#' Each level is given its own seed, realised in the calling session before the
#' levels are dispatched and derived from \code{seed} where you passed one. The
#' realised seed is passed to every equation fitted for that level and is
#' reapplied when the model-averaging draw is realised. A grouped call therefore
#' gives the same estimates whichever arrangement it took, and the worker count
#' does not change an answer. With a \code{seed} it repeats with no
#' \code{\link[base]{set.seed}} in the session; without one,
#' \code{\link[base]{set.seed}} before the call fixes it.
#'
#' What was realised is kept: the returned object carries \code{level_seeds},
#' one per level in the order of \code{levels}. A regenerated seed cannot be
#' trusted to be the same one years later --- \code{sample.int()}'s algorithm
#' changed once already, in R 3.6.0 --- and these objects are archived and
#' reopened, so the realisation is stored rather than described.
#'
#' The estimates a grouped call reports are not those earlier versions reported,
#' because the levels are now seeded rather than drawing from the session's
#' stream as each is reached. They are another realisation of the same
#' weighting. \code{\link{bnec}} describes the rest of what a plan does to a
#' fit, and none of that changes here.
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
  # Read the family argument unevaluated, before `family` is reassigned below.
  link_source <- family_link_source(substitute(family), env = parent.frame())
  predictor_scale <- validate_predictor_scale(predictor_scale)
  if (!is.character(group_var) || length(group_var) != 1) {
    stop("`group_var` must be a single column name.", call. = FALSE)
  }
  if (!group_var %in% names(data)) {
    stop("`group_var` \"", group_var, "\" is not a column of `data`.",
         call. = FALSE)
  }
  # Coerce before model.frame() below. A plain formula dispatches to
  # stats::model.frame(), which fails with "variable lengths differ".
  formula <- bayesnecformula(formula, env = parent.frame())
  # Narrow the formula's environment before anything is built from it. Every
  # level and every model sends the formula, with that environment, to a worker.
  formula <- narrow_formula_environment(formula, data)
  grp <- data[[group_var]]
  if (is.numeric(grp)) {
    stop("The grouping column \"", group_var, "\" is numeric. A factor",
         " covariate must be a character or a factor, so that its levels are",
         " unambiguous; a numeric column is almost always a predictor that",
         " belongs in crf() instead.", call. = FALSE)
  }
  # Refuse missing groups rather than drop them. factor() and table() ignore
  # NA, so an NA group would pass the level checks below unnoticed.
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
  # The checks from here to the level loop run once, on the whole data. Left
  # to each level's bnec() call, a refusal would arrive only after the earlier
  # levels had compiled and sampled, and a report would print once per level.
  # Missing values are found here so that rows are named as the user recorded
  # them, not by their position within a level.
  mod_dat <- model.frame(formula, data = data)
  check_complete_cases(mod_dat)
  validate_predictor_scale(
    predictor_scale, retrieve_var(mod_dat, "x_var", error = TRUE)
  )
  check_disp_finite(formula, data)
  check_reserved_names(data)
  dots <- list(...)
  # dots_arg() matches an abbreviation such as `asymptote = FALSE`, as bnec()
  # does when do.call() passes it these arguments.
  declared <- dots_arg(dots, "asymptote_observed", TRUE)
  # Check the value before printing any message, as bnec() does.
  chk_flag(declared, x_name = "`asymptote_observed`")
  # Each level's bnec() call reports its own response substitutions, because
  # the values substituted differ between levels.
  # Choose the family once, from the whole response; see Details.
  if (is.null(family)) {
    y <- retrieve_var(mod_dat, "y_var", error = TRUE)
    tr <- retrieve_var(mod_dat, "trials_var")
    family <- set_distribution(y, support_integer = TRUE, trials = tr)
    message("Family chosen once from the whole response: ", family,
            ". Pass `family` to override.")
  }
  family <- validate_family(family, link_source = link_source)
  # Refuse a missing trials() term before the reports below read the response,
  # which they cannot do without it.
  check_trials_term(mod_dat, family)
  # A level with every observation at a bound is fitted with ecxflat alone, and
  # the other levels with the requested set. constant_fallback_levels() reports
  # those levels, refuses a level ecxflat cannot fit either, and refuses a name
  # that is not an equation. It must run before the flatness report, which
  # reads the set each level will fit.
  requested_models <- try(get_model_from_formula(formula), silent = TRUE)
  flat_levels <- constant_fallback_levels(
    mod_dat, family, grp, group_var,
    model = if (inherits(requested_models, "try-error")) {
      NULL
    } else {
      requested_models
    }
  )
  dots[[".bayesnec_bound_reported"]] <- TRUE
  # bnec() removes the private .bayesnec_* markers before brms sees them.
  # Read the equation set per level, because check_models() can drop an
  # equation in one level and keep it in another. Where the set cannot be
  # read, report nothing here and let bnec() raise the error.
  level_sets <- character(0)
  level_survival <- NULL
  if (!inherits(requested_models, "try-error")) {
    # A level at a bound fits ecxflat alone, which has no bot, nec or ec50
    # prior and no lower asymptote, so use that set for it here.
    level_models <- setNames(lapply(levs, function(level) {
      level_set <- if (level %in% flat_levels) {
        constant_equations()
      } else {
        requested_models
      }
      suppressMessages(
        check_models(level_set, family,
                     mod_dat[grp == level, , drop = FALSE])
      )
    }), levs)
    # Exclude here, for every level, an equation that keeps no group-level
    # term on a grouping variable, and report it once. Each level's bnec() call
    # still makes and records the exclusion; the marker set below silences only
    # its report. This must precede the flatness report.
    group_excluded <- NULL
    for (level in levs) {
      level_excluded <- group_term_exclusions(formula, level_models[[level]])
      level_kept <- setdiff(level_models[[level]], level_excluded$model)
      if (length(level_kept) == 0 && nrow(level_excluded) > 0) {
        stop("At level \"", level, "\" of \"", group_var, "\": ",
             group_terms_refusal(level_excluded), call. = FALSE)
      }
      level_models[[level]] <- level_kept
      group_excluded <- rbind(group_excluded, level_excluded)
    }
    if (!is.null(group_excluded) && nrow(group_excluded) > 0) {
      message(group_terms_report(unique(group_excluded)))
    }
    dots[[".bayesnec_group_terms_reported"]] <- TRUE
    level_survival <- suppressMessages(
      check_model_survival(dots_arg(dots, "model_survival"), family, mod_dat)
    )
    sensitive_blocks <- lapply(level_models, function(models) {
      uses_response_range_defaults(dots_arg(dots, "prior"), models, family,
                                   level_survival)
    })
    if (any(unlist(sensitive_blocks, use.names = FALSE))) {
      check_response_flattened(
        mod_dat, family, group = grp,
        blocks = lapply(sensitive_blocks, function(blocks) {
          names(blocks)[blocks]
        }),
        pool_dispersion = is.null(parse_disp_term(formula)),
        asymptote_observed = declared
      )
    }
    # The union of the per-level sets: every equation some level will fit.
    level_sets <- unique(unlist(level_models, use.names = FALSE))
  }
  dots[[".bayesnec_flatness_checked"]] <- TRUE
  # Use the whole response: it decides whether a floor can be derived, and that
  # cannot differ by level. Where the set could not be read, level_sets is
  # empty and only the refusal can be raised.
  check_asymptote_declaration(mod_dat, family, level_sets,
                              asymptote_observed = declared,
                              prior = dots_arg(dots, "prior"),
                              model_survival = level_survival)
  dots[[".bayesnec_asymptote_checked"]] <- TRUE
  # Record the requested weighting method for crossed_group_weights(), which
  # cannot always read it off the fits: row-subsetting mod_stats drops the
  # method attribute, and a single-model level has no mod_stats.
  wt_method <- dots_arg(dots, "loo_controls")$weights$method
  if (is.null(wt_method)) {
    wt_method <- "pseudobma"
  }
  # Count the equations the formula asks for, an upper bound on what each level
  # fits. Where the set cannot be read, n_models is NA, the levels run in
  # sequence, and bnec() raises the error.
  models <- try(get_model_from_formula(formula), silent = TRUE)
  n_models <- if (inherits(models, "try-error")) NA_integer_ else length(models)
  level_plan <- plan_group_levels(length(levs), n_models)
  # Read the option here, not in the worker: future does not export options,
  # so a multisession worker would see it unset.
  cache_root <- getOption("cmdstanr_write_stan_file_dir")
  # One seed per level, derived from `seed` where one was given. Each level
  # applies it to its model fits and to its model-averaging draw; without both,
  # the estimates would depend on the number of workers.
  level_seeds <- group_level_seeds(length(levs), dots[["seed"]])
  if (level_plan$concurrent) {
    # Announce every level here. A message sent from a worker arrives out of
    # order or not at all.
    message("Levels: ",
            paste0("\"", levs, "\" (", counts[levs], " observations)",
                   collapse = ", "), ".")
  }
  # Dispatch the subsets themselves, not indices into `data`: future sends a
  # function's globals with every future, so each level would receive all of
  # `data`. split() orders the pieces by levels(grp), the order of `levs`.
  parts <- Map(
    function(i, d) list(i = i, data = d),
    seq_along(levs), unname(split(data, grp))
  )
  concurrent <- level_plan$concurrent
  # narrow_environment() keeps only the names listed below, so that a worker
  # is not sent this frame, which holds `data`. It cannot hold `...`, so the
  # arguments are forwarded with do.call(). Pass "bnec" by name: given the
  # function itself, do.call() prints its source in every error from a level.
  fit_level <- narrow_environment(
    function(part) {
      i <- part$i
      set.seed(level_seeds[i])
      if (concurrent) {
        # Restore the option on exit, in case the plan evaluates in this
        # session. See worker_stan_cache_dir().
        old <- options(
          cmdstanr_write_stan_file_dir = worker_stan_cache_dir(cache_root)
        )
        on.exit(options(old), add = TRUE)
      }
      # `seed` fixes initialisation and sampling. .bayesnec_group_seed, which
      # bnec() removes before brms sees it, fixes the model-averaging draw.
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
  # Restore the caller's random number stream after the loop. fit_level()
  # calls set.seed() on every path, the sequential one included.
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
    # Dispatch one level at a time, so that each level's message prints as it
    # starts: future_lapply() relays messages only when it collects. Entering
    # a future still hands the inner strategy of a nested plan to the models.
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
              # Store the realised seeds: a seed regenerated in a later R
              # version may differ, as sample.int() has changed before.
              level_seeds = level_seeds)
  allot_class(out, c("bayesnecgroupfit", "bnecfit"))
}

#' The weighting method a fit was built with
#'
#' Reads the method \code{expand_manec()} stores on the weight vector.
#' Row-subsetting \code{mod_stats} drops it, and a single-model fit has no
#' \code{mod_stats}, so \code{NULL} means unknown, not pseudo-BMA.
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

#' The model weights one fit holds
#'
#' @param x A fit for one level, of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#'
#' @details A single-model fit has a weight of one on its own equation.
#'
#' @return A named \code{\link[base]{numeric}} vector.
#'
#' @importFrom stats setNames
#'
#' @noRd
fit_model_weights <- function(x) {
  if (inherits(x, "bayesmanecfit")) {
    setNames(x$mod_stats$wi, rownames(x$mod_stats))
  } else {
    setNames(1, x$model)
  }
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
#' to be materialised --- which matters, because with the 24 equations of the
#' default set and \emph{G} levels it has \code{24^G} cells.
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
#' every level. \code{bayesnec} could not answer that before, and it is often
#' the question a reader of the analysis actually has.
#'
#' \bold{Does the factor matter at all?}
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
  # Refuse rather than warn: no crossed table is correct under stacking.
  # Prefer the method the fits hold to object$weights_method, which is what was
  # requested and can differ once a fit has been amended.
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
  per_level <- lapply(object$fits, fit_model_weights)
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
  # NULL for every component of a bayesmanecfit, whose stored fits carry no
  # criteria. Only the standard error is lost: mod_stats$waic holds the point
  # estimate.
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
  # The checks below guard the standard error only. Where the pointwise values
  # are missing or do not line up by observation, se_diff stays NA.
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
      # The grouped values are in level order, so put the pooled values in
      # level order too before pairing them.
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

#' One argument of \code{bnec()}, read from a \code{bnec_group()} dot list
#'
#' @details Matches \code{name} as \code{do.call(bnec, ...)} will, so that an
#' abbreviation is found: \code{model_s = "nec3param"} reaches \code{bnec()} as
#' \code{model_survival}, and an exact lookup would read it as absent.
#'
#' @param dots The dot list \code{\link{bnec_group}} collected.
#' @param name The \code{\link{bnec}} formal to read.
#' @param default What to return where the argument was not supplied.
#'
#' @return The supplied value, or \code{default}.
#'
#' @noRd
dots_arg <- function(dots, name, default = NULL) {
  hit <- which(!is.na(pmatch(names(dots), name)))
  if (length(hit) == 1) dots[[hit]] else default
}
