#' Group-level terms in a fitted brms formula
#'
#' @param fit A brms fit.
#' @return A list of grouping expressions, without duplicates.
#' @noRd
fitted_group_terms <- function(fit) {
  walk <- function(expr) {
    if (!is.call(expr)) return(list())
    if (identical(expr[[1]], as.name("|")) ||
        identical(expr[[1]], as.name("||"))) {
      return(list(expr[[3]]))
    }
    unlist(lapply(as.list(expr)[-1], walk), recursive = FALSE)
  }
  forms <- c(list(fit$formula$formula), fit$formula$pforms)
  terms <- unlist(lapply(forms, function(f) walk(f[[3]])), recursive = FALSE)
  keys <- vapply(terms, deparse1, character(1))
  terms[!duplicated(keys)]
}

#' Warn when a saved fit still holds population-level display values
#' @param object A single fit or model set.
#' @return Invisible NULL.
#' @noRd
warn_stale_observed_display <- function(object) {
  if (!needs_observed_rebuild(object)) return(invisible(NULL))
  if (inherits(object, "bayesmanecfit")) {
    stored <- object$w_pred_vals
  } else {
    stored <- object$pred_vals
  }
  warning("This fit was saved before observed-level display values were",
          " stored. Its stored summary and plotted band still describe the",
          " zero-deviation curve. Rebuild the display values with",
          " amend(x, resolution = ", nrow(stored$data), "); ecx(), nsec(),",
          " ecnsec() and nec() already read the observed levels directly.",
          call. = FALSE)
  invisible(NULL)
}

#' Whether a saved fit needs its observed-level display rebuilt
#' @param object A single fit or model set.
#' @return A logical value.
#' @noRd
needs_observed_rebuild <- function(object) {
  if (inherits(object, "bayesmanecfit")) {
    if (isTRUE(object$w_pred_vals$observed_levels)) return(FALSE)
    return(any(vapply(object$mod_fits, function(fit) {
      !is.null(fit$fit) && !is.null(observed_level_spec(fit))
    }, logical(1))))
  }
  !isTRUE(object$pred_vals$observed_levels) &&
    !is.null(object$fit) && !is.null(observed_level_spec(object))
}

#' Observed combinations of grouping terms that span the series
#'
#' A term spans the series if at least one observed level has more than one
#' predictor value. Terms whose every level occurs at one value are excluded
#' from the prediction formula. A combination observed at one predictor value
#' has no curve of its own, so it is not read; its label is kept in
#' \code{excluded}. Where no combination remains, the terms are read as nested.
#'
#' @param object A single fit, a level of a joint refit, or a hurdle pair.
#' @return A list of observed combinations, their counts and ranges, or NULL.
#' @noRd
observed_level_spec <- function(object) {
  if (!is.null(object$observed_group)) return(NULL)
  if (is_bayesnechurdlefit(object)) {
    # The survival component holds every exposed individual, so it has the
    # whole series of each level.
    object <- object$survival
  }
  if (inherits(object, "bayesmanecfit")) object <- object$mod_fits[[1]]
  fit <- object$fit
  groups <- fitted_group_terms(fit)
  if (!length(groups)) return(NULL)
  dat <- fit$data
  if (inherits(object, "bayesnecjointlevel")) {
    dat <- dat[joint_row_levels(object) == object$level, , drop = FALSE]
  }
  frame <- stats::model.frame(object$bayesnecformula, data = fit$data)
  x_var <- attr(frame, "bnec_pop")[["x_var"]]
  spanning <- vapply(groups, function(group) {
    vars <- all.vars(group)
    if (!length(vars) || !all(vars %in% names(dat))) {
      stop("The fitted group term ", deparse1(group),
           " cannot be matched to its data columns.", call. = FALSE)
    }
    keys <- interaction(dat[vars], drop = TRUE, lex.order = TRUE)
    any(vapply(split(dat[[x_var]], keys), function(x) {
      length(unique(x[is.finite(x)])) > 1L
    }, logical(1)))
  }, logical(1))
  if (!any(spanning)) return(NULL)
  groups <- groups[spanning]
  vars <- unique(unlist(lapply(groups, all.vars), use.names = FALSE))
  combinations <- unique(dat[vars])
  rownames(combinations) <- NULL
  n <- integer(nrow(combinations))
  x <- vector("list", nrow(combinations))
  for (i in seq_len(nrow(combinations))) {
    keep <- rep(TRUE, nrow(dat))
    for (v in vars) keep <- keep & dat[[v]] == combinations[[v]][i]
    n[i] <- sum(keep)
    x[[i]] <- dat[[x_var]][keep]
  }
  spans <- vapply(x, function(v) length(unique(v[is.finite(v)])) > 1L,
                  logical(1))
  if (!any(spans)) return(NULL)
  excluded <- level_labels(combinations[!spans, , drop = FALSE])
  combinations <- combinations[spans, , drop = FALSE]
  rownames(combinations) <- NULL
  rhs <- paste(vapply(groups, function(g) {
    paste0("(1 | ", deparse1(g), ")")
  }, character(1)), collapse = " + ")
  re_formula <- stats::as.formula(paste("~", rhs),
                                   env = environment(fit$formula$formula))
  list(combinations = combinations, n = n[spans], x = x[spans],
       re_formula = re_formula, x_var = x_var, excluded = excluded)
}

#' Labels of observed group combinations
#' @param combinations A data frame with one row per combination.
#' @return A character vector such as \code{"plate=a"}.
#' @noRd
level_labels <- function(combinations) {
  vapply(seq_len(nrow(combinations)), function(i) {
    paste(paste0(names(combinations), "=",
                 vapply(combinations[i, , drop = FALSE], as.character,
                        character(1))), collapse = ", ")
  }, character(1))
}

#' Say which group levels an estimate leaves out
#'
#' Signals a message of class \code{bayesnec_level_note}, which
#' \code{once_level_note()} reduces to one per call.
#'
#' @param spec An observed level specification.
#' @return Invisible NULL.
#' @noRd
note_excluded_levels <- function(spec) {
  if (!length(spec$excluded)) return(invisible(NULL))
  text <- paste0(
    "Group level(s) ", paste(spec$excluded, collapse = "; "),
    " were observed at one concentration, so they have no curve of their",
    " own and are left out of the estimate."
  )
  message(structure(
    class = c("bayesnec_level_note", "message", "condition"),
    list(message = paste0(text, "\n"), call = NULL)
  ))
  invisible(NULL)
}

#' Print each excluded-level note once
#'
#' The equations of a model set share their data, so each repeats the note.
#'
#' @param expr The expression to evaluate.
#' @return The value of \code{expr}.
#' @noRd
once_level_note <- function(expr) {
  seen <- character()
  withCallingHandlers(expr, bayesnec_level_note = function(m) {
    text <- conditionMessage(m)
    if (text %in% seen) invokeRestart("muffleMessage")
    seen <<- c(seen, text)
  })
}

#' Mutable state for internal calls
#' @noRd
bnec_state <- new.env(parent = emptyenv())

#' One equation of a model set, for an estimator to read
#'
#' \code{pull_out()} rebuilds the stored display values, which for a fit with
#' a spanning group term means predicting every level. The estimators read the
#' levels directly, so those values are built on the zero-deviation basis here.
#' Do not return the result to a user.
#'
#' @param object A model set.
#' @param model The equation to extract.
#' @return A single fit.
#' @noRd
pull_equation <- function(object, model) {
  old <- bnec_state$skip_level_display
  bnec_state$skip_level_display <- TRUE
  on.exit(bnec_state$skip_level_display <- old, add = TRUE)
  out <- suppressMessages(pull_out(object, model = model))
  source <- object$mod_fits[[model]]
  if (!is.null(object$observed_group)) {
    for (field in c("observed_group", "level_x", "re_formula")) {
      out[[field]] <- object[[field]]
    }
    if (!is.null(source$ne_posterior)) {
      out$ne_posterior <- source$ne_posterior
    }
  }
  out
}

#' One observed combination viewed as a single fit
#' @param object The parent fit, or a hurdle pair.
#' @param spec Its observed level specification.
#' @param i The combination to read.
#' @param with_ne Whether to read the level's nec parameter.
#' @return A single fit with prediction metadata for one combination.
#' @noRd
observed_level_fit <- function(object, spec, i, with_ne = FALSE) {
  if (is_bayesnechurdlefit(object)) {
    out <- object
    out$observed_group <- spec$combinations[i, , drop = FALSE]
    out$level_x <- spec$x[[i]]
    out$re_formula <- spec$re_formula
    for (part in c("growth", "survival")) {
      component <- out[[part]]
      if (inherits(component, "bayesnecfit")) {
        out[[part]] <- observed_level_fit(
          component, spec, i,
          with_ne = with_ne &&
            has_nec_parameter(component$model, component$fit)
        )
      } else if (inherits(component, "bayesmanecfit")) {
        component$observed_group <- out$observed_group
        component$level_x <- out$level_x
        component$re_formula <- out$re_formula
        for (model in names(component$mod_fits)) {
          source <- component$mod_fits[[model]]
          level <- observed_level_fit(
            allot_class(source, c("bayesnecfit", "bnecfit")), spec, i,
            with_ne = with_ne && has_nec_parameter(
              model, source$fit
            )
          )
          for (field in c("observed_group", "level_x", "re_formula",
                          "ne_posterior")) {
            source[[field]] <- level[[field]]
          }
          component$mod_fits[[model]] <- source
        }
        out[[part]] <- component
      }
    }
    return(out)
  }
  out <- object
  out$observed_group <- spec$combinations[i, , drop = FALSE]
  out$level_x <- spec$x[[i]]
  out$re_formula <- spec$re_formula
  if (with_ne) {
    nd <- newdata_eval(out, resolution = 2, x_range = NA)$newdata[1, ,
                                                                 drop = FALSE]
    ne <- as.numeric(brms::posterior_linpred(
      out$fit, newdata = nd, nlpar = "nec", re_formula = spec$re_formula,
      transform = TRUE
    )[, 1])
    if (!is.null(out$hurdle) &&
        identical(out$hurdle$ne_type, "NEC")) {
      hu_ne <- as.numeric(brms::posterior_linpred(
        out$fit, newdata = nd,
        nlpar = paste0(hurdle_dpar(out$fit$family), "nec"),
        re_formula = spec$re_formula, transform = TRUE
      )[, 1])
      ne <- pmin(ne, hu_ne)
    }
    bounds <- sub_x_transformation(range(out$level_x),
                                   out$bayesnecformula)
    attr(ne, "censored") <- censoring_record(
      max(bounds), min(bounds), ne >= max(bounds), ne <= min(bounds)
    )
    out$ne_posterior <- ne
  }
  out
}

#' Keep the control concentrations with a result that combines levels
#' @param result An estimate table, draw list or pooled estimate.
#' @param values Named control concentrations in level order.
#' @return The result with its control concentrations recorded.
#' @noRd
record_level_controls <- function(result, values) {
  attr(result, "x_control") <- values
  result
}

#' Control record for a model set containing observed group levels
#' @param first The first equation's stored fit.
#' @param choice The validated control option.
#' @return A function that records the controls, or identity.
#' @noRd
model_set_control_recorder <- function(first, choice) {
  spec <- observed_level_spec(first)
  if (is.null(spec)) return(identity)
  labels <- level_labels(spec$combinations)
  values <- setNames(vapply(spec$x, min, numeric(1)), labels)
  if (!identical(choice, "own")) values[] <- as.numeric(choice)
  function(x) record_level_controls(x, values)
}

#' Summarise estimates over the observed combinations of a fitted term
#' @param object The parent fit.
#' @param spec Its observed level specification.
#' @param what The estimator name.
#' @param fun A function that returns draws for one combination.
#' @param pool The requested pooling method.
#' @param posterior Whether to return draws.
#' @param prob_vals Summary probabilities.
#' @return A per-level table or list, or a pooled vector.
#' @noRd
estimate_observed_levels <- function(object, spec, what, fun, pool,
                                     posterior, prob_vals,
                                     x_control = "own", x_range = NA,
                                     curve_args = NULL) {
  if (is.null(pool)) pool <- "mixture"
  pool <- validate_pool(pool, what)
  note_excluded_levels(spec)
  labels <- level_labels(spec$combinations)
  controls <- setNames(vapply(spec$x, min, numeric(1)), labels)
  control_value <- x_control
  range_override <- x_range
  if (identical(what, "nec")) {
    record_level_controls <- function(result, values) result
  } else if (identical(x_control, "shared") || is.numeric(x_control)) {
    lows <- vapply(spec$x, min, numeric(1))
    control_value <- checked_shared_control(object, x_control, lows)
    controls[] <- as.numeric(control_value)
    if (is.na(x_range[1])) {
      range_override <- c(as.numeric(control_value),
                          max(unlist(spec$x, use.names = FALSE)))
    }
  }
  if (identical(pool, "mean_curve")) {
    fits <- lapply(seq_len(nrow(spec$combinations)), function(i) {
      observed_level_fit(object, spec, i)
    })
    curve_args$x_control <- control_value
    curve_args$x_range <- range_override
    curve_args$posterior <- posterior
    return(record_level_controls(
      mean_curve_estimate(fits, spec$n, what, curve_args), controls
    ))
  }
  with_ne <- identical(what, "nec") &&
    (is.null(object$hurdle) || identical(object$hurdle$ne_type, "NEC"))
  draws <- lapply(seq_along(labels), function(i) {
    fun(observed_level_fit(object, spec, i, with_ne = with_ne),
        control_value, range_override)
  })
  names(draws) <- labels
  if (identical(pool, "none")) {
    if (posterior) return(record_level_controls(draws, controls))
    summaries <- lapply(draws, function(x) {
      out <- summarise_censored(x, prob_vals)
      names(out) <- clean_names(out)
      out
    })
    return(record_level_controls(
      estimate_table(summaries, labels, what), controls
    ))
  }
  pooled <- pool_estimate_draws(draws, spec$n, pool)
  if (posterior) return(record_level_controls(pooled, controls))
  out <- summarise_censored(pooled, prob_vals)
  names(out) <- clean_names(out)
  if (identical(what, "nsec") &&
      !is.null(attr(pooled, "ecnsec_relativeP"))) {
    attr(out, "ecnsec_relativeP") <- stats::quantile(
      attr(pooled, "ecnsec_relativeP"), probs = prob_vals, na.rm = TRUE
    )
  }
  record_level_controls(out, controls)
}

#' Control concentrations used when reading separate fitted levels
#' @param fits Named one-level fits.
#' @param choice The shared or own control option after validation.
#' @return A named numeric vector.
#' @noRd
level_control_values <- function(fits, choice) {
  if (is.null(choice) || identical(choice, "own")) {
    return(vapply(fits, control_x, numeric(1)))
  }
  setNames(rep(as.numeric(choice), length(fits)), names(fits))
}

#' Use one control concentration across separate or joint levels
#' @param fits A named list of one-level fits.
#' @param dots Arguments passed to the estimator.
#' @return Updated arguments with one control and one prediction range.
#' @noRd
shared_level_control <- function(fits, dots) {
  choice <- dots$x_control
  if (is.null(choice) || identical(choice, "own")) return(dots)
  if (!identical(choice, "shared") && !is.numeric(choice)) return(dots)
  lows <- unlist(lapply(fits, function(fit) {
    spec <- observed_level_spec(fit)
    if (is.null(spec)) control_x(fit) else
      vapply(spec$x, min, numeric(1))
  }), use.names = FALSE)
  value <- checked_shared_control(fits[[1]], choice, lows)
  dots$x_control <- value
  if (is.null(dots$x_range) || is.na(dots$x_range[1])) {
    highs <- vapply(fits, function(f) level_x_bounds(f)[2], numeric(1))
    dots$x_range <- c(as.numeric(value), max(highs))
  }
  dots
}

#' Issue one shared-control warning for a model set
#' @param object A model set.
#' @param choice The requested control setting.
#' @return The control setting passed to each equation.
#' @noRd
model_set_control <- function(object, choice) {
  if (identical(choice, "own")) return(choice)
  if (!identical(choice, "shared") && !is.numeric(choice)) return(choice)
  spec <- observed_level_spec(object)
  lows <- if (is.null(spec)) control_x(object) else
    vapply(spec$x, min, numeric(1))
  checked_shared_control(object, choice, lows)
}

#' Validate one control concentration for several levels
#'
#' Refuses a value the predictor transformation cannot take before warning
#' about levels observed only above it.
#'
#' @param object A fit whose formula names the predictor transformation.
#' @param choice \code{"shared"} or one number.
#' @param lows The lowest observed concentration of each level.
#' @return The control concentration, marked so that it warns once.
#' @noRd
checked_shared_control <- function(object, choice, lows) {
  if (isTRUE(attr(choice, "warned"))) return(choice)
  shared <- identical(choice, "shared")
  value <- if (shared) min(lows) else choice
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
    stop("`x_control` must be \"own\", \"shared\", or one finite number.",
         call. = FALSE)
  }
  check_control_finite(object, value)
  beyond <- value < lows
  if (any(beyond)) {
    warning(if (shared) "The shared control concentration, " else
              "The control concentration, ",
            signif(value, 3), ", is below the lowest observation of ",
            sum(beyond), " level(s), by up to ",
            signif(max(lows[beyond] - value), 3),
            ". Their curves are extrapolated down to it.", call. = FALSE)
  }
  value <- as.numeric(value)
  attr(value, "warned") <- TRUE
  value
}

#' Refuse a control concentration the predictor transformation maps off the
#' real line
#' @param object A single fit, model set, hurdle pair or joint level.
#' @param value The control concentration on the recorded scale.
#' @return Invisible NULL.
#' @noRd
check_control_finite <- function(object, value) {
  formula <- if (is_bayesnechurdlefit(object)) {
    object$formula
  } else if (inherits(object, "bayesmanecfit")) {
    object$mod_fits[[1]]$bayesnecformula
  } else {
    object$bayesnecformula
  }
  transformed <- tryCatch(
    suppressWarnings(sub_x_transformation(value, formula)),
    error = function(e) NA_real_
  )
  if (length(transformed) != 1L || !is.finite(transformed)) {
    stop("`x_control = ", value, "` is not finite under the crf()",
         " predictor transformation.", call. = FALSE)
  }
  invisible(NULL)
}

#' Draw one weighted mixture sample from each observed level
#' @param lengths Number of available draws at each level.
#' @param weights Observation counts at each level.
#' @param n Total number of draws to keep.
#' @param seed Seed for reproducible sampling.
#' @return Integer indices, one vector per level.
#' @noRd
mixture_draw_indices <- function(lengths, weights, n, seed = 216) {
  expected <- n * weights / sum(weights)
  counts <- floor(expected)
  remainder <- n - sum(counts)
  if (remainder > 0L) {
    ord <- order(expected - counts, decreasing = TRUE)
    counts[ord[seq_len(remainder)]] <- counts[ord[seq_len(remainder)]] + 1L
  }
  with_preserved_rng_state({
    set.seed(as.integer(seed %% .Machine$integer.max),
             sample.kind = "Rejection")
    lapply(seq_along(lengths), function(i) {
      sample.int(lengths[i], counts[i])
    })
  })
}

#' Posterior of the observed-level mixture on one prediction grid
#' @param fit A brms fit.
#' @param formula Its bayesnec formula.
#' @param newdata Prediction rows on a common grid.
#' @param spec Observed level specification.
#' @return A draws by grid matrix.
#' @noRd
observed_grid_posterior <- function(fit, formula, newdata, spec) {
  n <- brms::ndraws(fit)
  indices <- mixture_draw_indices(rep(n, nrow(spec$combinations)),
                                   spec$n, n)
  out <- matrix(NA_real_, nrow = n, ncol = nrow(newdata))
  next_row <- 1L
  for (i in seq_len(nrow(spec$combinations))) {
    idx <- indices[[i]]
    if (!length(idx)) next
    nd <- newdata
    for (v in names(spec$combinations)) {
      nd[[v]] <- spec$combinations[[v]][rep(i, nrow(nd))]
    }
    pred <- factorised_count_epred(
      fit, formula, newdata = nd, re_formula = spec$re_formula
    )
    rows <- seq.int(next_row, length.out = length(idx))
    out[rows, ] <- pred[idx, , drop = FALSE]
    next_row <- next_row + length(idx)
  }
  out
}

#' Curves and controls paired by posterior draw for curve pooling
#' @param object A single fit, model set, or joint level.
#' @param x_vec A common prediction grid.
#' @param x_control A number or \code{"own"}.
#' @param type ECx reference type, for the asymptote.
#' @param dpar Optional hurdle block.
#' @return A curve matrix, control vector and asymptote vector.
#' @noRd
level_curve_draws <- function(object, x_vec, x_control = "own",
                              type = "absolute", dpar = NULL) {
  if (inherits(object, "bayesmanecfit")) {
    models <- names(object$mod_fits)
    idx <- pull_draw_index(object, models, object$sample_size)
    models <- models[lengths(idx[models]) > 0L]
    parts <- lapply(models, function(m) {
      part <- pull_equation(object, m)
      out <- level_curve_draws(part, x_vec, x_control, type, dpar)
      take <- idx[[m]]
      list(curve = out$curve[take, , drop = FALSE],
           control = out$control[take],
           asymptote = rep_len(out$asymptote, nrow(out$curve))[take])
    })
    return(list(curve = do.call(rbind, lapply(parts, `[[`, "curve")),
                control = unlist(lapply(parts, `[[`, "control")),
                asymptote = unlist(lapply(parts, `[[`, "asymptote"))))
  }
  spec <- if (is.null(object$observed_group)) {
    observed_level_spec(object)
  } else {
    NULL
  }
  if (!is.null(spec)) {
    n <- brms::ndraws(object$fit)
    indices <- mixture_draw_indices(rep(n, nrow(spec$combinations)),
                                     spec$n, n)
    curve <- matrix(NA_real_, nrow = n, ncol = length(x_vec))
    control <- numeric(n)
    asymptote <- numeric(n)
    next_row <- 1L
    for (i in seq_len(nrow(spec$combinations))) {
      take <- indices[[i]]
      if (!length(take)) next
      level <- observed_level_fit(object, spec, i)
      part <- level_curve_draws(level, x_vec, x_control, type, dpar)
      rows <- seq.int(next_row, length.out = length(take))
      curve[rows, ] <- part$curve[take, , drop = FALSE]
      control[rows] <- part$control[take]
      asymptote[rows] <- rep_len(part$asymptote, n)[take]
      next_row <- next_row + length(take)
    }
    return(list(curve = curve, control = control,
                asymptote = asymptote))
  }
  nd <- newdata_eval(object, resolution = length(x_vec),
                     x_range = range(x_vec))$newdata
  re_formula <- if (is.null(object$re_formula)) NA else object$re_formula
  epred <- function(data) joint_hurdle_epred(object, data, dpar,
                                               re_formula = re_formula)
  curve <- epred(nd)
  at <- if (identical(x_control, "own")) {
    control_x(object)
  } else {
    resolve_x_control(object, x_control, range(x_vec))$value
  }
  control <- control_posterior(object, nd, epred, x_at = at)
  asymptote <- if (identical(type, "relative")) {
    count_positive_asymptote(
      object, dpar, ecx_asymptote(object, type, dpar), nd
    )
  } else {
    0
  }
  list(curve = curve, control = control, asymptote = asymptote)
}

#' The tested predictor range of one level
#' @param object A single fit or model set.
#' @return The minimum and maximum observed predictor values.
#' @noRd
level_x_bounds <- function(object) {
  if (length(object$level_x)) return(range(object$level_x))
  if (is_bayesnechurdlefit(object)) object <- object$survival
  if (inherits(object, "bayesmanecfit")) object <- grid_equation(object)
  if (length(object$level_x)) return(range(object$level_x))
  frame <- stats::model.frame(object$bayesnecformula, data = object$fit$data)
  x_var <- attr(frame, "bnec_pop")[["x_var"]]
  range(object$fit$data[[x_var]])
}

#' Estimate the weighted mean of observed level curves
#' @param fits One fit per level.
#' @param weights Observation counts.
#' @param what \code{"ecx"} or \code{"nsec"}.
#' @param dots Estimator arguments.
#' @return Draws or their summary, with censoring retained.
#' @noRd
mean_curve_estimate <- function(fits, weights, what, dots) {
  if (!what %in% c("ecx", "nsec", "ecnsec")) {
    stop("`mean_curve` is unavailable for ", what, "().", call. = FALSE)
  }
  bounds <- t(vapply(fits, level_x_bounds, numeric(2)))
  x_control <- if (is.null(dots$x_control)) "own" else dots$x_control
  if (identical(x_control, "shared")) x_control <- min(bounds[, 1])
  own <- identical(x_control, "own")
  requested <- dots$x_range
  if (is.null(requested) || is.na(requested[1])) {
    low <- if (own) max(bounds[, 1]) else as.numeric(x_control)
    high <- if (own) min(bounds[, 2]) else max(bounds[, 2])
  } else {
    low <- min(requested)
    high <- max(requested)
  }
  if (own) {
    low <- max(low, max(bounds[, 1]))
    high <- min(high, min(bounds[, 2]))
  }
  if (identical(what, "nsec")) {
    extrapolate <- if (is.null(dots$extrapolate)) FALSE else dots$extrapolate
    lims <- extrapolate_limits(extrapolate,
                               list(lower = low, upper = high), "NSEC")
    if (!is.null(lims)) {
      control_start <- if (own) max(bounds[, 1]) else as.numeric(x_control)
      low <- max(lims$lower, control_start)
      high <- lims$upper
    }
  }
  if (!is.finite(low) || !is.finite(high) || low >= high) {
    stop("The levels have no common predictor range for mean_curve.",
         call. = FALSE)
  }
  resolution <- if (is.null(dots$resolution)) 500L else dots$resolution
  x_vec <- seq(low, high, length.out = resolution)
  type <- if (is.null(dots$type)) "absolute" else dots$type
  if (identical(what, "ecnsec") && identical(type, "direct")) {
    stop("`type = \"direct\"` has no percent effect for ecnsec().",
         call. = FALSE)
  }
  dpar <- dots$dpar
  if (identical(type, "relative") && is.null(dpar) &&
      what %in% c("ecx", "ecnsec")) {
    for (fit in fits) {
      if (is_bayesnechurdlefit(fit)) {
        stop("type = \"relative\" is not defined for a hurdle fit.",
             call. = FALSE)
      }
      family <- if (inherits(fit, "bayesmanecfit")) {
        fit$mod_fits[[1]]$fit$family
      } else {
        fit$fit$family
      }
      check_combined_relative(family, what)
    }
  }
  weights <- weights / sum(weights)
  curve <- NULL
  control <- NULL
  asymptote <- NULL
  at_nsec <- NULL
  if (identical(what, "ecnsec")) {
    xform <- dots$xform
    if (!is.null(xform) && !is.function(xform)) {
      stop("`xform` must be a function or NULL.", call. = FALSE)
    }
    nsec_use <- if (is.null(xform)) dots$nsec else xform(dots$nsec)
    if (length(nsec_use) != 1L || !is.finite(nsec_use) ||
        nsec_use < low || nsec_use > high) {
      stop("The supplied NSEC lies outside the shared prediction range.",
           call. = FALSE)
    }
  }
  for (i in seq_along(fits)) {
    if (is_bayesnechurdlefit(fits[[i]])) {
      pred <- hurdle_component_preds(fits[[i]], resolution = resolution,
                                     x_range = c(low, high),
                                     x_control = x_control)
      component <- if (is.null(dots$which)) "combined" else dots$which
      part <- list(curve = pred[[component]],
                   control = pred$control[[component]], asymptote = 0)
    } else {
      part <- level_curve_draws(fits[[i]], x_vec, x_control, type, dpar)
    }
    if (identical(what, "ecnsec")) {
      at_part <- if (is_bayesnechurdlefit(fits[[i]])) {
        curve_draws_at(part$curve, x_vec, nsec_use)
      } else {
        level_curve_draws(fits[[i]], rep(nsec_use, 2),
                          x_control, type, dpar)$curve[, 1]
      }
    }
    if (is.null(curve)) {
      curve <- part$curve * weights[i]
      control <- part$control * weights[i]
      asymptote <- rep_len(part$asymptote, length(control)) * weights[i]
      if (identical(what, "ecnsec")) at_nsec <- at_part * weights[i]
    } else {
      n <- min(nrow(curve), nrow(part$curve))
      curve <- curve[seq_len(n), , drop = FALSE] +
        part$curve[seq_len(n), , drop = FALSE] * weights[i]
      control <- control[seq_len(n)] + part$control[seq_len(n)] * weights[i]
      asymptote <- asymptote[seq_len(n)] +
        rep_len(part$asymptote, n)[seq_len(n)] * weights[i]
      if (identical(what, "ecnsec")) {
        at_nsec <- at_nsec[seq_len(n)] + at_part[seq_len(n)] * weights[i]
      }
    }
  }
  if (identical(what, "ecnsec")) {
    floor <- switch(type, absolute = 0, relative = asymptote,
                    range = apply(curve, 1, min, na.rm = TRUE))
    out <- (control - at_nsec) / (control - floor) * 100
    if (isTRUE(dots$posterior)) return(out)
    probs <- if (is.null(dots$prob_vals)) c(0.5, 0.025, 0.975) else
      dots$prob_vals
    summary <- stats::quantile(out, probs = probs, na.rm = TRUE)
    names(summary) <- clean_names(summary)
    return(summary)
  }
  if (identical(what, "ecx")) {
    effect <- if (is.null(dots$ecx_val)) 10 else dots$ecx_val
    out <- ecx_from_posterior(curve, x_vec, effect, type, control,
                              asymptote)
  } else {
    sig <- if (is.null(dots$sig_val)) 0.01 else dots$sig_val
    reference <- stats::quantile(control, sig)
    start <- if (own) min(bounds[, 1]) else as.numeric(x_control)
    out <- nsec_from_posterior(curve, reference, x_vec, start, control)
    if (own && low > start) {
      # Averaged own controls have no single concentration. Put a draw already
      # below its reference at the first concentration shared by all levels.
      out[!is.na(out) & out == start] <- low
    }
    attr(out, "ecnsec_relativeP") <-
      as.numeric((control - reference) / control * 100)
  }
  below <- attr(out, "below_range")
  above <- is.na(out) & !below
  searched_from <- if (identical(what, "nsec")) {
    attr(out, "x_searched_from")
  } else {
    low
  }
  effect_draws <- attr(out, "ecnsec_relativeP")
  cens <- censoring_record(high, searched_from, above, below)
  first <- fits[[1]]
  if (inherits(first, "bayesmanecfit")) first <- grid_equation(first)
  xform <- dots$xform
  if (!returns_recorded(first, xform)) {
    to_fitted <- if (is_bayesnechurdlefit(first)) {
      function(value) hurdle_xform_x(first, value)
    } else {
      function(value) sub_x_transformation(value, first$bayesnecformula)
    }
    out <- to_fitted(out)
    cens <- xform_censoring(cens, to_fitted)
    if (!is.null(xform)) {
      out <- xform(out)
      cens <- xform_censoring(cens, xform)
    }
  }
  attr(out, "below_range") <- NULL
  attr(out, "n_below_range") <- NULL
  attr(out, "x_searched_from") <- NULL
  attr(out, "censored") <- cens
  if (!is.null(effect_draws)) attr(out, "ecnsec_relativeP") <- effect_draws
  attr(out, "resolution") <- resolution
  attr(out, "toxicity_estimate") <- what
  if (identical(what, "ecx")) {
    attr(out, "ecx_val") <- effect
  } else {
    attr(out, "sig_val") <- sig
  }
  if (isTRUE(dots$posterior)) return(out)
  probs <- if (is.null(dots$prob_vals)) c(0.5, 0.025, 0.975) else
    dots$prob_vals
  summary <- summarise_censored(out, probs)
  names(summary) <- clean_names(summary)
  attr(summary, "resolution") <- resolution
  attr(summary, "toxicity_estimate") <- what
  if (identical(what, "ecx")) {
    attr(summary, "ecx_val") <- effect
  } else {
    attr(summary, "sig_val") <- sig
  }
  if (!is.null(effect_draws)) {
    attr(summary, "ecnsec_relativeP") <- stats::quantile(
      effect_draws, probs = probs, na.rm = TRUE
    )
  }
  summary
}

#' Combine observed-level posteriors across equations of a model set
#' @param object A model set.
#' @param what Estimator name.
#' @param fun Function returning per-level draws for one equation.
#' @param posterior Whether to return draws.
#' @param prob_vals Summary probabilities.
#' @return Per-level model-averaged draws or a table.
#' @noRd
model_set_level_estimates <- function(object, what, fun, posterior, prob_vals) {
  models <- names(object$mod_fits)
  idx <- pull_draw_index(object, models, object$sample_size)
  models <- models[lengths(idx[models]) > 0L]
  components <- once_level_note(lapply(models, function(m) {
    fun(pull_equation(object, m))
  }))
  levels <- names(components[[1]])
  if (is.null(levels) || any(!vapply(components, function(x) {
    identical(names(x), levels)
  }, logical(1)))) {
    stop("The equations do not have the same observed levels.",
         call. = FALSE)
  }
  out <- lapply(levels, function(level) {
    parts <- lapply(seq_along(models), function(i) {
      subset_draws(components[[i]][[level]], idx[[models[i]]])
    })
    draws <- unlist(lapply(parts, as.numeric), use.names = FALSE)
    attr(draws, "censored") <- concat_censoring(
      lapply(parts, attr, "censored"), vapply(parts, length, integer(1)),
      draws
    )
    if (identical(what, "nsec")) {
      attr(draws, "ecnsec_relativeP") <- unlist(lapply(
        seq_along(models), function(i) {
          attr(components[[i]][[level]], "ecnsec_relativeP")[idx[[models[i]]]]
        }
      ), use.names = FALSE)
    }
    draws
  })
  names(out) <- levels
  if (posterior) return(out)
  summaries <- lapply(out, function(draws) {
    ans <- summarise_censored(draws, prob_vals)
    names(ans) <- clean_names(ans)
    ans
  })
  estimate_table(summaries, levels, what)
}
