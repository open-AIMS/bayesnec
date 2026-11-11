#' The share of draws above which a prior-bound record is reported
#'
#' A reporting heuristic, not a test of identification.
#'
#' @return A \code{\link[base]{numeric}} value.
#' @noRd
prior_bound_threshold <- function() {
  0.075
}

#' The bounds a fit's stored prior places on one or more parameters
#'
#' Read off the prior stored on the fit rather than recomputed from the data,
#' so that a bound the user supplied is found. Only population-level rows are
#' read: the standard deviation of a group-level term on the same parameter
#' has a lower bound of zero of its own.
#'
#' @param fit A \code{\link[brms]{brmsfit}}.
#' @param nlpars The non-linear parameter names, such as \code{"ec50"} or
#' \code{"huec50"}.
#'
#' @return A \code{\link[base]{list}} with elements \code{lb} and \code{ub},
#' either of which may be \code{NA}, or \code{NULL} where the fit declares
#' none of the parameters.
#' @noRd
prior_par_bounds <- function(fit, nlpars) {
  prior <- fit$prior
  if (is.null(prior) || !all(c("nlpar", "lb", "ub") %in% names(prior))) {
    return(NULL)
  }
  keep <- !is.na(prior$nlpar) & prior$nlpar %in% nlpars
  if ("class" %in% names(prior)) {
    keep <- keep & prior$class == "b"
  }
  rows <- prior[keep, , drop = FALSE]
  if (nrow(rows) == 0) {
    return(NULL)
  }
  # Take the smallest bound over the rows, not the first row. The per-draw
  # minimum of two parameters can reach the smaller of each pair of bounds.
  tightest <- function(values) {
    values <- values[!is.na(values) & nzchar(values)]
    values <- suppressWarnings(as.numeric(values))
    values <- values[!is.na(values)]
    if (length(values) == 0) NA_real_ else min(values)
  }
  list(lb = tightest(rows$lb), ub = tightest(rows$ub))
}

#' Whether the second block of a two-block fit samples a no-effect threshold
#'
#' @param object A \code{\link{bayesnecfit}} or \code{prebayesnecfit}.
#' @param hu_dpar The second block's distributional parameter.
#' @param has_draws Whether the fit holds that block's \code{nec} draws, used
#' where the block's equation cannot be identified.
#'
#' @return A \code{\link[base]{logical}} value.
#' @noRd
hu_block_samples_nec <- function(object, hu_dpar, has_draws) {
  hu_model <- dpar_models(object)[[hu_dpar]]
  if (is.character(hu_model) && length(hu_model) == 1 && !is.na(hu_model)) {
    sampled_nec_is_no_effect(hu_model)
  } else {
    has_draws
  }
}

#' The sampled no-effect draws of one equation of a model set
#'
#' The draws \code{expand_nec()} stores as \code{ne_posterior}, read from the
#' \code{\link[brms]{brmsfit}} without rebuilding the prediction grid and the
#' LOO criteria, which \code{pull_equation()} does.
#'
#' @param pre A \code{prebayesnecfit} from \code{mod_fits}.
#' @param model The equation's name.
#'
#' @return A \code{\link[base]{numeric}} vector on the fitted scale, or
#' \code{NULL} where the equation's no-effect estimate is not a sampled NEC.
#'
#' @importFrom brms as_draws_df
#' @noRd
set_equation_ne_draws <- function(pre, model) {
  fit <- pre$fit
  if (!sampled_nec_is_no_effect(model, fit)) {
    return(NULL)
  }
  hu <- if (is_hurdle_family(fit$family)) hurdle_dpar(fit$family) else NULL
  vars <- intersect(c("b_nec_Intercept", paste0("b_", hu, "nec_Intercept")),
                    brms::variables(fit))
  if (!"b_nec_Intercept" %in% vars) {
    return(NULL)
  }
  draws <- as_draws_df(fit, variable = vars)
  out <- draws[["b_nec_Intercept"]]
  if (!is.null(hu)) {
    hu_out <- draws[[paste0("b_", hu, "nec_Intercept")]]
    if (is.null(hu_out) || !hu_block_samples_nec(pre, hu, TRUE)) {
      return(NULL)
    }
    out <- pmin(out, hu_out)
  }
  as.numeric(out)
}

#' The width of the stored prediction range on the fitted scale
#'
#' @param object A \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @return A \code{\link[base]{numeric}} value, \code{NA} where no grid is
#' stored.
#' @noRd
fitted_grid_span <- function(object) {
  grid <- ne_grid_bounds(object)
  if (is.null(grid) || is.null(ne_grid_formula(object))) {
    return(NA_real_)
  }
  fitted <- suppressWarnings(sub_x_transformation(c(grid$lower, grid$upper),
                                                  ne_grid_formula(object)))
  fitted <- fitted[is.finite(fitted)]
  if (length(fitted) < 2) NA_real_ else diff(range(fitted))
}

#' Draws of one sampled parameter near the bounds of its prior
#'
#' The window is the outer 5% of the prior range on the fitted scale, or of
#' the stored prediction range where only one bound is finite.
#'
#' @param draws Posterior draws on the fitted scale.
#' @param bounds A record from \code{prior_par_bounds()}.
#' @param grid_span A function returning the value of
#' \code{fitted_grid_span()}, called only where one bound is finite.
#' @param map The function the estimate is returned through, from
#' \code{recorded_xform()}.
#'
#' @return A \code{\link[base]{data.frame}} with one row per finite bound and
#' columns \code{side}, \code{bound} (on the scale of the returned estimate),
#' \code{fitted_bound}, \code{n_near}, \code{n_draws} and \code{share}, or
#' \code{NULL}.
#' @noRd
prior_bound_rows <- function(draws, bounds, grid_span, map) {
  if (is.null(draws) || is.null(bounds)) {
    return(NULL)
  }
  ends <- c(lower = bounds$lb, upper = bounds$ub)
  ends <- ends[is.finite(ends)]
  if (length(ends) == 0) {
    return(NULL)
  }
  span <- if (length(ends) == 2) ends[["upper"]] - ends[["lower"]] else
    grid_span()
  if (length(span) != 1 || !is.finite(span) || span <= 0) {
    return(NULL)
  }
  draws <- as.numeric(unlist(draws))
  draws <- draws[is.finite(draws)]
  if (length(draws) == 0) {
    return(NULL)
  }
  window <- 0.05 * span
  rows <- lapply(names(ends), function(side) {
    bound <- ends[[side]]
    inner <- if (side == "upper") bound - window else bound + window
    near <- draws >= min(bound, inner) & draws <= max(bound, inner)
    shown <- tryCatch(suppressWarnings(as.numeric(map(c(bound, inner)))),
                      error = function(e) c(NA_real_, NA_real_))
    # A decreasing map puts the fitted upper bound at the low end of the
    # returned scale, so name the end where it lies on that scale.
    if (length(shown) == 2 && all(is.finite(shown)) && shown[1] != shown[2]) {
      side <- if (shown[1] > shown[2]) "upper" else "lower"
    }
    data.frame(side = side, bound = shown[1], fitted_bound = bound,
               n_near = sum(near), n_draws = length(draws),
               share = mean(near), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Draws of a sampled NEC near a bound of its prior
#'
#' Describes the no-effect estimate \code{\link{nec}} returns, so it is
#' \code{NULL} wherever that estimate is not a sampled NEC: an ECx or hormesis
#' threshold equation, a two-block fit with a smooth block, and a fit read per
#' observed level. For a model set each equation's own draws are read, and
#' \code{attr(, "weighted_share")} holds the model-weighted share at each end.
#'
#' @param object A \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#' @param map The function the estimate is returned through.
#'
#' @return A \code{\link[base]{data.frame}} with columns \code{model},
#' \code{wi}, \code{parameter} and those of \code{prior_bound_rows()}, or
#' \code{NULL}.
#' @noRd
nec_prior_boundary <- function(object, map = identity) {
  set <- inherits(object, "bayesmanecfit")
  if (!set && (!sampled_nec_is_no_effect(object$model, object$fit) ||
               (!is.null(object$hurdle) &&
                !identical(object$hurdle$ne_type, "NEC")))) {
    return(NULL)
  }
  if (isTRUE(object$pred_vals$observed_levels) ||
      isTRUE(object$w_pred_vals$observed_levels) ||
      !is.null(observed_level_spec(object))) {
    return(NULL)
  }
  span <- function() fitted_grid_span(object)
  label <- function(rows, model, wi) {
    if (is.null(rows)) {
      return(NULL)
    }
    data.frame(model = model, wi = wi, parameter = "nec", rows,
               stringsAsFactors = FALSE)
  }
  if (!set) {
    return(label(prior_bound_rows(object$ne_posterior,
                                  ne_prior_bounds(object$fit), span, map),
                 object$model, 1))
  }
  # mod_fits holds prebayesnecfit objects, which store no no-effect posterior.
  models <- object$success_models
  rows <- lapply(models, function(model) {
    pre <- object$mod_fits[[model]]
    if (is.null(pre$fit)) {
      return(NULL)
    }
    prior_bound_rows(set_equation_ne_draws(pre, model),
                     ne_prior_bounds(pre$fit), span, map)
  })
  kept <- !vapply(rows, is.null, logical(1))
  if (!any(kept)) {
    return(NULL)
  }
  wi <- model_weights_for(object, models[kept])
  out <- do.call(rbind, Map(label, rows[kept], models[kept], wi))
  rownames(out) <- NULL
  attr(out, "weighted_share") <- prior_bound_weighted(out)
  out
}

#' The model-weighted share of draws near each end of a prior
#' @param record A record from \code{nec_prior_boundary()}.
#' @return A named \code{\link[base]{numeric}} vector, \code{lower} and
#' \code{upper}.
#' @noRd
prior_bound_weighted <- function(record) {
  weighted <- record$wi * record$share
  vapply(c("lower", "upper"), function(side) {
    sum(weighted[record$side == side])
  }, numeric(1))
}

#' The record a no-effect estimate keeps
#' @param record A record from \code{nec_prior_boundary()}.
#' @return The record where a model-weighted share exceeds the threshold, and
#' otherwise \code{NULL}.
#' @noRd
flagged_prior_boundary <- function(record) {
  if (is.null(record) ||
      !any(prior_bound_weighted(record) > prior_bound_threshold())) {
    return(NULL)
  }
  record
}

#' One row of a prior-bound record as a clause
#' @param row One row of a record.
#' @return A \code{\link[base]{character}} value.
#' @noRd
prior_bound_clause <- function(row) {
  paste0(row$n_near, " of ", row$n_draws, " draws (",
         signif(100 * row$share, 2), "%) within the ", row$side,
         " 5% of its prior range, which ends at ", prior_bound_value(row))
}

#' A bound on the scale it is reported on
#' @param row One row of a record.
#' @return A \code{\link[base]{character}} value.
#' @noRd
prior_bound_value <- function(row) {
  fitted <- signif(row$fitted_bound, 3)
  if (!is.finite(row$bound)) {
    return(paste(fitted, "on the fitted scale"))
  }
  shown <- signif(row$bound, 3)
  if (isTRUE(all.equal(row$bound, row$fitted_bound))) {
    return(as.character(shown))
  }
  paste0(shown, " (", fitted, " on the fitted scale)")
}

#' The note for a no-effect estimate's prior-bound record
#' @param record A record from \code{nec_prior_boundary()}.
#' @param label The equation name, or \code{"model-averaged"}.
#' @return A \code{\link[base]{character}} value, or \code{NULL}.
#' @noRd
format_nec_prior_boundary <- function(record, label) {
  record <- flagged_prior_boundary(record)
  if (is.null(record)) {
    return(NULL)
  }
  limit <- "A credible limit near this bound may be set by the prior."
  if (is.null(attr(record, "weighted_share"))) {
    flagged <- record[record$share > prior_bound_threshold(), , drop = FALSE]
    clauses <- vapply(seq_len(nrow(flagged)), function(i) {
      prior_bound_clause(flagged[i, ])
    }, character(1))
    return(paste0("The ", label, " NEC posterior has ",
                  paste(clauses, collapse = " and "), ". ", limit))
  }
  weighted <- attr(record, "weighted_share")
  sides <- names(weighted)[weighted > prior_bound_threshold()]
  clauses <- vapply(sides, function(side) {
    rows <- record[record$side == side & record$n_near > 0, , drop = FALSE]
    rows <- rows[order(rows$wi * rows$share, decreasing = TRUE), ,
                 drop = FALSE]
    parts <- paste0(rows$model, ": ", signif(100 * rows$share, 2),
                    "% at model weight ", signif(rows$wi, 2))
    paste0(signif(100 * weighted[[side]], 2), "% lie within the ", side,
           " 5% of their prior range (", paste(parts, collapse = "; "), ")")
  }, character(1))
  paste0("Weighted by model weight, of the draws of the sampled NEC ",
         "equations, ", paste(clauses, collapse = " and "), ". ", limit,
         " The prior_bound_summary attribute gives the share for each ",
         "equation.")
}

#' A message that a caller can relabel
#' @param text The message text.
#' @return A condition of class \code{bayesnec_prior_bound_note}.
#' @noRd
prior_bound_note <- function(text) {
  structure(class = c("bayesnec_prior_bound_note", "message", "condition"),
            list(message = paste0(text, "\n"), call = NULL))
}

#' Report a substantial share of NEC draws near a prior bound
#' @inheritParams format_nec_prior_boundary
#' @return \code{NULL}, invisibly.
#' @noRd
report_nec_prior_boundary <- function(record, label) {
  note <- format_nec_prior_boundary(record, label)
  if (!is.null(note)) message(prior_bound_note(note))
  invisible(NULL)
}

#' @noRd
print_nec_prior_boundary <- function(record, label) {
  note <- format_nec_prior_boundary(record, label)
  if (!is.null(note)) cat(note, "\n")
  invisible(NULL)
}

#' Prior-bound rows for the predictor-scale parameters of one block
#'
#' @param fit A \code{\link[brms]{brmsfit}}.
#' @param draws The block's draws-by-parameter matrix, on the fitted scale.
#' @param prefix The block's \pkg{brms} prefix, \code{""} for the mean block.
#' @param model,wi,dpar The equation, its model weight and its block.
#' @param grid_span As for \code{prior_bound_rows()}.
#' @param map The function the parameters are returned through.
#'
#' @return A \code{\link[base]{data.frame}}, or \code{NULL}.
#' @noRd
param_prior_boundary <- function(fit, draws, prefix, model, wi, dpar,
                                 grid_span, map) {
  pars <- intersect(colnames(draws), x_scale_par_names())
  out <- do.call(rbind, lapply(pars, function(p) {
    rows <- prior_bound_rows(draws[, p],
                             prior_par_bounds(fit, paste0(prefix, p)),
                             grid_span, map)
    if (is.null(rows)) {
      return(NULL)
    }
    data.frame(model = unname(model), wi = wi, dpar = dpar, parameter = p,
               rows, stringsAsFactors = FALSE)
  }))
  if (!is.null(out)) rownames(out) <- NULL
  out
}

#' Report the curve parameters with a substantial share near a prior bound
#'
#' The share is multiplied by the equation's model weight, which is 1 for a
#' single fit.
#'
#' @param record The \code{prior_bound_summary} of a
#' \code{\link{curve_params}} table.
#' @param set Whether the table is of a model set, whose weights are named.
#' @return \code{NULL}, invisibly.
#' @noRd
report_param_prior_boundary <- function(record, set = FALSE) {
  if (is.null(record)) {
    return(invisible(NULL))
  }
  # Weight the share, so that an equation the model average barely uses is
  # not named, as nec() weights it.
  flagged <- record[record$wi * record$share > prior_bound_threshold(), ,
                    drop = FALSE]
  if (nrow(flagged) == 0) {
    return(invisible(NULL))
  }
  clauses <- vapply(seq_len(nrow(flagged)), function(i) {
    row <- flagged[i, ]
    block <- if (identical(row$dpar, "mu")) "" else paste0(row$dpar, " block ")
    weight <- if (set) paste0(" (model weight ", signif(row$wi, 2), ")") else
      ""
    paste0("The ", block, row$parameter, " posterior of ", row$model, weight,
           " has ", prior_bound_clause(row), ".")
  }, character(1))
  message(prior_bound_note(paste(
    c(clauses, "A credible limit near such a bound may be set by the prior."),
    collapse = " "
  )))
  invisible(NULL)
}
