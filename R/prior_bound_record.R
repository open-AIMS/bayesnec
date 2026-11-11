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

#' A message that a caller can relabel
#' @param text The message text.
#' @return A condition of class \code{bayesnec_prior_bound_note}.
#' @noRd
prior_bound_note <- function(text) {
  structure(class = c("bayesnec_prior_bound_note", "message", "condition"),
            list(message = paste0(text, "\n"), call = NULL))
}
