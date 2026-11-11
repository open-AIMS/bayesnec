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
