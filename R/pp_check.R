#' Posterior predictive checks for bayesnec fits
#'
#' Dispatches \code{\link[brms]{pp_check}} for \code{\link{bayesnecfit}},
#' \code{\link{bayesmanecfit}} and \code{\link{bayesnechurdlefit}} objects, so a
#' user does not have to unwrap the underlying \code{\link[brms]{brmsfit}}
#' themselves.
#'
#' @name pp_check
#' @order 1
#'
#' @param object An object of class \code{\link{bayesnecfit}},
#' \code{\link{bayesmanecfit}} or \code{\link{bayesnechurdlefit}} as returned by
#' \code{\link{bnec}} or \code{\link{bnec_hurdle}}.
#' @param ... Further arguments passed to \code{\link[brms]{pp_check}}, for
#' example \code{type} and \code{ndraws}.
#'
#' @details \code{pp_check(x, type = "loo_pit_overlay")} gives a LOO-PIT check,
#' the Bayesian counterpart of a uniform quantile residual. It needs a
#' \code{loo} criterion on the fit, which \code{\link{bnec}} already adds, so no
#' extra step is required and no extra dependency is involved.
#'
#' For a \code{\link{bayesmanecfit}} the check is run on each candidate model in
#' turn rather than on the model-averaged posterior. The averaged posterior
#' predictive is a weighted mixture and \pkg{brms} has no object representing
#' it; \code{\link{check_fit}} is the function that reports on the average, and
#' it reports per-model rows alongside for the same reason.
#'
#' For a \code{\link{bayesnechurdlefit}} there is no single posterior predictive
#' to check: the two components carry different response vectors, since growth
#' is fitted on survivors only and survival on every individual. One result is
#' returned per component, following the precedent \code{\link{dispersion}}
#' already sets.
#'
#' @seealso \code{\link{check_fit}}, \code{\link{check_chains}},
#' \code{\link{check_priors}}
#'
#' @return For a \code{\link{bayesnecfit}}, a \code{\link[ggplot2]{ggplot}}
#' object. For the other classes, a named \code{\link[base]{list}} of them.
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' pp_check(nec4param)
#' pp_check(nec4param, type = "loo_pit_overlay")
#' }
#'
#' @importFrom brms pp_check
#'
#' @export
pp_check.bayesnecfit <- function(object, ...) {
  pp_check(pull_brmsfit(object), ...)
}

#' @rdname pp_check
#' @order 2
#'
#' @inherit pp_check description return examples
#'
#' @importFrom brms pp_check
#'
#' @export
pp_check.bayesmanecfit <- function(object, ...) {
  out <- lapply(object$mod_fits, function(x) pp_check(x$fit, ...))
  names(out) <- names(object$mod_fits)
  out
}

#' @rdname pp_check
#' @order 3
#'
#' @inherit pp_check description return examples
#'
#' @importFrom brms pp_check
#'
#' @export
pp_check.bayesnechurdlefit <- function(object, ...) {
  list(growth = pp_check(object$growth, ...),
       survival = pp_check(object$survival, ...))
}
