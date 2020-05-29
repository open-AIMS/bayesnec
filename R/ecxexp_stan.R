#' Bayesian regression with Stan
#'
#' @export
#' @param data Numeric vector of input values.
#' @param ... Arguments passed to `brms::brm` (e.g. iter, chains).
#' @return An object of class `brmsfit` returned by `brms::brm`
#' @importFrom stats update
#'
ecxexp_stan <- function(data, ...) {
  ecxexp_brms$fit@stanmodel <- stanmodels$ecxexp
  update(ecxexp_brms, ...)
}
