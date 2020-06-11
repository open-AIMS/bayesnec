#' pull_out
#'
#' Pulls a single model from an existing bayesmanecfit object,
#' and converts into a bayesnecfit object.
#'
#' @param manec An object of class bayesmanecfit output list
#' as returned by \code{\link{bnec}}.
#' @param ... Additional arguments to \code{\link{expand_nec}}.
#'
#' @return An object of class bayesnecfit.
#' @export
pull_out <- function(manec, model, ...) {
    mod_fit <- expand_nec(manec$mod_fits[[model]], ...)
    allot_class(mod_fit, "bayesnecfit")
}
