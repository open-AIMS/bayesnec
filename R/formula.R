#' Retrieve formulas from models fitted by \code{\link{bnec}}
#'
#' Retrieve formulas from models fitted by \code{\link{bnec}}.
#' \code{x} should be of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#'
#' @name formula
#' @order 1
#'
#' @param x An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#' @param ... Unused.
#'
#' @return An object of class \code{\link{bayesnecformula}}.
#'
#' @examples
#' \donttest{
#' library(bayesnec)
#' formula(manec_example, model = "nec4param")
#' nec4param <- pull_out(manec_example, "nec4param")
#' formula(nec4param)
#' }
NULL

#' @rdname formula
#' @order 2
#'
#' @method formula bayesnecfit
#'
#' @inherit formula description return examples
#'
#' @export
formula.bayesnecfit <- function(x, ...) {
  x$bayesnecformula
}

#' @rdname formula
#' @order 3
#'
#' @param model A valid model string.
#'
#' @method formula bayesmanecfit
#'
#' @inherit formula description return examples
#' 
#' @importFrom chk chk_character
#'
#' @export
formula.bayesmanecfit <- function(x, model, ...) {
  chk_character(model)
  pull_out(x, model) |>
    suppressMessages() |>
    suppressWarnings() |>
    (\(x)x$bayesnecformula)()
}
