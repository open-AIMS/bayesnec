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
#' @return For a \code{bayesmanecfit}, the \code{bayesnecformula} stored
#' for the named equation.
#' 
#' @importFrom chk chk_character
#'
#' @export
formula.bayesmanecfit <- function(x, model, ...) {
  selected_model_component(x, model)$bayesnecformula
}

#' Select one stored equation of a model set
#'
#' @param x A \code{bayesmanecfit}.
#' @param model One equation name.
#' @return The stored component for that equation.
#' @noRd
selected_model_component <- function(x, model) {
  if (!is.character(model) || length(model) != 1L || is.na(model) ||
      !nzchar(model) || !model %in% names(x$mod_fits)) {
    stop("model must name one fitted equation: ",
         paste(names(x$mod_fits), collapse = ", "), ".", call. = FALSE)
  }
  x$mod_fits[[model]]
}
