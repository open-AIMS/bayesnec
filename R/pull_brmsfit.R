#' Extract and object of class \code{\link[brms]{brmsfit}} from
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#' @param ... Arguments passed to other methods.
#'
#' @seealso \code{\link{bnec}}
#'
#' @return An object of class \code{\link[brms]{brmsfit}}.
#'
#' @examples
#' library(bayesnec)
#' data(manec_example)
#' brms_fit <- pull_brmsfit(manec_example, model = "nec4param")
#'
#' @export
pull_brmsfit <- function(object, ...) {
  UseMethod("pull_brmsfit")
}

#' @rdname pull_brmsfit
#' @order 2
#'
#' @method pull_brmsfit bayesnecfit
#'
#' @inherit pull_brmsfit examples return
#'
#' @export
pull_brmsfit.bayesnecfit <- function(object, ...) {
  object$fit
}

#' @rdname pull_brmsfit
#' @order 3
#'
#' @param model An optional \code{\link[base]{character}} vector specifying
#' the model to extract.
#'
#' @method pull_brmsfit bayesmanecfit
#'
#' @inherit pull_brmsfit examples return
#'
#' @export
pull_brmsfit.bayesmanecfit <- function(object, model = NA, ...) {
  models <- names(object$mod_fits)
  if (is.na(model)) {
    stop("Input is a bayesmanecfit, containing the models ",
         paste0(models, collapse = ", "), ". You must specify the model",
         " to pull.")
  }
  if (!(model %in% models)) {
    stop("Model must be one of ", paste(models, collapse = ", "))
  }
  object$mod_fits[[model]]$fit
}
