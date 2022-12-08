#' Extract and object of class \code{\link{brmsfit}} from
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#' @param model An optional \code{\link[base]{character}} vector specifying the 
#' model to extract in the case of a \code{\link{bayesmanecfit}}. 
#'
#' @seealso \code{\link{bnec}}
#'
#' @return A plot of the prior and posterior parameter probability densities.
#'
#' @examples
#' library(bayesnec)
#' data(manec_example)
#' brms_fit <- pull_brmsfit(manec_example, model = "nec4param")
#'
#' @export
pull_brmsfit <- function(object, model = NA) {
  UseMethod("pull_brmsfit")
}

#' Plots the prior and posterior parameter probability densities from an
#' object of class \code{\link{bayesnecfit}}.
#'
#' @inheritParams pull_brmsfit
#'
#' @param object An object of class \code{\link{bayesnecfit}} returned by
#' \code{\link{bnec}}.
#'
#' @inherit pull_brmsfit examples return
#'
#' @noRd
#'
#' @export
pull_brmsfit.bayesnecfit <- function(object) {
  object$fit
}

#' Plots the prior and posterior parameter probability densities from an
#' object of class \code{\link{bayesmanecfit}}.
#'
#' @inheritParams pull_brmsfit
#'
#' @param object An object of class \code{\link{bayesmanecfit}} returned by
#' \code{\link{bnec}}.
#'
#' @inherit pull_brmsfit examples return
#'
#' @noRd
#'
#' @export
pull_brmsfit.bayesmanecfit <- function(object, model = NA) {
  models <- names(object$mod_fits)
  if (is.na(model)) {
    stop(paste("Input is a bayesmanecfit, containing the models ",
          paste(models, collapse = ", "), ". 
          You must specify the model to pull.",
          sep=""))
  }
  if(model %in% models == FALSE){
    stop(paste ("Model must be one of ", paste(models, collapse = ", ")))
  }
  object$mod_fits[[model]]$fit

}
