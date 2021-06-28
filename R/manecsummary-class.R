#' Class \code{manecsummary} of models fitted with the \pkg{brms} package
#'
#' Multiple models fitted with the
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}} package are summarised
#' as a \code{manecsummary} object, which contains the name of the
#' non-linear models fitted, the family distribution used
#' to fit all the models, the total post-warm-up sample size, a table
#' containing the model weights, the method to calculate the weights,
#' whether this model is an ECx-type model
#' (see details below), and the ECx summary values should the user decide
#' to calculate them.
#'
#' @name manecsummary-class
#' @aliases manecsummary
#' @docType class
#'
#' @details
#' See \code{methods(class = "manecsummary")} for an overview of available
#' methods.
#' @slot models A \code{\link[base]{character}} string indicating the name of
#' the fitted non-linear models.
#' @slot family The family distribution used to fit all the models.
#' @slot sample_size The total post-warm-up sample size.
#' @slot mod_weights A table containing the model weights.
#' @slot mod_weights_method The method to calculate the weights.
#' @slot ecx_mods A \code{\link[base]{logical}} indicating which \code{models}
#' are ECx-type models.
#' @slot nec_vals The model-averaged NEC values. Note that if model stack
#' contains ECx-type models, these will be via NSEC proxies.
#' @slot ecs A \code{\link[base]{list}} containing the ECx values
#' should the user decide to calculate them (see the non-exported
#' \code{bayesnec:::summary.bayesnecfit} help file for details). Different
#' from the single-model case of class \code{\link{bayesnecfit}}, these ECx
#' estimates will be based on the model weights.
#' @slot rhat_issues A \code{\link[base]{list}} detailing whether each fitted
#' model exhibited convergence issues based on the Rhat evaluation.
#'
#' @seealso
#'   \code{\link{bayesnec}},
#'   \code{\link{bnec}},
#'   \code{\link{bayesnecfit}},
#'   \code{\link{bayesmanecfit}},
#'   \code{\link{necsummary}}
#'
NULL

#' Checks if argument is a \code{manecsummary} object
#'
#' @param x An \R object
#' 
#' @return A \code{\link[base]{logical}}
#'
#' @export
is_manecsummary <- function(x) {
  inherits(x, "manecsummary")
}
