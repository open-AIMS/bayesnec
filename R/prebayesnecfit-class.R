#' Class \code{prebayesnecfit} of models fitted with the \pkg{brms} package
#'
#' This is an intermediate class that was created to make both
#' \code{\link{bayesnecfit}} and \code{\link{bayesmanecfit}} objects lighter
#' to handle. It contains the original \code{\link[brms]{brmsfit}}
#' fitted object, name of non-linear model that was fitted, and the list of
#' initialisation values applied.
#'
#' @name prebayesnecfit-class
#' @aliases prebayesnecfit
#' @docType class
#'
#' @details See \code{methods(class = "prebayesnecfit")} for an overview of
#' available methods.
#'
#' @slot fit The fitted Bayesian model of class \code{\link[brms]{brmsfit}}.
#' @slot model A \code{\link[base]{character}} string indicating the name of
#' the fitted model.
#' @slot inits A \code{\link[base]{list}} containing the initialisation values
#' for to fit the model.
#'
#' @seealso
#'   \code{\link{bayesnec}},
#'   \code{\link{bnec}},
#'   \code{\link{bayesnecfit}},
#'   \code{\link{bayesmanecfit}}
#'
NULL

#' Checks if argument is a \code{prebayesnecfit} object
#'
#' @param x An \R object
#' 
#' @return A \code{\link[base]{logical}} vector.
#'
#' @export
is_prebayesnecfit <- function(x) {
  inherits(x, "prebayesnecfit")
}
