#' Class \code{necsummary} of models fitted with the \pkg{brms} package
#'
#' Single models fitted with the
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}} package are summarised
#' as a \code{necsummary} object, which contains the original
#' \code{\link[brms]{brmsfit}} object summary, the name of the
#' non-linear model fitted, whether this model is an ECx-type model
#' (see details below), and the ECx summary values should the user decide
#' to calculate them.
#'
#' @name necsummary-class
#' @aliases necsummary
#' @docType class
#'
#' @details
#' See \code{methods(class = "necsummary")} for an overview of available
#' methods.
#'
#' @slot brmssummary The standard summary for the fitted Bayesian model of
#' class \code{\link[brms]{brmsfit}}.
#' @slot model A \code{\link[base]{character}} string indicating the name of
#' the fitted non-linear model.
#' @slot is_ecx A \code{\link[base]{logical}} indicating whether \code{model}
#' is an ECx-type model.
#' @slot ecs A \code{\link[base]{list}} containing the ECx values
#' should the user decide to calculate them (see the non-exported
#' \code{bayesnec:::summary.bayesnecfit} help file for details).
#'
#' @seealso
#'   \code{\link{bayesnec}},
#'   \code{\link{bnec}},
#'   \code{\link{bayesnecfit}},
#'   \code{\link{bayesmanecfit}},
#'   \code{\link{manecsummary}}
#'
NULL

#' Checks if argument is a \code{necsummary} object
#'
#' @param x An \R object
is_necsummary <- function(x) {
  inherits(x, "necsummary")
}
