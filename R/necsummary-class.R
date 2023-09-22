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
#' @slot nec_vals The NEC values. Note that if model is an ECx-type model,
#' this estimate will be a NSEC proxy.
#' @slot ecs A \code{\link[base]{list}} containing the ECx values
#' should the user decide to calculate them (see the non-exported
#' \code{bayesnec:::summary.bayesnecfit} help file for details).
#' @slot bayesr2 The model Bayesian R2 as calculated by
#' \code{\link[brms]{bayes_R2}}.
#'
#' @seealso
#'   \code{\link{bayesnec}},
#'   \code{\link{bnec}},
#'   \code{\link{bayesnecfit}},
#'   \code{\link{bayesmanecfit}},
#'   \code{\link{manecsummary}}
#'
#' @references
#' Fisher R, Fox DR (2023). Introducing the no significant effect concentration 
#' (NSEC).Environmental Toxicology and Chemistry, 42(9), 2019–2028. 
#' doi: 10.1002/etc.5610.
#'
#' Fisher R, Fox DR, Negri AP, van Dam J, Flores F, Koppel D (2023). Methods for
#' estimating no-effect toxicity concentrations in ecotoxicology. Integrated 
#' Environmental Assessment and Management. doi:10.1002/ieam.4809.
#' 
#' Fox DR (2010). A Bayesian Approach for Determining the No Effect
#' Concentration and Hazardous Concentration in Ecotoxicology. Ecotoxicology
#' and Environmental Safety, 73(2), 123–131. doi: 10.1016/j.ecoenv.2009.09.012.
#'
NULL

#' Checks if argument is a \code{necsummary} object
#'
#' @param x An \R object
is_necsummary <- function(x) {
  inherits(x, "necsummary")
}
