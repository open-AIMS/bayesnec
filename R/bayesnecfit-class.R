#' Class \code{bayesnecfit} of models fitted with the \pkg{brms} package
#'
#' Models fitted with the \code{\link[bayesnec:bayesnec-package]{bayesnec}}
#' package are represented as a \code{bayesnecfit} object, which contain the
#' original \code{\link[brms]{brmsfit}} fitted object, list of initialisation
#' values used, the validated \code{\link{bayesnecformula}}, name of non-linear
#' model that was fitted, posterior predictions, posterior parameter estimates
#' and a series of other statistics.
#'
#' @name bayesnecfit-class
#' @aliases bayesnecfit
#' @docType class
#'
#' @details See \code{methods(class = "bayesnecfit")} for an overview of
#' available methods.
#'
#' @slot fit The fitted Bayesian model of class \code{\link[brms]{brmsfit}}.
#' @slot model A \code{\link[base]{character}} string indicating the name of
#' the fitted model.
#' @slot init A \code{\link[base]{list}} containing the initialisation values
#' to fit the model.
#' @slot bayesnecformula An object of class \code{\link{bayesnecformula}} and
#' \code{\link[stats]{formula}}.
#' @slot pred_vals A \code{\link[base]{list}} containing a
#' \code{\link[base]{data.frame}} of summary posterior predicted values
#' and a vector containing based on the supplied \code{resolution} and
#' \code{x_range}.
#' @slot top The estimate for parameter "top" in the fitted model.
#' @slot beta The estimate for parameter "beta" in the fitted model.
#' @slot ne The estimated NEC.
#' @slot f The estimate for parameter "f" in the fitted model, NA if
#' absent for the fitted model type.
#' @slot bot The estimate for parameter "bot" in the fitted model, NA if
#' absent for the fitted model type.
#' @slot d The estimate for parameter "d" in the fitted model, NA if absent
#' for the fitted model type.
#' @slot slope The estimate for parameter "slope" in the fitted model, NA if
#' absent for the fitted model type.
#' @slot ec50 The estimate for parameter "ec50" in the fitted model, NA if
#' absent for the fitted model type.
#' @slot dispersion An estimate of dispersion.
#' @slot predicted_y The predicted values for the observed data.
#' @slot residuals Residual values of the observed data from the fitted model.
#' @slot ne_posterior A full posterior estimate of the NEC.
#' @slot ne_type A \code{\link[base]{character}} vector indicating the type of
#' no-effect toxicity estimate. Where the fitted model is an NEC model
#' (threshold model, containing a step function) the no-effect estimate is
#' a true no-effect-concentration (\code{NEC}, see Fox 2010). Where the fitted
#' model is a smooth ECx model with no step function, the no-effect estimate is
#' a no-significant-effect-concentration (\code{NSEC}, see Fisher and Fox 2023).
#'
#' @seealso
#'   \code{\link{bayesnec}},
#'   \code{\link{bnec}},
#'   \code{\link{bayesmanecfit}},
#'   \code{\link{bayesnecformula}}
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

#' Checks if argument is a \code{\link{bayesnecfit}} object
#'
#' @param x An \R object
#'
#' @noRd
is_bayesnecfit <- function(x) {
  inherits(x, "bayesnecfit")
}
