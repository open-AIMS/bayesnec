#' Class \code{bayesmanecfit} of models fitted with the \pkg{brms} package
#'
#' Multiple models fitted with the
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}} package are
#' represented as a \code{bayesmanecfit} object, which contains the original
#' \code{\link[brms]{brmsfit}} fitted objects, names of non-linear models that
#' were fitted, model averaging WAIC stats, sample size, mean posterior
#' no-effect toxicity values (NEC, NSEC or N(S)EC), mean model averaged
#' predictions on the data  scale, model averaged residuals, full posterior
#' distribution of predicated values, and summary statistics of no-effect
#' toxicity.
#'
#' @name bayesmanecfit-class
#' @aliases bayesmanecfit
#' @docType class
#'
#' @details See \code{methods(class = "bayesmanecfit")} for an overview of
#' available methods.
#'
#' @slot mod_fits A \code{\link[base]{list}} of fitted model outputs of class
#' \code{prebayesnecfit} for each of the fitted models.
#' @slot success_models A \code{\link[base]{character}} vector indicating the
#' name of the successfully fitted models.
#' @slot mod_stats A \code{\link[base]{data.frame}} of model fit statistics.
#' @slot sample_size The size of the posterior sample.
#' Information on the priors used in the model.
#' @slot w_ne_posterior The model-weighted posterior estimate of the no-effect
#' toxicity estimate.
#' @slot w_predicted_y The model-weighted predicted values for the observed
#' data.
#' @slot w_residuals Model-weighted residual values
#' (i.e. observed - w_predicted_y).
#' @slot w_pred_vals A \code{\link[base]{list}} containing model-weighted
#' posterior predicted values based on the supplied \code{resolution} and
#' \code{x_range}.
#' @slot w_ne The summary stats (median and 95% credibility intervals) of
#' w_ne_posterior.
#' @slot ne_type A \code{\link[base]{character}} vector indicating the type of
#' no-effect toxicity estimate. Where the fitted model(s) are NEC models
#' (threshold models, containing a step function) the no-effect estimate is
#' a true no-effect-concentration (\code{NEC}, see Fox 2010). Where the fitted
#' model(s) are 
#' smooth ECx models with no step function, the no-effect estimate is a 
#' no-significant-effect-concentration (\code{NSEC}, see Fisher and Fox 2023).
#' In the 
#' case of a \code{\link{bayesmanecfit}} that contains a mixture of both NEC and
#' ECx models, the no-effect estimate is a model averaged combination of the NEC
#' and NSEC estimates, and is reported as the \code{N(S)EC}
#' (see Fisher et al. 2023).
#'
#' @seealso
#'   \code{\link{bayesnec}},
#'   \code{\link{bnec}},
#'   \code{\link{bayesnecfit}}
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

#' Checks if argument is a \code{\link{bayesmanecfit}} object
#'
#' @param x An \R object
#'
#' @noRd
is_bayesmanecfit <- function(x) {
  inherits(x, "bayesmanecfit")
}
