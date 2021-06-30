#' Class \code{bayesmanecfit} of models fitted with the \pkg{brms} package
#'
#' Multiple models fitted with the
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}} package are
#' represented as a \code{bayesmanecfit} object, which contains the original
#' \code{\link[brms]{brmsfit}} fitted objects, names of non-linear models that
#' were fitted, model averaging WAIC stats, sample size, mean posterior NEC
#' values, mean model averaged predictions on the data scale, model averaged
#' residuals, #' full posterior distribution of predicated values, and summary
#' statistics of NEC statistics.
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
#' information on the priors used in the model.
#' @slot w_nec_posterior The model-weighted posterior estimate of the NEC.
#' @slot w_predicted_y The model-weighted predicted values for the observed
#' data.
#' @slot w_residuals Model-weighted residual values
#' (i.e. observed - w_predicted_y).
#' @slot w_pred_vals A \code{\link[base]{list}} containing model-weighted
#' posterior predicted values based on the supplied \code{precision} and
#' \code{x_range}.
#' @slot w_nec The summary stats (median and 95% credibility intervals) of
#' w_nec_posterior.
#'
#' @seealso
#'   \code{\link{bayesnec}},
#'   \code{\link{bnec}},
#'   \code{\link{bayesnecfit}}
#'
NULL

#' Checks if argument is a \code{bayesmanecfit} object
#'
#' @param x An \R object
is_bayesmanecfit <- function(x) {
  inherits(x, "bayesmanecfit")
}
