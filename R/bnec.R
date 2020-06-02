#' bnec
#'
#' Fits a variety of NEC models using Bayesian analysis and provides a model averaged predictions based on WAIC model weights
#'
#' @param data A \code{\link[base]{data.frame}} containing the data to use for the model.
#' @param x_var A \code{\link[base]{character}} indicating the column heading indicating the concentration (x) variable.
#' @param y_var A \code{\link[base]{character}} indicating the column heading indicating the response (y) variable.
#' @param model A \code{\link[base]{character}} vector indicating the model(s) to fit. See Details for more information.
#' @param trials_var The column heading indicating the column for the number of "trials" for binomial response data. 
#' If not supplied, the model may run but will not be the model you intended!
#' @param x_type The statistical distribution of the x (concentration) data. This will be guessed based on 
#' the characteristic of the input data if not supplied and will inform the priors used for estimating nec in NEC step models.
#' @param y_type The statistical distribution (family) to use for the y (response) data.
#' @param x_range A range of x values over which to consider extracting ECx.
#' @param over_disp If an over-dispersed model should be used. Only changes the model fit for "poisson" and "binomial" \code{y_type} 
#' data. For "poisson", a negative binomial model will be fit. For binomial a beta model will be fit.
#' @param precision The length of the x vector used for posterior predictions, and over which to extract ecx values. Large values will be slower but more precise.
#' @param sig_val Probability value to use as the lower quantile to test significance of the predictor posterior values 
#' against the lowest observed concentration (assumed to be the control), to estimate NEC as an interpolated NOEC value from smooth ECX curves.
#' @param iter The number of iterations to be passed to \code{\link[brms]{brm}}. Defaults to 2e3 to be consistent with brms defaults.
#' @param warmup A positive integer specifying number of warmup (a.k.a. burnin) iterations. This also specifies the number 
#' of iterations used for stepsize adaptation, so warmup samples should not be used for inference. The number of warmup 
#' should not be larger than "iter" and the default is "floor(iter / 5) * 4".
#' @param ... Further arguments to \code{\link[brms]{brm}} via \code{\link{fit_bayesnec}}.
#' 
#' @details As some concentration-response data will use zero concentration which can cause numerical estimation issues, a 
#' small offset is added (1 / 10th of the next lowest value) to zero values of concentration where \code{x_var} are distributed 
#' on a continuous scale from 0 to infinity, or are bounded to 0, or 1.
#' 
#' The argument \code{y_type} is a character vector indicating the family to use for the response variable in the brms call, 
#' and may currently be one of "binomial", "beta", "poisson", "negbin","gaussian", or "gamma". 
#' Others can be added as required, please raise an issue on the github development site if your required familiy is not currently available. 
#' If not supplied, the appropriate distribution will be guessed based on the characteristics of the input data through \code{\link{check_data}}.
#' 
#' The argument \code{model} may be one of "nec3param", "nec4param", "necsigm", "nechorme", "ecx4param", "ecxwb1", "ecxwb2", 
#' "ecxexp", "ecxlin", or "excsigm", in which case a single model of the specified type it fit, and bnec returns a model 
#' object of class "bayesnecfit". 
#' 
#' If a vector of two or more of the available models is supplied, bnec returns a model object of class "bayesmanecfit" 
#' containing model averaged predictions for the supplied models, providing they were successfully fitted.
#' 
#' Model averaging is achieved through a weighted sample of each fitted models posterior predictions, with weights derived 
#' using the \code{\link[brms]{loo_model_weights}} from \pkg{brms}. Individual model fits can be extracted from the \code{mod_fits} 
#' element and can be examined individually.
#' 
#' \code{model} may also be one of "all", meaning all of the available models will be fit; 
#' "ecx" meaning only models excluding a specific NEC step parameter fill be fit; "nec" meaning only models with a specific NEC step
#' parameter will be fit; or "bot_free" meaning only models without a "bot" parameter (without a bottom plateau) will be fit.
#' 
#' Models are fit using model formula passed to \pkg{brms} You can view the model formula for each type X_HOW_X. 
#' Other types can be added as required, please open an issue at the package site on GitHub to request an additional model.
#' 
#' All models provide an estimate for NEC. For model types with "nec" as a prefix, NEC is directly estimated as parameter nec
#' in the model. Models with "ecx" as a prefix are continuous curve models, typically used for extracting ECX values 
#' from concentration response data. In this instance the NEC value is defined as the concentration at which there is 
#' a user supplied (see \code{sig_val}) percentage certainty (based on the Bayesian posterior estimate) that the response 
#' falls below the estimated value of the upper asymptote (top) of the response (i.e the response value is significantly 
#' lower than that expected in the case of no exposure). 
#' The default value for sig_val is 0.01, which corresponsed to an alpha value of 0.01 nfor a one-sided test of significance.
#' 
#' @return When only a single model is passed \code{\link{bnec}} returns a list of class bayesnecfit, containing the the fitted bayesian model, 
#' \code{pred_vals} a list of posterior predicted values based on the supplied \code{precision}, 
#' \code{nec} the estimated nec incuding all estimated model paramters, 
#' \code{od} an estimate of dispersion; \code{predicted_y} the predicted values for the observed data;
#' \code{residuals} Residual values of the observed data from the fitted model; and \code{nec_posterior} a full posterior estimate of the nec.
#' Then more than one model is passed, \code{\link{bnec}} returns a list of class bayesnecfit,
#' a table mod_stats a \code{\link[base]{data.frame}} of model fit statistics, nec a model
#' averaged posterior of the estimated nec, and pred.vals a list of model averaged predictions.
#' 
#' @export
bnec <- function(data, x_var, y_var, model = NA, trials_var = NA,
                 x_type = NA, y_type = NA, x_range = NA,
                 over_disp = FALSE, precision = 1000, sig_val = 0.01, iter = 2e3, 
                 warmup = floor(iter / 5) * 4, ...) {
  if (is.na(model)) {
    stop("You need to define a model type. See ?bnec")
  }

  
  if (length(model) == 1 & model %in% names(mod_groups)) {
      model <- mod_groups[[model]]
  }

  if (length(model) > 1) {
    mod_fits <- vector(mode = "list", length = length(model))
    names(mod_fits) <- model
    for (m in seq_along(model)) {
      model_m <- model[m]
      fit_m <- try(
        fit_bayesnec(data = data, x_var = x_var, y_var = y_var,
                     trials_var = trials_var, x_type = x_type, y_type = y_type,
                     iter = iter, over_disp = over_disp, model = model_m,
                     x_range = x_range, precision = precision,
                     open_progress = FALSE, ...),
        silent = TRUE)
      if (!inherits(fit_m, "try-error")) {
        mod_fits[[m]] <- fit_m  
      } else {
        mod_fits[[m]] <- NA 
      }
    }
    export_list <- c(extract_modstats(mod_fits), 
                     list(data = data, x_var = x_var, y_var = y_var,
                          trials_var = trials_var, over_disp = over_disp))
    class(export_list) <- "bayesmanecfit"
  }
  
  if (length(model) == 1) {
    export_list <-  fit_bayesnec(data = data, x_var = x_var, y_var = y_var,
                                 trials_var = trials_var, x_type = x_type,
                                 y_type = y_type, iter = iter, over_disp = over_disp,
                                 model = model, x_range = x_range, precision = precision,
                                 open_progress = FALSE, ...)
  }

  export_list
}
