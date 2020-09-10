#' bnec
#'
#' Fits a variety of NEC models using Bayesian analysis and provides a model averaged predictions based on WAIC model weights
#'
#' @param data A \code{\link[base]{data.frame}} containing the data to use for the model.
#' @param x_var A \code{\link[base]{character}} indicating the column heading containing the concentration (x) variable.
#' @param y_var A \code{\link[base]{character}} indicating the column heading containing the response (y) variable.
#' @param model A \code{\link[base]{character}} vector indicating the model(s) to fit. See Details for more information.
#' @param trials_var The column heading indicating the column for the number of "trials" for binomial response data. 
#' If not supplied, the model may run but will not be the model you intended!
#' @param family Either a \code{\link[base]{character}} string, a function, or an object of class \code{\link[stats]{family}} defining the statistical distribution (family) to use for the y (response) data. See details.
#' @param priors An object of class \code{\link[brms]{brmsprior}} which specifies user-desired prior distributions of model parameters.
#' If missing, \code{\link{bnec}} will figure out a baseline prior for each parameter. It can also be specified as a named \code{\link[base]{list}} where each
#' name needs to correspond to the same string as "model". See details.
#' @param x_range A range of x values over which to consider extracting ECx.
#' @param precision The length of the x vector used for posterior predictions, and over which to extract ECx values. Large values will be slower but more precise.
#' @param sig_val Probability value to use as the lower quantile to test significance of the predictor posterior values 
#' against the lowest observed concentration (assumed to be the control), to estimate NEC as an interpolated NOEC value from smooth ECx curves.
#' @param iter The number of iterations to be passed to \code{\link[brms]{brm}}. Defaults to 2e3 to be consistent with brms defaults.
#' @param warmup A positive integer specifying number of warmup (a.k.a. burnin) iterations. This also specifies the number 
#' of iterations used for stepsize adaptation, so warmup samples should not be used for inference. The number of warmup 
#' should not be larger than "iter" and the default is "floor(iter / 5) * 4".
#' @param inits Optional. Initialisation values. Must be a \code{\link[base]{list}} of "n" names lists, where "n" corresponds to the number of chains, and names correspond to the parameter names of a given model.
#' @param ... Further arguments to \code{\link[brms]{brm}} via \code{\link{fit_bayesnec}}.
#' 
#' @details As some concentration-response data will use zero concentration which can cause numerical estimation issues, a
#' small offset is added (1 / 10th of the next lowest value) to zero values of concentration where \code{x_var} are distributed
#' on a continuous scale from 0 to infinity, or are bounded to 0, or 1.
#' 
#' The argument \code{family} indicates the family to use for the response variable in the \code{\link[brms]{brm}} call,
#' and may currently be "Beta" / Beta / Beta(), "binomial" / binomial / binomial(), "Gamma" / Gamma / Gamma(), "gaussian" / gaussian / gaussian(),
#' "negbinomial" / negbinomial / negbinomial(), or "poisson" / poisson / poisson(). Notice that families Beta and negbinomial are exported objects
#' of package \pkg{brms}, so the user needs to load \pkg{brms} before calling these families.
#' Other families can be added as required, please raise an \href{https://github.com/AIMS/bayesnec/issues}{issue} on the GitHub development site if your 
#' required family is not currently available. 
#' If not supplied, the appropriate distribution will be guessed based on the characteristics of the input data through \code{\link{check_data}}. Guesses
#' include all of the above families but "negbinomial" because this latter requires knowledge on whether the data is over-dispersed. As explained below in the 
#' Return section, the user can extract the dispersion parameter from a bnec call, and if they so wish, can refit the model using the "negbinomial" family.
#' 
#' The argument \code{model} may be one of "nec3param", "nec4param", "necsigm", "nechorme", "ecx4param", "ecxwb1", "ecxwb2", 
#' "ecxexp", "ecxlin", or "excsigm", in which case a single model of the specified type is fit, and \code{\link{bnec}} returns a model 
#' object of class \code{\link{bayesnecfit}}.
#' 
#' If a vector of two or more of the available models is supplied, \code{\link{bnec}} returns a model object of class \code{\link{bayesmanecfit}}
#' containing model averaged predictions for the supplied models, providing they were successfully fitted.
#' 
#' Model averaging is achieved through a weighted sample of each fitted models posterior predictions, with weights derived 
#' using the \code{\link[brms]{loo_model_weights}} from \pkg{brms}. Individual model fits can be extracted from the \code{mod_fits} 
#' element and can be examined individually.
#' 
#' \code{model} may also be one of "all", meaning all of the available models will be fit; 
#' "ecx" meaning only models excluding a specific NEC step parameter fill be fit; "nec" meaning only models with a specific NEC step
#' parameter will be fit; or "bot_free" meaning only models without a "bot" parameter (without a bottom plateau) will be fit. Notice that
#' if one of these group strings is provided together with a user-specified named list for the argument \code{priors}, the list names need to contain
#' the actual model names, and not the group string, e.g. if \code{model = "ecx"} and \code{priors = my_priors} then \code{names(my_priors)} must
#' contain \code{c("ecx4param", "ecxlin", "ecxexp", "ecxsigm", "ecxwb1", "ecxwb2")}. To check available models and associated parameters for each group,
#' use the function \code{\link{show_params}}.
#' 
#' Models are fitted using model formula passed to \pkg{brms}.
#' 
#' All models provide an estimate for NEC. For model types with "nec" as a prefix, NEC is directly estimated as parameter "nec"
#' in the model. Models with "ecx" as a prefix are continuous curve models, typically used for extracting ECx values 
#' from concentration response data. In this instance the NEC value is defined as the concentration at which there is 
#' a user supplied (see \code{sig_val}) percentage certainty (based on the Bayesian posterior estimate) that the response 
#' falls below the estimated value of the upper asymptote (top) of the response (i.e. the response value is significantly 
#' lower than that expected in the case of no exposure). 
#' The default value for \code{sig_val} is 0.01, which corresponds to an alpha value of 0.01 for a one-sided test of significance.
#' 
#' @return If argument model is a single string, then an object of class \code{\link{bayesnecfit}}; if many strings or a set,
#' an object of class \code{\link{bayesmanecfit}}.
#' @export
bnec <- function(data, x_var, y_var, model, trials_var = NA,
                 family = NULL, priors, x_range = NA,
                 precision = 1000, sig_val = 0.01,
                 iter = 2e4, warmup = floor(iter / 5) * 4,
                 inits, ...) {
  if (missing(model)) {
    stop("You need to define a model type. See ?bnec")
  }

  msets <- names(mod_groups)
  if (any(model %in% msets)) {
      group_mods <- intersect(model, msets)
      model <- union(model, unname(unlist(mod_groups[group_mods])))
      model <- setdiff(model, msets)
  }

  if (length(model) > 1) {
    mod_fits <- vector(mode = "list", length = length(model))
    names(mod_fits) <- model
    for (m in seq_along(model)) {
      model_m <- model[m]
      fit_m <- try(
        fit_bayesnec(data = data, x_var = x_var, y_var = y_var,
                     trials_var = trials_var, family = family,
                     priors = priors, model = model_m,
                     iter = iter, warmup = warmup, inits = inits,
                     ...),
        silent = TRUE)
      if (!inherits(fit_m, "try-error")) {
        mod_fits[[m]] <- fit_m
      } else {
        mod_fits[[m]] <- NA
      }
    }
    mod_fits <- expand_manec(mod_fits, x_range = x_range,
                             precision = precision,
                             sig_val = sig_val)
    if (!inherits(mod_fits, "prebayesnecfit")) {
      allot_class(mod_fits, "bayesmanecfit")
    } else {
      mod_fits <- expand_nec(mod_fits, x_range = x_range,
                             precision = precision,
                             sig_val = sig_val)
      allot_class(mod_fits, "bayesnecfit")
    }
  } else {
    mod_fit <- fit_bayesnec(data = data, x_var = x_var, y_var = y_var,
                            trials_var = trials_var, family = family,
                            priors = priors, model = model,
                            iter = iter, warmup = warmup,
                            inits = inits, ...)
    mod_fit <- expand_nec(mod_fit, x_range = x_range,
                          precision = precision,
                          sig_val = sig_val)
    allot_class(mod_fit, "bayesnecfit")
  }
}
