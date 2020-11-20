#' bnec
#'
#' Fits a variety of NEC models using Bayesian analysis and provides a model averaged predictions based on WAIC model weights
#'
#' @param data A \code{\link[base]{data.frame}} containing the data to use for the model.
#' @param x_var A \code{\link[base]{character}} indicating the column heading containing the concentration (x) variable.
#' @param y_var A \code{\link[base]{character}} indicating the column heading containing the response (y) variable.
#' @param model A \code{\link[base]{character}} vector indicating the model(s) to fit. See Details for more information.
#' @param trials_var The column heading indicating the column for the number of "trials" for binomial or beta_binomial2 response data.
#' If not supplied, the model may run but will not be the model you intended!
#' @param family Either a \code{\link[base]{character}} string, a function, or an object of class \code{\link[stats]{family}} defining the statistical distribution (family)
#' to use for the y (response) data. See details.
#' @param priors An object of class \code{\link[brms]{brmsprior}} which specifies user-desired prior distributions of model parameters.
#' If missing, \code{\link{bnec}} will figure out a baseline prior for each parameter. It can also be specified as a named \code{\link[base]{list}} where each
#' name needs to correspond to the same string as "model". See details.
#' @param x_range A range of x values over which to consider extracting ECx.
#' @param precision The length of the x vector used for posterior predictions, and over which to extract ECx values. Large values will be slower but more precise.
#' @param sig_val Probability value to use as the lower quantile to test significance of the predicted posterior values 
#' against the lowest observed concentration (assumed to be the control), to estimate NEC as an interpolated NOEC value from smooth ECx curves.
#' @param iter The number of iterations to be passed to \code{\link[brms]{brm}}. Defaults to 2e3 to be consistent with brms defaults.
#' @param warmup A positive integer specifying number of warmup (a.k.a. burnin) iterations. This also specifies the number 
#' of iterations used for stepsize adaptation, so warmup samples should not be used for inference. The number of warmup 
#' should not be larger than "iter" and the default is "floor(iter / 5) * 4".
#' @param inits Optional. Initialisation values. Must be a \code{\link[base]{list}} of "n" names lists, where "n" corresponds to the number of chains, a
#' nd names correspond to the parameter names of a given model.
#' @param wi_method A \code{\link[base]{character}} vector containing the desired weighting method to pass to \code{\link{loo_model_weights}}.
#' @param ... Further arguments to \code{\link[brms]{brm}} via \code{\link{fit_bayesnec}}.
#' 
#' @details As some concentration-response data will use zero concentration which can cause numerical estimation issues, a
#' small offset is added (1 / 10th of the next lowest value) to zero values of concentration where \code{x_var} are distributed
#' on a continuous scale from 0 to infinity, or are bounded to 0, or 1.
#' 
#' The argument \code{family} indicates the family to use for the response variable in the \code{\link[brms]{brm}} call,
#' and may currently be "Beta" / Beta / Beta(), "binomial" / binomial / binomial(), "beta_binomial2" / beta_binomial2, "Gamma" / Gamma / Gamma(), "gaussian" / gaussian / gaussian(),
#' "negbinomial" / negbinomial / negbinomial(), or "poisson" / poisson / poisson(). Notice that families Beta and negbinomial are exported objects
#' of package \pkg{brms}, so the user needs to load \pkg{brms} before calling these families.
#' Other families can be added as required, please raise an \href{https://github.com/AIMS/bayesnec/issues}{issue} on the GitHub development site if your 
#' required family is not currently available. 
#' If not supplied, the appropriate distribution will be guessed based on the characteristics of the input data through \code{\link{check_data}}. Guesses
#' include all of the above families but "negbinomial" and "betabinomimal2" because these requires knowledge on whether the data is over-dispersed. As explained below in the 
#' Return section, the user can extract the dispersion parameter from a bnec call, and if they so wish, can refit the model using the "negbinomial" family.
#' 
#' The argument \code{model} may be a character string indicating the names of the desired model. see ?models for more details, and the list of models available. If a recongised model name is provided a single model of the specified type is fit, and \code{\link{bnec}} returns a model object of class \code{\link{bayesnecfit}}.
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
#' the actual model names, and not the group string , e.g. if \code{model = "ecx"} and \code{priors = my_priors} then \code{names(my_priors)} must
#' contain \code{models("ecx")}. To check available models and associated parameters for each group,
#' use the function \code{\link{models}} or to check the parameters of a specific model use the function \code{\link{show_params}}.
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
#' 
#' @examples
#' \dontrun{
#' library(brms)
#' library(bayesnec)
#' options(mc.cores = parallel::detectCores())
#' data(nec_data)
#'
#' # a single model
#' exmp_a <- bnec(data = nec_data, x_var = "x", y_var = "y",
#'                model = "nec3param",
#'                family = Beta(link = "identity"),
#'                iter = 1e4, control = list(adapt_delta = 0.99))
#'
#' class(exmp_a) # bayesnecfit
#'
#' # check fit
#' plot(exmp_a)
#' plot(exmp_a$fit) # plot method from brms
#'
#' # one can specify custom priors too
#' # tweak from bayesnec default
#' pull_prior(exmp_a)
#' my_prior <- c(prior_string("beta(5, 1)", nlpar = "top"),
#'               prior_string("normal(1.3, 2.7)", nlpar = "nec"),
#'               prior_string("gamma(0.5, 2)", nlpar = "beta"))
#'
#' exmp_b <- bnec(data = nec_data, x_var = "x", y_var = "y",
#'                model = "nec3param", priors = my_prior,
#'                family = Beta(link = "identity"),
#'                iter = 1e4, control = list(adapt_delta = 0.99))
#'
#' pull_prior(exmp_b)
#'
#' # multiple models; user-specified priors are not necessary
#' # though we show it here in case this is wanted
#' my_priors <- list(nec3param = c(prior_string("beta(5, 1)", nlpar = "top"),
#'                                 prior_string("normal(1.3, 2.7)", nlpar = "nec"),
#'                                 prior_string("gamma(0.5, 2)", nlpar = "beta")),
#'                   nec4param = c(prior_string("beta(5, 1)", nlpar = "top"),
#'                                 prior_string("normal(1.3, 2.7)", nlpar = "nec"),
#'                                 prior_string("gamma(0.5, 2)", nlpar = "beta"),
#'                                 prior_string("beta(1, 5)", nlpar = "bot")))
#' 
#' exmp_c <- bnec(data = nec_data, x_var = "x", y_var = "y",
#'                model = c("nec3param", "nec4param"),
#'                family = Beta(link = "identity"), priors = my_priors,
#'                iter = 1e4, control = list(adapt_delta = 0.99))
#'
#' pull_prior(exmp_c)
#' class(exmp_c) # bayesmanecfit
#' plot(exmp_c, all_models = FALSE) # model average, default
#' plot(exmp_c, all_models = TRUE) # individual models separately
#' }
#'
#' @export
bnec <- function(data, x_var, y_var, model, trials_var = NA,
                 family = NULL, priors, x_range = NA,
                 precision = 1000, sig_val = 0.01,
                 iter = 2e4, warmup = floor(iter / 5) * 4,
                 inits, wi_method = "stacking", ...) {
  if (missing(model)) {
    stop("You need to define a model type. See ?bnec")
  }
  msets <- names(mod_groups)
  if (any(model %in% msets)) {
      group_mods <- intersect(model, msets)
      model <- union(model, unname(unlist(mod_groups[group_mods])))
      model <- setdiff(model, msets)
  }
  if (is.null(family)) {
    if (is.na(trials_var)) {
      m_trials <- NULL
    } else {
      m_trials <- data[, trials_var]
    }
    family <- set_distribution(data[, y_var], support_integer = TRUE,
                               trials = m_trials)
  }
  family <- validate_family(family)
  fam_tag <- family$family
  link_tag <- family$link

  model <- check_models(model, family)

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
        silent = FALSE)
      if (!inherits(fit_m, "try-error")) {
        mod_fits[[m]] <- fit_m
      } else {
        mod_fits[[m]] <- NA
      }
    }
    mod_fits <- expand_manec(mod_fits, x_range = x_range,
                             precision = precision,
                             sig_val = sig_val, wi_method = wi_method)
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
