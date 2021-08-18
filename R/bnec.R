#' bnec
#'
#' Fits a variety of NEC models using Bayesian analysis and provides a model
#' averaged predictions based on WAIC model weights
#' 
#' @param x A \code{\link[base]{numeric}} vector to use for the x-variable (typically concentration) in 
#' modelling, or a \code{\link[base]{data.frame}} containing both the x and y data.
#' @param y A \code{\link[base]{numeric}} vector to use for the y-variable (typically the response) in 
#' modelling .
#' @param data A \code{\link[base]{data.frame}} containing the data to use for
#' the model.
#' @param x_var A \code{\link[base]{character}} indicating the column heading
#' containing the concentration (x) variable.
#' @param y_var A \code{\link[base]{character}} indicating the column heading
#' containing the response (y) variable.
#' @param model A \code{\link[base]{character}} vector indicating the model(s)
#' to fit. See Details for more information.
#' @param trials_var A \code{\link[base]{character}} indicating the column heading for the number
#' of "trials" for binomial or beta_binomial2 response data, as it appears in "data" (if data is supplied). 
#' Alternatively a \code{\link[base]{numeric}} 
#' vector representing the trials associated with each observation, 
#' if data are supplied as an x argument.
#' or a \code{\link[base]{numeric}} vector indicating the trials
#' If not supplied, the model may run but will not be the model you intended!
#' @param family Either a \code{\link[base]{character}} string, a function, or
#' an object of class \code{\link[stats]{family}} defining the statistical
#' distribution (family) to use for the y (response) data. See details.
#' @param priors An object of class \code{\link[brms]{brmsprior}} which
#' specifies user-desired prior distributions of model parameters.
#' If missing, \code{\link{bnec}} will figure out a baseline prior for each
#' parameter. It can also be specified as a named \code{\link[base]{list}}
#' where each name needs to correspond to the same string as "model". See
#' details.
#' @param x_range A range of x values over which to consider extracting ECx.
#' @param precision The length of the x vector used for posterior predictions,
#' and over which to extract ECx values. Large values will be slower but more
#' precise.
#' @param sig_val Probability value to use as the lower quantile to test
#' significance of the predicted posterior values
#' against the lowest observed concentration (assumed to be the control), to
#' estimate NEC as an interpolated NOEC value from smooth ECx curves.
#' @param iter The number of iterations to be passed to
#' \code{\link[brms]{brm}}. Defaults to 2e3 to be consistent with brms
#' defaults.
#' @param warmup A positive integer specifying number of warmup (a.k.a.
#' burnin) iterations. This also specifies the number of iterations used for
#' stepsize adaptation, so warmup samples should not be used for inference.
#' The number of warmup should not be larger than "iter" and the default is
#' "floor(iter / 5) * 4".
#' @param inits Optional. Initialisation values. Must be a
#' \code{\link[base]{list}} of "n" names lists, where "n" corresponds to the
#' number of chains, and names correspond to the parameter names of a given
#' model.
#' @param sample_prior Indicate if samples from priors should be drawn
#' additionally to the posterior samples. Options are "no", "yes"
#' (the default), and "only". Among others, these samples can be used to
#' calculate Bayes factors for point hypotheses via hypothesis.
#' @param loo_controls A named \code{\link[base]{list}} of two elements
#' ("fitting" and/or "weights"), each being a named \code{\link[base]{list}}
#' containing the desired arguments to be passed on to \code{\link[brms]{loo}}
#' (via "fitting") or to \code{\link[loo]{loo_model_weights}} (via "weights").
#' If "fitting" is provided with argument \code{pointwise = TRUE}
#' (due to memory issues) and \code{family = "beta_binomial2"}, the
#' \code{\link{bnec}} will fail because that is a custom family. If "weights" is
#' not provided by the user, \code{\link{bnec}} will set the default
#' \code{method} argument in \code{\link[loo]{loo_model_weights}} to
#' "pseudobma". See ?\code{\link[loo]{loo_model_weights}} for further info.
#' @param random = A named \code{\link[base]{list}} containing the random model 
#' formula to apply to model parameters.
#' @param random_vars = A \code{\link[base]{character}} vector containing the names of 
#' the columns containing the variables used in the random model formula.
#' @param ... Further arguments to \code{\link[brms]{brm}} via
#' \code{\link{fit_bayesnec}}.
#'
#' @details As some concentration-response data will use zero concentration
#' which can cause numerical estimation issues, a small offset is added (1 /
#' 10th of the next lowest value) to zero values of concentration where
#' \code{x_var} are distributed on a continuous scale from 0 to infinity, or
#' are bounded to 0, or 1.
#'
#' The argument \code{family} indicates the family to use for the response
#' variable in the \code{\link[brms]{brm}} call, and may currently be "Beta" /
#' Beta / Beta(), "binomial" / binomial / binomial(), "beta_binomial2" /
#' beta_binomial2, "Gamma" / Gamma / Gamma(), "gaussian" / gaussian /
#' gaussian(), "negbinomial" / negbinomial / negbinomial(), or "poisson" /
#' poisson / poisson(). Notice that families Beta and negbinomial are exported
#' objects of package \pkg{brms}, so the user needs to load \pkg{brms} before
#' calling these families.
#'
#' Other families can be added as required, please raise an
#' \href{https://github.com/open-AIMS/bayesnec/issues}{issue} on the GitHub
#' development site if your required family is not currently available.
#'
#' If not supplied, the appropriate distribution will be guessed based on the
#' characteristics of the input data through \code{\link{check_data}}. Guesses
#' include all of the above families but "negbinomial" and "betabinomimal2"
#' because these requires knowledge on whether the data is over-dispersed. As
#' explained below in the Return section, the user can extract the dispersion
#' parameter from a \code{\link{bnec}} call, and if they so wish, can refit the
#' model using the "negbinomial" family.
#'
#' The argument \code{model} may be a character string indicating the names of
#' the desired model. see ?models for more details, and the list of models
#' available. If a recongised model name is provided a single model of the
#' specified type is fit, and \code{\link{bnec}} returns a model object of
#' class \code{\link{bayesnecfit}}.
#'
#' If a vector of two or more of the available models is supplied,
#' \code{\link{bnec}} returns a model object of class
#' \code{\link{bayesmanecfit}} containing model averaged predictions for the
#' supplied models, providing they were successfully fitted.
#'
#' Model averaging is achieved through a weighted sample of each fitted models
#' posterior predictions, with weights derived using the
#' \code{\link[brms]{loo_model_weights}} from \pkg{brms}. Individual model
#' fits can be extracted from the \code{mod_fits} element and can be examined
#' individually.
#'
#' \code{model} may also be one of "all", meaning all of the available models
#' will be fit; "ecx" meaning only models excluding a specific NEC step
#' parameter will be fit; "nec" meaning only models with a specific NEC step
#' parameter will be fit; or "bot_free" meaning only models without a "bot"
#' parameter (without a bottom plateau) will be fit. Notice that
#' if one of these group strings is provided together with a user-specified
#' named list for the argument \code{priors}, the list names need to contain
#' the actual model names, and not the group string , e.g. if
#' \code{model = "ecx"} and \code{priors = my_priors} then
#' \code{names(my_priors)} must #' contain \code{models("ecx")}. To check
#' available models and associated parameters for each group,
#' use the function \code{\link{models}} or to check the parameters of a
#' specific model use the function \code{\link{show_params}}.
#'
#' Models are fitted using model formula passed to \pkg{brms}.
#'
#' All models provide an estimate for NEC. For model types with "nec" as a
#' prefix, NEC is directly estimated as parameter "nec"
#' in the model. Models with "ecx" as a prefix are continuous curve models,
#' typically used for extracting ECx values 
#' from concentration response data. In this instance the NEC value is defined
#' as the concentration at which there is 
#' a user supplied (see \code{sig_val}) percentage certainty (based on the
#' Bayesian posterior estimate) that the response
#' falls below the estimated value of the upper asymptote (top) of the
#' response (i.e. the response value is significantly
#' lower than that expected in the case of no exposure).
#' The default value for \code{sig_val} is 0.01, which corresponds to an alpha
#' value of 0.01 for a one-sided test of significance.
#'
#' **NAs are thrown away**
#' Stan's default behaviour is to fail when the input data contains NAs. For
#' that reason \pkg{brms} excludes any NAs from input data prior to fitting,
#' and does not allow them back in as is the case with e.g. \code{stats::lm} and
#' \code{na.action = exclude}. So we advise that you exclude any NAs in your
#' data prior to fitting because if you so wish that should facilitate merging
#' predictions back onto your original dataset.
#'
#' @return If argument model is a single string, then an object of class
#' \code{\link{bayesnecfit}}; if many strings or a set,
#' an object of class \code{\link{bayesmanecfit}}.
#'
#' @examples
#' \donttest{
#' library(brms)
#' library(bayesnec)
#' data(nec_data)
#'
#' # A single model
#' exmp_a <- bnec(data = nec_data, x_var = "x", y_var = "y",
#'                model = "nec4param", chains = 2)
#' # Two models model
#' exmp_b <- bnec(data = nec_data, x_var = "x", y_var = "y",
#'                model = c("nec4param", "ecx4param"), chains = 2)
#' }
#'
#' @export
bnec <- function(x, y = NULL, data, x_var, y_var, model, trials_var = NA,
                 family = NULL, priors, x_range = NA, precision = 1000,
                 sig_val = 0.01, iter = 10e3, warmup = floor(iter / 10) * 9,
                 inits, sample_prior = "yes", loo_controls, random = NA,
                 random_vars = NA, ...) {
  if (!missing(x)) {
    parse_out <- parse_x(x, y, data, x_var, y_var, model, trials_var, family)
    data <- parse_out$data
    x_var <- parse_out$x_var
    y_var <- parse_out$y_var
    trials_var <- parse_out$trials_var
    model <- parse_out$model
  } else {
    if (missing(data) | missing(x_var) | missing(y_var) | missing(model)) {
      stop("You must supply x, or all of data, x_var, y_var and model.",
           " See ?bnec")
    }
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
  if (missing(loo_controls)) {
    loo_controls <- list(fitting = list(), weights = list(method = "pseudobma"))
  } else {
    loo_controls <- validate_loo_controls(loo_controls, fam_tag)
    if (!"method" %in% names(loo_controls$weights)) {
      loo_controls$weights$method <- "pseudobma"
    }
  }
  model <- check_models(model, family)
  if (length(model) > 1) {
    mod_fits <- vector(mode = "list", length = length(model))
    names(mod_fits) <- model
    for (m in seq_along(model)) {
      model_m <- model[m]
      fit_m <- try(
        fit_bayesnec(data = data, x_var = x_var, y_var = y_var, family = family,
                     trials_var = trials_var, priors = priors, model = model_m,
                     iter = iter, warmup = warmup, inits = inits,
                     sample_prior = sample_prior, random = random,
                     random_vars = random_vars, ...),
        silent = FALSE
      )
      if (!inherits(fit_m, "try-error")) {
        mod_fits[[m]] <- fit_m
      } else {
        mod_fits[[m]] <- NA
      }
    }
    mod_fits <- expand_manec(mod_fits, x_range = x_range, precision = precision,
                             sig_val = sig_val, loo_controls = loo_controls)
    if (length(mod_fits) > 1) {
      allot_class(mod_fits, "bayesmanecfit")
    } else {
      mod_fits <- expand_nec(mod_fits[[1]], x_range = x_range,
                             precision = precision, sig_val = sig_val,
                             loo_controls = loo_controls,
                             model = names(mod_fits))
      allot_class(mod_fits, "bayesnecfit")
    }
  } else {
    mod_fit <- fit_bayesnec(data = data, x_var = x_var, y_var = y_var,
                            trials_var = trials_var, family = family,
                            priors = priors, model = model, iter = iter,
                            warmup = warmup, inits = inits,
                            sample_prior = sample_prior, random = random,
                            random_vars = random_vars, ...)
    mod_fit <- expand_nec(mod_fit, x_range = x_range, precision = precision,
                          sig_val = sig_val, loo_controls = loo_controls,
                          model = model)
    allot_class(mod_fit, "bayesnecfit")
  }
}
