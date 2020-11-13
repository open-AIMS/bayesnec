#' amend.default
#'
#' Modifies an existing \code{\link{bayesmanecfit}} object, for example, by adding or removing fitted models.
#' 
#' @inheritParams bnec
#' 
#' @param object An object of class \code{\link{bayesmanecfit}}, as returned by \code{\link{bnec}}.
#' @param drop A \code{\link[base]{character}} vector containing the names of model types you which to drop for the modified fit.
#' @param add A \code{\link[base]{character}} vector containing the names of model types to add to the modified fit.
#' @param wi_method A \code{\link[base]{character}} vector containing the desired weighting method to pass to \code{\link{loo_model_weights}}.
#' @return All successfully fitted \code{\link{bayesmanecfit}} model fits.
#'
#' @examples
#' \dontrun{
#' library(brms)
#' library(bayesnec)
#' options(mc.cores = parallel::detectCores())
#' data(nec_data)
#'
#' exmp <- bnec(data = nec_data, x_var = "x", y_var = "y",
#'              model = c("nec3param", "nec4param"),
#'              family = Beta(link = "identity"), priors = my_priors,
#'              iter = 1e4, control = list(adapt_delta = 0.99))
#'
#' # custom priors are not necessary, just added here for example usage
#' ecxlin_priors <- c(prior_string("beta(5, 1)", nlpar = "top"),
#'                    prior_string("gamma(2, 6.5)", nlpar = "slope"))
#' exmp_2 <- amend(exmp, add = "ecxlin", priors = ecxlin_priors)
#' }
#'
#' @export
amend.default <- function(object, drop, add, x_range = NA,
                          precision = 1000, sig_val = 0.01,
                          priors, wi_method = "stacking") {
  if (missing(drop) && missing(add) && missing(wi_method)) {
    message("Nothing to amend, please specify a model to ",
            "either add or drop, or a wi_method;\n",
            "Returning original model set and weights")
    return(object)
  }
  
  if (!missing(wi_method) && !wi_method %in% c("stacking", "pseudobma")) {
    stop("The weighting method you have supplied is invalid, it must be one of 'stacking' or 'pseudobma'")
  }
  
  model_set <- names(object$mod_fits)
  if (!missing(drop)) {
    model_set <- handle_set(model_set, drop = drop)
  }
  if (!missing(add)) {
    model_set <- handle_set(model_set, add = add)
  }
  if (is.logical(model_set)) {
     message("Returning original model set")
   if (!grepl(wi_method, class(object$mod_stats$wi))) {
     message("wi_method not modified, please call amend and specify only wi_method if you do not need to drop or add any models and simply want to update the weighting method.")   
   }
      return(object)
  }
  simdat <- extract_simdat(object$mod_fits[[1]])
  data <- object$mod_fits[[1]]$fit$data
  family <- object$mod_fits[[1]]$fit$family
  mod_fits <- vector(mode = "list", length = length(model_set))
  names(mod_fits) <- model_set

  for (m in seq_along(model_set)) {
    model <- model_set[m]
    mod_m <- try(object$mod_fits[[model]], silent = TRUE)
    if (!inherits(mod_m, "prebayesnecfit")) {
      fit_m <- try(
        fit_bayesnec(data = data,
                     family = family,
                     model = model,
                     skip_check = TRUE,
                     iter = simdat$iter,
                     thin = simdat$thin,
                     warmup = simdat$warmup,
                     inits = simdat$inits,
                     chains = simdat$chains,
                     priors = priors),
        silent = FALSE)
      if (!inherits(fit_m, "try-error")) {
        mod_fits[[model]] <- fit_m
      } else {
        mod_fits[[model]] <- NA
      }
    } else {
      mod_fits[[m]] <- mod_m
    }
  }
  mod_fits <- expand_manec(mod_fits, x_range = x_range,
                           precision = precision, sig_val = sig_val, wi_method = wi_method)
  if (!inherits(mod_fits, "prebayesnecfit")) {
    allot_class(mod_fits, "bayesmanecfit")
  } else {
    mod_fits <- expand_nec(mod_fits, x_range = x_range,
                           precision = precision,
                           sig_val = sig_val)
    allot_class(mod_fits, "bayesnecfit")
  }
}

#' amend
#'
#' Amends an existing \code{\link{bayesmanecfit}} object, for example, by adding or removing fitted models.
#' 
#' @inheritParams amend.default
#' 
#' @param object An object of class \code{\link{bayesmanecfit}}, as returned by \code{\link{bnec}}.
#' 
#' @inherit amend.default return examples
#' 
#' @export
amend <- function(object, drop, add, x_range = NA,
                  precision = 1000, sig_val = 0.01,
                  priors, wi_method = "stacking") {
  UseMethod("amend")
}

#' amend.bayesmanecfit
#'
#' Modifies an existing \code{\link{bayesmanecfit}} object, for example, by adding or removing fitted models.
#'
#' @inheritParams amend
#' 
#' @param ... Additional arguments to \code{\link{amend}}
#' 
#' @inherit amend return examples
#' @export
amend.bayesmanecfit <- function(object, ...) {
  amend.default(object, ...)
}
