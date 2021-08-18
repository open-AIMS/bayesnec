#' amend.default
#'
#' Modifies an existing \code{\link{bayesmanecfit}} object, for example, by
#' adding or removing fitted models.
#'
#' @inheritParams bnec
#'
#' @param object An object of class \code{\link{bayesmanecfit}}, as returned
#' by \code{\link{bnec}}.
#' @param drop A \code{\link[base]{character}} vector containing the names of
#' model types you which to drop for the modified fit.
#' @param add A \code{\link[base]{character}} vector containing the names of
#' model types to add to the modified fit.
#'
#' @return All successfully fitted \code{\link{bayesmanecfit}} model fits.
#'
#' @examples
#' library(bayesnec)
#' data(manec_example)
#' exmp <- amend(manec_example, drop = "nec4param")
#'
#' @export
amend.default <- function(object, drop, add, loo_controls, x_range = NA,
                          precision = 1000, sig_val = 0.01, priors) {
  general_error <- paste(
    "Nothing to amend, please specify a proper model to either add or drop, or",
    "changes to loo_controls;\n Returning original model set."
  )
  if (missing(drop) & missing(add) & missing(loo_controls)) {
    message(general_error)
    return(object)
  }
  old_method <- attributes(object$mod_stats$wi)$method
  if (!missing(loo_controls)) {
    fam_tag <- object$mod_fits[[1]]$fit$family$family
    loo_controls <- validate_loo_controls(loo_controls, fam_tag)
    if (!"method" %in% names(loo_controls$weights)) {
      loo_controls$weights$method <- old_method
    }
    is_new_method_old <- loo_controls$weights$method == old_method
    if (length(loo_controls$fitting) == 0 & is_new_method_old) {
      message("No new LOO fitting/weighting arguments have been specified;",
              " ignoring argument loo_controls.")
      if (missing(drop) & missing(add)) {
        message(general_error)
        return(object)
      }
    }
  } else {
    loo_controls <- list(fitting = list(), weights = list(method = old_method))
  }
  model_set <- names(object$mod_fits)
  if (!missing(drop)) {
    model_set <- handle_set(model_set, drop = drop)
  }
  if (!missing(add)) {
    model_set <- handle_set(model_set, add = add)
  }
  if (any(model_set == "wrong_model_output")) {
    message(general_error)
    return(object)
  }
  simdat <- extract_simdat(object$mod_fits[[1]])
  data <- object$mod_fits[[1]]$fit$data
  family <- object$mod_fits[[1]]$fit$family
  model_set <- check_models(model_set, family)
  fam_tag <- family$family
  link_tag <- family$link
  mod_fits <- vector(mode = "list", length = length(model_set))
  names(mod_fits) <- model_set
  for (m in seq_along(model_set)) {
    model <- model_set[m]
    mod_m <- try(object$mod_fits[[model]], silent = TRUE)
    if (!inherits(mod_m, "prebayesnecfit")) {
      fit_m <- try(
        fit_bayesnec(
          data = data, family = family, model = model, skip_check = TRUE,
          iter = simdat$iter, thin = simdat$thin, warmup = simdat$warmup,
          inits = simdat$inits, chains = simdat$chains, priors = priors
        ),
        silent = FALSE
      )
      if (!inherits(fit_m, "try-error")) {
        mod_fits[[model]] <- fit_m
      } else {
        mod_fits[[model]] <- NA
      }
    } else {
      mod_fits[[m]] <- mod_m
    }
  }
  mod_fits <- expand_manec(mod_fits, x_range = x_range, precision = precision,
                           sig_val = sig_val, loo_controls = loo_controls)
  if (length(mod_fits) > 1) {
    allot_class(mod_fits, "bayesmanecfit")
  } else {
    mod_fits <- expand_nec(mod_fits[[1]], x_range = x_range,
                           precision = precision, sig_val = sig_val,
                           loo_controls = loo_controls, model = names(mod_fits))
    allot_class(mod_fits, "bayesnecfit")
  }
}

#' amend
#'
#' Amends an existing \code{\link{bayesmanecfit}} object, for example, by
#' adding or removing fitted models.
#'
#' @inheritParams amend.default
#'
#' @inherit amend.default return examples
#'
#' @export
amend <- function(object, drop, add, loo_controls, x_range = NA,
                  precision = 1000, sig_val = 0.01, priors) {
  UseMethod("amend")
}

#' amend.bayesmanecfit
#'
#' Modifies an existing \code{\link{bayesmanecfit}} object, for example, by
#' adding or removing fitted models.
#'
#' @inheritParams amend
#'
#' @inherit amend.default return examples
#' @export
amend.bayesmanecfit <- function(object, drop, add, loo_controls, x_range = NA,
                                precision = 1000, sig_val = 0.01, priors) {
  amend.default(object, drop, add, loo_controls, x_range = x_range,
                precision = precision, sig_val = sig_val, priors)
}
