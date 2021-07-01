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
#' @param loo_controls A named \code{\link[base]{list}} containing the desired
#' arguments to be passed on to \code{\link[loo]{loo_model_weights}}. It can
#' be used to change the default method from "pseudobma". See help
#' documentation ?loo_model_weights from package loo.
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
                          precision = 1000, sig_val = 0.01, priors,
                          pointwise) {
  if (missing(drop) && missing(add) && missing(loo_controls)) {
    message("Nothing to amend, please specify a model to ",
            "either add or drop, or a weighting method via loo_controls;\n",
            "Returning original model set and weights.")
    return(object)
  }
  if (!missing(loo_controls) && !loo_controls %in% c("stacking", "pseudobma")) {
    stop("The weighting method you have supplied is invalid,",
         " it must be one of \"stacking\" or \"pseudobma\".")
  }
  if (missing(drop) && missing(add) && !missing(loo_controls)) {
    if (grepl(loo_controls$method, class(object$mod_stats$wi))) {
      message("Returning original model set.")
      message("Weighting method specified is the same as the original.")
      return(object)
    }
  }
  if (missing(loo_controls)) {
    to_keep <- sapply(c("stacking", "pseudobma"), function(x, object) {
      grepl(x, attributes(object$mod_stats$wi)$class)
    }, object)
    loo_controls <- list(method = c("stacking", "pseudobma")[to_keep])
  }
  model_set <- names(object$mod_fits)
  if (!missing(drop)) {
    model_set <- handle_set(model_set, drop = drop)
  }
  if (!missing(add)) {
    model_set <- handle_set(model_set, add = add)
  }
  if (is.logical(model_set)) {
     message("Returning original model set.")
    if (grepl(loo_controls$method, class(object$mod_stats$wi))) {
     message("Weighting method not modified, please call amend and specify",
             " only loo_controls if you do not need to drop or add any models",
             " and simply want to update the weighting method.")
    }
    return(object)
  }
  simdat <- extract_simdat(object$mod_fits[[1]])
  data <- object$mod_fits[[1]]$fit$data
  family <- object$mod_fits[[1]]$fit$family
  model_set <- check_models(model_set, family)
  fam_tag <- family$family
  link_tag <- family$link
  if (missing(pointwise)) {
    if (fam_tag == "custom") {
      pointwise <- FALSE
    } else {
      pointwise <- TRUE
    }
  } else {
    if (pointwise & fam_tag == "custom") {
      stop("You cannot currently set pointwise = TRUE for custom families.")
    }
  }
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
                     pointwise = pointwise,
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
                           precision = precision, sig_val = sig_val,
                           loo_controls = loo_controls)
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
#' Amends an existing \code{\link{bayesmanecfit}} object, for example, by
#' adding or removing fitted models.
#'
#' @inheritParams amend.default
#'
#' @inherit amend.default return examples
#'
#' @export
amend <- function(object, drop, add, loo_controls, x_range = NA,
                  precision = 1000, sig_val = 0.01,
                  priors, pointwise) {
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
                                precision = 1000, sig_val = 0.01,
                                priors, pointwise) {
  amend.default(object, drop, add, loo_controls, x_range = NA,
                precision = 1000, sig_val = 0.01,
                priors, pointwise)
}
