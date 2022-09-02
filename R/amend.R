#' Amends an existing \code{\link{bayesmanecfit}} object, for example, by
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
#' @param priors An object of class \code{\link[brms]{brmsprior}} which
#' specifies user-desired prior distributions of model parameters.
#' If missing, \code{\link{amend}} will figure out a baseline prior for each
#' parameter. It can also be specified as a named \code{\link[base]{list}}
#' where each name needs to correspond to the same string as \code{model}. See
#' details.
#'
#' @return All successfully fitted \code{\link{bayesmanecfit}} model fits.
#'
#' @examples
#' library(bayesnec)
#' data(manec_example)
#' exmp <- amend(manec_example, drop = "nec4param")
#'
#' @export
amend <- function(object, drop, add, loo_controls, x_range = NA,
                  precision = 1000, sig_val = 0.01, priors) {
  UseMethod("amend")
}

#' Modifies an existing \code{\link{bayesmanecfit}} object, for example, by
#' adding or removing fitted models.
#'
#' @inheritParams amend
#'
#' @inherit amend return examples
#'
#' @noRd
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
  formula <- object$mod_fits[[1]]$bayesnecformula
  bdat <- model.frame(formula, data = data)
  model_set <- check_models(model_set, family, bdat)
  fam_tag <- family$family
  link_tag <- family$link
  mod_fits <- vector(mode = "list", length = length(model_set))
  names(mod_fits) <- model_set
  for (m in seq_along(model_set)) {
    model <- model_set[m]
    mod_m <- try(object$mod_fits[[model]], silent = TRUE)
    if (!inherits(mod_m, "prebayesnecfit")) {
      brm_args <- list(
        family = family, iter = simdat$iter, thin = simdat$thin,
        warmup = simdat$warmup, init = simdat$init, chains = simdat$chains,
        sample_prior = simdat$sample_prior
      )
      if (missing(priors)) {
        brm_args$prior <- NULL
      } else {
        brm_args$prior <- priors
      }
      priors <- try(validate_priors(brm_args$prior, model), silent = TRUE)
      if (inherits(priors, "try-error")) {
        x <- retrieve_var(bdat, "x_var", error = TRUE)
        y <- retrieve_var(bdat, "y_var", error = TRUE)
        custom_name <- check_custom_name(family)
        if (family$family == "binomial" || custom_name == "beta_binomial2") {
          tr <- retrieve_var(bdat, "trials_var", error = TRUE)
          y <- y / tr
        }
        brm_args$prior <- define_prior(model, family, x, y)
      } else {
        brm_args$prior <- priors
      }
      fit_m <- try(
        fit_bayesnec(
          formula = formula, data = data, model = model,
          brm_args = brm_args, skip_check = TRUE
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
  formulas <- lapply(mod_fits, extract_formula)
  mod_fits <- expand_manec(mod_fits, formula = formulas, x_range = x_range,
                           precision = precision, sig_val = sig_val,
                           loo_controls = loo_controls)
  if (length(mod_fits) > 1) {
    allot_class(mod_fits, c("bayesmanecfit", "bnecfit"))
  } else {
    mod_fits <- expand_nec(mod_fits[[1]], formula = formula, x_range = x_range,
                           precision = precision, sig_val = sig_val,
                           loo_controls = loo_controls, model = names(mod_fits))
    allot_class(mod_fits, c("bayesnecfit", "bnecfit"))
  }
}

#' amend.bayesmanecfit
#'
#' Modifies an existing \code{\link{bayesmanecfit}} object, for example, by
#' adding or removing fitted models.
#'
#' @inheritParams amend
#'
#' @inherit amend return examples
#'
#' @noRd
#'
#' @export
amend.bayesmanecfit <- function(object, drop, add, loo_controls, x_range = NA,
                                precision = 1000, sig_val = 0.01, priors) {
  amend.default(object, drop, add, loo_controls, x_range = x_range,
                precision = precision, sig_val = sig_val, priors)
}
