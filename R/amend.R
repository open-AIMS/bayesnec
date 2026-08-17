#' Amends an existing \code{\link{bayesmanecfit}} or \code{\link{bayesnecfit}}
#' object
#'
#' Amends an existing \code{\link{bayesmanecfit}} or \code{\link{bayesnecfit}}
#' object, for example, by adding or removing fitted models.
#'
#' @inheritParams bnec
#'
#' @param object An object of class \code{\link{bayesmanecfit}} or
#' \code{\link{bayesnecfit}}, as returned by \code{\link{bnec}}.
#' @param drop A \code{\link[base]{character}} vector containing the names of
#' model types you which to exclude for the modified fit. A
#' \code{\link{bayesnecfit}} contains a single model, so there is nothing that
#' can be dropped from it and \code{drop} is an error in that case.
#' @param add A \code{\link[base]{character}} vector containing the names of
#' model types you which to include to the modified fit. Adding models to a
#' \code{\link{bayesnecfit}} promotes it to a \code{\link{bayesmanecfit}}.
#' @param priors An object of class \code{\link[brms]{brmsprior}} which
#' specifies user-desired prior distributions of model parameters.
#' If missing, \code{\link{amend}} will figure out a baseline prior for each
#' parameter. It can also be specified as a named \code{\link[base]{list}}
#' where each name needs to correspond to the same string as \code{model}. See
#' Details.
#' @param prior_type A \code{\link[base]{character}} string selecting the set of
#' default priors to build for any newly added models when \code{priors} is not
#' supplied. Either \code{"uninformative"} (the default) or
#' \code{"regularizing"}. See \code{\link{bnec}}. Note this is not automatically
#' inherited from the original fit; pass it explicitly to match the priors used
#' when the object was first fitted.
#'
#' @return All successfully fitted model fits. A \code{\link{bayesmanecfit}} if
#' more than one model remains, otherwise a \code{\link{bayesnecfit}}.
#'
#' @examples
#' library(bayesnec)
#' data(manec_example)
#' exmp <- amend(manec_example, drop = "nec4param")
#'
#' @export
amend <- function(object, drop, add, loo_controls, x_range = NA,
                  resolution = 1000, sig_val = 0.01, priors,
                  prior_type = "uninformative", timeout = Inf) {
  UseMethod("amend")
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
#' @importFrom chk chk_character chk_numeric chk_number
#'
#' @noRd
#'
#' @export
amend.bayesmanecfit <- function(object, drop, add, loo_controls, x_range = NA,
                                resolution = 1000, sig_val = 0.01, priors,
                                prior_type = "uninformative", timeout = Inf) {
  prior_type <- match.arg(prior_type, c("uninformative", "regularizing"))
  chk_number(timeout)
  if (timeout <= 0) {
    stop("Argument `timeout` must be a positive number (or Inf).")
  }

  if (missing(drop) & missing(add) & missing(loo_controls)) {
    message(amend_general_error())
    return(object)
  }

  if (!missing(drop)) {chk_character(drop)}
  if (!missing(add)) {chk_character(add)}
  if (!is.na(x_range[1])) {chk_numeric(x_range)}
  chk_numeric(resolution)
  chk_numeric(sig_val)
  if(!inherits(object, "bayesmanecfit")){
    stop("object is not of class bayesmanecfit")
  }

  amend_model_set(
    object = object, mod_fits = object$mod_fits,
    old_method = attributes(object$mod_stats$wi)$method,
    drop = if (missing(drop)) NULL else drop,
    add = if (missing(add)) NULL else add,
    loo_controls = if (missing(loo_controls)) NULL else loo_controls,
    x_range = x_range, resolution = resolution, sig_val = sig_val,
    priors = if (missing(priors)) NULL else priors,
    prior_type = prior_type, timeout = timeout
  )
}

#' amend.bayesnecfit
#'
#' Modifies an existing \code{\link{bayesnecfit}} object, for example, by
#' adding further fitted models.
#'
#' @inheritParams amend
#'
#' @inherit amend return
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' data(manec_example)
#' single <- pull_out(manec_example, model = "nec4param")
#' both <- amend(single, add = "ecxexp")
#' }
#'
#' @importFrom chk chk_character chk_numeric chk_number
#'
#' @noRd
#'
#' @export
amend.bayesnecfit <- function(object, drop, add, loo_controls, x_range = NA,
                              resolution = 1000, sig_val = 0.01, priors,
                              prior_type = "uninformative", timeout = Inf) {
  prior_type <- match.arg(prior_type, c("uninformative", "regularizing"))
  chk_number(timeout)
  if (timeout <= 0) {
    stop("Argument `timeout` must be a positive number (or Inf).")
  }
  if (!missing(drop)) {
    chk_character(drop)
    # Erroring rather than silently returning the object: a bayesnecfit holds
    # exactly one model, so honouring `drop` would have to return an empty
    # object, and quietly ignoring it would hide a user mistake.
    stop("Cannot drop models from a bayesnecfit: it contains the single model",
         " \"", object$model, "\". Refit with bnec(..., model = ) to change",
         " which model is fitted.", call. = FALSE)
  }
  if (missing(add) & missing(loo_controls)) {
    message(amend_general_error())
    return(object)
  }
  if (!missing(add)) {chk_character(add)}
  if (!is.na(x_range[1])) {chk_numeric(x_range)}
  chk_numeric(resolution)
  chk_numeric(sig_val)
  # Promote the single fit to a one-element model set and hand it to the same
  # worker the bayesmanecfit method uses, so the two cannot drift apart. This
  # is the reachable-through-amend() version of what `+` and `c()` already do.
  amend_model_set(
    object = object, mod_fits = recover_prebayesnecfit(object),
    old_method = NULL, drop = NULL,
    add = if (missing(add)) NULL else add,
    loo_controls = if (missing(loo_controls)) NULL else loo_controls,
    x_range = x_range, resolution = resolution, sig_val = sig_val,
    priors = if (missing(priors)) NULL else priors,
    prior_type = prior_type, timeout = timeout
  )
}

#' amend_general_error
#'
#' The message returned whenever there is nothing for \code{\link{amend}} to do.
#'
#' @return A \code{\link[base]{character}} string.
#'
#' @noRd
amend_general_error <- function() {
  paste(
    "Nothing to amend, please specify a proper model to either add or drop, or",
    "changes to loo_controls;\n Returning original model set."
  )
}

#' amend_model_set
#'
#' Workhorse behind the \code{\link{amend}} methods. Operates on a named
#' \code{\link[base]{list}} of \code{\link{prebayesnecfit}} objects so that a
#' single-model \code{\link{bayesnecfit}} and a multi-model
#' \code{\link{bayesmanecfit}} share one implementation.
#'
#' @param object The original object, returned unchanged when there is nothing
#' to amend.
#' @param mod_fits A named \code{\link[base]{list}} of
#' \code{\link{prebayesnecfit}} objects.
#' @param old_method The LOO weighting method used by \code{object}, or
#' \code{NULL}.
#'
#' @inheritParams amend
#'
#' @inherit amend return
#'
#' @noRd
amend_model_set <- function(object, mod_fits, old_method, drop = NULL,
                            add = NULL, loo_controls = NULL, x_range = NA,
                            resolution = 1000, sig_val = 0.01, priors = NULL,
                            prior_type = "uninformative", timeout = Inf) {
  general_error <- amend_general_error()
  if (!is.null(loo_controls)) {
    fam_tag <- mod_fits[[1]]$fit$family$family
    loo_controls <- validate_loo_controls(loo_controls, fam_tag)
    if (!"method" %in% names(loo_controls$weights)) {
      loo_controls$weights$method <- old_method
    }
    is_new_method_old <- identical(loo_controls$weights$method, old_method)
    if (length(loo_controls$fitting) == 0 & is_new_method_old) {
      message("No new LOO fitting/weighting arguments have been specified;",
              " ignoring argument loo_controls.")
      if (is.null(drop) & is.null(add)) {
        message(general_error)
        return(object)
      }
    }
  } else {
    # A bayesnecfit carries no weighting method, so leave `weights` empty
    # rather than passing method = NULL down to loo_model_weights().
    old_weights <- if (is.null(old_method)) list() else list(method = old_method)
    loo_controls <- list(fitting = list(), weights = old_weights)
  }
  model_set <- names(mod_fits)
  if (!is.null(drop)) {
    model_set <- handle_set(model_set, drop = drop)
  }
  if (!is.null(add)) {
    model_set <- handle_set(model_set, add = add)
  }
  if (any(model_set == "wrong_model_output")) {
    message(general_error)
    return(object)
  }
  simdat <- extract_simdat(mod_fits[[1]])
  data <- mod_fits[[1]]$fit$data
  family <- mod_fits[[1]]$fit$family
  formula <- mod_fits[[1]]$bayesnecformula
  bdat <- model.frame(formula, data = data)
  model_set <- check_models(model_set, family, bdat)
  old_fits <- mod_fits
  mod_fits <- vector(mode = "list", length = length(model_set))
  names(mod_fits) <- model_set
  failed <- list()
  for (m in seq_along(model_set)) {
    model <- model_set[m]
    mod_m <- try(old_fits[[model]], silent = TRUE)
    if (!inherits(mod_m, "prebayesnecfit")) {
      brm_args <- list(
        family = family, iter = simdat$iter, thin = simdat$thin,
        warmup = simdat$warmup, init = simdat$init, chains = simdat$chains,
        sample_prior = simdat$sample_prior
      )
      brm_args$prior <- priors
      model_priors <- try(validate_priors(brm_args$prior, model),
                          silent = TRUE)
      if (inherits(model_priors, "try-error")) {
        x <- retrieve_var(bdat, "x_var", error = TRUE)
        y <- retrieve_var(bdat, "y_var", error = TRUE)
        custom_name <- check_custom_name(family)
        if (family$family == "binomial" || family$family == "beta_binomial") {
          tr <- retrieve_var(bdat, "trials_var", error = TRUE)
          y <- y / tr
        }
        brm_args$prior <- define_prior(model, family, x, y,
                                       prior_type = prior_type)
      } else {
        brm_args$prior <- model_priors
      }
      fit_m <- try(
        fit_bayesnec(
          formula = formula, data = data, model = model,
          brm_args = brm_args, skip_check = TRUE, prior_type = prior_type,
          timeout = timeout
        ),
        silent = FALSE
      )
      if (!inherits(fit_m, "try-error")) {
        mod_fits[[model]] <- fit_m
      } else {
        mod_fits[[model]] <- NA
        failed[[model]] <- failure_record(model, attr(fit_m, "condition"))
      }
    } else {
      mod_fits[[m]] <- mod_m
    }
  }
  formulas <- lapply(mod_fits, extract_formula)
  mod_fits <- expand_manec(mod_fits, formula = formulas, x_range = x_range,
                           resolution = resolution, sig_val = sig_val,
                           loo_controls = loo_controls)
  if (length(mod_fits) > 1) {
    out <- allot_class(mod_fits, c("bayesmanecfit", "bnecfit"))
  } else {
    mod_fits <- expand_nec(mod_fits[[1]], formula = formula, x_range = x_range,
                           resolution = resolution, sig_val = sig_val,
                           loo_controls = loo_controls, model = names(mod_fits))
    out <- allot_class(mod_fits, c("bayesnecfit", "bnecfit"))
  }
  # Only the models this call attempted. Failures recorded on the object being
  # amended are not carried forward: a model that failed then may have been
  # dropped now, or is being retried here with different priors.
  attach_failed_models(out, failed)
}
