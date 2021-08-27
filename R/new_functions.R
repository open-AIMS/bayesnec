#' @noRd
trials <- function(...) {
  identity(...)
}

#' @noRd
expand_model_set <- function(model) {
  msets <- names(mod_groups)
  if (any(model %in% msets)) {
    group_mods <- intersect(model, msets)
    model <- union(model, unname(unlist(mod_groups[group_mods])))
    model <- setdiff(model, msets)
  }
  model
}

#' @noRd
retrieve_valid_family <- function(named_list, data) {
  if (!"family" %in% names(named_list)) {
    y <- retrieve_var(data, "y_var", error = TRUE)
    tr <- retrieve_var(data, "trials_var")
    family <- set_distribution(y, support_integer = TRUE, trials = tr)
  } else {
    family <- named_list$family
  }
  validate_family(family)
}

#' @noRd
define_loo_controls <- function(loo_controls, family_str) {
  if (missing(loo_controls)) {
    loo_controls <- list(fitting = list(), weights = list(method = "pseudobma"))
  } else {
    loo_controls <- validate_loo_controls(loo_controls, family_str)
    if (!"method" %in% names(loo_controls$weights)) {
      loo_controls$weights$method <- "pseudobma"
    }
  }
  loo_controls
}

#' @noRd
retrieve_var <- function(data, var, error = FALSE) {
  bnec_vars <- attr(data, "bnec_pop")
  bnec_pop <- names(bnec_vars)
  v_pos <- which(bnec_pop == var)
  out <- try(data[[v_pos]], silent = TRUE)
  if (inherits(out, "try-error")) {
    if (error) {
      stop("The input variable \"", gsub("_var", "", var),
           "\" was not properly specified in formula. See ?bayesnecformula")
    }
    NULL
  } else if (is.numeric(out)) {
    out
  } else {
    stop("The input variable \"", gsub("_var", "", var),
         " is not numeric.")
  }
}

#' @noRd
add_brm_defaults <- function(brm_args, model, family, predictor, response,
                             skip_check, custom_name) {
  if (!("chains" %in% names(brm_args))) {
    brm_args$chains <- 4
  }
  if (!("sample_prior" %in% names(brm_args))) {
    brm_args$sample_prior <- "yes"
  }
  if (!("iter" %in% names(brm_args))) {
    brm_args$iter <- 1e4
  }
  if (!("warmup" %in% names(brm_args))) {
    brm_args$warmup <- floor(brm_args$iter / 5) * 4
  }
  priors <- try(validate_priors(brm_args$prior, model), silent = TRUE)
  if (inherits(priors, "try-error")) {
    brm_args$prior <- define_prior(model, family, predictor, response)
  }
  if (!("inits" %in% names(brm_args)) || skip_check) {
    msg_tag <- ifelse(family$family == "custom", custom_name, family$family)
    message(paste0("Finding initial values which allow the response to be",
                   " fitted using a ", model, " model and a ", msg_tag,
                   " distribution."))
    response_link <- response_link_scale(response, family)
    inits <- make_good_inits(model, predictor, response_link,
                             priors = brm_args$prior, chains = brm_args$chains)
    if (length(inits) == 1 && "random" %in% names(inits)) {
      inits <- inits$random
    }
    brm_args$inits <- inits
  }
  brm_args
}
