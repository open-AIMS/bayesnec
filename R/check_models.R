#' check_models
#'
#' Check model input for a Bayesian model fit
#'
#' @inheritParams bnec
#'
#' @param family A \code{\link[stats]{family}} function.
#' @param data A \code{\link[base]{data.frame}}.
#'
#' @details This is a wrapper function to test input model criteria and find the
#' correct models for use in \code{\link{fit_bayesnec}}.
#'
#' @return A \code{\link[base]{list}} of modified elements
#' necessary for \code{\link{fit_bayesnec}}.
#'
#' @noRd
check_models <- function(model, family, data) {
  # Custom families all report family$family as "custom", so the tag has to
  # come from family$name for those. Unchanged for the built-in families.
  fam_tag <- family_tag(family)
  link_tag <- family$link
  if (link_tag %in% c("logit", "log")) {
    use_model <-  model[!model %in% mod_groups$zero_bounded]
    drop_model <- setdiff(model, use_model)
    if (length(drop_model) > 0) {
      message(paste("Dropping the model(s)",
                    paste0(drop_model, collapse = ", "),
                    "as they are not valid in the case of a",
                    link_tag, "link."))
    }
    if (length(use_model) == 0) {
      stop(paste("None of the model(s) specified are valid for a",
                 link_tag, "link."))
    } else {
      model <- use_model
    }
  }
  if (link_tag == "identity" & fam_tag %in%
        c("bernoulli", "beta", "binomial", "beta_binomial")) {
    use_model <-  model[!model %in% c("neclin", "neclinhorme", "ecxlin")]
    drop_model <- setdiff(model, use_model)
    if (length(drop_model) > 0) {
      message(paste("Dropping the model(s)",
                    paste0(drop_model, collapse = ", "),
                    "as they are not valid in the case of a",
                    fam_tag, "with identity link."))
    }
    if (length(use_model) == 0) {
      stop(paste("None of the model(s) specified are valid for a",
                 fam_tag, "with identity link."))
    } else {
      model <- use_model
    }
  }
  if (link_tag == "identity" & is_hurdle_family(fam_tag)) {
    # A hurdle fit must satisfy both sets of restrictions at once: its mu block
    # is zero-bounded (as Gamma/identity) and its hu block is 0-1 bounded (as
    # bernoulli/identity). Take the union of what each would drop.
    use_model <- model[!model %in% c("neclin", "neclinhorme", "ecxlin",
                                     "nechormepwr01")]
    drop_model <- setdiff(model, use_model)
    if (length(drop_model) > 0) {
      message(paste("Dropping the model(s)",
                    paste0(drop_model, collapse = ", "),
                    "as they are not valid in the case of a",
                    fam_tag, "with identity link."))
    }
    if (length(use_model) == 0) {
      stop(paste("None of the model(s) specified are valid for a",
                 fam_tag, "with identity link."))
    } else {
      model <- use_model
    }
  }
  # beta_ub sits with the zero-bounded families rather than with Beta. Its
  # response is positive and continuous but not a proportion: the ceiling is a
  # parameter, so nechormepwr01 -- the 0-1 bounded hormesis equation -- is as
  # inappropriate here as it is for a Gamma, and the linear equations are
  # dropped for the same reason as there, since they can take the fitted curve
  # negative.
  if (link_tag == "identity" &
        fam_tag %in% c("Gamma", "poisson", "negbinomial", "beta_ub")) {
    use_model <-  model[!model %in% c("neclin", "neclinhorme",
                                      "ecxlin", "nechormepwr01")]
    drop_model <- setdiff(model, use_model)
    if (length(drop_model) > 0) {
      message(paste("Dropping the model", paste0(drop_model, collapse = ", "),
                    "as they are not valid in the case of a",
                    fam_tag, "with identity link."))
    }
    if (length(use_model) == 0) {
      stop(paste("None of the model(s) specified are valid for a",
                 fam_tag, "with identity link."))
    } else {
      model <- use_model
    }
  }
  if (fam_tag == "gaussian") {
    use_model <-  model[!model %in% mod_groups$zero_bounded]
    drop_model <- setdiff(model, use_model)
    if (length(drop_model) > 0) {
      message(paste("Dropping the model(s)",
                    paste0(drop_model, collapse = ", "),
                    "as they are not valid in the case of Gaussian y data."))
    }
    if (length(use_model) == 0) {
      stop("None of the model(s) specified are valid for Gaussian y data.")
    } else {
      model <- use_model
    }
  }
  if (!missing(data)) {
    x <- retrieve_var(data, "x_var")
    if (contains_negative(x)) {
      not_allowed <- c("ecxsigm", "nechorme4pwr", "nechormepwr", "necsigm")
      use_models <- setdiff(model, not_allowed)
      drop_models <- setdiff(model, use_models)
      model <- use_models
      if (length(drop_models) > 0) {
        message(
          paste("Dropping the model(s)", paste0(drop_models, collapse = ", "),
                "as they are not valid for data with negative predictor (x)",
                "values.")
        )
      }
    }
  }
  if (!all(model %in% mod_groups$all)) {
    to_flag <- paste0(model[!model %in% mod_groups$all], collapse = "; ")
    stop("The model(s): ", to_flag, "; is not a valid",
         " model entry. Please check ?bnec for valid model calls.")
  }
  model
}
