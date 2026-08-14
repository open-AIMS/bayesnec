#' Models excluded from 0-1 bounded identity families because they can go
#' negative
#'
#' \code{neclin}, \code{neclinhorme} and \code{ecxlin} decay by subtraction
#' rather than by an exponential factor, so their fitted mean is unbounded
#' below. Named in one place because both the single-block and the two-block
#' branch need the same list.
#'
#' @return A \code{\link[base]{character}} vector.
#'
#' @noRd
bounded_linear_drops <- function() {
  c("neclin", "neclinhorme", "ecxlin")
}

#' Models excluded from 0-1 bounded identity families because they can exceed 1
#'
#' \code{nechormepwr} and \code{nechorme4pwr} carry the hormesis term
#' \code{x^(1 / (1 + exp(slope)))}, which has no coefficient. The exponent lies
#' in (0, 1), so at \code{x = 1} the term contributes exactly 1 whatever
#' \code{slope} is, and below the threshold -- where the decay factor is exactly
#' 1 -- the fitted mean is at least \code{top + 1}. No parameter value keeps that
#' inside (0, 1) for a predictor that reaches 1, which is why the initial-value
#' search cannot be fixed for this combination: there is nothing to find. See
#' #177.
#'
#' @return A \code{\link[base]{character}} vector.
#'
#' @noRd
bounded_power_drops <- function() {
  c("nechormepwr", "nechorme4pwr")
}

#' The message explaining an unscaled-power exclusion
#'
#' Separate from the generic drop message because the reason is different, and
#' because the previous behaviour -- roughly eight minutes of failed
#' initialisation followed by "Initialization failed" buried in a long run --
#' is exactly what makes an explicit reason worth the extra line.
#'
#' @param drop_model The models being dropped.
#' @param fam_tag The family tag.
#'
#' @return A \code{\link[base]{character}} string.
#'
#' @noRd
unscaled_power_message <- function(drop_model, fam_tag) {
  paste0("Dropping the model(s) ", paste0(drop_model, collapse = ", "),
         " as they are not valid in the case of a ", fam_tag,
         " with identity link: their hormesis term",
         " x^(1 / (1 + exp(slope))) has no scale parameter, so the fitted mean",
         " is at least top + 1 wherever the predictor reaches 1 and cannot be",
         " held inside (0, 1). Use nechorme, nechorme4 or nechormepwr01 for a",
         " hormesis model on a bounded response. See ?models.")
}

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
  fam_tag <- family$family
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
    use_model <- model[!model %in% bounded_linear_drops()]
    drop_model <- setdiff(model, use_model)
    if (length(drop_model) > 0) {
      message(paste("Dropping the model(s)",
                    paste0(drop_model, collapse = ", "),
                    "as they are not valid in the case of a",
                    fam_tag, "with identity link."))
    }
    model <- use_model
    use_model <- model[!model %in% bounded_power_drops()]
    drop_model <- setdiff(model, use_model)
    if (length(drop_model) > 0) {
      message(unscaled_power_message(drop_model, fam_tag))
    }
    if (length(use_model) == 0) {
      stop(paste("None of the model(s) specified are valid for a",
                 fam_tag, "with identity link."))
    } else {
      model <- use_model
    }
  }
  if (link_tag == "identity" & is_hurdle_family(fam_tag)) {
    # A two-block fit must satisfy both sets of restrictions at once. The
    # zero-probability block is always 0-1 bounded (as bernoulli/identity), so
    # the linear-decay models go in every case. The mu block depends on the
    # family: zero-bounded for hurdle_gamma, which additionally rules out
    # nechormepwr01; 0-1 bounded for zero_inflated_beta, which does not, since
    # nechormepwr01 is the equation designed for that range.
    drop_always <- bounded_linear_drops()
    mu_fam <- unname(hurdle_mu_fams[[fam_tag]])
    if (mu_fam %in% c("Gamma", "poisson", "negbinomial")) {
      drop_always <- c(drop_always, "nechormepwr01")
    }
    use_model <- model[!model %in% drop_always]
    drop_model <- setdiff(model, use_model)
    if (length(drop_model) > 0) {
      message(paste("Dropping the model(s)",
                    paste0(drop_model, collapse = ", "),
                    "as they are not valid in the case of a",
                    fam_tag, "with identity link."))
    }
    model <- use_model
    # The zero-probability block is 0-1 bounded whatever the response family
    # is, so the unscaled-power hormesis models cannot be used for it either.
    # Without this a `model = "zero_bounded"` call under hurdle_gamma silently
    # averaged over 9 equations rather than 11 -- one dropped by design and one
    # lost to an eight-minute initialisation failure. See #177.
    use_model <- model[!model %in% bounded_power_drops()]
    drop_model <- setdiff(model, use_model)
    if (length(drop_model) > 0) {
      message(unscaled_power_message(drop_model, fam_tag))
    }
    if (length(use_model) == 0) {
      stop(paste("None of the model(s) specified are valid for a",
                 fam_tag, "with identity link."))
    } else {
      model <- use_model
    }
  }
  if (link_tag == "identity" &
        fam_tag %in% c("Gamma", "poisson", "negbinomial",
                       "zero_inflated_poisson",
                       "zero_inflated_negbinomial")) {
    # The zero-inflated count families sit here rather than with the two-block
    # families: their mu block is an ordinary count mean, unbounded above, and
    # the mixture at zero is fitted by brms with a constant zi rather than by a
    # second bayesnec equation. See ?bnec and #104.
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

#' check_model_survival
#'
#' Validates the equation requested for the second (zero-probability) block of
#' a joint hurdle or zero-inflated fit.
#'
#' @inheritParams bnec
#'
#' @param family A \code{\link[stats]{family}} function.
#' @param data A \code{\link[base]{data.frame}}.
#'
#' @details The block is a probability, so it is checked against the
#' restrictions a \code{\link[brms]{bernoulli}} fit with an identity link would
#' face rather than those of the response family. Unlike \code{model}, this is
#' a single equation: model averaging in the joint route runs over the response
#' block, with the second block held fixed. Averaging over both is what
#' \code{\link{bnec_hurdle}} and \code{\link{crossed_weights}} are for.
#'
#' @return A \code{\link[base]{character}} string, or \code{NULL}.
#'
#' @importFrom brms bernoulli
#'
#' @noRd
check_model_survival <- function(model_survival, family, data) {
  if (is.null(model_survival)) {
    return(NULL)
  }
  if (!is_hurdle_family(family)) {
    stop("Argument `model_survival` only applies to the two-block families",
         " \"hurdle_gamma\" and \"zero_inflated_beta\". For two separate fits",
         " with a different model set on each component, see ?bnec_hurdle.",
         call. = FALSE)
  }
  if (!is.character(model_survival) || length(model_survival) != 1) {
    stop("Argument `model_survival` must be a single model name. Model",
         " averaging over both blocks at once means fitting every pair; use",
         " bnec_hurdle() and crossed_weights() for that.", call. = FALSE)
  }
  out <- check_models(model_survival, bernoulli(link = "identity"), data)
  if (length(out) == 0) {
    stop("Model \"", model_survival, "\" is not valid for the survival block,",
         " which is 0-1 bounded.", call. = FALSE)
  }
  out
}
