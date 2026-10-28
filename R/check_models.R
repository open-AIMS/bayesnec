#' Every equation that can be requested by name
#'
#' The members of \code{mod_groups$all} and the equations in no group, such as
#' \code{ecxflat}. Read from the \code{pred_functions} registry, not from a
#' group: \code{mod_groups$all} is the default set, and an equation can be valid
#' by name without being in it.
#'
#' @return A \code{\link[base]{character}} vector.
#'
#' @noRd
equation_names <- function() {
  names(pred_functions)
}

#' Refuse a name that is no equation
#'
#' Checks against every equation, not \code{mod_groups$all}, since
#' \code{ecxflat} is in no group. \code{constant_fallback()} calls it before it
#' replaces the requested set, so that a misspelt name is refused there too.
#'
#' @param model A \code{\link[base]{character}} vector of equation names.
#'
#' @return \code{NULL}, invisibly. Called for its error.
#'
#' @noRd
check_equation_names <- function(model) {
  if (!all(model %in% equation_names())) {
    to_flag <- paste0(model[!model %in% equation_names()], collapse = "; ")
    stop("The model(s): ", to_flag, "; is not a valid",
         " model entry. Please check ?bnec for valid model calls.")
  }
  invisible(NULL)
}

#' Equations whose mean does not change with the predictor
#'
#' \code{ecxflat}, whose mean is its one curve parameter \code{top}. Named in
#' one place because \code{\link{bnec}}, the refusal of a response at a bound,
#' and the initial-value search each treat it differently.
#'
#' @return A \code{\link[base]{character}} vector.
#'
#' @noRd
constant_equations <- function() {
  "ecxflat"
}

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
#' \code{nechormepwr} and \code{nechorme4pwr} have the hormesis term
#' \code{x^(1 / (1 + exp(slope)))}, which has no coefficient. As \code{slope}
#' grows the term tends to 1 for each positive \code{x}. At a positive
#' \code{x} below \code{nec}, the mean tends to \code{top + 1}, above the upper
#' bound when \code{top} is positive. The term remains zero at \code{x = 0}.
#' The exclusion is conservative and does not inspect the predictor. Bounding
#' \code{nec} below 1 or finding an admissible initial draw does not ensure
#' that the mean stays within (0, 1).
#'
#' @return A \code{\link[base]{character}} vector.
#'
#' @noRd
bounded_power_drops <- function() {
  c("nechormepwr", "nechorme4pwr")
}

#' The message explaining an unscaled-power exclusion
#'
#' Separate from the generic drop message because the reason differs.
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
         " is at least top + 1 at any concentration at or above 1 that",
         " falls below nec, and cannot be held inside (0, 1) there. The prior",
         " on nec places mass across the predictor range, so the sampler is",
         " free to propose such a value. More generally",
         " x^(1 / (1 + exp(slope))) tends to 1",
         " as slope grows, for every concentration above 0, so for any top",
         " above 0 there is a slope at which the mean exceeds 1 whatever the",
         " predictor range. That is why the exclusion does not depend on the",
         " range of yours.",
         " Use nechorme, nechorme4 or nechormepwr01 for a",
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
check_models <- function(model, family, data, record = FALSE) {
  # Record the exclusions only where the caller asks. Callers such as
  # check_model_survival() and get_priors() compare the return with a plain
  # character vector, which an unexpected attribute breaks. bnec() asks, so that
  # the fit records the set requested, the set fitted and the reason.
  excluded <- data.frame(model = character(), reason = character(),
                         stringsAsFactors = FALSE)
  note_drop <- function(dropped, reason) {
    if (length(dropped) > 0) {
      excluded <<- rbind(excluded,
                         data.frame(model = dropped, reason = reason,
                                    stringsAsFactors = FALSE))
    }
    invisible(NULL)
  }
  # amend() passes the family stored in the fit, which records Gamma as gamma.
  fam_tag <- canonical_fam_tag(family)
  link_tag <- family$link
  if (link_tag %in% c("logit", "log")) {
    use_model <-  model[!model %in% mod_groups$zero_bounded]
    drop_model <- setdiff(model, use_model)
    if (length(drop_model) > 0) {
      note_drop(drop_model, paste("zero-bounded, and not valid under a", link_tag, "link"))
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
      note_drop(drop_model, paste("decays by subtraction, so its mean is unbounded below for", fam_tag, "with an identity link"))
      message(paste("Dropping the model(s)",
                    paste0(drop_model, collapse = ", "),
                    "as they are not valid in the case of a",
                    fam_tag, "with identity link."))
    }
    model <- use_model
    use_model <- model[!model %in% bounded_power_drops()]
    drop_model <- setdiff(model, use_model)
    if (length(drop_model) > 0) {
      note_drop(drop_model, paste("unscaled power term, unbounded for", fam_tag))
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
    # A two-block fit must satisfy both blocks' restrictions. The
    # zero-probability block is 0-1 bounded, so the linear-decay models always
    # go. A count or Gamma mu block also rules out nechormepwr01; a
    # zero_inflated_beta mu block does not, since nechormepwr01 is built for
    # that range.
    drop_always <- bounded_linear_drops()
    mu_fam <- unname(hurdle_mu_fams[[fam_tag]])
    if (mu_fam %in% c("Gamma", "poisson", "negbinomial")) {
      drop_always <- c(drop_always, "nechormepwr01")
    }
    use_model <- model[!model %in% drop_always]
    drop_model <- setdiff(model, use_model)
    if (length(drop_model) > 0) {
      note_drop(drop_model, paste("not valid for the second block of a", fam_tag, "fit"))
      message(paste("Dropping the model(s)",
                    paste0(drop_model, collapse = ", "),
                    "as they are not valid in the case of a",
                    fam_tag, "with identity link."))
    }
    model <- use_model
    # The zero-probability block is 0-1 bounded whatever the family, so the
    # unscaled-power hormesis models go too.
    use_model <- model[!model %in% bounded_power_drops()]
    drop_model <- setdiff(model, use_model)
    if (length(drop_model) > 0) {
      note_drop(drop_model, paste("unscaled power term, unbounded for the zero-probability block"))
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
    # The zero-inflated count families belong here, not with the two-block
    # families: brms fits their zero mixture with a constant zi, and their mu
    # is an ordinary count mean.
    use_model <-  model[!model %in% c("neclin", "neclinhorme",
                                      "ecxlin", "nechormepwr01")]
    drop_model <- setdiff(model, use_model)
    if (length(drop_model) > 0) {
      note_drop(drop_model, paste("not valid for", fam_tag, "with an identity link"))
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
  # The zero-bounded equations are not dropped for gaussian: its likelihood
  # never tests the sign of y, so a mean falling to zero with gaussian error is
  # consistent. Model weights can then assess that shape against the data.
  if (!missing(data)) {
    x <- retrieve_var(data, "x_var")
    if (contains_negative(x)) {
      not_allowed <- c("ecxsigm", "nechorme4pwr", "nechormepwr", "necsigm")
      use_models <- setdiff(model, not_allowed)
      drop_models <- setdiff(model, use_models)
      model <- use_models
      if (length(drop_models) > 0) {
        note_drop(drop_models, paste("raises the predictor to a fractional",
                                     "power, which is undefined for negative",
                                     "predictor values"))
        message(
          paste("Dropping the model(s)", paste0(drop_models, collapse = ", "),
                "as they are not valid for data with negative predictor (x)",
                "values.")
        )
      }
      # ecxhormebc5 cannot be reliably initialised on a negative predictor where
      # an identity link must keep the mean positive. A gaussian mean may be
      # negative and a log link keeps it positive, so neither is affected.
      # ecxhormebc4 is kept: it fitted the same cases.
      support <- mu_support(family)
      constrained_identity <- identical(link_tag, "identity") &&
        isTRUE(support[1] == 0)
      if (constrained_identity && "ecxhormebc5" %in% model) {
        model <- setdiff(model, "ecxhormebc5")
        reason <- paste("cannot be reliably initialised for", fam_tag,
                        "with an identity link when the predictor contains",
                        "negative values")
        note_drop("ecxhormebc5", reason)
        message("Dropping the model ecxhormebc5 because it cannot be reliably",
                " initialised for a ", fam_tag, " with identity link when the",
                " predictor (x) contains negative values. Use a scientifically",
                " appropriate non-negative predictor scale or select another",
                " equation.")
      }
    }
  }
  check_equation_names(model)
  if (record) {
    attr(model, "excluded") <- excluded
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
