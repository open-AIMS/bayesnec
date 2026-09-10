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
#' 1 -- the fitted mean is at least \code{top + 1}. Wherever a concentration at
#' or above 1 falls strictly below \code{nec} there is therefore a point at
#' which no parameter value keeps the mean inside (0, 1). \code{nec} is
#' truncated to the predictor range, so on a predictor reaching above 1 every
#' such \code{nec} is a value the sampler is free to propose, and each proposal
#' is outside the likelihood's support.
#'
#' \strong{That is the sharpest case, not the whole reason.} It is a statement
#' about a predictor reaching above 1, and the exclusion is unconditional on the
#' data. What justifies that is the exponent. \code{1 / (1 + exp(slope))} tends
#' to 0 as \code{slope} grows, so \code{x^(1 / (1 + exp(slope)))} tends to 1 for
#' \emph{every} \code{x} above 0, however small. Below the threshold the mean is
#' then \code{top + 1} in the limit, so for any \code{top} above 0 there is a
#' \code{slope} at which the mean exceeds 1 --- on any predictor, at any
#' concentration. At \code{x = 0.001}, the smallest case, that slope is 4.90 for
#' \code{top = 0.05}, 2.19 for \code{top = 0.5} and 0.27 for \code{top = 0.95},
#' against a \code{normal(0, 5)} prior on \code{slope}. These are ordinary
#' values, not tail values.
#'
#' This is what \code{\link{mu_support}} records as \code{unscaled_excess}: the
#' mean can exceed 1 through a term with no coefficient, so the fit cannot
#' shrink it. Corroborated by measurement: on a predictor confined below 1,
#' where the \code{top + 1} argument says nothing, 2,899 of 3,591 grid points
#' over \code{top}, \code{slope}, \code{beta} and \code{nec} put the mean above
#' 1, reaching 1.95.
#'
#' It is also why bounding \code{nec} below 1 is not the fix it appears to be.
#' On \code{nec_data}, with \code{nec} below 1, 3,696 of 4,788 grid points put
#' the mean above 1, reaching 3.03: a low \code{nec} is necessary and nowhere
#' near sufficient, and the admissible set is a curved region in \code{top},
#' \code{slope} and \code{nec} jointly rather than a bound on \code{nec}.
#'
#' So the initial-value search finding an admissible draw does not make the
#' model usable. It does now find one, where the gamma prior it replaced did
#' not, because the draws that succeed sit near \code{nec} = 0.08 to 0.24 and
#' the truncated prior probability of reaching there changed by a factor of 40
#' to 900: \code{P(nec < 0.25)} from 0.0063 to 0.269 and \code{P(nec < 0.1)}
#' from 0.00011 to 0.0975. \code{P(nec < 1)} moved only 0.481 to 0.670, which
#' would not explain it. See #177 and #302.
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
         " is at least top + 1 at any concentration at or above 1 that",
         " falls below nec, and cannot be held inside (0, 1) there. nec is",
         " bounded to the predictor range, so the sampler is free to propose",
         " such a value. More generally x^(1 / (1 + exp(slope))) tends to 1",
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
  # The exclusions are recorded as well as messaged, but only when the caller
  # asks. The record is attached as an attribute, and several callers pass this
  # function's return straight on -- check_model_survival(), get_priors(),
  # amend(), check_update_data() -- where an attribute nobody expects makes the
  # value compare unequal to the plain character vector it used to be. bnec()
  # is the only consumer that wants the record, so it is the only one that asks.
  #
  # The exclusions are recorded as well as messaged. bnec() decides which of
  # the requested equations it will not attempt, tells the user once by
  # message(), and used to discard the decision, so the composition of the
  # candidate set could not be recovered from the fit -- only from console
  # output, which a knitted document or a suppressMessages() call does not
  # keep. The set as requested, the set as fitted, and the reason for the
  # difference are what a methods section has to state. See #261.
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
  fam_tag <- family$family
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
      note_drop(drop_model, paste("not valid for the second block of a", fam_tag, "fit"))
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
    # The zero-inflated count families sit here rather than with the two-block
    # families: their mu block is an ordinary count mean, unbounded above, and
    # the mixture at zero is fitted by brms with a constant zi rather than by a
    # second bayesnec equation. See ?bnec and #104.
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
  # The block that stood here dropped every zero-bounded equation --
  # nec3param, ecxexp, ecxsigm, ecxwb1p3, ecxwb2p3, ecxll3 -- whenever the
  # family was gaussian, on the grounds that they "cannot generate predictions
  # of negative response values". That conflates the range of the mean
  # function with the support of the likelihood: a gaussian likelihood
  # evaluates f(y | mu, sigma) and the data enter only through y - mu, so the
  # sign of y is never tested. A mean function asymptoting to zero with
  # gaussian error is internally consistent -- near the asymptote it predicts
  # negative observations at a rate set by mu and sigma, which are ordinary
  # negative residuals.
  #
  # Removed with #206. The exclusion prevented the curve shape OECD TG 201 and
  # Ritz, Gerhard & Streibig (2026) both recommend for algal growth-rate data
  # -- a lower asymptote fixed at zero, representing complete inhibition --
  # from being fitted at all, nec3param, the package's namesake equation,
  # included. #206 measured that these equations fit cleanly under gaussian
  # and that model weights reject them where the shape is wrong, so the
  # candidate set is the right place for that judgement rather than a
  # pre-fit refusal. The separate link exclusion above is unaffected and still
  # applies: it is keyed on a log or logit link, which is a different
  # condition and a correct one.
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
    }
  }
  if (!all(model %in% mod_groups$all)) {
    to_flag <- paste0(model[!model %in% mod_groups$all], collapse = "; ")
    stop("The model(s): ", to_flag, "; is not a valid",
         " model entry. Please check ?bnec for valid model calls.")
  }
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
