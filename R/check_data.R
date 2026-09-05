#' on_rational_grid
#'
#' Are all values expressible as k / n for a single small integer n?
#'
#' Genuine proportion data derived from counts (survival, fertilisation,
#' bleaching scores) lies on such a grid; a continuous response divided by a
#' continuous maximum does not. This is the discriminator that makes the
#' divided-by-maximum check usable: without it the check fires on up to 38% of
#' simulated genuine count proportions, purely because one replicate happened
#' to record every individual as alive. With it the false positive rate is zero
#' in both the simulation and the real-data sweep (see
#' notes/normalisation_detection.md).
#'
#' The cost is that a count proportion that really was divided by its own
#' maximum stays on a rational grid and is therefore missed. That is the safe
#' direction to fail for a diagnostic message.
#'
#' @noRd
on_rational_grid <- function(y, max_n = 100, tol = 1e-8) {
  y <- y[is.finite(y)]
  if (!length(y)) {
    return(FALSE)
  }
  for (n in seq_len(max_n)) {
    if (all(abs(y * n - round(y * n)) < tol)) {
      return(TRUE)
    }
  }
  FALSE
}

#' check_normalisation
#'
#' Detect a response that has been normalised to a quantity estimated from the
#' dataset being analysed, and say why that is a problem.
#'
#' Two practices leave an exact arithmetic trace:
#'
#' A. divided by the observed maximum -- the maximum is exactly 1 and exactly
#'    one observation attains it, because \code{v / max(v)} is exactly 1 in
#'    floating point at the maximum and nowhere else.
#' B. divided by the control mean -- the mean of the observations at the lowest
#'    predictor value is exactly 1.
#'
#' Messages rather than warnings: neither is fatal, both are recoverable by
#' refitting the raw response, and the user may have a reason. They are emitted
#' from \code{\link{bnec}} rather than \code{check_data} so that they fire once
#' per call rather than once per model in a set.
#'
#' @noRd
check_normalisation <- function(data) {
  y <- try(retrieve_var(data, "y_var", error = TRUE), silent = TRUE)
  x <- try(retrieve_var(data, "x_var", error = TRUE), silent = TRUE)
  if (inherits(y, "try-error") || inherits(x, "try-error")) {
    return(invisible(NULL))
  }
  ok <- is.finite(y) & is.finite(x)
  y <- y[ok]
  x <- x[ok]
  if (length(y) < 5) {
    return(invisible(NULL))
  }
  cite <- paste0("See Ritz et al. (2026) doi:10.1007/s10651-025-00698-y, and",
                 " ?ecx for what to do instead.")
  if (max(y) == 1 && sum(y == 1) == 1 && !on_rational_grid(y)) {
    message("Your response has a maximum of exactly 1 attained by exactly one",
            " observation, which is the signature of a response divided by its",
            " own observed maximum. Dividing by an extreme order statistic",
            " correlates every observation, discards the uncertainty in the",
            " divisor and biases ECx estimates upwards; it also forces one",
            " observation outside the open support of the Beta family. Prefer",
            " fitting the raw response and reading effective concentrations",
            " off the fitted curve with ecx(type = \"absolute\"), which is",
            " already the default. ", cite)
  }
  ctl <- y[x == min(x)]
  if (length(ctl) >= 3 && !all(ctl == ctl[1]) && abs(mean(ctl) - 1) < 1e-8) {
    message("The observations at your lowest predictor value average to",
            " exactly 1, which is the signature of a response divided by the",
            " control mean. Dividing every observation by the same estimated",
            " quantity discards the uncertainty in the control level, so ECx",
            " is biased upwards and both ECx and NSEC intervals are narrower",
            " than the data support. Nothing is lost by not normalising: the",
            " concentration giving an x percent rise in inhibition is the",
            " concentration giving an x percent decline in the raw response,",
            " which ecx(type = \"absolute\") returns by default. ", cite)
  }
  invisible(NULL)
}

#' check_data
#'
#' Check data input for a Bayesian NEC model fit
#'
#' @inheritParams bnec
#'
#' @param family A \code{\link[stats]{family}} function.
#'
#' @details This is a wrapper function to test input data criteria and find the
#' correct priors for use in \code{\link{fit_bayesnec}}.
#'
#' @return A \code{\link[base]{list}} of modified elements
#' necessary for \code{\link{fit_bayesnec}}.
#'
#' @noRd
check_data <- function(data, family, model) {
  y <- retrieve_var(data, "y_var", error = TRUE)
  x <- retrieve_var(data, "x_var", error = TRUE)
  bnec_pop_vars <- attr(data, "bnec_pop")
  y_pos <- which(names(bnec_pop_vars) == "y_var")
  x_pos <- which(names(bnec_pop_vars) == "x_var")
  # model.frame() removes incomplete cases before check_data() is given the
  # data, so an NA or NaN never reached the finiteness guard below and the fit
  # ran on fewer rows than were supplied with nothing said. Refused rather than
  # announced: Inf is refused here already, and the sample the estimates are
  # derived from is not something the package should change silently. The rows
  # removed are recorded on the model frame by na.omit(), which is the only
  # remaining evidence that they existed. See #278.
  dropped <- attr(data, "na.action")
  if (!is.null(dropped)) {
    stop("Your data contains ", length(dropped), " row(s) with missing values",
         " (NA or NaN), at row(s) ", paste0(unname(dropped), collapse = ", "),
         ". The function bnec requires complete cases; remove or impute those",
         " rows before fitting.")
  }
  # is.finite() elementwise rather than on the mean, so that a column reaching
  # this point with NA still present -- a user with options(na.action =
  # "na.pass"), for which the guard above sees nothing -- is named as well.
  if (!all(is.finite(x))) {
    stop("Your predictor column contains values that are not finite.")
  }
  if (!all(is.finite(y))) {
    stop("Your response column contains values that are not finite.")
  }
  resp_check <- mean(y[which(x < mean(x))]) <
    mean(y[which(x > mean(x))])
  if (resp_check && !grepl("horme", model)) {
    warning("The mean value of the response column for the lower half of the ",
            "predictor column are lower than that of the upper half ",
            "of the predictor column. bnec only allows for ",
            "response values to decline with increasing values of predictor.")
  }
  fam_tag <- family$family
  # A censored response states that the truth lies in an interval whose bound is
  # the recorded value. The boundary nudges below assert a point instead, so
  # where the two meet the nudge would silently move the bound the user declared.
  # NULL when the formula carried no cens() term, in which case nothing changes.
  cens <- retrieve_cens(data)
  # Called for its errors, not its value: nothing here needs to know which
  # distribution describes the predictor, but this call rejects an integer one,
  # which reaches this point unchallenged because is.numeric() is TRUE for an
  # integer and retrieve_var() preserves the type. define_prior() makes the same
  # call and would raise the same error, so deleting this one would relocate the
  # error rather than remove it -- but it belongs here, in the documented data
  # check, rather than inside prior construction. See #269.
  set_distribution(x, silence_y_msgs = TRUE, silence_x_msgs = FALSE)
  # Families whose support is open at a boundary cannot express a censored
  # observation sitting exactly on it: the censored likelihood contribution is
  # F(0) = 0 on the left and 1 - F(1) = 0 on the right, so Stan sees log(0) and
  # initialisation fails with nothing informative to point at. Catch it here,
  # where the remedy can be named. Checked before the nudges below, so the
  # diagnostic does not depend on whether a nudge would have fired.
  if (fam_tag %in% c("Gamma", "hurdle_gamma", "beta", "zero_inflated_beta")) {
    check_cens_support(y, cens, bound = 0, direction = -1L, fam_tag = fam_tag)
  }
  if (fam_tag %in% c("beta", "zero_inflated_beta")) {
    check_cens_support(y, cens, bound = 1, direction = 1L, fam_tag = fam_tag)
  }
  # Runs here as a backstop for get_priors() and for a direct fit_bayesnec()
  # call. bnec() runs it once for the whole call, before the model loop, so that
  # a model set stops once rather than repeating the message per model.
  check_inline_boundary(data, family)
  # The corrections below concern the response alone. No family constrains the
  # values a predictor may take, so a zero concentration is an ordinary control
  # and reaches brm() as recorded; the predictor corrections that used to sit
  # here were written by symmetry with these rather than for a reason of their
  # own. See #269.

  # Which zeros are a boundary artefact to be nudged. A censored row is exempt
  # for the same reason a hurdle zero is: the value there is a declared bound,
  # not an artefact, and moving it would restate the bound the user chose.
  to_shift <- y == 0 & !is_censored(cens)
  # NB: this nudge must never apply to a hurdle family. There the zeros are the
  # hurdle signal, not a boundary problem -- moving them off zero would leave
  # the hu block with nothing to identify itself from. fam_tag is
  # "hurdle_gamma" rather than "Gamma" in that case, so the condition below
  # already excludes it; the guard is explicit so it survives refactoring.
  if (any(to_shift) & fam_tag == "Gamma" & !is_hurdle_family(fam_tag)) {
    min_val <- min(y[y > 0])
    data[to_shift, y_pos] <- y[to_shift] + (min_val / 10)
    message("Your response contains zeros, which a Gamma distribution cannot",
            " represent. They have been shifted to ", signif(min_val / 10, 3),
            " (one tenth of the smallest non-zero value). If those zeros are",
            " meaningful -- for example individuals that died -- consider",
            " family = hurdle_gamma() instead, which models them explicitly.")
  }
  if (any(to_shift) & fam_tag == "beta") {
    min_val <- min(y[y > 0])
    data[to_shift, y_pos] <- y[to_shift] + (min_val / 10)
  }
  # A zero-inflated Beta keeps its zeros -- they are the signal -- but ones are
  # still outside Beta's open (0, 1) support and must be nudged as usual. A
  # right-censored one is exempt, as a left-censored zero is above.
  to_drop <- y == 1 & !is_censored(cens)
  if (any(to_drop) & (fam_tag == "beta" || fam_tag == "zero_inflated_beta")) {
    data[to_drop, y_pos] <- y[to_drop] - 0.001
  }
  mod_dat <- data.frame(x = data[[x_pos]], y = data[[y_pos]],
                        trials = nrow(data))
  if (fam_tag == "binomial" || fam_tag == "beta_binomial") {
    mod_dat$trials <- retrieve_var(data, "trials_var", error = TRUE)
  }
  rate_var <- retrieve_var(data, "rate_var")
  if (!is.null(rate_var)) {
    # Named `denom` rather than `rate`: it is the denominator of the rate, not
    # the rate itself, and the response divided by it is what the priors below
    # are built from.
    mod_dat$denom <- rate_var
  }
  list(mod_dat = mod_dat, family = family)
}

#' Refuse a formula whose transformed response sits on a boundary its family
#' excludes
#'
#' A property of the data and the formula together, fixed for a whole
#' \code{\link{bnec}} call, so it is checked once there rather than once per
#' model. \code{\link{check_normalisation}} is hoisted out of
#' \code{\link{check_data}} for the same reason: a model set would otherwise
#' repeat the diagnostic for each of its members and end on the generic
#' all-models-failed advice, long after the cause.
#'
#' The conditions mirror the corrections in \code{\link{check_data}} exactly,
#' censoring exemptions included, because a value this refuses is precisely one
#' that would otherwise have been corrected.
#'
#' @noRd
check_inline_boundary <- function(data, family) {
  if (!pop_var_is_transformed(data, "y_var")) {
    return(invisible(NULL))
  }
  y <- try(retrieve_var(data, "y_var", error = TRUE), silent = TRUE)
  if (inherits(y, "try-error")) {
    return(invisible(NULL))
  }
  fam_tag <- if (inherits(family, "family")) family$family else family
  bnec_pop_vars <- attr(data, "bnec_pop")
  expr <- names(data)[which(names(bnec_pop_vars) == "y_var")]
  cens <- retrieve_cens(data)
  at_zero <- any(y == 0 & !is_censored(cens))
  at_one <- any(y == 1 & !is_censored(cens))
  if (at_zero & fam_tag == "Gamma" & !is_hurdle_family(fam_tag)) {
    stop_inline_boundary(expr, fam_tag, bound = 0,
                         hint = paste0(" If those zeros are meaningful -- for",
                                       " example individuals that died --",
                                       " consider family = hurdle_gamma()",
                                       " instead, which models them",
                                       " explicitly."))
  }
  if (at_zero & fam_tag == "beta") {
    stop_inline_boundary(expr, fam_tag, bound = 0)
  }
  if (at_one & (fam_tag == "beta" || fam_tag == "zero_inflated_beta")) {
    stop_inline_boundary(expr, fam_tag, bound = 1)
  }
  invisible(NULL)
}

#' Reject a boundary value on a response transformed inside the formula
#'
#' The shift a boundary value would receive is computed on the transformed
#' scale, but \code{brm()} is handed the user's data frame and re-evaluates the
#' transformation from the recorded column, so the shift cannot reach the fit.
#' Before #258 it was computed, reported to the user and then discarded, and
#' \code{brm()} failed naming the condition the package had just said it had
#' repaired. Raising the conflict here names the variable and gives a remedy.
#'
#' @param expr The response as written in the formula, e.g. \code{"log(y)"}.
#' @param fam_tag The family name, as \code{family$family} gives it.
#' @param bound The boundary the family excludes, 0 or 1.
#' @param hint Optional further advice appended to the message.
#'
#' @noRd
stop_inline_boundary <- function(expr, fam_tag, bound, hint = "") {
  stop("Your response reaches the model as \"", expr, "\", a transformation",
       " written inside the model formula, and the transformed response",
       " contains values of ", bound, ", which a ", fam_tag, " distribution",
       " cannot represent. bayesnec shifts such values off the boundary, but",
       " the shift cannot be carried through a transformation written inline:",
       " brm() re-evaluates \"", expr, "\" from the data it is given, so the",
       " shift would be discarded and the fit would fail. Compute the",
       " transformation into a column of your data and pass that column to the",
       " formula instead.", hint, call. = FALSE)
}

#' Which rows carry a censoring declaration
#'
#' Returns a scalar \code{FALSE} when there is no censoring variable at all, so
#' that it recycles harmlessly against the response in the callers above.
#'
#' @noRd
is_censored <- function(cens) {
  if (is.null(cens)) {
    FALSE
  } else {
    !is.na(cens) & cens != 0
  }
}

#' Reject a censored observation sitting on a boundary the family excludes
#'
#' @param bound The boundary value, 0 or 1.
#' @param direction The brms censoring code that would be degenerate there,
#' -1 (left) at 0 and 1 (right) at 1.
#'
#' @noRd
check_cens_support <- function(y, cens, bound, direction, fam_tag) {
  if (is.null(cens)) {
    return(invisible(NULL))
  }
  bad <- which(!is.na(cens) & cens == direction & y == bound)
  if (length(bad) == 0) {
    return(invisible(NULL))
  }
  side <- if (direction < 0) "left" else "right"
  beyond <- if (direction < 0) "at or below" else "at or above"
  stop("Row(s) ", paste0(bad[seq_len(min(10, length(bad)))], collapse = ", "),
       if (length(bad) > 10) ", ..." else "",
       " of your response are declared ", side, "-censored at ", bound,
       ", but a ", fam_tag, " distribution has no probability mass ", beyond,
       " ", bound, ", so the censored likelihood is degenerate there. For a ",
       side, "-censored row the response value must carry the bound -- the",
       " value the truth is known to be ", beyond, " -- so replace those",
       " entries with that bound (for a rounded response, half the recording",
       " resolution). See ?bayesnecformula.", call. = FALSE)
}
