#' on_rational_grid
#'
#' Are all values expressible as k / n for a single small integer n?
#'
#' Genuine proportion data derived from counts (survival, fertilisation,
#' bleaching scores) lies on such a grid; a continuous response divided by a
#' continuous maximum does not. This is the discriminator that makes the
#' divided-by-maximum check usable: without it the check fires on up to 38\% of
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
  if (!is.numeric(x)) {
    x_flag <- names(data)[x_pos]
    stop(paste0("Your indicated predictor column \"", x_flag,
                "\" contains data that is class ", class(x),
                ". The function bnec requires the predictor",
                " column to be numeric."))
  }
  test_x <- mean(x)
  test_y <- mean(y)
  if (!is.finite(test_x)) {
    stop("Your predictor column contains values that are not finite.")
  }
  if (!is.finite(test_y)) {
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
  x_type <- set_distribution(x, silence_y_msgs = TRUE, silence_x_msgs = FALSE)
  if (min(x) == 0 & x_type == "Gamma") {
    min_val <- min(x[x > 0])
    data[x == 0, x_pos] <- x[x == 0] + (min_val / 10)
  }
  # NB: this nudge must never apply to a hurdle family. There the zeros are the
  # hurdle signal, not a boundary problem -- moving them off zero would leave
  # the hu block with nothing to identify itself from. fam_tag is
  # "hurdle_gamma" rather than "Gamma" in that case, so the condition below
  # already excludes it; the guard is explicit so it survives refactoring.
  if (min(y) == 0 & fam_tag == "Gamma" & !is_hurdle_family(fam_tag)) {
    min_val <- min(y[y > 0])
    data[y == 0, y_pos] <- y[y == 0] + (min_val / 10)
    message("Your response contains zeros, which a Gamma distribution cannot",
            " represent. They have been shifted to ", signif(min_val / 10, 3),
            " (one tenth of the smallest non-zero value). If those zeros are",
            " meaningful -- for example individuals that died -- consider",
            " family = hurdle_gamma() instead, which models them explicitly.")
  }
  if (min(x) == 0 & x_type == "beta") {
    min_val <- min(x[x > 0])
    data[x == 0, x_pos] <- x[x == 0] + (min_val / 10)
  }
  if (min(y) == 0 & fam_tag == "beta") {
    min_val <- min(y[y > 0])
    data[y == 0, y_pos] <- y[y == 0] + (min_val / 10)
  }
  if (max(x) == 1 & x_type == "beta") {
    data[x == 1, x_pos] <- x[x == 1] - 0.001
  }
  # A zero-inflated Beta keeps its zeros -- they are the signal -- but ones are
  # still outside Beta's open (0, 1) support and must be nudged as usual.
  if (max(y) == 1 & (fam_tag == "beta" || fam_tag == "zero_inflated_beta")) {
    data[y == 1, y_pos] <- y[y == 1] - 0.001
  }
  mod_dat <- data.frame(x = data[[x_pos]], y = data[[y_pos]],
                        trials = nrow(data))
  bnec_group_vars <- attr(data, "bnec_group")
  if (any(!is.na(bnec_group_vars))) {
    are_numeric <- sapply(data[, bnec_group_vars, drop = FALSE], is.numeric)
    if (any(are_numeric)) {
      to_flag <- paste0(names(are_numeric)[are_numeric], collapse = "; ")
      stop("Your group-level column(s): ", to_flag, "; must be either a",
           " character or a factor.")
    }
  }
  custom_name <- check_custom_name(family)
  if (fam_tag == "binomial" || fam_tag == "beta_binomial") {
    mod_dat$trials <- retrieve_var(data, "trials_var", error = TRUE)
  }
  list(mod_dat = mod_dat, family = family)
}
