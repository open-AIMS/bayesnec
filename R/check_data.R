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
  # brms reports every custom family as "custom", so the effective tag has to
  # come from family$name for those. Everything below that switches on fam_tag
  # is unchanged for the built-in families.
  fam_tag <- family_tag(family)
  x_type <- set_distribution(x, silence_y_msgs = TRUE, silence_x_msgs = FALSE)
  ymax <- NULL
  if (is_beta_ub_family(family)) {
    # No proportion guard here, and no nudge of any kind. The response is not a
    # proportion: its ceiling is a parameter, so a maximum at 1 is unremarkable
    # and moving it would be the very thing this family exists to avoid.
    if (any(y < 0)) {
      stop("The beta_ub family requires a strictly positive response, but",
           " yours contains negative values. A response that can genuinely go",
           " negative -- a specific growth rate, an increment, a yield -- needs",
           " a family on the real line; truncating or flooring it at zero to",
           " fit a bounded family introduces a bias of its own. See",
           " https://github.com/open-AIMS/bayesnec/issues/175.",
           call. = FALSE)
    }
    if (any(y == 0)) {
      stop("The beta_ub family requires a strictly positive response, but",
           " yours contains zeros. If those zeros mean the response failed",
           " entirely -- colonies that died, cultures that crashed -- they are",
           " a hurdle, and family = \"hurdle_gamma\" models them explicitly.",
           " If instead they are a rate that reached zero, or a measurement",
           " floored at the recording limit, neither a hurdle nor a bounded",
           " family is right. See",
           " https://github.com/open-AIMS/bayesnec/issues/175.",
           call. = FALSE)
    }
    # Stashed rather than recomputed downstream: check_data may have altered
    # the predictor, and post-processing must see the same ceiling reference
    # the likelihood used even if the data are later subset.
    ymax <- max(y)
  }
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
  if (max(y) == 1 & fam_tag == "beta") {
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
  list(mod_dat = mod_dat, family = family, ymax = ymax)
}
