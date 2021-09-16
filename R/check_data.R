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
#' @importFrom stats na.omit
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
    stop("The mean value of the response column for the lower half of the ",
         "predictor column are lower than that of the upper half ",
         "of the predictor column. bnec only allows for ",
         "response values to decline with increasing values of predictor.")
  }
  fam_tag <- family$family
  x_type <- set_distribution(x)
  if (min(x) == 0 & x_type == "Gamma") {
    min_val <- min(x[x > 0])
    data[x == 0, x_pos] <- x[x == 0] + (min_val / 10)
  }
  if (min(y) == 0 & fam_tag == "Gamma") {
    min_val <- min(y[y > 0])
    data[y == 0, y_pos] <- y[y == 0] + (min_val / 10)
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
  if (!is.na(bnec_group_vars)) {
    are_numeric <- sapply(data[, bnec_group_vars, drop = FALSE], is.numeric)
    if (any(are_numeric)) {
      to_flag <- paste0(names(are_numeric)[are_numeric], collapse = "; ")
      stop("Your group-level column(s): ", to_flag, "; must be either a",
           " character or a factor.")
    }
  }
  custom_name <- check_custom_name(family)
  if (fam_tag == "binomial" || custom_name == "beta_binomial2") {
    mod_dat$trials <- retrieve_var(data, "trials_var", error = TRUE)
  }
  list(mod_dat = mod_dat, family = family)
}
