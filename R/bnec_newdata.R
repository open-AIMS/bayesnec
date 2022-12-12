#' bnec_newdata
#' 
#' Create a dataset for predictions
#'
#' @param x An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} as returned by \code{\link{bnec}}.
#' @param precision A \code{\link[base]{numeric}} vector of length 1 indicating
#' the number of x values over which to predict values.
#' @param x_range A \code{\link[base]{numeric}} vector of length 2 indicating
#' the range of x values over which to make predictions.
#'
#' @return A \code{\link[base]{data.frame}} to be used in predictions.
#' @export
bnec_newdata <- function(x, precision = 100, x_range = NA) {
  UseMethod("bnec_newdata")
}

#' bnec_newdata.bayesnecfit
#' 
#' Create a dataset for predictions
#'
#' @inheritParams bnec_newdata
#' @inherit bnec_newdata description return
#' 
#' @importFrom stats model.frame
#' @noRd
#' @export
bnec_newdata.bayesnecfit <- function(x, precision = 100, x_range = NA) {
  check_args_newdata(precision, x_range)
  data <- model.frame(x$bayesnecformula, data = x$fit$data)
  x_var <- attr(data, "bnec_pop")[["x_var"]]
  fit <- x$fit
  x_vec <- fit$data[[x_var]]
  if (any(is.na(x_range))) {
    x_seq <- seq(min(x_vec), max(x_vec), length = precision)
  } else {
    x_seq <- seq(min(x_range), max(x_range), length = precision)
  }
  newdata <- data.frame(x_seq)
  names(newdata) <- x_var
  fam_tag <- fit$family$family
  custom_name <- check_custom_name(fit$family)
  if (fam_tag == "binomial" || custom_name == "beta_binomial2") {
    trials_var <- attr(data, "bnec_pop")[["trials_var"]]
    newdata[[trials_var]] <- 1
  }
  newdata
}

#' bnec_newdata.bayesmanecfit
#' 
#' Create a dataset for predictions
#'
#' @inheritParams bnec_newdata
#' @inherit bnec_newdata description return
#' @noRd
#' @export
bnec_newdata.bayesmanecfit <- function(x, precision = 100, x_range = NA) {
  model_set <- names(x$mod_fits)
  bayesnecfit_x <- pull_out(x, model = model_set[1]) |>
    suppressMessages()
  bnec_newdata(bayesnecfit_x, precision, x_range)
}
