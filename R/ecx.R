#' ecx.default
#'
#' Extracts the predicted ECx value as desired from an object of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}}.
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#' @param ecx_val The desired percentage effect value. This must be a value
#' between 1 and 99 (for type = "relative" and "absolute"), defaults to 10.
#' @param type A \code{\link[base]{character}} vector, taking values of
#' "relative", "absolute" (the default) or "direct". See Details.
#' @param precision The number of unique x values over which to find ECx -
#' large values will make the ECx estimate more precise.
#' @param posterior A \code{\link[base]{logical}} value indicating if the full
#' posterior sample of calculated ECx values should be returned instead of
#' just the median and 95 credible intervals.
#' @param hormesis_def A \code{\link[base]{character}} vector, taking values
#' of "max" or "control". See Details.
#' @param xform A function to apply to the returned estimated concentration
#' values.
#' @param x_range A range of x values over which to consider extracting ECx.
#' @param prob_vals A vector indicating the probability values over which to
#' return the estimated ECx value. Defaults to 0.5 (median) and 0.025 and
#' 0.975 (95 percent credible intervals).
#'
#' @details \code{type} "relative" is calculated as the percentage decrease
#' from the maximum predicted value of the response (top) to the minimum
#' predicted value of the response. Type "absolute" (the default) is
#' calculated as the percentage decrease from the maximum value of the
#' response (top) to 0 (or bot for a 4 parameter model fit). Type "direct"
#' provides a direct estimate of the x value for a given y.
#' Note that for the current version, ECx for an "nechorme" (NEC Hormesis)
#' model is estimated at a percent decline from the control.
#' For \code{hormesis_def}, if "max", then ECx values are calculated as a
#' decline from the maximum estimates (i.e. the peak at nec);
#' if "control", then ECx values are calculated relative to the control, which
#' is assumed to be the lowest observed concentration.
#'
#' @seealso \code{\link{bnec}}
#'
#' @return A vector containing the estimated ECx value, including upper and
#' lower 95% credible interval bounds.
#'
#' @importFrom stats quantile predict
#'
#' @examples
#' \donttest{
#' library(brms)
#' library(bayesnec)
#' data(manec_example)
#' ecx(manec_example, ecx_val = 50)
#' ecx(manec_example)
#' }
#'
#' @export
ecx.default <- function(object, ecx_val = 10, precision = 1000,
                        posterior = FALSE, type = "absolute",
                        hormesis_def = "control", x_range = NA,
                        xform = NA, prob_vals = c(0.5, 0.025, 0.975)) {
  if (length(prob_vals) < 3 | prob_vals[1] < prob_vals[1] |
        prob_vals[1] > prob_vals[3] | prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order")
    }
  if (type != "direct") {
    if (ecx_val < 1 | ecx_val > 99) {
      stop("Supplied ecx_val is not in the required range. ",
           "Please supply a percentage value between 1 and 99.")
    }
  }
  if (length(grep("ecx", object$model)) > 0) {
    mod_class <- "ecx"
  } else {
    mod_class <- "nec"
  }
  if (!is.null(object$bot)) {
    m4param <- 1
  } else {
    m4param <- 0
  }
  if (object$fit$family$family == "gaussian" & type == "absolute" &
        m4param == 0) {
    stop("Absolute ECx values are not valid for a gaussian ",
         "response variable unless a model with a bot parameter is fit")
  }
  pred_vals <- predict(object, precision = precision, x_range = x_range)
  p_samples <- pred_vals$posterior
  x_vec <- pred_vals$data$x
  if (grepl("horme", object$model)) {
    n <- seq_len(nrow(p_samples))
    p_samples <- do_wrapper(n, modify_posterior, object, x_vec,
                            p_samples, hormesis_def, fct = "rbind")
  }
  ecx_fct <- get(paste0("ecx_x_", type))
  ecx_out <- apply(p_samples, 1, ecx_fct, ecx_val, x_vec)
  if (inherits(xform, "function")) {
    ecx_out <- xform(ecx_out)
  }
  label <- paste("ec", ecx_val, sep = "_")
  ecx_estimate <- quantile(unlist(ecx_out), probs = prob_vals)
  names(ecx_estimate) <- paste(label, clean_names(ecx_estimate), sep = "_")
  attr(ecx_estimate, "precision") <- precision
  attr(ecx_out, "precision") <- precision
  if (signif(ecx_estimate[1], 3) == signif(ecx_estimate[3], 3)) {
    message("The estimated mean is identical or nearly identical to your",
            " upper credible interval for the ", object$model, " model.",
            " This suggests the ecx estimate lies beyond the upper bound of",
            " your x_range and should be reported as greater than, and used",
            " as a censored value. You could try increasing x_range, although",
            " extrapolation beyond the data range should be done with",
            " caution.")
  } else if (signif(ecx_estimate[3], 3) == signif(max(x_vec), 3)) {
    message("The estimated upper credible interval is identical or nearly",
            " identical to the upper bound of your x_range value for the ",
            object$model, " model. This suggests the estimated uncertainty",
            "may be constrained. You could try increasing x_range to ensure",
            " this is not the case.")
  }
  if (!posterior) {
    ecx_estimate
  } else {
    ecx_out
  }
}

#' ecx
#'
#' Extracts the predicted ECx value as desired from an object of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}}.
#'
#' @inheritParams ecx.default
#'
#' @inherit ecx.default return details seealso examples
#'
#' @export
ecx <- function(object, ecx_val = 10, precision = 1000,
                posterior = FALSE, type = "absolute",
                hormesis_def = "control", x_range = NA,
                xform = NA, prob_vals = c(0.5, 0.025, 0.975)) {
  UseMethod("ecx")
}

#' ecx.bayesnecfit
#'
#' Extracts the predicted ECx value as desired from an object of class
#' \code{\link{bayesnecfit}}.
#'
#' @param object An object of class \code{\link{bayesnecfit}}
#' returned by \code{\link{bnec}}.
#' @param ... Additional arguments to \code{\link{ecx}}
#'
#' @inherit ecx return details seealso examples
#' @export
ecx.bayesnecfit <- function(object, ...) {
  ecx.default(object, ...)
}

#' ecx.bayesmanecfit
#'
#' Extracts the predicted ECx value as desired from an object of class
#' \code{\link{bayesmanecfit}}.
#'
#' @inheritParams ecx
#'
#' @param object An object of class \code{\link{bayesmanecfit}} returned by
#' \code{\link{bnec}}.
#'
#' @inherit ecx return details seealso examples
#'
#' @importFrom stats quantile
#' @export
ecx.bayesmanecfit <- function(object, ecx_val = 10, precision = 1000,
                              posterior = FALSE, type = "absolute",
                              hormesis_def = "control", x_range = NA,
                              xform = NA, prob_vals = c(0.5, 0.025, 0.975)) {
  sample_ecx <- function(x, object, ecx_val, precision,
                         posterior, type, hormesis_def,
                         x_range, xform, prob_vals, sample_size) {
    mod <- names(object$mod_fits)[x]
    target <- suppressMessages(pull_out(object, model = mod))
    out <- ecx.default(target, ecx_val = ecx_val,
                       precision = precision, posterior = posterior,
                       type = type, hormesis_def = hormesis_def,
                       x_range = x_range, xform = xform,
                       prob_vals = prob_vals)
    n_s <- as.integer(round(sample_size * object$mod_stats[x, "wi"]))
    sample(out, n_s)
  }
  sample_size <- object$sample_size
  to_iter <- seq_len(length(object$success_models))
  ecx_out <- sapply(to_iter, sample_ecx, object, ecx_val, precision,
                    posterior = TRUE, type, hormesis_def, x_range,
                    xform, prob_vals, sample_size)
  ecx_out <- unlist(ecx_out)
  label <- paste("ec", ecx_val, sep = "_")
  ecx_estimate <- quantile(ecx_out, probs = prob_vals)
  names(ecx_estimate) <- c(label, paste(label, "lw", sep = "_"),
                           paste(label, "up", sep = "_"))
  attr(ecx_estimate, "precision") <- precision
  attr(ecx_out, "precision") <- precision
  if (!posterior) {
    ecx_estimate
  } else {
    ecx_out
  }
}

ecx_x_relative <- function(y, ecx_val, x_vec) {
  if (length(which(!is.na(y))) == 0) {
    outval <- max(x_vec)
  } else {
    range_y <- range(y, na.rm = TRUE)
    ecx_y <- max(range_y) - diff(range_y) * (ecx_val / 100)
    outval <- x_vec[min_abs(y - ecx_y)]
  }
  outval
}

ecx_x_absolute <- function(y, ecx_val, x_vec) {
  if (length(which(!is.na(y))) == 0) {
    outval <- max(x_vec)
  } else {
    range_y <- c(0, max(y, na.rm = TRUE))
    ecx_y <- max(range_y) - diff(range_y) * (ecx_val / 100)
    outval <- x_vec[min_abs(y - ecx_y)]
  }
 outval
}

ecx_x_direct <- function(y, ecx_val, x_vec) {
  if (length(which(!is.na(y))) == 0) {
    outval <- max(x_vec)
  } else {
    ecx_y <- ecx_val
    outval <- x_vec[min_abs(y - ecx_y)]
  }
  outval
}
