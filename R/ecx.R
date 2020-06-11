#' ecx
#'
#' Extracts the predicted ECx value as desired from a "bayesnecfit" or a "bayesmanecfit" object.
#'
#' @param object An object of class "bayesnecfit" or "bayesmanecfit" returned by \code{\link{bnec}}.
#' @param ecx_val The desired percentage effect value. This must be a value between 1 and 99 (for type = "relative" 
#' and "absolute"), defaults to 10.
#' @param type A \code{\link[base]{character}} vector, taking values of "relative", "absolute" (the default) or "direct". See Details.
#' @param precision The number of unique x values over which to find ECx - large values will make the ECx estimate more 
#' precise.
#' @param posterior A \code{\link[base]{logical}} value indicating if the full posterior sample of calculated ECx values should be returned 
#' instead of just the median and 95 credible intervals
#' @param hormesis_def A \code{\link[base]{character}} vector, taking values of "max" or "control". See Details.
#' @param xform A function to apply to the returned estimated concentration values
#' @param x_range A range of x values over which to consider extracting ECx
#' @param prob_vals A vector indicating the probability values over which to return the estimated ECx value. Defaults to 0.5 (median) and 0.025 and 0.975 (95 percent credible intervals). 
#' 
#' @details \code{type} "relative" is calculated as the percentage decrease from the maximum predicted value of the response (top) to the minimum predicted value 
#' of the response. Type "absolute" (the default) is calculated as the percentage decrease from the maximum value of the response (top)
#' to 0 (or bot for a 4 parameter model fit). Type "direct" provides a direct estimate of the x value for a given y.
#' Note that for the current version, ECx for an "nechorme" (NEC Hormesis) model is estimated at a percent decline from the control.
#' For \code{hormesis_def}, if "max", then ECx values are calculated as a decline from the maximum estimates (i.e. the peak at nec);
#' if "control", then ECx values are calculated relative to the control, which is assumed to be the lowest observed concentration.
#' @seealso \code{\link{bnec}}
#' @return A vector containing the estimated ECx value, including upper and lower 95% credible interval bounds.
#' @export
ecx <- function(object, ecx_val = 10, precision = 1000, posterior = FALSE,
                type = "absolute", hormesis_def = "control", xform = NA,
                x_range = NA, prob_vals = c(0.5, 0.025, 0.975)) {
  if (inherits(object, "bayesnecfit")) {
    ecx_nec(object, ecx_val = ecx_val, precision = precision,
             posterior = posterior, type = type,
             hormesis_def = hormesis_def, xform = xform,
             prob_vals = prob_vals)
  } else if (inherits(object, "bayesmanecfit")) {
    ecx_manec(object, ecx_val = ecx_val, precision = precision,
           posterior = posterior, type = type, xform = xform,
           x_range = x_range, prob_vals = prob_vals)
  } else {
    stop("Failed to estimate ECx value for the supplied object class. ",
         "Only bayesnecfit and bayesmanecfit classes are supported.")
  }
}

#' ecx_nec
#'
#' Extracts the predicted ECx value as desired from a "bayesnecfit" object.
#'
#' @inheritParams ecx
#' @inherit ecx return details seealso
#' @importFrom brms posterior_epred
#' @importFrom stats quantile predict
ecx_nec <- function(object, ecx_val = 10, precision = 1000,
                     posterior = FALSE, type = "absolute",
                     hormesis_def = "control", x_range = NA,
                     xform = NA, prob_vals = c(0.5, 0.025, 0.975)) {
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

  if (object$family$family == "gaussian" & type == "absolute") {
    stop("Absolute ECx values are not valid for a gaussian ",
         "response variable unless a 4 parameter model is fit")
  }

  pred_vals <- predict(object, precision = precision, x_range = x_range)
  p_samples <- pred_vals$posterior
  x_vec <- pred_vals$data$x
  if (object$model == "nechorme") {
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
  ecx_estimate <- quantile(ecx_out, probs = prob_vals)
  names(ecx_estimate) <- paste(label, clean_names(ecx_estimate), sep = "_")

  if (!posterior) {
    ecx_estimate
  } else {
    ecx_out
  }
}

#' ecx_manec
#'
#' Extracts the predicted ECx value as desired from a "bayesmanecfit" object.
#'
#' @inheritParams ecx
#' @inherit ecx return details seealso
#' @importFrom stats quantile
ecx_manec <- function(object, ecx_val = 10, precision = 1000, posterior = FALSE,
                   type = "absolute", hormesis_def = "control", xform = NA,
                   x_range = NA, prob_vals = c(0.5, 0.025, 0.975)) {
  sample_size <- object$sample_size
  ecx_out <- unlist(sapply(seq_len(length(object$success_models)), function(x) {
    sample(ecx_nec(object$mod_fits[[x]], ecx_val = ecx_val,
                    precision = precision, posterior = TRUE,
                    x_range = x_range, type = type),
           as.integer(round(sample_size * object$mod_stats[x, "wi"])))
  }))

  label <- paste("ec", ecx_val, sep = "_")
  ecx_estimate <- quantile(ecx_out, probs = prob_vals)
  names(ecx_estimate) <- c(label, paste(label, "lw", sep = "_"),
                           paste(label, "up", sep = "_"))

  if (inherits(xform, "function")) {
    ecx_estimate <- xform(ecx_estimate)
    ecx_out <- xform(ecx_out)
  }
  if (!posterior) {
    ecx_estimate
  } else {
    ecx_out
  }
}

modify_posterior <- function(n, object, x_vec, p_samples, hormesis_def) {
  posterior_sample <- p_samples[n, ]
  if (hormesis_def == "max") {
    target <- object$nec_posterior[n]
    change <- x_vec < target
  } else if (hormesis_def == "control") {
    target <- posterior_sample[n, 1]
    change <- posterior_sample >= target
  }
  posterior_sample[change] <- NA
  posterior_sample
}

ecx_x_absolute <- function(y, ecx_val, x_vec) {
  range_y <- range(y, na.rm = TRUE)
  ecx_y <- max(range_y) - diff(range_y) * (ecx_val / 100)
  x_vec[min_abs(y - ecx_y)]
}

ecx_x_relative <- function(y, ecx_val, x_vec) {
  range_y <- c(0, max(y, na.rm = TRUE))
  ecx_y <- max(range_y) - diff(range_y) * (ecx_val / 100)
  x_vec[min_abs(y - ecx_y)]
}

ecx_x_direct <- function(y, ecx_val, x_vec) {
  ecx_y <- ecx_val
  x_vec[min_abs(y - ecx_y)]
}

clean_names <- function(x) {
  paste0("Q", gsub("%", "", names(x), fixed = TRUE))
}
