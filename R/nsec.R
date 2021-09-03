#' Extracts the predicted NSEC value as desired from an object of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#' @param sig_val Probability value to use as the lower quantile to test
#' significance of the predicted posterior values.
#' against the lowest observed concentration (assumed to be the control), to
#' estimate NEC as an interpolated NOEC value from smooth ECx curves.
#' @param precision The number of unique x values over which to find NSEC -
#' large values will make the NSEC estimate more precise.
#' @param posterior A \code{\link[base]{logical}} value indicating if the full
#' posterior sample of calculated NSEC values should be returned instead of
#' just the median and 95 credible intervals.
#' @param hormesis_def A \code{\link[base]{character}} vector, taking values
#' of "max" or "control". See Details.
#' @param xform A function to apply to the returned estimated concentration
#' values.
#' @param x_range A range of x values over which to consider extracting NSEC.
#' @param prob_vals A vector indicating the probability values over which to
#' return the estimated NSEC value. Defaults to 0.5 (median) and 0.025 and
#' 0.975 (95 percent credible intervals).
#'
#' @details For \code{hormesis_def}, if "max", then NSEC values are calculated
#' as a decline from the maximum estimates (i.e. the peak at nec);
#' if "control", then ECx values are calculated relative to the control, which
#' is assumed to be the lowest observed concentration.
#'
#' @seealso \code{\link{bnec}}
#'
#' @return A vector containing the estimated NSEC value, including upper and
#' lower 95% credible interval bounds.
#'
#' @examples
#' \donttest{
#' library(bayesnec)
#'
#' data(manec_example)
#' nsec(manec_example)
#' }
#'
#' @export
nsec <- function(object, sig_val = 0.01, precision = 1000,
                 posterior = FALSE, x_range = NA, hormesis_def = "control",
                 xform = NA, prob_vals = c(0.5, 0.025, 0.975)) {
  UseMethod("nsec")
}

#' @inheritParams nsec
#'
#' @param object An object of class \code{\link{bayesnecfit}} returned by
#' \code{\link{bnec}}.
#'
#' @inherit nsec details seealso return examples
#' 
#' @importFrom stats quantile predict
#' @importFrom brms as_draws_df
#'
#' @noRd
#'
#' @export
nsec.default <- function(object, sig_val = 0.01, precision = 1000,
                         posterior = FALSE, x_range = NA,
                         hormesis_def = "control", xform = NA,
                         prob_vals = c(0.5, 0.025, 0.975)) {
  if (length(prob_vals) < 3 | prob_vals[1] < prob_vals[1] |
        prob_vals[1] > prob_vals[3] | prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order.")
  }
  if (length(grep("ecx", object$model)) > 0) {
    mod_class <- "ecx"
  } else {
    mod_class <- "nec"
  }
  pred_vals <- predict(object, precision = precision, x_range = x_range)
  p_samples <- pred_vals$posterior
  x_vec <- pred_vals$data$x
  reference <- quantile(p_samples[, 1], sig_val)
  if (grepl("horme", object$model)) {
    n <- seq_len(nrow(p_samples))
    p_samples <- do_wrapper(n, modify_posterior, object, x_vec,
                            p_samples, hormesis_def, fct = "rbind")
    nec_posterior <- as_draws_df(object$fit)[["b_nec_Intercept"]]
    if (hormesis_def == "max") {
      reference <- quantile(apply(pred_vals$posterior, 2, max),
                            probs = sig_val)
    }
  }
  nsec_out <- apply(p_samples, 1, nsec_fct, reference, x_vec)
  formula <- object$bayesnecformula
  x_str <- grep("crf(", labels(terms(formula)), fixed = TRUE, value = TRUE)
  x_call <- str2lang(eval(parse(text = x_str)))
  if (inherits(x_call, "call")) {
    x_call[[2]] <- str2lang("nsec_out")
    nsec_out <- eval(x_call)
  }
  if (inherits(xform, "function")) {
    nsec_out <- xform(nsec_out)
  }
  label <- paste("ec", sig_val, sep = "_")
  nsec_estimate <- quantile(unlist(nsec_out), probs = prob_vals)
  names(nsec_estimate) <- paste(label, clean_names(nsec_estimate), sep = "_")
  attr(nsec_estimate, "precision") <- precision
  attr(nsec_out, "precision") <- precision
  attr(nsec_estimate, "sig_val") <- sig_val
  attr(nsec_out, "sig_val") <- sig_val
  if (!posterior) {
    nsec_estimate
  } else {
    nsec_out
  }
}

#' @inheritParams nsec
#'
#' @param object An object of class \code{\link{bayesnecfit}} returned by
#' \code{\link{bnec}}.
#'
#' @inherit nsec details seealso return examples
#' 
#' @noRd
#'
#' @export
nsec.bayesnecfit <- function(object, sig_val = 0.01, precision = 1000,
                             posterior = FALSE, x_range = NA,
                             hormesis_def = "control", xform = NA,
                             prob_vals = c(0.5, 0.025, 0.975)) {
  nsec.default(object, sig_val = sig_val, precision = precision,
               posterior = posterior, x_range = x_range,
               hormesis_def = hormesis_def, xform = xform, 
               prob_vals = prob_vals)
}

#' @inheritParams nsec
#'
#' @param object An object of class \code{\link{bayesmanecfit}} returned by
#' \code{\link{bnec}}.
#'
#' @inherit nsec details seealso return examples
#' 
#' @importFrom stats quantile
#'
#' @noRd
#'
#' @export
nsec.bayesmanecfit <- function(object, sig_val = 0.01, precision = 1000,
                               posterior = FALSE, x_range = NA,
                               hormesis_def = "control", xform = NA,
                               prob_vals = c(0.5, 0.025, 0.975)) {
  sample_nsec <- function(x, object, sig_val, precision,
                          posterior, hormesis_def,
                          x_range, xform, prob_vals, sample_size) {
    mod <- names(object$mod_fits)[x]
    target <- suppressMessages(pull_out(object, model = mod))
    out <- nsec(target, sig_val = sig_val, precision = precision,
                posterior = posterior, hormesis_def = hormesis_def,
                x_range = x_range, xform = xform, prob_vals = prob_vals)
    n_s <- as.integer(round(sample_size * object$mod_stats[x, "wi"]))
    sample(out, n_s)
  }
  sample_size <- object$sample_size
  to_iter <- seq_len(length(object$success_models))
  nsec_out <- sapply(to_iter, sample_nsec, object, sig_val, precision,
                     posterior = TRUE, hormesis_def, x_range,
                     xform, prob_vals, sample_size)
  nsec_out <- unlist(nsec_out)
  label <- paste("ec", sig_val, sep = "_")
  nsec_estimate <- quantile(nsec_out, probs = prob_vals)
  names(nsec_estimate) <- c(label, paste(label, "lw", sep = "_"),
                            paste(label, "up", sep = "_"))
  attr(nsec_estimate, "precision") <- precision
  attr(nsec_out, "precision") <- precision
  attr(nsec_estimate, "sig_val") <- sig_val
  attr(nsec_out, "sig_val") <- sig_val
  if (!posterior) {
    nsec_estimate
  } else {
    nsec_out
  }
}

#' @noRd
nsec_fct <- function(y, reference, x_vec) {
  x_vec[min_abs(y - reference)]
}
