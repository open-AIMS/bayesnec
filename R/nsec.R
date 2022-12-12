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
#' @param make_newdata Should the user allow the package to create
#' \code{newdata} for predictions? If so, arguments \code{precision} and
#' \code{x_range} will be used. Defaults to TRUE. See details.
#' @param ... Further arguments that control posterior predictions via
#' \code{\link[brms]{posterior_epred}}.
#'
#' @details For \code{hormesis_def}, if "max", then NSEC values are calculated
#' as a decline from the maximum estimates (i.e. the peak at NEC);
#' if "control", then ECx values are calculated relative to the control, which
#' is assumed to be the lowest observed concentration.
#' 
#' The argument \code{make_newdata} is relevant to those who want the package
#' to create a data.frame from which to make predictions. this is done via
#' \code{\link{bnec_newdata}} and uses arguments \code{precision} and
#' \code{x_range}. If \code{make_newdata = FALSE} and no additional
#' \code{newdata} argument is provided (via \code{...}), then the predictions
#' are made for the raw data. Else, to generate predictions for a specific
#' user-specific data.frame, set \code{make_newdata = FALSE} and provide
#' an additional data.frame via the \code{newdata} argument. For guidance
#' on how to structure \code{newdata}, see for example
#' \code{\link[brms]{posterior_epred}}.
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
                 xform = identity, prob_vals = c(0.5, 0.025, 0.975),
                 make_newdata = TRUE, ...) {
  UseMethod("nsec")
}

#' @inheritParams nsec
#'
#' @param object An object of class \code{\link{bayesnecfit}} returned by
#' \code{\link{bnec}}.
#'
#' @inherit nsec details seealso return examples
#' 
#' @importFrom stats quantile
#' @importFrom brms as_draws_df posterior_epred
#' @importFrom chk chk_logical chk_numeric
#' 
#' @noRd
#'
#' @export
nsec.bayesnecfit <- function(object, sig_val = 0.01, precision = 1000,
                             posterior = FALSE, x_range = NA,
                             hormesis_def = "control", xform = identity,
                             prob_vals = c(0.5, 0.025, 0.975),
                             make_newdata = TRUE, ...) {
  chk_numeric(sig_val)
  chk_numeric(precision)
  chk_logical(posterior)
  if ((hormesis_def %in% c("max", "control")) == FALSE) {
    stop("type must be one of \"max\" or \"control\" (the default). ",
         "Please see ?ecx for more details.")
  }
  if(!inherits(xform, "function")) { 
    stop("xform must be a function.")}  
  if (length(prob_vals) < 3 | prob_vals[1] < prob_vals[2] |
      prob_vals[1] > prob_vals[3] | prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order.")
  }
  if (length(grep("ecx", object$model)) > 0) {
    mod_class <- "ecx"
  } else {
    mod_class <- "nec"
  }
  newdata_list <- newdata_eval(
    object, precision = precision, x_range = x_range,
    make_newdata = make_newdata, fct_eval = "nsec", ...
  )
  x_vec <- newdata_list$x_vec
  precision <- newdata_list$precision
  dot_list <- list(...)
  dot_list$object <- object
  dot_list$newdata <- newdata_list$newdata
  dot_list$re_formula <- newdata_list$re_formula
  p_samples <- do.call(posterior_epred, dot_list)
  reference <- quantile(p_samples[, 1], sig_val)
  if (grepl("horme", object$model)) {
    n <- seq_len(nrow(p_samples))
    p_samples <- do_wrapper(n, modify_posterior, object, x_vec,
                            p_samples, hormesis_def, fct = "rbind")
    nec_posterior <- as_draws_df(object$fit)[["b_nec_Intercept"]]
    if (hormesis_def == "max") {
      reference <- quantile(apply(p_samples, 2, max), probs = sig_val)
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
                               hormesis_def = "control", xform = identity,
                               prob_vals = c(0.5, 0.025, 0.975),
                               make_newdata = TRUE, ...) {
  sample_nsec <- function(x, object, sig_val, precision,
                          posterior, hormesis_def,
                          x_range, xform, prob_vals, sample_size,
                          make_newdata, ...) {
    mod <- names(object$mod_fits)[x]
    target <- suppressMessages(pull_out(object, model = mod))
    out <- nsec(target, sig_val = sig_val, precision = precision,
                posterior = posterior, hormesis_def = hormesis_def,
                x_range = x_range, xform = xform, prob_vals = prob_vals,
                make_newdata = make_newdata, ...)
    n_s <- as.integer(round(sample_size * object$mod_stats[x, "wi"]))
    sample(out, n_s)
  }
  sample_size <- object$sample_size
  to_iter <- seq_len(length(object$success_models))
  nsec_out <- sapply(to_iter, sample_nsec, object, sig_val, precision,
                     posterior = TRUE, hormesis_def, x_range,
                     xform, prob_vals, sample_size, make_newdata, ...)
  nsec_out <- unlist(nsec_out)
  label <- paste("ec", sig_val, sep = "_")
  nsec_estimate <- quantile(nsec_out, probs = prob_vals)
  names(nsec_estimate) <- c(label, paste(label, "lw", sep = "_"),
                            paste(label, "up", sep = "_"))
  dot_list <- list(...)
  if (!("newdata" %in% names(dot_list))) {
    if (!make_newdata) {
      precision <- "from raw data"
    }
  } else {
    precision <- "from user-specified newdata"
  }
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
