#' nec.default
#'
#' Extracts the predicted nec value as desired from an object of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}}.
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#' @param posterior A \code{\link[base]{logical}} value indicating if the full
#' posterior sample of calculated nec values should be returned instead of
#' just the median and 95 credible intervals.
#' @param xform A function to apply to the returned estimated concentration
#' values.
#' @param prob_vals A vector indicating the probability values over which to
#' return the estimated nec value. Defaults to 0.5 (median) and 0.025 and
#' 0.975 (95 percent credible intervals).
#'
#' @seealso \code{\link{bnec}}
#'
#' @return A vector containing the estimated nec value, including upper and
#' lower 95% credible interval bounds
#' (or other interval as specified by prob_vals).
#'
#' @importFrom stats quantile predict
#'
#' @examples
#' library(bayesnec)
#' data(manec_example)
#' nec(manec_example)
#'
#' @export
nec.default <- function(object, posterior = FALSE,  xform = NA,
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
  if (mod_class == "ecx") {
    stop("nec is not a parameter in ecx model types.")
  }
  nec_out <- object$nec_posterior
  if (inherits(xform, "function")) {
    nec_out <- xform(nec_out)
  }
  nec_estimate <- quantile(unlist(nec_out), probs = prob_vals)
  if (!posterior) {
    nec_estimate
  } else {
    nec_out
  }
}

#' nec
#'
#' Extracts the predicted nec value as desired from an object of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}}.
#'
#' @inheritParams nec.default
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesnecfit}} returned by \code{\link{bnec}}.
#'
#' @inherit nec.default return details seealso examples
#'
#' @export
nec <- function(object, posterior = FALSE,
                 xform = NA, prob_vals = c(0.5, 0.025, 0.975)) {
  UseMethod("nec")
}

#' nec.bayesnecfit
#'
#' Extracts the predicted nec value as desired from an object of class
#' \code{\link{bayesnecfit}}.
#'
#' @param object An object of class \code{\link{bayesnecfit}}
#' returned by \code{\link{bnec}}.
#' @param ... Additional arguments to \code{\link{nec}}
#'
#' @inherit nec return details seealso examples
#' @export
nec.bayesnecfit <- function(object, ...) {
  nec.default(object, ...)
}

#' nec.bayesmanecfit
#'
#' Extracts the predicted nec value as desired from an object of class
#' \code{\link{bayesmanecfit}}.
#'
#' @inheritParams nec
#'
#' @param object An object of class \code{\link{bayesmanecfit}} returned by
#' \code{\link{bnec}}.
#'
#' @inherit nec return details seealso examples
#'
#' @importFrom stats quantile
#'
#' @export
nec.bayesmanecfit <- function(object, posterior = FALSE,
                               xform = NA, prob_vals = c(0.5, 0.025, 0.975)) {
  if (max(grepl("ecx", names(object$mod_fits))) == 1) {
    message("bayesmanecfit contains ecx model types and therefore nec",
            " estimate includes nsec values.")
  }
  nec_out <- object$w_nec_posterior
  if (inherits(xform, "function")) {
    nec_out <- xform(nec_out)
  }
  nec_estimate <- quantile(unlist(nec_out), probs = prob_vals)
  if (!posterior) {
    nec_estimate
  } else {
    nec_out
  }
}
