#' Generates mean posterior linear predictions for objects fitted by
#' \code{\link{bnec}}
#'
#' Generates mean posterior linear predictions for objects fitted by
#' \code{\link{bnec}}. \code{object} should be of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @name fitted
#' @aliases fitted
#' @order 1
#'
#' @usage NULL
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#' @param ... Additional arguments to \code{\link[brms]{fitted.brmsfit}} if
#' object is of class \code{\link{bayesnecfit}}, or to
#' \code{\link[brms]{posterior_epred.brmsfit}} if object is of class
#' \code{\link{bayesmanecfit}}.
#'
#' @method fitted bnecfit
#'
#' @importFrom stats fitted
#'
#' @return See \code{?brms:fitted.brmsfit}.
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' # Uses default `precision` and `x_range` to generate `newdata` internally
#' fitted(manec_example)
#' # Provide user-specified `newdata`
#' nd_ <- data.frame(x = seq(0, 3, length.out = 200))
#' fits <- fitted(manec_example, ecx_val = 50, newdata = nd_,
#'                make_newdata = FALSE)
#' nrow(fits) == 200
#' # Predictions for raw input data
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' fits <- fitted(nec4param, make_newdata = FALSE)
#' x <- pull_brmsfit(nec4param)$data$x
#' plot(x, fits[, 1])
#' }
#'
#' @export
#' @export fitted
fitted.bnecfit <- function(object, ...) {
  UseMethod("fitted")
}

#' @rdname fitted
#' @order 2
#'
#' @method fitted bayesnecfit
#'
#' @inherit fitted description return examples
#'
#' @importFrom stats fitted
#'
#' @export
fitted.bayesnecfit <- function(object, ...) {
  fitted(pull_brmsfit(object), ...)
}

#' @rdname fitted
#' @order 3
#'
#' @param summary Should summary statistics be returned
#'  instead of the raw values? Default is \code{TRUE}.
#' @param robust If \code{FALSE} (the default) the mean is used as
#'  the measure of central tendency and the standard deviation as
#'  the measure of variability. If \code{TRUE}, the median and the
#'  median absolute deviation (MAD) are applied instead.
#'  Only used if \code{summary} is \code{TRUE}.
#' @param probs The percentiles to be computed by the \code{quantile}
#'  function. Only used if \code{summary} is \code{TRUE}.
#'
#' @method fitted bayesmanecfit
#'
#' @inherit fitted description return examples
#'
#' @importFrom brms posterior_summary posterior_epred
#'
#' @export
fitted.bayesmanecfit <- function(object, summary = TRUE, robust = FALSE,
                                 probs = c(0.025, 0.975), ...) {
  av_post_preds <- posterior_epred(object, ...)
  if (!summary) {
    av_post_preds
  } else {
    out <- apply(av_post_preds, 2, posterior_summary,
                 robust = robust, probs = probs) |>
      t()
    colnames(out) <- c("Estimate", "Est.Error",
                       paste0("Q", probs * 100))
    out
  }
}
