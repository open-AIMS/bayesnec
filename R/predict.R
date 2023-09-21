#' Generates mean posterior predictions for objects fitted by
#' \code{\link{bnec}}
#'
#' Generates mean posterior predictions for objects fitted by
#' \code{\link{bnec}}. \code{object} should be of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @name predict
#' @order 1
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#' @param ... Additional arguments to \code{\link[brms]{predict.brmsfit}} if
#' object is of class \code{\link{bayesnecfit}}, or to
#' \code{\link[brms]{posterior_predict.brmsfit}} if object is of class
#' \code{\link{bayesmanecfit}}.
#'
#' @return See \code{?brms::predict.brmsfit}.
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' # Uses default `resolution` and `x_range` to generate `newdata` internally
#' predict(manec_example)
#' # Provide user-specified `newdata`
#' nd_ <- data.frame(x = seq(0, 3, length.out = 200))
#' predict(manec_example, ecx_val = 50, newdata = nd_, make_newdata = FALSE)
#' # Predictions for raw input data
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' preds <- predict(nec4param, make_newdata = FALSE)
#' x <- pull_brmsfit(nec4param)$data$x
#' plot(x, preds[, 1])
#' }
NULL

#' @rdname predict
#' @order 2
#'
#' @method predict bayesnecfit
#'
#' @inherit predict description return examples
#'
#' @importFrom stats predict
#'
#' @export
predict.bayesnecfit <- function(object, ...) {
  predict(pull_brmsfit(object), ...)
}

#' @rdname predict
#' @order 3
#'
#' @param summary Should summary statistics be returned
#'  instead of the raw values? Default is \code{TRUE}.
#' @param robust If \code{FALSE} (the default) the mean is used as
#'  the measure of central tendency and the standard deviation as
#'  the measure of variability. If \code{TRUE}, the median and the
#'  median absolute deviation (MAD) are applied instead.
#'  Only used if \code{summary} is \code{TRUE}.
#' @param probs  The percentiles to be computed by the \code{quantile}
#'  function. Only used if \code{summary} is \code{TRUE}.
#'
#' @method predict bayesmanecfit
#'
#' @inherit predict description return examples
#'
#' @importFrom brms posterior_summary posterior_predict
#'
#' @export
predict.bayesmanecfit <- function(object, summary = TRUE,
                                  robust = FALSE,
                                  probs = c(0.025, 0.975), ...) {
  av_post_preds <- posterior_predict(object, ...)
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
