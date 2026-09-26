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
#' # The model average beside one equation of the set, as a named list
#' preds_list <- predict(manec_example, model = "nec4param")
#' names(preds_list)
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
#' @param model \code{NULL} (the default), or a \code{\link[base]{character}}
#'  vector naming equations of the model set, for each of which the
#'  predictions of that equation alone are returned. These are what
#'  \code{predict()} returns for the \code{\link{bayesnecfit}} that
#'  \code{\link{pull_out}} gives for the equation, drawn from its whole
#'  posterior. A name that is not an equation of the set is refused.
#' @param average A \code{\link[base]{logical}} value indicating if the model
#'  averaged predictions should be returned. Defaults to \code{TRUE}.
#'  \code{average = FALSE} requires \code{model}.
#'
#' @return For a \code{\link{bayesmanecfit}} with \code{model = NULL}, the
#'  model averaged predictions, in the form \code{?brms::predict.brmsfit}
#'  describes. Where \code{model} is given, a named
#'  \code{\link[base]{list}} of such results instead: an element
#'  \code{average} holding the model averaged predictions, where
#'  \code{average = TRUE}, followed by one element for each equation, named
#'  by the equation, in the order given in \code{model}. The list is returned
#'  even when it has one element, so that its form depends on whether
#'  \code{model} is given and on nothing else.
#'
#' @method predict bayesmanecfit
#'
#' @inherit predict description examples
#'
#' @importFrom brms posterior_summary posterior_predict
#' @importFrom stats predict
#'
#' @export
predict.bayesmanecfit <- function(object, summary = TRUE,
                                  robust = FALSE,
                                  probs = c(0.025, 0.975), ...,
                                  model = NULL, average = TRUE) {
  # model and average follow the dots so that a positional argument after
  # probs still reaches posterior_predict() as it did before they existed.
  shown <- resolve_model_average(
    object, model, average,
    new_supplied = !missing(model) || !missing(average)
  )
  if (is.null(shown$model)) {
    return(predict_manec_average(object, summary = summary, robust = robust,
                                 probs = probs, ...))
  }
  out <- list()
  # The model average is computed before the equations so that, for a given
  # seed, it matches what predict() returns with model = NULL: the equations
  # draw from the random number stream as well, and computed first they would
  # change the stream the average starts from.
  if (shown$average) {
    out$average <- predict_manec_average(object, summary = summary,
                                         robust = robust, probs = probs, ...)
  }
  for (m in shown$model) {
    # The equation's brmsfit directly rather than through pull_out(), which
    # rebuilds the whole bayesnecfit, prediction grid included, to reach the
    # same brmsfit that predict.bayesnecfit() would then use.
    out[[m]] <- predict(object$mod_fits[[m]]$fit, summary = summary,
                        robust = robust, probs = probs, ...)
  }
  out
}

#' The model averaged predictions of a model set
#'
#' The body of \code{predict.bayesmanecfit()} before \code{model} and
#' \code{average} were added, unchanged.
#'
#' @importFrom brms posterior_summary posterior_predict
#'
#' @noRd
predict_manec_average <- function(object, summary, robust, probs, ...) {
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
