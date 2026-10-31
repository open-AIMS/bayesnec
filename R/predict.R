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
#' @details A \code{newdata} for a fit with a \code{disp(~z)} term, \code{z}
#' a column other than the predictor, is treated as
#' \code{\link{posterior_predict}} treats it. One without \code{z} is
#' refused. In one from \code{\link{bnec_newdata}}, which holds \code{z} at
#' a marked placeholder value, \code{z} is drawn for each posterior draw from
#' a row of the fitted data, so the summaries describe a new reading from the
#' design's mix of \code{z}. One that holds \code{z} unmarked, or changed
#' from the placeholder, is used as given. Without \code{newdata},
#' predictions are for the fitted rows, each with its own value.
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
#' @param newdata An optional \code{\link[base]{data.frame}} to predict over.
#'  Where it is \code{NULL}, the default, predictions are made for the data
#'  the fit was made from. See Details.
#'
#' @importFrom stats predict
#'
#' @export
predict.bayesnecfit <- function(object, newdata = NULL, ...) {
  disp_predict(pull_brmsfit(object), object$bayesnecformula,
               newdata = newdata, ...)
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
  # all_models is ignored, not mapped as plot() and autoplot() map it:
  # mapping it would turn the matrix such a call returns into a list.
  if ("all_models" %in% ...names()) {
    warning("`all_models` is not an argument of predict() for a model set ",
            "and is ignored, as it always has been. Use `model` to name ",
            "equations whose predictions to return and `average` to choose ",
            "whether the model averaged predictions are returned.",
            call. = FALSE)
  }
  if (is.null(shown$model)) {
    return(predict_manec_average(object, summary = summary, robust = robust,
                                 probs = probs, ...))
  }
  out <- list()
  # Compute the model average before the equations, which also draw from the
  # random number stream, so that it matches predict() with model = NULL.
  if (shown$average) {
    out$average <- predict_manec_average(object, summary = summary,
                                         robust = robust, probs = probs, ...)
  }
  for (m in shown$model) {
    # Use the equation's brmsfit directly; pull_out() would rebuild the whole
    # bayesnecfit to reach it.
    out[[m]] <- disp_predict(object$mod_fits[[m]]$fit,
                             object$mod_fits[[m]]$bayesnecformula,
                             summary = summary, robust = robust,
                             probs = probs, ...)
  }
  out
}

#' The model averaged predictions of a model set
#'
#' Summarises the model-averaged \code{\link{posterior_predict}} draws, as
#' \code{predict.bayesmanecfit()} returns them with \code{model = NULL}.
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
