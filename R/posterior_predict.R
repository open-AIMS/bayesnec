#' Generates posterior predictions for objects fitted by \code{\link{bnec}}
#'
#' Generates posterior predictions for objects fitted by \code{\link{bnec}}.
#' \code{object} should be of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#'
#' @name posterior_predict
#' @order 1
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#' @param ... Additional arguments to
#' \code{\link[brms]{posterior_predict}}.
#'
#' @return See \code{?brms::posterior_predict}.
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' # Uses default `precision` and `x_range` to generate `newdata` internally
#' posterior_predict(manec_example)
#' # Provide user-specified `newdata`
#' nd_ <- data.frame(x = seq(0, 3, length.out = 200))
#' ppreds <- posterior_predict(manec_example, ecx_val = 50, newdata = nd_,
#'                             make_newdata = FALSE)
#' ncol(ppreds) == 200 # cols are x, rows are iterations
#' # Posterior predictions for raw input data
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' preds <- posterior_predict(nec4param, make_newdata = FALSE)
#' x <- pull_brmsfit(nec4param)$data$x
#' plot(sort(x), preds[1, order(x)], type = "l", col = alpha("black", 0.1),
#'      ylim = c(-8, 5))
#' for (i in seq_len(nrow(preds))[-1]) {
#'   lines(sort(x), preds[i, order(x)], type = "l", col = alpha("black", 0.1))
#' }
#' }
NULL

#' @rdname posterior_predict
#' @order 2
#'
#' @method posterior_predict bayesnecfit
#'
#' @inherit posterior_predict description return examples
#'
#' @importFrom brms posterior_predict
#'
#' @export
posterior_predict.bayesnecfit <- function(object, ...) {
  posterior_predict(pull_brmsfit(object), ...)
}

#' @rdname posterior_predict
#' @order 3
#'
#' @method posterior_predict bayesmanecfit
#'
#' @inherit posterior_predict description return examples
#'
#' @importFrom brms posterior_predict
#'
#' @export
posterior_predict.bayesmanecfit <- function(object, ...) {
  mod_fits <- object$mod_fits
  model_set <- names(mod_fits)
  mod_stats <- object$mod_stats
  pred_list <- lapply(mod_fits, function(x, ...) {
    posterior_predict(x$fit, ...)
  }, ...)
  sample_size <- min(sapply(pred_list, nrow))
  do_wrapper(model_set, w_pred_list_calc, pred_list, sample_size,
             mod_stats, fct = "rbind")
}
