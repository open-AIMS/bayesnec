#' Generates posterior linear predictions for objects fitted by
#' \code{\link{bnec}}
#'
#' Generates posterior linear predictions for objects fitted by
#' \code{\link{bnec}}. \code{object} should be of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @name posterior_epred
#' @aliases posterior_epred
#' @order 1
#'
#' @usage NULL
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#' @param ... Additional arguments to
#' \code{\link[brms]{posterior_epred}}.
#'
#' @method posterior_epred bnecfit
#'
#' @importFrom brms posterior_predict
#'
#' @return See \code{?brms:posterior_epred}.
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' # Uses default `precision` and `x_range` to generate `newdata` internally
#' posterior_epred(manec_example)
#' # Provide user-specified `newdata`
#' nd_ <- data.frame(x = seq(0, 3, length.out = 200))
#' ppreds <- posterior_epred(manec_example, ecx_val = 50, newdata = nd_,
#'                             make_newdata = FALSE)
#' ncol(ppreds) == 200 # cols are x, rows are iterations
#' # Predictions for raw input data
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' preds <- posterior_epred(nec4param, make_newdata = FALSE)
#' x <- pull_brmsfit(nec4param)$data$x
#' plot(sort(x), preds[1, order(x)], type = "l", col = alpha("black", 0.1),
#'      ylim = c(-6, 3))
#' for (i in seq_len(nrow(preds))[-1]) {
#'   lines(sort(x), preds[i, order(x)], type = "l", col = alpha("black", 0.1))
#' }
#' }
#'
#' @export
#' @export posterior_epred
posterior_epred.bnecfit <- function(object, ...) {
  UseMethod("posterior_epred")
}

#' @rdname posterior_epred
#' @order 2
#'
#' @method posterior_epred bayesnecfit
#'
#' @inherit posterior_epred description return examples
#'
#' @importFrom brms posterior_epred
#'
#' @export
posterior_epred.bayesnecfit <- function(object, ...) {
  posterior_epred(pull_brmsfit(object), ...)
}

#' @rdname posterior_epred
#' @order 3
#'
#' @method posterior_epred bayesmanecfit
#'
#' @inherit posterior_epred description return examples
#'
#' @importFrom brms posterior_epred
#'
#' @export
posterior_epred.bayesmanecfit <- function(object, ...) {
  mod_fits <- object$mod_fits
  model_set <- names(mod_fits)
  mod_stats <- object$mod_stats
  pred_list <- lapply(mod_fits, function(x, ...) {
    posterior_epred(x$fit, ...)
  }, ...)
  sample_size <- min(sapply(pred_list, nrow))
  do_wrapper(model_set, w_pred_list_calc, pred_list, sample_size,
             mod_stats, fct = "rbind")
}
