#' Generates posterior linear predictions for objects fitted by
#' \code{\link{bnec}}
#'
#' Generates posterior linear predictions for objects fitted by
#' \code{\link{bnec}}. \code{object} should be of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @name posterior_epred
#' @order 1
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#' @param newdata An optional \code{\link[base]{data.frame}} to predict over.
#' Where it is \code{NULL}, predictions use the fitted rows. A fit with a
#' \code{disp(~...)} term requires every column named by that term in a
#' supplied \code{newdata}.
#' @param ... Additional arguments to
#' \code{\link[brms]{posterior_epred}}.
#'
#' @return See \code{?brms:posterior_epred}.
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' # Uses default `resolution` and `x_range` to generate `newdata` internally
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
NULL

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
posterior_epred.bayesnecfit <- function(object, newdata = NULL, ...) {
  # Refused here with the term named, rather than by brms; see
  # check_newdata_disp().
  check_newdata_disp(object$bayesnecformula, newdata)
  factorised_count_epred(
    pull_brmsfit(object), object$bayesnecformula, newdata = newdata, ...
  )
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
posterior_epred.bayesmanecfit <- function(object, newdata = NULL, ...) {
  mod_fits <- object$mod_fits
  model_set <- names(mod_fits)
  check_newdata_disp(mod_fits[[1]]$bayesnecformula, newdata)
  pred_list <- lapply(mod_fits, function(x, ...) {
    factorised_count_epred(x$fit, x$bayesnecformula,
                           newdata = newdata, ...)
  }, ...)
  sample_size <- min(sapply(pred_list, nrow))
  # Use the index stored when the object was built, so that repeated calls
  # return identical draws.
  draw_index <- pull_draw_index(object, model_set, sample_size)
  do_wrapper(model_set, w_pred_list_calc, pred_list, draw_index, fct = "rbind")
}
