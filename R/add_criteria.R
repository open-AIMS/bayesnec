#' add_criteria
#'
#' Adds LOO and WAIC to a prebayesnecfit object
#'
#' @inheritParams bnec
#'
#' @param object An object of class prebayesnecfit.
#' @param loo_f_controls A named \code{\link[base]{list}} containing the
#' desired arguments to be passed on to \code{\link[brms]{loo}}.
#'
#' @return An object of class prebayesnecfit.
#' @importFrom brms add_criterion
add_criteria <- function(object, loo_f_controls, model) {
  fit <- object$fit
  local_loo_list <- list(x = fit, criterion = c("loo", "waic"))
  if (!"model_name" %in% names(loo_f_controls)) {
    loo_f_controls <- c(loo_f_controls, list(model_name = model))
  }
  loo_args <- c(local_loo_list, loo_f_controls)
  fit <- do.call(add_criterion, loo_args)
  object$fit <- fit
  object
}
