#' Extract Diagnostic Quantities of 'brms' Models
#'
#' Extract Rhat statistic that can be used to diagnose sampling behaviour
#' of the algorithms applied by 'Stan' at the back-end of 'brms'.
#' \code{object} should be of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @name rhat
#' @order 1
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#' @param rhat_cutoff A \code{\link[base]{numeric}} vector indicating the Rhat
#' cut-off used to test for model convergence.
#' @param ... Unused.
#'
#' @return A \code{\link[base]{list}} containing a vector or Rhat values
#' returned for each parameter for a \code{\link[brms]{brmsfit}} object,
#' for each of the fitted models.
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' rhat(manec_example)
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' rhat(nec4param)
#' }
NULL

#' @rdname rhat
#' @order 2
#'
#' @method rhat bayesnecfit
#'
#' @inherit rhat description return examples
#'
#' @importFrom brms rhat
#' @importFrom chk chk_numeric
#'
#' @export
rhat.bayesnecfit <- function(object, rhat_cutoff = 1.05, ...) {
  chk_numeric(rhat_cutoff)
  rhat_vals <- pull_brmsfit(object) |>
    rhat() |>
    clean_rhat_names()
  failed <- any(rhat_vals > rhat_cutoff)
  out <- list(list(rhat_vals = rhat_vals, failed = failed))
  names(out) <- object$model
  out
}

#' @rdname rhat
#' @order 3
#'
#' @method rhat bayesmanecfit
#'
#' @inherit rhat description return examples
#'
#' @importFrom brms rhat
#'
#' @export
rhat.bayesmanecfit <- function(object, rhat_cutoff = 1.05, ...) {
  out <- sapply(object$success_models, function(x, object, ...) {
    pull_out(object, model = x) |>
      suppressMessages() |>
      rhat(...)
  }, object = object, rhat_cutoff = rhat_cutoff, USE.NAMES = FALSE)
  failed <- sapply(out, "[[", "failed")
  if (all(failed)) {
    message("All models failed the rhat_cutoff of ", rhat_cutoff)
  }
  out
}

#' @noRd
clean_rhat_names <- function(x) {
  y <- names(x)
  names(x) <- gsub("^b\\_", "", y) |>
    (\(.)gsub("\\_b\\_", "_", .))() |>
    (\(.)gsub("\\_Intercept$", "", .))()
  x
}
