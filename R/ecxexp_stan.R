#' Fits model ECX_EXP
#'
#' @param new_priors An object of class \code{\link[brms]{brmsprior}}.
#' @param new_data A \code{\link[base]{data.frame}}.
#' @param ... Arguments passed to \code{\link[brms]{update}}
#' (e.g. iter, chains).
#' @return An object of class \code{\link[brms]{brmsfit}} returned
#' by \code{\link[brms]{update}}
#' @seealso \code{\link{alter_model}}
#' @importFrom stats update
#' @importFrom brms prior_summary
ecxexp_stan <- function(new_priors = NULL, new_data = NULL, ...) {
  ecxexp_brms$fit@stanmodel <- stanmodels$ecxexp
  if (!is.null(new_priors) | !is.null(new_data)) {
    new_model_str <- alter_model(ecxexp_brms, new_priors = new_priors,
                                 new_data = new_data)
    ecxexp_brms <- new_model_str$brmodel
    brpriors <- new_model_str$brpriors
    brdata <- new_model_str$brdata
  } else {
    brpriors <- prior_summary(ecxexp_brms)
    brdata <- ecxexp_brms$data
  }
  update(ecxexp_brms, newdata = brdata, prior = brpriors, ...)
}
