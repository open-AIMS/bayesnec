#' Fits brm models based on model type and distribution family
#'
#' @param model A \code{\link[base]{character}} vector indicating the model(s)
#' to fit.
#' @param family_code The statistical distribution to use for the y (response) data.
#' @param new_priors An object of class \code{\link[brms]{brmsprior}}.
#' @param new_data A \code{\link[base]{data.frame}}.
#' @param ... Arguments passed to \code{\link[brms]{update}}
#' (e.g. iter, chains).
#' @return An object of class \code{\link[brms]{brmsfit}} returned
#' by \code{\link[brms]{update}}
#' @seealso \code{\link{alter_model}}
#' @importFrom stats update
#' @importFrom brms prior_summary
fit_stan <- function(model, family_code, new_priors = NULL,
                     new_data = NULL, ...) {
  brms_fit <- get(paste0(model, "_brms_", family_code))
  stan_mod <- paste0(model, family_code)
  brms_fit$fit@stanmodel <- stanmodels[[stan_mod]]
  if (!is.null(new_priors) | !is.null(new_data)) {
    new_model_str <- alter_model(brms_fit, new_priors = new_priors,
                                 new_data = new_data)
    brms_fit <- new_model_str$brmodel
    brpriors <- new_model_str$brpriors
    brdata <- new_model_str$brdata
  } else {
    brpriors <- prior_summary(brms_fit)
    brdata <- brms_fit$data
  }
  update(brms_fit, newdata = brdata, prior = brpriors, ...)
}
