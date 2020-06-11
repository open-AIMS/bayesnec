#' fit_bayesnec
#'
#' Fits a concentration(dose)-response model using brms
#'
#' @inheritParams bnec
#'
#' @param skip_check Should data check via \code{\link{check_data}}
#' be avoided? Only relevant to function \code{\link{modify}}.
#' Defaults to FALSE.
#'
#' @importFrom brms brm loo waic
#' @seealso \code{\link{bnec}}
#' @return The fitted \pkg{brms} model, including an estimate of the NEC
#' value and predicted posterior values.
#' A posterior sample of the NEC is also available under \code{nec_posterior}
fit_bayesnec <- function(data, x_var, y_var, trials_var = NA,
                         family = NULL, priors, model = NA,
                         skip_check = FALSE, ...) {

  if (skip_check) {
    mod_dat <- data
    if (missing(priors)) {
      response <- ifelse(family$family != "binomial", data$y,
                         data$y / data$trials)
      x_type <- set_distribution(data$x)
      priors <- define_prior(model = model, x_type = x_type,
                             family = family, response = response)
    }
  } else {
    data_check <- check_data(data = data, x_var = x_var, y_var = y_var,
                             trials_var = trials_var, family = family,
                             model = model)
    mod_dat <- data_check$mod_dat
    family <- data_check$family
    if (missing(priors)) {
      priors <- data_check$priors
    }
  }

  suffix <- ifelse(family$family == "binomial", "_binom", "_deflt")
  brms_bf <- get(paste0("bf_", model, suffix))
  fit <- brm(formula = brms_bf, data = mod_dat, family = family,
             prior = priors, ...)
  fit$loo <- loo(fit)
  fit$waic <- waic(fit)

  message(paste0("Response variable modelled as a ",
                 model, " model using a ", family$family,
                 " distribution."))

  out <- list(fit = fit, model = model)
  allot_class(out, "prebayesnecfit")
}
