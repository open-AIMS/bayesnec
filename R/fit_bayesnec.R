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
                         inits, skip_check = FALSE, ...) {
  if (skip_check) {
    mod_dat <- data
    if (missing(priors)) {
      if (family$family != "binomial") {
        response <- data$y
      } else {
        response <- data$y / data$trials
      }
      priors <- define_prior(model = model, family = family,
                             predictor = data$x, response = response)
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
  add_args <- list(...)
  chs <- 4
  if ("chains" %in% names(add_args)) {
    chs <- add_args$chains
  }
  if (missing(inits) | skip_check) {
    inits <- make_inits(priors, chs)
  }
  fit <- brm(formula = brms_bf, data = mod_dat, family = family,
             prior = priors, inits = 'random', ...)
  w <- 1
  n_tries <- 5
  pass <- are_chains_correct(fit, chs)
  # try with bayesnec random initial values
  while (!pass & w < n_tries) {
    inits <- make_inits(priors, chs)
    fit <- update(fit, inits = inits, ...)
    pass <- are_chains_correct(fit, chs)
    if (!pass) {
      inits <- make_inits(priors, chs, stan_like = TRUE)
      fit <- update(fit, inits = inits, ...)
      pass <- are_chains_correct(fit, chs)   
    }

    w <- w + 1
  }
  
  if (!pass) {
    fit <- brm(formula = brms_bf, data = mod_dat, family = family,
               prior = priors,  ...) 
    pass <- are_chains_correct(fit, chs)     
  }
  if (!pass) {
    stop(paste0("Failed to fit model ", model, "."),
         call. = FALSE)
  }
  fit$loo <- loo(fit)
  fit$waic <- waic(fit)
  message(paste0("Response variable modelled as a ",
                 model, " model using a ", family$family,
                 " distribution."))
  out <- list(fit = fit, model = model, inits = inits)
  allot_class(out, "prebayesnecfit")
}
