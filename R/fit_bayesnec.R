#' fit_bayesnec
#'
#' Fits a concentration(dose)-response model using brms
#'
#' @inheritParams bnec
#'
#' @param skip_check Should data check via \code{\link{check_data}}
#' be avoided? Only relevant to function \code{\link{amend}}.
#' Defaults to FALSE.
#'
#' @importFrom brms brm loo waic
#' @importFrom stats update
#'
#' @seealso \code{\link{bnec}}
#' @return The fitted \pkg{brms} model, including an estimate of the NEC
#' value and predicted posterior values.
#' A posterior sample of the NEC is also available under \code{nec_posterior}
fit_bayesnec <- function(data, x_var, y_var, trials_var = NA,
                         family = NULL, priors, model = NA,
                         inits, skip_check = FALSE, ...) {
  if (skip_check) {
    mod_dat <- data
    if (family$family != "binomial") {
      response <- data$y
    } else {
      response <- data$y / data$trials
    }
    priors <- validate_priors(priors, model)
    if (is.null(priors)) {
      priors <- define_prior(model = model, family = family,
                             predictor = data$x, response = response)
    }
  } else {
    data_check <- check_data(data = data, x_var = x_var, y_var = y_var,
                             trials_var = trials_var, family = family,
                             model = model)
    mod_dat <- data_check$mod_dat
    family <- data_check$family
    priors <- validate_priors(priors, model)
    if (is.null(priors)) {
      priors <- data_check$priors
    }
    if (family$family != "binomial") {
      response <- mod_dat$y
    } else {
      response <- mod_dat$y / mod_dat$trials
    }
  }
  suffix <- ifelse(family$family == "binomial", "_binom", "_deflt")
  brms_bf <- get(paste0("bf_", model, suffix))
  add_args <- list(...)
  if (!("chains" %in% names(add_args))) {
    add_args[["chains"]] <- 4
  }
  chs <- add_args$chains
  if (missing(inits) | skip_check) {
    inits <- make_good_inits(model, mod_dat$x,
                             response, priors = priors,
                             chains = chs)
  }
  all_args <- c(list(formula = brms_bf, data = mod_dat,
                     family = family, prior = priors,
                     inits = inits),
                add_args)
  fit <- do.call(brm, all_args)
  w <- 1
  n_tries <- 5
  pass <- are_chains_correct(fit, chs)
  # try with bayesnec random initial values
  while (!pass & w < n_tries) {
    inits <- make_good_inits(model, mod_dat$x,
                             response, priors = priors,
                             chains = chs)
    up_args <- c(list(object = fit, inits = inits),
                 add_args)
    fit <- do.call(update, up_args)
    pass <- are_chains_correct(fit, chs)
    if (!pass) {
      inits <- make_good_inits(model, mod_dat$x,
                               response, priors = priors,
                               chains = chs,
                               stan_like = TRUE)
      up_args <- c(list(object = fit, inits = inits),
                   add_args)
      fit <- do.call(update, up_args)
      pass <- are_chains_correct(fit, chs)
    }
    w <- w + 1
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
