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

  pass <- FALSE
  w <- 1
  n_tries <- 5
  
  suffix <- ifelse(family$family == "binomial", "_binom", "_deflt")
  brms_bf <- get(paste0("bf_", model, suffix))
  add_args <- list(...)
  
  if (missing(inits) | skip_check) {
    if ("chains" %in% names(add_args)) {
      chs <- add_args$chains
    } else {
      # brms default
      chs <- 4
    }
    inits <- make_inits(priors, chs)
  }

  fit <- brm(formula = brms_bf, data = mod_dat, family = family,
               prior = priors, inits = inits, ...)
  fit.chs <- fit$fit@sim$chains
  if(is.null(fit.chs)){
    pass <- FALSE
  }else{
    if(fit.chs==chs){
      pass <- TRUE
    }
    if(fit.chs!=chs){
      pass <- FALSE
    }    
  }

  while(pass==FALSE & w < n_tries){
    inits <- make_inits(priors, chs)
    #fit <- brm(formula = brms_bf, data = mod_dat, family = family,
    #           prior = priors, inits = inits, ...)
    update(fit, inits = inits)
    fit.chs <- fit$fit@sim$chains
    if(is.null(fit.chs)){
      pass <- FALSE
    }else{
      if(fit.chs==chs){
        pass <- TRUE
      }
      if(fit.chs!=chs){
        pass <- FALSE
      }    
    }
    w <- w+1
  }
  
  if(pass==FALSE){ # try with brms default initial values
    fit <- brm(formula = brms_bf, data = mod_dat, family = family,
               prior = priors, ...)
  }  
  fit.chs <- fit$fit@sim$chains
  if(is.null(fit.chs)){
    pass <- FALSE
  }else{
    if(fit.chs==chs){
      pass <- TRUE
    }
    if(fit.chs!=chs){
      pass <- FALSE
    }    
  }
  
  if(pass==FALSE){stop(paste("Failed to fit model ", model, ".", sep=""), call. = FALSE)}

  fit$loo <- loo(fit)
  fit$waic <- waic(fit)

  message(paste0("Response variable modelled as a ",
                 model, " model using a ", family$family,
                 " distribution."))

  out <- list(fit = fit, model = model, inits = inits)
  allot_class(out, "prebayesnecfit")
}
