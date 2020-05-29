#' define_model
#'
#' Writes an NEC model file for a three parameter model (top, beta and NEC) and generates a function for initial values to pass to jags
#' @param x_type the statistical distribution to use for the x (concentration) data. 
#' This may currently be one of  'beta', 'gaussian', or 'gamma'. 
#' Others can be added as required, please contact the package maintainer.
#' 
#' @param y_type the statistical distribution to use for the y (response) data. 
#' This may currently be one of  'binomial', 'beta', 'poisson', 'gaussian', 'negbin' or 'gamma'. 
#' Others can be added as required, please contact the package maintainer.
#'
#' @param model a character string indicating the model to fit
#'
#' @param mod_dat the model data to use for the NEC model fit
#'
#' @export
#' @return a model formula, priors and the family to use
#' @importFrom brms bf prior_string negbinomial Beta
#' @importFrom stats qlogis binomial quantile Gamma poisson gaussian

define_model <- function(model, x_type, y_type, mod_dat) {
 
  # Prior for beta - decay slope ---------
  # Currently the same prior for all models.
  priors <- prior_string("normal(0, 10)", nlpar = "beta", lb = 0)
  prior_slope <- prior_string("normal(0, 100)", nlpar = "slope", lb = 0)
  
  # y_type family and prior for 'top', 'bot', 'ec50' ------------
  if(y_type=="binomial"){
    mod_family <- binomial()
    prior_top <- quantile(qlogis(mod_dat$y/mod_dat$trials), probs = 0.8)
    prior_bot <- quantile(qlogis(mod_dat$y/mod_dat$trials), probs = 0.2)
    prior_ec50 <- quantile(qlogis(mod_dat$y/mod_dat$trials), probs = 0.5)
    }
  if(y_type=="gamma"){
    mod_family <- Gamma()
    prior_top <- quantile(log(mod_dat$y), probs = 0.8)
    prior_bot <- quantile(log(mod_dat$y), probs = 0.2)
    prior_ec50 <- quantile(log(mod_dat$y), probs = 0.5)
    }
  if(y_type=="poisson"){
    mod_family <- poisson()
    prior_top <- quantile(log(mod_dat$y), probs = 0.8)
    prior_bot <- quantile(log(mod_dat$y), probs = 0.2)
    prior_ec50 <- quantile(log(mod_dat$y), probs = 0.5)
    }
  if(y_type=="gaussian"){
    mod_family <- gaussian()
    prior_top <- quantile(mod_dat$y, probs = 0.8)
    prior_bot <- quantile(mod_dat$y, probs = 0.2)   
    prior_ec50 <- quantile(mod_dat$y, probs = 0.5)   
    }
  if(y_type=="beta"){
    mod_family <- Beta()
    prior_top <- quantile(qlogis(mod_dat$y), probs = 0.8)
    prior_bot <- quantile(qlogis(mod_dat$y), probs = 0.2)
    prior_ec50 <- quantile(qlogis(mod_dat$y), probs = 0.5)
    }
  if(y_type=="negbin"){
    mod_family <- negbinomial()
    prior_top <- quantile(log(mod_dat$y), probs = 0.8)
    prior_bot <- quantile(log(mod_dat$y), probs = 0.2)
    prior_ec50 <- quantile(log(mod_dat$y), probs = 0.5)
    }
  
  priors <- c(priors, 
                prior_string(paste0("normal(", prior_top, ", 5)"), nlpar = "top")) 
  
  # x_type and prior for 'nec' --------------
     if(x_type=="beta"){
       prior_nec <- prior_string("uniform(0.0001, 0.9999)", nlpar = "nec")   
     }
     if(x_type=="gamma"){
       prior_nec <- prior_string("normal(0, 100)", nlpar = "nec", lb = 0)
     }
     if(x_type=="gaussian"){
       prior_nec <- prior_string("normal(3, 100)", nlpar = "nec") 
     }   
       
  # nec3param ------
  # as per Fox 2010
  if(model=="nec3param"){
    if(y_type=="binomial"){
           bform <- bf(y | trials(trials) ~ top *
                           exp(-beta * (x - nec) *
                                 step(x - nec)),
                         top + beta + nec ~ 1,
                         nl = TRUE)
    }else{
           bform <- bf(y ~ top *
                          exp(-beta * (x - nec) *
                                step(x - nec)),
                        top + beta + nec ~ 1,
                        nl = TRUE)
    }
  priors <- priors + prior_nec
    
  }

  # ecxexp ----
  # Exponential decay
  if(model=="ecxexp"){
    if(y_type=="binomial"){
      bform <- bf(y | trials(trials) ~ top * exp(-beta * x),
                        top + beta ~ 1,
                        nl = TRUE)
    }else{
      bform <- bf(y ~ top * exp(-beta * x),
                        top + beta ~ 1,
                        nl = TRUE)
    }
  } 
  
  # ecxlin ----
  # Simple linear decay 
  if(model=="ecxlin"){
    if(y_type=="binomial"){
      bform <- bf(y | trials(trials) ~ top - beta * x,
                        top + beta ~ 1,
                        nl = TRUE)
    }else{
      bform <- bf(y ~ top - beta * x,
                        top + beta ~ 1,
                        nl = TRUE)
    }
  } 
  
  # ecxsigm ----
  # Sigmoidal decay
  if(model=="ecxsigm"){
    if(y_type=="binomial"){
      bform <- bf(y | trials(trials) ~ top * exp(-beta * x)^d,
                        d + top + beta ~ 1,
                        nl = TRUE)
    }else{
      bform <- bf(y ~ top * exp(-beta * x)^d,
                        d + top + beta ~ 1,
                        nl = TRUE)
    }
  priors <- priors + 
      prior_string("normal(0, 100)", nlpar = "d")
  } 
  
 
  # ecx4param --------------
  if(model=="ecx4param"){
    if(y_type=="binomial"){
      bform <- bf(y | trials(trials) ~  top + (bot-top)/(1+exp((ec50-x)*beta)),
                        bot + ec50 + top + beta ~ 1,
                        nl = TRUE)
    }else{
      bform <- bf(y ~  top + (bot-top)/(1+exp((ec50-x)*beta)),
                        bot + ec50 + top + beta ~ 1,
                        nl = TRUE)
    }
    priors <- c(priors, 
                prior_string(paste0("normal(", prior_bot, ", 100)"), nlpar = "bot"),
                prior_string(paste0("normal(", prior_ec50, ", 100)"), nlpar = "ec50"))  
  } 
  
 
  # nec4param ----
  # as per Fox 2010 but with an estimate for the lower plateau 
  if(model=="nec4param"){
    if(y_type=="binomial"){
      bform <- bf(y | trials(trials) ~ bot + (bot-top) *
                          exp(-beta * (x - nec) *
                                step(x - nec)),
                        bot + top + beta + nec ~ 1,
                        nl = TRUE)
    }else{
      bform <- bf(y ~ bot + (bot-top) *
                          exp(-beta * (x - nec) *
                                step(x - nec)),
                        bot + top + beta + nec ~ 1,
                        nl = TRUE)
    }
    
    priors <- c(priors, 
                prior_string(paste0("normal(", prior_bot, ", 100)"), nlpar = "bot"),
                prior_nec)    
    
  }
  
  # wb1 --------------
  if(model=="ecxwb1"){
    if(y_type=="binomial"){
      bform <- bf(y | trials(trials) ~ bot + (top-bot) * exp(-exp(beta*(x - ec50))),
                        bot + ec50 + top + beta ~ 1,
                        nl = TRUE)
    }else{
      bform <- bf(y ~ bot + (top-bot) * exp(-exp(beta*(x - ec50))),
                        bot + ec50 + top + beta ~ 1,
                        nl = TRUE)
    }
    priors <- c(priors, 
                prior_string(paste0("normal(", prior_bot, ", 100)"), nlpar = "bot"),
                prior_string(paste0("normal(", prior_ec50, ", 100)"), nlpar = "ec50"))  
  }  
  
  # wb2 --------------
  if(model=="ecxwb2"){
    if(y_type=="binomial"){
      bform <- bf(y | trials(trials) ~ bot + (top-bot) * (1-exp(-exp(beta*(x - ec50)))),
                        bot + ec50 + top + beta ~ 1,
                        nl = TRUE)
    }else{
      bform <- bf(y ~ bot + (top-bot) * (1-exp(-exp(beta*(x - ec50)))),
                        bot + ec50 + top + beta ~ 1,
                        nl = TRUE)
    }
    priors <- c(priors, 
                prior_string(paste0("normal(", prior_bot, ", 100)"), nlpar = "bot"),
                prior_string(paste0("normal(", prior_ec50, ", 100)"), nlpar = "ec50"))  
  }
  
  # nechorme ----
  # as per Fox 2010 but with an increase slope prior to nec
  if(model=="nechorme"){
    if(y_type=="binomial"){
      bform <- bf(y | trials(trials) ~ (top + slope*x) *
                          exp(-beta * (x - nec) *
                                step(x - nec)),
                        top + beta + nec + slope ~ 1,
                        nl = TRUE)
    }else{
      bform <- bf(y ~ (top + slope*x) *
                          exp(-beta * (x - nec) *
                                step(x - nec)),
                        top + beta + nec + slope ~ 1,
                        nl = TRUE)
   }
    
   priors <- c(priors, prior_nec, prior_slope) 
   
   }

  # necsigm ------
  # as per Fox 2010 but with a sigmoidal decay function
  if(model=="necsigm"){
    if(y_type=="binomial"){
      bform <- bf(y | trials(trials) ~ top *
                          exp(-beta * (x - nec)^d *
                                step(x - nec)),
                        top + beta + nec + d ~ 1,
                        nl = TRUE)
    }else{
      bform <- bf(y ~ top *
                          exp(-beta * (x - nec)^d *
                                step(x - nec)),
                        top + beta + nec + d ~ 1,
                        nl = TRUE)
    }
    priors <- priors + prior_nec + prior_string("normal(0, 100)", nlpar = "d")
    
  }  
  
  # Return outcomes ---- 
  #the model formula, priors and family to use in the model fit
  return(list(bform=bform, priors=priors, mod_family=mod_family))
  
  
}
