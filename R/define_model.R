#    Copyright 2020 Australian Institute of Marine Science
#
#    Licensed under the Apache License, Version 2.0 (the "License");
#    you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.

#' define_model
#'
#' Writes an NEC model file for a three parameter model (top, beta and NEC) and generates a function for initial values to pass to jags
#' 
#' @param x_type the statistical distribution to use for the x (concentration) data. This may currently be one of  'beta', 'gaussian', or 'gamma'. Others can be added as required, please contact the package maintainer.
#' 
#' @param y_type the statistical distribution to use for the y (response) data. This may currently be one of  'binomial', 'beta', 'poisson', 'gaussian', or 'gamma'. Others can be added as required, please contact the package maintainer.
#'
#' @param model a character string indicating the model to fit
#'
#' @param mod_dat the model data to use for the NEC model fit
#'
#' @export
#' @return a model formula, priors and the family to use
#' @importFrom brms prior bf
#' @importFrom base qlogis

define_model <- function(model, x_type, y_type, mod_dat) {
 
  # Prior for beta - decay slope ---------
  # Currently the same prior for all models.
  priors <- brms::prior(normal(0, 10), nlpar = "beta", lb = 0)
  prior_slope <- brms::prior(normal(0, 100), nlpar = "slope", lb = 0)
  
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
                brms::prior_string(paste0("normal(", prior_top, ", 5)"), nlpar = "top")) 
  
  # x_type and prior for 'nec' --------------
     if(x_type=="beta"){
       prior_nec <- brms::prior(uniform(0.0001, 0.9999), nlpar = "nec")   
     }
     if(x_type=="gamma"){
       prior_nec <- brms::prior(normal(0, 100), nlpar = "nec", lb = 0)
     }
     if(x_type=="gaussian"){
       prior_nec <- brms::prior(normal(3, 100), nlpar = "nec") 
     }   
       
  # nec3param ------
  # as per Fox 2010
  if(model=="nec3param"){
    if(y_type=="binomial"){
           bform <- brms::bf(y | trials(trials) ~ top *
                           exp(-beta * (x - nec) *
                                 step(x - nec)),
                         top + beta + nec ~ 1,
                         nl = TRUE)
    }else{
           bform <- brms::bf(y ~ top *
                          exp(-beta * (x - nec) *
                                step(x - nec)),
                        top + beta + nec ~ 1,
                        nl = TRUE)
    }
  priors <- priors + prior_nec
    
  }

  # Exponential decay ----
  if(model=="ecxexp"){
    if(y_type=="binomial"){
      bform <- brms::bf(y | trials(trials) ~ top * exp(-beta * x),
                        top + beta ~ 1,
                        nl = TRUE)
    }else{
      bform <- brms::bf(y ~ top * exp(-beta * x),
                        top + beta ~ 1,
                        nl = TRUE)
    }
  } 
  
  # Simple linear decay ----
  if(model=="ecxlin"){
    if(y_type=="binomial"){
      bform <- brms::bf(y | trials(trials) ~ top - beta * x,
                        top + beta ~ 1,
                        nl = TRUE)
    }else{
      bform <- brms::bf(y ~ top - beta * x,
                        top + beta ~ 1,
                        nl = TRUE)
    }
  } 
  
  # Sigmoidal decay ----
  if(model=="ecxsigm"){
    if(y_type=="binomial"){
      bform <- brms::bf(y | trials(trials) ~ top * exp(-beta * x)^d,
                        d + top + beta ~ 1,
                        nl = TRUE)
    }else{
      bform <- brms::bf(y ~ top * exp(-beta * x)^d,
                        d + top + beta ~ 1,
                        nl = TRUE)
    }
  priors <- priors + 
      brms::prior(normal(0, 100), nlpar = "d")   
  } 

  # nec4param ----
  # as per Fox 2010 but with an estimate for the lower plateau 
  if(model=="nec4param"){
    if(y_type=="binomial"){
      bform <- brms::bf(y | trials(trials) ~ bot + (bot-top) *
                          exp(-beta * (x - nec) *
                                step(x - nec)),
                        bot + top + beta + nec ~ 1,
                        nl = TRUE)
    }else{
      bform <- brms::bf(y ~ bot + (bot-top) *
                          exp(-beta * (x - nec) *
                                step(x - nec)),
                        bot + top + beta + nec ~ 1,
                        nl = TRUE)
    }
    
    priors <- c(priors, 
                brms::prior_string(paste0("normal(", prior_bot, ", 100)"), nlpar = "bot"),
                prior_nec)    
    
  }
  
  # wb1 --------------
  if(model=="ecxwb1"){
    if(y_type=="binomial"){
      bform <- brms::bf(y | trials(trials) ~ bot + (top-bot) * exp(-exp(beta*(x - ec50))),
                        ec50 + top + beta ~ 1,
                        nl = TRUE)
    }else{
      bform <- brms::bf(y ~ bot + (top-bot) * exp(-exp(beta*(x - ec50))),
                        ec50 + top + beta ~ 1,
                        nl = TRUE)
    }
    priors <- c(priors, 
                brms::prior_string(paste0("normal(", prior_bot, ", 100)"), nlpar = "bot"),
                brms::prior_string(paste0("normal(", prior_ec50, ", 100)"), nlpar = "ec50"))  
  }  
  
  # wb2 --------------
  if(model=="ecxwb2"){
    if(y_type=="binomial"){
      bform <- brms::bf(y | trials(trials) ~ bot + (top-bot) * (1-exp(-exp(beta*(x - ec50)))),
                        ec50 + top + beta ~ 1,
                        nl = TRUE)
    }else{
      bform <- brms::bf(y ~ bot + (top-bot) * (1-exp(-exp(beta*(x - ec50)))),
                        ec50 + top + beta ~ 1,
                        nl = TRUE)
    }
    priors <- c(priors, 
                brms::prior_string(paste0("normal(", prior_bot, ", 100)"), nlpar = "bot"),
                brms::prior_string(paste0("normal(", prior_ec50, ", 100)"), nlpar = "ec50"))  
  }
  
  # nechorme ----
  # as per Fox 2010 but with an increase slope prior to nec
  if(model=="nechorme"){
    if(y_type=="binomial"){
      bform <- brms::bf(y | trials(trials) ~ (top + slope*x) *
                          exp(-beta * (x - nec) *
                                step(x - nec)),
                        top + beta + nec + slope ~ 1,
                        nl = TRUE)
    }else{
      bform <- brms::bf(y ~ (top + slope*x) *
                          exp(-beta * (x - nec) *
                                step(x - nec)),
                        top + beta + nec + slope ~ 1,
                        nl = TRUE)
   }
    
   priors <- c(priors, prior_nec, prior_slope) 
   
   }

  # necsigmoidal ------
  # as per Fox 2010 but with a sigmoidal decay function
  if(model=="necsigmoidal"){
    if(y_type=="binomial"){
      bform <- brms::bf(y | trials(trials) ~ top *
                          exp(-beta * (x - nec)^d *
                                step(x - nec)),
                        top + beta + nec + d ~ 1,
                        nl = TRUE)
    }else{
      bform <- brms::bf(y ~ top *
                          exp(-beta * (x - nec)^d *
                                step(x - nec)),
                        top + beta + nec + d ~ 1,
                        nl = TRUE)
    }
    priors <- priors + prior_nec + brms::prior(normal(0, 100), nlpar = "d") 
    
  }  
  
  # Return outcomes ---- 
  #the model formula, priors and family to use in the model fit
  return(list(bform=bform, priors=priors, mod_family=mod_family))
  
  
}
