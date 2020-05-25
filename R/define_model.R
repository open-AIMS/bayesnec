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

define_model <- function(model, x_type, y_type, mod_dat){
 
  # Set prior for beta - decay slope. Currently the same prior for all models.
  priors <- brms::prior(gamma(0.0001, 0.0001), nlpar = "beta")
  
  # Set the y_type family and prior for 'top'
  if(y_type=="binomial"){
    mod_family <- binomial()
    prior_top <- quantile(qlogis(mod_dat$y/mod_dat$trials), probs = 0.8)
    }
  if(y_type=="gamma"){
    mod_family <- Gamma()
    prior_top <- quantile(log(mod_dat$y), probs = 0.8)
    }
  if(y_type=="poisson"){
    mod_family <- poisson()
    prior_top <- quantile(log(mod_dat$y), probs = 0.8)
    }
  if(y_type=="gaussian"){
    mod_family <- gaussian()
    prior_top <- quantile(mod_dat$y, probs = 0.8)
    }
  if(y_type=="beta"){
    mod_family <- Beta()
    prior_top <- quantile(qlogis(mod_dat$y/mod_dat$trials), probs = 0.8)
    }
  if(y_type=="negbin"){
    mod_family <- negbinomial()
    prior_top <- quantile(log(mod_dat$y), probs = 0.8)
    }
  
  priors <- c(priors, 
                brms::prior_string(paste0("normal(", prior_top, ", 100)"), nlpar = "top")) 
       
  # nec3param - as per Fox 2010 ----
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

     if(x_type=="beta"){
       priors <- c(priors, 
         brms::prior(uniform(0.0001, 0.9999), nlpar = "nec"))   
     }
     if(x_type=="gamma"){
       priors <- c(priors,
         brms::prior(normal(0, 100), nlpar = "nec", lb = 0)) 
     }
     if(x_type=="gaussian"){
       priors <- priors + 
         brms::prior(normal(3, 100), nlpar = "nec") 
     }     
  }

  # Simple exponential decay ----
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
  
  # Return outcomes ---- 
  #the model formula, priors and family to use in the model fit
  return(list(bform=bform, priors=priors, mod_family=mod_family))
  
  
}
