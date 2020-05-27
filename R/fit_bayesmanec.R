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

#' fit_bayesmanec
#'
#' Fits a variety of nec models using bayes and provides a model averaged predictions based on DIC model weights
#'
#' @param data A data.frame containing the data to use for the model
#' @param x_var The column heading indicating the concentration (x) variable
#' @param y_var The column heading indicating the response (y) variable
#' @param trials_var The column heading indicating the column for the number of "trials" for binomial response data. If not supplied, the model may run but will not be the model you intended!
#' @param x_type the statistical distribution to use for the x (concentration) data. This will be guess based on the characteristic of the input data if not supplied. As some concentration-response data will use zero concentration, and there is no distribution on the continuous scale from 0 to in (ie tweedie) available in bayes, 
#' a small offset is added (1/10^3 of the next lowest value) to zero values of concentration where these are gamma distributed.
#' @param y_type the statistical distribution to use for the y (response) data. This may currently be one of  'binomial', 'poisson',' 'gaussian', or 'gamma'. Others can be added as required, please contact the package maintainer. 
#' If not supplied, the appropriate distribution will be guessed based on the distribution of the input data.
#' @param iter the number of interations to for the brms fit. Defaults to 2e4.
#' @param over_disp If an overdispersed model should be used. Only changes the model fit for poisson and binomial y_type data. 
#' For poisson, a negative binomial model will be fit. For binomial a beta model will be fit.
#' @param model_set A vector of the names of model types to be fit. Currently defaults to 
#' all available model types. If "nec" is supplied, only the nec models will be fit. If "ecx" is supplied,
#'  only continuous curve models will be fit. 
#' @param sig_val Probability value to use as the lower quantile to test significance of the predictor posterior values
#' against the control, to estimate nec as an interpolated NOec value from smooth ecx curves.
#' @export
#' @return All successully fitted bayes model fits, mod.stats a data.frame of model fit statistics, nec a model
#' averaged posterior of the estimated nec, and pred.vals a list of model averaged predictions.

fit_bayesmanec <- function(data,
                          x_var,
                          y_var,
                          trials_var = NA,
                          x_type = NA, 
                          y_type = NA,
                          iter = 2e4,
                          over_disp=FALSE,
                          model_set="all",
                          sig_val=0.025,
                          x_range=NA,
                          ...){
  
  if(model_set[1]=="nec"){model_set=c("nec3param", "nec4param", "nechorme", "necsigm")}
  if(model_set[1]=="ecx"){model_set=c("ecx4param", "ecxwb1", "ecxwb2")}#, "ecxlin"
  if(model_set[1]=="all"){model_set=c("nec3param", "nec4param", "nechorme", "necsigm", 
                                   #"ecxlin",
                                   "ecx4param", "ecxwb1", "ecxwb2"
  )}
  
  # Fit each of the models
  mod_fits <- vector(mode = 'list', length = length(model_set))
  names(mod_fits) <- model_set
  
  for(m in seq_along(model_set)){
    model <- model_set[m] 
    fit_m <- try(
      fit_bayesnec(data=data,
                  x_var=x_var,
                  y_var=y_var,
                  trials_var = trials_var,
                  x_type = x_type, 
                  y_type = y_type,
                  iter = iter,
                  over_disp=over_disp,
                  model=model,
                  x_range=x_range), 
      silent = TRUE)
    if (!inherits(fit_m, 'try-error')) {
      mod_fits[[m]] <- fit_m  
    } else {
      mod_fits[[m]] <- NA 
    }
    
  }
  
  # collate all the elements
  export.list <- c(extract_modstats(mod_fits), 
                   list(data=data, x_var=x_var, y_var=y_var, trials_var=trials_var, over_disp=over_disp))
  # assign a class to the output
  class(export.list) <- "bayesmanecfit"
  
  # return the collated output
  return(export.list)
  
}
