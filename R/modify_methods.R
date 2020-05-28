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

#'modify.bayesmanec
#'
#' Modifys an existing bayesmanecfit, for example, but adding or removing fitted models.
#'
#' @param  bayesmanecfit a bayesmanecfit output list, as returned by fit.bayesmanec
#'
#' @param model_set A character vector containing the of names of model types to be included in the modified fit.
#' 
#' @param drop_models A character vector containing the names of model types you which to drop for the modified fit.
#' 
#' @param add_models A character vector containing the names of model types to add to the modified fit.
#'
#' @export
#' @return All successully fitted bayesmanecfit model fit.

modify.bayesmanecfit <- function(bayesmanecfit, model_set=NA, drop_models=NA, add_models=NA, ...) {
  
  # if the model set is NA 
  if(is.na(model_set[1])){
    model_set <- names(bayesmanecfit$mod_fits)
  }
  
  if(model_set[1]=="nec"){model_set=c("nec3param", "nec4param", "nechorme", "necsigm")}
  if(model_set[1]=="ecx"){model_set=c("ecx4param", "ecxwb1", "ecxwb2")}
  if(model_set[1]=="all"){model_set=c("nec3param", "nec4param", "nechorme", "necsigm", 
                                      "ecxlin", "ecxexp", "ecxsigm",
                                      "ecx4param", "ecxwb1", "ecxwb2"
  )}
  
  # if drop_models is not NA 
  if(is.na(drop_models[1])==F){
    model_set <- model_set[is.na(match(model_set, drop_models))]
  }
  
  # if add_models is not NA 
  if(is.na(add_models[1])==F){
    model_set <- unique(c(model_set,add_models))
  }
  
  # Get the original model simulation attributes
  simdat <- extract_simdat(bayesmanecfit$mod_fits[[1]])
  
  # Fit each of the models
  mod_fits <- vector(mode = 'list', length = length(model_set))
  names(mod_fits) <- model_set
  
  for(m in 1:length(model_set)){
    model <- model_set[m] 
    mod_m <- NULL
    mod_m <- try(bayesmanecfit$mod_fits[[model]], silent=T)
    if(class(mod_m)!="bayesnecfit"){
      fit_m <- try(
        fit_bayesnec(data = bayesmanecfit,
                     x_var = bayesmanecfit$x_var,
                     y_var = bayesmanecfit$y_var,
                     trials_var = bayesmanecfit$trials_var,
                     x_type = bayesmanecfit$x_type, 
                     y_type = bayesmanecfit$y_type,
                     over_disp = bayesmanecfit$over_disp,
                     model = model,
                     iter = simdat$iter,
                     thin = simdat$thin,
                     warmup = simdat$warmup,
                     chains = simdat$chains),
        silent = TRUE)
      if (!inherits(fit_m, 'try-error')) {
        mod_fits[[model]] <- fit_m  
      } else {
        mod_fits[[model]] <- NA 
      }   
 
    
    }else{
      mod_fits[[m]] <- mod_m
    }
  
  }

  # collate all the elements
  export_list <- c(extract_modstats(mod_fits), 
                   list(data=bayesmanecfit$data, 
                        x_var=bayesmanecfit$x_var, 
                        y_var=bayesmanecfit$y_var, 
                        trials_var=bayesmanecfit$trials_var, 
                        over_disp=bayesmanecfit$over_disp))
  # assign a class to the output
  class(export_list) <- "bayesmanecfit"
  
  # return the collated output
  return(export_list) 
  
  
  
}