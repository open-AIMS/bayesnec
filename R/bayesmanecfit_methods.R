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

#' plot.bayesmanec
#'
#' Generates a plot of a fitted bayes nec model, as returned by fit.bayesnec.
#' 
#' @param X the bayes nec model fit as returned by fit.bayesnec.
#' @param CI a logical value indicating if confidence intervals on the model fit should be plotted, calculated as the upper and lower bounds of the individual predicted values from all posterior samples
#' @param add_nec a logical value indicating if the estimated nec value and 95\% credible intervals should be added to the plot.
#' @param add_ec10 a logical value indicating if an estimated ec10 value and 95\% credible intervals should be added to the plot.
#' @param position_legend a vector indicating the location of the nec or ec10 legend, as per a call to legend.
#' @param xform a function to be applied as a transformation of the x data.
#' @param lxform a function to be applied as a transformation only to axis labels and the annoted nec/ec10 values.
#' @param jitter_x a logical value indicating if the x data points on the plot should be jittered.
#' @param jitter_y a logical value indicating if the y data points on the plot should be jittered.
#' @param xlab a character vector to use for the x-axis label
#' @param ylab a character vector to use for the y-axis label
#' @param x_lim a numeric vector of length two to use for the lower and uper limits of the x-axis range
#' @param y_lim a numeric vector of length two to use for the lower and uper limits of the y-axis range
#' @param xticks a numeric vector indicate where to place the tick marks of the x-axis
#'
#' @export
#' @return a plot of the fitted model

plot.bayesmanecfit <- function(X,  CI=TRUE, 
                               add_nec=TRUE, 
                               position_legend="topright",  
                               add_ec10=FALSE,
                               xform=NA, 
                               lxform=NA,
                               jitter_x=FALSE, 
                               jitter_y=FALSE, 
                               ylab="response", 
                               xlab="concentration", 
                               x_lim = NA, 
                               y_lim = NA,
                               xticks = NA, 
                               all_models = FALSE, ...){
  
   if(all_models){
    mod_fits <- X$mod_fits 
    par(mfrow=c(ceiling(length(mod_fits)/2), 2), mar=c(1.5, 1.5, 1.5, 1.5), oma=c(3, 3, 0, 0)) 
    for(m in 1:length(mod_fits)){
      plot(X = mod_fits[[m]],
           CI = CI, add_nec = add_nec, 
           position_legend = position_legend,  
           add_ec10 = add_ec10,
           xform = xform, lxform = lxform,
           jitter_x = jitter_x, jitter_y = jitter_y, 
           ylab = "", xlab = "", 
           x_lim = x_lim, 
           xticks = xticks,  ...)   
      mtext(xlab, side=1, outer=T, line=2)
      mtext(ylab, side=2, outer=T, line=2)      
    }
    
     
   }else{
    plot.bayesnecfit(X = X, CI = CI, add_nec = add_nec, 
                    position_legend = position_legend,  
                    add_ec10 = add_ec10,
                    xform = xform, lxform = lxform,
                    jitter_x = jitter_x, jitter_y = jitter_y, 
                    ylab = ylab, xlab = xlab, 
                    x_lim = x_lim, 
                    xticks = xticks,  ...)     
   }

  

  
  
}

#'modify.bayesmanec
#'
#' Modifys an existing bayesmanecfit, for example, but adding or removing fitted models.
#'
#' @param  bayesmanecfit a bayesmanecfit output list, as returned by fit.bayesmanec
#' @param model_set A character vector containing the of names of model types to be included in the modified fit.
#' @param drop_models A character vector containing the names of model types you which to drop for the modified fit.
#' @param add_models A character vector containing the names of model types to add to the modified fit.
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
                                      "ecx4param", "ecxwb1", "ecxwb2")}
  
  if(model_set[1]=="bot_free"){model_set=c("nec3param", "nechorme", "necsigm", 
                                           "ecxlin", "ecxexp", "ecxsigm")}
  
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
