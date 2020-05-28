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

#'extract_modstats
#'
#' Extracts a range of statistics from a list of bayesnecfit model fits.
#'
#' @param  mod_fits a bayesMAnecfit mod_fits output list, as returned by fit.bayesMAnec
#'
#' @export
#' @return A list of model statistical output derived from the input model list 

extract_modstats <- function(mod_fits){
  model_set <- names(mod_fits)
  # extract model parameters that do not vary across models
  y_type <- mod_fits[[1]]$y_type
  x_type <- mod_fits[[1]]$x_type
  mod_dat <- mod_fits[[1]]$mod_dat
  
  success_models <- model_set[sapply(mod_fits, FUN=class)=="bayesnecfit"]
  if(length(success_models)==0){
    stop("None of the models fit successfully, 
     try using fit.bayesnec instead using the default settings as a starting point for trouble shooting.")}else{
       warning(paste("successfully fitted the models: ", paste(success_models, collapse=" ")))
     }
  
  mod_fits <- mod_fits[success_models] 
  # extract the model statistics for each fit
  mod_stats <- data.frame(model = success_models)
  mod_stats$waic <- sapply(mod_fits, FUN=function(x){brms::waic(x$fit)$estimates["waic","Estimate"]})
  mod_stats$wi <- brms::loo_model_weights(lapply(mod_fits, FUN=function(x){x$fit$loo}))
  mod_stats$over_disp <- unlist(lapply(mod_fits, FUN=function(x){x$over_disp}))
  
  sample_size <- nrow(predict(mod_fits[[1]]$fit, summary=FALSE)) 
  
  # model averaged nec posterior
  nec_posterior <- unlist(lapply(1:length(success_models), FUN=function(x){
      base::sample(mod_fits[[x]]$nec_posterior, size=as.integer(round(sample_size*mod_stats[x, "wi"])))})
  )

  # model averaged predicted y
  predicted_y <- rowSums(do.call("cbind", lapply(1:length(success_models), FUN=function(x){
    mod_fits[[x]]$predicted_y*mod_stats[x, "wi"]
  })))
  
  # model averaged pred_vals
  x <- mod_fits[[success_models[1]]]$pred_vals$x
  
  y_m <- rowSums(do.call("cbind", lapply(success_models, FUN=function(x){
    mod_fits[[x]]$pred_vals$y_m*mod_stats[x, "wi"]
  })))
  
  # model weighted posterior
  posterior_predicted <- do.call("cbind", lapply(1:length(success_models), FUN=function(x){
    mod_fits[[x]]$pred_vals$posterior[, base::sample(1:sample_size, round(sample_size*mod_stats[x, "wi"]))] 
  }))
  
  y <- apply(posterior_predicted, MARGIN=1, FUN=median)
  up <- apply(posterior_predicted, MARGIN=1, FUN=quantile, probs=0.975)
  lw <- apply(posterior_predicted, MARGIN=1, FUN=quantile, probs=0.025)
  
  nec <- quantile(nec_posterior, c(0.5, 0.025,  0.975))  
  names(nec) <- c("Estimate", "Q2.5", "Q97.5")
  
  # collate all the elements
  export.list <- 
      list(mod_fits=mod_fits,
           success_models=success_models,
           mod_dat=mod_dat,
           y_type=y_type,
           x_type=x_type,
           mod_stats=mod_stats,
           sample_size=sample_size,
           nec_posterior=nec_posterior,
           predicted_y=predicted_y,
           residuals=mod_dat$y-predicted_y,
           pred_vals=list(x=x, y=y, up=up, lw=lw, posterior=posterior_predicted, y_m=y_m),
           nec=nec)
  return(export.list)
  
  
}
