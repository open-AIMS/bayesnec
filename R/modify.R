#' modify
#'
#' Modifies an existing bayesmanecfit object, for example, but adding or removing fitted models.
#'
#' @aliases modify
#' 
#' @param object a bayesmanecfit output list, as returned by fit.bayesmanec
#' @param model_set A character vector containing the of names of model types to be included in the modified fit.
#' @param drop_models A character vector containing the names of model types you which to drop for the modified fit.
#' @param add_models A character vector containing the names of model types to add to the modified fit.
#' 
#' @return All successully fitted bayesmanecfit model fit.
#' @export
modify <- function(object, model_set=NA, drop_models=NA, add_models=NA) {
  
  # if the model set is NA 
  if(is.na(model_set[1])){
    model_set <- names(object$mod_fits)
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
  simdat <- extract_simdat(object$mod_fits[[1]])
  
  # Fit each of the models
  mod_fits <- vector(mode = 'list', length = length(model_set))
  names(mod_fits) <- model_set
  
  for(m in 1:length(model_set)){
    model <- model_set[m] 
    mod_m <- NULL
    mod_m <- try(object$mod_fits[[model]], silent=T)
    if(class(mod_m)!="bayesnecfit"){
      fit_m <- try(
        fit_bayesnec(data = object,
                     x_var = object$x_var,
                     y_var = object$y_var,
                     trials_var = object$trials_var,
                     x_type = object$x_type, 
                     y_type = object$y_type,
                     over_disp = object$over_disp,
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
                   list(data=object$data, 
                        x_var=object$x_var, 
                        y_var=object$y_var, 
                        trials_var=object$trials_var, 
                        over_disp=object$over_disp))
  # assign a class to the output
  class(export_list) <- "bayesmanecfit"
  
  # return the collated output
  return(export_list) 
  
  
  
}
