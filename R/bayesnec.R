#' bayesnec
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
#' @param model A \code{\link[base]{character}} vector indicating the model(s) to fit. See details for more information.
#' @param sig_val Probability value to use as the lower quantile to test significance of the predictor posterior values against the lowest observed concentration.
#' against the control, to estimate nec as an interpolated NOec value from smooth ecx curves.
#' @param x_range A range of x values over which to consider extracting ecx.
#' @param ... Further arguments to \code{\link{fit_bayesnec}}
#' @details 
#' 
#' As some concentration-response data will use zero concentration which can cause numerical estimation issues, a small offset 
#' is added (1/10th of the next lowest value) to zero values of concentration where x_var are distributed on a continuous scale from 0 to Inf, or are bounded to 0, or 1.
#' 
#' The argument \code{model} may be one of "nec3param", "nec4param", "necsigm", "nechorme", "ecx4param", "ecxwb1", "ecxwb2", "ecxexp", "ecxlin", or 'excsigm', 
#' in which case a single model of the specified type it fit, and bayenec returns a model object of class 'bayesnecfit'. 
#' 
#' If a vector of two or more of the available models is supplied, bayesnec returns a model object of class 'bayesmanecfit' containing 
#' model averaged predictions for the supplied models, providing they were successfully fitted.
#' 
#' Model averaging is acheived through a weighted sample of each fitted models posterior predictions, with weights derived using the \code{\link{loo_model_weights}} from \code{\link{brms}}
#' Individual model fits can be extracted from the $mod_fits element and can be examined individually.
#' 
#' \code{model} may also be one of "all", meaning all of the available models will be fit; 
#' "ecx" meaning only models excluding a specific nec step parameter fill be fit; "nec" meaning only models with a specific nec step parameter will be fit; 
#' or "bot_free" meaning only models without a 'bot' parameter (without a bottom plataeu) will be fit.
#' 
#' Models are fit using model formula passed to brms. You can view the model formula for each type X_HOW_X. 
#' Other types can be added as required, please open an issue at the packge site on github to request an additional model.
#' 
#' All models provide an estimate for nec. For model types with "nec" as a prefix, nec is directly estimated as a paremeter 
#' in the model. Models with "ecx" as a prefix are continuous curve models, tyipically used for extracting ecx values 
#' from concentration response data. In this instance the nec value is defined as the concentration at which there is 
#' a user supplied (see \code{sig_val}) percentage certainty (based on the Bayesian posterior estimate) that the response falls below the estimated value of
#' the upper assymptote (top) of the response (i.e the response value is significantly lower than that expected in the case of
#' no exposure).
#' 
#' @export
#' @return All successully fitted bayes model fits, mod.stats a data.frame of model fit statistics, nec a model
#' averaged posterior of the estimated nec, and pred.vals a list of model averaged predictions.

bayesnec <- function(data,
                          x_var,
                          y_var,
                          model=NA,                     
                          trials_var = NA,
                          x_type = NA, 
                          y_type = NA,
                          x_range=NA, 
                          precision = 1000,
                          over_disp=FALSE,
                          sig_val=0.01,
                          iter = 2e3,                          
                          warmup = floor(iter/5)*4, 
                          ...){
  
  if(length(model)==1){
      if(model=="nec"){model=c("nec3param", "nec4param", "nechorme", "necsigm")}
      if(model=="ecx"){model=c("ecx4param", "ecxlin", "ecxexp", "ecxsimg","ecxwb1", "ecxwb2")}
      if(model=="all"){model=c("nec3param", "nec4param", "nechorme", "necsigm", 
                                      "ecxlin", "ecxexp", "ecxsigm",
                                      "ecx4param", "ecxwb1", "ecxwb2")}
      if(model=="bot_free"){model=c("nec3param", "nechorme", "necsigm", 
                                      "ecxlin", "ecxexp", "ecxsigm")}  

  }
  
  
  if(length(model)>1){
 
    # Fit each of the models
    mod_fits <- vector(mode = 'list', length = length(model))
    names(mod_fits) <- model
    
    for(m in seq_along(model)){
      model_m <- model[m] 
      fit_m <- try(
        fit_bayesnec(data=data,
                     x_var=x_var,
                     y_var=y_var,
                     trials_var = trials_var,
                     x_type = x_type, 
                     y_type = y_type,
                     iter = iter,
                     over_disp=over_disp,
                     model=model_m,
                     x_range=x_range,
                     precision=precision,
                     open_progress = FALSE), 
        silent = TRUE)
      if (!inherits(fit_m, 'try-error')) {
        mod_fits[[m]] <- fit_m  
      } else {
        mod_fits[[m]] <- NA 
      }
      
    }
    
    # collate all the elements
    export_list <- c(extract_modstats(mod_fits), 
                     list(data=data, x_var=x_var, y_var=y_var, trials_var=trials_var, over_disp=over_disp))
    # assign a class to the output
    class(export_list) <- "bayesmanecfit"
    
  }else{
    export_list <-  fit_bayesnec(data=data,
                                 x_var=x_var,
                                 y_var=y_var,
                                 trials_var = trials_var,
                                 x_type = x_type, 
                                 y_type = y_type,
                                 iter = iter,
                                 over_disp=over_disp,
                                 model=model,
                                 x_range=x_range,
                                 precision=precision,
                                 open_progress = FALSE)
    
    # assign a class to the output
    class(export_list) <- "bayesnecfit"
  }
  
  # return the collated output
  return(export_list)
  
}
