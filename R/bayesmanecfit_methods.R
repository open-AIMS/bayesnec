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
#' @param xlim a numeric vector of length two to use for the lower and uper limits of the x-axis range
#' @param xticks a numeric vector indicate where to place the tick marks of the x-axis
#'
#' @export
#' @return a plot of the fitted model

plot.bayesmanecfit <- function(X,  CI=TRUE, 
                               add_nec=TRUE, position_legend="topright",  add_ec10=FALSE,
                               xform=NA, lxform=NA,
                               jitter_x=FALSE, jitter_y=FALSE, 
                               ylab="response", 
                               xlab="concentration", 
                               xlim = NA, 
                               ylim = NA,
                               xticks = NA,  ...){
  
  
  # check if y_type is binomial
  y_type <- X$y_type
  if(y_type=="binomial"){
    y_dat <- X$mod_dat$y/X$mod_dat$trials}else{
      y_dat <- X$mod_dat$y}
  
  ## extract ec10
  ec10 <- c(NA, NA, NA)
  if(add_ec10==TRUE & X$y_type!="gaussian"){
    ec10 <- extract_ecx(X)
  }
  if(add_ec10==TRUE & X$y_type=="gaussian"){
    ec10 <- extract_ecx(X, type="relative")
  }
  
  # check if a transformation is required for x
  if(class(xform)=="function"){
    x_dat <- xform(X$mod_dat$x)
    nec <- xform(X$nec)
    x_vec <- xform(X$pred_vals$x)
    ec10 <- xform(ec10)
  }else{
    x_dat <- X$mod_dat$x
    nec <- X$nec
    x_vec <- X$pred_vals$x
  }
  
  if(jitter_x==TRUE){x_dat <- jitter(x_dat)}
  if(jitter_y==TRUE){y_dat <- jitter(y_dat)}  
  
  # check if a range for the x axis has been specified
  if(max(is.na(xlim))==1){
    x_lim <- range(x_dat)}else{
      x_lim <- xlim
    }
  # Check if x axis tick marks have been specified
  if(max(is.na(xticks))==1){
    xticks <- seq(min(x_dat), max(x_dat), length=7)
  }else{
    xticks <- xticks
  }
  
  if(is.na(xlim)==TRUE){xlim=range(x_dat)}
  if(is.na(ylim)==TRUE){ylim=range(y_dat)}
  
  plot(x_dat, y_dat, 
       ylab=ylab, 
       xlab=xlab,        
       pch=16, xaxt="n", 
       xlim = xlim, ylim=ylim,
       col=adjustcolor(1, alpha=0.25), 
       cex=1.5) 
  
  if(class(lxform)!="function"){
    if(max(is.na(xticks))==1){axis(side=1)}else{axis(side=1, at=xticks)}  
    legend_nec <- paste("nec: ", signif(nec["Estimate"],2), 
                        " (", signif(nec["Q2.5"],2),"-", 
                        signif(nec["Q97.5"],2),")",sep="")
    legend_ec10 <- paste("ec10: ", signif(ec10[1],2), 
                         " (", signif(ec10[2],2),"-", 
                         signif(ec10[3],2),")",sep="")
  }else{
    x_labs <- signif(lxform(xticks),2)
    axis(side=1, at=xticks, labels = x_labs)
    legend_nec <- paste("nec: ", signif(lxform(nec["Estimate"]),2), 
                        " (",    signif(lxform(nec["Q2.5"]),2),"-", 
                        signif(lxform(nec["Q97.5"]),2),")",sep="")    
    legend_ec10 <- paste("ec10: ", signif(lxform(ec10[1]),2), 
                         " (",    signif(lxform(ec10[2]),2),"-", 
                         signif(lxform(ec10[3]),2),")",sep="")  
  }
  
  if(CI==TRUE){
    lines(x_vec, X$pred_vals$up, lty=2) 
    lines(x_vec, X$pred_vals$lw, lty=2)  
  }
  
  lines(x_vec, X$pred_vals$y)
  
  if(add_nec==TRUE & add_ec10==FALSE){
    abline(v=nec, col = "red", lty=c(1,1,3))   
    legend(position_legend, bty="n",
           legend=legend_nec, lty=1, col="red")
  }
  if(add_ec10==TRUE & add_nec==FALSE){
    abline(v=ec10, col = "red", lty=c(1,3,3))  
    legend(position_legend, bty="n",
           legend=legend_ec10, lty=1, col="red")
  }
  if(add_ec10==TRUE & add_nec==TRUE){
    abline(v=nec, col = "red", lty=c(1,3,3))  
    abline(v=ec10, col = "orange", lty=c(1,3,3)) 
    
    legend(position_legend, bty="n",
           legend=c(legend_nec, legend_ec10), 
           lty=1, col=c("red", "orange"))
  }
}

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
