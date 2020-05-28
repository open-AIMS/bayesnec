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

#' plot.bayesnecfit
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
#' @export
#' @return a plot of the fitted model

plot.bayesnecfit <- function(X,  CI=TRUE,
                            add_nec=TRUE, position_legend="topright",  add_ec10=FALSE,
                            xform=NA, lxform=NA,
                            jitter_x=FALSE, jitter_y=FALSE, 
                            ylab="response", 
                            xlab="concentration", 
                            xlim = NA, xticks = NA,  ...){
  
  if(X$model=="ecx4param" & add_nec==TRUE){
    add_nec=FALSE; add_ec10=TRUE
  }
  
  # check if y_type is binomial
  y_type <- X$y_type
  if(y_type=="binomial"){
    y_dat <- X$mod_dat$y/X$mod_dat$trials}else{
      y_dat <- X$mod_dat$y}
  
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
  
  plot(x_dat, y_dat, 
       ylab=ylab, 
       xlab=xlab,        
       pch=16, xaxt="n", xlim = x_lim,
       col=adjustcolor(1, alpha=0.25), 
       cex=1.5) 
  
  if(class(lxform)!="function"){
    if(max(is.na(xticks))==1){axis(side=1)}else{axis(side=1, at=xticks)}  
    legend_nec <- paste("nec: ", signif(nec["Estimate"],2), 
                        " (", signif(nec["Q2.5"],2),"-", signif(nec["Q97.5"],2),")",sep="")
    legend_ec10 <- paste("ec10: ", signif(ec10[1],2), 
                         " (", signif(ec10[2],2),"-", signif(ec10[3],2),")",sep="")
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
    abline(v=nec, col = "red", lty=c(1,3,3))   
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