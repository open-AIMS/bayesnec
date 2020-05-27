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
#' 
#' @param CI a logical value indicating if confidence intervals on the model fit should be plotted, calculated as the upper and lower bounds of the individual predicted values from all posterior samples
#'
#' @param add.nec a logical value indicating if the estimated nec value and 95\% credible intervals should be added to the plot.
#' 
#' @param add.ec10 a logical value indicating if an estimated ec10 value and 95\% credible intervals should be added to the plot.
#' 
#' @param legend.loc a vector indicating the location of the nec or ec10 legend, as per a call to legend.
#'
#' @param xform a function to be applied as a transformation of the x data.
#'
#' @param lxform a function to be applied as a transformation only to axis labels and the annoted nec/ec10 values.
#'
#' @param x.jitter a logical value indicating if the x data points on the plot should be jittered.
#'
#' @param y.jitter a logical value indicating if the y data points on the plot should be jittered.
#' 
#' @param xlab a character vector to use for the x-axis label
#' 
#' @param ylab a character vector to use for the y-axis label
#' 
#' @param xlim a numeric vector of length two to use for the lower and uper limits of the x-axis range
#' 
#' @param xticks a numeric vector indicate where to place the tick marks of the x-axis
#'
#' @export
#' @return a plot of the fitted model

plot.bayesnecfit <- function(X,  CI=TRUE,
                            add.nec=TRUE, legend.loc="topright",  add.ec10=FALSE,
                            xform=NA, lxform=NA,
                            jitter.x=FALSE, jitter.y=FALSE, 
                            ylab="response", 
                            xlab="concentration", 
                            xlim = NA, xticks = NA,  ...){
  
  if(X$model=="ecx4param" & add.nec==TRUE){
    add.nec=FALSE; add.ec10=TRUE
  }
  
  # check if y_type is binomial
  y_type <- X$y_type
  if(y_type=="binomial"){
    y_dat <- X$mod_dat$y/X$mod_dat$trials}else{
      y_dat <- X$mod_dat$y}
  
  ec10 <- c(NA, NA, NA)
  if(add.ec10==TRUE & X$y_type!="gaussian"){
    ec10 <- extract_ecx.bayesnecfit(X)
  }
  if(add.ec10==TRUE & X$y_type=="gaussian"){
    ec10 <- extract_ecx.bayesnecfit(X, type="relative")
  }  
  
  # check if a transformation is required for x
  if(class(xform)=="function"){
    x_dat <- xform(X$mod_dat$x)
    nec <- xform(X$nec)
    x.vec <- xform(X$pred.vals$x)
    ec10 <- xform(ec10)
  }else{
    x_dat <- X$mod_dat$x
    nec <- X$nec
    x.vec <- X$pred.vals$x
  }
  
  if(jitter.x==TRUE){x_dat <- jitter(x_dat)}
  if(jitter.y==TRUE){y_dat <- jitter(y_dat)}  
  
  # check if a range for the x axis has been specified
  if(max(is.na(xlim))==1){
    x.lim <- range(x_dat)}else{
      x.lim <- xlim
    }
  # Check if x axis tick marks have been specified
  if(max(is.na(xticks))==1){
    x.ticks <- seq(min(x_dat), max(x_dat), length=7)
  }else{
    x.ticks <- xticks
  }
  
  plot(x_dat, y_dat, 
       ylab=ylab, 
       xlab=xlab,        
       pch=16, xaxt="n", xlim = x.lim,
       col=adjustcolor(1, alpha=0.25), 
       cex=1.5) 
  
  if(class(lxform)!="function"){
    if(max(is.na(xticks))==1){axis(side=1)}else{axis(side=1, at=x.ticks)}  
    nec.legend <- paste("nec: ", signif(nec["Estimate"],2), 
                        " (", signif(nec["Q2.5"],2),"-", signif(nec["Q97.5"],2),")",sep="")
    ec10.legend <- paste("ec10: ", signif(ec10[1],2), 
                         " (", signif(ec10[2],2),"-", signif(ec10[3],2),")",sep="")
  }else{
    x.labs <- signif(lxform(x.ticks),2)
    axis(side=1, at=x.ticks, labels = x.labs)
    nec.legend <- paste("nec: ", signif(lxform(nec["Estimate"]),2), 
                        " (",    signif(lxform(nec["Q2.5"]),2),"-", 
                        signif(lxform(nec["Q97.5"]),2),")",sep="")    
    ec10.legend <- paste("ec10: ", signif(lxform(ec10[1]),2), 
                         " (",    signif(lxform(ec10[2]),2),"-", 
                         signif(lxform(ec10[3]),2),")",sep="")  
  }
  
  if(CI==TRUE){
    lines(x.vec, X$pred.vals$up, lty=2) 
    lines(x.vec, X$pred.vals$lw, lty=2)  
  }

    lines(x.vec, X$pred.vals$y)
  
  if(add.nec==TRUE & add.ec10==FALSE){
    abline(v=nec, col = "red", lty=c(1,3,3))   
    legend(legend.loc, bty="n",
           legend=nec.legend, lty=1, col="red")
  }
  if(add.ec10==TRUE & add.nec==FALSE){
    abline(v=ec10, col = "red", lty=c(1,3,3))  
    legend(legend.loc, bty="n",
           legend=ec10.legend, lty=1, col="red")
  }
  if(add.ec10==TRUE & add.nec==TRUE){
    abline(v=nec, col = "red", lty=c(1,3,3))  
    abline(v=ec10, col = "orange", lty=c(1,3,3)) 
    
    legend(legend.loc, bty="n",
           legend=c(nec.legend, ec10.legend), 
           lty=1, col=c("red", "orange"))
  }
}

#' plot.bayesmanec
#'
#' Generates a plot of a fitted bayes nec model, as returned by fit.bayesnec.
#' 
#' @param X the bayes nec model fit as returned by fit.bayesnec.
#' 
#' @param CI a logical value indicating if confidence intervals on the model fit should be plotted, calculated as the upper and lower bounds of the individual predicted values from all posterior samples
#'
#' @param add.nec a logical value indicating if the estimated nec value and 95\% credible intervals should be added to the plot.
#' 
#' @param add.ec10 a logical value indicating if an estimated ec10 value and 95\% credible intervals should be added to the plot.
#' 
#' @param legend.loc a vector indicating the location of the nec or ec10 legend, as per a call to legend.
#'
#' @param xform a function to be applied as a transformation of the x data.
#'
#' @param lxform a function to be applied as a transformation only to axis labels and the annoted nec/ec10 values.
#'
#' @param x.jitter a logical value indicating if the x data points on the plot should be jittered.
#'
#' @param y.jitter a logical value indicating if the y data points on the plot should be jittered.
#' 
#' @param xlab a character vector to use for the x-axis label
#' 
#' @param ylab a character vector to use for the y-axis label
#' 
#' @param xlim a numeric vector of length two to use for the lower and uper limits of the x-axis range
#' 
#' @param xticks a numeric vector indicate where to place the tick marks of the x-axis
#'
#' @export
#' @return a plot of the fitted model

plot.bayesmanecfit <- function(X,  CI=TRUE, 
                              add.nec=TRUE, legend.loc="topright",  add.ec10=FALSE,
                              xform=NA, lxform=NA,
                              jitter.x=FALSE, jitter.y=FALSE, 
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
  if(add.ec10==TRUE & X$y_type!="gaussian"){
    ec10 <- extract_ecx.bayesmanecfit(X)
  }
  if(add.ec10==TRUE & X$y_type=="gaussian"){
    ec10 <- extract_ecx.bayesmanecfit(X, type="relative")
  }
  
  # check if a transformation is required for x
  if(class(xform)=="function"){
    x_dat <- xform(X$mod_dat$x)
    nec <- xform(X$nec)
    x.vec <- xform(X$pred.vals$x)
    ec10 <- xform(ec10)
  }else{
    x_dat <- X$mod_dat$x
    nec <- X$nec
    x.vec <- X$pred.vals$x
  }
  
  if(jitter.x==TRUE){x_dat <- jitter(x_dat)}
  if(jitter.y==TRUE){y_dat <- jitter(y_dat)}  
  
  # check if a range for the x axis has been specified
  if(max(is.na(xlim))==1){
    x.lim <- range(x_dat)}else{
      x.lim <- xlim
    }
  # Check if x axis tick marks have been specified
  if(max(is.na(xticks))==1){
    x.ticks <- seq(min(x_dat), max(x_dat), length=7)
  }else{
    x.ticks <- xticks
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
    if(max(is.na(xticks))==1){axis(side=1)}else{axis(side=1, at=x.ticks)}  
    nec.legend <- paste("nec: ", signif(nec["Estimate"],2), 
                        " (", signif(nec["Q2.5"],2),"-", 
                        signif(nec["Q97.5"],2),")",sep="")
    ec10.legend <- paste("ec10: ", signif(ec10[1],2), 
                         " (", signif(ec10[2],2),"-", 
                         signif(ec10[3],2),")",sep="")
  }else{
    x.labs <- signif(lxform(x.ticks),2)
    axis(side=1, at=x.ticks, labels = x.labs)
    nec.legend <- paste("nec: ", signif(lxform(nec["Estimate"]),2), 
                        " (",    signif(lxform(nec["Q2.5"]),2),"-", 
                        signif(lxform(nec["Q97.5"]),2),")",sep="")    
    ec10.legend <- paste("ec10: ", signif(lxform(ec10[1]),2), 
                         " (",    signif(lxform(ec10[2]),2),"-", 
                         signif(lxform(ec10[3]),2),")",sep="")  
  }
  
  if(CI==TRUE){
    lines(x.vec, X$pred.vals$up, lty=2) 
    lines(x.vec, X$pred.vals$lw, lty=2)  
  }

    lines(x.vec, X$pred.vals$y)

  if(add.nec==TRUE & add.ec10==FALSE){
    abline(v=nec, col = "red", lty=c(1,1,3))   
    legend(legend.loc, bty="n",
           legend=nec.legend, lty=1, col="red")
  }
  if(add.ec10==TRUE & add.nec==FALSE){
    abline(v=ec10, col = "red", lty=c(1,3,3))  
    legend(legend.loc, bty="n",
           legend=ec10.legend, lty=1, col="red")
  }
  if(add.ec10==TRUE & add.nec==TRUE){
    abline(v=nec, col = "red", lty=c(1,3,3))  
    abline(v=ec10, col = "orange", lty=c(1,3,3)) 
    
    legend(legend.loc, bty="n",
           legend=c(nec.legend, ec10.legend), 
           lty=1, col=c("red", "orange"))
  }
}