#' plot.bayesmanecfit
#'
#' Generates a plot of a fitted bayes nec model, as returned by fit.bayesnec.
#' 
#' @param x the bayes nec model fit as returned by fit.bayesnec.
#' @param ... Additional arguments to \code{\link[graphics]{plot}}.
#' @param CI a logical value indicating if confidence intervals on the model fit should be plotted, calculated as the upper and lower bounds of the individual predicted values from all posterior samples
#' @param add_nec a logical value indicating if the estimated nec value and 95% credible intervals should be added to the plot.
#' @param add_ec10 a logical value indicating if an estimated ec10 value and 95% credible intervals should be added to the plot.
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
#' @param all_models Logical indicating if all models in the model set should be plotted simultaneousl, or if a model average plot should be returned.
#'
#' @export
#' @importFrom graphics par plot mtext legend
#' @return a plot of the fitted model

plot.bayesmanecfit <- function(x, ..., CI=TRUE, 
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
                               all_models = FALSE){
  
   if(all_models){
    mod_fits <- x$mod_fits 
    par(mfrow=c(ceiling(length(mod_fits)/2), 2), mar=c(1.5, 1.5, 1.5, 1.5), oma=c(3, 3, 0, 0)) 
    for(m in 1:length(mod_fits)){
      plot(x = mod_fits[[m]],
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
      legend("top", legend=names(mod_fits[m]), bty="n")
    }
    
     
   }else{
    plot.bayesnecfit(x = x, CI = CI, add_nec = add_nec, 
         position_legend = position_legend,  
         add_ec10 = add_ec10,
         xform = xform, lxform = lxform,
         jitter_x = jitter_x, jitter_y = jitter_y, 
         ylab = ylab, xlab = xlab, 
         x_lim = x_lim, 
         xticks = xticks, ...)
   }

  

  
  
}
