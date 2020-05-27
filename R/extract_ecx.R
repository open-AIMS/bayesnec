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

#' extract_ecx
#'
#' Extracts the predicted ecx value as desired from a bayesnec or a bayesmanec model fit.
#'
#' @param  X a jag model fit as returned by a call to bayes from fit.bayesnec
#' 
#' @param ecx.val the desired percentage effect value. This must be a value between 1 and 99 (for type = "relative" 
#' and "absolute"), defaults to 10.
#' 
#' @param type a character vector, taking values of "relative",  "absolute" (the default) or "direct". 
#' Type "relative" is calculated as the percentage decrease from the maximum predicted value of the response (top) to the minimum predicted value 
#' of the response. Type "absolute" (the default) is calculated as the percentage decrease from the maximum value of the response (top) 
#' to 0 (or bot for a 4 parameter model fit). Type "direct" provides a direct estimate of the x value for a given y.
#' Note that for the current version, ecx for an necHormesis model is estimated at a percent decline from the control
#' 
#' @param precision The number of unique x values over which to find ecx - large values will make the ecx estimate more 
#' precise.
#' 
#' @param posterior A logical value indicating if the full posterior sample of calculated ecx values should be returned 
#' instead of just the median and 95 credible intervals.
#' 
#' @param xform A function to apply to the returned estimated concentration values
#' 
#' @param x_range A range of x values over which to consider extracting ecx
#' 
#' @param prob.vals A vector indicating the probability values over which to return the estimated ecx value. Defaults to 0.5 (median) and 0.025 and 0.975 (95 percent credible intervals). 
#' 
#' @export
#' @return A vector containing the estimated ecx value, including upper and lower 95 percent Credible Interval bounds
#' 
extract_ecx <- function(X, ecx.val=10, precision=1000, posterior = FALSE, type="absolute", 
                        hormesis.def = "control", xform=NA, x_range=NA,
                        prob.vals=c(0.5, 0.025, 0.975), link="identity"){
  
  if(class(X)=="bayesnecfit"){
    ecx <- extract_ecx.bayesnecfit(X, ecx.val=ecx.val, precision=precision, 
                                  posterior = posterior, type=type, hormesis.def=hormesis.def, 
                                  xform=xform, 
                                  prob.vals=prob.vals)
  }
  if(class(X)== "bayesmanecfit"){
    ecx <- extract_ecx.bayesmanecfit(X, ecx.val=ecx.val, precision=precision, 
                                    posterior = posterior, type=type, xform=xform, x_range=x_range,
                                    prob.vals=prob.vals) 
  }
  
  if(exists("ecx")==FALSE){
    stop("Failed to estimate ecx value for the supplied object class. Only bayesnecfit and bayesmanecfit classes are suported.")
  }
  
  return(ecx) 
  
}

#' extract_ecx.bayesnec
#'
#' Extracts the predicted ecx value as desired from a bayesnec model fit obeject
#'
#' @param  X a jag model fit as returned by a call to bayes from fit.bayesnec
#' 
#' @param ecx.val the desired percentage effect value.
#' 
#' @param type a character vector indicating if relative or absolute values for the ecx should be calculated.
#' 
#' @param precision The number of unique x values over which to find ecx.
#' 
#' @param posterior A logical value indicating if the full posterior sample of calculated ecx values should be returned 
#' instead of just the median and 95 credible intervals.
#' 
#' @param xform A function to apply to the returned estimated concentration values
#' 
#' @param prob.vals A vector indicating the probability values over which to return the estimated ecx value. 
#' 
#' @export
#' @return A vector containing the estimated ecx value, including upper and lower 95 percent Credible Interval bounds

extract_ecx.bayesnecfit <- function(X, ecx.val=10, precision=1000, posterior = FALSE, type="absolute", 
                                   hormesis.def = "control", x_range=NA,
                                   xform=NA, prob.vals=c(0.5, 0.025, 0.975)){

  if(type!="direct"){
    if(ecx.val<1 | ecx.val>99){
      stop("Supplied ecx.val is not in the required range. Please supply a percentage value between 1 and 99.")
    }   
  }
  
  if(length(grep("ecx", X$model))>0){mod.class <- "ecx"}else{mod.class <- "nec"}
  if(is.null(X$bot)==FALSE){m4param <- 1}else{m4param <- 0}
  
  if(X$y_type=="gaussian"  &  type=="absolute"){
    stop("Absolute ecx values are not valid for a gaussian response variable unless a 4 parameter model is fit") 
  }
  if(X$x_type=="gaussian" &  X$model == "ecxLinear"  & type=="absolute"){
    stop("Absolute ecx values are not valid for a linear model when
         x-values are gaussian, because 'top' merely indicates the y-intercept. Use type 'relative'.") 
  }  
  
  label <- paste("ec", ecx.val, sep="_")
  
  pred.vals <- predict.bayesnec(X, precision=precision, x_range=x_range)
  posterior.sample <- pred.vals$posterior
  x.vec <- pred.vals$'x' 
  
  
  if(X$model=="necHormesis" & hormesis.def=="max"){ # remove values prior to the nec for the hormesis model
    posterior.sample <- do.call("rbind",
                                lapply(1:nrow(posterior.sample), FUN=function(x){
                                  nec.x <- X$nec.posterior[x]
                                  posterior.x <- posterior.sample[x,]
                                  posterior.x[which(x.vec<nec.x)] <- NA
                                  return(posterior.x)
                                }))
  }
  
  if(X$model=="necHormesis"& hormesis.def=="control"){ # remove values greater than the control for the hormesis model
    posterior.sample <- do.call("rbind", 
                                lapply(1:nrow(posterior.sample), FUN=function(x){
                                  control.x <- posterior.sample[x, 1]                                    
                                  posterior.x <- posterior.sample[x, ] 
                                  posterior.x[which(posterior.x>=control.x)] <- NA
                                  return(posterior.x)
                                }))
  }   
  
  if(type=="relative"){
    ecx.out <- apply(posterior.sample, MARGIN=1, FUN=function(y){
      range.y <- range(y, na.rm=T)
      ecx.y <- max(range.y)-diff(range.y)*(ecx.val/100)
      ecx.x <- x.vec[which.min(abs(y-ecx.y))]
      return(ecx.x)
    })    
  }
  
  
  if(type=="absolute"){
    ecx.out <- apply(posterior.sample, MARGIN=1, FUN=function(y){
      range.y <- c(0, max(y, na.rm=T))
      ecx.y <- max(range.y)-diff(range.y)*(ecx.val/100)
      ecx.x <- x.vec[which.min(abs(y-ecx.y))]
      return(ecx.x)  
    })     
  }
  
  if(type=="direct"){
    ecx.out <- apply(posterior.sample, MARGIN=1, FUN=function(y){
      ecx.y <- ecx.val
      ecx.x <- x.vec[which.min(abs(y-ecx.y))]
      return(ecx.x)
      
    }) 
  }
  
  # calculate the quantile values from the posterior?
  ecx.estimate <- quantile(unlist(ecx.out), probs=prob.vals)
  names(ecx.estimate) <- c(label, paste(label, "lw", sep="_"), paste(label, "up", sep="_"))
  
  # if a transformation is required
  if(class(xform)=="function"){
    ecx.estimate <- xform(ecx.estimate)
    ecx.out <- xform(ecx.out)
  }   
  
  if(posterior==FALSE){
    return(ecx.estimate)
  }else{
    return(ecx.out)}
  
  
}

#' extract_ecx.bayesmanec
#'
#' Extracts the predicted ecx value as desired from a bayesnec model fit obeject
#'
#' @param  X a fitted bayesmanec model object, containing a list of jag model fit as returned by a call to bayes from 
#' fit.bayesnec
#' 
#' @param ecx.val the desired percentage effect value.
#' 
#' @param type a character vector indicating if relative or absolute values for the ecx should be calculated. 
#' 
#' @param precision The number of unique x values over which to find ecx.
#' 
#' @param posterior A logical value indicating if the full posterior sample of calculated ecx values 
#' should be returned instead of just the median and 95 credible intervals.
#' 
#' @param xform A function to apply to the returned estimated concentration values
#' 
#' @param prob.vals A vector indicating the probability values over which to return the estimated ecx value. 
#' 
#' @export
#' @return A vector containing the estimated ecx value, including upper and lower 95 percent Credible Interval bounds

extract_ecx.bayesmanecfit <- function(X, ecx.val=10, precision=1000, posterior = FALSE, type="absolute", 
                                     hormesis.def="control", xform=NA, x_range=NA,
                                     prob.vals=c(0.5, 0.025, 0.975)){
  sample.size <- X$sample.size
  ecx.out <- unlist(sapply(1:length(X$success_models), FUN=function(x){
    base::sample(extract_ecx.bayesnecfit(X$mod_fits[[x]], 
                                        ecx.val=ecx.val, 
                                        precision=precision, 
                                        posterior = TRUE, 
                                        x_range = x_range,
                                        type=type), 
                 as.integer(round(sample.size*X$mod_stats[x, "wi"])))
  }))
  
  label <- paste("ec", ecx.val, sep="_")
  # calculate the quantile values from the posterior
  ecx.estimate <- quantile(ecx.out, probs=prob.vals)
  names(ecx.estimate) <- c(label, paste(label, "lw", sep="_"), paste(label, "up", sep="_"))
  
  # if a transformation is required
  if(class(xform)=="function"){
    ecx.estimate <- xform(ecx.estimate)
    ecx.out <- xform(ecx.out)
  }   
  
  
  if(posterior==FALSE){
    return(ecx.estimate)
  }else{
    return(ecx.out)}
  
  
}
