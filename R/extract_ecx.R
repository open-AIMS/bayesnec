#' extract_ecx
#'
#' Extracts the predicted ecx value as desired from a bayesnec or a bayesmanec model fit.
#'
#' @param  X a jag model fit as returned by a call to bayes from fit_bayesnec
#' @param ecx_val the desired percentage effect value. This must be a value between 1 and 99 (for type = "relative" 
#' and "absolute"), defaults to 10.
#' @param type a character vector, taking values of "relative",  "absolute" (the default) or "direct". 
#' Type "relative" is calculated as the percentage decrease from the maximum predicted value of the response (top) to the minimum predicted value 
#' of the response. Type "absolute" (the default) is calculated as the percentage decrease from the maximum value of the response (top) 
#' to 0 (or bot for a 4 parameter model fit). Type "direct" provides a direct estimate of the x value for a given y.
#' Note that for the current version, ecx for an necHormesis model is estimated at a percent decline from the control
#' @param precision The number of unique x values over which to find ecx - large values will make the ecx estimate more 
#' precise.
#' @param posterior A logical value indicating if the full posterior sample of calculated ecx values should be returned 
#' instead of just the median and 95 credible intervals
#' @param hormesis_def to complete
#' @param link to complete
#' @param xform A function to apply to the returned estimated concentration values
#' @param x_range A range of x values over which to consider extracting ecx
#' @param prob_vals A vector indicating the probability values over which to return the estimated ecx value. Defaults to 0.5 (median) and 0.025 and 0.975 (95 percent credible intervals). 
#' 
#' @export
#' @return A vector containing the estimated ecx value, including upper and lower 95 percent Credible Interval bounds
#' 
extract_ecx <- function(X, ecx_val=10, precision=1000, posterior = FALSE, type="absolute", 
                        hormesis_def = "control", xform=NA, x_range=NA,
                        prob_vals=c(0.5, 0.025, 0.975), link="identity"){
  
  if(class(X)=="bayesnecfit"){
    ecx <- extract_ecx_nec(X, ecx_val=ecx_val, precision=precision, 
                                  posterior = posterior, type=type, hormesis_def=hormesis_def, 
                                  xform=xform, 
                                  prob_vals=prob_vals)
  }
  if(class(X)== "bayesmanecfit"){
    ecx <- extract_ecx_manec(X, ecx_val=ecx_val, precision=precision, 
                                    posterior = posterior, type=type, xform=xform, x_range=x_range,
                                    prob_vals=prob_vals) 
  }
  
  if(exists("ecx")==FALSE){
    stop("Failed to estimate ecx value for the supplied object class. Only bayesnecfit and bayesmanecfit classes are suported.")
  }
  
  return(ecx) 
  
}

#' extract_ecx_nec
#'
#' Extracts the predicted ecx value as desired from a bayesnec model fit obeject
#'
#' @param  X a jag model fit as returned by a call to bayes from fit_bayesnec
#' @param ecx_val the desired percentage effect value.
#' @param type a character vector indicating if relative or absolute values for the ecx should be calculated.
#' @param precision The number of unique x values over which to find ecx.
#' @param posterior A logical value indicating if the full posterior sample of calculated ecx values should be returned 
#' instead of just the median and 95 credible intervals.
#' @param xform A function to apply to the returned estimated concentration values
#' @param prob_vals A vector indicating the probability values over which to return the estimated ecx value. 
#' @param hormesis_def to complete
#' @param x_range A range of x values over which to consider extracting ecx
#' 
#' @export
#' @importFrom brms posterior_predict
#' @importFrom stats quantile
#' @return A vector containing the estimated ecx value, including upper and lower 95 percent Credible Interval bounds

extract_ecx_nec <- function(X, ecx_val=10, precision=1000, posterior = FALSE, type="absolute", 
                                   hormesis_def = "control", x_range=NA,
                                   xform=NA, prob_vals=c(0.5, 0.025, 0.975)){

  if(type!="direct"){
    if(ecx_val<1 | ecx_val>99){
      stop("Supplied ecx_val is not in the required range. Please supply a percentage value between 1 and 99.")
    }   
  }
  
  if(length(grep("ecx", X$model))>0){mod_class <- "ecx"}else{mod_class <- "nec"}
  if(is.null(X$bot)==FALSE){m4param <- 1}else{m4param <- 0}
  
  if(X$y_type=="gaussian"  &  type=="absolute"){
    stop("Absolute ecx values are not valid for a gaussian response variable unless a 4 parameter model is fit") 
  }
  if(X$x_type=="gaussian" &  X$model == "ecxLinear"  & type=="absolute"){
    stop("Absolute ecx values are not valid for a linear model when
         x-values are gaussian, because 'top' merely indicates the y-intercept. Use type 'relative'.") 
  }  
  
  label <- paste("ec", ecx_val, sep="_")
  
  pred_vals <- predict(X, precision=precision, x_range=x_range)
  posterior_sample <- pred_vals$posterior
  x_vec <- pred_vals$'x' 
  
  
  if(X$model=="necHormesis" & hormesis_def=="max"){ # remove values prior to the nec for the hormesis model
    posterior_sample <- do.call("rbind",
                                lapply(1:nrow(posterior_sample), FUN=function(x){
                                  nec_x <- X$nec_posterior[x]
                                  posterior_x <- posterior_sample[x,]
                                  posterior_x[which(x_vec<nec_x)] <- NA
                                  return(posterior_x)
                                }))
  }
  
  if(X$model=="necHormesis"& hormesis_def=="control"){ # remove values greater than the control for the hormesis model
    posterior_sample <- do.call("rbind", 
                                lapply(1:nrow(posterior_sample), FUN=function(x){
                                  control_x <- posterior_sample[x, 1]                                    
                                  posterior_x <- posterior_sample[x, ] 
                                  posterior_x[which(posterior_x>=control_x)] <- NA
                                  return(posterior_x)
                                }))
  }   
  
  if(type=="relative"){
    ecx_out <- apply(posterior_sample, MARGIN=1, FUN=function(y){
      range_y <- range(y, na.rm=T)
      ecx_y <- max(range_y)-diff(range_y)*(ecx_val/100)
      ecx_x <- x_vec[which.min(abs(y-ecx_y))]
      return(ecx_x)
    })    
  }
  
  
  if(type=="absolute"){
    ecx_out <- apply(posterior_sample, MARGIN=1, FUN=function(y){
      range_y <- c(0, max(y, na.rm=T))
      ecx_y <- max(range_y)-diff(range_y)*(ecx_val/100)
      ecx_x <- x_vec[which.min(abs(y-ecx_y))]
      return(ecx_x)  
    })     
  }
  
  if(type=="direct"){
    ecx_out <- apply(posterior_sample, MARGIN=1, FUN=function(y){
      ecx_y <- ecx_val
      ecx_x <- x_vec[which.min(abs(y-ecx_y))]
      return(ecx_x)
      
    }) 
  }
  
  # calculate the quantile values from the posterior?
  ecx_estimate <- quantile(unlist(ecx_out), probs=prob_vals)
  names(ecx_estimate) <- c(label, paste(label, "lw", sep="_"), paste(label, "up", sep="_"))
  
  # if a transformation is required
  if(class(xform)=="function"){
    ecx_estimate <- xform(ecx_estimate)
    ecx_out <- xform(ecx_out)
  }   
  
  if(posterior==FALSE){
    return(ecx_estimate)
  }else{
    return(ecx_out)}
  
  
}

#' extract_ecx_manec
#'
#' Extracts the predicted ecx value as desired from a bayesnec model fit obeject
#'
#' @param  X a fitted bayesmanec model object, containing a list of jag model fit as returned by a call to bayes from 
#' fit_bayesnec
#' @param ecx_val the desired percentage effect value.
#' @param type a character vector indicating if relative or absolute values for the ecx should be calculated. 
#' @param precision The number of unique x values over which to find ecx.
#' @param posterior A logical value indicating if the full posterior sample of calculated ecx values 
#' should be returned instead of just the median and 95 credible intervals.
#' @param xform A function to apply to the returned estimated concentration values
#' @param prob_vals A vector indicating the probability values over which to return the estimated ecx value. 
#' @param hormesis_def to complete
#' @param x_range A range of x values over which to consider extracting ecx
#' 
#' @export
#' @return A vector containing the estimated ecx value, including upper and lower 95 percent Credible Interval bounds
#' @importFrom stats quantile
extract_ecx_manec <- function(X, ecx_val=10, precision=1000, posterior = FALSE, type="absolute", 
                                     hormesis_def="control", xform=NA, x_range=NA,
                                     prob_vals=c(0.5, 0.025, 0.975)){
  sample_size <- X$sample_size
  ecx_out <- unlist(sapply(1:length(X$success_models), FUN=function(x){
    base::sample(extract_ecx_nec(X$mod_fits[[x]], 
                                        ecx_val=ecx_val, 
                                        precision=precision, 
                                        posterior = TRUE, 
                                        x_range = x_range,
                                        type=type), 
                 as.integer(round(sample_size*X$mod_stats[x, "wi"])))
  }))
  
  label <- paste("ec", ecx_val, sep="_")
  # calculate the quantile values from the posterior
  ecx_estimate <- quantile(ecx_out, probs=prob_vals)
  names(ecx_estimate) <- c(label, paste(label, "lw", sep="_"), paste(label, "up", sep="_"))
  
  # if a transformation is required
  if(class(xform)=="function"){
    ecx_estimate <- xform(ecx_estimate)
    ecx_out <- xform(ecx_out)
  }   
  
  
  if(posterior==FALSE){
    return(ecx_estimate)
  }else{
    return(ecx_out)}
  
  
}
