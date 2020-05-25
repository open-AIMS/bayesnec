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

#' check_data
#'
#' Check data input for a bayesian nec model fit
#'
#' @param  data a data.frame containing the data to use for the model
#' 
#' @param x.var the column heading indicating the concentration (x) variable
#' 
#' @param y.var the column heading indicating the response (y) variable
#' 
#' @param trials.var the column heading indicating the column for the number of "trials" for binomial response data. 
#' If not supplied, the model may run but will not be the model you intended!
#' 
#' @param x.type the statistical distribution to use for the x (concentration) data. This will be guess based on the 
#' characteristic of the input data if not supplied.
#'
#' @param y.type the statistical distribution to use for the y (response) data. This may currently be one of  'binomial', 
#' 'poisson',' 'gaussian', or 'gamma'. Others can be added as required, please contact the package maintainer. 
#' If not supplied, the appropriate distribution will be guessed based on the distribution of the input data.
#'
#' @param over.disp. If an overdispersed model should be used. Only changes the model fit for poisson and binomial y.type 
#' data. For poisson, a negative binomial model will be fit. For binomial a beta model will be fit.
#'
#' @param model The type of model to be fit. Currently takes values of "nec3param",  
#' "nec4param", "necsigmoidal", "nechorme", "ecx4param", "ecxwb1", or "ecxwb2".
#' 
#' @param  params A vector of names indicating the parameters that to trace during the jags fit. For the NEC jags model 
#' this is typically 'NEC','top' and 'beta'. If left out, fit.jagsNEC will supply this based on the selected y.type and 
#' x.type.
#' 
#' @details   
#' 
#' This is a wrapper function to test input data criteria and write the brms model file for use in a bayesnec model fit
#'
#' @export
#' @return Modified elements of the bayesnec input data.

check_data <- function(data,
            x.var,
            y.var,
            trials.var,
            x.type, 
            y.type,
            params,
            over.disp,
            model){
  
  if(is.na(y.type)==F){
    if(over.disp==TRUE & y.type=="beta"){y.type=NA}
  }

  
  # check the specified columns exist in data
  use.vars <- na.omit(c(y.var=y.var, x.var=x.var, trials.var))
  var.colms <- match(use.vars, colnames(data))
  missing.colms <- data.frame(val=use.vars[which(is.na(var.colms))], stringsAsFactors = FALSE)
  missing.colms$element <- rownames(missing.colms)
  if(length(na.omit(var.colms))<length(use.vars)){
    stop(paste("Your indicated ", paste(paste(missing.colms$element, " '", missing.colms$val,"'", sep=""),
                                        collapse = ", "),
               " is not present in your input data. Has this been mispecified?", sep=""))
  }
  
  # extract the data
  y.dat <- data[, y.var]
  x.dat <- data[, x.var]
  
  # check the x data are numeric
  if(class(x.dat)!="numeric"){
    stop(paste("Your indicated x.var column ", x.var," contains data that is class ", class(x.dat),".
                 The function bayesnec requires the concentration data (argument x.var) to be numeric.",sep=""))
  }
  
  # check data contains only finite values
  test.x <- mean(x.dat)
  test.y <- mean(y.dat)
  if(is.finite(test.x)!=TRUE){
    stop("Your x.var column contains values that are not finite.")
  }
  if(is.finite(test.y)!=TRUE){
    stop("Your y.var column contains values that are not finite.")
  }
  
  # check the data are lower at high x compared to low x (ie the response variable declines within increase in the x)
  if(mean(y.dat[which(x.dat<mean(x.dat))])< mean(y.dat[which(x.dat>mean(x.dat))]) & model != "nechorme"){
    stop("The mean value of the response for the lower half of the
           concentration data are lower than that of the upper half of the concentration data.
           bayesnec only fits concentration response data where the
           response declines with increasing values of concentration.")
  }
  
  # check variable type x.var
  if(is.na(x.type)==TRUE){ # if the x.var is not specified, then guess
    if(class(x.dat)=="integer"){
      stop("bayesnec does not currently support integer concentration data. Please provide
           a numeric x.var")}
    if(class(x.dat)=="numeric" & max(x.dat)>1 & min(x.dat)>=0){x.type="gamma"}
    if(class(x.dat)=="numeric" & max(x.dat)<=1 & min(x.dat)>=0){x.type="beta"}
    if(class(x.dat)=="numeric" & min(x.dat)<0){x.type="gaussian"}
  }
  
  # check variable type y.var
  if(is.na(y.type)==T){ # if the y.var is not specified, then guess
    if(class(y.dat)=="numeric" & max(y.dat)>1 & min(y.dat)>=0){y.type="gamma"}
    if(class(y.dat)=="numeric" & max(y.dat)<=1 & min(y.dat)>=0){y.type="beta"}
    if(class(y.dat)=="numeric" & min(y.dat)<0){y.type="gaussian"}
    if(class(y.dat)=="integer" & min(y.dat)>=0 & is.na(trials.var) == TRUE){
      y.type="poisson"}
    if(is.na(trials.var)!= TRUE & class(y.dat)!="integer"){
      stop("You have supplied a trials.var argument, suggesting you wish to model a binomial.
             Please ensure y.var is an integer representing the number of successes.")}
    
    if(class(y.dat)=="integer" & min(y.dat)>=0 & is.na(trials.var)!= TRUE){
      y.type="binomial"}
  }
  
  # check there is a valid model type
  if(is.na(match(model, c("nec3param", "necsigmoidal", "nec4param", "nechorme",
                          "ecx4param", "ecxwb1", "ecxwb2", "ecxlin", "ecxexp", "ecxsigm")
  ))){
    stop("The model type you have specified does not extist.")
  }
  
  if(y.type=="poisson" & over.disp==TRUE){
    y.type="negbin"}
  if(y.type=="binomial" & over.disp==TRUE){
    y.type <- "beta"
    data[,y.var] <-  data[,y.var]/data[,trials.var]
    
  }
  
  if(y.type=="gamma"){params=c(params,"shape")}
  if(y.type=="gaussian"){params=c(params,"alpha","sigma")}
  if(y.type=="negbin"){params=c(params,"size")}
  
  # error catching for 0 for gamma by adding very small value
  if(min(data[,x.var])==0 & x.type=="gamma"){
    tt <- data[,x.var]
    min.val <- min(tt[which(tt>0)])
    data[which(tt==0),x.var] <- tt[which(tt==0)]+(min.val/10)
  }
  
  if(min(data[,y.var])==0 & y.type=="gamma"){
    tt <- data[,y.var]
    min.val <- min(tt[which(tt>0)])
    data[which(tt==0),y.var] <- tt[which(tt==0)]+(min.val/10)
  }
  # error catching for 0 for beta by adding very small value (beta does not take zero)
  if(min(data[,x.var])==0 & x.type=="beta"){
    tt <- data[,x.var]
    min.val <- min(tt[which(tt>0)])
    data[which(tt==0),x.var] <- tt[which(tt==0)]+(min.val/10)
  }
  
  if(min(data[,y.var])==0 & y.type=="beta"){
    tt <- data[,y.var]
    min.val <- min(tt[which(tt>0)])
    data[which(tt==0),y.var] <- tt[which(tt==0)]+(min.val/10)
  }
  
  # error catching for 1 for beta by subtracting very small value (beta does not take 1)
  if(max(data[,x.var])==1 & x.type=="beta"){
    tt <- data[,x.var]
    data[which(tt==1),x.var] <- tt[which(tt==1)]-0.001
  }
  
  if(max(data[,y.var])==1 & y.type=="beta"){
    tt <- data[,y.var]
    data[which(tt==1),y.var] <- tt[which(tt==1)]-0.001
  }
  
  # create brms model data
  mod.dat <<- data.frame(
    x = data[,x.var],   # concentration
    y = data[,y.var], # response (successes)
    
    N = nrow(data))  # Sample size
  
  
  response = data[,y.var]
  
  if(y.type=="binomial"){
    mod.dat$trials = data[, trials.var] # number of "trials"
    response = data[, y.var]/data[,trials.var]
  }
  
 mod.file <- define_model(model=model, x.type=x.type, y.type=y.type, mod.dat=mod.dat)
 bform <- mod.file$bform
 priors <- mod.file$priors
 family.type <- mod.file$family.type
    
 return(list(priors=priors,
            response=response,
            mod.dat=mod.dat,
            data=data,
            y.type=y.type,
            x.type=x.type,
            x.dat=x.dat,
            y.dat=y.dat,
            bform=bform,
            family.type=family.type))  
}