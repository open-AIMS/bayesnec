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

#' fit_bayesnec
#'
#' Fits an nec model as per Fox 2010, using bayes and R2bayes
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
#' @param  params A vector of names indicating the parameters that to trace during the bayes fit. For the nec bayes model 
#' this is typically 'nec','top' and 'beta'. If left out, fit_bayesnec will supply this based on the selected y.type and 
#' x.type.
#'
#' @param over.disp. If an overdispersed model should be used. Only changes the model fit for poisson and binomial y.type 
#' data. For poisson, a negative binomial model will be fit. For binomial a beta model will be fit.
#'
#' @param model The type of model to be fit. Currently takes values of "nec3param",  
#' "nec4param", "necsigm", "nechorme", "ecx4param", "ecxwb1", "ecxexp", "ecxlin", or "ecxwb2".
#' 
#' @param sig.val Probability value to use as the lower quantile to test significance of the predictor posterior values
#' against the control, to estimate nec as an interpolated NOEC value from smooth ecx curves.
#' 
#' @param iter Number of iterations, default is 2e3
#' 
#' @param chains Number of chains, default is 3
#' 
#' @param cores Number of cores, default is 3
#'
#' @details   
#' 
#' As some concentration-response data will use zero concentration, 
#' and there is no distribution on the continuous scale from 0 to in (ie tweedie) available in bayes, a small offset 
#' is added (1/10^3 of the next lowest value) to zero values of concentration where x.var are gamma distributed.
#' 
#' If the initial model fit fails because it cannot be fit by bayes (miss-specified priors, invalid 
#' initial values), or because there is poor chain mixing fit_bayesnec will try n.tries many times using the init.fun 
#' defined by the write model function. If all those attempts fail, fit_bayesnec will then try using default values 
#' provided by bayes. The function will only return an error if all n.tries fail.
#' 
#' All models other than "nec3param" (which is that defined by Fox 2010) are currently undergoing beta testing and are 
#' experimental. These should not yet be used for nec reporting for official purposes. Comments and feedback are welcome, 
#' especially reproducible examples of issues, as well as example test cases.
#' 
#' All models provide an estimate for nec. For model types with "nec" as a prefix, nec is directly estimated as a paremeter 
#' in the model. Models with "ecx" as a prefix are continuos curve models, tyipically used for extracting ecx values 
#' from concentration response data. In this instance the nec value is defined as the concentration at which there is 
#' 90 percent certainty (based on the Bayesian posterior estimate) that the response falls below the estimated value of
#' the upper assymptote (top) of the response (i.e the response value is significantly lower than that expected in the case of
#' no exposure).
#'
#' @export
#' @return The $BUGSoutput element of fitted bayes model, including an estimate of the nec value. 
#' A posterior sample of the nec is also available under $sims.list.

fit_bayesnec <- function(data,
                        x.var,
                        y.var,
                        trials.var = NA,
                        x.type = NA, 
                        y.type = NA,
                        params=c("top", "beta", "nec", "SS", "SSsim"),
                        over.disp=FALSE,
                        model="nec3param",
                        added.model=FALSE,
                        sig.val=0.025,
                        iter = 2e4,
                        chains = 3, 
                        cores = 3,
                        x.seq=NA,
                        precision=1000,
                        ...){
  if(added.model==FALSE){
    data.check <- check_data(data=data,
                                x.var=x.var,
                                y.var=y.var,
                                trials.var = trials.var,
                                x.type = x.type, 
                                y.type = y.type,
                                params=params,
                                over.disp=over.disp,
                                model=model)
    mod.dat <- data.check$mod.dat
    
    y.type <- data.check$y.type
    x.type <- data.check$x.type
    response <- data.check$response
    data <- data.check$data
    x.dat <- data.check$x.dat
    y.dat <- data.check$y.dat 
    params <- data.check$params 
    init.fun <- data.check$init.fun
    bform <- data.check$bform
    priors <- data.check$priors  
    family.type <- data.check$family.type  
    
  }else{
    
    response = data[,y.var]
    
    if(y.type=="binomial"){
      mod.dat$trials = data[, trials.var] # number of "trials"
      response = data[, y.var]/data[,trials.var]
    }
    
    y.dat <- data[, y.var]
    x.dat <- data[, x.var]
    
    mod.file <- define_model(model=model, x.type=x.type, y.type=y.type, mod.dat=mod.dat)
    bform <- mod.file$bform
    priors <- mod.file$priors
    family.type <- mod.file$family.type
  }
  
  
  #link <- attributes(init.fun)$link    
  fit <- fit_bayesMod(mod.dat, bform, priors, family.type, iter, chains, cores)

  out <- list(fit=fit, mod.dat=mod.dat, y.type = y.type, x.type = x.type, model = model)
  
  # extract the relevant model parameters
  extract.params <- c("top", "beta", "nec", "alpha", "bot", "d", "slope", "ec50")
  extracted.params <- lapply(extract.params, FUN=function(x){
    tt <- fixef(fit)[grep(x, rownames(fixef(fit))), c("Estimate", "Q2.5", "Q97.5") ]
    if(is.na(tt["Estimate"])){tt=NA}
    return(tt)
    
  })
  names(extracted.params) <- extract.params

  top <- extracted.params$top
  beta <- extracted.params$beta
  nec <- extracted.params$nec
  alpha <- extracted.params$alpha
  bot <- extracted.params$bot
  d <- extracted.params$d
  slope <- extracted.params$slope
  ec50 <- extracted.params$ec50 
  

  if(is.na(extracted.params$nec["Estimate"])){mod.class <- "ecx"}else{mod.class <- "nec"}
  
  if(is.na(x.seq)){
      x.seq <- seq(min(mod.dat$x), max(mod.dat$x), length=precision)
  }

  new.dat <- data.frame(x=x.seq)
  if(y.type=="binomial"){new.dat$trials=10^3}
  
  y.pred.m <- predict(fit, newdata = new.dat, robust = TRUE, re_formula = NA)
  predicted.y <- predict(fit, robust = TRUE, re_formula = NA)

  if(y.type=="binomial"){
    top <- top/10^3
    predicted.y <- predicted.y/10^3
    y.pred.m <-  y.pred.m/10^3
  }
  
  # calculate the residuals
  residuals <-  response - predicted.y 
  
  # entire posterior
  pred.posterior <- t(predict(fit, newdata = new.dat, re_formula = NA, summary = FALSE))
  if(y.type=="binomial"){
    pred.posterior <- pred.posterior/10^3
  }
  
  # calculate the predicted values using the entire posterior
  pred.vals <- c(list(x=x.seq, y=y.pred.m[,"Estimate"], up=y.pred.m[,"Q97.5"], lw=y.pred.m[,"Q2.5"],
                      posterior=pred.posterior), list(y.pred.m=y.pred.m[,"Estimate"]))  

  
  # Extract the overdispersion estimate
  od <- NA#mean(out$sims.list$SS > out$sims.list$SSsim)
  
  # get the posterior nec 
  
  # calculate the nec from the predicted values for the ecx model
  if(mod.class=="ecx"){
    reference <-  quantile(pred.vals$posterior[1, ], sig.val)
    nec.posterior  <-  sapply(1:ncol(pred.vals$posterior), function (x, pred.vals, reference) {
      pred.vals$x[which.min(abs(pred.vals$posterior[, x] - reference))]
      }, 
      pred.vals = pred.vals, reference = reference)
    
    nec <- quantile(nec.posterior, c(0.5, 0.025,  0.975))  
    names(nec) <- c("Estimate", "Q2.5", "Q97.5")
  }else{
    nec.posterior <- unlist(posterior_samples(fit, pars="nec_Intercept"))
  }
  

  
  # Put everyting in a list for output
  if(class(out)!="try-error"){
    out <- c(out, list(
      pred.vals = pred.vals,
      nec = nec,
      top = top,
      beta = beta,
      alpha = alpha,
      bot = bot,
      d = d,
      ec50 = ec50,
      #params = params,
      over.disp=od,
      predicted.y = predicted.y,
      residuals = residuals,
      nec.posterior = nec.posterior))
    
    # assign a class to the output
    class(out) <- "bayesnecfit"
  }
  
  message(paste("Response variable ", y.var, " modelled using a ", y.type, " distribution.", sep=""))
  return(out)    
}


