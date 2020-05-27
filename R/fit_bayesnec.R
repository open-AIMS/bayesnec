#' fit_bayesnec
#'
#' Fits an nec model as per Fox 2010, using bayes and R2bayes
#'
#' @param data a data.frame containing the data to use for the model
#' @param x_var the column heading indicating the concentration (x) variable
#' @param y_var the column heading indicating the response (y) variable
#' @param trials_var the column heading indicating the column for the number of "trials" for binomial response data. 
#' If not supplied, the model may run but will not be the model you intended!
#'#' @param x_type the statistical distribution to use for the x (concentration) data. This will be guess based on the 
#' characteristic of the input data if not supplied.
#' @param y_type the statistical distribution to use for the y (response) data. This may currently be one of  'binomial', 
#' 'poisson',' 'gaussian', or 'gamma'. Others can be added as required, please contact the package maintainer. 
#' If not supplied, the appropriate distribution will be guessed based on the distribution of the input data.
#' @param over_disp If an overdispersed model should be used. Only changes the model fit for poisson and binomial y_type 
#' data. For poisson, a negative binomial model will be fit. For binomial a beta model will be fit.
#' @param model The type of model to be fit. Currently takes values of "nec3param",  
#' "nec4param", "necsigm", "nechorme", "ecx4param", "ecxwb1", "ecxexp", "ecxlin", or "ecxwb2".
#' @param added_model Logical. something
#' @param sig_val Probability value to use as the lower quantile to test significance of the predictor posterior values
#' against the control, to estimate nec as an interpolated NOEC value from smooth ecx curves.
#' @param x_range The range of x values over which to obtained posterior predictions. Used for plot.bayesnecfit and to calculate nec for ecx models.
#' @param precision The length of the x_seq to pass to posterior_predict as new data. Used for plot.bayesnecfit and to calculate nec for ecx models.
#' @param iter the number of interations to for the brms fit. Defaults to 2e4.
#' @param ... further arguments to be passed to \code{\link[brms]{brm}}.
#' 
#' @details   
#' 
#' As some concentration-response data will use zero concentration, 
#' and there is no distribution on the continuous scale from 0 to in (ie tweedie) available in bayes, a small offset 
#' is added (1/10^3 of the next lowest value) to zero values of concentration where x_var are gamma distributed.
#' 
#' If the initial model fit fails because it cannot be fit by bayes (miss-specified priors, invalid 
#' initial values), or because there is poor chain mixing fit_bayesnec will try n.tries many times using the init_fun 
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
#' @importFrom brms fixef brm
#' @return The $BUGSoutput element of fitted bayes model, including an estimate of the nec value. 
#' A posterior sample of the nec is also available under $sims.list.
fit_bayesnec <- function(data, x_var, y_var, trials_var = NA,
                         x_type = NA, y_type = NA,
                         over_disp = FALSE, model = "nec3param",
                         added_model = FALSE, sig_val = 0.025,
                         x_range = NA, precision = 1000, iter=2e4, ...) {
  
  if (!added_model) {
    data_check <- check_data(data = data, x_var = x_var, y_var = y_var,
                             trials_var = trials_var, x_type = x_type,
                             y_type = y_type,
                             over_disp = over_disp, model = model)
    mod_dat <- data_check$mod_dat
    y_type <- data_check$y_type
    x_type <- data_check$x_type
    response <- data_check$response
    data <- data_check$data
    x_dat <- data_check$x_dat
    y_dat <- data_check$y_dat 
    init_fun <- data_check$init_fun
    bform <- data_check$bform
    priors <- data_check$priors  
    mod_family <- data_check$mod_family
  } else {
    response <- data[, y_var]
    
    if (y_type == "binomial") {
      response <- response / data[, trials_var]
    }
    
    y_dat <- data[, y_var]
    x_dat <- data[, x_var]
    
    mod_file <- define_model(model = model, x_type = x_type,
                             y_type = y_type, mod_dat = data)
    bform <- mod_file$bform
    priors <- mod_file$priors
    mod_family <- mod_file$mod_family
  }
  
  fit <- brms::brm(bform, data = mod_dat, prior = priors, warmup = floor(iter/5)*4, iter=iter,
                       family = mod_family, ...)
  
  fit$loo <- brms::loo(fit)
  fit$waic <- brms::waic(fit)

  out <- list(fit = fit, mod_dat = mod_dat,
              y_type = y_type, x_type = x_type, model = model)

  # extract the relevant model parameters
  extract_params <- c("top", "beta", "nec", "alpha", "bot", "d", "slope", "ec50")
  extracted_params <- lapply(extract_params, function(x, model_fit) {
    fef <- fixef(model_fit)
    tt <- fef[grep(x, rownames(fef)), c("Estimate", "Q2.5", "Q97.5")]
    if (is.na(tt["Estimate"])) {
      NA
    } else {
      tt
    }
  }, model_fit = fit)
  names(extracted_params) <- extract_params

  top <- extracted_params$top
  beta <- extracted_params$beta
  nec <- extracted_params$nec
  alpha <- extracted_params$alpha
  bot <- extracted_params$bot
  d <- extracted_params$d
  slope <- extracted_params$slope
  ec50 <- extracted_params$ec50 
  

  if(is.na(extracted_params$nec["Estimate"])){mod.class <- "ecx"}else{mod.class <- "nec"}
  
  if(is.na(x_range)){
      x_seq <- seq(min(mod_dat$x), max(mod_dat$x), length=precision)
  }

  new.dat <- data.frame(x=x_seq)
  if(y_type=="binomial"){new.dat$trials=10^3}
  
  y.pred.m <- predict(fit, newdata = new.dat, robust = TRUE, re_formula = NA)
  predicted.y <- predict(fit, robust = TRUE, re_formula = NA)

  if(y_type=="binomial"){
    top <- top/10^3
    predicted.y <- predicted.y/10^3
    y.pred.m <-  y.pred.m/10^3
  }
  
  # calculate the residuals
  residuals <-  response - predicted.y 
  
  # entire posterior
  pred.posterior <- t(predict(fit, newdata = new.dat, re_formula = NA, summary = FALSE))
  if(y_type=="binomial"){
    pred.posterior <- pred.posterior/10^3
  }
  
  # calculate the predicted values using the entire posterior
  pred.vals <- c(list(x=x_seq, y=y.pred.m[,"Estimate"], up=y.pred.m[,"Q97.5"], lw=y.pred.m[,"Q2.5"],
                      posterior=pred.posterior), list(y.pred.m=y.pred.m[,"Estimate"]))  

  
  # Extract the overdispersion estimate
  od <- NA#mean(out$sims.list$SS > out$sims.list$SSsim)
  
  # get the posterior nec 
  
  # calculate the nec from the predicted values for the ecx model
  if(mod.class=="ecx"){
    reference <-  quantile(pred.vals$posterior[1, ], sig_val)
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
      over_disp=od,
      predicted.y = predicted.y,
      residuals = residuals,
      nec.posterior = nec.posterior))
    
    # assign a class to the output
    class(out) <- "bayesnecfit"
  }
  
  message(paste("Response variable ", y_var, " modelled using a ", y_type, " distribution.", sep=""))
  return(out)    
}


