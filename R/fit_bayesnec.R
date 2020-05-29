#' fit_bayesnec
#'
#' Fits a concentration(dose)-response model using brms
#'
#' @inheritParams bayesnec
#'
#' @export
#' @importFrom brms fixef brm posterior_predict posterior_samples loo waic
#' @importFrom stats quantile
#' @return The fitted brms model, including an estimate of the nec value and predicted posterior values.
#' A posterior sample of the nec is also available under $nec_posterior

fit_bayesnec <- function(data, 
                         x_var, 
                         y_var, 
                         trials_var = NA,
                         x_type = NA, 
                         y_type = NA, 
                         x_range = NA, 
                         precision = 1000,
                         over_disp = FALSE, 
                         model = NA, 
                         sig_val = 0.01, 
                         iter = 2e3,
                         warmup = floor(iter/5)*4, 
                          ...) {
  
  if(class(data)== "bayesmanecfit"){
    response <- data$data[, y_var]
    mod_dat <- data$mod_dat
    #data <- data$data
       
    y_dat <- data$y_dat
    x_dat <- data$x_dat   
    y_type <- data$y_type
    x_type <- data$x_type
  
    if (y_type == "binomial") {
      response <- response / data$data[, trials_var]

    }

    mod_file <- define_model(model = model, x_type = x_type,
                             y_type = y_type, mod_dat = mod_dat)
    bform <- mod_file$bform
    priors <- mod_file$priors
    mod_family <- mod_file$mod_family
    
  } else {
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
  }
  
  fit <- brms::brm(bform, data = mod_dat, prior = priors, iter=iter,
                       family = mod_family, warmup = warmup, refresh=0)
  
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
  

  if(is.na(extracted_params$nec["Estimate"])){mod_class <- "ecx"}else{mod_class <- "nec"}
  
  if(is.na(x_range)){
      x_seq <- seq(min(mod_dat$x), max(mod_dat$x), length=precision)
  }

  new_dat <- data.frame(x=x_seq)
  if(y_type=="binomial"){new_dat$trials=10^3}
  
  y_pred_m <- predict(fit, newdata = new_dat, robust = TRUE, re_formula = NA)
  predicted_y <- predict(fit, robust = TRUE, re_formula = NA)

  if(y_type=="binomial"){
    top <- top/10^3
    predicted_y <- predicted_y/10^3
    y_pred_m <-  y_pred_m/10^3
  }
  
  # calculate the residuals
  residuals <-  response - predicted_y 
  
  # entire posterior
  pred_posterior <- t(predict(fit, newdata = new_dat, re_formula = NA, summary = FALSE))
  if(y_type=="binomial"){
    pred_posterior <- pred_posterior/10^3
  }
  
  # calculate the predicted values using the entire posterior
  pred_vals <- c(list(x=x_seq, y=y_pred_m[,"Estimate"], up=y_pred_m[,"Q97.5"], lw=y_pred_m[,"Q2.5"],
                      posterior=pred_posterior), list(y_pred_m=y_pred_m[,"Estimate"]))  

  # Extract the overdispersion estimate
  od <- dispersion(fit, summary = TRUE) 
  if(is.null(od)){od <- c(NA, NA, NA, NA)}
  
  # get the posterior nec 
  
  # calculate the nec from the predicted values for the ecx model
  if(mod_class=="ecx"){
    reference <-  quantile(pred_vals$posterior[1, ], sig_val)
    nec_posterior  <-  sapply(1:ncol(pred_vals$posterior), function (x, pred_vals, reference) {
      pred_vals$x[which.min(abs(pred_vals$posterior[, x] - reference))]
      }, 
      pred_vals = pred_vals, reference = reference)
    
    nec <- quantile(nec_posterior, c(0.5, 0.025,  0.975))  
    names(nec) <- c("Estimate", "Q2.5", "Q97.5")
  }else{
    nec_posterior <- unlist(posterior_samples(fit, pars="nec_Intercept"))
  }
  
  # Put everyting in a list for output
  if(class(out)!="try-error"){
    out <- c(out, list(
      pred_vals = pred_vals,
      nec = nec,
      top = top,
      beta = beta,
      alpha = alpha,
      bot = bot,
      d = d,
      ec50 = ec50,
      over_disp=od,
      predicted_y = predicted_y,
      residuals = residuals,
      nec_posterior = nec_posterior))
    
    # assign a class to the output
    class(out) <- "bayesnecfit"
  }
  
  message(paste("Response variable ", y_var, " modelled as a ", model ," model using a ", y_type, " distribution.", sep=""))
  return(out)    
}


