#' check_data
#'
#' Check data input for a bayesian nec model fit
#'
#' @param  data a data.frame containing the data to use for the model
#' 
#' @param x_var the column heading indicating the concentration (x) variable
#' 
#' @param y_var the column heading indicating the response (y) variable
#' 
#' @param trials_var the column heading indicating the column for the number of "trials" for binomial response data. 
#' If not supplied, the model may run but will not be the model you intended!
#' 
#' @param x_type the statistical distribution to use for the x (concentration) data. This will be guess based on the 
#' characteristic of the input data if not supplied.
#'
#' The argument \code{y_type} is a character vector indicating the family to use for the response variable in the brms call, 
#' and may currently be one of "binomial", "beta", poisson", "negbin","gaussian", or "gamma". 
#' Others can be added as required, please raise an issue on the github development site if your required familiy is not currently available. 
#' If not supplied, the appropriate distribution will be guessed based on the characteristics of the input data.
#'
#' @param over_disp If an overdispersed model should be used. Only changes the model fit for poisson and binomial y_type 
#' data. For poisson, a negative binomial model will be fit. For binomial a beta model will be fit.
#'
#' @param model The type of model to be fit. Currently takes values of "nec3param",  
#' "nec4param", "necsigmoidal", "nechorme", "ecx4param", "ecxwb1", or "ecxwb2".
#' 
#' @details
#' 
#' This is a wrapper function to test input data criteria and write the brms model file for use in a bayesnec model fit
#'
#' @importFrom stats na.omit
#' @export
#' @return Modified elements of the bayesnec input data.

check_data <- function(data,
            x_var,
            y_var,
            trials_var,
            x_type = NA, 
            y_type = NA,
            over_disp,
            model){
  
  if (!is.na(y_type)) {
    if (over_disp & y_type == "beta") {
      y_type <- NA
    }
    # check y_type is a valid family
    if (!y_type %in% c("binomial", "beta", "poisson", "negbin","gamma", "gaussian")) {
    stop(paste("You have specified y-type as", y_type, "which is not currently implemented."))
  }   
    
  }
  

  
  # check the specified columns exist in data
  use_vars <- na.omit(c(y_var = y_var, x_var = x_var, trials_var))
  var_colms <- match(use_vars, colnames(data))
  missing_colms <- data.frame(val = use_vars[which(is.na(var_colms))], stringsAsFactors = FALSE)
  missing_colms$element <- rownames(missing_colms)
  if (length(na.omit(var_colms)) < length(use_vars)) {
    stop(paste0("Your indicated ", paste(paste0(missing_colms$element, " '", missing_colms$val,"'"),
                                        collapse = ", "),
               " is not present in your input data. Has this been mispecified?"))
  }
  
  # extract the data
  y_dat <- data[, y_var]
  x_dat <- data[, x_var]
  
  # check the x data are numeric
  if (!inherits(x_dat, "numeric")) {
    stop(paste0("Your indicated x_var column ", x_var," contains data that is class ", class(x_dat),".
                 The function bayesnec requires the concentration data (argument x_var) to be numeric."))
  }
  
  # check data contains only finite values
  test_x <- mean(x_dat)
  test_y <- mean(y_dat)
  if (!is.finite(test_x)) {
    stop("Your x_var column contains values that are not finite.")
  }
  if (!is.finite(test_y)) {
    stop("Your y_var column contains values that are not finite.")
  }
  
  # check the data are lower at high x compared to low x (ie the response variable declines within increase in the x)
  resp_check <- mean(y_dat[which(x_dat < mean(x_dat))]) < mean(y_dat[which(x_dat > mean(x_dat))])
  if (resp_check & model != "nechorme") {
    stop("The mean value of the response for the lower half of the
           concentration data are lower than that of the upper half of the concentration data.
           bayesnec only fits concentration response data where the
           response declines with increasing values of concentration.")
  }
  
  # check variable type x_var
  if (is.na(x_type)) {
    x_type <- set_distribution(x_dat)
  }
  if (is.na(y_type)) {
    y_type <- set_distribution(y_dat, support_integer = TRUE,
                               trials = data[, trials_var])
  }
  
  # check there is a valid model type
  if (!model %in% c("nec3param", "necsigm", "nec4param", "nechorme",
                    "ecx4param", "ecxwb1", "ecxwb2", "ecxlin", "ecxexp", "ecxsigm")) {
    stop("The model type you have specified does not exist.")
  }
  
  if (y_type == "poisson" & over_disp) {
    y_type <- "negbin"
  }
  if (y_type == "binomial" & over_disp) {
    y_type <- "beta"
    data[,y_var] <-  data[, y_var] / data[, trials_var]
  }
  
  # error catching for 0 for gamma by adding very small value
  if (min(data[,x_var])==0 & x_type=="gamma"){
    tt <- data[,x_var]
    min_val <- min(tt[which(tt>0)])
    data[which(tt==0),x_var] <- tt[which(tt==0)]+(min_val/10)
  }
  
  if (min(data[,y_var])==0 & y_type=="gamma"){
    tt <- data[,y_var]
    min_val <- min(tt[which(tt>0)])
    data[which(tt==0),y_var] <- tt[which(tt==0)]+(min_val/10)
  }
  # error catching for 0 for beta by adding very small value (beta does not take zero)
  if (min(data[,x_var])==0 & x_type=="beta"){
    tt <- data[,x_var]
    min_val <- min(tt[which(tt>0)])
    data[which(tt==0),x_var] <- tt[which(tt==0)]+(min_val/10)
  }
  
  if (min(data[,y_var])==0 & y_type=="beta"){
    tt <- data[,y_var]
    min_val <- min(tt[which(tt>0)])
    data[which(tt==0),y_var] <- tt[which(tt==0)]+(min_val/10)
  }
  
  # error catching for 1 for beta by subtracting very small value (beta does not take 1)
  if (max(data[,x_var])==1 & x_type=="beta"){
    tt <- data[,x_var]
    data[which(tt==1),x_var] <- tt[which(tt==1)]-0.001
  }
  
  if (max(data[,y_var])==1 & y_type=="beta"){
    tt <- data[,y_var]
    data[which(tt==1),y_var] <- tt[which(tt==1)]-0.001
  }
  
  # create brms model data
  mod_dat <- data.frame(x = data[,x_var],   # concentration
                         y = data[,y_var], # response (successes)
                         N = nrow(data))  # Sample size
  
  response <- data[, y_var]
  
  if (y_type == "binomial") {
    mod_dat$trials <- data[, trials_var] # number of "trials"
    response <- data[, y_var] / data[, trials_var]
  }

  mod_file <- define_model(model=model, x_type=x_type, y_type=y_type, mod_dat=mod_dat)
  bform <- mod_file$bform
  priors <- mod_file$priors
  mod_family <- mod_file$mod_family
    
  list(priors = priors,
       response = response,
       mod_dat = mod_dat,
       data = data,
       y_type = y_type,
       x_type = x_type,
       x_dat = x_dat,
       y_dat = y_dat,
       bform = bform,
       mod_family = mod_family)
}
