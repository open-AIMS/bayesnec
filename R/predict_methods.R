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


#' predict.bayesnec
#'
#' 
#' @param X the bayesnec model fit (as returned by fit_bayesnec)
#' 
#' @param precision the number of x values over which to predict values
#' 
#' @param x_range The range of x values over which to make predictions
#'
#' @export
#' @return A list containing x and fitted y, with up and lw values

predict.bayesnecfit <- function(X, precision=100, x_range=NA){
  mod_dat <- X$mod_dat

  y_type <- X$y_type
  x_type <- X$x_type
  
  fit <- X$fit
  
  if(is.na(x_range[1])){
    x_seq <- seq(min(mod_dat$x), max(mod_dat$x), length=precision)
  }else{
    x_seq <- seq(min(x_range), max(x_range), length=precision)}

  new.dat <- data.frame(x=x_seq)
  if(y_type=="binomial"){new.dat$trials=10^3}
  
  # entire posterior
  pred.vals.out <- posterior_predict(fit, newdata = new.dat, re_formula = NA, summary = FALSE)
  if(y_type=="binomial"){
    pred.vals.out <- pred.vals.out/10^3
  }
  
  m.vals <- apply(pred.vals.out, MARGIN=2, FUN=quantile, probs=0.5)
  up.vals <- apply(pred.vals.out, MARGIN=2, FUN=quantile, probs=0.975)
  lw.vals <- apply(pred.vals.out, MARGIN=2, FUN=quantile, probs=0.025)
  
  return(list(
    x=x_seq,
    y=m.vals,
    up=up.vals,
    lw=lw.vals,
    posterior=pred.vals.out
  ))
}