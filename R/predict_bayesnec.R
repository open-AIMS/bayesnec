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
#' @param x.range The range of x values over which to make predictions
#'
#' @export
#' @return A list containing x and fitted y, with up and lw values

predict.bayesnec <- function(X, precision=100, x.range=NA){
  mod_dat <- X$mod_dat
  min.x <- min(mod_dat$x)
  max.x <- max(mod_dat$x)
  
  
  y_type <- X$y_type
  x_type <- X$x_type
  
  fit <- X$fit
  
  if(is.na(x.range[1])){
    x.seq <- seq(min.x, max.x, length=precision)
  }else{
    x.seq <- seq(min(x.range), max(x.range), length=precision)}

  new.dat <- data.frame(x=x.seq)
  if(y_type=="binomial"){new.dat$trials=10^3}
  
  # entire posterior
  pred.vals.out <- predict(fit, newdata = new.dat, re_formula = NA, summary = FALSE)
  if(y_type=="binomial"){
    pred.vals.out <- pred.vals.out/10^3
  }
  
  m.vals <- apply(pred.vals.out, MARGIN=2, FUN=quantile, probs=0.5)
  up.vals <- apply(pred.vals.out, MARGIN=2, FUN=quantile, probs=0.975)
  lw.vals <- apply(pred.vals.out, MARGIN=2, FUN=quantile, probs=0.025)
  
  return(list(
    x=x.seq,
    y=m.vals,
    up=up.vals,
    lw=lw.vals,
    posterior=pred.vals.out
  ))
}