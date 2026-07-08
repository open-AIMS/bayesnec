#' Extracts the percent effect associated with a predicted NSEC value from an 
#' object of class \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#' @param nsec A numeric value indicating the NSEC value for which to extract 
#' the percentage effect.
#' @param resolution The number of unique x values over which to find NSEC -
#' large values will make the NSEC estimate more precise.
#' @param hormesis_def A \code{\link[base]{character}} vector, taking values
#' @param type A \code{\link[base]{character}} vector, taking values of
#' "relative", "absolute" (the default) or "direct". See Details.
#' @param xform A function to apply to the returned estimated NSEC concentration
#' values prior to estimating the percentage effect.
#' @param x_range A range of x values over which to consider extracting NSEC.
#' @param prob_vals A vector indicating the probability values over which to
#' return the estimated ECNSEC value. Defaults to 0.5 (median) and 0.025 and
#' 0.975 (95 percent credible intervals).
#' @param ... Further arguments to pass to class specific methods.
#'
#' @details NSEC is no-effect toxicity metric that estimates the concentration 
#' at which the modeled mean response is statistically indistinguishable from 
#' the mean control response. See the detailed derivation in
#' Fisher and Fox (2023). Like NOEC, NSEC estimates will have an associated 
#' effect size. This function estimates the effect of a given NSEC estimate.
#' 
#' For \code{hormesis_def}, if "max", then the ECNSEC values are calculated
#' as a decline from the maximum estimates (i.e. the peak);
#' if "control", then ECNSEC values are calculated relative to the control, which
#' is assumed to be the lowest observed concentration in the input data contained
#' in the supplied model fit.
#' 
#' For \code{type} "relative" is calculated as the percentage decrease
#' from the maximum predicted value of the response (top) to the minimum
#' predicted value of the response. Type "absolute" (the default) is
#' calculated as the percentage decrease from the maximum value of the
#' response (top) to 0. Type "direct"
#' provides a direct estimate of the x value for a given y.
#' Note that for the current version, ECx for an "nechorme" (NEC Hormesis)
#' model is estimated at a percent decline from the control.
#'
#' @seealso \code{\link{bnec}}
#'
#' @return A vector containing the estimated ECNSEC value, including upper and
#' lower 95% credible interval bounds.
#'
#' @examples
#' \donttest{
#' library(bayesnec)
#'
#' data(manec_example)
#' nsec_vals <- nsec(manec_example)
#' ecnsec(manec_example, nsec = nsec_vals)
#' }
#'
#' @export
ecnsec <- function(object, nsec, resolution = 1000, x_range = NA, 
                   hormesis_def = "control", type = "absolute",
                 xform = identity, prob_vals = c(0.5, 0.025, 0.975), ...) {
  UseMethod("ecnsec")
}

#' @inheritParams ecnsec
#'
#' @param object An object of class \code{\link{bayesnecfit}} returned by
#' \code{\link{bnec}}.
#' @param posterior A \code{\link[base]{logical}} value indicating if the full
#' posterior sample of calculated ECNSEC values should be returned instead of
#' just the median and 95 credible intervals.
#'
#' @inherit ecnsec details seealso return examples
#' 
#' @importFrom stats quantile
#' @importFrom brms as_draws_df posterior_epred
#' @importFrom chk chk_logical chk_numeric
#' 
#' @noRd
#'
#' @export
ecnsec.bnecfit <- function(object, nsec, resolution = 10, x_range = NA, 
                               hormesis_def = "control", type = "absolute",
                             xform = identity, prob_vals = c(0.5, 0.025, 0.975), ..., 
                             posterior = FALSE) {
  chk_numeric(nsec)
  chk_logical(posterior)

  if ((hormesis_def %in% c("max", "control")) == FALSE) {
    stop("type must be one of \"max\" or \"control\" (the default). ",
         "Please see ?ecx for more details.")
  }
  if(!inherits(xform, "function")) { 
    stop("xform must be a function.")}  
  if (length(prob_vals) < 3 | prob_vals[1] < prob_vals[2] |
      prob_vals[1] > prob_vals[3] | prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order.")
  }
  if (inherits(xform, "function")) {
    nsec_use <- xform(nsec)
  }

  newdata_list <- newdata_eval(
    object, resolution = resolution, x_range = x_range
  )
  x_name <- colnames(newdata_list$newdata)
  p_samples <- posterior_epred(object, 
                               newdata = newdata_list$newdata,
                               re_formula = NA)
  x_vec <- newdata_list$x_vec

  newdat_nsec <- data.frame(x=c(max(x_vec), nsec[1]))
  colnames(newdat_nsec) <- x_name
  pred_val_nsec <- posterior_epred(object, 
                                   newdata = newdat_nsec,
                                   re_formula = NA)
  reference <- median(pred_val_nsec[, 2])

  if (hormesis_def == "max") {
    control_posterior <- apply(p_samples, 1, max)
  } else {
    control_posterior <- p_samples[, 1]
  }
  
  if(type=="relative"){  
    min_posterior <- p_samples[, ncol(p_samples)]
  } else {
    min_posterior <- 0   
  }
  
  dif_valsC <- control_posterior-min_posterior
  
  ecnsecP <-  (control_posterior -  reference)/  dif_valsC * 100

  ecnsec <- quantile(ecnsecP, probs = prob_vals)

    if (!posterior) {
      ecnsec
  } else {
      ecnsecP
  }
}




