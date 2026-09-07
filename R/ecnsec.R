#' Extracts the percent effect associated with a predicted NSEC value from an 
#' object of class \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#' @param nsec A numeric value indicating the NSEC value for which to extract 
#' the percentage effect.
#' @param resolution The number of unique x values over which to find NSEC -
#' large values will make the NSEC estimate more precise.
#' @param type A \code{\link[base]{character}} vector, taking values of
#' "absolute" (the default), "relative" or "range". See Details.
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
#' The effect is measured from the control --- the predicted mean at the
#' lowest concentration in the supplied predictor, per posterior draw --- and
#' \code{type} names what it is measured towards, exactly as in
#' \code{\link{ecx}}: "absolute" (the default) towards 0, "relative" towards
#' the equation's theoretical asymptote, "range" towards the lowest response
#' the curve predicts. \code{ecnsec} is the inverse of the \code{ecx}
#' reference construction under the same \code{type}, so the two answer the
#' same question of the same curve. "direct" names a response value rather
#' than a percentage and is refused here.
#'
#' The \code{hormesis_def} argument has been removed; the control is now
#' always the reference. See \code{\link{ecx}}.
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
#' ecnsec(manec_example, nsec = 2)
#' }
#'
#' @export
ecnsec <- function(object, nsec, resolution = 1000, x_range = NA, 
                   type = "absolute",
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
                               type = "absolute",
                             xform = identity, prob_vals = c(0.5, 0.025, 0.975), ..., 
                             posterior = FALSE) {
  chk_numeric(nsec)
  chk_logical(posterior)
  check_removed_args(list(...))

  type <- validate_ecx_type(type, match.call())
  if (identical(type, "direct")) {
    stop("type = \"direct\" names a response value rather than a ",
         "percentage, so there is no percent effect for ecnsec to report. ",
         "Use type = \"absolute\" (the default), \"relative\" or ",
         "\"range\".", call. = FALSE)
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

  p_samples <- posterior_epred(object, 
                               newdata = newdata_list$newdata,
                               re_formula = NA)
  newdat_nsec <- newdata_eval(
    object, resolution = 2, x_range = c(nsec_use, nsec_use)
  )

  pred_val_nsec <- posterior_epred(object, 
                                   newdata = newdat_nsec$newdata,
                                   re_formula = NA)
  reference <- median(pred_val_nsec[, 2])

  # The percent effect at the NSEC is the inverse of the ecx reference
  # construction under the same type, so ecnsec and ecx answer the same
  # question of the same curve. The control is the predicted mean at the
  # lowest observed concentration, per draw, and the denominator is the span
  # from the control to whatever that type measures towards. The branch this
  # replaces used the maximum of the curve as the control when hormesis_def
  # was "max", and the response at the highest concentration as the floor
  # under "relative", neither of which matches ecx. See toxval#49, T8, and
  # D15 ruling 5.
  control_draws <- control_posterior(
    object, newdata_list$newdata,
    function(nd) posterior_epred(object, newdata = nd, re_formula = NA)
  )
  floor_draws <- switch(
    type,
    absolute = 0,
    relative = ecx_asymptote(object, "relative"),
    range = apply(p_samples, 1, min, na.rm = TRUE)
  )
  ecnsecP <- (control_draws - reference) / (control_draws - floor_draws) * 100
  ecnsec <- quantile(ecnsecP, probs = prob_vals, na.rm = TRUE)

    if (!posterior) {
      ecnsec
  } else {
      ecnsecP
  }
}




