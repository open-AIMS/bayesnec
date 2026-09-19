#' Extracts the predicted NEC value as desired from an object of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#' @param posterior A \code{\link[base]{logical}} value indicating if the full
#' posterior sample of calculated NEC values should be returned instead of
#' just the median and 95% credible intervals.
#' @param xform A function to apply to the returned estimated concentration
#' values.
#' @param prob_vals A vector indicating the probability values over which to
#' return the estimated NEC value. Defaults to 0.5 (median) and 0.025 and
#' 0.975 (95 percent credible intervals).
#' @param ... Additional arguments passed to methods.
#'
#' @seealso \code{\link{bnec}}, \code{\link{nsec}}, \code{\link{summary}}
#'
#' @details The NEC is a parameter in a threshold model (for example,
#' see Fox 2010), and is a true measure
#' of No-effect-concentration (the minimum concentration above which an effect
#' is predicted to occur.
#'
#' \bold{What is actually returned depends on the model set.} Despite the
#' function name, \code{nec} returns a no-effect estimate of whichever type the
#' fitted model(s) support, following the convention set out in Fisher et al.
#' (2023):
#'
#' \itemize{
#'   \item For a \code{\link{bayesnecfit}} holding a threshold (\code{nec}-type)
#'     model, the \bold{NEC}: the \code{nec} parameter itself.
#'   \item For a \code{\link{bayesnecfit}} holding a smooth (\code{ecx}-type)
#'     model, which has no threshold parameter, the \bold{NSEC} (Fisher and Fox
#'     2023) -- the concentration at which the fitted curve falls below a
#'     reference taken from the \code{sig_val} quantile of the control
#'     posterior. This is computed when the model is fitted, using the
#'     \code{sig_val} passed to \code{\link{bnec}}.
#'   \item For a \code{\link{bayesmanecfit}} whose model set contains both
#'     kinds, the weighted posterior mixes NEC draws from the threshold models
#'     with NSEC draws from the smooth ones, in proportion to the model
#'     weights. The result is the model-averaged \bold{N(S)EC}, and a message is
#'     emitted to say so. It is not a pure NEC and should not be reported as
#'     one.
#' }
#'
#' \code{\link{summary}} labels the estimate NEC, NSEC or N(S)EC accordingly,
#' and is the better choice where the type matters, because \code{nec} always
#' returns an unlabelled vector. Use \code{\link{nsec}} where a NSEC is wanted
#' from every model regardless of type.
#'
#' The default \code{nec} prior is bounded by the tested predictor range.
#' Where the recorded combined estimate is a NEC, \code{nec} reports when the
#' posterior median or upper interval limit reaches a common fitted prior upper
#' bound. The estimate may then be constrained by that prior and should be
#' treated as censored unless its bound and shape are scientifically justified.
#' No bound diagnostic is inferred for a mixed NEC/NSEC model average, a joint
#' fit with a smooth response block, or component models with different bounds.
#' Contact with the bound is assessed on the fitted scale before \code{xform},
#' so decreasing and non-monotone transformations cannot reverse or otherwise
#' change the quantile used for the comparison. The bound is transformed by
#' \code{xform} only for display.
#'
#' @return A vector containing the estimated no-effect value, including upper
#' and lower 95% credible interval bounds (or other interval as specified by
#' prob_vals).
#'
#' @references
#' Fisher R, Fox DR (2023). Introducing the no significant effect concentration
#' (NSEC). Environmental Toxicology and Chemistry, 42(9), 2019–2028.
#' doi: 10.1002/etc.5610.
#'
#' Fisher R, Fox DR, Negri AP, van Dam J, Flores F, Koppel D (2023). Methods for
#' estimating no-effect toxicity concentrations in ecotoxicology. Integrated
#' Environmental Assessment and Management. doi: 10.1002/ieam.4809.
#'
#' Fox DR (2010). A Bayesian Approach for Determining the No Effect
#' Concentration and Hazardous Concentration in Ecotoxicology. Ecotoxicology
#' and Environmental Safety, 73(2), 123–131. doi: 10.1016/j.ecoenv.2009.09.012.
#'
#' @examples
#' library(bayesnec)
#' data(manec_example)
#' nec(manec_example)
#'
#' @export
nec <- function(object, posterior = FALSE, xform = identity,
                prob_vals = c(0.5, 0.025, 0.975), ...) {
  UseMethod("nec")
}


#' @inheritParams nec
#'
#' @param object An object of class \code{\link{bayesnecfit}} returned by
#' \code{\link{bnec}}.
#'
#' @inherit nec seealso return examples
#' 
#' @importFrom stats quantile
#' @importFrom chk chk_logical
#'
#' @noRd
#'
#' @export
nec.bayesnecfit <- function(object, posterior = FALSE, xform = identity,
                            prob_vals = c(0.5, 0.025, 0.975), ...) {
  check_component_arg(list(...), object)
  check_nec_no_dpar(list(...))
  chk_logical(posterior)
  if(!inherits(xform, "function")){ 
    stop("xform must be a function.")} 
  if (length(prob_vals) < 3 | prob_vals[1] < prob_vals[2] |
      prob_vals[1] > prob_vals[3] | prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order.")
  }
  if (length(grep("ecx", object$model)) > 0) {
    mod_class <- "ecx"
  } else {
    mod_class <- "nec"
  }
  if (mod_class == "ecx") {
    stop("nec is not a parameter in ecx model types.")
  }
  fitted_nec_out <- object$ne_posterior
  nec_out <- fitted_nec_out
  if (inherits(xform, "function")) {
    nec_out <- xform(nec_out)
  }
  # na.rm because the stored posterior can contain NA. A threshold equation's
  # b_nec_Intercept cannot, but a joint two-block fit whose survival block is
  # smooth has its combined no-effect estimate read off the curve by
  # nsec_off_curve(), which returns NA for any draw that never reaches the
  # reference. Without this such a fit's nec() is an error rather than a
  # censored estimate. See #39 and D15 ruling 3.
  warn_censored_draws(nec_out, ne_label(object))
  nec_estimate <- quantile(unlist(nec_out), probs = prob_vals, na.rm = TRUE)
  names(nec_estimate) <- clean_names(nec_estimate)
  fitted_estimate <- quantile(unlist(fitted_nec_out), probs = prob_vals,
                              na.rm = TRUE)
  report_nec_prior_bound(object, fitted_estimate, xform)
  attr(nec_estimate, "toxicity_estimate") <- "nec"
  attr(nec_out, "toxicity_estimate") <-  "nec"
  if (!posterior) {
    nec_estimate
  } else {
    nec_out
  }
}

#' @inheritParams nec
#'
#' @param object An object of class \code{\link{bayesmanecfit}} returned by
#' \code{\link{bnec}}.
#'
#' @inherit nec seealso return examples
#' 
#' @importFrom stats quantile
#' @importFrom chk chk_logical
#'
#' @noRd
#'
#' @export
nec.bayesmanecfit <- function(object, posterior = FALSE, xform = identity,
                              prob_vals = c(0.5, 0.025, 0.975), ...) {
  check_component_arg(list(...), object)
  check_nec_no_dpar(list(...))
  chk_logical(posterior)
  if (!inherits(xform, "function")) {
    stop("xform must be a function.")
  }
  if (length(prob_vals) < 3 | prob_vals[1] < prob_vals[2] |
      prob_vals[1] > prob_vals[3] | prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order.")
  }
  if (max(grepl("ecx", names(object$mod_fits))) == 1) {
    message("This bayesmanecfit contains smooth (ecx) models, which have no",
            " threshold parameter, so the returned estimate is a weighted",
            " mixture of NEC and NSEC draws -- the model-averaged N(S)EC",
            " rather than a NEC. See ?nec and summary(), which labels it.")
  }
  fitted_nec_out <- object$w_ne_posterior
  nec_out <- fitted_nec_out
  if (inherits(xform, "function")) {
    nec_out <- xform(nec_out)
  }
  # na.rm, as above. Here the NA arrive by a second route as well: every smooth
  # equation in the set contributes NSEC draws read off its own curve, so a set
  # containing one whose curve does not reach the reference has NA in the
  # weighted posterior even where every threshold equation in it is fine.
  warn_censored_draws(nec_out, ne_label(object))
  nec_estimate <- quantile(unlist(nec_out), probs = prob_vals, na.rm = TRUE)
  names(nec_estimate) <- clean_names(nec_estimate)
  fitted_estimate <- quantile(unlist(fitted_nec_out), probs = prob_vals,
                              na.rm = TRUE)
  report_nec_prior_bound(object, fitted_estimate, xform)
  attr(nec_estimate, "toxicity_estimate") <- "nec"
  attr(nec_out, "toxicity_estimate") <-  "nec"
  if (!posterior) {
    nec_estimate
  } else {
    nec_out
  }
}

#' Report a no-effect estimate constrained by its fitted prior
#'
#' Reads the bound from the prior the fit actually used rather than from the
#' data, so a user-supplied bound and an inline predictor transformation are
#' handled on the same scale as the posterior. Vectorised brms prior rows repeat
#' the same bound; \code{unique()} removes those copies.
#'
#' @param object A \code{bayesnecfit} or \code{bayesmanecfit}.
#' @param estimate The three-quantile result on the fitted predictor scale.
#' @param xform The transformation applied to the posterior and its bound.
#'
#' @return \code{NULL}, invisibly. Called for its message.
#'
#' @noRd
report_nec_prior_bound <- function(object, estimate, xform = identity) {
  fits <- if (inherits(object, "bayesmanecfit")) {
    object$mod_fits
  } else {
    list(object)
  }
  ne_types <- if (inherits(object, "bayesmanecfit")) {
    c(object$ne_type,
      vapply(object$mod_fits, function(fit) {
        if (is.null(fit$ne_type)) NA_character_ else fit$ne_type
      }, character(1)))
  } else {
    object$ne_type
  }
  # A mixed model average, including a joint fit with one smooth hurdle block,
  # combines sampled NEC parameters with NSEC values read from smooth curves.
  # Equality of that mixture with a bound belonging to one component does not
  # establish that the reported N(S)EC is constrained. ne_type is recorded
  # from the expanded fit and is authoritative for both cases.
  if (!length(ne_types) || anyNA(ne_types) || !all(ne_types == "NEC")) {
    return(invisible(NULL))
  }
  bounds <- unlist(lapply(fits, function(fit) {
    prior <- fit$fit$prior
    if (is.null(prior) || !all(c("nlpar", "ub") %in% names(prior))) {
      return(numeric(0))
    }
    upper <- suppressWarnings(as.numeric(prior$ub))
    use <- grepl("nec$", prior$nlpar) & is.finite(upper)
    unique(upper[use])
  }), use.names = FALSE)
  if (!length(bounds) || all(is.na(estimate))) {
    return(invisible(NULL))
  }
  bounds <- unique(bounds)
  bounds <- bounds[is.finite(bounds)]
  if (length(bounds) != 1L) {
    return(invisible(NULL))
  }
  central_at_bound <- any(signif(estimate[[1]], 3) == signif(bounds, 3))
  upper_at_bound <- any(signif(estimate[[3]], 3) == signif(bounds, 3))
  if (!central_at_bound && !upper_at_bound) {
    return(invisible(NULL))
  }
  statistic <- if (central_at_bound) "median" else "upper interval limit"
  index <- if (central_at_bound) 1 else 3
  bound <- bounds[which.min(abs(bounds - estimate[[index]]))]
  display_bound <- xform(bound)
  message(
    "The estimated ", statistic, " is at the upper bound of the fitted nec ",
    "prior (", signif(display_bound, 3), "). The comparison is made on the ",
    "fitted scale before xform. The estimate may be constrained by that prior; ",
    "report it as censored unless the bound and prior are scientifically ",
    "justified."
  )
  invisible(NULL)
}
