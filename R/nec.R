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
  nec_out <- object$ne_posterior
  if (inherits(xform, "function")) {
    nec_out <- xform(nec_out)
  }
  nec_estimate <- quantile(unlist(nec_out), probs = prob_vals)
  names(nec_estimate) <- clean_names(nec_estimate)
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
  nec_out <- object$w_ne_posterior
  if (inherits(xform, "function")) {
    nec_out <- xform(nec_out)
  }
  nec_estimate <- quantile(unlist(nec_out), probs = prob_vals)
  names(nec_estimate) <- clean_names(nec_estimate)
  attr(nec_estimate, "toxicity_estimate") <- "nec"
  attr(nec_out, "toxicity_estimate") <-  "nec"
  if (!posterior) {
    nec_estimate
  } else {
    nec_out
  }
}
