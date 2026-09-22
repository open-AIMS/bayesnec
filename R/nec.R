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
#' @param extrapolate The bound beyond which the estimate is reported as
#' censored. \code{FALSE}, the default, censors at the prediction range the fit
#' was built on. \code{TRUE} removes the bound at both ends. A single number is
#' an upper limit, and a pair of numbers is a lower and an upper limit, in that
#' order. See the \emph{Extrapolation} section of \code{\link{nec}}.
#' @param ... Additional arguments passed to methods. \code{sig_val} and
#' \code{resolution} are forwarded to \code{\link{nsec}} where a finite
#' \code{extrapolate} limit requires a curve to be re-evaluated, and are
#' otherwise unused.
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
#' prob_vals). Where any posterior draw lies beyond the range the model was
#' predicted over, that draw keeps its rank in the summary and is given no
#' value, so an entry falling among such draws is the end of the prediction
#' range rather than a quantile. Attribute \code{"censored_summary"} marks
#' which entries those are and states how many draws lie beyond each end.
#'
#' @section Capping and dropping:
#' Two operations are both called censoring and they are not the same. A
#' threshold equation's \code{nec} is a sampled parameter: a draw above the top
#' of the prediction range has a value, and that value is known to exceed the
#' bound, which is right-censoring in the ordinary sense. An NSEC or an ECx is
#' read off a fitted curve: a draw whose curve never reaches the target has no
#' such concentration in the range and may have none at any concentration, so
#' the draw is not capped at the bound, it is recorded as lying beyond it. Both
#' are reported the same way here, because neither claims a value for the draw.
#'
#' Which quantile estimator is used depends on whether anything is censored.
#' A posterior with no beyond-range draw is summarised exactly as it was up to
#' version 2.1.3, with \code{\link[stats]{quantile}}'s default type 7, which
#' interpolates between two adjacent order statistics. Once any draw is
#' censored every reported entry becomes an order statistic instead, because a
#' value interpolated across a draw that has no value would be one the
#' posterior does not support.
#' Every entry therefore changes a little when the first draw is censored,
#' including entries at the other end of the interval. Keeping the uncensored
#' summary bit-identical to the release was preferred to making the two agree
#' at the boundary, because every archived analysis is compared against it.
#'
#' @section Extrapolation:
#' \code{extrapolate} chooses the bound the estimate is censored at. It is read
#' on the recorded predictor scale, the scale the data were supplied on and the
#' scale \code{\link{bnec}} and \code{\link{nsec}} take \code{x_range} on. That
#' is neither the scale \code{crf()} fits on nor the scale \code{xform}
#' displays, so under \code{crf(log(concentration))} the limit is a
#' concentration and not a log concentration.
#'
#' \code{TRUE} removes the bound at both ends. It is accepted only where every
#' component of the reported estimate samples a NEC. An NSEC is read off a
#' fitted curve and a curve cannot be evaluated on an infinite grid, so where
#' any component is an NSEC --- which includes the default \code{\link{bnec}}
#' model set, because that set fits equations of both classes --- \code{TRUE}
#' is an error naming the finite form. For a single fit the class is the one
#' recorded in \code{ne_type}, which takes both blocks of a joint two-block
#' fit into account. For a model-averaged set it is read from each equation's
#' name, which is where \code{\link{summary}} reads the label it prints, so
#' the two cannot disagree about what the set is.
#'
#' A finite limit applies to both classes, by a different route for each. The
#' threshold components are compared against it, which requires no further
#' computation because every draw of a sampled \code{nec} already has a value.
#' The curve-read components are re-evaluated on a grid extended to it, through
#' the \code{x_range} argument of \code{\link{nsec}}. That recomputes from the
#' stored fit and needs no refit, but it does evaluate the curve once for each
#' such equation, at a run time proportional to \code{resolution}. The
#' re-evaluation reuses the number of grid points the fit stored unless
#' \code{resolution} is given, so over a wider range the grid is proportionally
#' coarser, and it reads its control at the lowest observed concentration, so
#' its numbers can differ in the last digits from the stored summary.
#'
#' Two requests re-evaluate nothing and return what \code{extrapolate = FALSE}
#' returns: a limit equal to the current bounds, and any limit on a fit with no
#' draw beyond either end, where every draw was identified inside the range the
#' fit used and a wider grid has none left to identify.
#'
#' A limit inside the prediction range is an error rather than a silent
#' tightening, because it would report an estimate as censored at a value the
#' fit had no trouble identifying. Narrow the range with \code{x_range} where
#' the fit or the estimate is built instead.
#'
#' Two constraints on the reach of an extrapolation are reported rather than
#' corrected. Where the \code{nec} prior stored on the fit is itself bounded at
#' the prediction range, the posterior holds no draw beyond that bound and a
#' wider limit returns the same truncated posterior; a message states the
#' bound. And a curve-read component is measured from the control, so a lower
#' limit below the lowest observed concentration extends the grid without
#' extending the search, and those components stay censored at the control.
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
                prob_vals = c(0.5, 0.025, 0.975), extrapolate = FALSE, ...) {
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
                            prob_vals = c(0.5, 0.025, 0.975),
                            extrapolate = FALSE, ...) {
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
  nec_out <- object$ne_posterior
  # extrapolate names a bound on top of that record rather than replacing it:
  # the default returns the stored posterior and the stored record untouched,
  # so every existing call is unaffected. The bounds are read off the
  # prediction grid the fit stored, which is the value the record censors at
  # and the value ecx() censors at, so the whole package reports against one
  # number.
  # ne_grid_bounds() is left as a promise rather than assigned: under the
  # default, extrapolate_limits() returns before forcing it, so a fit that
  # stores no prediction grid raises nothing on the path every existing call
  # takes. It is read again below, where the limits say it is needed.
  lims <- extrapolate_limits(extrapolate, ne_grid_bounds(object),
                             necfit_ne_type(object), object$model)
  if (!is.null(lims)) {
    dots <- list(...)
    nec_out <- extrapolated_necfit_ne(
      object, lims, ne_grid_bounds(object),
      sig_val = if (is.null(dots$sig_val)) 0.01 else dots$sig_val,
      resolution = if (is.null(dots$resolution)) {
        stored_resolution(object)
      } else {
        dots$resolution
      }
    )
  }
  # Read before xform, which for a general function need not preserve an
  # attribute. It says whether the record below is the prediction range or a
  # limit the caller named, which is what the report has to state.
  range_label <- censoring_range_label(nec_out)
  # The record expand_nec() wrote when the posterior was realised, read rather
  # than derived. This is the whole point of #395: summary() reports object$ne,
  # which is the summary of exactly this vector, so a censoring report invented
  # here would not be the one a user reads.
  cens <- attr(nec_out, "censored")
  if (inherits(xform, "function")) {
    nec_out <- xform(nec_out)
    cens <- xform_censoring(cens, xform)
    attr(nec_out, "censored") <- cens
  }
  # The stored posterior can hold a draw that is beyond the prediction range. A
  # threshold equation's b_nec_Intercept is such a draw wherever the prior no
  # longer holds it inside; a joint two-block fit whose survival block is smooth
  # has its combined no-effect estimate read off the curve by nsec_off_curve(),
  # which returns NA for any draw that never reaches the reference. Either way
  # the draw keeps its rank and is given no value. See #39 and D15 ruling 3 for
  # why it is not given max(x_vec) instead.
  warn_censored_draws(nec_out, ne_label(object), cens = cens,
                      range_label = range_label)
  nec_estimate <- summarise_censored(unlist(nec_out), prob_vals, cens)
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
                              prob_vals = c(0.5, 0.025, 0.975),
                              extrapolate = FALSE, ...) {
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
  nec_out <- object$w_ne_posterior
  # Validated against every equation in the set, not against the set's own
  # label: an infinite limit turns on whether each component samples a NEC,
  # and the label of a mixed set says only that at least one does not.
  # As in nec.bayesnecfit(), left as a promise so that the default path reads
  # nothing off the object.
  lims <- extrapolate_limits(extrapolate, ne_grid_bounds(object),
                             manec_ne_types(object), object$success_models)
  if (!is.null(lims)) {
    dots <- list(...)
    nec_out <- extrapolated_manec_ne(
      object, lims, ne_grid_bounds(object),
      sig_val = if (is.null(dots$sig_val)) 0.01 else dots$sig_val,
      resolution = if (is.null(dots$resolution)) {
        stored_resolution(object)
      } else {
        dots$resolution
      }
    )
  }
  range_label <- censoring_range_label(nec_out)
  # As above, and the record expand_manec() wrote is the weighted one: it was
  # assembled under the draw index that built the mixture, so its fraction is
  # the share of the model-averaged posterior that is censored rather than the
  # share of equations that are.
  cens <- attr(nec_out, "censored")
  if (inherits(xform, "function")) {
    nec_out <- xform(nec_out)
    cens <- xform_censoring(cens, xform)
    attr(nec_out, "censored") <- cens
  }
  # Censoring arrives by a second route here: every smooth equation in the set
  # contributes NSEC draws read off its own curve, so a set containing one whose
  # curve does not reach the reference is censored even where every threshold
  # equation in it is inside the range.
  warn_censored_draws(nec_out, ne_label(object), cens = cens,
                      range_label = range_label)
  nec_estimate <- summarise_censored(unlist(nec_out), prob_vals, cens)
  names(nec_estimate) <- clean_names(nec_estimate)
  attr(nec_estimate, "toxicity_estimate") <- "nec"
  attr(nec_out, "toxicity_estimate") <-  "nec"
  if (!posterior) {
    nec_estimate
  } else {
    nec_out
  }
}
