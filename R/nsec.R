#' Extracts the predicted NSEC value as desired from an 
#' object of class \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#' @param sig_val Probability value to use as the lower quantile to test
#' significance of the predicted posterior values.
#' @param resolution The number of unique x values over which to find NSEC.
#' The crossing is located by linear interpolation between the two grid values
#' that bracket it, so precision saturates well below the grid spacing.
#' The default is 500 grid values, matching \pkg{toxval}. A finer grid can
#' change an interpolated crossing and increases prediction time.
#' @param xform \code{NULL}, the default, or a function. \code{NULL} returns
#' the estimate on the scale the predictor was recorded on. A function is
#' applied to the estimate on the scale the model was fitted on. The two
#' differ only where \code{crf()} transforms the predictor inline; see
#' Details.
#' @param x_range A range of x values over which to consider extracting NSEC.
#' @param pool How observed group levels are combined. `"none"` returns
#' per-level estimates. `"mixture"` gives each posterior draw to one level, in
#' proportion to the levels' observation counts, and keeps it at its position,
#' so that draw \emph{i} of the mixture comes from posterior iteration
#' \emph{i}. Separate fits with different numbers of draws are instead each
#' sampled over their full posterior. `"mean_estimate"` averages the levels'
#' estimates at each draw, and `"mean_curve"` estimates from their weighted
#' mean curve. A joint fit defaults to `"none"`; other fits with spanning group
#' terms default to `"mixture"`.
#' @param x_control `"own"` reads each level's control recorded at fitting, or
#' its lowest observed concentration for an older fit or one fitted without
#' \code{x_control}. `"shared"` reads the lowest control across levels;
#' a single finite number specifies
#' the concentration directly. A control below a level's observations
#' extrapolates its curve, and gives a warning unless it is the control
#' recorded at fitting. Level controls are recorded in the result's
#' \code{x_control} attribute.
#' @param prob_vals A vector indicating the probability values over which to
#' return the estimated NSEC value. Defaults to 0.5 (median) and 0.025 and
#' 0.975 (95 percent credible intervals).
#' @param extrapolate The bound beyond which the estimate is reported as
#' censored. \code{FALSE}, the default, censors at the ends of the prediction
#' grid. A single number is an upper limit, and a pair of numbers is a lower
#' and an upper limit, in that order; the grid is extended to reach them.
#' \code{TRUE} is refused here, because every NSEC is read off a fitted curve.
#' See the \emph{Extrapolation} section of \code{\link{nsec}}.
#' @param dpar For a joint two-block fit only (\code{family = "hurdle_gamma"},
#' \code{"zero_inflated_beta"}, \code{"hurdle_poisson"} or
#' \code{"hurdle_negbinomial"}), the parameter block to report:
#' \code{"mu"} for the response block, or \code{"hu"} (\code{"zi"} for the
#' zero-inflated families) for survival. Defaults to \code{NULL}, which gives
#' the expected positive response multiplied by \code{1 - hu}. For continuous
#' hurdles the positive response is \code{mu}; for count hurdles it is
#' \code{E[Y | Y > 0]}. The zero-probability block is
#' inverted to survival before computing, so the NSEC is read off a declining
#' curve. See Details.
#' For the count hurdles, \code{"mu"} is converted to the positive-count mean
#' \code{E[Y | Y > 0]} so it matches the growth component returned by
#' \code{\link{bnec_hurdle}}.
#' @param ... Further arguments to pass to class specific methods.
#'
#' @details NSEC is no-effect toxicity metric that estimates the concentration 
#' at which the modeled mean response is statistically indistinguishable from 
#' the mean control response. See the detailed derivation in
#' Fisher and Fox (2023).
#'
#' With a fitted group term whose levels span concentrations, the reference
#' and curve are predicted for each observed level with its group deviations.
#' Terms whose levels each occur at one concentration are set to zero. The
#' level of a spanning term observed at only one concentration is excluded from
#' the estimate, with a message naming it. The default estimate mixes the
#' remaining observed levels by their observation counts;
#' \code{pool = "none"} returns them separately. A grouped or joint fit already
#' separates levels and defaults to \code{pool = "none"}.
#'
#' On a \code{\link{bayesnecjointfit}}, each level of the refit is mixed over
#' its own observed group levels. A group term whose values occur in the data
#' of more than one level, such as a batch in which two levels were both
#' tested, is read at the same value for every level at each posterior draw,
#' as often as their observation counts on that term allow. Terms they do not
#' share are chosen for each level independently. The levels can therefore be
#' contrasted draw by draw, as \code{\link{compare_posterior}} does.
#' 
#' The reference is the \code{sig_val} quantile of the control posterior,
#' the control being the predicted mean at the control concentration: the
#' \code{x_control} stated in \code{\link{bnec}}, or else the lowest
#' concentration in the supplied predictor. That holds for every equation,
#' hormetic ones included: the \code{hormesis_def} argument selected between
#' the control and the maximum of the predicted curve and has been removed,
#' because a target below the control is crossed exactly once whatever the
#' curve does above it.
#' For a hormesis threshold equation, \code{\link{nec}} uses this NSEC as its
#' no-effect estimate. Its \code{nec} parameter, the concentration at which
#' the decline term starts, remains available through
#' \code{\link{curve_params}}.
#'
#' The curve is searched on a grid of recorded concentrations. Where
#' \code{crf()} transforms the predictor inline, as in
#' \code{crf(log(concentration))}, the NSEC is therefore returned on the
#' recorded scale without \code{xform}, and no inverse is involved. A supplied
#' \code{xform} is applied to the NSEC on the transformed scale, so
#' \code{xform = identity} returns that scale and \code{xform = exp} on
#' \code{crf(log(concentration))} returns the recorded scale again.
#'
#' The attached \code{ecnsec} attribute is the percent effect at the NSEC,
#' defined as \code{\link{ecx}} defines it under \code{type = "absolute"}:
#' the decline from the control towards zero. Up to 2.1.3 it was measured
#' against the fitted range and computed by three different formulas that
#' agreed only for a monotonic curve.
#'
#' The reference is a quantile of the control posterior, so some draws start
#' at or below it. A draw with a later decreasing crossing takes that crossing.
#' Without one, it takes the control concentration unless its response first
#' changes away from the effect and later rises above the reference. Such a
#' draw shows no decreasing effect in the searched range and is censored there.
#' The crossing is sought at or above the control, so extending \code{x_range}
#' below the data does not place an estimate below any tested concentration.
#'
#' Two limits on that. The value is the control concentration, which is the
#' lowest \emph{observed} concentration unless \code{\link{bnec}} was given
#' \code{x_control}. Fisher and Fox (2023) report zero concentration, which
#' they reach by extrapolating the fitted curve below the data. The two agree
#' where the control is 0 and the predictor is untransformed; otherwise the
#' bound reported here is the control rather than the 0 of their Table 3. And
#' it applies where the prediction grid begins at the control. Where
#' \code{x_range} begins at a higher concentration, a draw already at or below
#' the reference at the first grid point reached it somewhere below the range
#' asked for, which is not identified within that range: such a draw returns
#' \code{NA} and is reported as lying at or below the start of the range.
#'
#' Where a draw's curve does not reach the reference at any tested concentration
#' its NSEC is above the highest concentration in the prediction grid. Such a
#' draw returns \code{NA}, keeps its rank in the summary and is given no value,
#' so the summary is censored above that concentration, as the \emph{Capping
#' and dropping} section describes, and a warning reports how many draws were
#' affected. Extending \code{x_range} will estimate it, at the price of reading
#' the curve where there are no data.
#' 
#' Calls to functions \code{\link{ecx}} and \code{\link{nsec}} and
#' \code{\link{compare_fitted}} do not require the same level of flexibility
#' in the context of allowing argument \code{newdata}
#' (from a \code{\link[brms]{posterior_predict}} perspective) to
#' be supplied manually, as this is and should be handled within the function
#' itself. The argument \code{resolution} controls how precisely the
#' \code{\link{ecx}} or \code{\link{nsec}} value is estimated, with 
#' argument \code{x_range} allowing estimation beyond the existing range of
#' the observed data (otherwise the default range) which can be useful in a
#' small number of cases. There is also no reasonable case where estimating
#' these from the raw data would be of value, because both functions would
#' simply return one of the treatment concentrations, making NOEC a better
#' metric in that case.
#'
#' \bold{Selecting a component of a hurdle model}
#'
#' The two implementations of a hurdle model name the component differently,
#' and the two arguments are not interchangeable. A
#' \code{\link{bayesnechurdlefit}} from \code{\link{bnec_hurdle}} holds two
#' separate fits, so it takes \code{which = "growth"}, \code{"survival"} or
#' \code{"combined"}. A joint fit from \code{bnec(family = "hurdle_gamma")}
#' holds two parameter blocks inside one model, so it takes \code{dpar} naming
#' the \pkg{brms} distributional parameter. Supplying one where the other is
#' expected is an error rather than silently ignored.
#'
#' @seealso \code{\link{bnec}}, \code{\link{bnec_hurdle}}, \code{\link{ecx}}
#'
#' @return A vector containing the estimated NSEC value, including upper and
#' lower 95% credible interval bounds.
#'
#' @section Capping and dropping:
#' A draw whose curve does not reach the target anywhere in the prediction
#' range has no such concentration in that range, and may have none at any
#' concentration. It is not capped at the top of the range, which would assert
#' that its estimate equals that value; it is recorded as lying beyond it. The
#' summary is then censored: the draw keeps its rank and is given no value, so
#' a reported quantile that falls among such draws is the end of the prediction
#' range rather than a quantile of the draws that did reach the target.
#' Attribute \code{"censored_summary"} marks which entries those are, and
#' states how many draws lie beyond each end and where the ends are. A draw
#' whose curve had already passed the target where the range begins is recorded
#' at the other end and reported the same way.
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
#' Up to version 2.1.3 such a draw was deleted and the remaining draws were
#' summarised as though nothing had been removed, which reported an estimate
#' lower than the quantity it was labelled as, with an interval narrower than
#' the posterior supports. See \code{\link{nec}} for the same treatment of a
#' threshold parameter, where the draw does have a value and the operation is
#' right-censoring in the ordinary sense.
#'
#' @section Extrapolation:
#' \code{extrapolate} chooses the bound the estimate is censored at, on the
#' same predictor scale as \code{x_range}, the data and the estimate returned
#' without \code{xform}, not the scale \code{crf()} fits on.
#'
#' \code{TRUE} asks for no bound at either end, and is an error here whatever
#' the fit is. An NSEC is the concentration at which a fitted curve reaches a
#' reference, so reading one means evaluating that curve on a grid, and no grid
#' reaches infinity. Name a finite limit instead.
#'
#' A finite limit extends the grid the curve is searched on and then censors
#' there, so \code{extrapolate} does through a checked argument what
#' \code{x_range} does through an unchecked one: a limit inside the current
#' range is an error rather than a silent tightening, because it would report
#' an estimate as censored at a value the fit had no trouble identifying. Use
#' \code{x_range} to narrow the range.
#'
#' A limit is measured against the range this call would otherwise use.
#' Where \code{x_range} is given, that is the range, at both ends: it is a
#' deliberate narrowing, and \code{extrapolate} extends from it rather than
#' discarding it. Where it is not, the range is the wider of the observed range
#' of the predictor and the prediction range the fit itself stores, which
#' differ for a fit built over a grid above the data; a limit between them
#' would report the estimate as censored inside the grid the fit was built on,
#' so both have to be cleared. \code{\link{nec}} measures against that stored
#' grid alone, having no \code{x_range} of its own, so the two agree on a fit
#' whose grid reaches beyond the data and can differ on one whose grid stops
#' short of it.
#'
#' A lower limit extends the grid but not the search. The reference is a
#' quantile of the control posterior, so the control is where the search
#' begins, and a draw already past the reference there has no identifiable
#' crossing at any lower concentration. Such a limit is accepted, and a message
#' states that the estimate stays censored at the control.
#'
#' @references
#' Fisher R, Fox DR (2023). Introducing the no significant effect concentration
#' (NSEC). Environmental Toxicology and Chemistry, 42(9), 2019–2028.
#' doi: 10.1002/etc.5610.
#'
#' @examples
#' \donttest{
#' library(bayesnec)
#'
#' data(manec_example)
#' nsec(manec_example)
#' }
#'
#' @export
# dpar sits after `...` to match the methods and appear in \usage.
nsec <- function(object, sig_val = 0.01, resolution = 500,
                 x_range = NA,
                 xform = NULL, prob_vals = c(0.5, 0.025, 0.975),
                 extrapolate = FALSE, ...,
                 dpar = NULL, pool = NULL, x_control = "own") {
  UseMethod("nsec")
}

#' @inheritParams nsec
#'
#' @param object An object of class \code{\link{bayesnecfit}} returned by
#' \code{\link{bnec}}.
#' @param posterior A \code{\link[base]{logical}} value indicating if the full
#' posterior sample of calculated NSEC values should be returned instead of
#' just the median and 95 credible intervals.
#'
#' @inherit nsec details seealso return examples
#' 
#' @importFrom stats quantile
#' @importFrom brms as_draws_df posterior_epred
#' @importFrom chk chk_logical chk_numeric
#' 
#' @noRd
#'
#' @export
nsec.bayesnecfit <- function(object, sig_val = 0.01, resolution = 500,
                             x_range = NA,
                             xform = NULL, prob_vals = c(0.5, 0.025, 0.975),
                             extrapolate = FALSE, ...,
                             posterior = FALSE, dpar = NULL, pool = NULL,
                             x_control = "own") {
  check_component_arg(list(...), object)
  check_removed_args(list(...))
  chk_numeric(sig_val)
  chk_numeric(resolution)
  chk_logical(posterior)
  if (length(sig_val)>1) {
    stop("You may only pass one sig_val")  
  }
  check_xform(xform)
  if (length(prob_vals) < 3 | prob_vals[1] < prob_vals[2] |
      prob_vals[1] > prob_vals[3] | prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order.")
  }
  if (is.null(object$observed_group)) {
    level_spec <- observed_level_spec(object)
    if (!is.null(level_spec)) {
      return(estimate_observed_levels(
        object, level_spec, "nsec",
        function(level, control_value, range_override) {
          nsec(level, sig_val = sig_val, resolution = resolution,
               x_range = range_override, xform = xform,
               prob_vals = prob_vals, extrapolate = extrapolate,
               posterior = TRUE, dpar = dpar, pool = "none",
               x_control = control_value)
        },
        pool, posterior, prob_vals, x_control, x_range,
        curve_args = list(sig_val = sig_val, resolution = resolution,
                          xform = xform, prob_vals = prob_vals,
                          dpar = dpar, extrapolate = extrapolate)
      ))
    }
  }
  # extrapolate becomes x_range, since an NSEC is read off the curve on the
  # grid. It adds two refusals a bare x_range lacks: of a limit that narrows
  # the range, and of an infinite limit.
  lims <- extrapolate_limits(extrapolate,
                             searched_or_stored_bounds(object, x_range),
                             "NSEC")
  if (!is.null(lims)) {
    report_curve_read_lower_limit(object, lims)
    x_range <- c(lims$lower, lims$upper)
  }
  control_reading <- resolve_x_control(object, x_control, x_range)
  newdata_list <- newdata_eval(
    object, resolution = resolution, x_range = control_reading$x_range
  )
  # dpar selects one block of a two-block fit, as in ecx(). NULL gives the
  # positive-part mean times (1 - hu) for a two-block family, and the mean
  # curve otherwise. The zero-probability block is inverted to survival first,
  # so that the NSEC is read off a declining curve.
  epred_fun <- function(nd) joint_hurdle_epred(
    object, nd, dpar,
    re_formula = if (is.null(object$re_formula)) NA else object$re_formula
  )
  p_samples <- epred_fun(newdata_list$newdata)
  x_vec <- newdata_list$x_vec
  # Read the control at the control concentration, not the first grid column,
  # so that x_range does not change the reference.
  control <- control_posterior(object, newdata_list$newdata, epred_fun,
                               x_at = control_reading$value)
  reference <- quantile(control, sig_val)
  # ecnsec is the percent effect at the NSEC, defined as ecx() defines it: the
  # decline from the control towards zero, not across the fitted range.
  ecnsecP <- as.numeric((control - reference) / control * 100)
  ecnsec <- quantile(ecnsecP, probs = prob_vals, na.rm = TRUE)
  # The control is the reference whether or not the curve shows hormesis.
  nsec_out <- nsec_from_posterior(p_samples, reference, x_vec,
                                  control_reading$value,
                                  control)
  # Draws above and below the range return NA for opposite reasons, so each
  # class has its own warning. Count them before the transformations below,
  # which need not keep the attributes.
  below <- attr(nsec_out, "below_range")
  n_below <- sum(below)
  searched_from <- attr(nsec_out, "x_searched_from")
  above <- is.na(nsec_out) & !below
  n_above <- sum(above)
  # Build the censoring record here, since x_range may differ from the fit's
  # grid. Its lower bound is where the search started, not min(x_vec): they
  # differ wherever the grid reaches below the control. It is built on the
  # recorded scale and remapped, which swaps the ends under a decreasing crf().
  cens <- censoring_record(max(x_vec), searched_from, above, below)
  # The default on an inline-transformed fit leaves both on the recorded scale.
  if (!returns_recorded(object, xform)) {
    to_fitted <- function(value) {
      sub_x_transformation(value, object$bayesnecformula)
    }
    nsec_out <- to_fitted(nsec_out)
    cens <- xform_censoring(cens, to_fitted)
    # Apply xform to the censoring bounds as well, so that the warnings report
    # them on the caller's scale.
    if (!is.null(xform)) {
      nsec_out <- xform(nsec_out)
      cens <- xform_censoring(cens, xform)
    }
  }
  # Take each end from the record's own flag, so that a decreasing crf() or
  # xform names the end each set of draws actually lies beyond.
  if (n_above > 0) {
    warning("The ", object$model, " curve does not fall below the control's ",
            sig_val, " quantile anywhere in the predictor range for ",
            n_above, " of ", length(nsec_out), " draws. The NSEC is censored ",
            "at ", signif(censored_end(cens, "above"), 3),
            ": those draws keep their rank in the summary and are given no ",
            "value.", call. = FALSE)
  }
  if (n_below > 0) {
    below_at <- signif(censored_end(cens, "below"), 3)
    warning("The ", object$model, " curve falls below the control's ", sig_val,
            " quantile before ", below_at, ", the lowest concentration in the ",
            "prediction range, for ", n_below, " of ", length(nsec_out),
            " draws. The NSEC is censored at ", below_at,
            ", which this x_range does not cover.", call. = FALSE)
  }
  # Drop the attributes nsec_from_posterior() left for the warnings, which the
  # transformation keeps. The censoring record replaces them.
  attr(nsec_out, "n_below_range") <- NULL
  attr(nsec_out, "below_range") <- NULL
  attr(nsec_out, "x_searched_from") <- NULL
  attr(nsec_out, "censored") <- cens
  nsec_estimate <- summarise_censored(unlist(nsec_out), prob_vals, cens)
  names(nsec_estimate) <- clean_names(nsec_estimate)
  attr(nsec_estimate, "resolution") <- resolution
  attr(nsec_out, "resolution") <- resolution
  attr(nsec_estimate, "sig_val") <- sig_val
  attr(nsec_out, "sig_val") <- sig_val
  attr(nsec_estimate, "toxicity_estimate") <- "nsec"
  attr(nsec_out, "toxicity_estimate") <-  "nsec"
  attr(nsec_estimate, "ecnsec_relativeP") <- ecnsec
  attr(nsec_out, "ecnsec_relativeP") <-  ecnsecP
  if (!posterior) {
    nsec_estimate
  } else {
    nsec_out
  }
}

#' @inheritParams nsec
#'
#' @param object An object of class \code{\link{bayesmanecfit}} returned by
#' \code{\link{bnec}}.
#' @param posterior A \code{\link[base]{logical}} value indicating if the full
#' posterior sample of calculated NSEC values should be returned instead of
#' just the median and 95 credible intervals.
#'
#' @inherit nsec details seealso return examples
#' 
#' @importFrom stats quantile
#'
#' @noRd
#'
#' @export
nsec.bayesmanecfit <- function(object, sig_val = 0.01, resolution = 500,
                               x_range = NA,
                               xform = NULL, prob_vals = c(0.5, 0.025, 0.975),
                               extrapolate = FALSE, ...,
                               posterior = FALSE, dpar = NULL, pool = NULL,
                               x_control = "own") {
  check_component_arg(list(...), object)
  check_removed_args(list(...))
  if (length(sig_val)>1) {
    stop("You may only pass one sig_val")
  }
  check_xform(xform)
  x_control <- model_set_control(object, x_control)
  first <- object$mod_fits[[1]]
  finish <- model_set_control_recorder(first, x_control)
  if (identical(pool, "none") &&
      !is.null(observed_level_spec(first))) {
    return(finish(model_set_level_estimates(
      object, "nsec",
      function(f) nsec(f, sig_val = sig_val, resolution = resolution,
                       x_range = x_range, xform = xform,
                       prob_vals = prob_vals, extrapolate = extrapolate,
                       posterior = TRUE, dpar = dpar, pool = "none",
                       x_control = x_control),
      posterior, prob_vals
    )))
  }
  # Resolve the limits once for the set, so that every component is searched
  # over one grid rather than against its own range.
  lims <- extrapolate_limits(extrapolate,
                             searched_or_stored_bounds(object, x_range),
                             "NSEC")
  if (!is.null(lims)) {
    report_curve_read_lower_limit(object, lims)
    x_range <- c(lims$lower, lims$upper)
  }
  sample_size <- object$sample_size
  # Use the weighted draw index the rest of the object uses, not a fresh
  # sample(): the estimate is then the same on every call, and each NSEC and
  # its ecnsec come from the same draw.
  draw_index <- pull_draw_index(object, names(object$mod_fits), sample_size)
  # A closure passes every argument by name. Positional dispatch through
  # sapply() would drop any argument it did not list, dpar included.
  sample_nsec <- function(x) {
    mod <- names(object$mod_fits)[x]
    target <- pull_equation(object, mod)
    out <- nsec(target, sig_val = sig_val, resolution = resolution,
                x_range = x_range, xform = xform, prob_vals = prob_vals,
                posterior = TRUE, dpar = dpar, pool = pool,
                x_control = x_control)
    idx <- draw_index[[mod]]
    sample_out <- out[idx]
    attr(sample_out, "ecnsec_relativeP") <-
      attributes(out)$ecnsec_relativeP[idx]
    # Subsetting drops the censoring record, so rebuild it for these draws.
    # Otherwise a fit and its one-equation average would report differently.
    attr(sample_out, "censored") <-
      subset_censoring(attr(out, "censored"), idx)
    sample_out
  }
  to_iter <- unname(which(lengths(draw_index[names(object$mod_fits)]) > 0L))
  nsec_out <- once_level_note(lapply(to_iter, sample_nsec))
  ecnsecP <- unlist(lapply(nsec_out, 
                    FUN = function(p){attributes(p)$ecnsec_relativeP}))
  ecnsec <- quantile(ecnsecP, probs = prob_vals, na.rm = TRUE)
  cens <- concat_censoring(lapply(nsec_out, attr, "censored"),
                           vapply(nsec_out, length, integer(1)),
                           unlist(lapply(nsec_out, as.numeric)))
  nsec_out <- unlist(lapply(nsec_out, as.numeric))
  attr(nsec_out, "censored") <- cens
  nsec_estimate <- summarise_censored(nsec_out, prob_vals, cens)
  names(nsec_estimate) <- clean_names(nsec_estimate)
  attr(nsec_estimate, "resolution") <- resolution
  attr(nsec_out, "resolution") <- resolution
  attr(nsec_estimate, "sig_val") <- sig_val
  attr(nsec_out, "sig_val") <- sig_val
  attr(nsec_estimate, "toxicity_estimate") <- "nsec"
  attr(nsec_out, "toxicity_estimate") <-  "nsec"
  attr(nsec_estimate, "ecnsec_relativeP") <- ecnsec
  attr(nsec_out, "ecnsec_relativeP") <-  ecnsecP
  if (!posterior) {
    finish(nsec_estimate)
  } else {
    finish(nsec_out)
  }
}
