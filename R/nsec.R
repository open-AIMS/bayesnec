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
#' change an interpolated crossing and increases prediction time. With an
#' inline predictor transformation the grid is even on the fitted scale.
#' @param xform \code{NULL} or \code{identity} leaves the estimate on
#' \code{x_scale}. Any other function is applied on the fitted scale and
#' requires \code{x_scale = "recorded"} when the predictor is
#' transformed inline; see Details.
#' @param x_scale \code{"recorded"} (the default) returns the predictor on
#' its recorded scale. \code{"fitted"} returns it on the scale fitted by
#' \code{crf()}. The scales differ only where \code{crf()} transforms the
#' predictor inline.
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
#' and an upper limit, in that order; points are appended while the original
#' grid points and their spacing are retained.
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
#' @param ... Further arguments to pass to class specific methods. A method
#' refuses any argument it does not use.
#' @param posterior A \code{\link[base]{logical}} value indicating if the full
#' posterior sample of calculated NSEC values should be returned instead of
#' just the median and 95 credible intervals.
#'
#' @details NSEC is no-effect toxicity metric that estimates the concentration 
#' at which the modelled mean response is statistically indistinguishable from
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
#' hormetic ones included, because a target below the control is crossed
#' exactly once whatever the curve does above it.
#' For a hormesis threshold equation, \code{\link{nec}} uses this NSEC as its
#' no-effect estimate. Its \code{nec} parameter, the concentration at which
#' the decline term starts, remains available through
#' \code{\link{curve_params}}.
#'
#' Where \code{crf()} transforms the predictor inline, as in
#' \code{crf(log(concentration))}, the curve is searched on a grid even on
#' the fitted scale. The crossing is interpolated there and returned on the
#' recorded scale through the inverse. \code{x_scale = "fitted"} returns the
#' fitted-scale crossing. A supplied \code{xform} is applied on that scale;
#' \code{xform = exp} on \code{crf(log(concentration))} returns the recorded
#' concentration.
#'
#' The attached \code{ecnsec} attribute is the percent effect at the NSEC,
#' defined as \code{\link{ecx}} defines it under \code{type = "absolute"}:
#' the decline from the control towards zero. It is the same quantity returned
#' by \code{\link{ecnsec}} for the same options.
#'
#' The reference is a quantile of the control posterior, so some draws start
#' at or below it. Such a draw takes the control concentration, where its
#' effect is zero. Other draws take the first decreasing crossing.
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
#' \code{-Inf} with \code{posterior = TRUE} and is reported as lying at or
#' below the start of the range.
#'
#' Where a draw's curve does not reach the reference at any tested concentration
#' its NSEC is above the highest concentration in the prediction grid. Such a
#' draw returns \code{Inf} with \code{posterior = TRUE}, keeps its rank
#' in the summary and is given no finite value,
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
#' lower 95% credible interval bounds, or with \code{posterior = TRUE} the
#' value for each posterior draw. Censored draws are \code{Inf} above the
#' search range and \code{-Inf} below it; attribute \code{"censored"}
#' records their bounds and positions.
#'
#' @section Capping and dropping:
#' A draw whose curve does not reach the target anywhere in the prediction
#' range has no such concentration in that range, and may have none at any
#' concentration. It is not capped at the top of the range, which would assert
#' that its estimate equals that value; it is recorded as lying beyond it. The
#' summary is then censored: the draw keeps its rank and is given no value, so
#' a reported quantile that falls among such draws is the end of the prediction
#' range rather than a quantile of the draws that did reach the target.
#' With \code{posterior = TRUE}, censored draws are returned as \code{Inf}
#' or \code{-Inf}, so \code{sort()} and \code{quantile()} retain their
#' positions. Attribute \code{"censored"} records their bounds and ends.
#' Attribute \code{"censored_summary"} marks which entries those are, and
#' states how many draws lie beyond each end and where the ends are. A draw
#' whose curve had already passed the target where the range begins is recorded
#' at the other end and reported the same way.
#'
#' Which quantile estimator is used depends on whether anything is censored.
#' A posterior with no beyond-range draw is summarised with
#' \code{\link[stats]{quantile}}'s default type 7, which interpolates between
#' two adjacent order statistics. Once any draw is censored every reported
#' entry becomes an order statistic instead, because a value interpolated
#' across a draw that has no value would be one the posterior does not
#' support. Every entry therefore changes a little when the first draw is
#' censored, including entries at the other end of the interval. See
#' \code{\link{nec}} for the same treatment of a threshold parameter, where
#' the draw does have a value and the operation is right-censoring in the
#' ordinary sense.
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
#' A finite limit appends points to the grid the curve is searched on, at the
#' grid's own spacing, and then censors there. The points of the unextended
#' grid are kept, so an estimate inside the data is read off the same points
#' as without \code{extrapolate}. Widening \code{x_range} to the same limit
#' instead spreads \code{resolution} points over the wider range, so its grid
#' is coarser and its estimates can differ slightly. A limit inside the current
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
#' short of it. For each observed level of a group term the range is that
#' level's own, from its control to its highest concentration.
#'
#' A lower limit below the control does not extend the search. The reference
#' is a quantile of the control posterior, so the control is where the search
#' begins, and a draw already past the reference there has no identifiable
#' crossing at any lower concentration. Such a limit is accepted, and a message
#' states that the search begins at the control.
#'
#' @references
#' Fisher R, Fox DR (2023). Introducing the no significant effect concentration
#' (NSEC). Environmental Toxicology and Chemistry, 42(9), 2019--2028.
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
# posterior and dpar follow `...` to match the methods and appear in \usage.
nsec <- function(object, sig_val = 0.01, resolution = 500,
                 x_range = NA,
                 xform = NULL,
                 x_scale = c("recorded", "fitted"), prob_vals = c(0.5, 0.025, 0.975),
                 extrapolate = FALSE, ...,
                 posterior = FALSE, dpar = NULL, pool = NULL,
                 x_control = "own") {
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
#' @importFrom chk chk_flag
#' 
#' @noRd
#'
#' @export
nsec.bayesnecfit <- function(object, sig_val = 0.01, resolution = 500,
                             x_range = NA,
                             xform = NULL,
                             x_scale = c("recorded", "fitted"),
                             prob_vals = c(0.5, 0.025, 0.975),
                             extrapolate = FALSE, ...,
                             posterior = FALSE, dpar = NULL, pool = NULL,
                             x_control = "own") {
  check_component_arg(list(...), object)
  check_removed_args(list(...))
  check_nsec_no_type(list(...))
  check_unused_dots(list(...), "nsec")
  check_sig_val(sig_val)
  check_resolution(resolution)
  chk_flag(posterior)
  xform <- resolve_estimator_xform(object, xform, x_scale)
  if (length(prob_vals) < 3 | prob_vals[1] < prob_vals[2] |
      prob_vals[1] > prob_vals[3] | prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order.")
  }
  if (!is.null(pool)) validate_pool(pool, "nsec")
  if (is.null(object$observed_group)) {
    level_spec <- observed_level_spec(object)
    if (!is.null(level_spec)) {
      return(estimate_observed_levels(
        object, level_spec, "nsec",
        function(level, control_value, range_override) {
          nsec(level, sig_val = sig_val, resolution = resolution,
               x_range = range_override, xform = xform, x_scale = x_scale,
               prob_vals = prob_vals, extrapolate = extrapolate,
               posterior = TRUE, dpar = dpar, pool = "none",
               x_control = control_value)
        },
        pool, posterior, prob_vals, x_control, x_range,
        curve_args = list(sig_val = sig_val, resolution = resolution,
                          xform = xform, x_scale = x_scale, prob_vals = prob_vals,
                          dpar = dpar, extrapolate = extrapolate)
      ))
    }
  }
  lims <- curve_extrapolate_limits(extrapolate, object, x_range, x_control,
                                   "NSEC")
  control_reading <- resolve_x_control(object, x_control, x_range)
  report_curve_read_lower_limit(extrapolate, lims, control_reading$value,
                                curve_start_label(object, x_control))
  newdata_list <- extended_newdata_eval(
    object, resolution = resolution, x_range = control_reading$x_range,
    lims = lims
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
  axis <- if (!is.null(inline_x_transform(object))) {
    inline_curve_axis(object, x_vec, xform)
  }
  if (!is.null(axis)) x_vec <- axis$x
  # Read the control at the control concentration, not the first grid column,
  # so that x_range does not change the reference.
  control <- control_posterior(object, newdata_list$newdata, epred_fun,
                               x_at = control_reading$value)
  reference <- quantile(control, sig_val)
  # ecnsec is the percent effect at the NSEC, defined as ecx() defines it: the
  # decline from the control towards zero, not across the fitted range.
  ecnsecP <- as.numeric(pmax(control - reference, 0) / control * 100)
  ecnsecP[!is.na(control) & control <= reference] <- 0
  ecnsec <- quantile(ecnsecP, probs = prob_vals, na.rm = TRUE)
  # The control is the reference whether or not the curve shows hormesis.
  control_axis <- if (is.null(axis)) control_reading$value else
    axis$at(control_reading$value)
  nsec_out <- nsec_from_posterior(p_samples, reference, x_vec,
                                  control_axis,
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
  if (!is.null(axis)) {
    at_control <- !is.na(nsec_out) & nsec_out == control_axis
    nsec_out <- map_draws(nsec_out, axis$map, cens, "NSEC")
    if (is.null(xform)) nsec_out[at_control] <- control_reading$value
    cens <- xform_censoring(cens, axis$map)
  } else if (!returns_recorded(object, xform)) {
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
    censored_infinite(nsec_out)
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
                               xform = NULL,
                               x_scale = c("recorded", "fitted"),
                               prob_vals = c(0.5, 0.025, 0.975),
                               extrapolate = FALSE, ...,
                               posterior = FALSE, dpar = NULL, pool = NULL,
                               x_control = "own") {
  check_component_arg(list(...), object)
  check_removed_args(list(...))
  check_nsec_no_type(list(...))
  check_unused_dots(list(...), "nsec")
  check_sig_val(sig_val)
  check_resolution(resolution)
  chk_flag(posterior)
  xform <- resolve_estimator_xform(object, xform, x_scale)
  if (!is.null(pool)) validate_pool(pool, "nsec")
  x_control <- model_set_control(object, x_control)
  first <- object$mod_fits[[1]]
  finish <- model_set_control_recorder(first, x_control)
  if (identical(pool, "none") &&
      !is.null(observed_level_spec(first))) {
    return(finish(model_set_level_estimates(
      object, "nsec",
      function(f) nsec(f, sig_val = sig_val, resolution = resolution,
                       x_range = x_range, xform = xform, x_scale = x_scale,
                       prob_vals = prob_vals, extrapolate = extrapolate,
                       posterior = TRUE, dpar = dpar, pool = "none",
                       x_control = x_control),
      posterior, prob_vals
    )))
  }
  # Refuse a bad limit once for the set. Each equation reads the limit as
  # given, and once_level_note() below prints its lower-limit message once.
  curve_extrapolate_limits(extrapolate, object, x_range, x_control, "NSEC")
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
                x_range = x_range, xform = xform, x_scale = x_scale, prob_vals = prob_vals,
                extrapolate = extrapolate,
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
    finish(censored_infinite(nsec_out))
  }
}
