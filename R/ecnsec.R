#' Extracts the percent effect associated with a predicted NSEC value from an
#' object of class \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#' @param nsec A single number, the concentration at which to read the
#' percentage effect. A vector is refused, including the three values
#' \code{\link{nsec}} returns; pass the one wanted.
#' @param resolution The number of unique x values over which the curve is
#' predicted. It affects only \code{type = "range"}, where the denominator is
#' the lowest response the curve predicts over the grid. The default of 500
#' matches \code{\link{ecx}} and \code{\link{nsec}}.
#' @param type A \code{\link[base]{character}} vector, taking values of
#' "absolute" (the default), "relative" or "range". See Details.
#' @param xform \code{NULL}, the default, or a function applied to
#' \code{nsec} before the curve is read there. \code{NULL} reads \code{nsec}
#' as given, on the recorded scale. See Details.
#' @param x_range A range of x values over which to predict the curve. An
#' \code{nsec} outside it is refused; see Details.
#' @param extrapolate \code{FALSE}, the default, uses the prediction range.
#' A single finite number extends the upper end; two finite numbers may
#' extend both ends. The original grid points are retained. Limits are on
#' the recorded predictor scale. \code{TRUE} and narrowing limits are refused.
#' @param prob_vals A vector indicating the probability values over which to
#' return the estimated ECNSEC value. Defaults to 0.5 (median) and 0.025 and
#' 0.975 (95 percent credible intervals).
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
#' the concentration directly. A shared control below a level's observations
#' extrapolates its curve and gives a warning. Level controls are recorded in
#' the result's \code{x_control} attribute.
#' @param ... Further arguments to pass to class specific methods.
#'
#' @details NSEC is no-effect toxicity metric that estimates the concentration
#' at which the modeled mean response is statistically indistinguishable from
#' the mean control response. See the detailed derivation in
#' Fisher and Fox (2023). Like NOEC, NSEC estimates will have an associated
#' effect size. This function estimates the effect of a given NSEC estimate.
#'
#' With a fitted group term whose levels span concentrations, the curve and
#' control are predicted for each observed level with its group deviations.
#' Terms whose levels each occur at one concentration are set to zero. The
#' level of a spanning term observed at only one concentration is excluded from
#' the estimate, with a message naming it. The default mixes the remaining
#' observed levels by their observation counts;
#' \code{pool = "none"} returns them separately.
#'
#' On a \code{\link{bayesnecjointfit}}, each level of the refit is mixed over
#' its own observed group levels. A group term whose values occur in the data
#' of more than one level, such as a batch in which two levels were both
#' tested, is read at the same value for every level at each posterior draw,
#' as often as their observation counts on that term allow. Terms they do not
#' share are chosen for each level independently. The levels can therefore be
#' contrasted draw by draw, as \code{\link{compare_posterior}} does.
#'
#' The effect is measured from the control --- the predicted mean at the
#' lowest concentration in the supplied predictor, per posterior draw --- and
#' \code{type} names what it is measured towards, exactly as in
#' \code{\link{ecx}}: "absolute" (the default) towards 0, "relative" towards
#' the equation's theoretical asymptote, "range" towards the lowest response
#' the curve predicts. \code{ecnsec} is the inverse of the \code{ecx}
#' reference construction under the same \code{type}, so the two answer the
#' same question of the same curve. "direct" names a response value rather
#' than a percentage and is refused here. On a joint two-block fit
#' (\code{family = "hurdle_gamma"}, \code{"zero_inflated_beta"},
#' \code{"hurdle_poisson"} or \code{"hurdle_negbinomial"}) the curve read is
#' the combined one, \code{(1 - hu) * E[Y | Y > 0]}, and "relative" is refused,
#' because that curve's asymptote is the product of the two blocks' asymptotes
#' rather than a fitted parameter.
#'
#' Each posterior draw of the control is compared with the same draw's value
#' of the curve at \code{nsec}, as \code{\link{ecx}} reads each draw's own
#' curve. The interval therefore includes the uncertainty of the curve at
#' \code{nsec} as well as that of the control. On \code{nec4param}, for
#' example, every draw returns 0 at an \code{nsec} below the \code{nec} of
#' every draw, where every draw of the curve still equals its control.
#'
#' The \code{hormesis_def} argument has been removed; the control is now
#' always the reference. See \code{\link{ecx}}.
#'
#' \code{nsec} is read on the recorded predictor scale once \code{xform} has
#' been applied to it. \code{\link{nsec}} returns its estimate on that scale
#' by default, so \code{ecnsec(fit, nsec = nsec(fit)[1])} reads the curve at
#' the NSEC whether or not \code{crf()} transforms the predictor inline. Where
#' it does, as in \code{crf(log(concentration))}, an NSEC taken with
#' \code{xform = identity} is on the transformed scale, and needs the inverse,
#' here \code{xform = exp}.
#'
#' The curve is read at \code{nsec} only inside the range it is predicted
#' over: the observed range of the predictor, or \code{x_range} where one is
#' supplied. An \code{nsec} outside that range once \code{xform} has been
#' applied is refused with a message naming the range, rather than read off
#' the curve extended past the data without a message. Supply an
#' \code{x_range} or finite \code{extrapolate} that includes \code{nsec} to
#' read the curve there. On a
#' \code{\link{bayesnecjointfit}} each level is read over its own observed
#' range, and the message names the level that refused \code{nsec}.
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
ecnsec <- function(object, nsec, resolution = 500, x_range = NA,
                   type = "absolute", xform = NULL,
                   prob_vals = c(0.5, 0.025, 0.975), ...,
                   pool = NULL, x_control = "own", extrapolate = FALSE) {
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
#' @importFrom chk chk_logical chk_number
#'
#' @noRd
#'
#' @export
ecnsec.bnecfit <- function(object, nsec, resolution = 500, x_range = NA,
                             type = "absolute",
                             xform = NULL, prob_vals = c(0.5, 0.025, 0.975), ...,
                             posterior = FALSE, pool = NULL,
                             x_control = "own", extrapolate = FALSE) {
  # One value: the curve is read at one point.
  chk_number(nsec)
  chk_logical(posterior)
  check_removed_args(list(...))

  type <- validate_ecx_type(type, match.call())
  if (identical(type, "direct")) {
    stop("type = \"direct\" names a response value rather than a ",
         "percentage, so there is no percent effect for ecnsec to report. ",
         "Use type = \"absolute\" (the default), \"relative\" or ",
         "\"range\".", call. = FALSE)
  }
  if (identical(type, "relative")) {
    fit_family <- if (inherits(object, "bayesmanecfit")) {
      object$mod_fits[[1]]$fit$family
    } else {
      object$fit$family
    }
    check_combined_relative(fit_family, "ecnsec")
  }
  check_xform(xform)
  if (is.null(xform)) {
    xform <- identity
  }
  if (length(prob_vals) < 3 | prob_vals[1] < prob_vals[2] |
      prob_vals[1] > prob_vals[3] | prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order.")
  }
  if (inherits(object, "bayesmanecfit")) {
    x_control <- model_set_control(object, x_control)
    first <- object$mod_fits[[1]]
    finish <- model_set_control_recorder(first, x_control)
    if (!is.null(observed_level_spec(first))) {
      per_model <- function(f) ecnsec(
        f, nsec = nsec, resolution = resolution, x_range = x_range,
        extrapolate = extrapolate,
        type = type, xform = xform, prob_vals = prob_vals,
        posterior = TRUE, pool = pool, x_control = x_control
      )
      if (identical(pool, "none")) {
        return(finish(model_set_level_estimates(
          object, "ecnsec", per_model, posterior, prob_vals
        )))
      }
      models <- names(object$mod_fits)
      idx <- pull_draw_index(object, models, object$sample_size)
      models <- models[lengths(idx[models]) > 0L]
      parts <- once_level_note(lapply(models, function(m) {
        per_model(pull_equation(object, m))[idx[[m]]]
      }))
      out <- unlist(parts, use.names = FALSE)
      if (posterior) return(finish(out))
      result <- stats::quantile(out, probs = prob_vals, na.rm = TRUE)
      names(result) <- clean_names(result)
      return(finish(result))
    }
  }
  if (!inherits(object, "bayesmanecfit") &&
      is.null(object$observed_group)) {
    level_spec <- observed_level_spec(object)
    if (!is.null(level_spec)) {
      return(estimate_observed_levels(
        object, level_spec, "ecnsec",
        function(level, control_value, range_override) {
          ecnsec(level, nsec = nsec, resolution = resolution,
                 x_range = range_override, extrapolate = extrapolate,
                 type = type, xform = xform,
                 prob_vals = prob_vals, posterior = TRUE, pool = "none",
                 x_control = control_value)
        },
        pool, posterior, prob_vals, x_control, x_range,
        curve_args = list(nsec = nsec, resolution = resolution,
                          extrapolate = extrapolate,
                          type = type, xform = xform,
                          prob_vals = prob_vals)
      ))
    }
  }
  nsec_use <- xform(nsec)

  lims <- curve_extrapolate_limits(extrapolate, object, x_range, x_control,
                                   "ECNSEC")
  control_reading <- resolve_x_control(object, x_control, x_range)
  newdata_list <- extended_newdata_eval(
    object, resolution = resolution, x_range = control_reading$x_range,
    lims = lims
  )
  # Refuse an nsec outside the chosen grid. Otherwise the curve is evaluated
  # past that grid without reporting the extrapolation.
  check_ecnsec_range(object, nsec, nsec_use, range(newdata_list$x_vec),
                     x_range, xform)

  p_samples <- posterior_epred(object,
                               newdata = newdata_list$newdata,
                               re_formula = if (is.null(object$re_formula)) NA
                                 else object$re_formula)
  newdat_nsec <- newdata_eval(
    object, resolution = 2, x_range = c(nsec_use, nsec_use)
  )

  pred_val_nsec <- posterior_epred(object,
                                   newdata = newdat_nsec$newdata,
                                   re_formula = if (is.null(object$re_formula)) NA
                                     else object$re_formula)
  at_nsec <- pred_val_nsec[, 2]

  # Each draw's control is compared with the same draw's curve at nsec. Keep
  # every posterior_epred() call here at the full set of draws: a model set
  # pairs row i across calls only because each call thins by the stored index.
  control_draws <- control_posterior(
    object, newdata_list$newdata,
    function(nd) posterior_epred(
      object, newdata = nd,
      re_formula = if (is.null(object$re_formula)) NA else object$re_formula
    ), x_at = control_reading$value
  )
  floor_draws <- switch(
    type,
    absolute = 0,
    relative = count_positive_asymptote(
      object, NULL, ecx_asymptote(object, "relative"),
      newdata_list$newdata
    ),
    range = apply(p_samples, 1, min, na.rm = TRUE)
  )
  ecnsecP <- (control_draws - at_nsec) / (control_draws - floor_draws) * 100
  ecnsec <- quantile(ecnsecP, probs = prob_vals, na.rm = TRUE)

    if (!posterior) {
      ecnsec
  } else {
      ecnsecP
  }
}




#' Refuse an nsec that lies outside the range ecnsec() reads the curve over
#'
#' One refusal for every class, so that the same question gets the same kind
#' of answer whichever object it is asked of.
#'
#' The comparison is made on the recorded scale, the scale of the prediction
#' grid and of \code{nsec} once \code{xform} is applied. The fitted scale would
#' fail where the transformation is undefined, as \code{log()} is at a
#' negative \code{nsec}.
#'
#' A value within a relative tolerance of an end is accepted: an estimate
#' censored at the end of the range is reported as that end, and a round trip
#' such as \code{exp(log(3.22))} can leave it a rounding error outside.
#'
#' @param object The object \code{ecnsec()} was called on.
#' @param nsec The \code{nsec} supplied.
#' @param nsec_use \code{nsec} after \code{xform}, on the recorded scale.
#' @param grid_range The range of the prediction grid, on the recorded scale.
#' @param x_range The \code{x_range} supplied.
#' @param xform The \code{xform} supplied.
#' @param curve A noun phrase naming the curve, for the message.
#'
#' @return \code{NULL}, invisibly. Called for the error.
#' @noRd
check_ecnsec_range <- function(object, nsec, nsec_use, grid_range, x_range,
                               xform, curve = "the curve") {
  tol <- sqrt(.Machine$double.eps) * max(1, abs(grid_range))
  # A non-finite value, such as log() of a negative nsec, is refused here too,
  # or it reaches newdata_eval() and is refused there as an x_range.
  outside <- !is.finite(nsec_use) | nsec_use < grid_range[1] - tol |
    nsec_use > grid_range[2] + tol
  if (!any(outside, na.rm = TRUE)) {
    return(invisible(NULL))
  }
  num <- function(v) paste(signif(v, 4), collapse = ", ")
  value <- if (identical(xform, identity)) {
    paste0("nsec = ", num(nsec))
  } else {
    paste0("nsec = ", num(nsec), ", which xform takes to ", num(nsec_use), ",")
  }
  tr <- inline_x_transform(object)
  on_scale <- if (is.null(tr)) "" else paste(" on the scale of", tr$variable)
  supplied <- !(length(x_range) == 1 && is.na(x_range))
  # One level of a joint refit is read over its own rows, unless the level has
  # no rows to read and the whole data are used.
  own_level <- !supplied && inherits(object, "bayesnecjointlevel") &&
    length(object$level_x) > 0
  where <- if (supplied) {
    "the x_range supplied"
  } else if (own_level) {
    paste0("the observed range of the predictor at level \"", object$level,
           "\" of ", object$group_var)
  } else {
    "the observed range of the predictor"
  }
  over <- if (own_level) {
    "the curve of each level over that level's own range"
  } else {
    paste(curve, "over that range")
  }
  remedy <- if (supplied) {
    "Supply an x_range that includes nsec."
  } else if (own_level) {
    paste0("Supply an x_range that includes nsec to read ", curve,
           " extended past that level's data.")
  } else {
    paste0("Supply an x_range that includes nsec to read ", curve,
           " extended past the data.")
  }
  stop(value, " lies outside ", where, ", ", signif(grid_range[1], 4), " to ",
       signif(grid_range[2], 4), on_scale, ". ecnsec() reads ", over,
       " only. ", remedy, call. = FALSE)
}
