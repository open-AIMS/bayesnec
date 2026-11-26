#' Extracts the percent effect associated with a predicted NSEC value from an
#' object of class \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#' @param resolution The number of unique x values over which the curve is
#' predicted. It affects only \code{type = "range"}, where the denominator is
#' the lowest response the curve predicts over the grid. The default of 500
#' matches \code{\link{ecx}} and \code{\link{nsec}}.
#' @param type A \code{\link[base]{character}} vector, taking values of
#' "absolute" (the default), "relative" or "range". See Details.
#' @param x_range A range of x values over which to predict the curve.
#' @param sig_val The control posterior quantile used as the NSEC reference.
#' The default is 0.01, as in \code{\link{nsec}}.
#' @param extrapolate \code{FALSE}, the default, uses the prediction range.
#' A single finite number extends the upper end; two finite numbers may
#' extend both ends. The original grid points are retained. Limits are on
#' the recorded predictor scale. \code{TRUE} and narrowing limits are refused.
#' For \code{type = "range"} the lowest response is taken over the prediction
#' range before it is extended.
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
#' the concentration directly. A control below a level's observations
#' extrapolates its curve, and gives a warning unless it is the control
#' recorded at fitting. Level controls are recorded in the result's
#' \code{x_control} attribute.
#' @param ... Further arguments to pass to class specific methods. A method
#' refuses any argument it does not use.
#' @param posterior A \code{\link[base]{logical}} value indicating if the full
#' posterior sample of calculated ECNSEC values should be returned instead of
#' just the median and 95 credible intervals.
#'
#' @details Each posterior draw is evaluated at its own NSEC, where its
#' curve reaches the \code{sig_val} quantile of the control posterior. The
#' effect is the decline from that draw's control to the reference, divided
#' by the distance from its control to the floor selected by \code{type}.
#' The floor is zero for \code{"absolute"}, the fitted asymptote for
#' \code{"relative"}, and the lowest predicted response over the original
#' range for \code{"range"}. A draw whose control is at or below the
#' reference has zero effect at the control concentration. With
#' \code{type = "range"}, a draw whose NSEC lies above the prediction
#' range is censored because its effect exceeds the response span read there.
#' The \code{"absolute"} values equal the \code{ecnsec_relativeP}
#' attribute of \code{\link{nsec}} under matching options.
#'
#' The \code{pool}, \code{x_control} and \code{extrapolate} options use
#' the same rules as \code{\link{nsec}}. \code{type = "relative"} is
#' unavailable for a joint two-block hurdle curve.
#'
#' @seealso \code{\link{bnec}}
#'
#' @return A percentage summary or, with \code{posterior = TRUE}, one
#' percentage per posterior draw. Grouped results follow \code{\link{nsec}}.
#' @examples
#' \donttest{
#' library(bayesnec)
#' data(manec_example)
#' ecnsec(manec_example)
#' }
#'
#' @export
ecnsec <- function(object, resolution = 500, x_range = NA,
                   type = "absolute", sig_val = 0.01,
                   prob_vals = c(0.5, 0.025, 0.975), ...,
                   posterior = FALSE, pool = NULL, x_control = "own",
                   extrapolate = FALSE) {
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
#' @importFrom chk chk_flag chk_number
#'
#' @noRd
#'
#' @export
ecnsec.bnecfit <- function(object, resolution = 500, x_range = NA,
                             type = "absolute", sig_val = 0.01,
                             prob_vals = c(0.5, 0.025, 0.975), ...,
                             posterior = FALSE, pool = NULL,
                             x_control = "own", extrapolate = FALSE) {
  chk_flag(posterior)
  check_ecnsec_dots(list(...))
  check_sig_val(sig_val)
  check_resolution(resolution)
  if (!is.null(pool)) validate_pool(pool, "ecnsec")

  type <- validate_ecx_type(type, match.call(), warn = FALSE)
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
  warn_relative_rename(type, match.call())
  if (length(prob_vals) < 3 | prob_vals[1] < prob_vals[2] |
      prob_vals[1] > prob_vals[3] | prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order.")
  }
  if (identical(type, "absolute")) {
    result <- nsec(object, sig_val = sig_val, resolution = resolution,
                   x_range = x_range, extrapolate = extrapolate,
                   prob_vals = prob_vals, posterior = TRUE, pool = pool,
                   x_control = x_control)
    return(ecnsec_from_nsec(result, posterior, prob_vals))
  }
  if (inherits(object, "bayesmanecfit")) {
    x_control <- model_set_control(object, x_control)
    first <- object$mod_fits[[1]]
    finish <- model_set_control_recorder(first, x_control)
    if (!is.null(observed_level_spec(first))) {
      per_model <- function(f) ecnsec(
        f, resolution = resolution, x_range = x_range,
        extrapolate = extrapolate,
        type = type, sig_val = sig_val, prob_vals = prob_vals,
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
          ecnsec(level, resolution = resolution,
                 x_range = range_override, extrapolate = extrapolate,
                 type = type, sig_val = sig_val,
                 prob_vals = prob_vals, posterior = TRUE, pool = "none",
                 x_control = control_value)
        },
        pool, posterior, prob_vals, x_control, x_range,
        curve_args = list(sig_val = sig_val, resolution = resolution,
                          extrapolate = extrapolate,
                          type = type,
                          prob_vals = prob_vals)
      ))
    }
  }
  lims <- curve_extrapolate_limits(extrapolate, object, x_range, x_control,
                                   "ECNSEC")
  control_reading <- resolve_x_control(object, x_control, x_range)
  newdata_list <- extended_newdata_eval(
    object, resolution = resolution, x_range = control_reading$x_range,
    lims = lims
  )
  p_samples <- estimator_epred(object,
                               newdata = newdata_list$newdata,
                               re_formula = if (is.null(object$re_formula)) NA
                                 else object$re_formula)
  # Keep each prediction call at the full set of draws. A model set pairs
  # rows only because each call uses the stored draw index.
  control_draws <- control_posterior(
    object, newdata_list$newdata,
    function(nd) estimator_epred(
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
    range = apply(p_samples[, newdata_list$original, drop = FALSE], 1, min,
                  na.rm = TRUE)
  )
  reference <- stats::quantile(control_draws, sig_val)
  ecnsecP <- pmax(control_draws - reference, 0) /
    (control_draws - floor_draws) * 100
  if (identical(type, "range")) {
    crossing <- nsec(object, sig_val = sig_val, resolution = resolution,
                     x_range = x_range, extrapolate = extrapolate,
                     posterior = TRUE, pool = pool, x_control = x_control)
    ecnsecP[is.infinite(crossing) & crossing > 0 &
              control_draws > reference] <- Inf
  }
  if (posterior) ecnsecP else
    stats::quantile(ecnsecP, probs = prob_vals, na.rm = TRUE)
}




#' Refuse arguments removed when ecnsec became the effect at each draw's NSEC
#' @param dots The arguments in `...`.
#' @noRd
check_ecnsec_dots <- function(dots, forwarded = FALSE) {
  for (arg in c("nsec", "xform", "x_scale")) {
    if (arg %in% names(dots)) {
      stop("ecnsec() returns the effect at each draw's own NSEC and has no `",
           arg, "` argument.", call. = FALSE)
    }
  }
  if (!forwarded) {
    check_removed_args(dots)
    check_unused_dots(dots, "ecnsec")
  }
}

#' Read and summarise the effect attribute returned by nsec()
#' @param result A posterior vector or a named list of level vectors.
#' @param posterior Whether to return draws.
#' @param prob_vals Summary probabilities.
#' @noRd
ecnsec_from_nsec <- function(result, posterior, prob_vals) {
  if (is.list(result) && is.null(attr(result, "ecnsec_relativeP"))) {
    draws <- lapply(result, function(x) attr(x, "ecnsec_relativeP"))
    if (posterior) return(draws)
    summaries <- lapply(draws, function(x) {
      out <- stats::quantile(x, prob_vals, na.rm = TRUE)
      names(out) <- clean_names(out)
      out
    })
    return(estimate_table(summaries, names(result), "ecnsec"))
  }
  draws <- attr(result, "ecnsec_relativeP")
  if (posterior) return(draws)
  stats::quantile(draws, prob_vals, na.rm = TRUE)
}
