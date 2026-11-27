#' Percent effect at a stated concentration
#'
#' For each posterior draw, calculates the decline from the predicted control
#' response to the response at `x`, as a percentage of the distance from the
#' control to the floor selected by `type`.
#'
#' @param object A `bayesnecfit`, `bayesmanecfit`, `bayesnecgroupfit`,
#'   `bayesnecjointfit` or `bayesnechurdlefit` object.
#' @param x One finite concentration. It is on the recorded predictor scale by
#'   default.
#' @param resolution Number of grid points used to find the floor for
#'   `type = "range"` and to interpolate the response of a hurdle pair
#'   at `x`. The default is 500.
#' @param x_range Prediction range on the recorded scale; `NA` uses the fit's
#'   range.
#' @param type `"absolute"` measures towards zero, `"relative"` towards the
#'   equation's theoretical asymptote, and `"range"` towards its lowest
#'   predicted response over the original prediction range. `"direct"` is
#'   unavailable because it names a response rather than a percentage.
#' @param x_scale `"recorded"` (the default) reads `x` on the recorded scale;
#'   `"fitted"` reads it on the scale fitted by `crf()` and converts it to the
#'   recorded scale before prediction.
#' @param prob_vals Summary probabilities, in central, lower, upper order.
#' @param ... Further arguments passed to methods. Unused arguments are refused.
#' @param posterior Return one value per posterior draw instead of quantiles.
#' @param pool How observed group levels are combined: `"none"`, `"mixture"`,
#'   `"mean_estimate"` or `"mean_curve"`. See [ecx()].
#' @param x_control The control concentration: `"own"`, `"shared"` or a
#'   single finite number. See [ecx()].
#' @param extrapolate `FALSE` uses the prediction range. One finite upper
#'   limit or two finite limits may extend it on the recorded scale. The floor
#'   for `type = "range"` is still read over the original range.
#' @param dpar On a joint two-block fit, `"mu"`, `"hu"` or `"zi"` selects one
#'   block; `NULL` uses the combined curve. See [ecx()].
#' @param which On a `bayesnechurdlefit`, `"combined"`, `"growth"` or
#'   `"survival"`. See [ecx()].
#'
#' @details The draw-level calculation is
#'   `(control - response_at_x) / (control - floor) * 100`. `"relative"` is
#'   refused for the combined curve of a joint two-block fit and for a hurdle
#'   pair, as in [ecx()]. Values are not truncated: a response below zero can
#'   give more than 100 percent under `"absolute"`, and a response above the
#'   control, such as the rising limb of `nechorme`, gives a negative value.
#'   At the control concentration every draw is exactly zero. A concentration
#'   outside the prediction range is refused unless `x_range` or
#'   `extrapolate` includes it.
#'
#' @return Named quantiles or, with `posterior = TRUE`, one percentage per draw.
#'   Grouped and joint fits return the same per-level structure as [ecx()].
#' @seealso [ecx()], [ecnsec()], [nsec()]
#' @examples
#' \donttest{
#' data(manec_example)
#' epx(manec_example, x = 2)
#' }
#' @export
epx <- function(object, x, resolution = 500, x_range = NA,
                type = "absolute", x_scale = c("recorded", "fitted"),
                prob_vals = c(0.5, 0.025, 0.975), ...,
                posterior = FALSE, pool = NULL, x_control = "own",
                extrapolate = FALSE, dpar = NULL,
                which = "combined") {
  UseMethod("epx")
}

#' @noRd
#' @export
epx.bayesnecfit <- function(object, x, resolution = 500, x_range = NA,
                             type = "absolute", x_scale = c("recorded", "fitted"),
                             prob_vals = c(0.5, 0.025, 0.975), ...,
                             posterior = FALSE, pool = NULL,
                             x_control = "own", extrapolate = FALSE,
                             dpar = NULL) {
  check_component_arg(list(...), object)
  check_removed_args(list(...))
  check_unused_dots(list(...), "epx")
  chk::chk_number(x)
  if (!is.finite(x)) stop("x must be finite.", call. = FALSE)
  check_resolution(resolution)
  chk::chk_flag(posterior)
  if (!is.null(pool)) validate_pool(pool, "epx")
  type <- epx_type(object, type, match.call(), dpar)
  epx_probs(prob_vals)
  x_scale <- match.arg(x_scale)
  if (is.null(object$observed_group)) {
    spec <- observed_level_spec(object)
    if (!is.null(spec)) {
      return(estimate_observed_levels(
        object, spec, "epx",
        function(level, control_value, range_override) {
          epx(level, x = x, resolution = resolution, x_range = range_override,
              type = type, x_scale = x_scale, prob_vals = prob_vals,
              posterior = TRUE, pool = "none", x_control = control_value,
              extrapolate = extrapolate, dpar = dpar)
        }, pool, posterior, prob_vals, x_control, x_range,
        curve_args = list(x = x, resolution = resolution, type = type,
                          x_scale = x_scale, prob_vals = prob_vals,
                          extrapolate = extrapolate, dpar = dpar)
      ))
    }
  }
  x_recorded <- epx_recorded_x(object, x, x_scale, extrapolate, x_range)
  lims <- curve_extrapolate_limits(extrapolate, object, x_range, x_control,
                                   "EPx")
  control_reading <- resolve_x_control(object, x_control, x_range)
  grid <- extended_newdata_eval(object, resolution, control_reading$x_range,
                                lims)
  check_epx_range(object, x, x_recorded, range(grid$x_vec), x_range,
                  x_scale)
  epred_fun <- function(nd) joint_hurdle_epred(
    object, nd, dpar,
    re_formula = if (is.null(object$re_formula)) NA else object$re_formula
  )
  curve <- epred_fun(grid$newdata)
  at_data <- newdata_eval(object, resolution = 2,
                          x_range = c(x_recorded, x_recorded))
  at_x <- epred_fun(at_data$newdata)[, 2]
  control <- control_posterior(object, grid$newdata, epred_fun,
                               x_at = control_reading$value)
  floor <- switch(type,
                  absolute = 0,
                  relative = count_positive_asymptote(
                    object, dpar, ecx_asymptote(object, "relative", dpar),
                    grid$newdata),
                  range = apply(curve[, grid$original, drop = FALSE], 1,
                                min, na.rm = TRUE))
  out <- (control - at_x) / (control - floor) * 100
  if (isTRUE(x_recorded == control_reading$value)) out[] <- 0
  if (posterior) return(epx_result(out, x, resolution))
  result <- stats::quantile(out, probs = prob_vals, na.rm = TRUE)
  names(result) <- clean_names(result)
  epx_result(result, x, resolution)
}

#' @noRd
#' @export
epx.bayesmanecfit <- function(object, x, resolution = 500,
                               x_range = NA, type = "absolute",
                               x_scale = c("recorded", "fitted"),
                               prob_vals = c(0.5, 0.025, 0.975), ...,
                               posterior = FALSE, pool = NULL,
                               x_control = "own", extrapolate = FALSE,
                               dpar = NULL) {
  check_component_arg(list(...), object)
  check_removed_args(list(...))
  check_unused_dots(list(...), "epx")
  chk::chk_number(x)
  if (!is.finite(x)) stop("x must be finite.", call. = FALSE)
  check_resolution(resolution)
  chk::chk_flag(posterior)
  if (!is.null(pool)) validate_pool(pool, "epx")
  type <- epx_type(object, type, match.call(), dpar)
  epx_probs(prob_vals)
  x_scale <- match.arg(x_scale)
  x_control <- model_set_control(object, x_control)
  first <- object$mod_fits[[1]]
  finish <- model_set_control_recorder(first, x_control)
  per_model <- function(f) epx(f, x = x, resolution = resolution,
                                x_range = x_range, type = type,
                                x_scale = x_scale, prob_vals = prob_vals,
                                posterior = TRUE, pool = pool,
                                x_control = x_control,
                                extrapolate = extrapolate, dpar = dpar)
  if (identical(pool, "none") && !is.null(observed_level_spec(first))) {
    return(finish(model_set_level_estimates(
      object, "epx", per_model, posterior, prob_vals)))
  }
  curve_extrapolate_limits(extrapolate, object, x_range, x_control, "EPx")
  warned <- options(bayesnec.relative_warned = TRUE)
  on.exit(options(warned), add = TRUE)
  models <- names(object$mod_fits)
  idx <- pull_draw_index(object, models, object$sample_size)
  models <- models[lengths(idx[models]) > 0L]
  parts <- once_level_note(lapply(models, function(m) {
    per_model(pull_equation(object, m))[idx[[m]]]
  }))
  out <- unlist(parts, use.names = FALSE)
  if (posterior) return(finish(epx_result(out, x, resolution)))
  result <- stats::quantile(out, probs = prob_vals, na.rm = TRUE)
  names(result) <- clean_names(result)
  finish(epx_result(result, x, resolution))
}

#' @noRd
#' @export
epx.bayesnecjointlevel <- function(object, ...) epx.bayesnecfit(object, ...)

#' Convert a stated fitted concentration using the estimator's inverse
#' @param object A fitted object.
#' @param x The stated concentration.
#' @param x_scale Its scale.
#' @param extrapolate The requested extension.
#' @param x_range The requested prediction range.
#' @return A recorded concentration.
#' @noRd
epx_recorded_x <- function(object, x, x_scale, extrapolate,
                           x_range = NA) {
  x_scale <- match.arg(x_scale, c("recorded", "fitted"))
  if (identical(x_scale, "recorded")) return(x)
  tr <- inline_x_transform(object)
  if (is.null(tr)) return(x)
  grid <- recorded_grid(object)
  limits <- x_range[is.finite(x_range)]
  if (is.numeric(extrapolate)) {
    limits <- c(limits, extrapolate[is.finite(extrapolate)])
  }
  if (length(limits) > 0L) {
    bounds <- range(c(grid, limits))
    grid <- sort(unique(c(grid, seq(bounds[1], bounds[2],
                                    length.out = max(1000L, length(grid))))))
  }
  inverse <- recorded_x_map(scale_source_fit(object)$bayesnecformula,
                            grid, tr)
  as.numeric(inverse(x))
}

#' Validate the percentage reference
#' @param object A fitted object.
#' @param type The requested floor.
#' @param mc The call, for the relative rename warning.
#' @param dpar The requested joint block.
#' @return The validated type.
#' @noRd
epx_type <- function(object, type, mc, dpar = NULL) {
  type <- validate_ecx_type(type, mc, warn = FALSE)
  if (identical(type, "direct")) {
    stop('type = "direct" names a response value rather than a percentage.',
         call. = FALSE)
  }
  if (identical(type, "relative") && is.null(dpar)) {
    fit <- if (inherits(object, "bayesmanecfit")) {
      object$mod_fits[[1]]
    } else object
    check_combined_relative(fit$fit$family, "epx")
  }
  warn_relative_rename(type, mc)
  type
}

#' Record the estimator and grid on a result
#' @param out Posterior draws or their summary.
#' @param x The stated concentration.
#' @param resolution Grid resolution.
#' @return The result with estimator attributes.
#' @noRd
epx_result <- function(out, x, resolution) {
  attr(out, "x") <- x
  attr(out, "resolution") <- resolution
  attr(out, "toxicity_estimate") <- "epx"
  out
}

#' @noRd
epx_probs <- function(prob_vals) {
  if (length(prob_vals) < 3 || prob_vals[1] < prob_vals[2] ||
      prob_vals[1] > prob_vals[3] || prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order.", call. = FALSE)
  }
}

#' Refuse concentrations outside the curve's prediction range
#' @param object A fitted object.
#' @param x The stated value.
#' @param x_recorded Its recorded-scale value.
#' @param grid_range The prediction range.
#' @param x_range The supplied prediction range.
#' @param x_scale The scale of the stated value.
#' @param curve A noun phrase for the curve.
#' @return NULL invisibly, or an error.
#' @noRd
check_epx_range <- function(object, x, x_recorded, grid_range, x_range,
                            x_scale, curve = "the curve") {
  tol <- sqrt(.Machine$double.eps) * max(1, abs(grid_range))
  if (isTRUE(is.finite(x_recorded) && x_recorded >= grid_range[1] - tol &&
             x_recorded <= grid_range[2] + tol)) return(invisible(NULL))
  value <- paste0("x = ", signif(x, 4))
  if (identical(x_scale, "fitted") &&
      !is.null(inline_x_transform(object))) {
    value <- paste0(value, ", which maps to ", signif(x_recorded, 4),
                    " on the recorded scale,")
  }
  supplied <- !is.null(x_range) &&
    !(length(x_range) == 1 && is.na(x_range))
  own_level <- !supplied && inherits(object, "bayesnecjointlevel") &&
    length(object$level_x) > 0
  where <- if (supplied) "the x_range supplied" else if (own_level) {
    paste0('the observed range of the predictor at level "', object$level,
           '" of ', object$group_var)
  } else "the observed range of the predictor"
  over <- if (own_level) "the curve of each level over that level's own range" else
    paste(curve, "over that range")
  remedy <- if (own_level) {
    paste0("Supply an x_range that includes x to read ", curve,
           " extended past that level's data.")
  } else {
    paste0("Supply an x_range that includes x to read ", curve,
           if (supplied) "." else " extended past the data.")
  }
  stop(value, " lies outside ", where, ", ", signif(grid_range[1], 4),
       " to ", signif(grid_range[2], 4), ". epx() reads ", over,
       " only. ", remedy, call. = FALSE)
}

#' Read one posterior curve at a stated concentration
#' @param p_samples Draws by grid point.
#' @param grid Increasing recorded concentrations.
#' @param x The requested recorded concentration.
#' @return One value per draw.
#' @noRd
epx_curve_at <- function(p_samples, grid, x) {
  if (x <= grid[1]) return(p_samples[, 1])
  n <- length(grid)
  if (x >= grid[n]) return(p_samples[, n])
  i <- findInterval(x, grid)
  if (grid[i] == x) return(p_samples[, i])
  w <- (x - grid[i]) / (grid[i + 1] - grid[i])
  (1 - w) * p_samples[, i] + w * p_samples[, i + 1]
}

#' @noRd
#' @export
epx.bayesnechurdlefit <- function(object, x, resolution = 500,
                                   x_range = NA, type = "absolute",
                                   x_scale = c("recorded", "fitted"),
                                   prob_vals = c(0.5, 0.025, 0.975), ...,
                                   posterior = FALSE, which = "combined",
                                   pool = NULL, x_control = "own",
                                   extrapolate = FALSE) {
  check_component_arg(list(...), object)
  check_removed_args(list(...))
  check_unused_dots(list(...), "epx")
  chk::chk_number(x)
  if (!is.finite(x)) stop("x must be finite.", call. = FALSE)
  check_resolution(resolution)
  chk::chk_flag(posterior)
  if (!is.null(pool)) validate_pool(pool, "epx")
  which <- hurdle_check_which(which)
  type <- validate_ecx_type(type, match.call(), warn = FALSE)
  if (identical(type, "direct")) {
    stop('type = "direct" names a response value rather than a percentage.',
         call. = FALSE)
  }
  if (identical(type, "relative")) {
    stop('type = "relative" is not defined for a hurdle fit, whose ',
         'asymptote is not a single fitted parameter. Use ',
         'type = "absolute" or type = "range".', call. = FALSE)
  }
  epx_probs(prob_vals)
  x_scale <- match.arg(x_scale)
  if (is.null(object$observed_group)) {
    spec <- observed_level_spec(object)
    if (!is.null(spec)) {
      result <- estimate_observed_levels(
        object, spec, "epx",
        function(level, control_value, range_override) {
          epx(level, x = x, resolution = resolution,
              x_range = range_override, type = type,
              x_scale = x_scale, prob_vals = prob_vals,
              posterior = TRUE, which = which, pool = "none",
              x_control = control_value, extrapolate = extrapolate)
        }, pool, posterior, prob_vals, x_control, x_range,
        curve_args = list(x = x, resolution = resolution, type = type,
                          x_scale = x_scale, prob_vals = prob_vals,
                          extrapolate = extrapolate, which = which))
      attr(result, "component") <- which
      return(result)
    }
  }
  x_recorded <- epx_recorded_x(object, x, x_scale, extrapolate, x_range)
  lims <- curve_extrapolate_limits(extrapolate, object, x_range, x_control,
                                   "EPx")
  preds <- hurdle_component_preds(object, resolution = resolution,
                                  x_range = x_range, x_control = x_control,
                                  grid_limits = lims, fitted_grid = TRUE)
  check_epx_range(object, x, x_recorded, range(preds$x), x_range,
                  x_scale, curve = paste("the", which, "curve"))
  curve <- preds[[which]]
  control <- preds$control[[which]]
  original <- if (is.null(preds$original)) TRUE else preds$original
  floor <- switch(type, absolute = 0,
                  range = apply(curve[, original, drop = FALSE], 1,
                                min, na.rm = TRUE))
  at_x <- epx_curve_at(curve, preds$x, x_recorded)
  out <- (control - at_x) / (control - floor) * 100
  control_at <- if (is.numeric(x_control)) as.numeric(x_control) else
    control_x(object)
  if (isTRUE(x_recorded == control_at)) out[] <- 0
  if (posterior) {
    result <- epx_result(out, x, resolution)
    attr(result, "component") <- which
    return(result)
  }
  estimate <- stats::quantile(out, probs = prob_vals, na.rm = TRUE)
  names(estimate) <- clean_names(estimate)
  estimate <- epx_result(estimate, x, resolution)
  attr(estimate, "component") <- which
  estimate
}

#' Name positional arguments and refuse unused arguments before pooling
#' @param dots The arguments passed through `...`.
#' @return Named estimator arguments, or an error.
#' @noRd
epx_wrapper_dots <- function(dots) {
  allowed <- c("resolution", "x_range", "type", "x_scale", "prob_vals",
               "posterior", "pool", "x_control", "extrapolate", "dpar",
               "which")
  positional <- allowed[1:5]
  nms <- names(dots)
  if (is.null(nms)) nms <- rep("", length(dots))
  for (i in which(!nzchar(nms))) {
    open <- setdiff(positional, nms[nzchar(nms)])
    if (length(open) == 0L) {
      stop("epx() has an unused positional argument.", call. = FALSE)
    }
    nms[i] <- open[1]
  }
  names(dots) <- nms
  bad <- setdiff(nms, allowed)
  if (length(bad) > 0L) {
    stop("epx() does not use `", bad[1], "`.", call. = FALSE)
  }
  dots
}
