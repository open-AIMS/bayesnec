#' Extracts the predicted ECx value
#' 
#' Extracts the predicted ECx value as desired from an object of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}}.
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#' @param ecx_val The desired percentage effect value, defaults to 10. Any
#' value above 0 is accepted for every \code{type} except "direct", where it
#' is a response value rather than a percentage. Values above 100 are
#' meaningful under "absolute" for a response that can go negative --- see
#' Details.
#' @param type A \code{\link[base]{character}} vector, taking values of
#' "absolute" (the default), "relative", "range" or "direct". See Details.
#' @param resolution The number of unique x values over which to find ECx.
#' The crossing is located by linear interpolation between the two grid values
#' that bracket it, so precision saturates well below the grid spacing.
#' Increasing the resolution beyond the default of 200 changed the estimate by
#' less than 0.01 percent on the fits tested, and increases the run time
#' roughly in proportion.
#' @param posterior A \code{\link[base]{logical}} value indicating if the full
#' posterior sample of calculated ECx values should be returned instead of
#' just the median and 95 credible intervals.
#' @param xform A function to apply to the returned estimated concentration
#' values.
#' @param x_range A range of x values over which to consider extracting ECx.
#' @param prob_vals A vector indicating the probability values over which to
#' return the estimated ECx value. Defaults to 0.5 (median) and 0.025 and
#' 0.975 (95 percent credible intervals).
#' @param dpar For a joint two-block fit only (\code{family = "hurdle_gamma"},
#' \code{"zero_inflated_beta"}, \code{"hurdle_poisson"} or
#' \code{"hurdle_negbinomial"}), the parameter block to report:
#' \code{"mu"} for the response block, or \code{"hu"} (\code{"zi"} for the
#' zero-inflated families) for survival. Defaults to \code{NULL}, which gives
#' the expected positive response multiplied by \code{1 - hu}. For continuous
#' hurdles the positive response is \code{mu}; for count hurdles it is
#' \code{E[Y | Y > 0]}. The zero-probability block is
#' inverted to survival before computing, so ECx keeps its usual meaning of a
#' percentage decline from the fitted control value. See Details.
#' For the count hurdles, \code{"mu"} is converted to the positive-count mean
#' \code{E[Y | Y > 0]} so it matches the growth component returned by
#' \code{\link{bnec_hurdle}}. With \code{type = "relative"}, \code{"mu"} is
#' measured towards the response block's asymptote and \code{"hu"} or
#' \code{"zi"} towards the zero block's own \code{bot}, or 0 where its
#' equation has none. The combined curve, \code{dpar = NULL}, is refused under
#' \code{type = "relative"}, because its asymptote is the product of the two
#' blocks' asymptotes rather than a fitted parameter.
#' @param ... Additional arguments passed to methods.
#'
#' @details \bold{Every ECx is measured from the control.} The control is the
#' predicted mean at the lowest concentration in the supplied predictor, taken
#' per posterior draw. \code{type} names what the percentage is measured
#' \emph{towards}:
#'
#' \itemize{
#'   \item \code{"absolute"} (the default) --- control to 0.
#'   \item \code{"relative"} --- control to the equation's theoretical
#'     asymptote, the \code{bot} parameter where the equation has one and 0
#'     otherwise. Refused where the equation has no \code{bot} and the family
#'     is unbounded below, because there is then no finite denominator. On the
#'     positive counts of a negative binomial count hurdle the asymptote is
#'     their zero-truncated mean at \code{mu = bot}, which needs the shape
#'     there, and the estimate is refused where the shape is modelled by
#'     \code{disp(~x)}; see \code{\link{bnec}}.
#'   \item \code{"range"} --- control to the lowest response the curve
#'     predicts over the predictor range.
#'   \item \code{"direct"} --- \code{ecx_val} is a response value rather than
#'     a percentage, and the x value at which the curve reaches it is returned.
#' }
#'
#' \code{"range"} is what \code{"relative"} computed in versions up to
#' 2.1.3, except that the span was taken from the maximum of the predicted
#' curve rather than from the control. The two differ only for a curve whose
#' maximum is not at the lowest concentration, which in practice means a
#' hormetic one.
#'
#' Measuring from the control rather than from the maximum of the predicted
#' curve is what makes an ECx well defined for a hormesis equation: the target
#' response lies below the control, the rising limb lies above it, so the
#' curve crosses the target exactly once. The \code{hormesis_def} argument
#' selected between those two references and has been removed; the control is
#' now always the reference.
#'
#' \code{"absolute"} measures towards 0 even for a family whose support is
#' unbounded below. This follows OECD TG 201, whose percent inhibition
#' rearranges to the same reference and which permits a value above 100 per
#' cent rather than truncating it, so \code{ecx_val} is not capped at 100.
#'
#' Where the curve does not reach the target anywhere in the predictor range
#' the ECx is not identified and \code{NA} is returned, with a warning
#' reporting how many draws were affected.
#'
#' Where \code{crf()} transforms the predictor inline, as in
#' \code{crf(log(concentration))}, the ECx is on the transformed scale unless
#' \code{xform} is supplied, and a message says so once per call. It names the
#' transformation and the \code{xform} that returns the estimate on the
#' recorded scale.
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
#' @seealso \code{\link{bnec}}, \code{\link{bnec_hurdle}}, \code{\link{nsec}}
#'
#' @return A vector containing the estimated ECx value, including upper and
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
#' One case is reported less precisely than it was before that change. A draw
#' whose curve declines away from the target everywhere --- \code{type =
#' "direct"} with a response value the curve never attains, and an absolute
#' target above a control that is itself negative --- has no such concentration
#' at any value of the predictor, not merely none within the range. It is
#' reported here as at or beyond the top of the range, which is the treatment
#' specification 4.5 gives to every draw the search did not identify; up to
#' version 2.1.3 it returned \code{NA}, which said only that the estimate was
#' not identified. The censored fraction beside the estimate is what
#' distinguishes the two: where it is the whole posterior, no draw reached the
#' target.
#'
#' @examples
#' \donttest{
#' library(brms)
#' library(bayesnec)
#' data(manec_example)
#' ecx(manec_example, ecx_val = 50)
#' ecx(manec_example)
#' }
#'
#' @export
# Keep dpar on the generic, after `...` as in the methods. The methods are
# @noRd, so the generic's usage is the only place it can appear, and R CMD
# check warns about a documented argument missing from the usage.
ecx <- function(object, ecx_val = 10, resolution = 200,
                posterior = FALSE, type = "absolute", x_range = NA,
                xform = identity, prob_vals = c(0.5, 0.025, 0.975), ...,
                dpar = NULL) {
  UseMethod("ecx")
}

#' @inheritParams ecx
#'
#' @inherit ecx details return seealso examples
#'
#' @param object An object of class \code{\link{bayesnecfit}} returned by
#' \code{\link{bnec}}.
#' 
#' @importFrom stats quantile
#' @importFrom brms posterior_epred
#' @importFrom chk chk_logical chk_numeric
#'
#' @noRd
#'
#' @export
ecx.bayesnecfit <- function(object, ecx_val = 10, resolution = 200,
                            posterior = FALSE, type = "absolute",
                            x_range = NA, xform = identity,
                            prob_vals = c(0.5, 0.025, 0.975), ...,
                            dpar = NULL) {
  check_component_arg(list(...), object)
  check_removed_args(list(...))
  chk_numeric(ecx_val)
  if (length(ecx_val)>1) {
    stop("You may only pass one ecx_val")  
  }
  chk_numeric(resolution)  
  chk_logical(posterior)
  type <- validate_ecx_type(type, match.call())
  if (!inherits(xform, "function")) {
    stop("xform must be a function.")
  }
  if (length(prob_vals) < 3 || prob_vals[1] < prob_vals[2] ||
      prob_vals[1] > prob_vals[3] || prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order")
  }
  # Allow ecx_val > 100: with a positive control, "absolute" then targets a
  # negative response, as OECD TG 201 permits. An unreached target gives NA
  # for that draw, with the warning below.
  if (type != "direct" && ecx_val <= 0) {
    stop("Supplied ecx_val is not in the required range. ",
         "Please supply a percentage value greater than 0.", call. = FALSE)
  }
  if (identical(type, "relative") && is.null(dpar)) {
    check_combined_relative(object$fit$family, "ecx")
  }
  # Once for the call. See report_fitted_scale().
  quiet <- report_fitted_scale(object, xform, "ecx")
  on.exit(options(quiet), add = TRUE)
  # Do not refuse an absolute ECx for a gaussian response without a bot
  # parameter. Absolute measures towards 0 by design, and a gaussian mean that
  # declines towards 0 is consistent: the likelihood never tests the sign of y.
  newdata_list <- newdata_eval(
    object, resolution = resolution, x_range = x_range
  )
  # NULL gives a two-block fit's combined mean: positive-part mean times its
  # non-zero probability (1 - hu or 1 - zi). dpar selects one block; invert
  # the zero-probability block so that declines represent decreasing response.

  # The closure applies the same dpar handling to the control prediction as to
  # the curve, so that both are read on one quantity.
  epred_fun <- function(nd) joint_hurdle_epred(object, nd, dpar)
  p_samples <- epred_fun(newdata_list$newdata)
  x_vec <- newdata_list$x_vec
  control <- control_posterior(object, newdata_list$newdata, epred_fun)
  asymptote <- ecx_asymptote(object, type, dpar)
  asymptote <- count_positive_asymptote(
    object, dpar, asymptote, newdata_list$newdata
  )
  ecx_out <- ecx_from_posterior(p_samples, x_vec, ecx_val, type, control,
                                asymptote)
  below <- attr(ecx_out, "below_range")
  above <- is.na(ecx_out) & !below
  attr(ecx_out, "below_range") <- NULL
  ecx_out <- sub_x_transformation(ecx_out, object$bayesnecformula)
  # Build the censoring record here, not from the fit, because x_range may
  # have changed the grid. Build it on the recorded scale, where the curve was
  # searched, then remap it with sub_x_transformation() as the estimates were.
  # Comparing against fitted-scale bounds directly would misname the ends
  # under a decreasing crf() transformation.
  x_kept <- x_vec[is.finite(x_vec)]
  cens <- xform_censoring(
    censoring_record(max(x_kept), min(x_kept), above, below),
    function(value) sub_x_transformation(value, object$bayesnecformula)
  )
  # Apply xform to the censoring bounds as well as the estimates, before the
  # warning, so that the bound it reports is on the estimates' scale.
  if (inherits(xform, "function")) {
    ecx_out <- xform(ecx_out)
    cens <- xform_censoring(cens, xform)
  }
  attr(ecx_out, "censored") <- cens
  if (sum(above) > 0) {
    warning("The ", object$model, " curve does not reach the ", type,
            " ECx", ecx_val, " target anywhere in the predictor range for ",
            sum(above), " of ", length(ecx_out), " draws. The estimate is ",
            "censored at ", signif(censored_end(cens, "above"), 3),
            ": those draws keep their rank in the summary and are given no ",
            "value.", call. = FALSE)
  }
  if (sum(below) > 0) {
    warning("The ", object$model, " curve has already reached the ", type,
            " ECx", ecx_val, " target where the predictor range begins, for ",
            sum(below), " of ", length(ecx_out), " draws. The estimate is ",
            "censored at ", signif(censored_end(cens, "below"), 3), ".",
            call. = FALSE)
  }

  ecx_estimate <- summarise_censored(unlist(ecx_out), prob_vals, cens)
  names(ecx_estimate) <- clean_names(ecx_estimate)
  attr(ecx_estimate, "resolution") <- resolution
  attr(ecx_out, "resolution") <- resolution
  attr(ecx_estimate, "ecx_val") <- ecx_val
  attr(ecx_out, "ecx_val") <- ecx_val
  attr(ecx_estimate, "toxicity_estimate") <- "ecx"
  attr(ecx_out, "toxicity_estimate") <-  "ecx"
  # Skip both advisory messages when every draw is NA: signif(NA) == signif(NA)
  # is NA, and if() on it is an error. The warning above has reported it.
  if (all(is.na(ecx_estimate))) {
    NULL
  } else if (!is.null(attr(ecx_estimate, "censored_summary"))) {
    # The censored summary already reports the affected entries as bounds,
    # which is what the advisories below ask the caller to do by hand.
    NULL
  } else if (signif(ecx_estimate[1], 3) == signif(ecx_estimate[3], 3)) {
    message("The estimated mean is identical or nearly identical to your",
            " upper credible interval for the ", object$model, " model.",
            " This suggests the ecx estimate lies beyond the upper bound of",
            " your x_range and should be reported as greater than, and used",
            " as a censored value. You could try increasing x_range, although",
            " extrapolation beyond the data range should be done with",
            " caution.")
  } else if (signif(ecx_estimate[3], 3) == signif(max(x_vec), 3)) {
    message("The estimated upper credible interval is identical or nearly",
            " identical to the upper bound of your x_range value for the ",
            object$model, " model. This suggests the estimated uncertainty",
            " may be constrained. You could try increasing x_range to ensure",
            " this is not the case.")
  }
  if (!posterior) {
    ecx_estimate
  } else {
    ecx_out
  }
}

#' @inheritParams ecx
#'
#' @param object An object of class \code{\link{bayesmanecfit}} returned by
#' \code{\link{bnec}}.
#'
#' @inherit ecx details return seealso examples
#'
#' @importFrom stats quantile
#' @importFrom chk chk_logical chk_numeric
#'
#' @noRd
#'
#' @export
ecx.bayesmanecfit <- function(object, ecx_val = 10, resolution = 200,
                              posterior = FALSE, type = "absolute",
                              x_range = NA, xform = identity,
                              prob_vals = c(0.5, 0.025, 0.975), ...,
                              dpar = NULL) {
  check_component_arg(list(...), object)
  check_removed_args(list(...))
  chk_numeric(ecx_val)
  chk_numeric(resolution)  
  chk_logical(posterior)
  if (length(ecx_val)>1) {
    stop("You may only pass one ecx_val")  
  }
  type <- validate_ecx_type(type, match.call())
  if (!inherits(xform, "function")) { 
    stop("xform must be a function.")}   
  if (length(prob_vals) < 3 || prob_vals[1] < prob_vals[2] ||
      prob_vals[1] > prob_vals[3] || prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order")
  }
  # Warn about the relative rename once for the call. sample_ecx() below calls
  # ecx() for each equation with type passed explicitly, which would otherwise
  # warn again for every equation.
  warned <- options(bayesnec.relative_warned = TRUE)
  on.exit(options(warned), add = TRUE)
  # The scale message likewise, by the same device. See report_fitted_scale().
  quiet <- report_fitted_scale(object, xform, "ecx")
  on.exit(options(quiet), add = TRUE)
  sample_size <- object$sample_size
  # Use the weighted draw index every other quantity on this object uses. A
  # fresh unseeded sample() would give a different estimate on every call.
  draw_index <- pull_draw_index(object, names(object$mod_fits), sample_size)
  # Close over the arguments rather than pass them by position. Positional
  # dispatch through sapply() drops any argument it does not list, dpar among
  # them, and would silently return a two-block fit's combined curve.
  sample_ecx <- function(x) {
    mod <- names(object$mod_fits)[x]
    target <- suppressMessages(pull_out(object, model = mod))
    out <- ecx(target, ecx_val = ecx_val, resolution = resolution,
               posterior = TRUE, type = type,
               x_range = x_range, xform = xform, prob_vals = prob_vals,
               dpar = dpar)
    idx <- draw_index[[mod]]
    sample_out <- out[idx]
    # Subsetting drops the censoring record, so rebuild the part for these
    # draws. Otherwise a single fit and its one-model average would report the
    # same estimate differently.
    attr(sample_out, "censored") <-
      subset_censoring(attr(out, "censored"), idx)
    sample_out
  }
  to_iter <- seq_len(length(object$success_models))
  ecx_parts <- lapply(to_iter, sample_ecx)
  ecx_out <- unlist(lapply(ecx_parts, as.numeric))
  cens <- concat_censoring(lapply(ecx_parts, attr, "censored"),
                           vapply(ecx_parts, length, integer(1)))
  attr(ecx_out, "censored") <- cens
  ecx_estimate <- summarise_censored(ecx_out, prob_vals, cens)
  names(ecx_estimate) <- clean_names(ecx_estimate)
  attr(ecx_estimate, "resolution") <- resolution
  attr(ecx_out, "resolution") <- resolution
  attr(ecx_estimate, "ecx_val") <- ecx_val
  attr(ecx_out, "ecx_val") <- ecx_val
  attr(ecx_estimate, "toxicity_estimate") <- "ecx"
  attr(ecx_out, "toxicity_estimate") <-  "ecx"
  if (!posterior) {
    ecx_estimate
  } else {
    ecx_out
  }
}

#' Validate the type argument, and warn on the relative rename
#'
#' \code{"relative"} meant the control-to-minimum span up to 2.1.3 and means
#' the control-to-asymptote span from 2.2.0. A caller who writes it explicitly
#' is warned once, and told to write \code{"range"} for the old behaviour. It
#' warns rather than stops, because the new meaning is a valid request, and
#' uses a plain warning to avoid a dependency on \pkg{lifecycle}.
#'
#' @noRd
validate_ecx_type <- function(type, mc) {
  valid <- c("absolute", "relative", "range", "direct")
  if (length(type) != 1 || !is.character(type) || !(type %in% valid)) {
    stop("type must be one of 'absolute' (the default), 'relative', 'range' ",
         "or 'direct'. Please see ?ecx for more details.", call. = FALSE)
  }
  if (identical(type, "relative") && "type" %in% names(mc) &&
      !isTRUE(getOption("bayesnec.relative_warned"))) {
    warning("type = \"relative\" now measures from the control to the ",
            "equation's theoretical asymptote (the bot parameter, or 0 where ",
            "the equation has none). Up to bayesnec 2.1.3 it measured from ",
            "the maximum of the predicted curve to its minimum, which is now ",
            "type = \"range\". The two differ for a hormetic curve and ",
            "wherever the equation has a bot parameter.", call. = FALSE)
  }
  type
}

#' The theoretical asymptote each draw's curve declines towards
#'
#' Used by \code{type = "relative"} only, and \code{NA} for every other type.
#' An equation with a \code{bot} parameter declines towards it, and one
#' without declines towards 0. An equation without \code{bot} is refused under
#' a family not bounded below, because 0 is then no bound.
#'
#' A \code{\link{bayesmanecfit}} has no single \code{fit}, and its curve is the
#' model-averaged one, so \code{manec_asymptote()} assembles the model-averaged
#' asymptote.
#'
#' With \code{dpar} naming the zero block of a two-block fit, the curve is
#' that block's and so is the asymptote; see \code{zero_block_asymptote()}.
#'
#' @importFrom brms as_draws_df
#' @noRd
ecx_asymptote <- function(object, type, dpar = NULL) {
  if (!identical(type, "relative")) {
    return(NA_real_)
  }
  if (inherits(object, "bayesmanecfit")) {
    return(manec_asymptote(object))
  }
  if (!is.null(dpar) && is_hurdle_family(object$fit$family) &&
        identical(dpar, hurdle_dpar(object$fit$family))) {
    return(zero_block_asymptote(object, dpar))
  }
  # A joint refit level names its coefficient b_bot_<group_var><level>, which
  # the b_bot_Intercept read below does not find. Without this branch such a
  # level would be measured towards 0 instead of bot, without an error.
  if (inherits(object, "bayesnecjointlevel")) {
    bot_draws <- joint_level_draws(object, "bot")
    if (!is.null(bot_draws)) {
      return(bot_draws)
    }
  } else {
    bot_draws <- as_draws_df(object$fit)[["b_bot_Intercept"]]
    if (!is.null(bot_draws)) {
      return(as.numeric(bot_draws))
    }
  }
  if (!family_has_lower_bound(object$fit$family)) {
    stop("type = \"relative\" needs a finite asymptote to measure towards. ",
         "The ", object$model, " equation has no bot parameter, so it ",
         "declines towards 0, and the ", object$fit$family$family,
         " family is not bounded below, so 0 is not a bound. Use ",
         "type = \"absolute\" to measure towards 0 regardless, or ",
         "type = \"range\" to measure towards the lowest predicted response.",
         call. = FALSE)
  }
  0
}

#' The asymptote of a two-block fit's zero block
#'
#' @param object A \code{bayesnecfit}, or one level of a joint refit.
#' @param dpar The zero block's name, \code{"hu"} or \code{"zi"}.
#'
#' @details \code{ecx(dpar = "hu")} reads \code{1 - hu}, and the block's
#' \code{bot} parameter is the floor of that curve. An equation without
#' \code{bot} declines towards 0. Only the response block takes level terms on
#' a joint refit, so the plain name is the fallback for a level.
#'
#' @return Draws of the asymptote, or 0.
#'
#' @importFrom brms as_draws_df
#' @noRd
zero_block_asymptote <- function(object, dpar) {
  par <- paste0(dpar, "bot")
  draws <- NULL
  if (inherits(object, "bayesnecjointlevel")) {
    draws <- joint_level_draws(object, par)
  }
  if (is.null(draws)) {
    draws <- as_draws_df(object$fit)[[paste0("b_", par, "_Intercept")]]
  }
  if (is.null(draws)) 0 else as.numeric(draws)
}

#' Refuse a relative estimate of a two-block fit's combined curve
#'
#' @param family The family of the fit.
#' @param fun \code{"ecx"}, whose message names \code{dpar}, or
#' \code{"ecnsec"}, which has no \code{dpar} argument.
#'
#' @details The combined curve is \code{(1 - hu) * E[Y | Y > 0]}, and its
#' floor is the product of the two blocks' asymptotes, not a fitted parameter.
#'
#' @return \code{invisible(NULL)}, called for its side effect.
#'
#' @noRd
check_combined_relative <- function(family, fun = c("ecx", "ecnsec")) {
  fun <- match.arg(fun)
  if (!is_hurdle_family(family)) {
    return(invisible(NULL))
  }
  block <- hurdle_dpar(family)
  remedy <- if (identical(fun, "ecx")) {
    paste0(" Use dpar = \"mu\" or dpar = \"", block, "\" to measure one",
           " block towards its own asymptote, or type = \"absolute\" or",
           " type = \"range\" for the combined curve.")
  } else {
    " Use type = \"absolute\" or type = \"range\"."
  }
  stop("type = \"relative\" is not defined for the combined curve of the ",
       canonical_fam_tag(family), " family, (1 - ", block,
       ") * E[Y | Y > 0], whose asymptote is the product of the two blocks'",
       " asymptotes rather than a fitted parameter.", remedy, call. = FALSE)
}

#' Put a count-hurdle asymptote on the expected-positive scale
#'
#' @param object A \code{bayesnecfit}.
#' @param dpar The requested distributional parameter, if any.
#' @param asymptote Draws of the underlying count-mean asymptote.
#' @param newdata Prediction data used to obtain shape draws.
#'
#' @details The negative binomial zero-truncated mean needs the shape at the
#' asymptote. The function reads a constant shape at the first row of
#' \code{newdata}, and passes any other \code{disp()} term to
#' \code{disp_positive_asymptote()}.
#'
#' @return \code{asymptote}, transformed where the reported curve is
#' \code{E[Y | Y > 0]}.
#'
#' @noRd
count_positive_asymptote <- function(object, dpar, asymptote, newdata) {
  if (all(is.na(asymptote))) {
    return(asymptote)
  }
  if (inherits(object, "bayesmanecfit")) {
    model_set <- names(object$mod_fits)
    sample_size <- min(vapply(
      object$mod_fits, function(x) nrow(as_draws_df(x$fit)), numeric(1)
    ))
    draw_index <- pull_draw_index(object, model_set, sample_size)
    return(unlist(lapply(model_set, function(m) {
      part <- suppressMessages(pull_out(object, model = m))
      part_asymptote <- ecx_asymptote(part, "relative")
      part_asymptote <- count_positive_asymptote(
        part, dpar, part_asymptote, newdata
      )
      idx <- draw_index[[m]]
      if (length(part_asymptote) == 1) {
        rep_len(part_asymptote, length(idx))
      } else {
        part_asymptote[idx]
      }
    })))
  }
  family <- object$fit$family$family
  factorised <- family %in% c("poisson", "negbinomial") &&
    is_factorised_count_formula(object$bayesnecformula, object$fit$family) &&
    is.null(dpar)
  joint <- family %in% c("hurdle_poisson", "hurdle_negbinomial") &&
    !is.null(dpar) &&
    identical(match.arg(dpar, c("mu", hurdle_dpar(object$fit$family))), "mu")
  if (!factorised && !joint) {
    return(asymptote)
  }
  # The asymptote is on the link scale and the predictions on the response
  # scale, so apply linkinv() before converting to E[Y | Y > 0].
  mean_asymptote <- object$fit$family$linkinv(asymptote)
  shape <- NULL
  if (family %in% c("negbinomial", "hurdle_negbinomial")) {
    spec <- parse_disp_term(object$bayesnecformula)
    # disp(~1) fits one shape, so read it at the control, as without disp().
    constant <- is.null(spec) ||
      (spec$route == "A" && length(all.vars(str2lang(spec$value))) == 0)
    if (!constant) {
      return(disp_positive_asymptote(object, spec, mean_asymptote))
    }
    shape <- posterior_epred(
      object, newdata = newdata[1, , drop = FALSE], re_formula = NA,
      dpar = "shape"
    )
  }
  as.numeric(hurdle_positive_mean(mean_asymptote, family, shape))
}

#' A count-hurdle asymptote on the expected-positive scale under disp()
#'
#' @param object A \code{bayesnecfit} with a negative binomial positive block
#' and a \code{disp()} term.
#' @param spec The output of \code{\link{parse_disp_term}}.
#' @param mean_asymptote Draws of the count-mean asymptote on the response
#' scale, or 0 for an equation without \code{bot}.
#'
#' @details Under a variance function of the mean the shape is the sub-model
#' at \code{mu = bot}, draw by draw. Under \code{disp(~x)} the function stops,
#' because the asymptote lies beyond the data and has no single shape.
#'
#' At a mean of 0, for an equation without \code{bot}, the zero-truncated mean
#' is 0/0, and the function returns its limit \code{r / log1p(r)}, where
#' \code{r} is the limit of \code{mu / shape}. \code{r} is 0, giving 1,
#' unless \code{"power"} has \code{c1 >= 1} on a log or softplus shape link.
#'
#' @return A \code{\link[base]{numeric}} vector, one value per draw.
#'
#' @importFrom brms ndraws
#'
#' @noRd
disp_positive_asymptote <- function(object, spec, mean_asymptote) {
  family <- object$fit$family$family
  if (identical(spec$route, "A")) {
    stop("type = \"relative\" measures towards the asymptote of the",
         " positive-count curve, the zero-truncated mean",
         " mu / (1 - (shape / (mu + shape))^shape) at mu = bot, and so needs",
         " the negative binomial shape at the asymptote. The disp(~",
         spec$value, ") term of this ", object$model, " fit models the shape",
         " as a function of ", spec$value, ", and the asymptote, which the",
         " curve reaches only beyond the data, has no single shape. Use",
         " type = \"absolute\" or",
         " type = \"range\", or fit a variance function of the mean, such as",
         " disp(\"power\"), whose shape at the asymptote is the sub-model at",
         " mu = bot.", call. = FALSE)
  }
  n_draws <- ndraws(object$fit)
  mu <- rep_len(mean_asymptote, n_draws)
  shape <- disp_at_mean(object$fit, spec, mu)
  out <- as.numeric(hurdle_positive_mean(mu, family, shape$value))
  zero <- mu == 0
  if (any(zero)) {
    r <- numeric(n_draws)
    if (identical(spec$value, "power") &&
          shape$link %in% c("log", "softplus")) {
      c1 <- shape$pars$c1
      r <- ifelse(c1 < 1, 0, ifelse(c1 > 1, Inf,
                                    exp(shape$centre[["LOGREF"]] -
                                          shape$pars$c0)))
    }
    limit <- ifelse(r == 0, 1, ifelse(is.infinite(r), Inf, r / log1p(r)))
    out[zero] <- limit[zero]
  }
  out
}

#' The model-averaged theoretical asymptote
#'
#' Each equation's own asymptote, its \code{bot} draws or 0 where it has no
#' \code{bot}, thinned to the draws the weighting keeps and concatenated in the
#' order the model-averaged posterior concatenates them.
#'
#' The order must match \code{posterior_epred.bayesmanecfit()} exactly, or
#' element \emph{i} of the asymptote and row \emph{i} of the curve belong to
#' different draws. Both take the order from \code{names(mod_fits)} and the
#' draws from \code{pull_draw_index()}.
#'
#' An equation that refuses \code{"relative"} refuses here too.
#'
#' @param object An object of class \code{\link{bayesmanecfit}}.
#'
#' @return A \code{\link[base]{numeric}} vector, one asymptote per draw of the
#' model-averaged posterior.
#'
#' @importFrom brms as_draws_df
#' @noRd
manec_asymptote <- function(object) {
  model_set <- names(object$mod_fits)
  sample_size <- min(vapply(object$mod_fits,
                            function(x) nrow(as_draws_df(x$fit)), numeric(1)))
  draw_index <- pull_draw_index(object, model_set, sample_size)
  unlist(lapply(model_set, function(m) {
    part <- suppressMessages(pull_out(object, model = m))
    a <- ecx_asymptote(part, "relative")
    idx <- draw_index[[m]]
    if (length(a) == 1) rep_len(a, length(idx)) else a[idx]
  }))
}

#' The x value at which each draw reaches its ECx target
#'
#' The reference for every type is the control, this draw's predicted mean at
#' the lowest observed concentration.
#'
#' @noRd
ecx_from_posterior <- function(p_samples, x_vec, ecx_val, type, control,
                               asymptote) {
  n_draws <- nrow(p_samples)
  control <- rep_len(control, n_draws)
  asymptote <- rep_len(asymptote, n_draws)
  # Record which end of the range an NA comes from, as nsec_from_posterior()
  # does: the two ends are opposite statements about the estimate. A curve can
  # already be below the target where the grid begins when x_range starts above
  # the lowest observed concentration, where the control is read.
  below_range <- logical(n_draws)
  out <- vapply(seq_len(n_draws), function(i) {
    y <- p_samples[i, ]
    target <- switch(
      type,
      absolute = control[i] * (1 - ecx_val / 100),
      relative = control[i] - (control[i] - asymptote[i]) * (ecx_val / 100),
      range = control[i] -
        (control[i] - min(y, na.rm = TRUE)) * (ecx_val / 100),
      direct = ecx_val
    )
    val <- crossing_x(y, target, x_vec)
    # With no crossing, mark a draw below range only if its first grid response
    # is at or below a target below the control. Classify every other missing
    # crossing at the upper end, whether outside the grid or absent altogether.
    if (is.na(val) && !is.na(y[1]) && !is.na(target) && y[1] <= target &&
        target < control[i]) {
      below_range[i] <<- TRUE
    }
    val
  }, numeric(1))
  attr(out, "below_range") <- below_range
  out
}
