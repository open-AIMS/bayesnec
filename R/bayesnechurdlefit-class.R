#' Class \code{bayesnechurdlefit} of models fitted with \code{\link{bnec_hurdle}}
#'
#' A pair of ordinary \pkg{bayesnec} fits describing the two components of a
#' hurdle concentration-response model, held together so that the combined
#' endpoint can be derived from them.
#'
#' @name bayesnechurdlefit
#' @docType class
#'
#' @details See \code{methods(class = "bayesnechurdlefit")} for an overview of
#' available methods.
#'
#' @slot growth An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} for the response of survivors.
#' @slot survival An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} for the probability of survival.
#' @slot data The \code{\link[base]{data.frame}} of all exposed individuals.
#' @slot formula The \code{\link{bayesnecformula}} supplied by the user.
#' @slot y_var The response variable name.
#' @slot n_exposed Number of individuals that entered the experiment.
#' @slot n_dead Number recorded as dead, i.e. response equal to zero.
#'
#' @seealso \code{\link{bnec_hurdle}}, \code{\link{bnec}}
NULL

#' @noRd
is_bayesnechurdlefit <- function(x) {
  inherits(x, "bayesnechurdlefit")
}

#' Aligned component predictions from a hurdle fit
#'
#' Returns growth and survival posterior predictions on a common predictor
#' grid, with equal numbers of draws so that they can be multiplied row-wise.
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#' @param resolution The number of unique predictor values to predict over.
#' @param x_range A range of predictor values to predict over.
#' @details Draws are paired by row, which is valid because the two posteriors
#' are independent. Where the fits hold different numbers of draws, both are
#' truncated to the smaller.
#'
#' The control is the prediction at the lowest observed concentration,
#' not the first column of the grid, so that \code{x_range} does not change
#' the reference. It is computed here so that the control and the curve are
#' truncated to the same draws.
#'
#' @return A \code{\link[base]{list}} with elements \code{x}, \code{growth},
#' \code{survival} and \code{combined}, and \code{control}, a list of the same
#' three names holding one control value per draw.
#'
#' @importFrom brms posterior_epred
#'
#' @noRd
hurdle_component_preds <- function(object, resolution = 1000, x_range = NA) {
  # Default every curve to the survival fit's range, the whole concentration
  # series. The growth data stop at the last survivor, and an older fit stores
  # a growth grid that stops there too.
  if (any(is.na(x_range))) {
    nd_s <- newdata_eval(object$survival, resolution = resolution,
                         x_range = NA)
    x_range <- range(nd_s$x_vec)
  } else {
    nd_s <- newdata_eval(object$survival, resolution = resolution,
                         x_range = x_range)
  }
  # The survival fit carries a different response column (.alive), so each
  # component's newdata must be built from its own model frame.
  nd_g <- newdata_eval(object$growth, resolution = resolution,
                       x_range = x_range)
  g <- posterior_epred(object$growth, newdata = nd_g$newdata, re_formula = NA)
  s <- posterior_epred(object$survival, newdata = nd_s$newdata,
                       re_formula = NA)
  x_at <- control_x(object)
  epred_part <- function(part) {
    function(nd) posterior_epred(part, newdata = nd, re_formula = NA)
  }
  c_g <- control_posterior(object$growth, nd_g$newdata, epred_part(object$growth),
                           x_at = x_at)
  c_s <- control_posterior(object$survival, nd_s$newdata,
                           epred_part(object$survival), x_at = x_at)
  n <- min(nrow(g), nrow(s))
  g <- g[seq_len(n), , drop = FALSE]
  s <- s[seq_len(n), , drop = FALSE]
  c_g <- c_g[seq_len(n)]
  c_s <- c_s[seq_len(n)]
  list(x = nd_g$x_vec, growth = g, survival = s, combined = g * s,
       control = list(growth = c_g, survival = c_s, combined = c_g * c_s))
}

#' @noRd
hurdle_check_which <- function(which) {
  which <- match.arg(which, c("combined", "growth", "survival"))
  which
}

#' Extracts the predicted NEC value from a \code{\link{bayesnechurdlefit}}
#'
#' @inheritParams nec
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#' @param which Which component to return: \code{"combined"} (the default),
#' \code{"growth"} or \code{"survival"}.
#'
#' @details \code{extrapolate} is passed to each component and measured
#' against that component's own prediction range. The growth component stops
#' short of any concentration where nothing survived, so its range can end
#' below the survival component's, and a limit between the two is an extension
#' for one and inside the range for the other. Such a limit is refused, and
#' the refusal names which component raised it.
#'
#' The combined no-effect concentration is
#' \code{pmin(ne_growth, ne_survival)} evaluated per posterior draw, on the
#' scale the model was fitted on. Below both thresholds the growth curve sits
#' at \code{top} and the survival curve at its own control value, so their
#' product is flat; it departs that plateau at whichever threshold binds first.
#' The minimum, and the rule for censored draws below, apply on that scale, and
#' the result is then returned on the scale \code{xform} names. Under a
#' decreasing transformation in \code{crf()}, such as \code{crf(I(-x))}, each
#' curve is flat above its threshold on the recorded scale, so the combined
#' value is the larger of the two recorded thresholds. An \code{xform} changes
#' the scale the value is reported on, not which threshold it is.
#'
#' A component draw beyond the top of its prediction range is known only to
#' exceed that limit. Both components are predicted over the whole
#' concentration series, so they share their limits, unless a fit made before
#' that rule stores a growth range ending at the last concentration with
#' survivors. Where the limits differ and one component is known only to
#' exceed its limit while the other is estimated above that limit, the
#' combined draw is not identified. It is reported as beyond the top of the
#' range, at the smaller of the two components' upper limits, which is true
#' of every such draw.
#'
#' \bold{The combined estimate therefore reduces to the growth estimate
#' whenever growth is the more sensitive endpoint} --
#' which it usually is, since a contaminant that kills has generally slowed
#' growth at a lower concentration first. This is not a defect of the
#' combination but a property of thresholds: a threshold marks where an effect
#' \emph{begins}, and the combined effect begins as soon as either process
#' does. Where the two components differ is in the \emph{magnitude} of effect
#' above that point, which is what \code{\link{ecx}} measures. Use
#' \code{ecx(which = "combined")} rather than this function where the question
#' is what the hurdle model adds over a survivors-only analysis.
#'
#' As for \code{\link{nec}} on any fit, what is returned is a NEC only where
#' the underlying model(s) are threshold models. A component whose model set
#' contains smooth (\code{ecx}-type) models contributes NSEC draws instead, and
#' the combination is then an N(S)EC; a message is emitted in that case and
#' \code{summary} labels each component. A component fitted to a single smooth
#' equation, such as \code{ecx4param}, contributes the NSEC draws stored when it
#' was fitted, although \code{nec()} refuses that equation fitted on its own
#' with \code{\link{bnec}}. Only the component or components that \code{which}
#' names are computed, so \code{which = "growth"} reads nothing from the
#' survival component. \code{\link{nsec}} with
#' \code{which = "combined"} is the alternative that reads a single value off
#' the combined curve itself rather than taking the minimum of two component
#' estimates, and it does not reduce to the growth value.
#'
#' @return A vector containing the estimated no-effect value, including upper
#' and lower credible interval bounds.
#'
#' @importFrom stats quantile
#' @importFrom chk chk_logical
#'
#' @method nec bayesnechurdlefit
#'
#' @export
nec.bayesnechurdlefit <- function(object, posterior = FALSE, xform = NULL,
                                  prob_vals = c(0.5, 0.025, 0.975),
                                  extrapolate = FALSE,
                                  which = "combined", ...) {
  check_component_arg(list(...), object)
  check_removed_args(list(...))
  check_no_effect_arg(list(...), object)
  chk_logical(posterior)
  which <- hurdle_check_which(which)
  check_xform(xform)
  # Muffle the component censoring warnings; one is raised below for the
  # vector returned. `extrapolate` goes to each component, which has its own
  # prediction range, and a refusal is re-raised naming that component, since
  # an older fit's growth range can stop short of the survival range.
  component_nec <- function(part, what) {
    call_it <- function() {
      without_censored_warning(
        hurdle_component_ne(part, extrapolate = extrapolate, ...)
      )
    }
    # Relabel an error only where extrapolate was given; otherwise an unrelated
    # error would be blamed on it.
    if (identical(extrapolate, FALSE)) {
      return(call_it())
    }
    tryCatch(call_it(), error = function(e) {
      stop("The ", what, " component refused extrapolate: ",
           conditionMessage(e), call. = FALSE)
    })
  }
  # Compute only what `which` names, so that the other component can neither
  # print a message nor stop the call.
  if (which == "growth") {
    g_post <- component_nec(object$growth, "growth")
    out <- unlist(g_post)
    cens <- attr(g_post, "censored")
  } else if (which == "survival") {
    s_post <- component_nec(object$survival, "survival")
    out <- unlist(s_post)
    cens <- attr(s_post, "censored")
  } else {
    g_post <- component_nec(object$growth, "growth")
    s_post <- component_nec(object$survival, "survival")
    n <- min(length(g_post), length(s_post))
    # Keep the minimum on the fitted scale, where each curve is flat below its
    # threshold. Under a decreasing crf() it is the larger recorded threshold,
    # which is where the product leaves its plateau.
    combined <- combine_censored_min(g_post, s_post, n)
    out <- combined$values
    cens <- combined$censored
    # Read each component's kind from ne_label(), not the "toxicity_estimate"
    # attribute, which is "nec" whatever the equation. Match the NSEC labels
    # only: a fit that recorded no ne_type has the label "no-effect estimate".
    kinds <- c(ne_label(object$growth), ne_label(object$survival))
    if (any(kinds %in% c("NSEC", "N(S)EC"))) {
      message("At least one component is an ecx-type (NSEC) estimate, so the",
              " combined value is approximate. See ?nec.bayesnechurdlefit.")
    }
  }
  # The components return the fitted scale, and the map is applied once here.
  map <- recorded_xform(object, xform, extrapolate)
  out <- map_draws(out, map, cens, "no-effect estimate")
  cens <- xform_censoring(cens, map)
  attr(out, "censored") <- cens
  # A draw beyond the prediction range keeps its rank in the summary and is
  # given no number, rather than the bound as its value.
  warn_censored_draws(out, "no-effect estimate", cens = cens)
  estimate <- summarise_censored(out, prob_vals, cens)
  names(estimate) <- clean_names(estimate)
  attr(estimate, "toxicity_estimate") <- "nec"
  attr(out, "toxicity_estimate") <- "nec"
  attr(estimate, "component") <- which
  if (!posterior) estimate else out
}

#' The no-effect draws one hurdle component contributes
#'
#' A component fitted to one smooth equation has no \code{nec} parameter, so
#' \code{nec()} refuses it. It contributes its stored NSEC draws instead, as an
#' equation of a model set does.
#'
#' @param part The \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}
#' of one component.
#' @param extrapolate As for \code{\link{nec}}.
#' @param ... Passed to \code{\link{nec}}.
#'
#' @return The posterior on the fitted scale, as
#' \code{nec(part, posterior = TRUE, xform = identity)} returns it.
#' @noRd
hurdle_component_ne <- function(part, extrapolate, ...) {
  if (inherits(part, "bayesnecfit") &&
        !has_nec_parameter(part$model, part$fit)) {
    return(necfit_ne(part, posterior = TRUE, xform = identity,
                     extrapolate = extrapolate, ...))
  }
  nec(part, posterior = TRUE, xform = identity, extrapolate = extrapolate,
      ...)
}

#' Extracts the predicted ECx value from a \code{\link{bayesnechurdlefit}}
#'
#' @inheritParams ecx
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#' @param which Which component to return: \code{"combined"} (the default),
#' \code{"growth"} or \code{"survival"}.
#'
#' @details The combined curve is the product of the growth curve and the
#' survival curve. Because both decline, the combined ECx is always reached at
#' or below either component's own ECx.
#'
#' Without \code{x_range}, every ECx is read over the whole concentration
#' series, the range the survival component is fitted over. The growth
#' component is fitted to survivors only, so it has no observations above the
#' highest concentration at which anything survived. Those concentrations were
#' tested, so a growth ECx that falls there is reported as a value, read off
#' the growth curve, and its credible interval reflects that the curve there is
#' constrained by the data at lower concentrations. A supplied \code{x_range}
#' applies to all three curves alike. \code{\link{summary.bayesnechurdlefit}},
#' \code{\link{nsec.bayesnechurdlefit}},
#' \code{\link{ecnsec.bayesnechurdlefit}}, the plots and
#' \code{\link{posterior_epred.bayesnechurdlefit}} use the same range.
#'
#' @return A vector containing the estimated ECx value, including upper and
#' lower credible interval bounds.
#'
#' @importFrom stats quantile
#' @importFrom chk chk_logical chk_numeric
#'
#' @method ecx bayesnechurdlefit
#'
#' @export
ecx.bayesnechurdlefit <- function(object, ecx_val = 10, resolution = 200,
                                  posterior = FALSE, type = "absolute",
                                  x_range = NA, xform = NULL,
                                  prob_vals = c(0.5, 0.025, 0.975),
                                  which = "combined", ...) {
  check_component_arg(list(...), object)
  check_removed_args(list(...))
  chk_numeric(ecx_val)
  chk_numeric(resolution)
  chk_logical(posterior)
  which <- hurdle_check_which(which)
  type <- validate_ecx_type(type, match.call())
  if (identical(type, "relative")) {
    # The combined endpoint, mu * (1 - hu), has no single bot parameter for
    # "relative" to measure towards.
    stop("type = \"relative\" is not defined for a hurdle fit, whose ",
         "asymptote is not a single fitted parameter. Use ",
         "type = \"absolute\" or type = \"range\".", call. = FALSE)
  }
  check_xform(xform)
  # Every curve, growth included, over the whole concentration series.
  preds <- hurdle_component_preds(object, resolution = resolution,
                                  x_range = x_range)
  p_samples <- preds[[which]]
  # Take the control from hurdle_component_preds(), at the lowest observed
  # concentration. p_samples[, 1] would make every estimate depend on x_range.
  control <- preds$control[[which]]
  out <- ecx_from_posterior(p_samples, preds$x, ecx_val, type, control,
                            NA_real_)
  below <- attr(out, "below_range")
  above <- is.na(out) & !below
  attr(out, "below_range") <- NULL
  # As in ecx.bayesnecfit(). Build the record on the recorded grid, then remap
  # it: a decreasing crf() swaps its two ends.
  x_kept <- preds$x[is.finite(preds$x)]
  cens <- censoring_record(max(x_kept), min(x_kept), above, below)
  if (!returns_recorded(object, xform)) {
    to_fitted <- function(value) sub_x_transformation(value, object$formula)
    out <- to_fitted(out)
    cens <- xform_censoring(cens, to_fitted)
    if (!is.null(xform)) {
      out <- xform(out)
      cens <- xform_censoring(cens, xform)
    }
  }
  attr(out, "censored") <- cens
  # After xform, as in ecx.bayesnecfit: the bound the report names and the
  # numbers the caller is about to read are then on one scale.
  warn_censored_draws(out, paste0("ECx", ecx_val), cens = cens)
  estimate <- summarise_censored(out, prob_vals, cens)
  names(estimate) <- clean_names(estimate)
  attr(estimate, "ecx_val") <- ecx_val
  attr(estimate, "resolution") <- resolution
  attr(estimate, "toxicity_estimate") <- "ecx"
  attr(estimate, "component") <- which
  attr(out, "ecx_val") <- ecx_val
  attr(out, "toxicity_estimate") <- "ecx"
  if (!posterior) estimate else out
}

#' Crossed model weights for a \code{\link{bayesnechurdlefit}}
#'
#' Returns weights over every combination of the growth and survival model sets.
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#'
#' @details Because the hurdle likelihood factorises, the expected log
#' predictive density of a crossed model is the sum of its components'
#' (\code{elpd(a, b) = elpd_growth(a) + elpd_survival(b)}). Under pseudo-BMA,
#' \code{w_ab} is proportional to \code{exp(elpd_a + elpd_b)}, so the crossed
#' weights are exactly the outer product of the two components' weights. All
#' \code{n_growth * n_survival} combinations are therefore available from the
#' two fits alone.
#'
#' Note this identity is specific to pseudo-BMA. Stacking optimises a different
#' objective whose solution is not generally an outer product; obtaining stacked
#' crossed weights requires building the full pointwise matrix and is not done
#' here.
#'
#' @return A \code{\link[base]{matrix}} of weights, growth models in rows and
#' survival models in columns.
#'
#' @export
crossed_weights <- function(object) {
  if (!is_bayesnechurdlefit(object)) {
    stop("crossed_weights requires an object of class bayesnechurdlefit.")
  }
  wt <- function(x) {
    if (inherits(x, "bayesmanecfit")) {
      x$mod_stats$wi
    } else {
      stats::setNames(1, x$model)
    }
  }
  nm <- function(x) {
    if (inherits(x, "bayesmanecfit")) rownames(x$mod_stats) else x$model
  }
  w_g <- wt(object$growth)
  w_s <- wt(object$survival)
  out <- outer(as.vector(w_g), as.vector(w_s))
  dimnames(out) <- list(growth = nm(object$growth),
                        survival = nm(object$survival))
  out
}

#' @noRd
#' @method print bayesnechurdlefit
#' @export
print.bayesnechurdlefit <- function(x, ...) {
  cat("Object of class bayesnechurdlefit\n\n")
  cat(" ", x$n_exposed, "individuals exposed;", x$n_dead, "recorded dead (",
      sprintf("%.1f%%", 100 * x$n_dead / x$n_exposed), ")\n\n")
  mods <- function(f) {
    if (inherits(f, "bayesmanecfit")) {
      paste0(length(f$mod_fits), " models: ",
             paste0(names(f$mod_fits), collapse = ", "))
    } else {
      f$model
    }
  }
  cat("  growth   (survivors) :", mods(x$growth), "\n")
  cat("  survival (all)       :", mods(x$survival), "\n\n")
  cat("Combined endpoint = growth * survival. Use nec(), ecx() with",
      "which = \"combined\" (default),\n\"growth\" or \"survival\";",
      "crossed_weights() for the full crossed model weights.\n")
  invisible(x)
}

#' Take the smaller of two component no-effect posteriors, keeping censoring
#'
#' Takes the minimum of the component no-effect draws, which must be on the
#' fitted scale, not the recorded one. Censoring flags and
#' upper limits decide whether an outside-range minimum is identified;
#' plain \code{pmin()} would propagate \code{NA} for a censored component draw.
#'
#' @param g,s The two component posteriors, each carrying its \code{"censored"}
#' record where it has one.
#' @param n The number of draws to combine.
#'
#' @return A \code{\link[base]{list}} with elements \code{values} and
#' \code{censored}.
#' @noRd
combine_censored_min <- function(g, s, n) {
  idx <- seq_len(n)
  g_cens <- subset_censoring(attr(g, "censored"), idx)
  s_cens <- subset_censoring(attr(s, "censored"), idx)
  g_v <- as.numeric(g)[idx]
  s_v <- as.numeric(s)[idx]
  if (is.null(g_cens) && is.null(s_cens)) {
    return(list(values = pmin(g_v, s_v), censored = NULL))
  }
  blank <- list(above = logical(n), below = logical(n),
                upper = Inf, lower = -Inf)
  g_cens <- if (is.null(g_cens)) blank else g_cens
  s_cens <- if (is.null(s_cens)) blank else s_cens
  # A draw censored above U against an identified draw above U: the minimum
  # lies between U and that value, so it is not identified. Setting the
  # censored draw to Inf alone would return the identified value. The test is
  # strict, because an identified value at U is the minimum. %in% TRUE leaves
  # an identified NA to pmin() below.
  g_known <- !g_cens$above & !g_cens$below
  s_known <- !s_cens$above & !s_cens$below
  unresolved <- ((g_cens$above & s_known & s_v > g_cens$upper) |
                   (s_cens$above & g_known & g_v > s_cens$upper)) %in% TRUE
  g_v[g_cens$above] <- Inf
  g_v[g_cens$below] <- -Inf
  s_v[s_cens$above] <- Inf
  s_v[s_cens$below] <- -Inf
  out <- pmin(g_v, s_v)
  out[unresolved] <- Inf
  above <- is.infinite(out) & out > 0
  below <- is.infinite(out) & out < 0
  out[above | below] <- NA_real_
  # The record holds one bound per posterior: the smallest upper limit and the
  # largest lower one, as in concat_censoring(). Both are true of every
  # censored draw. The test above keeps every identified value at or below the
  # upper limit, which summarise_censored() assumes. Where the two feet
  # differ, an identified value can lie below the lower limit; correcting that
  # belongs in the summary, not in this record.
  combined <- censoring_record(min(g_cens$upper, s_cens$upper),
                               max(g_cens$lower, s_cens$lower),
                               above, below)
  # As in concat_censoring(): the two blocks of one fit share a formula and so
  # agree on whether the predictor was reversed.
  attr(combined, "swapped") <- isTRUE(attr(attr(g, "censored"), "swapped")) ||
    isTRUE(attr(attr(s, "censored"), "swapped"))
  list(values = out, censored = combined)
}
