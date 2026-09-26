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
#'
#' @details Draws are paired by row. That is valid precisely because the two
#' posteriors are independent -- the hurdle likelihood factorises and the
#' components share no parameters -- so any pairing represents a draw from the
#' joint posterior. Where the two fits carry different numbers of draws (model
#' averaging harmonises sample sizes within, but not between, fits) both are
#' truncated to the smaller.
#'
#' @return A \code{\link[base]{list}} with elements \code{x}, \code{growth},
#' \code{survival} and \code{combined}, and \code{control}, a list of the same
#' three names holding one control value per draw.
#'
#' @details The control is the prediction at the lowest observed concentration,
#' pinned to the same concentration on both sides, rather than the first column
#' of the grid. Computed here rather than in each caller so that the control and
#' the curve are truncated to the same draws by the same operation; reading the
#' control off a differently truncated matrix would pair a draw's curve with
#' another draw's reference. This is D15 ruling 2 applied to the two-block
#' class: without it, supplying \code{x_range} changes every reported ECx and
#' NSEC.
#'
#' @importFrom brms posterior_epred
#'
#' @noRd
hurdle_component_preds <- function(object, resolution = 1000, x_range = NA) {
  # Default to the survival fit's predictor range, not the growth fit's. The
  # growth fit is built on survivors only, so it does not see concentrations
  # where nothing lived -- exactly the upper end the combined endpoint needs.
  # Growth is therefore extrapolated over that stretch, which is harmless in
  # the product because survival there is ~0, but it is why the range comes
  # from the survival side.
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
#' \code{pmin(ne_growth, ne_survival)} evaluated per posterior draw. Below both
#' thresholds the growth curve sits at \code{top} and the survival curve at its
#' own control value, so their product is flat; it departs that plateau at
#' whichever threshold binds first.
#'
#' A component draw beyond the top of its prediction range is known only to
#' exceed that limit. The two components' ranges can differ, because growth
#' is fitted to survivors only and its range stops short of any
#' concentration at which nothing survived. Where one component is known
#' only to exceed its limit and the other is estimated above that limit, the
#' combined draw is not identified. It is reported as beyond the top of the
#' range, at the smaller of the two components' upper limits, which is true
#' of every such draw. At the foot of the range the larger of the two lower
#' limits is reported. Where the lower limits differ, a combined draw
#' estimated below the larger of them is reported as below that limit, so that
#' it is ranked consistently with the draws known only to lie below it.
#'
#' \bold{The combined estimate is therefore the smaller of the two, and reduces
#' to the growth estimate whenever growth is the more sensitive endpoint} --
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
#' \code{summary} labels each component. \code{\link{nsec}} with
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
nec.bayesnechurdlefit <- function(object, posterior = FALSE, xform = identity,
                                  prob_vals = c(0.5, 0.025, 0.975),
                                  extrapolate = FALSE,
                                  which = "combined", ...) {
  check_component_arg(list(...), object)
  check_removed_args(list(...))
  chk_logical(posterior)
  which <- hurdle_check_which(which)
  if (!inherits(xform, "function")) {
    stop("xform must be a function.")
  }
  # The component reports are muffled and one is raised below for the vector
  # this method actually returns. Left on, a censored component was reported by
  # each of the calls here and again by the report below, three times over for
  # the combined value.
  # Passed to each component rather than resolved here: the two components are
  # separate fits with prediction ranges of their own, and combine_censored_min
  # below already reduces the two records to the bound true of both. Each
  # refusal is re-raised under the name of the component that raised it,
  # because the growth fit stops short of the survival fit wherever nothing
  # survived at the top concentrations, so a limit between the two ranges is an
  # extension for one component and inside the range for the other, and the
  # error otherwise names a range without saying whose.
  component_nec <- function(part, what) {
    call_it <- function() {
      without_censored_warning(
        nec(part, posterior = TRUE, extrapolate = extrapolate, ...)
      )
    }
    # Only where there is something to attribute. Relabelling every error on
    # this path reported "the growth component refused extrapolate: nec is not
    # a parameter in ecx model types" for a call that had named no extrapolate
    # at all. tryCatch() rather than withCallingHandlers(), because this
    # replaces the condition rather than running beside it.
    if (identical(extrapolate, FALSE)) {
      return(call_it())
    }
    tryCatch(call_it(), error = function(e) {
      stop("The ", what, " component refused extrapolate: ",
           conditionMessage(e), call. = FALSE)
    })
  }
  g_post <- component_nec(object$growth, "growth")
  s_post <- component_nec(object$survival, "survival")
  if (which == "growth") {
    out <- unlist(g_post)
    cens <- attr(g_post, "censored")
  } else if (which == "survival") {
    out <- unlist(s_post)
    cens <- attr(s_post, "censored")
  } else {
    n <- min(length(g_post), length(s_post))
    combined <- combine_censored_min(g_post, s_post, n)
    out <- combined$values
    cens <- combined$censored
    g_type <- attr(without_censored_warning(nec(object$growth)),
                   "toxicity_estimate")
    s_type <- attr(without_censored_warning(nec(object$survival)),
                   "toxicity_estimate")
    if (!identical(g_type, "nec") || !identical(s_type, "nec")) {
      message("At least one component is an ecx-type (NSEC) estimate, so the",
              " combined value is approximate. See ?nec.bayesnechurdlefit.")
    }
  }
  if (inherits(xform, "function")) {
    out <- xform(out)
    cens <- xform_censoring(cens, xform)
  }
  attr(out, "censored") <- cens
  # The report and the summary below: either component may be an ecx-type fit
  # whose no-effect estimate is read off the curve, and such a draw lies beyond
  # the prediction range rather than having no value. It keeps its rank in the
  # summary and is given no number. See #39 and D15 ruling 3 for why it is not
  # given the bound as a value instead.
  warn_censored_draws(out, "no-effect estimate", cens = cens)
  estimate <- summarise_censored(out, prob_vals, cens)
  names(estimate) <- clean_names(estimate)
  attr(estimate, "toxicity_estimate") <- "nec"
  attr(out, "toxicity_estimate") <- "nec"
  attr(estimate, "component") <- which
  if (!posterior) estimate else out
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
                                  x_range = NA, xform = identity,
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
    # A two-block fit has no single bot parameter to measure towards: the
    # combined endpoint is mu * (1 - hu) and its asymptote is a product of two
    # equations rather than a fitted quantity. "range" measures towards the
    # lowest predicted response and is defined here; "absolute" measures
    # towards 0. See D15 ruling 6.
    stop("type = \"relative\" is not defined for a hurdle fit, whose ",
         "asymptote is not a single fitted parameter. Use ",
         "type = \"absolute\" or type = \"range\".", call. = FALSE)
  }
  if (!inherits(xform, "function")) {
    stop("xform must be a function.")
  }
  preds <- hurdle_component_preds(object, resolution = resolution,
                                  x_range = x_range)
  p_samples <- preds[[which]]
  # The control comes from hurdle_component_preds(), which reads it at the
  # lowest observed concentration. Taking p_samples[, 1] instead made every
  # estimate a function of x_range, which is what D15 ruling 2 removes for the
  # single-fit class.
  control <- preds$control[[which]]
  out <- ecx_from_posterior(p_samples, preds$x, ecx_val, type, control,
                            NA_real_)
  below <- attr(out, "below_range")
  above <- is.na(out) & !below
  attr(out, "below_range") <- NULL
  # Put the estimate back on the fitted scale, matching ecx.bayesnecfit. The
  # record is built on the recorded grid, the scale the curve was searched on,
  # and remapped onto the fitted scale, which swaps the two ends under a
  # decreasing crf(). See the same construction in ecx.bayesnecfit.
  out <- sub_x_transformation(out, object$formula)
  x_kept <- preds$x[is.finite(preds$x)]
  cens <- xform_censoring(
    censoring_record(max(x_kept), min(x_kept), above, below),
    function(value) sub_x_transformation(value, object$formula)
  )
  if (inherits(xform, "function")) {
    out <- xform(out)
    cens <- xform_censoring(cens, xform)
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
#' Below both thresholds the combined endpoint is flat, so it leaves that
#' plateau at whichever component binds first and the combined estimate is the
#' minimum of the two. A component draw that lies beyond an end of its
#' prediction range has no value to take a minimum with, so it enters the
#' comparison at the end it is known to be beyond and nothing else. Below the
#' foot of its range it is the smaller whatever the other draw is. Above the
#' top of its range, at \code{U}, it is known only to exceed \code{U}: the
#' minimum is the other draw where that draw is identified at or below
#' \code{U}, and is otherwise known only to exceed \code{U} as well.
#'
#' The two components need not share a range. Growth is fitted to survivors
#' only, so its grid stops short of any concentration at which nothing
#' survived (\code{hurdle_summary_range()}), and a survival draw identified
#' at 20 is not the minimum when growth is known only to exceed 10 (#415).
#' The record holds one limit at each end, the smaller upper limit and the
#' larger lower one. Where the lower limits differ, a minimum identified below
#' the larger of them is marked as below it, which keeps every identified
#' value inside the recorded limits.
#'
#' \code{pmin()} on the raw vectors cannot do this. It propagates the
#' \code{NA} that an ecx-type component returns for a beyond-range draw, so a
#' draw whose growth NSEC was above the range and whose survival NEC was well
#' inside it came back as no estimate at all, and was then deleted from the
#' summary.
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
  # A draw censored above its component's limit U, compared with a draw of the
  # other component identified above U. The minimum then lies between U and
  # that value and is not identified. Placing the censored draw at Inf alone,
  # as below, returned the identified value as the minimum: growth known only
  # to exceed 10 against survival at 20 gave 20 as an exact combined
  # threshold (#415). The comparison is strict, because an identified value at
  # U itself is the minimum whatever the censored draw is. Both directions are
  # tested, so that the result does not depend on which component is growth.
  # %in% TRUE drops an identified draw whose value is NA, which pmin() below
  # leaves unexplained, as it did before.
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
  # One bound per posterior, because the record holds one (D20): the smallest
  # upper limit and the largest lower one, as in concat_censoring(). A draw
  # marked above exceeds its own component's upper limit, so it exceeds the
  # smaller of the two as well; that is weaker than the per-draw truth where
  # the limits differ, and never false. An interval record holding both
  # limits was not adopted, because it would change the record and every
  # reader of it.
  upper <- min(g_cens$upper, s_cens$upper)
  lower <- max(g_cens$lower, s_cens$lower)
  # summarise_censored() ranks every draw marked below lower than every
  # identified draw, and every draw marked above higher. That is true only
  # where each identified value lies inside the recorded limits. The
  # comparison above ensures it at the top. At the foot, where the two lower
  # limits differ, a minimum identified below the larger limit was left
  # unmarked, although a draw marked below that limit may lie above it. With
  # growth on 5 to 10 at 7 and survival on 0 to 10 at 3 in one draw, and
  # growth below 5 with survival at 8 in the other, the 97.5 per cent entry
  # was an unmarked 3 where the second draw may be 4. D20's rule is therefore
  # applied at the foot as well: such a minimum is marked below the larger
  # limit, which is true of it and weaker than its value. Only where the
  # limits differ, so that a posterior whose components share a foot is
  # unchanged. The feet differ only where nothing survived at the foot of the
  # fitted range, or where one component has no record.
  if (g_cens$lower != s_cens$lower) {
    out[is.finite(out) & out < lower] <- -Inf
  }
  above <- is.infinite(out) & out > 0
  below <- is.infinite(out) & out < 0
  out[above | below] <- NA_real_
  combined <- censoring_record(upper, lower, above, below)
  # As in concat_censoring(): the two blocks of one fit share a formula and so
  # agree on whether the predictor was reversed.
  attr(combined, "swapped") <- isTRUE(attr(attr(g, "censored"), "swapped")) ||
    isTRUE(attr(attr(s, "censored"), "swapped"))
  list(values = out, censored = combined)
}
