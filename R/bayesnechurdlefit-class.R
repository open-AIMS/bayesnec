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
#' @details The combined no-effect concentration is
#' \code{pmin(ne_growth, ne_survival)} evaluated per posterior draw. Below both
#' thresholds the growth curve sits at \code{top} and the survival curve at its
#' own control value, so their product is flat; it departs that plateau at
#' whichever threshold binds first.
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
  g_post <- without_censored_warning(unlist(nec(object$growth,
                                                posterior = TRUE)))
  s_post <- without_censored_warning(unlist(nec(object$survival,
                                                posterior = TRUE)))
  if (which == "growth") {
    out <- g_post
  } else if (which == "survival") {
    out <- s_post
  } else {
    n <- min(length(g_post), length(s_post))
    out <- pmin(g_post[seq_len(n)], s_post[seq_len(n)])
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
  }
  # na.rm and the report above it: either component may be an ecx-type fit
  # whose no-effect estimate is read off the curve, and such a draw is NA where
  # the curve never reaches the reference. pmin() propagates that into the
  # combined value. See #39 and D15 ruling 3.
  warn_censored_draws(out, "no-effect estimate")
  estimate <- quantile(out, probs = prob_vals, na.rm = TRUE)
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
  # Put the estimate back on the fitted scale, matching ecx.bayesnecfit.
  out <- sub_x_transformation(out, object$formula)
  warn_censored_draws(out, paste0("ECx", ecx_val))
  if (inherits(xform, "function")) {
    out <- xform(out)
  }
  estimate <- quantile(out, probs = prob_vals, na.rm = TRUE)
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
