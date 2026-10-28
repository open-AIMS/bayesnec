# Methods for bayesnechurdlefit, the paired object returned by bnec_hurdle().
#
# Most methods take `which`, defaulting to "combined", to select one of the
# three curves. Methods that describe a curve compute it from both components;
# methods that describe a fit, such as diagnostics and priors, return one
# result per component.

#' @noRd
hurdle_check_which <- function(which) {
  match.arg(which, c("combined", "growth", "survival"))
}

#' Message used when a method has no combined analogue
#'
#' @noRd
hurdle_no_combined <- function(what) {
  paste0(what, " describes a single model fit, and the combined endpoint is",
         " not one -- it is derived from two. Returning one element per",
         " component.")
}

#' Generates posterior predictions for a \code{\link{bayesnechurdlefit}}
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#' @param which Which curve to return: \code{"combined"} (the default),
#' \code{"growth"} or \code{"survival"}.
#' @param resolution The number of unique predictor values to predict over.
#' @param x_range A range of predictor values to predict over.
#' @param newdata An optional \code{\link[base]{data.frame}} of predictor
#' values. When supplied, \code{resolution} and \code{x_range} are ignored.
#' @param ... Unused.
#'
#' @details The combined curve is the product of the growth curve and the
#' probability of survival, i.e. the expected response per individual
#' \emph{exposed}. Draws are paired by row, which is valid because the two
#' posteriors are independent -- see \code{\link{bnec_hurdle}}.
#'
#' @return A \code{\link[base]{matrix}} with draws as rows and predictor
#' values as columns.
#'
#' @importFrom brms posterior_epred
#'
#' @method posterior_epred bayesnechurdlefit
#'
#' @export
posterior_epred.bayesnechurdlefit <- function(object, which = "combined",
                                              resolution = 1000, x_range = NA,
                                              newdata = NULL, ...) {
  which <- hurdle_check_which(which)
  if (is.null(newdata)) {
    return(hurdle_component_preds(object, resolution = resolution,
                                  x_range = x_range)[[which]])
  }
  # Use a supplied grid as given: a caller such as compare_fitted() pairs its
  # columns with its own x vector. Both components share the predictor term.
  g <- posterior_epred(object$growth, newdata = newdata, re_formula = NA)
  s <- posterior_epred(object$survival, newdata = newdata, re_formula = NA)
  n <- min(nrow(g), nrow(s))
  g <- g[seq_len(n), , drop = FALSE]
  s <- s[seq_len(n), , drop = FALSE]
  switch(which, growth = g, survival = s, combined = g * s)
}

#' Generates mean posterior predictions for a \code{\link{bayesnechurdlefit}}
#'
#' @inheritParams posterior_epred.bayesnechurdlefit
#'
#' @param summary Should summary statistics be returned instead of the raw
#' values? Default is \code{TRUE}.
#' @param robust If \code{FALSE} (the default) the mean is used as the measure
#' of central tendency and the standard deviation as the measure of
#' variability. If \code{TRUE}, the median and the median absolute deviation
#' are applied instead.
#' @param probs The percentiles to be computed.
#'
#' @return A \code{\link[base]{matrix}}.
#'
#' @importFrom brms posterior_summary
#' @importFrom chk chk_lgl chk_numeric
#' @importFrom stats fitted
#'
#' @method fitted bayesnechurdlefit
#'
#' @export
fitted.bayesnechurdlefit <- function(object, which = "combined",
                                     resolution = 1000, x_range = NA,
                                     summary = TRUE, robust = FALSE,
                                     probs = c(0.025, 0.975), ...) {
  chk_lgl(summary)
  chk_lgl(robust)
  chk_numeric(probs)
  preds <- posterior_epred(object, which = which, resolution = resolution,
                           x_range = x_range)
  if (!summary) {
    return(preds)
  }
  out <- t(apply(preds, 2, posterior_summary, robust = robust, probs = probs))
  colnames(out) <- c("Estimate", "Est.Error", paste0("Q", probs * 100))
  out
}

#' Generates posterior draws of the response for a
#' \code{\link{bayesnechurdlefit}}
#'
#' @inheritParams fitted.bayesnechurdlefit
#'
#' @details For \code{which = "combined"} this simulates from the two-part
#' process itself rather than summarising a mean curve: a survival outcome is
#' drawn for each posterior draw and predictor value, and individuals that do
#' not survive contribute an exact zero. The result therefore has the spike at
#' zero that the observed data have, which the mean curve does not.
#'
#' @return A \code{\link[base]{matrix}}.
#'
#' @importFrom brms posterior_predict posterior_epred
#' @importFrom stats rbinom
#'
#' @method posterior_predict bayesnechurdlefit
#'
#' @export
posterior_predict.bayesnechurdlefit <- function(object, which = "combined",
                                                resolution = 1000,
                                                x_range = NA, ...) {
  which <- hurdle_check_which(which)
  nd <- hurdle_newdata_pair(object, resolution = resolution, x_range = x_range)
  if (which == "growth") {
    return(posterior_predict(object$growth, newdata = nd$growth,
                             re_formula = NA))
  }
  if (which == "survival") {
    return(posterior_predict(object$survival, newdata = nd$survival,
                             re_formula = NA))
  }
  g <- posterior_predict(object$growth, newdata = nd$growth, re_formula = NA)
  s <- posterior_epred(object$survival, newdata = nd$survival, re_formula = NA)
  n <- min(nrow(g), nrow(s))
  g <- g[seq_len(n), , drop = FALSE]
  s <- s[seq_len(n), , drop = FALSE]
  # A non-survivor contributes an exact zero, not a small response.
  g * matrix(rbinom(length(s), 1, s), nrow = nrow(s))
}

#' Generates posterior predictions for a \code{\link{bayesnechurdlefit}}
#'
#' @inheritParams fitted.bayesnechurdlefit
#'
#' @return A \code{\link[base]{matrix}}.
#'
#' @importFrom brms posterior_summary
#' @importFrom chk chk_lgl chk_numeric
#' @importFrom stats predict
#'
#' @method predict bayesnechurdlefit
#'
#' @export
predict.bayesnechurdlefit <- function(object, which = "combined",
                                      resolution = 1000, x_range = NA,
                                      summary = TRUE, robust = FALSE,
                                      probs = c(0.025, 0.975), ...) {
  chk_lgl(summary)
  preds <- posterior_predict(object, which = which, resolution = resolution,
                             x_range = x_range)
  if (!summary) {
    return(preds)
  }
  out <- t(apply(preds, 2, posterior_summary, robust = robust, probs = probs))
  colnames(out) <- c("Estimate", "Est.Error", paste0("Q", probs * 100))
  out
}

#' Predictor grid for both components of a \code{\link{bayesnechurdlefit}}
#'
#' The two components carry different response columns and, because the growth
#' fit only sees non-zero rows, different observed predictor ranges. Both grids
#' therefore have to be built separately but must span the same range.
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#' @param resolution The number of unique predictor values.
#' @param x_range A range of predictor values.
#'
#' @return A \code{\link[base]{list}} with elements \code{growth},
#' \code{survival} and \code{x}.
#'
#' @noRd
hurdle_newdata_pair <- function(object, resolution = 1000, x_range = NA) {
  if (any(is.na(x_range))) {
    nd_s <- newdata_eval(object$survival, resolution = resolution,
                         x_range = NA)
    x_range <- range(nd_s$x_vec)
  } else {
    nd_s <- newdata_eval(object$survival, resolution = resolution,
                         x_range = x_range)
  }
  nd_g <- newdata_eval(object$growth, resolution = resolution,
                       x_range = x_range)
  list(growth = nd_g$newdata, survival = nd_s$newdata, x = nd_g$x_vec)
}

#' Predictor grid for a \code{\link{bayesnechurdlefit}}
#'
#' @param x An object of class \code{\link{bayesnechurdlefit}}.
#' @param resolution The number of unique predictor values.
#' @param x_range A range of predictor values.
#'
#' @return A \code{\link[base]{data.frame}}.
#'
#' @method bnec_newdata bayesnechurdlefit
#'
#' @export
bnec_newdata.bayesnechurdlefit <- function(x, resolution = 100,
                                           x_range = NA) {
  object <- x
  # Taken from the survival component, which sees every row and therefore the
  # full exposed predictor range; the growth component stops short of any
  # concentration where nothing survived.
  newdata <- bnec_newdata(object$survival, resolution = resolution,
                          x_range = x_range)
  hurdle_disp_placeholders(newdata, object)
}

#' The growth component's disp(~...) columns on a hurdle fit's grid
#'
#' @param newdata A grid built from the survival component.
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#'
#' @details A grid built from the survival formula, which has no \code{disp()}
#' term, lacks the columns the growth term names, and \pkg{brms} would stop on
#' it. The columns are added at the growth component's fixed values and marked
#' as placeholders, as \code{prediction_grid()} does for a single fit.
#'
#' @return \code{newdata}, unchanged where the growth component's formula has
#' no such column.
#'
#' @noRd
hurdle_disp_placeholders <- function(newdata, object) {
  if (length(disp_covariates(object$formula)) == 0) {
    return(newdata)
  }
  growth <- object$growth
  if (inherits(growth, "bayesmanecfit")) {
    growth <- growth$mod_fits[[1]]
  }
  add_disp_placeholders(newdata, growth$fit$data, object$formula)
}

#' Extracts the predicted NSEC value from a \code{\link{bayesnechurdlefit}}
#'
#' @inheritParams nsec
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#' @param which Which curve to use: \code{"combined"} (the default),
#' \code{"growth"} or \code{"survival"}.
#' @param posterior Should the full posterior be returned instead of a summary?
#'
#' @details Without \code{x_range}, every NSEC, growth included, is searched
#' for over the whole concentration series, the survival component's range, and
#' is reported as censored at its top where the curve has not reached the
#' reference by the highest concentration tested. \code{extrapolate} extends
#' that range. See \code{\link{ecx.bayesnechurdlefit}} for the reasoning.
#'
#' @return A vector containing the estimated NSEC value and credible bounds.
#'
#' @importFrom stats quantile
#' @importFrom chk chk_logical
#'
#' @method nsec bayesnechurdlefit
#'
#' @export
nsec.bayesnechurdlefit <- function(object, sig_val = 0.01, resolution = 200,
                                   x_range = NA,
                                   xform = identity,
                                   prob_vals = c(0.5, 0.025, 0.975),
                                   extrapolate = FALSE, ...,
                                   posterior = FALSE, which = "combined") {
  check_component_arg(list(...), object)
  check_removed_args(list(...))
  chk_logical(posterior)
  which <- hurdle_check_which(which)
  if (!inherits(xform, "function")) {
    stop("xform must be a function.")
  }
  # Once for the call. See report_fitted_scale().
  quiet <- report_fitted_scale(object, xform, "nsec")
  on.exit(options(quiet), add = TRUE)
  # As in nsec.bayesnecfit(), extrapolate sets the searched grid. It is
  # measured against the whole concentration series.
  lims <- extrapolate_limits(extrapolate,
                             searched_or_stored_bounds(object, x_range),
                             "NSEC")
  if (!is.null(lims)) {
    report_curve_read_lower_limit(object, lims)
    x_range <- c(lims$lower, lims$upper)
  }
  preds <- hurdle_component_preds(object, resolution = resolution,
                                  x_range = x_range)
  p_samples <- preds[[which]]
  # The control is read at the lowest observed concentration, not the first
  # grid column, so x_range does not change the reference.
  reference <- quantile(preds$control[[which]], sig_val)
  out <- nsec_from_posterior(p_samples, reference, preds$x, control_x(object),
                             preds$control[[which]])
  below <- attr(out, "below_range")
  above <- is.na(out) & !below
  searched_from <- attr(out, "x_searched_from")
  attr(out, "n_below_range") <- NULL
  attr(out, "below_range") <- NULL
  attr(out, "x_searched_from") <- NULL
  out <- hurdle_xform_x(object, out)
  # The lower bound is where the search started, not the foot of the grid,
  # which can reach below the control. Build the record on the recorded grid,
  # then remap it: a decreasing crf() swaps its two ends.
  cens <- xform_censoring(
    censoring_record(max(preds$x), searched_from, above, below),
    function(value) hurdle_xform_x(object, value)
  )
  if (inherits(xform, "function")) {
    out <- xform(out)
    cens <- xform_censoring(cens, xform)
  }
  attr(out, "censored") <- cens
  # After xform, so that the bound the warning names is on the scale of the
  # numbers returned.
  warn_censored_draws(out, "NSEC", cens = cens)
  estimate <- summarise_censored(out, prob_vals, cens)
  names(estimate) <- clean_names(estimate)
  attr(estimate, "toxicity_estimate") <- "nsec"
  attr(estimate, "component") <- which
  attr(out, "toxicity_estimate") <- "nsec"
  if (!posterior) estimate else out
}

#' Back-transform predictor values through any function applied inside crf()
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#' @param out A \code{\link[base]{numeric}} vector of predictor values.
#'
#' @return A \code{\link[base]{numeric}} vector.
#'
#' @importFrom stats terms
#'
#' @noRd
hurdle_xform_x <- function(object, out) {
  sub_x_transformation(out, object$formula)
}

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

#' Summary of a \code{\link{bayesnechurdlefit}}
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#' @param ... Passed to \code{\link{nec}} and \code{\link{ecx}} for each
#' component, so that \code{xform} in particular applies to every estimate in
#' the table. \code{x_range} is the exception: it is passed to
#' \code{\link{ecx}} only, and must be given by its full name. See Details.
#' @param ecx Should ECx estimates be included? Defaults to \code{FALSE}.
#' @param ecx_vals The ECx levels to report.
#'
#' @details The no-effect estimate is reported for all three endpoints and
#' labelled by type: NEC where the component's model set is entirely threshold
#' models, NSEC where it is entirely smooth ones, and N(S)EC where it is a
#' mixture (Fisher et al. 2023). The combined estimate is the smaller of the
#' two component estimates per posterior draw, so it is a pure NEC only where
#' both components are; see \code{\link{nec.bayesnechurdlefit}}.
#'
#' With \code{ecx = TRUE}, every ECx row is read over the prediction grid
#' stored with the survival component, which covers the whole concentration
#' series. This is the range \code{\link{ecx.bayesnechurdlefit}} reads every
#' curve over by default, so each row agrees with a bare \code{\link{ecx}}
#' call for the same curve. A growth ECx above the highest concentration at
#' which anything survived is reported as a value, as it is by
#' \code{\link{ecx}}. Where the survival component stores no grid, the rows
#' are read over the grid \code{\link{ecx}} builds by default. An \code{x_range}
#' supplied in \code{...} replaces those grids for all three curves, and sets
#' the grid of the ECx rows only. \code{x_range = NULL} leaves
#' \code{\link{ecx}} to build its own grid, as it does where a component
#' stores none. The argument must be given by its full name, because an
#' abbreviation can be ignored without a message.
#'
#' \code{x_range} is not passed to \code{\link{nec}}, because each no-effect
#' estimate is read from the posterior stored when its component was fitted,
#' and is censored at the bound of the prediction grid stored then. To report a
#' no-effect estimate beyond that bound, supply \code{extrapolate}, which is
#' passed to \code{\link{nec}}.
#'
#' @return An object of class \code{hurdlesummary}.
#'
#' @references
#' Fisher R, Fox DR, Negri AP, van Dam J, Flores F, Koppel D (2023). Methods for
#' estimating no-effect toxicity concentrations in ecotoxicology. Integrated
#' Environmental Assessment and Management. doi: 10.1002/ieam.4809.
#'
#' @importFrom chk chk_lgl chk_numeric
#'
#' @method summary bayesnechurdlefit
#'
#' @export
summary.bayesnechurdlefit <- function(object, ..., ecx = FALSE,
                                      ecx_vals = c(10, 50, 90)) {
  chk_lgl(ecx)
  chk_numeric(ecx_vals)
  # The dots go to nec() and, with ecx = TRUE, to ecx(), so look for
  # posterior = TRUE as each of them will match it.
  check_summary_posterior(nec, c(list(which = "combined"), list(...)))
  ecs <- NULL
  if (ecx) {
    check_summary_ecx_posterior(list(...))
    # Report the scale once for the table rather than in each ecx() call.
    # Use summary_ecx_xform(), not dots_xform(ecx, ...): here `ecx` is the
    # logical argument, not the generic.
    quiet <- report_fitted_scale(object, summary_ecx_xform(list(...)), "ecx")
    on.exit(options(quiet), add = TRUE)
    # Read every ECx row over the survival component's stored grid, the whole
    # concentration series, so that the rows match the no-effect estimates
    # above them. NULL, where there is no stored grid, lets ecx() build one.
    # The x_range formal takes a caller's x_range out of `...`, so ecx() does
    # not receive it twice. It follows `...`, so it matches only the full name.
    ecx_row <- function(w, v, ..., x_range = hurdle_summary_range(object)) {
      if (is.null(x_range)) {
        ecx(object, ecx_val = v, which = w, ...)
      } else {
        ecx(object, ecx_val = v, which = w, x_range = x_range, ...)
      }
    }
    ecs <- lapply(c("combined", "growth", "survival"), function(w) {
      out <- lapply(ecx_vals, function(v) ecx_row(w, v, ...))
      names(out) <- paste0("ec", ecx_vals)
      out
    })
    names(ecs) <- c("combined", "growth", "survival")
  }
  # Suppress nec()'s N(S)EC message: the table labels every estimate by type.
  # nec_row() drops x_range, which cannot change a no-effect estimate read
  # from the stored posterior; it sets the grid of the ECx rows only.
  nec_row <- function(w, ..., x_range) {
    suppressMessages(nec(object, which = w, ...))
  }
  nes <- lapply(c("combined", "growth", "survival"), function(w) {
    nec_row(w, ...)
  })
  names(nes) <- c("combined", "growth", "survival")
  mods <- function(f) {
    if (inherits(f, "bayesmanecfit")) names(f$mod_fits) else f$model
  }
  # The combined estimate is the smaller of the two per draw, so it is a NEC
  # only where both components' ne_type is NEC.
  ne_type_of <- function(f) {
    if (is.null(f$ne_type)) NA_character_ else f$ne_type
  }
  ne_types <- c(growth = ne_type_of(object$growth),
                survival = ne_type_of(object$survival))
  combined_type <- if (anyNA(ne_types)) {
    NA_character_
  } else if (all(ne_types == "NEC")) {
    "NEC"
  } else if (all(ne_types == "NSEC")) {
    "NSEC"
  } else {
    "N(S)EC"
  }
  ne_types <- c(combined = combined_type, ne_types)
  # The growth component may be model-averaged, in which case pull_brmsfit()
  # needs a model to pull; every model in the set shares the family, so the
  # first will do.
  growth_fit <- if (inherits(object$growth, "bayesmanecfit")) {
    object$growth$mod_fits[[1]]$fit
  } else {
    pull_brmsfit(object$growth)
  }
  out <- list(
    n_exposed = object$n_exposed, n_dead = object$n_dead,
    # brms records Gamma() as "gamma". Print the tag bnec_hurdle() reported.
    growth_family = canonical_fam_tag(growth_fit$family$family),
    growth_models = mods(object$growth),
    survival_models = mods(object$survival),
    ne = nes, ne_types = ne_types, ecs = ecs,
    growth_averaged = inherits(object$growth, "bayesmanecfit"),
    survival_averaged = inherits(object$survival, "bayesmanecfit"),
    failed_models = failed_models(object)
  )
  allot_class(out, "hurdlesummary")
}

#' @noRd
#' @method print hurdlesummary
#' @export
print.hurdlesummary <- function(x, ...) {
  cat("Object of class bayesnechurdlefit\n\n")
  cat("  ", x$n_exposed, "individuals exposed;", x$n_dead, "recorded as zero (",
      sprintf("%.1f%%", 100 * x$n_dead / x$n_exposed), ")\n\n")
  cat("  growth   :", x$growth_family, "--",
      paste0(x$growth_models, collapse = ", "), "\n")
  cat("  survival : bernoulli --",
      paste0(x$survival_models, collapse = ", "), "\n\n")
  # Directly under the model lists, unlike the other print methods, because it
  # explains why either list is shorter than requested.
  print_failed_models(x$failed_models)
  # One matrix rather than one line per component: a per-line cat() prefix puts
  # the label above the column headings rather than beside the values.
  cat("No-effect toxicity estimates\n")
  ne_mat <- do.call(rbind, x$ne)
  tp <- x$ne_types[names(x$ne)]
  rownames(ne_mat) <- ifelse(is.na(tp), names(x$ne),
                             paste0(names(x$ne), " (", tp, ")"))
  # rbind() drops the censoring marks, so collect and attach them again, or a
  # censored estimate would print here as a bare number.
  attr(ne_mat, "censored_summary") <- row_censoring(x$ne)
  print_mat(ne_mat)
  print_row_censoring_notes(attr(ne_mat, "censored_summary"),
                            rownames(ne_mat))
  if (any(x$ne_types != "NEC", na.rm = TRUE)) {
    cat("\nNSEC values appear where a model set contains smooth (ECx) models,",
        "which\ncarry no threshold parameter; N(S)EC is a model-averaged",
        "combination\nof the two (Fisher et al. 2023).\n")
  }
  if (!is.null(x$ecs)) {
    cat("\nECx estimates\n")
    ec_flat <- unlist(x$ecs, recursive = FALSE, use.names = FALSE)
    ec_mat <- do.call(rbind, ec_flat)
    rownames(ec_mat) <- unlist(lapply(names(x$ecs), function(w) {
      paste0(w, " ", names(x$ecs[[w]]))
    }))
    attr(ec_mat, "censored_summary") <- row_censoring(ec_flat)
    print_mat(ec_mat)
    print_row_censoring_notes(attr(ec_mat, "censored_summary"),
                              rownames(ec_mat))
  }
  cat("\nThe combined endpoint is the expected response per individual",
      "exposed,\ni.e. growth * survival. Use which = to select a component.\n")
  invisible(x)
}

# ---------------------------------------------------------------------------
# Delegating methods
#
# These describe a model fit, and the combined endpoint is not one, so each
# returns one element per component.
# ---------------------------------------------------------------------------

#' @noRd
hurdle_delegate <- function(object, fun, ..., .quiet = FALSE) {
  list(growth = fun(object$growth, ...), survival = fun(object$survival, ...))
}

#' Component brmsfit objects of a \code{\link{bayesnechurdlefit}}
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#' @param ... Passed to the component method.
#'
#' @return A named \code{\link[base]{list}} of two
#' \code{\link[brms]{brmsfit}} objects.
#'
#' @method pull_brmsfit bayesnechurdlefit
#'
#' @export
pull_brmsfit.bayesnechurdlefit <- function(object, ...) {
  hurdle_delegate(object, pull_brmsfit, ...)
}

#' Rhat values for both components of a \code{\link{bayesnechurdlefit}}
#'
#' @param x An object of class \code{\link{bayesnechurdlefit}}.
#' @param ... Passed to the component method.
#' @param rhat_cutoff A \code{\link[base]{numeric}} cut-off.
#'
#' @return A named \code{\link[base]{list}} of two elements.
#'
#' @method rhat bayesnechurdlefit
#'
#' @export
rhat.bayesnechurdlefit <- function(x, ..., rhat_cutoff = 1.01) {
  hurdle_delegate(x, rhat, rhat_cutoff = rhat_cutoff, ...)
}

#' Chain plots for both components of a \code{\link{bayesnechurdlefit}}
#'
#' @param x An object of class \code{\link{bayesnechurdlefit}}.
#' @param ... Passed to the component method.
#'
#' @return Invisibly, a named \code{\link[base]{list}}.
#'
#' @method check_chains bayesnechurdlefit
#'
#' @export
check_chains.bayesnechurdlefit <- function(x, ...) {
  message(hurdle_no_combined("check_chains"))
  invisible(hurdle_delegate(x, check_chains, ...))
}

#' Prior checks for both components of a \code{\link{bayesnechurdlefit}}
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#' @param filename Optional filename to save plots to.
#' @param ask Should the user be prompted between plots?
#'
#' @return A named \code{\link[base]{list}}.
#'
#' @method check_priors bayesnechurdlefit
#'
#' @export
check_priors.bayesnechurdlefit <- function(object, filename = NA,
                                           ask = TRUE) {
  message(hurdle_no_combined("check_priors"))
  hurdle_delegate(object, check_priors, filename = filename, ask = ask)
}


#' Model frames of both components of a \code{\link{bayesnechurdlefit}}
#'
#' @param formula An object of class \code{\link{bayesnechurdlefit}}.
#' @param ... Passed to the component method.
#'
#' @return A named \code{\link[base]{list}} of two
#' \code{\link[base]{data.frame}}s.
#'
#' @importFrom stats model.frame
#'
#' @method model.frame bayesnechurdlefit
#'
#' @export
model.frame.bayesnechurdlefit <- function(formula, ...) {
  list(growth = model.frame(formula$growth, ...),
       survival = model.frame(formula$survival, ...))
}


# ---------------------------------------------------------------------------
# Model-set operations
#
# Each applies to both components, which must stay in step: their fits must
# describe the same data.
# ---------------------------------------------------------------------------

#' @noRd
hurdle_rewrap <- function(object, growth, survival) {
  object$growth <- growth
  object$survival <- survival
  object
}

#' Adds or drops models from both components of a
#' \code{\link{bayesnechurdlefit}}
#'
#' @inheritParams amend
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#'
#' @details Applied to both components, so that the two model sets stay in
#' step. Where a model is valid for one component but not the other -- the
#' survival component is 0-1 bounded and the growth component may not be --
#' \code{\link{bnec}} will drop it from that component only, and the sets will
#' legitimately differ.
#'
#' @return An object of class \code{\link{bayesnechurdlefit}}.
#'
#' @method amend bayesnechurdlefit
#'
#' @export
amend.bayesnechurdlefit <- function(object, drop, add, loo_controls,
                                    x_range = NA, resolution = 1000,
                                    sig_val = 0.01, priors,
                                    prior_type = "uninformative",
                                    asymptote_observed = TRUE,
                                    timeout = Inf,
                                    predictor_scale = "auto") {
  predictor_scale <- validate_predictor_scale(predictor_scale)
  # Validate against the whole stored predictor before either component is
  # refitted: the growth data omit deaths, and growth is amended first.
  validate_predictor_scale(predictor_scale, hurdle_raw_data(object)$x)
  args <- list(x_range = x_range, resolution = resolution, sig_val = sig_val,
               prior_type = prior_type,
               asymptote_observed = asymptote_observed,
               predictor_scale = predictor_scale,
               timeout = timeout)
  if (!missing(priors)) args$priors <- priors
  if (!missing(drop)) args$drop <- drop
  if (!missing(add)) args$add <- add
  if (!missing(loo_controls)) args$loo_controls <- loo_controls
  amend_part <- function(part, label) {
    tryCatch(do.call(amend, c(list(part), args)), error = function(e) {
      stop("Could not amend the ", label, " component: ",
           conditionMessage(e), call. = FALSE)
    })
  }
  hurdle_rewrap(object, amend_part(object$growth, "growth"),
                amend_part(object$survival, "survival"))
}

#' Refuse a component's new response where it is at a bound of its refit
#' family in every observation
#'
#' @param part One component of a \code{\link{bayesnechurdlefit}}: a
#' \code{\link{bayesnecfit}} or a \code{\link{bayesmanecfit}}.
#' @param data The data that component's refit is given.
#' @param family The validated family passed to \code{update()}, or
#' \code{NULL}.
#' @param subject The subject of the message, naming the component.
#' @param flat_remedy How to fit \code{ecxflat} to the component; see
#' \code{check_response_at_bound()}.
#'
#' @details Makes the check \code{check_update_data()} makes per model, once
#' for a whole component, so that both components are checked before either
#' is refitted. A component fitted with \code{ecxflat} alone passes, as there.
#'
#' @return \code{NULL}, invisibly. Called for its error.
#'
#' @noRd
check_refit_bound <- function(part, data, family = NULL, subject = NULL,
                              flat_remedy = NULL) {
  fits <- recover_prebayesnecfit(part)
  bdat <- model.frame(extract_formula(fits[[1]]), data = data,
                      run_par_checks = TRUE)
  # First, as in check_update_data(): model.frame() drops a row with a missing
  # value, and the bound would otherwise be read without it.
  check_complete_cases(bdat)
  check_response_at_bound(bdat, refit_family(fits[[1]]$fit$family, family),
                          subject = subject, model = names(fits),
                          flat_remedy = flat_remedy)
}

#' The growth component a hurdle \code{update()} refits
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#' @param family The validated family passed to \code{update()}, or
#' \code{NULL}.
#'
#' @details Checks a supplied family as \code{\link{bnec_hurdle}} checks
#' \code{family_growth}, and gives the growth formulas \code{trunc(lb = 1)}
#' where it is a count family and removes it where it is not. Without a family
#' the stored formulas are kept, and a growth component that holds the
#' truncation still needs the \pkg{brms} version \code{bnec_hurdle} needs.
#'
#' @return The growth component, a \code{\link{bayesnecfit}} or a
#' \code{\link{bayesmanecfit}}, for \code{update()} to refit.
#'
#' @noRd
hurdle_refit_growth <- function(object, family = NULL) {
  growth <- object$growth
  if (is.null(family)) {
    fits <- recover_prebayesnecfit(growth)
    if (is_factorised_count_formula(fits[[1]]$bayesnecformula,
                                    fits[[1]]$fit$family)) {
      check_count_truncation_support()
    }
    return(growth)
  }
  check_hurdle_growth_family(family)
  check_hurdle_count_growth(object$formula, family)
  set_hurdle_growth_truncation(growth, is_hurdle_count_growth(family))
}

#' Set the count-hurdle truncation on every equation of a growth component
#'
#' @param part The growth component of a \code{\link{bayesnechurdlefit}}: a
#' \code{\link{bayesnecfit}} or a \code{\link{bayesmanecfit}}.
#' @param truncate Whether each response should hold \code{trunc(lb = 1)}.
#'
#' @details Rewrites both formulas of each equation. \pkg{brms} refits from the
#' one stored in the \code{brmsfit}, and the \pkg{bayesnec} methods read the
#' truncation off the other.
#'
#' @return \code{part}, rewritten.
#'
#' @noRd
set_hurdle_growth_truncation <- function(part, truncate) {
  set_one <- function(m) {
    m$bayesnecformula[[2]] <- hurdle_truncation_lhs(lhs(m$bayesnecformula),
                                                    truncate)
    attr(m$bayesnecformula, "bayesnec_internal_truncation") <-
      if (truncate) TRUE
    m$fit$formula$formula[[2]] <- hurdle_truncation_lhs(
      m$fit$formula$formula[[2]], truncate
    )
    m
  }
  if (is_bayesmanecfit(part)) {
    part$mod_fits <- lapply(part$mod_fits, set_one)
  } else {
    part <- set_one(part)
  }
  part
}

#' Updates both components of a \code{\link{bayesnechurdlefit}}
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#' @param newdata Optional new data. Split into the two component datasets the
#' same way \code{\link{bnec_hurdle}} does, so zeros continue to denote the
#' hurdle rather than being modelled as small responses, and checked the same
#' way. A response that is not numeric, holds a missing or negative value, or
#' has no zero or no value above zero is refused with the message
#' \code{bnec_hurdle} gives, as is a row that is both zero and censored.
#' Each component is refitted with the family it was fitted
#' with, as \code{\link{update.bnecfit}} describes. Both components are checked
#' against their families before either is refitted, so new data that put the
#' growth component at a bound in every observation, such as a \code{Beta}
#' growth component of 1 throughout, are refused, naming that component.
#' @param ... Further arguments passed to \code{\link{update.bnecfit}} for
#' each component. A \code{family} is passed to the growth component only.
#' The survival component is always refitted as \code{bernoulli}. The growth
#' family is checked as \code{\link{bnec_hurdle}} checks
#' \code{family_growth}, before either component is refitted. A family
#' \code{bnec_hurdle} refuses for the growth component, such as
#' \code{hurdle_gamma}, is refused. A \code{poisson} or \code{negbinomial}
#' family needs \pkg{brms} 2.23.2 or later, and the growth component is then
#' refitted with the zero-truncated likelihood \code{bnec_hurdle} uses.
#'
#' @return An object of class \code{\link{bayesnechurdlefit}}.
#'
#' @importFrom stats update
#'
#' @method update bayesnechurdlefit
#'
#' @export
update.bayesnechurdlefit <- function(object, newdata = NULL, ...) {
  # Take `family` out of `...`, so that the growth family never reaches the
  # survival component. Forward `...` unchanged to the growth component:
  # update.bnecfit() reads the link the caller wrote off the unevaluated call.
  update_survival <- function(..., family) update(object$survival, ...)
  if (!is.null(newdata)) {
    y <- newdata[[object$y_var]]
    if (is.null(y)) {
      stop("\"newdata\" must contain the response column \"", object$y_var,
           "\".", call. = FALSE)
    }
    check_hurdle_response(y, object$y_var)
    # Check newdata for rows that are both zero and censored, as bnec_hurdle()
    # checks its data.
    check_hurdle_cens(check_hurdle_aterms(object$formula), newdata, y,
                      object$y_var)
  }
  # A supplied family is the growth component's. It is read here for its tag
  # only; update.bnecfit() validates it again, with its link.
  supplied <- if ("family" %in% ...names()) {
    validate_family(list(...)[["family"]])
  }
  growth <- hurdle_refit_growth(object, supplied)
  if (is.null(newdata)) {
    return(hurdle_rewrap(object, update(growth, ...), update_survival(...)))
  }
  growth_data <- newdata[y > 0, , drop = FALSE]
  surv_data <- newdata
  surv_data[[".alive"]] <- as.integer(y > 0)
  # Check both components before either is refitted, naming the component
  # rather than the generated .alive column.
  check_refit_bound(
    growth, growth_data, supplied,
    subject = paste0("The growth component of the response \"", object$y_var,
                     "\" (its ", sum(y > 0), " non-zero values)"),
    flat_remedy = hurdle_growth_flat_remedy()
  )
  check_refit_bound(
    object$survival, surv_data,
    subject = paste0("The survival component of the response \"",
                     object$y_var, "\" (", sum(y == 0), " deaths of ",
                     length(y), ")")
  )
  # Refit in turn, growth first, rather than inside hurdle_rewrap(), whose
  # argument evaluation would set the order.
  growth <- update(growth, newdata = growth_data, ...)
  survival <- update_survival(newdata = surv_data, ...)
  out <- hurdle_rewrap(object, growth, survival)
  out$data <- newdata
  out$n_exposed <- length(y)
  out$n_dead <- sum(y == 0)
  out
}

#' @noRd
hurdle_check_pair <- function(objects) {
  if (!all(vapply(objects, is_bayesnechurdlefit, logical(1)))) {
    stop("All objects must be of class bayesnechurdlefit. Hurdle fits cannot",
         " be combined with ordinary bnec fits, because each carries two",
         " component model sets rather than one.", call. = FALSE)
  }
  y_vars <- vapply(objects, function(z) z$y_var, character(1))
  if (length(unique(y_vars)) > 1) {
    stop("All objects must share the same response variable.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Combines the model sets of several \code{\link{bayesnechurdlefit}} objects
#'
#' @param x An object of class \code{\link{bayesnechurdlefit}}.
#' @param ... Further objects of the same class.
#'
#' @details Combines component-wise: the growth sets are merged with each
#' other, and the survival sets with each other.
#'
#' @return An object of class \code{\link{bayesnechurdlefit}}.
#'
#' @method c bayesnechurdlefit
#'
#' @export
c.bayesnechurdlefit <- function(x, ...) {
  objects <- c(list(x), list(...))
  hurdle_check_pair(objects)
  hurdle_rewrap(x,
                do.call(c, lapply(objects, `[[`, "growth")),
                do.call(c, lapply(objects, `[[`, "survival")))
}

#' Combines two \code{\link{bayesnechurdlefit}} objects
#'
#' @param e1 An object of class \code{\link{bayesnechurdlefit}}.
#' @param e2 An object of class \code{\link{bayesnechurdlefit}}.
#'
#' @return An object of class \code{\link{bayesnechurdlefit}}.
#'
#' @method + bayesnechurdlefit
#'
#' @export
`+.bayesnechurdlefit` <- function(e1, e2) {
  hurdle_check_pair(list(e1, e2))
  hurdle_rewrap(e1, e1$growth + e2$growth, e1$survival + e2$survival)
}

# ---------------------------------------------------------------------------
# Plotting
#
# which = "all" draws the components as separate panels: growth is in
# response units and survival is a probability, so they share no y-axis.
# ---------------------------------------------------------------------------

#' @noRd
hurdle_check_plot_which <- function(which) {
  match.arg(which, c("combined", "growth", "survival", "all"))
}

#' Raw observed data underlying a \code{\link{bayesnechurdlefit}}
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#' @param recorded \code{FALSE}, the default, returns the predictor as
#' \code{crf()} transforms it, the scale the model is fitted on. \code{TRUE}
#' returns the column \code{crf()} names, the scale the plots and the
#' prediction grid are on.
#'
#' @return A \code{\link[base]{data.frame}} with columns \code{x} and \code{y}.
#'
#' @importFrom stats terms
#'
#' @noRd
hurdle_raw_data <- function(object, recorded = FALSE) {
  x_expr <- crf_x_call(object$formula)
  if (recorded) {
    x_expr <- as.name(all.vars(x_expr)[1])
  }
  # Evaluate in the formula's environment, not eval()'s default, so that a
  # user's function in crf(sq(x), ...) resolves.
  data.frame(x = eval(x_expr, object$data,
                      enclos = formula_env(object$formula)),
             y = object$data[[object$y_var]])
}

#' Plot data for one panel of a \code{\link{bayesnechurdlefit}}
#'
#' @noRd
hurdle_panel <- function(object, which, resolution, x_range) {
  preds <- hurdle_component_preds(object, resolution = resolution,
                                  x_range = x_range)
  p <- preds[[which]]
  # On the recorded scale, as preds$x is.
  raw <- hurdle_raw_data(object, recorded = TRUE)
  if (which == "survival") {
    # The observed analogue of a survival curve is the proportion non-zero at
    # each predictor value, not the individual responses.
    obs <- survival_by_x(raw$x, raw$y)
    raw <- data.frame(x = obs$x, y = obs$y)
  } else if (which == "growth") {
    raw <- raw[raw$y > 0, , drop = FALSE]
  }
  list(x = preds$x,
       est = apply(p, 2, median),
       lo = apply(p, 2, quantile, 0.025),
       up = apply(p, 2, quantile, 0.975),
       raw = raw,
       ylab = switch(which,
                     combined = "Response per individual exposed",
                     growth = "Response of survivors",
                     survival = "Probability of survival"))
}

#' The map the hurdle plots apply to the points and the curve
#'
#' \code{xform}, except where \code{crf()} transforms the predictor inline. The
#' points and the grid are then left on the recorded scale, as the
#' \code{\link{bayesnecfit}} plots leave them.
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#' @param xform The function the caller supplied.
#'
#' @return A \code{\link[base]{function}}.
#' @noRd
hurdle_axis_xform <- function(object, xform) {
  if (is.null(inline_x_transform(object))) xform else identity
}

#' The map that puts a hurdle no-effect estimate on the plot's predictor axis
#'
#' The same choice \code{to_axis_scale()} makes for a
#' \code{\link{bayesnecfit}}: \code{xform} where the predictor is not
#' transformed or the caller supplied one, and otherwise the inverse of the
#' \code{crf()} transformation read off the whole-series prediction grid.
#' Read the inverse off that grid, not off a panel's \code{x_range}: the
#' inverse clamps at the grid ends, and the legend would print the clamped
#' value.
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#' @param xform The function the caller supplied.
#' @param resolution The number of grid points.
#'
#' @return A \code{\link[base]{function}}, for \code{\link{nec}} to apply to
#' each draw.
#' @noRd
hurdle_ne_axis_map <- function(object, xform, resolution) {
  if (is.null(inline_x_transform(object)) || !identical(xform, identity)) {
    return(xform)
  }
  grid <- newdata_eval(object$survival, resolution = resolution,
                       x_range = NA)$x_vec
  # A fit-time x_range can identify estimates beyond the observed range.
  grid <- sort(unique(c(grid, ne_grid_data(object$growth)$x,
                        ne_grid_data(object$survival)$x)))
  inverse <- grid_inverse(object$formula, grid)
  if (is.null(inverse)) identity else inverse
}

#' Plots a \code{\link{bayesnechurdlefit}}
#'
#' @param x An object of class \code{\link{bayesnechurdlefit}}.
#' @param ... Further arguments to \code{\link[graphics]{plot}}.
#' @param which Which curve to plot: \code{"combined"} (the default),
#' \code{"growth"}, \code{"survival"}, or \code{"all"} for three panels.
#' @param CI Should credible intervals be drawn? Defaults to \code{TRUE}.
#' @param add_nec Should the no-effect estimate be drawn? Defaults to
#' \code{TRUE}.
#' @param resolution The number of unique predictor values to predict over.
#' @param x_range A range of predictor values to plot over.
#' @param xform A function to apply to the predictor where \code{crf()} does
#' not transform it inline. Where it does, the function that returns the
#' no-effect estimate to the recorded scale. See Details.
#' @param ylab Y axis label. Defaults to a label describing \code{which}.
#' @param xlab X axis label.
#'
#' @details The default plots the combined endpoint only -- the expected
#' response per individual exposed -- because that is the quantity a hurdle
#' model exists to estimate. \code{which = "all"} adds the two components as
#' separate panels; they are not overlaid because the growth curve is in
#' response units and the survival curve is a probability, so sharing one pair
#' of axes would misrepresent their relative magnitude.
#'
#' Where \code{crf()} transforms the predictor inline, as
#' \code{crf(log(concentration))} does, the observed points, the curve and the
#' no-effect line are drawn on the recorded scale, the scale the prediction
#' grid is built on, as \code{\link{plot}} draws a \code{\link{bayesnecfit}}.
#' \code{\link{nec}} returns its estimate on the transformed scale, so the line
#' is returned to the recorded scale through \code{xform} where one is
#' supplied, and otherwise by inverting the transformation on the prediction
#' grid. \code{xform} is not applied to the points or the curve in that case.
#'
#' @return A plot.
#'
#' @importFrom graphics abline legend lines par polygon
#' @importFrom grDevices adjustcolor
#' @importFrom stats median quantile
#' @importFrom chk chk_lgl
#'
#' @method plot bayesnechurdlefit
#'
#' @export
plot.bayesnechurdlefit <- function(x, ..., which = "combined", CI = TRUE,
                                   add_nec = TRUE, resolution = 1000,
                                   x_range = NA, xform = identity,
                                   ylab = NULL, xlab = "Predictor") {
  which <- hurdle_check_plot_which(which)
  chk_lgl(CI)
  chk_lgl(add_nec)
  if (!inherits(xform, "function")) {
    stop("xform must be a function.")
  }
  panels <- if (which == "all") {
    c("combined", "growth", "survival")
  } else {
    which
  }
  if (length(panels) > 1) {
    old_par <- par(mfrow = c(1, length(panels)))
    on.exit(par(old_par), add = TRUE)
  }
  axis_x <- hurdle_axis_xform(x, xform)
  if (add_nec) {
    ne_map <- hurdle_ne_axis_map(x, xform, resolution)
  }
  for (w in panels) {
    p <- hurdle_panel(x, w, resolution, x_range)
    xv <- axis_x(p$x)
    plot(axis_x(p$raw$x), p$raw$y, pch = 16, cex = 1.5,
         col = adjustcolor(1, alpha.f = 0.25),
         ylab = if (is.null(ylab)) p$ylab else ylab, xlab = xlab,
         ylim = range(c(p$raw$y, p$lo, p$up), na.rm = TRUE), ...)
    if (CI) {
      lines(xv, p$up, lty = 2)
      lines(xv, p$lo, lty = 2)
    }
    lines(xv, p$est)
    if (add_nec) {
      # Give nec() the map rather than apply it to the summary, so that a
      # decreasing map keeps the interval and its censoring marks in order.
      # Suppress its messages, which describe the estimate, not the figure.
      ne <- suppressMessages(nec(x, which = w, xform = ne_map))
      abline(v = ne, col = "red", lty = c(1, 3, 3))
      legend("topright", bty = "n", lty = 1, col = "red",
             legend = paste0("N(S)EC: ", bound_prefix(ne, 1),
                             signif(ne[[1]], 2), " (", bound_prefix(ne, 2),
                             signif(ne[[2]], 2), "-", bound_prefix(ne, 3),
                             signif(ne[[3]], 2), ")"))
    }
  }
  invisible(x)
}

#' Data underlying a \code{\link{bayesnechurdlefit}} plot
#'
#' @param x An object of class \code{\link{bayesnechurdlefit}}.
#' @param add_nec Unused; retained for consistency with the generic.
#' @param add_ecx Unused; retained for consistency with the generic.
#' @param xform A function to apply to the predictor where \code{crf()} does
#' not transform it inline. Where it does, the predictor is returned on the
#' recorded scale, the scale the prediction grid is built on, and \code{xform}
#' is not applied.
#' @param ... Unused.
#' @param which Which curve(s) to return. See \code{\link{plot}}.
#' @param resolution The number of unique predictor values.
#' @param x_range A range of predictor values.
#'
#' @return A \code{\link[base]{list}} of two \code{\link[base]{data.frame}}s,
#' \code{curve} and \code{raw}, each carrying a \code{panel} column.
#'
#' @method ggbnec_data bayesnechurdlefit
#'
#' @export
ggbnec_data.bayesnechurdlefit <- function(x, add_nec = TRUE, add_ecx = FALSE,
                                          xform = identity, ...,
                                          which = "combined",
                                          resolution = 1000, x_range = NA) {
  which <- hurdle_check_plot_which(which)
  panels <- if (which == "all") {
    c("combined", "growth", "survival")
  } else {
    which
  }
  axis_x <- hurdle_axis_xform(x, xform)
  curve <- list()
  raw <- list()
  for (w in panels) {
    p <- hurdle_panel(x, w, resolution, x_range)
    curve[[w]] <- data.frame(panel = p$ylab, x = axis_x(p$x), y = p$est,
                             lo = p$lo, up = p$up)
    raw[[w]] <- data.frame(panel = p$ylab, x = axis_x(p$raw$x), y = p$raw$y)
  }
  curve <- do.call(rbind, curve)
  raw <- do.call(rbind, raw)
  # Fix panel order; the default factor ordering would be alphabetical.
  lev <- unique(curve$panel)
  curve$panel <- factor(curve$panel, levels = lev)
  raw$panel <- factor(raw$panel, levels = lev)
  list(curve = curve, raw = raw)
}

#' Creates a \pkg{ggplot2} plot of a \code{\link{bayesnechurdlefit}}
#'
#' @inheritParams ggbnec_data.bayesnechurdlefit
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#' @param CI Should credible intervals be drawn? Defaults to \code{TRUE}.
#' @param xlab X axis label.
#' @param ylab Y axis label. Defaults to \code{NULL}, which labels each panel
#' by what it shows.
#'
#' @details As for \code{\link{plot}}, the default is the combined endpoint
#' alone and \code{which = "all"} facets rather than overlays, because the
#' three curves are not on a common scale. Each panel carries a single series,
#' so no colour encoding or legend is needed.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_ribbon facet_wrap
#' @importFrom ggplot2 labs theme_classic vars
#' @importFrom chk chk_lgl
#'
#' @method autoplot bayesnechurdlefit
#'
#' @export
autoplot.bayesnechurdlefit <- function(object, ..., which = "combined",
                                       CI = TRUE, resolution = 1000,
                                       x_range = NA, xform = identity,
                                       xlab = "Predictor", ylab = NULL) {
  chk_lgl(CI)
  dat <- ggbnec_data(object, which = which, resolution = resolution,
                     x_range = x_range, xform = xform)
  p <- ggplot(dat$curve, aes(x = .data$x, y = .data$y))
  if (CI) {
    p <- p + geom_ribbon(aes(ymin = .data$lo, ymax = .data$up),
                         fill = "grey70", alpha = 0.4, colour = NA)
  }
  p <- p +
    geom_point(data = dat$raw, aes(x = .data$x, y = .data$y),
               colour = adjustcolor(1, alpha.f = 0.25), size = 1.6) +
    geom_line(linewidth = 0.7) +
    labs(x = xlab, y = if (is.null(ylab)) NULL else ylab) +
    theme_classic()
  if (nlevels(dat$curve$panel) > 1) {
    p <- p + facet_wrap(vars(.data$panel), scales = "free_y")
  } else if (is.null(ylab)) {
    p <- p + labs(y = levels(dat$curve$panel)[1])
  }
  p
}

#' Extracts ECx corresponding to an NSEC from a
#' \code{\link{bayesnechurdlefit}}
#'
#' @inheritParams ecnsec
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#' @param which Which curve to use: \code{"combined"} (the default),
#' \code{"growth"} or \code{"survival"}.
#' @param posterior Should the full posterior be returned instead of a summary?
#'
#' @details \code{type = "absolute"} (the default) and \code{"range"} are
#' defined here and invert the \code{\link{ecx}} reference construction under
#' the same \code{type}. \code{"relative"} is refused, as it is by
#' \code{\link{ecx}} for this class: a two-block fit has no single \code{bot}
#' parameter to measure towards. \code{"direct"} is refused because it names a
#' response value rather than a percentage.
#'
#' \code{xform} is applied to \code{nsec}, and the curve is read at the
#' result on the recorded predictor scale, as in \code{\link{ecnsec}} on a
#' \code{\link{bayesnecfit}}. Where \code{crf()} transforms the predictor
#' inline, \code{\link{nsec}} returns its estimate on the transformed scale
#' unless given an \code{xform}, so a message says so once per call where
#' \code{xform} is left at its default, and names the \code{xform} that puts
#' such a value on the recorded scale. In earlier versions this method applied
#' \code{xform} to the percentage it returned. A call that supplies an
#' \code{xform} other than \code{identity} is warned of the change in this
#' release. To transform the percentage, apply the function to the value
#' returned.
#'
#' Without \code{x_range}, every curve is read over the whole concentration
#' series, as in \code{\link{ecx.bayesnechurdlefit}}. The range sets the
#' lowest predicted response that \code{type = "range"} measures towards. The
#' curve is read at \code{nsec} by linear interpolation between the two grid
#' points either side of it. An \code{nsec} outside the range is refused with a
#' message naming the range, for the combined, growth and survival curves
#' alike and whether or not \code{x_range} is supplied. Supply an
#' \code{x_range} that includes \code{nsec} to read the curve there.
#'
#' @return A vector of estimates.
#'
#' @importFrom chk chk_number
#'
#' @method ecnsec bayesnechurdlefit
#'
#' @export
ecnsec.bayesnechurdlefit <- function(object, nsec, resolution = 200,
                                     x_range = NA,
                                     type = "absolute", xform = identity,
                                     prob_vals = c(0.5, 0.025, 0.975), ...,
                                     posterior = FALSE, which = "combined") {
  which <- hurdle_check_which(which)
  check_removed_args(list(...))
  # One value: the curve is interpolated at one point.
  chk_number(nsec)
  type <- validate_ecx_type(type, match.call())
  if (identical(type, "direct")) {
    stop("type = \"direct\" names a response value rather than a ",
         "percentage, so there is no percent effect for ecnsec to report. ",
         "Use type = \"absolute\" (the default) or \"range\".", call. = FALSE)
  }
  if (identical(type, "relative")) {
    # Refused for the reason ecx.bayesnechurdlefit() gives.
    stop("type = \"relative\" is not defined for a hurdle fit, whose ",
         "asymptote is not a single fitted parameter. Use ",
         "type = \"absolute\" or type = \"range\".", call. = FALSE)
  }
  if (!inherits(xform, "function")) {
    stop("xform must be a function.")
  }
  if (!identical(xform, identity)) {
    # Warn for one release that xform now applies to nsec rather than to the
    # percentage returned, then remove this warning. It is a classed
    # warning() because lifecycle is not in Imports.
    warning(structure(
      class = c("bayesnec_ecnsec_xform_changed", "warning", "condition"),
      list(message = paste0(
        "xform in ecnsec() on a hurdle pair is now applied to nsec before ",
        "the curve is read, as it is on every other class. In earlier ",
        "versions it was applied to the percentage returned. To transform the ",
        "percentage, apply the function to the value ecnsec() returns."
      ), call = NULL)
    ))
  }
  # As in ecnsec.bnecfit().
  quiet <- report_fitted_scale(object, xform, "ecnsec")
  on.exit(options(quiet), add = TRUE)
  nsec_use <- xform(nsec)
  # The grid ecx() and nsec() read, so that all three read one curve.
  preds <- hurdle_component_preds(object, resolution = resolution,
                                  x_range = x_range)
  # Check nsec against the grid the curve was predicted over, so that the range
  # refused and the range read cannot differ.
  check_ecnsec_range(object, nsec, nsec_use, range(preds$x), x_range, xform,
                     curve = paste("the", which, "curve"))
  p_samples <- preds[[which]]
  # The percentage effect at nsec, against the reference ecx() uses for the
  # same type, with the control at the lowest observed concentration.
  control <- preds$control[[which]]
  floor_draws <- switch(type,
                        absolute = 0,
                        range = apply(p_samples, 1, min, na.rm = TRUE))
  at_nsec <- curve_draws_at(p_samples, preds$x, nsec_use)
  out <- (control - at_nsec) / (control - floor_draws) * 100
  estimate <- quantile(out, probs = prob_vals, na.rm = TRUE)
  names(estimate) <- clean_names(estimate)
  attr(estimate, "component") <- which
  if (!posterior) estimate else out
}

#' Each posterior draw of a curve read at one predictor value
#'
#' Interpolates between grid points rather than taking the nearest one.
#' At a grid point, returns its column exactly; outside the grid, returns
#' the nearer end.
#'
#' @param p_samples A \code{\link[base]{matrix}} with draws as rows and grid
#' points as columns.
#' @param x The increasing grid the columns are predicted at.
#' @param at A single predictor value.
#'
#' @return A \code{\link[base]{numeric}} vector, one value per draw.
#' @noRd
curve_draws_at <- function(p_samples, x, at) {
  n <- length(x)
  if (at <= x[1]) {
    return(p_samples[, 1])
  }
  if (at >= x[n]) {
    return(p_samples[, n])
  }
  i <- findInterval(at, x)
  if (x[i] == at) {
    return(p_samples[, i])
  }
  w <- (at - x[i]) / (x[i + 1] - x[i])
  (1 - w) * p_samples[, i] + w * p_samples[, i + 1]
}

#' The prediction grid a hurdle summary reads its ECx rows over
#'
#' The grid stored with the survival component, the whole concentration series,
#' which a bare \code{\link{ecx}} also reads. A \code{\link{bayesmanecfit}}
#' stores it in \code{w_pred_vals} and a \code{\link{bayesnecfit}} in
#' \code{pred_vals}, so both are read.
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#'
#' @return A \code{\link[base]{numeric}} vector of length 2, or \code{NULL}
#' where the survival component has no stored grid.
#' @noRd
hurdle_summary_range <- function(object) {
  x <- object$survival$w_pred_vals$data$x
  if (is.null(x)) {
    x <- object$survival$pred_vals$data$x
  }
  if (is.null(x) || !length(x)) {
    return(NULL)
  }
  c(min(x), max(x))
}
