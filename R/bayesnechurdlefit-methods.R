# Methods for bayesnechurdlefit, the paired object returned by bnec_hurdle().
#
# Almost every method takes a `which` argument selecting one of the three
# curves the model describes. It defaults to "combined" throughout: the
# combined endpoint is the reason for fitting a hurdle model at all -- a user
# who only wanted the components would fit them separately with bnec() and not
# reach for this wrapper.
#
# Methods fall into two groups. Those that describe a curve compute it from
# both components ("implement"); those that describe a *fit* -- convergence
# diagnostics, priors, the underlying brmsfit -- have no combined analogue and
# return one result per component ("delegate").

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
  # A supplied grid must be honoured rather than regenerated, or callers that
  # build their own -- compare_fitted() among them -- get a different number of
  # columns than the x vector they are about to pair it with. The two
  # components share the same predictor term, so one grid serves both.
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
  bnec_newdata(object$survival, resolution = resolution, x_range = x_range)
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
#' @return A vector containing the estimated NSEC value and credible bounds.
#'
#' @importFrom stats quantile
#' @importFrom chk chk_logical
#'
#' @method nsec bayesnechurdlefit
#'
#' @export
nsec.bayesnechurdlefit <- function(object, sig_val = 0.01, resolution = 1000,
                                   x_range = NA, hormesis_def = "control",
                                   xform = identity,
                                   prob_vals = c(0.5, 0.025, 0.975), ...,
                                   posterior = FALSE, which = "combined") {
  chk_logical(posterior)
  if (!inherits(xform, "function")) {
    stop("xform must be a function.")
  }
  preds <- hurdle_component_preds(object, resolution = resolution,
                                  x_range = x_range)
  p_samples <- preds[[hurdle_check_which(which)]]
  reference <- quantile(p_samples[, 1], sig_val)
  out <- apply(p_samples, 1, nsec_fct, reference = reference,
               x_vec = preds$x)
  out <- hurdle_xform_x(object, out)
  if (inherits(xform, "function")) {
    out <- xform(out)
  }
  estimate <- quantile(out, probs = prob_vals)
  names(estimate) <- clean_names(estimate)
  attr(estimate, "toxicity_estimate") <- "nsec"
  attr(estimate, "component") <- hurdle_check_which(which)
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
  x_str <- grep("crf(", labels(terms(object$formula)), fixed = TRUE,
                value = TRUE)
  x_call <- str2lang(eval(parse(text = x_str)))
  if (inherits(x_call, "call")) {
    x_call[[2]] <- str2lang("out")
    out <- eval(x_call)
  }
  out
}

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

#' Summary of a \code{\link{bayesnechurdlefit}}
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#' @param ... Passed to \code{\link{nec}} and \code{\link{ecx}} for each
#' component, so that \code{xform} in particular applies to every estimate in
#' the table.
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
  ecs <- NULL
  if (ecx) {
    ecs <- lapply(c("combined", "growth", "survival"), function(w) {
      out <- lapply(ecx_vals, function(v) {
        ecx(object, ecx_val = v, which = w, ...)
      })
      names(out) <- paste0("ec", ecx_vals)
      out
    })
    names(ecs) <- c("combined", "growth", "survival")
  }
  # nec() warns once per call that a mixed model set yields an N(S)EC rather
  # than a NEC, which would be repeated up to six times here. The table labels
  # every estimate by type, so the message is redundant and is suppressed.
  nes <- lapply(c("combined", "growth", "survival"), function(w) {
    suppressMessages(nec(object, which = w, ...))
  })
  names(nes) <- c("combined", "growth", "survival")
  mods <- function(f) {
    if (inherits(f, "bayesmanecfit")) names(f$mod_fits) else f$model
  }
  # ne_type is what distinguishes a NEC from a NSEC or a mixture of the two,
  # and each component carries its own. The combined estimate is the smaller of
  # the two per draw, so it is a pure NEC only when both components are.
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
    growth_family = growth_fit$family$family,
    growth_models = mods(object$growth),
    survival_models = mods(object$survival),
    ne = nes, ne_types = ne_types, ecs = ecs,
    growth_averaged = inherits(object$growth, "bayesmanecfit"),
    survival_averaged = inherits(object$survival, "bayesmanecfit")
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
  # One matrix rather than one line per component: a per-line cat() prefix puts
  # the label above the column headings rather than beside the values.
  cat("No-effect toxicity estimates\n")
  ne_mat <- do.call(rbind, x$ne)
  tp <- x$ne_types[names(x$ne)]
  rownames(ne_mat) <- ifelse(is.na(tp), names(x$ne),
                             paste0(names(x$ne), " (", tp, ")"))
  print_mat(ne_mat)
  if (any(x$ne_types != "NEC", na.rm = TRUE)) {
    cat("\nNSEC values appear where a model set contains smooth (ECx) models,",
        "which\ncarry no threshold parameter; N(S)EC is a model-averaged",
        "combination\nof the two (Fisher et al. 2023).\n")
  }
  if (!is.null(x$ecs)) {
    cat("\nECx estimates\n")
    ec_mat <- do.call(rbind, lapply(x$ecs, function(z) do.call(rbind, z)))
    rownames(ec_mat) <- unlist(lapply(names(x$ecs), function(w) {
      paste0(w, " ", names(x$ecs[[w]]))
    }))
    print_mat(ec_mat)
  }
  cat("\nThe combined endpoint is the expected response per individual",
      "exposed,\ni.e. growth * survival. Use which = to select a component.\n")
  invisible(x)
}

# ---------------------------------------------------------------------------
# Delegating methods
#
# These describe a model *fit* rather than a curve. The combined endpoint is
# not a fit -- it is derived from two -- so there is nothing to return for it,
# and each yields one element per component instead.
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
rhat.bayesnechurdlefit <- function(x, ..., rhat_cutoff = 1.05) {
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
# All delegate to both components. The pair must stay in step: an object whose
# growth and survival fits describe different data, or different model sets
# than the user thinks, is worse than no method at all.
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
                                    timeout = Inf) {
  args <- list(x_range = x_range, resolution = resolution, sig_val = sig_val,
               prior_type = prior_type, timeout = timeout)
  if (!missing(priors)) args$priors <- priors
  if (!missing(drop)) args$drop <- drop
  if (!missing(add)) args$add <- add
  if (!missing(loo_controls)) args$loo_controls <- loo_controls
  amend_part <- function(part, label) {
    tryCatch(do.call(amend, c(list(part), args)), error = function(e) {
      stop("Could not amend the ", label, " component: ",
           conditionMessage(e),
           if (inherits(part, "bayesnecfit")) {
             paste0("\n  Note amend() requires a model-averaged fit; this",
                    " component holds a single model. Refit that component",
                    " with more than one model in crf().")
           } else "", call. = FALSE)
    })
  }
  hurdle_rewrap(object, amend_part(object$growth, "growth"),
                amend_part(object$survival, "survival"))
}

#' Updates both components of a \code{\link{bayesnechurdlefit}}
#'
#' @inheritParams update.bnecfit
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#' @param newdata Optional new data. Split into the two component datasets the
#' same way \code{\link{bnec_hurdle}} does, so zeros continue to denote the
#' hurdle rather than being modelled as small responses.
#'
#' @return An object of class \code{\link{bayesnechurdlefit}}.
#'
#' @importFrom stats update
#'
#' @method update bayesnechurdlefit
#'
#' @export
update.bayesnechurdlefit <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    return(hurdle_rewrap(object, update(object$growth, ...),
                         update(object$survival, ...)))
  }
  y <- newdata[[object$y_var]]
  if (is.null(y)) {
    stop("\"newdata\" must contain the response column \"", object$y_var,
         "\".", call. = FALSE)
  }
  surv_data <- newdata
  surv_data[[".alive"]] <- as.integer(y > 0)
  out <- hurdle_rewrap(
    object,
    update(object$growth, newdata = newdata[y > 0, , drop = FALSE], ...),
    update(object$survival, newdata = surv_data, ...)
  )
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
# The default shows the combined endpoint alone, because that is what the
# hurdle model exists to estimate. which = "all" adds the two components as
# separate panels rather than overlaying them: growth is in response units and
# survival is a probability, so a single pair of axes would mean two y-scales,
# which misleads about relative magnitude.
# ---------------------------------------------------------------------------

#' @noRd
hurdle_check_plot_which <- function(which) {
  match.arg(which, c("combined", "growth", "survival", "all"))
}

#' Raw observed data underlying a \code{\link{bayesnechurdlefit}}
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#'
#' @return A \code{\link[base]{data.frame}} with columns \code{x} and \code{y}.
#'
#' @importFrom stats terms
#'
#' @noRd
hurdle_raw_data <- function(object) {
  x_str <- grep("crf(", labels(terms(object$formula)), fixed = TRUE,
                value = TRUE)
  x_expr <- str2lang(eval(parse(text = x_str)))
  data.frame(x = eval(x_expr, object$data),
             y = object$data[[object$y_var]])
}

#' Plot data for one panel of a \code{\link{bayesnechurdlefit}}
#'
#' @noRd
hurdle_panel <- function(object, which, resolution, x_range) {
  preds <- hurdle_component_preds(object, resolution = resolution,
                                  x_range = x_range)
  p <- preds[[which]]
  raw <- hurdle_raw_data(object)
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
#' @param xform A function to apply to the predictor.
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
  for (w in panels) {
    p <- hurdle_panel(x, w, resolution, x_range)
    xv <- xform(p$x)
    plot(xform(p$raw$x), p$raw$y, pch = 16, cex = 1.5,
         col = adjustcolor(1, alpha.f = 0.25),
         ylab = if (is.null(ylab)) p$ylab else ylab, xlab = xlab,
         ylim = range(c(p$raw$y, p$lo, p$up), na.rm = TRUE), ...)
    if (CI) {
      lines(xv, p$up, lty = 2)
      lines(xv, p$lo, lty = 2)
    }
    lines(xv, p$est)
    if (add_nec) {
      ne <- xform(nec(x, which = if (w == "combined") "combined" else w))
      abline(v = ne, col = "red", lty = c(1, 3, 3))
      legend("topright", bty = "n", lty = 1, col = "red",
             legend = paste0("N(S)EC: ", signif(ne[1], 2), " (",
                             signif(ne[2], 2), "-", signif(ne[3], 2), ")"))
    }
  }
  invisible(x)
}

#' Data underlying a \code{\link{bayesnechurdlefit}} plot
#'
#' @param x An object of class \code{\link{bayesnechurdlefit}}.
#' @param add_nec Unused; retained for consistency with the generic.
#' @param add_ecx Unused; retained for consistency with the generic.
#' @param xform A function to apply to the predictor.
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
  curve <- list()
  raw <- list()
  for (w in panels) {
    p <- hurdle_panel(x, w, resolution, x_range)
    curve[[w]] <- data.frame(panel = p$ylab, x = xform(p$x), y = p$est,
                             lo = p$lo, up = p$up)
    raw[[w]] <- data.frame(panel = p$ylab, x = xform(p$raw$x), y = p$raw$y)
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
#' @return A vector of estimates.
#'
#' @method ecnsec bayesnechurdlefit
#'
#' @export
ecnsec.bayesnechurdlefit <- function(object, nsec, resolution = 10,
                                     x_range = NA, hormesis_def = "control",
                                     type = "absolute", xform = identity,
                                     prob_vals = c(0.5, 0.025, 0.975), ...,
                                     posterior = FALSE, which = "combined") {
  which <- hurdle_check_which(which)
  preds <- hurdle_component_preds(object, resolution = resolution,
                                  x_range = x_range)
  p_samples <- preds[[which]]
  # ecnsec asks: what percentage effect does a given predictor value
  # correspond to? Read off the same curve everything else uses.
  out <- apply(p_samples, 1, function(p) {
    100 * (1 - p[which.min(abs(preds$x - nsec))] / p[1])
  })
  if (inherits(xform, "function")) {
    out <- xform(out)
  }
  estimate <- quantile(out, probs = prob_vals)
  names(estimate) <- clean_names(estimate)
  attr(estimate, "component") <- which
  if (!posterior) estimate else out
}
