#' The prediction range a stored no-effect estimate is censored at
#'
#' On the recorded predictor scale, which is the scale \code{x_range} is given
#' on and the scale \code{extrapolate} is read on.
#'
#' A grid point is kept only where it is finite on the fitted scale as well as
#' on the recorded one. \code{expand_nec()} takes its own bounds from the
#' fitted grid with the non-finite entries dropped, because an \code{x_range}
#' reaching zero under \code{crf(log(x))} puts \code{-Inf} at the foot of it.
#' Filtering the recorded grid alone keeps that zero here, and the lower limit
#' of a single-number \code{extrapolate} defaults to this bound, so such a fit
#' refused every upper limit for naming a value the caller had not given.
#'
#' @param object A \code{\link{bayesnecfit}}, \code{\link{bayesmanecfit}} or
#' \code{\link{bayesnechurdlefit}}.
#'
#' @return A \code{\link[base]{list}} with elements \code{lower} and
#' \code{upper}, or \code{NULL} where the object stores no prediction grid.
#' @noRd
ne_grid_bounds <- function(object) {
  grid <- ne_grid_data(object)
  if (is.null(grid) || is.null(grid$x) || length(grid$x) == 0) {
    return(NULL)
  }
  x <- grid$x
  keep <- is.finite(x)
  formula <- ne_grid_formula(object)
  if (!is.null(formula)) {
    fitted <- try(suppressWarnings(sub_x_transformation(x, formula)),
                  silent = TRUE)
    if (!inherits(fitted, "try-error")) {
      keep <- keep & is.finite(fitted)
    }
  }
  if (!any(keep)) {
    return(NULL)
  }
  list(lower = min(x[keep]), upper = max(x[keep]))
}

#' @noRd
ne_grid_data <- function(object) {
  if (is_bayesnechurdlefit(object)) {
    # The survival component, which is the one newdata_eval() takes the grid
    # from: it sees every row and therefore the full exposed predictor range,
    # whereas the growth component stops short of any concentration where
    # nothing survived.
    return(ne_grid_data(object$survival))
  }
  if (inherits(object, "bayesmanecfit")) {
    return(object$w_pred_vals$data)
  }
  object$pred_vals$data
}

#' @noRd
ne_grid_formula <- function(object) {
  if (is_bayesnechurdlefit(object)) {
    return(ne_grid_formula(object$survival))
  }
  if (inherits(object, "bayesmanecfit")) {
    return(object$mod_fits[[object$success_models[1]]]$bayesnecformula)
  }
  object$bayesnecformula
}

#' The predictor range a curve-read estimator will search over
#'
#' Built through the same grid constructor the estimator itself uses, at a
#' resolution of two, so that the range an \code{x_range} of \code{NA} resolves
#' to is taken from that constructor rather than restated here.
#'
#' @param object A fit \code{\link{bnec_newdata}} has a method for.
#' @param x_range The caller's \code{x_range}.
#'
#' @return A \code{\link[base]{list}} with elements \code{lower} and
#' \code{upper}.
#' @noRd
grid_x_range <- function(object, x_range) {
  x <- newdata_eval(object, resolution = 2, x_range = x_range)$x_vec
  list(lower = min(x), upper = max(x))
}

#' The range a limit given to a curve-read estimator is measured against
#'
#' The wider of two: the range this call will search, and the prediction range
#' the fit itself stores. They are the same for a fit built with no
#' \code{x_range}, and they differ for one built with a grid above the data,
#' where \code{\link{nsec}} with no \code{x_range} of its own returns to the
#' observed range. Measured on a fit re-expanded with
#' \code{x_range = c(0.0324, 8)}: the search range ends at 3.22, so
#' \code{extrapolate = 5} read against it alone was accepted and censored the
#' estimate at 5, inside the grid the fit itself stores, which is the silent
#' tightening the argument exists to refuse. Taking the wider of the two makes
#' \code{\link{nec}} and \code{\link{nsec}} refuse the same numbers, which is
#' what specification 4.6 asks for.
#'
#' @param object A fit.
#' @param x_range The caller's \code{x_range}.
#'
#' @return A \code{\link[base]{list}} with elements \code{lower} and
#' \code{upper}.
#' @noRd
searched_or_stored_bounds <- function(object, x_range) {
  search <- grid_x_range(object, x_range)
  stored <- ne_grid_bounds(object)
  if (is.null(stored)) {
    return(search)
  }
  list(lower = min(search$lower, stored$lower),
       upper = max(search$upper, stored$upper))
}

#' Resolve \code{extrapolate} into the pair of limits it names
#'
#' \code{extrapolate} is read on the recorded predictor scale, the scale the
#' data were supplied on and the scale \code{x_range} takes, not the scale
#' \code{crf()} fits on and not the scale \code{xform} displays. A limit given
#' on the fitted scale where the recorded one is wanted is smaller than the top
#' of the prediction range under every increasing transformation, so it is
#' refused by the check below rather than acted on.
#'
#' @param extrapolate The caller's argument.
#' @param bounds The current prediction range, from \code{ne_grid_bounds()} or
#' \code{grid_x_range()}.
#' @param ne_types A \code{\link[base]{character}} vector, one entry per
#' component of the reported estimate, each \code{"NEC"}, \code{"NSEC"} or
#' \code{"N(S)EC"}.
#' @param components A \code{\link[base]{character}} vector naming those
#' components, used in the error raised for an infinite limit.
#'
#' @return \code{NULL} where \code{extrapolate} is \code{FALSE}, otherwise a
#' \code{\link[base]{list}} with elements \code{lower} and \code{upper} on the
#' recorded predictor scale.
#' @noRd
extrapolate_limits <- function(extrapolate, bounds, ne_types,
                               components = NULL) {
  bad <- paste0("extrapolate must be FALSE, TRUE, a single upper limit, or a ",
                "pair of lower and upper limits.")
  if (is.logical(extrapolate)) {
    if (length(extrapolate) != 1 || is.na(extrapolate)) {
      stop(bad, call. = FALSE)
    }
    if (!extrapolate) {
      return(NULL)
    }
    lims <- list(lower = -Inf, upper = Inf)
  } else if (is.numeric(extrapolate) && length(extrapolate) %in% c(1, 2) &&
             !anyNA(extrapolate)) {
    # Forced only here, so that extrapolate = FALSE reaches nothing that reads
    # the object. A fit with no stored prediction grid took min() and max()
    # over an empty vector on the default path and warned twice at each end.
    if (is.null(bounds)) {
      stop("extrapolate is measured against a prediction range and this ",
           "object stores none, so there is nothing to extend. Set the grid ",
           "with x_range instead.", call. = FALSE)
    }
    lims <- if (length(extrapolate) == 1) {
      list(lower = bounds$lower, upper = extrapolate)
    } else {
      list(lower = extrapolate[1], upper = extrapolate[2])
    }
  } else {
    stop(bad, call. = FALSE)
  }
  if (lims$lower == lims$upper) {
    stop("extrapolate names ", signif(lims$upper, 5), " at both ends, which ",
         "is a range of no width. Give a lower limit below the upper one.",
         call. = FALSE)
  }
  if (lims$lower > lims$upper) {
    stop("extrapolate names a lower limit of ", signif(lims$lower, 5),
         " and an upper limit of ", signif(lims$upper, 5),
         ". Give the lower limit first.", call. = FALSE)
  }
  if (!is.finite(lims$lower) || !is.finite(lims$upper)) {
    curve_read <- ne_types != "NEC"
    if (any(curve_read)) {
      named <- if (is.null(components)) {
        "The estimate is"
      } else {
        paste0("The estimate contributed by ",
               paste0(components[curve_read], collapse = ", "), " is")
      }
      stop("An infinite extrapolation limit is available only where every ",
           "component of the estimate samples a NEC. ", named, " read off a ",
           "fitted curve, and a curve cannot be evaluated on an infinite ",
           "grid. Name a finite limit instead -- extrapolate = <upper>, or ",
           "extrapolate = c(<lower>, <upper>) -- and the curve is ",
           "re-evaluated out to it.", call. = FALSE)
    }
  }
  if (is.null(bounds)) {
    stop("extrapolate is measured against a prediction range and this object ",
         "stores none, so there is nothing to extend. Set the grid with ",
         "x_range instead.", call. = FALSE)
  }
  if (lims$upper < bounds$upper) {
    stop("extrapolate names an upper limit of ", signif(lims$upper, 5),
         ", inside the prediction range, which ends at ",
         signif(bounds$upper, 5), ". extrapolate only extends that range; ",
         "narrow it with x_range instead.", call. = FALSE)
  }
  if (lims$lower > bounds$lower) {
    stop("extrapolate names a lower limit of ", signif(lims$lower, 5),
         ", inside the prediction range, which begins at ",
         signif(bounds$lower, 5), ". extrapolate only extends that range; ",
         "narrow it with x_range instead.", call. = FALSE)
  }
  lims
}

#' Put a pair of extrapolation limits on the fitted predictor scale
#'
#' The comparison against a sampled draw happens on the scale the draw is on,
#' which is the scale \code{crf()} fitted (specification 4.8). A decreasing
#' \code{crf()} takes the top of the recorded scale to the foot of the fitted
#' one, so the two limits are transformed and then ordered rather than kept
#' under the names they arrived with. An infinite recorded limit stays infinite, with its sign
#' decided by the direction the finite grid bounds map in.
#'
#' @param lims A pair of limits from \code{extrapolate_limits()}.
#' @param bounds The current prediction range, on the recorded scale.
#' @param formula A \code{\link{bayesnecformula}}.
#'
#' @return A \code{\link[base]{list}} with elements \code{lower} and
#' \code{upper} on the fitted scale.
#' @noRd
fitted_extrapolate_limits <- function(lims, bounds, formula) {
  reference <- sub_x_transformation(c(bounds$lower, bounds$upper), formula)
  decreasing <- reference[2] < reference[1]
  one <- function(value, end) {
    if (!is.finite(value)) {
      return(if (xor(identical(end, "upper"), decreasing)) Inf else -Inf)
    }
    out <- suppressWarnings(sub_x_transformation(value, formula))
    if (!is.finite(out)) {
      stop("The extrapolation limit ", signif(value, 5), " is ", out,
           " on the scale the model was fitted on, so no curve can be ",
           "evaluated there and no draw can be compared against it. Name a ",
           "limit inside the domain of the crf() transformation.",
           call. = FALSE)
    }
    out
  }
  values <- c(one(lims$lower, "lower"), one(lims$upper, "upper"))
  list(lower = min(values), upper = max(values))
}

#' Record which sampled draws lie beyond a pair of limits
#'
#' For a threshold posterior, whose every draw has a value. Releasing it to a
#' wider limit is a comparison and nothing more; no curve is re-evaluated. An
#' infinite limit leaves the record empty, so the summary reverts to the
#' uncensored one.
#'
#' @param values A \code{\link[base]{numeric}} vector of draws, on the fitted
#' scale.
#' @param lims A pair of limits on the fitted scale.
#'
#' @return A record from \code{censoring_record()}.
#' @noRd
recensor_sampled <- function(values, lims) {
  censoring_record(lims$upper, lims$lower,
                   values >= lims$upper, values <= lims$lower)
}

#' The bounds a fit's stored nec prior places on its threshold posterior
#'
#' Read off the prior stored on the fitted object rather than recomputed from
#' the data, so a bound a user supplied is handled (specification 2.3).
#'
#' @param fit A \code{\link[brms]{brmsfit}}.
#'
#' @return A \code{\link[base]{list}} with elements \code{lb} and \code{ub},
#' either of which may be \code{NA}, or \code{NULL} where the fit declares no
#' nec parameter.
#' @noRd
ne_prior_bounds <- function(fit) {
  prior <- fit$prior
  if (is.null(prior) || !all(c("nlpar", "lb", "ub") %in% names(prior))) {
    return(NULL)
  }
  rows <- prior[!is.na(prior$nlpar) & prior$nlpar == "nec", , drop = FALSE]
  if (nrow(rows) == 0) {
    return(NULL)
  }
  # The tightest bound across the rows, not the first of them. A joint
  # two-block fit declares a nec parameter in each block and the combined
  # estimate is their per-draw minimum, which is at or below the smaller upper
  # bound and at or above the smaller lower bound, so those are the two values
  # true of the quantity being reported. For a single-block fit there is one
  # row pair and this selects it.
  tightest <- function(values) {
    values <- values[!is.na(values) & nzchar(values)]
    values <- suppressWarnings(as.numeric(values))
    values <- values[!is.na(values)]
    if (length(values) == 0) NA_real_ else min(values)
  }
  list(lb = tightest(rows$lb), ub = tightest(rows$ub))
}

#' State that the fit itself stops where the extrapolation was asked to go
#'
#' A nec prior truncated at the prediction range holds every draw inside it, so
#' a wider limit returns the same truncated posterior and is not an
#' extrapolation. Raised only where the limit asked for is beyond the bound,
#' because a bound the request does not reach changes nothing about what is
#' reported.
#'
#' @param priors A named \code{\link[base]{list}} of records from
#' \code{ne_prior_bounds()}, one per equation.
#' @param lims The requested limits, on the fitted scale.
#'
#' @return \code{NULL}, invisibly. Called for the message.
#' @noRd
report_bounded_ne_prior <- function(priors, lims) {
  # brms stores a prior bound as the character form of the number, at fifteen
  # significant digits, so a bound taken from the prediction grid parses back a
  # few units in the last place either side of the grid value it was set from.
  # Without the tolerance, a request that reaches exactly the grid bound -- the
  # default lower limit of a single-number extrapolate, which extends nothing
  # at that end -- was reported as blocked by the prior.
  binds <- function(bound, asked) {
    vapply(priors, function(p) {
      !is.null(p) && is.finite(p[[bound]]) &&
        asked(p[[bound]], 1e-8 * max(abs(p[[bound]]), 1))
    }, logical(1))
  }
  above <- binds("ub", function(b, tol) lims$upper > b + tol)
  below <- binds("lb", function(b, tol) lims$lower < b - tol)
  if (any(above)) {
    values <- vapply(priors[above], function(p) p$ub, numeric(1))
    message("The nec prior on ", paste0(names(priors)[above], collapse = ", "),
            " is bounded above at ", signif(max(values), 3),
            ", so its posterior holds no draw above that value and ",
            "extrapolate cannot report one. The fit itself is bounded: refit ",
            "with a wider bound on the nec prior for the estimate to reach ",
            "further.")
  }
  if (any(below)) {
    values <- vapply(priors[below], function(p) p$lb, numeric(1))
    message("The nec prior on ", paste0(names(priors)[below], collapse = ", "),
            " is bounded below at ", signif(min(values), 3),
            ", so its posterior holds no draw below that value.")
  }
  invisible(NULL)
}

#' State that a lower limit does not extend a curve-read search
#'
#' An NSEC is the concentration at which a curve reaches a quantile of the
#' control posterior, so the control is both the reference and the point the
#' search starts from. A limit below the lowest observed concentration
#' therefore extends the grid without extending the search, and the draws that
#' had already passed the reference at the control stay censored there.
#'
#' @param object The fit the search runs on.
#' @param lims The requested limits, on the recorded scale.
#'
#' @return \code{NULL}, invisibly. Called for the message.
#' @noRd
report_curve_read_lower_limit <- function(object, lims) {
  start <- control_x(object)
  if (lims$lower < start) {
    message("A curve-read estimate is measured from the control, so the ",
            "search cannot begin below ", signif(start, 3),
            ", the lowest observed concentration. The lower limit extends ",
            "the prediction grid without extending that search, and those ",
            "components stay censored at the control.")
  }
  invisible(NULL)
}

#' Refuse \code{extrapolate} on a class that keeps no prediction range
#'
#' A \code{\link[brms]{brmsfit}} or a \code{drc} fit reaching these methods was
#' not built by \code{\link{bnec}}, so there is no stored grid to measure a
#' limit against and no censoring record to place one on. \code{x_range} sets
#' the grid for such an object and is the argument to use, with the checks
#' \code{extrapolate} adds unavailable.
#'
#' @param extrapolate The caller's argument.
#' @param what A \code{\link[base]{character}} naming the class.
#'
#' @return \code{TRUE}, invisibly.
#' @noRd
check_no_extrapolate <- function(extrapolate, what) {
  if (!identical(extrapolate, FALSE)) {
    stop("extrapolate is measured against the prediction range a bnec() fit ",
         "stores, and a ", what, " object holds no such range. Set the grid ",
         "with x_range instead.", call. = FALSE)
  }
  invisible(TRUE)
}

#' What the two bounds of a censoring record are the ends of
#'
#' \code{warn_censored_draws()} names them, and its default is right wherever
#' the record came from the grid the estimate was read on. Under
#' \code{extrapolate} the bound is the limit the caller named, which is
#' somewhere the prediction range does not reach, so calling it the end of that
#' range states the wrong number twice over.
#'
#' The flag is put on the posterior by the branches that replace the record,
#' rather than derived from whether \code{extrapolate} was given: a
#' limit equal to the current bounds, and a limit on a fit with nothing
#' censored, both return the stored record and are still the prediction range.
#'
#' @param values A posterior, possibly marked by an extrapolation.
#'
#' @return A \code{\link[base]{character}} value.
#' @noRd
censoring_range_label <- function(values) {
  if (isTRUE(attr(values, "extrapolated"))) {
    "extrapolation range"
  } else {
    "prediction range"
  }
}

#' @noRd
mark_extrapolated <- function(values) {
  attr(values, "extrapolated") <- TRUE
  values
}

#' Whether a pair of limits reaches beyond the current prediction range
#'
#' @noRd
extends_range <- function(lims, bounds) {
  lims$upper > bounds$upper || lims$lower < bounds$lower
}

#' The resolution a re-evaluated curve is read on
#'
#' The number of points in the grid the fit stored, rather than
#' \code{\link{nsec}}'s lighter default of 200. It is a count and not a
#' spacing, so over a range wider than the stored one the re-read grid is
#' proportionally coarser; the roxygen of \code{\link{nec}} says so, and a
#' caller who needs a given spacing passes \code{resolution}.
#'
#' @noRd
stored_resolution <- function(object) {
  out <- nrow(ne_grid_data(object))
  if (is.null(out) || !is.finite(out) || out < 2) 200 else out
}

#' The no-effect posterior of a single fit, released to a pair of limits
#'
#' A threshold posterior is released by comparison alone. A no-effect estimate
#' read off a curve -- which on a \code{\link{bayesnecfit}} arises only for a
#' joint two-block fit with a smooth block -- is re-evaluated on a grid
#' extended to the limits, through the \code{x_range} argument
#' \code{\link{nsec}} already takes, which recomputes from the stored fit and
#' needs no refit.
#'
#' @param object A \code{\link{bayesnecfit}}.
#' @param lims A pair of limits on the recorded scale.
#' @param bounds The current prediction range, on the recorded scale.
#' @param sig_val,resolution Passed to \code{\link{nsec}} for the
#' re-evaluation.
#'
#' @return The posterior, with its censoring record attached.
#' @noRd
extrapolated_necfit_ne <- function(object, lims, bounds, sig_val, resolution) {
  fitted_lims <- fitted_extrapolate_limits(lims, bounds,
                                           object$bayesnecformula)
  report_bounded_ne_prior(
    setNames(list(ne_prior_bounds(object$fit)), object$model), fitted_lims
  )
  if (!extends_range(lims, bounds) ||
      !has_censoring(attr(object$ne_posterior, "censored"))) {
    # Two cases with one answer. The limits may be the prediction range, in
    # which case there is nothing to release and nothing to re-evaluate. Or no
    # draw may lie beyond either end, in which case every draw was identified
    # inside the range the fit used, a wider bound censors none of them, and a
    # wider grid has no draw left to identify. Returned unaltered in both, so
    # that extrapolating a fit with nothing censored reports what
    # extrapolate = FALSE reports rather than a number recomputed for no gain.
    return(object$ne_posterior)
  }
  if (identical(necfit_ne_type(object), "NEC")) {
    out <- object$ne_posterior
    check_releasable(as.numeric(out), object$model)
    attr(out, "censored") <- recensor_sampled(as.numeric(out), fitted_lims)
    return(mark_extrapolated(out))
  }
  report_curve_read_lower_limit(object, lims)
  out <- nsec(object, sig_val = sig_val, resolution = resolution,
              x_range = c(lims$lower, lims$upper), posterior = TRUE)
  # The draws and the record, and nothing else. nsec() also attaches its own
  # sig_val, resolution and ecnsec_relativeP, which describe an NSEC read on a
  # grid the caller of nec() did not name, and which nec() has never returned.
  cens <- attr(out, "censored")
  out <- as.numeric(out)
  attr(out, "censored") <- cens
  mark_extrapolated(out)
}

#' One equation of a set, classed for a curve-read estimator and no more
#'
#' \code{\link{nsec}} reads its estimate from the stored \pkg{brms} fit and the
#' formula, on a grid it builds itself. Everything else a
#' \code{\link{bayesnecfit}} holds -- the prediction grid, the stored no-effect
#' posterior, the dispersion, the information criteria -- is built by
#' \code{expand_nec()} and none of it is read on that path.
#'
#' Reaching the component through \code{\link{pull_out}} therefore paid for a
#' full expansion whose every product was discarded: a
#' \code{\link[brms]{posterior_epred}} over a thousand grid points, a per-draw
#' root search, \code{dispersion()} and the \pkg{loo} weights, all at the
#' default grid rather than the one the caller asked for. Measured on the
#' two-equation, hundred-draw packaged fixture, \code{pull_out()} was 1.37 s of
#' the 3.39 s a finite limit took; on the twenty-three-equation default set the
#' saving is proportionally larger.
#'
#' @param object A \code{\link{bayesmanecfit}}.
#' @param model A \code{\link[base]{character}} naming one successful equation.
#'
#' @return The stored component, classed as a \code{\link{bayesnecfit}}.
#' @noRd
component_fit <- function(object, model) {
  allot_class(object$mod_fits[[model]], c("bayesnecfit", "bnecfit"))
}

#' The model-averaged no-effect posterior, released to a pair of limits
#'
#' The threshold components are released by comparison and the curve-read ones
#' are re-evaluated on the extended grid, which is what makes a finite limit
#' usable on the mixed set \code{\link{bnec}} fits by default. The two are
#' reassembled in the order the stored mixture was built in, so the combined
#' censored fraction stays the weighted one.
#'
#' @param object A \code{\link{bayesmanecfit}}.
#' @param lims A pair of limits on the recorded scale.
#' @param bounds The current prediction range, on the recorded scale.
#' @param sig_val,resolution Passed to \code{\link{nsec}} for the
#' re-evaluation.
#'
#' @return The posterior, with its censoring record attached.
#' @noRd
extrapolated_manec_ne <- function(object, lims, bounds, sig_val, resolution) {
  success <- object$success_models
  types <- manec_ne_types(object)
  formula <- object$mod_fits[[success[1]]]$bayesnecformula
  fitted_lims <- fitted_extrapolate_limits(lims, bounds, formula)
  report_bounded_ne_prior(
    setNames(lapply(seq_along(success), function(i) {
      if (types[i] == "NEC") {
        ne_prior_bounds(object$mod_fits[[success[i]]]$fit)
      } else {
        NULL
      }
    }), success),
    fitted_lims
  )
  if (!extends_range(lims, bounds) ||
      !has_censoring(attr(object$w_ne_posterior, "censored"))) {
    # As in extrapolated_necfit_ne(). The second test also keeps a set whose
    # stored weighted draw index predates #216 from being resampled where
    # there is nothing to gain: on manec_example, which has no stored index and
    # nothing censored, the rebuilt mixture read 1.449 (0.808, 1.528) against
    # the stored 1.450 (0.749, 1.527).
    return(object$w_ne_posterior)
  }
  values <- object$w_ne_posterior
  if (all(types == "NEC")) {
    check_releasable(as.numeric(values), paste0(success, collapse = ", "))
    attr(values, "censored") <- recensor_sampled(as.numeric(values),
                                                 fitted_lims)
    return(mark_extrapolated(values))
  }
  report_curve_read_lower_limit(object, lims)
  draw_index <- pull_draw_index(object, success, object$sample_size)
  lens <- vapply(success, function(m) length(draw_index[[m]]), integer(1))
  if (sum(lens) != length(values)) {
    stop("The stored model-averaged no-effect posterior holds ",
         length(values), " draws and the weighted draw index accounts for ",
         sum(lens), ", so the two cannot be matched up per equation. Rebuild ",
         "the set with c() or amend() before extrapolating.", call. = FALSE)
  }
  starts <- cumsum(lens) - lens + 1
  # Whether the index that built the stored mixture is the index being used
  # now. Where it is, a threshold component is sliced straight out of that
  # mixture and no posterior is recomputed for it. Where it is not -- an object
  # stored before the weighted index was kept, #216 -- pull_draw_index()
  # regenerates a different draw, and slicing would put those draws beside
  # curve-read draws taken under the new one. Measured on manec_example, whose
  # stored index is empty: the regenerated ecx4param draws are a different
  # seventeen of the hundred, and the mixed lower bound at the stored bounds
  # read 0.81 against the stored 0.75. Every component is therefore rebuilt
  # under the one index instead.
  have_stored_index <- !is.null(object$w_draw_index) &&
    all(success %in% names(object$w_draw_index))
  parts <- lapply(seq_along(success), function(i) {
    idx <- draw_index[[success[i]]]
    if (types[i] == "NEC") {
      part <- if (have_stored_index) {
        as.numeric(values[starts[i]:(starts[i] + lens[i] - 1)])
      } else {
        as.numeric(
          suppressMessages(pull_out(object, model = success[i]))$ne_posterior
        )[idx]
      }
      check_releasable(part, success[i])
      attr(part, "censored") <- recensor_sampled(part, fitted_lims)
      return(part)
    }
    out <- nsec(component_fit(object, success[i]), sig_val = sig_val,
                resolution = resolution,
                x_range = c(lims$lower, lims$upper), posterior = TRUE)
    part <- as.numeric(out)[idx]
    attr(part, "censored") <- subset_censoring(attr(out, "censored"), idx)
    part
  })
  out <- unlist(lapply(parts, as.numeric))
  attr(out, "censored") <- concat_censoring(lapply(parts, attr, "censored"),
                                            lens)
  mark_extrapolated(out)
}

#' The class of no-effect estimate each equation of a set contributes
#'
#' Derived from the equation name, which is how \code{expand_manec()} derives
#' the label it puts on the set, rather than by expanding every component. Two
#' things follow. The type of a single-block fit is a property of its equation
#' and needs no posterior, so this is exact and free for the sets
#' \code{\link{bnec}} usually fits. And where the components are joint
#' two-block fits, the equation name describes the response block alone, so a
#' set whose survival blocks hold a smooth equation is named \code{"NEC"} here
#' exactly as \code{expand_manec()} names it. \code{extrapolated_manec_ne()}
#' catches that case on the posterior itself rather than by second-guessing the
#' label, so that \code{\link{nec}} and \code{\link{summary}} do not disagree
#' about what the set is.
#'
#' @param object A \code{\link{bayesmanecfit}}.
#'
#' @return A \code{\link[base]{character}} vector, one entry per successful
#' equation.
#' @noRd
manec_ne_types <- function(object) {
  ifelse(object$success_models %in% mod_groups$nec, "NEC", "NSEC")
}

#' The class of no-effect estimate a single fit holds
#'
#' \code{expand_nec()} records it in \code{ne_type}, taking both blocks of a
#' two-block fit into account, and that record is the authority. The fallback
#' is for an object stored before the slot existed: a single-block fit reaching
#' \code{\link{nec}} has already had an \code{ecx}-type equation refused, so it
#' samples its threshold, while a two-block fit's combined estimate depends on
#' a block the equation name does not describe and is treated as read off a
#' curve.
#'
#' @param object A \code{\link{bayesnecfit}}.
#'
#' @return Either \code{"NEC"} or \code{"NSEC"}.
#' @noRd
necfit_ne_type <- function(object) {
  label <- object$ne_type
  if (is.character(label) && length(label) == 1) {
    return(if (identical(label, "NEC")) "NEC" else "NSEC")
  }
  if (is.null(object$hurdle) && !is_hurdle_family(object$fit$family)) {
    "NEC"
  } else {
    "NSEC"
  }
}

#' Refuse to release a posterior that has no value to release
#'
#' A threshold component is released by comparing each draw against the new
#' bound, which states nothing about a draw that has no value. Such a draw can
#' only reach this path where the class of the estimate was read from the
#' equation name and the name did not describe it --- a model-averaged set of
#' joint two-block fits whose survival blocks hold a smooth equation. Refusing
#' is the alternative to reporting the deleted-draw summary the censored
#' summary replaced, which would be silent.
#'
#' @param values A \code{\link[base]{numeric}} vector of draws.
#' @param what A \code{\link[base]{character}} naming the equation.
#'
#' @return \code{TRUE}, invisibly.
#' @noRd
check_releasable <- function(values, what) {
  if (anyNA(values)) {
    stop("The no-effect estimate of ", what, " has no value for ",
         sum(is.na(values)), " of ", length(values), " draws, so those draws ",
         "cannot be released to a wider bound by comparison. This is a ",
         "two-block fit whose second block holds a smooth equation: read it ",
         "with nsec(), which re-evaluates the curve.", call. = FALSE)
  }
  invisible(TRUE)
}
