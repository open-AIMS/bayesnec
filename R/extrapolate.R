#' The prediction range a stored no-effect estimate is censored at
#'
#' On the recorded predictor scale, which is the scale \code{x_range} is given
#' on and the scale \code{extrapolate} is read on.
#'
#' A grid point is kept only where it is finite on the fitted scale as well as
#' on the recorded one, as \code{expand_nec()} does for its own bounds. Under
#' \code{crf(log(x))} an \code{x_range} reaching zero is finite when recorded
#' and \code{-Inf} when fitted.
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
    # Use the survival component, as newdata_eval() does. The growth component
    # stops short of any concentration where nothing survived.
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
#' This is the range \code{extrapolate} must not narrow. Where the caller named
#' an \code{x_range}, it is that range at both ends, since anything wider would
#' discard their lower bound. Otherwise it is the wider of the range this call
#' searches and the prediction range the fit stores. Those differ for a fit
#' expanded over a grid above the data, where \code{\link{nsec}} searches the
#' observed range only. Taking the wider makes \code{\link{nec}} and
#' \code{\link{nsec}} refuse the same limits.
#'
#' @param object A fit.
#' @param x_range The caller's \code{x_range}.
#'
#' @return A \code{\link[base]{list}} with elements \code{lower} and
#' \code{upper}.
#' @noRd
searched_or_stored_bounds <- function(object, x_range) {
  search <- grid_x_range(object, x_range)
  if (!all(is.na(x_range))) {
    return(search)
  }
  stored <- ne_grid_bounds(object)
  if (is.null(stored)) {
    return(search)
  }
  list(lower = min(search$lower, stored$lower),
       upper = max(search$upper, stored$upper))
}

#' Resolve \code{extrapolate} into the pair of limits it names
#'
#' \code{extrapolate} is read on the recorded predictor scale, the scale of the
#' data and of \code{x_range}, not the scale \code{crf()} fits on or the scale
#' \code{xform} displays. A limit given on the fitted scale by mistake falls
#' inside the prediction range under any increasing transformation, so the
#' check below refuses it.
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
    # `bounds` is first evaluated here, so that extrapolate = FALSE returns
    # before the object is read. Do not refer to it above this point.
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
#' Draws are compared on the scale \code{crf()} fitted. A decreasing
#' \code{crf()} transformation maps the top of the recorded range to the foot
#' of the fitted one, so the limits are transformed and then reordered. An
#' infinite limit stays infinite, with the sign the finite grid bounds give it.
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
#' For a threshold posterior, every draw of which has a value. Releasing the
#' posterior to wider limits means censoring each draw against them afresh; no
#' curve is re-evaluated. An infinite limit leaves the record empty, so the
#' summary reverts to the uncensored one.
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
#' Read off the prior stored on the fit rather than recomputed from the data,
#' so that a bound the user supplied is found.
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
  # Take the smallest bound over the rows, not the first row. A joint two-block
  # fit has a nec in each block, and the reported estimate is their per-draw
  # minimum, to which the smaller of each pair of bounds applies.
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
  # brms stores a bound as text at fifteen significant digits, so a bound set
  # from the grid parses back slightly either side of it. Without the
  # tolerance, a limit at exactly the grid bound is reported as blocked.
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
#' \code{warn_censored_draws()} names the bounds. Under \code{extrapolate} a
#' bound is the limit the caller named, not the end of the prediction range.
#' The flag is set by the branches that replace the record, not inferred from
#' \code{extrapolate}: a limit equal to the current bounds, or a fit with
#' nothing censored, returns the stored record, whose bounds are still the
#' prediction range.
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
#' \code{\link{nsec}}'s default of 200. It is a count, not a spacing, so the
#' grid over a wider range is coarser.
#'
#' @noRd
stored_resolution <- function(object) {
  out <- nrow(ne_grid_data(object))
  if (is.null(out) || !is.finite(out) || out < 2) 200 else out
}

#' The no-effect posterior of a single fit, released to a pair of limits
#'
#' A threshold posterior is released by comparison alone. A no-effect estimate
#' read off a curve, which on a \code{\link{bayesnecfit}} arises only for a
#' joint two-block fit with a smooth block, is re-evaluated by
#' \code{\link{nsec}} over the extended \code{x_range}, without a refit.
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
  if (!extends_range(lims, bounds) ||
      !has_censoring(attr(object$ne_posterior, "censored"))) {
    # Return the stored posterior unaltered where the limits are the
    # prediction range, or where no draw is censored: a wider bound then
    # changes nothing, and the result matches extrapolate = FALSE.
    return(object$ne_posterior)
  }
  # After the early return: advice to widen the prior has no bearing on a
  # stored estimate returned unchanged.
  report_bounded_ne_prior(
    setNames(list(ne_prior_bounds(object$fit)), object$model), fitted_lims
  )
  if (identical(necfit_ne_type(object), "NEC")) {
    out <- object$ne_posterior
    check_releasable(as.numeric(out), object$model)
    attr(out, "censored") <- recensor_sampled(as.numeric(out), fitted_lims)
    return(mark_extrapolated(out))
  }
  report_curve_read_lower_limit(object, lims)
  out <- nsec(object, sig_val = sig_val, resolution = resolution,
              x_range = c(lims$lower, lims$upper), posterior = TRUE)
  # Keep the draws and the censoring record only. The other attributes nsec()
  # attaches describe a grid the caller of nec() did not name.
  cens <- attr(out, "censored")
  out <- as.numeric(out)
  attr(out, "censored") <- cens
  mark_extrapolated(out)
}

#' One equation of a set, classed for a curve-read estimator and no more
#'
#' \code{\link{nsec}} reads only the stored \pkg{brms} fit and the formula.
#' \code{\link{pull_out}} would also run the whole of \code{expand_nec()}, all
#' of whose products this path discards.
#'
#' @param object A \code{\link{bayesmanecfit}}.
#' @param model A \code{\link[base]{character}} naming one successful equation.
#'
#' @return The stored component, classed as a \code{\link{bayesnecfit}}.
#' @noRd
component_fit <- function(object, model) {
  allot_class(object$mod_fits[[model]], c("bayesnecfit", "bnecfit"))
}

#' One equation's stored threshold posterior, without expanding the component
#'
#' \code{expand_nec()} stores a single-block threshold posterior as
#' \code{as_draws_df(fit)[["b_nec_Intercept"]]}, so it is read back directly
#' rather than through \code{\link{pull_out}}, which expands the component. A
#' joint two-block fit is the exception: its estimate is the per-draw minimum
#' of two blocks, or a value read off the combined curve, so it goes through
#' \code{\link{pull_out}}.
#'
#' @param object A \code{\link{bayesmanecfit}}.
#' @param model A \code{\link[base]{character}} naming one equation.
#'
#' @return A \code{\link[base]{numeric}} vector of draws.
#' @noRd
component_ne_posterior <- function(object, model) {
  fit <- object$mod_fits[[model]]$fit
  if (!is_hurdle_family(fit$family)) {
    return(as.numeric(as_draws_df(fit)[["b_nec_Intercept"]]))
  }
  as.numeric(suppressMessages(pull_out(object, model = model))$ne_posterior)
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
  if (!extends_range(lims, bounds) ||
      !has_censoring(attr(object$w_ne_posterior, "censored"))) {
    # As in extrapolated_necfit_ne(). This also keeps a set saved without a
    # weighted draw index from being resampled where nothing would change.
    return(object$w_ne_posterior)
  }
  # As in extrapolated_necfit_ne(), after the early return.
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
  # Slice a threshold component out of the stored mixture only where the
  # stored weighted draw index is the one in use. For an object saved without
  # it, pull_draw_index() regenerates a different index, and slicing would pair
  # the old draws with curve-read draws taken under the new index. Every
  # component is then rebuilt under the new one.
  have_stored_index <- !is.null(object$w_draw_index) &&
    all(success %in% names(object$w_draw_index))
  parts <- lapply(seq_along(success), function(i) {
    idx <- draw_index[[success[i]]]
    if (types[i] == "NEC") {
      part <- if (have_stored_index) {
        as.numeric(values[starts[i]:(starts[i] + lens[i] - 1)])
      } else {
        component_ne_posterior(object, success[i])[idx]
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
#' Derived from the equation name, as \code{expand_manec()} derives the label
#' of the set, rather than by expanding every component. For joint two-block
#' fits the name describes the response block only, so a set whose survival
#' blocks hold a smooth equation is labelled \code{"NEC"} here, as
#' \code{expand_manec()} labels it. \code{extrapolated_manec_ne()} catches that
#' case on the posterior instead, so that \code{\link{nec}} and
#' \code{\link{summary}} agree about the set.
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
#' two-block fit into account, and that record decides. The fallback is for an
#' object saved before the slot existed: a single-block fit samples its
#' threshold, since \code{\link{nec}} has already refused an \code{ecx}
#' equation, and a two-block fit is treated as read off a curve.
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
#' Releasing a threshold component compares each draw with the new bound, which
#' says nothing about a draw with no value. Such draws reach this path only
#' from a model-averaged set of joint two-block fits whose survival blocks hold
#' a smooth equation. Refuse rather than silently summarise the remaining
#' draws.
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
