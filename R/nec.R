#' Extracts the predicted NEC value as desired from an object of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#' @param posterior A \code{\link[base]{logical}} value indicating if the full
#' posterior sample of calculated NEC values should be returned instead of
#' just the median and 95% credible intervals.
#' @param xform \code{NULL}, the default, or a function. \code{NULL} returns
#' the estimate on the scale the predictor was recorded on. A function is
#' applied to the estimate on the scale the model was fitted on. The two
#' differ only where \code{crf()} transforms the predictor inline; see
#' Details.
#' @param prob_vals A vector indicating the probability values over which to
#' return the estimated NEC value. Defaults to 0.5 (median) and 0.025 and
#' 0.975 (95 percent credible intervals).
#' @param pool How observed group levels are combined. `"none"` returns
#' per-level estimates. `"mixture"` gives each posterior draw to one level, in
#' proportion to the levels' observation counts, and keeps it at its position,
#' so that draw \emph{i} of the mixture comes from posterior iteration
#' \emph{i}. Separate fits with different numbers of draws are instead each
#' sampled over their full posterior. `"mean_estimate"` averages the levels'
#' estimates at each draw. `"mean_curve"` is unavailable for a sampled NEC
#' parameter. A joint fit defaults to `"none"`; other fits with spanning group
#' terms default to `"mixture"`.
#' @param extrapolate The bound beyond which the estimate is reported as
#' censored. \code{FALSE}, the default, censors at the prediction range the fit
#' was built on. \code{TRUE} removes the bound at both ends. A single number is
#' an upper limit, and a pair of numbers is a lower and an upper limit, in that
#' order. See the \emph{Extrapolation} section of \code{\link{nec}}.
#' @param ... Additional arguments passed to methods. \code{sig_val} and
#' \code{resolution} are forwarded to \code{\link{nsec}} where a finite
#' \code{extrapolate} limit requires a curve to be re-evaluated, and are
#' otherwise unused.
#' @param no_effect For a \code{\link{bayesnecjointfit}} only, a
#' \code{\link[base]{logical}} value. \code{FALSE}, the default, returns
#' \code{NA} for a smooth equation with no \code{nec} parameter. A monotone
#' threshold equation returns its sampled NEC, and a hormesis threshold
#' equation returns the NSEC of its fitted curve. \code{TRUE} additionally
#' returns the NSEC of each smooth equation. The \code{ne_type} column labels
#' every estimate. The other classes reject this argument.
#'
#' @seealso \code{\link{bnec}}, \code{\link{nsec}}, \code{\link{summary}}
#'
#' @details The monotone threshold equations are \code{nec3param},
#' \code{nec4param}, \code{neclin} and \code{necsigm}. In each, the sampled
#' \code{nec} parameter is the last concentration before the fitted response
#' declines from its control level (Fox 2010), and \code{nec} returns it as
#' the NEC.
#'
#' The hormesis threshold equations are \code{nechorme}, \code{nechorme4},
#' \code{neclinhorme}, \code{nechormepwr}, \code{nechorme4pwr} and
#' \code{nechormepwr01}. In each, the \code{nec} parameter is the
#' concentration at which the decline term starts. The fitted response there
#' can be above the control, and where the slope of the stimulation term
#' exceeds the initial rate of decline the curve continues to rise beyond it,
#' so the parameter is not a no-effect concentration. \code{nec} returns the
#' NSEC of these equations instead: the concentration at which the fitted
#' curve, after any rise, falls below the lower \code{sig_val} quantile of the
#' fitted control. This extends the NSEC of Fisher and Fox (2023), which was
#' defined for monotone curves, to a hormetic curve. The \code{nec} parameter
#' remains available through \code{\link{curve_params}}.
#'
#' Under a fitted group term whose levels span concentrations, the NEC is
#' read for each observed level, including its deviation on the parameter.
#' Terms whose levels each occur at one concentration are set to zero. The
#' level of a spanning term observed at only one concentration is excluded from
#' the estimate, with a message naming it. The default mixes the remaining
#' observed levels by their observation counts;
#' \code{pool = "none"} returns them separately.
#'
#' On a \code{\link{bayesnecjointfit}}, each level of the refit is mixed over
#' its own observed group levels. A group term whose values occur in the data
#' of more than one level, such as a batch in which two levels were both
#' tested, is read at the same value for every level at each posterior draw,
#' as often as their observation counts on that term allow. Terms they do not
#' share are chosen for each level independently. The levels can therefore be
#' contrasted draw by draw, as \code{\link{compare_posterior}} does.
#'
#' \bold{What is actually returned depends on the model set.} Despite the
#' function name, \code{nec} returns a no-effect estimate of whichever type the
#' fitted model(s) support, following the convention set out in Fisher et al.
#' (2023):
#'
#' \itemize{
#'   \item For a \code{\link{bayesnecfit}} holding a monotone threshold
#'     equation, the \bold{NEC}: the \code{nec} parameter itself.
#'   \item For a \code{\link{bayesnecfit}} holding a hormesis threshold
#'     equation, the \bold{NSEC} read off the fitted curve as for a smooth
#'     equation, below. This too is computed when the model is fitted.
#'   \item For a \code{\link{bayesnecfit}} holding a smooth (\code{ecx}-type)
#'     model, which has no threshold parameter, the \bold{NSEC} (Fisher and Fox
#'     2023) -- the concentration at which the fitted curve falls below a
#'     reference taken from the \code{sig_val} quantile of the control
#'     posterior. This is computed when the model is fitted, using the
#'     \code{sig_val} passed to \code{\link{bnec}}.
#'   \item For a \code{\link{bayesmanecfit}} whose model set contains both
#'     kinds, the weighted posterior mixes sampled NEC draws from monotone
#'     threshold equations with NSEC draws from smooth and hormesis threshold
#'     equations, in proportion to the model weights. The result is the
#'     model-averaged \bold{N(S)EC}, and a message is emitted to say so. It is
#'     not a pure NEC and should not be reported as one.
#'   \item For a \code{\link{bayesnecjointfit}}, a
#'     \code{\link[base]{data.frame}} with one row per level of the grouping
#'     factor, the equation fitted at that level in a \code{model} column and
#'     the type of the estimate in an \code{ne_type} column. A composed joint
#'     refit fits one equation per level, so each level's estimate is a NEC or
#'     an NSEC and never a mixture of the two. \code{no_effect = FALSE}, the
#'     default, reports a sampled NEC for a monotone threshold equation, a
#'     curve-read NSEC for a hormesis threshold equation and \code{NA} for a
#'     smooth equation. \code{no_effect = TRUE} also reports the NSEC of a
#'     smooth equation.
#'     The \code{ne_type} column is what a model-averaged N(S)EC cannot report.
#' }
#'
#' \code{\link{summary}} labels the estimate NEC, NSEC or N(S)EC accordingly,
#' and is the better choice where the type matters, because for every class but
#' the joint refit \code{nec} returns an unlabelled vector. Use
#' \code{\link{nsec}} where a NSEC is wanted from every model regardless of
#' type.
#'
#' Where \code{crf()} transforms the predictor inline, as in
#' \code{crf(log(concentration))}, a sampled \code{nec} parameter is on the
#' transformed scale. Without \code{xform} it is returned on the recorded
#' scale, a concentration, by the inverse of the transformation. The inverse
#' is exact for \code{log}, \code{log10}, \code{log2}, \code{log1p},
#' \code{sqrt} and \code{exp} of the predictor, alone or shifted by a
#' constant, as in \code{log(concentration + 1)}. For any other
#' transformation it is read off the prediction grid by linear interpolation,
#' and a draw beyond that grid that is not censored is an error naming the
#' remedy. A supplied \code{xform} is applied to the estimate on the
#' transformed scale instead, so \code{xform = identity} returns that scale
#' and \code{xform = exp} on \code{crf(log(concentration))} returns the
#' exact inverse.
#'
#' @return A vector containing the estimated no-effect value, including upper
#' and lower 95% credible interval bounds (or other interval as specified by
#' prob_vals). Where any posterior draw lies beyond the range the model was
#' predicted over, that draw keeps its rank in the summary and is given no
#' value, so an entry falling among such draws is the end of the prediction
#' range rather than a quantile. Attribute \code{"censored_summary"} marks
#' which entries those are and states how many draws lie beyond each end.
#'
#' @section Capping and dropping:
#' Two operations are both called censoring and they are not the same. A
#' threshold equation's \code{nec} is a sampled parameter: a draw above the top
#' of the prediction range has a value, and that value is known to exceed the
#' bound, which is right-censoring in the ordinary sense. An NSEC or an ECx is
#' read off a fitted curve: a draw whose curve never reaches the target has no
#' such concentration in the range and may have none at any concentration, so
#' the draw is not capped at the bound, it is recorded as lying beyond it. Both
#' are reported the same way here, because neither claims a value for the draw.
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
#' @section Extrapolation:
#' \code{extrapolate} chooses the bound the estimate is censored at. It is read
#' on the recorded predictor scale, the scale the data were supplied on, the
#' scale \code{\link{bnec}} and \code{\link{nsec}} take \code{x_range} on, and
#' the scale the estimate is returned on without \code{xform}. Under
#' \code{crf(log(concentration))} the limit is therefore a concentration and
#' not a log concentration, whatever \code{xform} is given.
#'
#' \code{TRUE} removes the bound at both ends. It is accepted only where every
#' component of the reported estimate samples a NEC. An NSEC is read off a
#' fitted curve and a curve cannot be evaluated on an infinite grid, so where
#' any component is an NSEC --- which includes the default \code{\link{bnec}}
#' model set, because that set fits equations of both classes --- \code{TRUE}
#' is an error naming the finite form. A hormesis threshold equation
#' contributes an NSEC, so \code{TRUE} is refused for a fit or a set that
#' contains one. For a single fit the class is the one recorded in
#' \code{ne_type}, which takes both blocks of a joint two-block fit into
#' account. For a model-averaged set it is the type recorded for each equation
#' when the set was built, from which \code{\link{summary}} also takes the
#' label it prints, so the two cannot disagree about what the set is.
#'
#' A finite limit applies to both classes, by a different route for each. The
#' threshold components are compared against it, which requires no further
#' computation because every draw of a sampled \code{nec} already has a value.
#' The curve-read components are re-evaluated on a grid extended to it, through
#' the \code{x_range} argument of \code{\link{nsec}}. That recomputes from the
#' stored fit and needs no refit, but it does evaluate the curve once for each
#' such equation, at a run time proportional to \code{resolution}. The
#' re-evaluation reuses the \code{sig_val} the fit was built with unless
#' \code{sig_val} is given, and the number of grid points the fit stored
#' unless \code{resolution} is given. Over a wider range the grid is
#' therefore proportionally coarser, and its numbers can differ in the last
#' digits from the stored summary.
#'
#' Two requests re-evaluate nothing and return what \code{extrapolate = FALSE}
#' returns: a limit equal to the current bounds, and any limit on a fit with no
#' draw beyond either end, where every draw was identified inside the range the
#' fit used and a wider grid has none left to identify. An object fitted before
#' version 2.2.0 records nothing about which of its draws lie beyond the range,
#' so it takes the second of those whatever its draws are, and
#' \code{extrapolate} does nothing to it. Refit it, or rebuild the set with
#' \code{\link{amend}}, to extrapolate from it.
#'
#' A limit inside the prediction range is an error rather than a silent
#' tightening, because it would report an estimate as censored at a value the
#' fit had no trouble identifying. Narrow the range with \code{x_range} where
#' the fit or the estimate is built instead.
#'
#' Two constraints on the reach of an extrapolation are reported rather than
#' corrected. Where the \code{nec} prior stored on the fit is itself bounded at
#' the prediction range, the posterior holds no draw beyond that bound and a
#' wider limit returns the same truncated posterior; where the call
#' re-evaluates anything, a message states the bound. The default prior bounds
#' \code{nec} to the tested range, so this applies to every fit made under the
#' default \code{asymptote_observed = TRUE} of \code{\link{bnec}}:
#' \code{extrapolate} reaches above that range only for a fit made under
#' \code{asymptote_observed = FALSE}, which lifts the upper bound, or with a
#' user prior that sets a wider one. The lower bound at the lowest value tested
#' applies under both settings and is reported by the other arm of the same
#' message. And a curve-read
#' component is measured from the control, so a lower limit below the lowest
#' observed concentration extends the grid without extending the search, and
#' those components stay censored at the control.
#'
#' @references
#' Fisher R, Fox DR (2023). Introducing the no significant effect concentration
#' (NSEC). Environmental Toxicology and Chemistry, 42(9), 2019–2028.
#' doi: 10.1002/etc.5610.
#'
#' Fisher R, Fox DR, Negri AP, van Dam J, Flores F, Koppel D (2023). Methods for
#' estimating no-effect toxicity concentrations in ecotoxicology. Integrated
#' Environmental Assessment and Management. doi: 10.1002/ieam.4809.
#'
#' Fox DR (2010). A Bayesian Approach for Determining the No Effect
#' Concentration and Hazardous Concentration in Ecotoxicology. Ecotoxicology
#' and Environmental Safety, 73(2), 123–131. doi: 10.1016/j.ecoenv.2009.09.012.
#'
#' @examples
#' library(bayesnec)
#' data(manec_example)
#' nec(manec_example)
#'
#' @export
# no_effect follows `...` so that it appears in \usage, although only one class
# uses it. The other methods take it through `...` and refuse it.
nec <- function(object, posterior = FALSE, xform = NULL,
                prob_vals = c(0.5, 0.025, 0.975), extrapolate = FALSE, ...,
                no_effect = FALSE, pool = NULL) {
  UseMethod("nec")
}


#' @inheritParams nec
#'
#' @param object An object of class \code{\link{bayesnecfit}} returned by
#' \code{\link{bnec}}.
#'
#' @inherit nec seealso return examples
#' 
#' @importFrom stats quantile
#' @importFrom chk chk_logical
#'
#' @noRd
#'
#' @export
nec.bayesnecfit <- function(object, posterior = FALSE, xform = NULL,
                            prob_vals = c(0.5, 0.025, 0.975),
                            extrapolate = FALSE, ..., pool = NULL) {
  check_component_arg(list(...), object)
  check_nec_no_dpar(list(...))
  check_no_effect_arg(list(...), object)
  chk_logical(posterior)
  check_xform(xform)
  if (length(prob_vals) < 3 | prob_vals[1] < prob_vals[2] |
      prob_vals[1] > prob_vals[3] | prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order.")
  }
  # Test for a nec parameter, not for "ecx" in the equation name: an equation
  # named without that prefix would otherwise return an NSEC as a NEC.
  if (!has_nec_parameter(object$model, object$fit)) {
    stop("nec is not a parameter in ecx model types.")
  }
  if (is.null(object$observed_group)) {
    level_spec <- observed_level_spec(object)
    if (!is.null(level_spec)) {
      # A curve-read level takes the caller's sig_val, resolution and x_range,
      # and otherwise those the fit recorded. An NA x_range means none given.
      args <- recorded_expand_args(list(object))
      dots <- list(...)
      for (arg in c("sig_val", "resolution", "x_range")) {
        if (!is.null(dots[[arg]]) && !all(is.na(dots[[arg]]))) {
          args[[arg]] <- dots[[arg]]
        }
      }
      return(estimate_observed_levels(
        object, level_spec, "nec",
        function(level, control_value, range_override) {
          # A mixed two-block endpoint has no combined sampled threshold.
          # Test the equation, as estimate_observed_levels() does, not
          # ne_type: an older saved fit records "NEC" for a hormesis equation.
          if ((!is.null(object$hurdle) &&
               !identical(object$hurdle$ne_type, "NEC")) ||
              !sampled_nec_is_no_effect(object$model, object$fit)) {
            nsec(level, sig_val = args$sig_val,
                 resolution = args$resolution, x_range = args$x_range,
                 posterior = TRUE, xform = xform, prob_vals = prob_vals,
                 extrapolate = extrapolate, pool = "none")
          } else {
            nec(level, posterior = TRUE, xform = xform,
                prob_vals = prob_vals, extrapolate = extrapolate,
                pool = "none")
          }
        },
        pool, posterior, prob_vals
      ))
    }
  }
  warn_stale_ne_type(object)
  necfit_ne(object, posterior = posterior, xform = xform,
            prob_vals = prob_vals, extrapolate = extrapolate, ...)
}

#' The no-effect estimate of one fit, whatever its equation
#'
#' The body of \code{nec.bayesnecfit()} after its argument checks. It does not
#' refuse an equation that has no \code{nec} parameter, and returns that
#' equation's stored NSEC draws instead. A hurdle component fitted to one
#' smooth equation is read through here; \code{nec()} on such a fit alone
#' still refuses.
#'
#' @inheritParams nec
#' @param object A \code{\link{bayesnecfit}}.
#'
#' @return As \code{\link{nec}}.
#' @noRd
necfit_ne <- function(object, posterior = FALSE, xform = NULL,
                      prob_vals = c(0.5, 0.025, 0.975), extrapolate = FALSE,
                      ...) {
  nec_out <- object$ne_posterior
  # The default returns the stored posterior and record untouched. The bounds
  # are the stored prediction grid, the value the record and ecx() censor at.
  # ne_grid_bounds() is passed unevaluated: under the default,
  # extrapolate_limits() returns before evaluating it, so a fit with no stored
  # grid raises nothing.
  lims <- extrapolate_limits(extrapolate, ne_grid_bounds(object),
                             necfit_ne_type(object), object$model)
  if (!is.null(lims)) {
    dots <- list(...)
    bounds <- ne_grid_bounds(object)
    nec_out <- extrapolated_necfit_ne(
      object, lims, bounds,
      sig_val = if (is.null(dots$sig_val)) {
        recorded_expand_args(list(object))$sig_val
      } else {
        dots$sig_val
      },
      resolution = if (is.null(dots$resolution)) {
        stored_resolution(object)
      } else {
        dots$resolution
      }
    )
  }
  # Read the label before xform, which need not keep attributes. It says
  # whether the bounds are the prediction range or limits the caller named.
  range_label <- censoring_range_label(nec_out)
  attr(nec_out, "extrapolated") <- NULL
  # Read the record expand_nec() wrote rather than derive one, so that this
  # reports the same censoring as summary().
  cens <- attr(nec_out, "censored")
  map <- recorded_xform(object, xform, extrapolate)
  nec_out <- map_draws(nec_out, map, cens, ne_label(object))
  cens <- xform_censoring(cens, map)
  attr(nec_out, "censored") <- cens
  # The stored posterior can hold draws beyond the prediction range: a
  # b_nec_Intercept draw the prior does not confine, or an NA from
  # nsec_off_curve() for a joint fit with a smooth survival block. Such a draw
  # keeps its rank and is given no value, not max(x_vec).
  warn_censored_draws(nec_out, ne_label(object), cens = cens,
                      range_label = range_label)
  nec_estimate <- summarise_censored(unlist(nec_out), prob_vals, cens)
  names(nec_estimate) <- clean_names(nec_estimate)
  attr(nec_estimate, "toxicity_estimate") <- "nec"
  attr(nec_out, "toxicity_estimate") <-  "nec"
  if (!posterior) {
    nec_estimate
  } else {
    nec_out
  }
}

#' @inheritParams nec
#'
#' @param object An object of class \code{\link{bayesmanecfit}} returned by
#' \code{\link{bnec}}.
#'
#' @inherit nec seealso return examples
#' 
#' @importFrom stats quantile
#' @importFrom chk chk_logical
#'
#' @noRd
#'
#' @export
nec.bayesmanecfit <- function(object, posterior = FALSE, xform = NULL,
                              prob_vals = c(0.5, 0.025, 0.975),
                              extrapolate = FALSE, ..., pool = NULL) {
  check_component_arg(list(...), object)
  check_nec_no_dpar(list(...))
  check_no_effect_arg(list(...), object)
  chk_logical(posterior)
  check_xform(xform)
  if (length(prob_vals) < 3 | prob_vals[1] < prob_vals[2] |
      prob_vals[1] > prob_vals[3] | prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order.")
  }
  first <- object$mod_fits[[1]]
  if (!is.null(object$observed_group) ||
      (!is.null(first$fit) && !is.null(observed_level_spec(first)))) {
    if (is.null(pool)) pool <- "mixture"
    pool <- validate_pool(pool, "nec")
    dots <- list(...)
    # Read smooth equations at the sig_val the fit stored, as the stored
    # no-effect posterior is.
    sig_val <- if (is.null(dots$sig_val)) {
      recorded_expand_args(list(object))$sig_val
    } else {
      dots$sig_val
    }
    # Each equation is read afresh below, so classify by the current rule.
    report_manec_ne_type(ifelse(vapply(names(object$mod_fits), function(m) {
      sampled_nec_is_no_effect(m, object$mod_fits[[m]]$fit)
    }, logical(1)), "NEC", "NSEC"))
    # A caller's resolution and x_range reach the curve-read equations too.
    curve_dots <- dots[names(dots) %in% c("resolution", "x_range")]
    per_model <- function(f) {
      if (sampled_nec_is_no_effect(f$model, f$fit)) {
        nec(f, posterior = TRUE, xform = xform, prob_vals = prob_vals,
            extrapolate = extrapolate, pool = pool)
      } else {
        do.call(nsec, c(list(f, sig_val = sig_val, posterior = TRUE,
                             xform = xform, prob_vals = prob_vals,
                             extrapolate = extrapolate, pool = pool),
                        curve_dots))
      }
    }
    if (identical(pool, "none") && is.null(object$observed_group)) {
      return(model_set_level_estimates(object, "nec", per_model,
                                        posterior, prob_vals))
    }
    models <- names(object$mod_fits)
    idx <- pull_draw_index(object, models, object$sample_size)
    models <- models[lengths(idx[models]) > 0L]
    parts <- once_level_note(lapply(models, function(m) {
      out <- per_model(pull_equation(object, m))
      subset_draws(out, idx[[m]])
    }))
    draws <- unlist(lapply(parts, as.numeric), use.names = FALSE)
    attr(draws, "censored") <- concat_censoring(
      lapply(parts, attr, "censored"), vapply(parts, length, integer(1)),
      draws
    )
    attr(draws, "toxicity_estimate") <- "nec"
    if (posterior) return(draws)
    estimate <- summarise_censored(draws, prob_vals)
    names(estimate) <- clean_names(estimate)
    attr(estimate, "toxicity_estimate") <- "nec"
    return(estimate)
  }
  warn_stale_ne_type(object)
  report_manec_ne_type(manec_component_types(object))
  nec_out <- object$w_ne_posterior
  # Check the limits against every equation, not the set's label: an infinite
  # limit depends on whether each component samples a NEC. As in
  # nec.bayesnecfit(), ne_grid_bounds() is passed unevaluated.
  lims <- extrapolate_limits(extrapolate, ne_grid_bounds(object),
                             manec_ne_types(object), object$success_models)
  if (!is.null(lims)) {
    dots <- list(...)
    bounds <- ne_grid_bounds(object)
    nec_out <- extrapolated_manec_ne(
      object, lims, bounds,
      sig_val = if (is.null(dots$sig_val)) {
        recorded_expand_args(list(object))$sig_val
      } else {
        dots$sig_val
      },
      resolution = if (is.null(dots$resolution)) {
        stored_resolution(object)
      } else {
        dots$resolution
      }
    )
  }
  range_label <- censoring_range_label(nec_out)
  attr(nec_out, "extrapolated") <- NULL
  # As above. expand_manec() built this record under the mixture's draw index,
  # so its fraction is the censored share of the averaged posterior.
  cens <- attr(nec_out, "censored")
  map <- recorded_xform(object, xform, extrapolate)
  nec_out <- map_draws(nec_out, map, cens, "no-effect estimate")
  cens <- xform_censoring(cens, map)
  attr(nec_out, "censored") <- cens
  # Smooth equations add a second route to censoring: their NSEC draws can be
  # censored even where every threshold equation is inside the range.
  warn_censored_draws(nec_out, ne_label(object), cens = cens,
                      range_label = range_label)
  nec_estimate <- summarise_censored(unlist(nec_out), prob_vals, cens)
  names(nec_estimate) <- clean_names(nec_estimate)
  attr(nec_estimate, "toxicity_estimate") <- "nec"
  attr(nec_out, "toxicity_estimate") <-  "nec"
  if (!posterior) {
    nec_estimate
  } else {
    nec_out
  }
}

#' Whether an equation has a nec parameter
#'
#' Read from the equation's own formula through
#' \code{\link{equation_par_names}}, which needs no fitted
#' \code{\link[brms]{brmsfit}} and so also answers for an object assembled
#' without one, as several tests build. Where the name is no equation, the fit
#' is read as \code{\link{expand_nec}} reads it. Only where neither is
#' available does the \code{"ecx"} substring in the name decide; no fit made by
#' \code{\link{bnec}} reaches that branch. For a two-block fit both readings
#' describe the response block.
#'
#' @param model A \code{\link[base]{character}} naming the equation.
#' @param fit The \code{\link[brms]{brmsfit}}, or \code{NULL}.
#'
#' @return A \code{\link[base]{logical}} of length 1.
#' @noRd
has_nec_parameter <- function(model, fit = NULL) {
  if (is.character(model) && length(model) == 1 && !is.na(model) &&
        model %in% equation_names()) {
    return("nec" %in% equation_par_names(model))
  }
  if (inherits(fit, "brmsfit")) {
    return(!all(is.na(extract_pars("nec", fit))))
  }
  !isTRUE(any(grepl("ecx", model, fixed = TRUE)))
}

#' The hormesis threshold equations
#'
#' @return The names of the equations in both the \code{nec} and the
#' \code{hormesis} groups.
#' @noRd
hormesis_threshold_models <- function() {
  intersect(mod_groups$nec, mod_groups$hormesis)
}

#' Whether the sampled breakpoint is a no-effect concentration
#'
#' A hormesis threshold equation samples a nec parameter where its decline
#' term starts, and the response there can be above the control. Its
#' no-effect estimate is the NSEC instead. Keep this distinct from
#' has_nec_parameter(), which describes the equation.
#'
#' @param model The equation name.
#' @param fit A fitted model, used for an unrecognised equation name.
#'
#' @return TRUE when the no-effect estimate is the sampled nec parameter.
#' @noRd
sampled_nec_is_no_effect <- function(model, fit = NULL) {
  has_nec_parameter(model, fit) &&
    !model %in% hormesis_threshold_models()
}

#' The equations whose stored no-effect estimate is a hormesis breakpoint
#'
#' A fit saved before hormesis threshold equations took the NSEC stores their
#' sampled nec parameter as the no-effect estimate. Such a single fit records
#' \code{ne_type} \code{"NEC"} or none, and such a set records no type for
#' each equation. \code{amend()} rebuilds the estimate.
#'
#' @param object A \code{\link{bayesnecfit}}, \code{\link{bayesmanecfit}},
#' \code{\link{bayesnechurdlefit}} or \code{\link{bayesnecgroupfit}}.
#'
#' @return A \code{\link[base]{character}} vector of equation names, empty
#' where the object is current.
#' @noRd
stale_ne_models <- function(object) {
  # A level view is built from its parent, which is checked instead.
  if (!is.null(object$observed_group)) return(character())
  if (is_bayesnechurdlefit(object)) {
    return(unique(c(stale_ne_models(object$growth),
                    stale_ne_models(object$survival))))
  }
  if (is_bayesnecgroupfit(object)) {
    return(as.character(unique(unlist(lapply(object$fits, stale_ne_models)))))
  }
  hormesis <- hormesis_threshold_models()
  # The survival block of a two-block fit can hold a hormesis equation too.
  hu_model <- function(part) {
    if (is.null(part$fit) || !is_hurdle_family(part$fit$family)) {
      return(NA_character_)
    }
    identify_dpar_model(part, hurdle_dpar(part$fit$family))
  }
  if (inherits(object, "bayesmanecfit")) {
    models <- names(object$mod_fits)
    stale <- vapply(models, function(m) {
      part <- object$mod_fits[[m]]
      is.null(part$ne_type) &&
        (m %in% hormesis || isTRUE(hu_model(part) %in% hormesis))
    }, logical(1))
    return(models[stale])
  }
  if (!inherits(object, "bayesnecfit") || is.null(object$model)) {
    return(character())
  }
  # The current rule never labels a hormesis response block "NEC", nor a
  # hormesis survival block "nec".
  stale_mu <- isTRUE(object$model %in% hormesis) &&
    (is.null(object$ne_type) || identical(object$ne_type, "NEC"))
  stale_hu <- identical(object$hurdle$hu_ne_type, "nec") &&
    isTRUE(hu_model(object) %in% hormesis)
  if (stale_mu || stale_hu) object$model else character()
}

#' Whether a saved fit needs its no-effect estimate rebuilt
#'
#' @inheritParams stale_ne_models
#' @return A logical value.
#' @noRd
needs_ne_rebuild <- function(object) {
  length(stale_ne_models(object)) > 0
}

#' Warn when a saved fit stores a hormesis breakpoint as its no-effect estimate
#'
#' The warning has class \code{bayesnec_stale_ne}, so that a method which
#' warns once can muffle the same warning from the methods it calls.
#'
#' @inheritParams stale_ne_models
#' @return Invisible NULL.
#' @noRd
warn_stale_ne_type <- function(object) {
  models <- stale_ne_models(object)
  if (!length(models)) return(invisible(NULL))
  warning(warningCondition(
    paste0("This fit was saved before bayesnec reported the NSEC of a ",
           "hormesis threshold equation as its no-effect estimate. The ",
           "stored estimate for ", paste(models, collapse = ", "), " is the ",
           "nec parameter, where the decline term starts, and the response ",
           "there can be above the control. Rebuild the estimate with ",
           "amend(x)."),
    class = "bayesnec_stale_ne"
  ))
  invisible(NULL)
}

#' Evaluate an expression without the stale no-effect warning
#'
#' @param expr An expression.
#' @return The value of \code{expr}.
#' @noRd
without_stale_ne_warning <- function(expr) {
  withCallingHandlers(
    expr,
    bayesnec_stale_ne = function(w) invokeRestart("muffleWarning")
  )
}

#' Raise the stale no-effect warning once for a call over several fits
#'
#' Muffles the warning from each fit \code{expr} reads and, where any was
#' raised, warns once for \code{object} after \code{expr} returns.
#'
#' @param object The object the call was made on.
#' @param expr An expression.
#' @return The value of \code{expr}.
#' @noRd
once_stale_ne_warning <- function(object, expr) {
  seen <- FALSE
  out <- withCallingHandlers(
    expr,
    bayesnec_stale_ne = function(w) {
      seen <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  if (seen) warn_stale_ne_type(object)
  out
}

#' Name the no-effect mixture returned by nec()
#'
#' @param kinds The no-effect type of each equation: \code{"NEC"},
#' \code{"NSEC"} or \code{"N(S)EC"}.
#'
#' @return NULL, invisibly.
#' @noRd
report_manec_ne_type <- function(kinds) {
  if (all(kinds == "NEC")) return(invisible(NULL))
  if (all(kinds == "NSEC")) {
    message("This bayesmanecfit uses curve-read NSEC estimates for every",
            " equation. See ?nec and summary(), which labels them.")
  } else {
    message("This bayesmanecfit combines sampled NEC and curve-read NSEC",
            " draws, so the returned estimate is the model-averaged N(S)EC.",
            " See ?nec and summary(), which labels it.")
  }
  invisible(NULL)
}
