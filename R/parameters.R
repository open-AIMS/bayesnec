#' Curve parameter estimates from a fitted model
#'
#' Reports the parameters of the concentration-response equation itself ---
#' \code{top}, \code{bot}, \code{beta}, \code{nec}, \code{ec50}, \code{slope},
#' \code{d} and \code{f} --- with their credible intervals, one row per
#' parameter per equation.
#'
#' @name parameters
#' @order 1
#'
#' @param object An object of class \code{\link{bayesnecfit}},
#' \code{\link{bayesmanecfit}}, \code{\link{bayesnechurdlefit}} or
#' \code{\link{bayesnecgroupfit}}.
#' @param summary A \code{\link[base]{logical}} vector of length 1. Whether to
#' return the summary table (the default) or the underlying posterior draws.
#' @param xform A function applied to \code{nec} and \code{ec50}, and to no
#' other parameter. See Details.
#' @param ... Unused.
#'
#' @details \code{\link{summary}} reports the model weights, the per-equation
#' dispersion, the weighted no-effect estimate and the per-equation Bayesian
#' R-squared. It does not report the parameters of the curve, and for a
#' \code{\link{bayesmanecfit}} nothing else did either. Those parameters are
#' what a methods section states alongside the threshold estimates: \code{top}
#' is the control level the whole curve is referenced to, \code{bot} the
#' asymptote a \code{"relative"} ECx is measured against, and \code{beta} the
#' decay rate.
#'
#' \bold{The estimates are per equation and are not averaged across the set.}
#' The equations of a set do not share a parameter list: \code{ecxexp} has no
#' \code{bot}, the three-parameter equations have no \code{d}, and only the
#' equations of \code{mod_groups$nec} estimate \code{nec}. Averaging a
#' parameter over whichever equations happen to estimate it would average over
#' a different subset for each parameter, using weights that were computed for
#' the whole set, so the weights of the rows contributing to one number would
#' not sum to 1 and would sum to something different for the next number. The
#' stacking weight is reported beside each row instead, so a reader can see
#' what share of the model average each curve holds, and no quantity is
#' reported that the weights do not support. See \code{\link{average_estimates}}
#' for the estimates that are averaged.
#'
#' \bold{This is not the \code{parameters()} of the \pkg{parameters}
#' package.} That one is an alias for \code{model_parameters()} and describes
#' a regression's coefficient table; this one returns the parameters of a
#' concentration-response equation. Neither package attaches the other, so the
#' two meet only where a session attaches both, and there the later attachment
#' wins: qualify the call as \code{bayesnec::parameters()} or
#' \code{parameters::parameters()} to say which is meant.
#'
#' \bold{The parameters are on the scale the model was fitted on.}
#' \code{\link{bnec}} assigns \code{link = "identity"}, so \code{top} and
#' \code{bot} are on the response scale for every fit that did not name a link
#' of its own. Where a link was named, \pkg{brms} applies its inverse to the
#' whole non-linear expression, so \code{top} and \code{bot} are on the link
#' scale; the returned object records the link in its \code{"link"} attribute
#' and a message names it.
#'
#' \bold{\code{xform} applies to \code{nec} and \code{ec50} only.} Those two
#' are measured on the predictor axis, so where \code{crf()} transforms the
#' predictor inline --- \code{crf(sqrt(x), ...)} --- they are on the
#' transformed scale, exactly as \code{\link{nec}} and \code{\link{ecx}} are,
#' and \code{xform} is the inverse that brings them back. Every other
#' parameter is a response level or a shape parameter and is not on the
#' predictor axis at all, so \code{xform} is not applied to it.
#'
#' @return With \code{summary = TRUE}, a \code{\link[base]{data.frame}} with
#' columns \code{model}, \code{wi}, \code{dpar}, \code{parameter},
#' \code{Estimate}, \code{Q2.5} and \code{Q97.5}, ordered by decreasing
#' \code{wi} and then by the canonical parameter order. \code{Estimate} is the
#' posterior median and the interval is equal-tailed, which is what
#' \code{\link[brms]{fixef}(robust = TRUE)} reports for the same fit and what
#' \code{\link{nec}} returns for \code{nec}. \code{wi} is the
#' stacking weight, and is 1 for a \code{\link{bayesnecfit}}, which is the
#' whole of its own set. \code{dpar} is the \pkg{brms} distributional
#' parameter the row belongs to: \code{"mu"} for every ordinary fit, and
#' additionally \code{"hu"} or \code{"zi"} for a joint two-block fit from
#' \code{bnec(family = "hurdle_gamma")} or \code{"zero_inflated_beta"}.
#'
#' With \code{summary = FALSE}, a named \code{\link[base]{list}} with one
#' element per equation, each a draws-by-parameter
#' \code{\link[base]{matrix}}. A two-block fit names the second block's columns
#' with the distributional parameter, as \code{hu_top}; a single-block fit has
#' no prefixed columns. A \code{\link{bayesnecgroupfit}} returns a list of
#' those lists, one per level, because there is no column to keep the levels
#' apart in a matrix of draws.
#'
#' A \code{\link{bayesnechurdlefit}} returns one such object per component,
#' as \code{\link{rhat}} and \code{\link{check_sampling}} do, because the two
#' components are separate fits with separate parameters and the combined
#' endpoint is not a curve with parameters of its own. A
#' \code{\link{bayesnecgroupfit}} returns one table for the whole group, with
#' a leading \code{level} column, matching \code{\link{nec}} and
#' \code{\link{ecx}} on that class.
#'
#' @seealso \code{\link{summary}}, \code{\link{show_params}},
#' \code{\link{nec}}, \code{\link{ecx}}, \code{\link{average_estimates}}
#'
#' @examples
#' library(bayesnec)
#' data(manec_example)
#' parameters(manec_example)
#' nec4param <- pull_out(manec_example, "nec4param")
#' parameters(nec4param)
#'
#' @export
parameters <- function(object, summary = TRUE, xform = identity, ...) {
  UseMethod("parameters")
}

#' @rdname parameters
#' @order 2
#'
#' @method parameters bayesnecfit
#'
#' @inherit parameters description details return seealso examples
#'
#' @importFrom chk chk_lgl
#'
#' @export
parameters.bayesnecfit <- function(object, summary = TRUE, xform = identity,
                                   ...) {
  chk_lgl(summary)
  check_xform(xform)
  # wi is 1 rather than NA: the object holds one equation and is the whole of
  # the set it describes. An object pulled out of a bayesmanecfit no longer
  # holds the weight it had there -- pull_out() returns a fit in its own
  # right -- so reading a weight off it would be reporting a number the object
  # does not hold.
  report_link(fit_links(object$fit), paste(object$model, "fit"))
  one_fit_parameters(object$fit, model = object$model, wi = 1,
                     summary = summary, xform = xform)
}

#' @rdname parameters
#' @order 3
#'
#' @method parameters bayesmanecfit
#'
#' @inherit parameters description details return seealso examples
#'
#' @importFrom chk chk_lgl
#'
#' @export
parameters.bayesmanecfit <- function(object, summary = TRUE,
                                     xform = identity, ...) {
  chk_lgl(summary)
  check_xform(xform)
  mods <- object$success_models
  # Once for the set, not once per equation. Every model in a bayesmanecfit is
  # fitted with the same family, chosen once by validate_family(), so the link
  # is a property of the set; reporting it per equation would print the same
  # sentence 23 times on the default model set.
  report_link(fit_links(object$mod_fits[[mods[1]]]$fit), "fitted model set")
  # Ordered by weight, so the equation holding most of the model average is
  # read first. Which equation a reported parameter belongs to is the whole
  # content of this table, and the weight is what says how much of the model
  # average that equation accounts for. Straight to $fit rather than through
  # pull_out(), which rebuilds predictions and posteriors to reach a brmsfit
  # already sitting in mod_fits.
  wi <- as.numeric(object$mod_stats[mods, "wi"])
  ord <- order(wi, decreasing = TRUE)
  out <- lapply(ord, function(i) {
    one_fit_parameters(object$mod_fits[[mods[i]]]$fit, model = mods[i],
                       wi = wi[i], summary = summary, xform = xform)
  })
  if (!summary) {
    res <- setNames(unlist(out, recursive = FALSE), mods[ord])
    attr(res, "link") <- attr(out[[1]], "link")
    return(res)
  }
  link <- attr(out[[1]], "link")
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  attr(out, "link") <- link
  out
}

#' @rdname parameters
#' @order 4
#'
#' @method parameters bayesnechurdlefit
#'
#' @inherit parameters description details return seealso examples
#'
#' @export
parameters.bayesnechurdlefit <- function(object, summary = TRUE,
                                         xform = identity, ...) {
  message(hurdle_no_combined("parameters"))
  hurdle_delegate(object, parameters, summary = summary, xform = xform, ...)
}

#' @rdname parameters
#' @order 5
#'
#' @method parameters bayesnecgroupfit
#'
#' @inherit parameters description details return seealso examples
#'
#' @export
parameters.bayesnecgroupfit <- function(object, summary = TRUE,
                                        xform = identity, ...) {
  out <- group_lapply(object, parameters, summary = summary, xform = xform,
                      ...)
  if (!summary) {
    return(out)
  }
  # One table with a level column, matching nec() and ecx() on this class
  # rather than returning a list the caller has to bind themselves. Safe here
  # in a way group_estimate_table() is not: the per-level results are already
  # data frames with named columns, so there are no positions to mislabel.
  link <- attr(out[[1]], "link")
  out <- do.call(rbind, lapply(seq_along(out), function(i) {
    cbind(data.frame(level = object$levels[i], stringsAsFactors = FALSE),
          out[[i]])
  }))
  rownames(out) <- NULL
  attr(out, "link") <- link
  out
}

#' The parameters of the concentration-response equations, in reporting order
#'
#' Fixed here rather than read from \code{\link{show_params}} so that the row
#' order of the table is a property of this function and not of the order in
#' which a \code{\link[brms]{brmsformula}} happens to list its terms. Every
#' parameter of every one of the 23 equations appears in this vector; an
#' equation contributes the subset it estimates.
#'
#' @return A \code{\link[base]{character}} vector.
#' @noRd
curve_par_names <- function() {
  c("top", "bot", "beta", "nec", "ec50", "slope", "d", "f")
}

#' The parameters measured on the predictor axis
#'
#' \code{xform} inverts a transformation of the predictor, so it applies to the
#' parameters that are predictor values and to no others. \code{top} and
#' \code{bot} are response levels, and \code{beta}, \code{slope}, \code{d} and
#' \code{f} are shape parameters; applying a predictor transformation to any of
#' them would return a number in the wrong units with nothing said.
#'
#' @return A \code{\link[base]{character}} vector.
#' @noRd
x_scale_par_names <- function() {
  c("nec", "ec50")
}

#' @noRd
check_xform <- function(xform) {
  if (!inherits(xform, "function")) {
    stop("xform must be a function.", call. = FALSE)
  }
  invisible(NULL)
}

#' The distributional parameter blocks of a fit
#'
#' One for an ordinary fit, two for a joint fit from
#' \code{bnec(family = "hurdle_gamma")} or \code{"zero_inflated_beta"}, where
#' the second block has curve parameters of its own named with the \pkg{brms}
#' distributional parameter as a prefix --- \code{b_hutop_Intercept}.
#'
#' @param family A \code{\link[stats]{family}} object.
#'
#' @return A named \code{\link[base]{character}} vector of prefixes, named by
#' distributional parameter.
#' @noRd
fit_dpars <- function(family) {
  out <- c(mu = "")
  if (is_hurdle_family(family)) {
    hu <- hurdle_dpar(family)
    out <- c(out, setNames(hu, hu))
  }
  out
}

#' The link a given distributional parameter is on
#'
#' @param family A \code{\link[stats]{family}} object.
#' @param dpar A \code{\link[base]{character}} naming the block.
#'
#' @return A \code{\link[base]{character}} value.
#' @noRd
dpar_link <- function(family, dpar) {
  if (identical(dpar, "mu")) {
    return(family$link)
  }
  out <- family[[paste0("link_", dpar)]]
  if (is.null(out)) "identity" else out
}

#' The links of every block of a fit
#'
#' @param fit A \code{\link[brms]{brmsfit}}.
#'
#' @return A named \code{\link[base]{character}} vector.
#' @noRd
fit_links <- function(fit) {
  dpars <- names(fit_dpars(fit$family))
  vapply(dpars, function(d) dpar_link(fit$family, d), character(1))
}

#' Posterior draws of the curve parameters of one block of one fit
#'
#' @param fit A \code{\link[brms]{brmsfit}}.
#' @param prefix The \pkg{brms} distributional parameter prefix, \code{""} for
#' the mean block.
#'
#' @details Read from the draws rather than from \code{\link[brms]{fixef}} so
#' that \code{summary = FALSE} and \code{summary = TRUE} report the same
#' numbers, and so that \code{xform} is applied to each draw rather than to a
#' quantile of the untransformed draws --- which for a non-linear \code{xform}
#' is a different number, since a quantile of a monotone transformation is the
#' transformation of the quantile only for the quantile itself and not for a
#' mean.
#'
#' The column name is matched in full rather than by prefix. Prefix matching
#' would make \code{b_top_Intercept} of a hurdle fit also match
#' \code{b_hutop_Intercept}, which is the defect \code{extract_pars()} records.
#' A group-level term on a parameter leaves the population-level
#' \code{b_<par>_Intercept} in place and adds \code{sd_} and \code{r_} entries,
#' which are not curve parameters and are not matched here.
#'
#' @return A draws-by-parameter \code{\link[base]{matrix}}, with zero columns
#' where the block estimates none of them.
#'
#' @importFrom brms as_draws_df
#' @noRd
curve_param_draws <- function(fit, prefix = "") {
  draws <- as.data.frame(as_draws_df(fit))
  pars <- curve_par_names()
  cols <- paste0("b_", prefix, pars, "_Intercept")
  keep <- cols %in% names(draws)
  out <- as.matrix(draws[, cols[keep], drop = FALSE])
  colnames(out) <- pars[keep]
  out
}

#' The parameter table, or the draws, for one fit
#'
#' @param fit A \code{\link[brms]{brmsfit}}.
#' @param model A \code{\link[base]{character}} naming the equation.
#' @param wi A \code{\link[base]{numeric}} stacking weight.
#' @param summary A \code{\link[base]{logical}} value.
#' @param xform A function.
#'
#' @return A \code{\link[base]{data.frame}}, or a
#' \code{\link[base]{list}} of one draws matrix.
#'
#' @noRd
one_fit_parameters <- function(fit, model, wi, summary, xform) {
  dpars <- fit_dpars(fit$family)
  links <- fit_links(fit)
  blocks <- lapply(names(dpars), function(d) {
    draws <- curve_param_draws(fit, dpars[[d]])
    for (p in intersect(colnames(draws), x_scale_par_names())) {
      draws[, p] <- xform(draws[, p])
    }
    draws
  })
  names(blocks) <- names(dpars)
  if (!summary) {
    out <- do.call(cbind, lapply(names(blocks), function(d) {
      b <- blocks[[d]]
      if (!identical(d, "mu") && ncol(b) > 0) {
        colnames(b) <- paste0(d, "_", colnames(b))
      }
      b
    }))
    out <- setNames(list(out), model)
    attr(out, "link") <- links
    return(out)
  }
  out <- do.call(rbind, lapply(names(blocks), function(d) {
    b <- blocks[[d]]
    if (ncol(b) == 0) {
      return(NULL)
    }
    ests <- t(vapply(colnames(b), function(p) estimates_summary(b[, p]),
                     numeric(3)))
    data.frame(model = model, wi = wi, dpar = d, parameter = colnames(b),
               ests, row.names = NULL, stringsAsFactors = FALSE,
               check.names = FALSE)
  }))
  if (is.null(out)) {
    # Reached only for a fit holding none of the eight parameters, which no
    # equation bnec() fits produces. Returned as an empty frame of the right
    # shape rather than NULL so that rbind over a set does not drop a column.
    out <- empty_parameter_table()
  }
  attr(out, "link") <- links
  out
}

#' @noRd
empty_parameter_table <- function() {
  data.frame(model = character(0), wi = numeric(0), dpar = character(0),
             parameter = character(0), Estimate = numeric(0),
             Q2.5 = numeric(0), Q97.5 = numeric(0),
             stringsAsFactors = FALSE, check.names = FALSE)
}

#' Say so where a parameter is not on the response scale
#'
#' \code{\link{bnec}} assigns \code{link = "identity"} to every family it
#' accepts, so the usual case is silent. Where the caller named a link,
#' \pkg{brms} applies its inverse to the whole non-linear expression and
#' \code{top} and \code{bot} are on the link scale --- a \code{top} of 2.5 on a
#' \code{log} link is a control response of 12.2. Nothing in the returned table
#' shows that, so it is reported rather than left to be inferred from the
#' family.
#'
#' @param links A named \code{\link[base]{character}} vector of links, one per
#' distributional parameter.
#' @param what A \code{\link[base]{character}} noun phrase naming what was
#' fitted, used as the subject of the message.
#'
#' @return \code{NULL}, invisibly. Called for the message.
#' @noRd
report_link <- function(links, what) {
  bad <- links[links != "identity"]
  if (length(bad) == 0) {
    return(invisible(NULL))
  }
  message("The ", what, " was made with ",
          paste0(names(bad), ": link = \"", bad, "\"", collapse = ", "),
          ". top and bot are on the link scale rather than the response ",
          "scale, because brms applies the inverse link to the whole ",
          "non-linear expression. Apply the inverse to read them as a ",
          "response.")
  invisible(NULL)
}
