#' Curve parameter estimates from a fitted model
#'
#' Reports the parameters of the concentration-response equation itself ---
#' \code{top}, \code{bot}, \code{beta}, \code{nec}, \code{ec50}, \code{slope},
#' \code{d} and \code{f} --- with their credible intervals, one row per
#' parameter per equation.
#'
#' @name curve_params
#' @order 1
#'
#' @param object An object of class \code{\link{bayesnecfit}},
#' \code{\link{bayesmanecfit}}, \code{\link{bayesnechurdlefit}} or
#' \code{\link{bayesnecgroupfit}}.
#' @param summary A \code{\link[base]{logical}} vector of length 1. Whether to
#' return the summary table (the default) or the underlying posterior draws.
#' @param xform \code{NULL}, the default, or a function, applied to
#' \code{nec} and \code{ec50} and to no other parameter. \code{NULL} returns
#' those two on the scale the predictor was recorded on. A function is applied
#' to them on the scale the model was fitted on. See Details.
#' @param ... Passed to the per-component and per-level calls by the
#' \code{\link{bayesnechurdlefit}} and \code{\link{bayesnecgroupfit}} methods,
#' and unused by the other two.
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
#' \bold{Only the parameters of the equation are reported.} The family's own
#' dispersion parameter is \code{\link{dispersion}}'s and \code{summary}'s, and
#' a group-level term's standard deviation, its per-level deviations and the
#' \code{ogl} offset are not parameters of the curve. Use
#' \code{\link[brms]{fixef}} or \code{\link[brms]{as_draws_df}} on
#' \code{\link{pull_brmsfit}(x)} for those.
#'
#' \bold{The estimates are per equation and are not averaged across the set.}
#' The equations of a set do not share a parameter list: \code{ecxexp} has no
#' \code{bot}, the three-parameter equations have no \code{d}, and only the
#' equations of \code{mod_groups$nec} estimate \code{nec}. Averaging a
#' parameter over whichever equations happen to estimate it would average over
#' a different subset for each parameter, using weights that were computed for
#' the whole set, so the weights of the rows contributing to one number would
#' not sum to 1 and would sum to something different for the next number. The
#' model weight is reported beside each row instead, so a reader can see what
#' share of the model average each curve holds, and no quantity is reported
#' that the weights do not support. The model-averaged estimates are returned
#' by \code{\link{nec}}, \code{\link{nsec}} and \code{\link{ecx}} on the set.
#'
#' \code{\link{show_params}} names the parameters of each equation without
#' fitting anything; this function reports their estimates from a fit.
#'
#' \bold{The parameters are on the scale the model was fitted on.}
#' \code{\link{bnec}} assigns \code{link = "identity"}, so \code{top} and
#' \code{bot} are on the response scale for every fit that did not name a link
#' of its own. Where a link was named, \pkg{brms} applies its inverse to the
#' whole non-linear expression, so \code{top} and \code{bot} are on the link
#' scale. The link of each block is reported in the \code{link} column, and a
#' message names any that is not the identity.
#'
#' \bold{\code{xform} applies to \code{nec} and \code{ec50} only.} Those two
#' are measured on the predictor axis, so where \code{crf()} transforms the
#' predictor inline --- \code{crf(sqrt(x), ...)} --- they are sampled on the
#' transformed scale. Without \code{xform} they are returned on the recorded
#' scale by the inverse of the transformation, as \code{\link{nec}} returns
#' the \code{nec} parameter: exactly where the inverse has a closed form, and
#' otherwise by interpolating on the prediction grid. A draw beyond that grid
#' then has no value, so it is \code{NA}, the row summarising it is
#' \code{NA}, and a message names the remedy. A supplied \code{xform} is
#' applied to them on the transformed scale, so \code{xform = identity}
#' returns that scale. Every other parameter is a response level or a shape
#' parameter and is not on the predictor axis at all, so it is returned as
#' sampled.
#'
#' @return With \code{summary = TRUE}, a \code{\link[base]{data.frame}} with
#' columns \code{model}, \code{wi}, \code{dpar}, \code{link}, \code{parameter},
#' \code{Estimate}, \code{Q2.5} and \code{Q97.5}, ordered by decreasing
#' \code{wi} and then by the parameter order given above.
#'
#' \code{Estimate} is the posterior median and the interval is equal-tailed,
#' which is what \code{\link[brms]{fixef}(robust = TRUE)} reports for the same
#' fit. For a single-block threshold fit the \code{nec} row is therefore the
#' same number \code{\link{nec}} returns; for a two-block fit \code{nec()}
#' returns the combined threshold rather than either block's, and for a
#' \code{\link{bayesmanecfit}} it returns the model-averaged estimate, neither
#' of which is a row here.
#'
#' \code{wi} is the model weight \code{\link{summary}} reports, under whichever
#' method the set was weighted by --- pseudo-BMA unless \code{loo_controls}
#' asked for stacking. It is 1 for a \code{\link{bayesnecfit}}, which is the
#' whole of its own set.
#'
#' \code{dpar} is the \pkg{brms} distributional parameter the row belongs to:
#' \code{"mu"} for every ordinary fit, and additionally \code{"hu"} or
#' \code{"zi"} for a joint two-block fit from
#' \code{bnec(family = "hurdle_gamma")} or \code{"zero_inflated_beta"}. The two
#' blocks of such a fit need not use the same equation --- see the
#' \code{model_survival} argument of \code{\link{bnec}} --- so \code{model}
#' names each block's own equation, and is \code{NA} for a second block whose
#' equation cannot be identified from the fitted formula.
#'
#' With \code{summary = FALSE}, a named \code{\link[base]{list}} with one
#' element per equation, each a draws-by-parameter
#' \code{\link[base]{matrix}}. A two-block fit names the second block's columns
#' with the distributional parameter, as \code{hu_top}; a single-block fit has
#' no prefixed columns. The list records the links of its fit in a \code{link}
#' attribute, since a matrix of draws has no column to put them in. For a
#' \code{\link{bayesmanecfit}} these are each
#' equation's own draws in full, not the weighted subset
#' \code{\link{nec}(posterior = TRUE)} returns, so they should not be pooled. A
#' \code{\link{bayesnecgroupfit}} returns a list of those lists, one per level,
#' because a matrix of draws has no column to keep the levels apart.
#'
#' A \code{\link{bayesnechurdlefit}} returns one such object per component, as
#' \code{\link{rhat}} and \code{\link{check_sampling}} do, because the two
#' components are separate fits with separate parameters and the combined
#' endpoint is not a curve with parameters of its own. A
#' \code{\link{bayesnecgroupfit}} returns one table for the whole group, with a
#' leading \code{level} column and sorted by level before weight, matching
#' \code{\link{nec}} and \code{\link{ecx}} on that class.
#'
#' @seealso \code{\link{summary}}, \code{\link{show_params}},
#' \code{\link{nec}}, \code{\link{ecx}}
#'
#' @examples
#' library(bayesnec)
#' data(manec_example)
#' curve_params(manec_example)
#' nec4param <- pull_out(manec_example, "nec4param")
#' curve_params(nec4param)
#'
#' @export
curve_params <- function(object, summary = TRUE, xform = NULL, ...) {
  UseMethod("curve_params")
}

#' @rdname curve_params
#' @order 2
#'
#' @method curve_params bayesnecfit
#'
#' @inherit curve_params description details return seealso examples
#'
#' @importFrom chk chk_lgl
#'
#' @export
curve_params.bayesnecfit <- function(object, summary = TRUE, xform = NULL,
                                   ...) {
  chk_lgl(summary)
  check_xform(xform)
  report_scales(object, paste(object$model, "fit"))
  # wi is 1, not NA: a fit holds one equation and is the whole of its set. A
  # fit from pull_out() does not keep the weight it had in its set.
  one_fit_params(object$fit, models = dpar_models(object), wi = 1,
                     summary = summary, xform = recorded_xform(object, xform))
}

#' @rdname curve_params
#' @order 3
#'
#' @method curve_params bayesmanecfit
#'
#' @inherit curve_params description details return seealso examples
#'
#' @importFrom chk chk_lgl
#'
#' @export
curve_params.bayesmanecfit <- function(object, summary = TRUE,
                                     xform = NULL, ...) {
  chk_lgl(summary)
  check_xform(xform)
  mods <- object$success_models
  # Report once for the set, whose equations share a family and a formula. The
  # loop below calls one_fit_params(), not curve_params(), so it does not
  # report again.
  report_scales(object$mod_fits[[mods[1]]], "fitted model set")
  # One map for the set, read off the set's prediction grid.
  xform <- recorded_xform(object, xform)
  # Order by weight. Read $fit directly: pull_out() would rebuild predictions
  # and posteriors to reach the same brmsfit.
  wi <- model_weights_for(object, mods)
  ord <- order(wi, decreasing = TRUE)
  out <- lapply(ord, function(i) {
    one_fit_params(object$mod_fits[[mods[i]]]$fit,
                       models = dpar_models(object$mod_fits[[mods[i]]]),
                       wi = wi[i], summary = summary, xform = xform)
  })
  if (!summary) {
    res <- setNames(unlist(out, recursive = FALSE), mods[ord])
    attr(res, "link") <- attr(out[[1]], "link")
    return(res)
  }
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

#' @rdname curve_params
#' @order 4
#'
#' @method curve_params bayesnechurdlefit
#'
#' @inherit curve_params description details return seealso examples
#'
#' @export
curve_params.bayesnechurdlefit <- function(object, summary = TRUE,
                                         xform = NULL, ...) {
  # Check the arguments here as well, so that a bad one fails before the
  # message below is printed.
  chk_lgl(summary)
  check_xform(xform)
  message(hurdle_no_combined("curve_params"))
  # Report each component's link here, named by component. The component
  # methods name the equation, which the two components often share.
  for (cmp in c("growth", "survival")) {
    report_link(fit_links(representative_fit(object[[cmp]])$fit),
                paste(cmp, "component"))
  }
  quiet <- options(bayesnec.link_reported = TRUE)
  on.exit(options(quiet), add = TRUE)
  hurdle_delegate(object, curve_params, summary = summary, xform = xform, ...)
}

#' @rdname curve_params
#' @order 5
#'
#' @method curve_params bayesnecgroupfit
#'
#' @export
curve_params.bayesnecgroupfit <- function(object, summary = TRUE,
                                        xform = NULL, ...) {
  chk_lgl(summary)
  check_xform(xform)
  # Every level shares one family, so report once for the group.
  report_scales(representative_fit(object$fits[[1]]), "fitted group")
  quiet <- options(bayesnec.link_reported = TRUE)
  on.exit(options(quiet), add = TRUE)
  out <- group_lapply(object, curve_params, summary = summary, xform = xform,
                      ...)
  if (!summary) {
    return(out)
  }
  # Bind into one table with a level column, as nec() and ecx() do on this
  # class. The per-level results already have named columns, so rbind() cannot
  # mislabel them.
  out <- do.call(rbind, lapply(seq_along(out), function(i) {
    cbind(data.frame(level = object$levels[i], stringsAsFactors = FALSE),
          out[[i]])
  }))
  rownames(out) <- NULL
  out
}

#' The parameters of the concentration-response equations, in reporting order
#'
#' \code{expand_nec()} holds the same eight names in another order, as
#' \code{extract_par_order()}. A parameter added for a new equation must be
#' added to both; \code{test-curve_params.R} checks that they agree. The order
#' here is the row order of the table, whatever order a
#' \code{\link[brms]{brmsformula}} lists its terms in.
#'
#' @return A \code{\link[base]{character}} vector.
#' @noRd
curve_par_names <- function() {
  c("top", "bot", "beta", "nec", "ec50", "slope", "d", "f")
}

#' The parameters one equation estimates
#'
#' Read from the equation's own \code{\link[brms]{brmsformula}}, which is what
#' \code{\link{show_params}} displays and what \code{make_hu_block()} reads to
#' build the second block. Used to say so where a fit does not sample one of
#' them.
#'
#' @param model A \code{\link[base]{character}} naming an equation, or
#' \code{NA}.
#'
#' @return A \code{\link[base]{character}} vector, empty for \code{NA}.
#' @noRd
equation_par_names <- function(model) {
  if (length(model) != 1 || is.na(model)) {
    return(character(0))
  }
  bf_obj <- tryCatch(get(paste0("bf_", model)), error = function(e) NULL)
  if (is.null(bf_obj)) {
    return(character(0))
  }
  intersect(curve_par_names(), names(bf_obj$pforms))
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

#' The model weight of each named equation
#'
#' Matched on the \code{model} column, not the row names of \code{mod_stats},
#' which \code{expand_manec()} sets only incidentally. Without them every
#' weight would be \code{NA}.
#'
#' @param object A \code{\link{bayesmanecfit}}.
#' @param mods A \code{\link[base]{character}} vector of equation names.
#'
#' @return A \code{\link[base]{numeric}} vector, one weight per element of
#' \code{mods}.
#' @noRd
model_weights_for <- function(object, mods) {
  stats <- object$mod_stats
  i <- if ("model" %in% names(stats)) {
    match(mods, as.character(stats$model))
  } else {
    match(mods, rownames(stats))
  }
  if (anyNA(i)) {
    stop("The model weights table holds no row for ",
         paste(mods[is.na(i)], collapse = ", "),
         ". This should not happen; please report it.", call. = FALSE)
  }
  as.numeric(stats$wi[i])
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

#' The equation fitted in each block of a fit
#'
#' The response block's equation is recorded on the object. The second block's
#' is stored nowhere, so it is recovered from the fitted formula, which stays
#' correct through \code{c()}, \code{+}, \code{\link{amend}} and
#' \code{update()}. A block whose expression matches no equation gives
#' \code{NA} rather than the response block's equation, which need not have
#' the parameters the second block holds.
#'
#' @param object A \code{\link{bayesnecfit}} or \code{prebayesnecfit}.
#'
#' @return A named \code{\link[base]{character}} vector, one equation name per
#' distributional parameter.
#' @noRd
dpar_models <- function(object) {
  dpars <- names(fit_dpars(object$fit$family))
  out <- setNames(rep(NA_character_, length(dpars)), dpars)
  out[["mu"]] <- object$model
  for (d in setdiff(dpars, "mu")) {
    out[[d]] <- identify_dpar_model(object, d)
  }
  out
}

#' Which equation a second block was built from
#'
#' \code{add_hu_block()} builds the block deterministically from the equation
#' name, so regenerating each candidate and comparing the deparsed expression
#' identifies it exactly. Comparing parameter sets would not: \code{ecx4param},
#' \code{ecxwb1} and \code{ecxwb2} estimate the same four.
#'
#' @param object A \code{\link{bayesnecfit}} or \code{prebayesnecfit}.
#' @param dpar A \code{\link[base]{character}} naming the block.
#'
#' @return A \code{\link[base]{character}} naming the equation, or \code{NA}.
#'
#' @importFrom stats model.frame
#' @noRd
identify_dpar_model <- function(object, dpar) {
  form <- object$fit$formula$pforms[[dpar]]
  if (is.null(form) || length(form) < 3) {
    return(NA_character_)
  }
  target <- deparse1(form[[3]])
  # The predictor name as wrangle_model_formula() substituted it: the model
  # frame's column name, which for an inline transformation is "log(x)" and
  # not the "x" that attr(, "bnec_pop") records.
  bdat <- tryCatch(model.frame(object$bayesnecformula, object$fit$data),
                   error = function(e) NULL)
  if (is.null(bdat)) {
    return(NA_character_)
  }
  x_pos <- which(names(attr(bdat, "bnec_pop")) == "x_var")
  if (length(x_pos) != 1) {
    return(NA_character_)
  }
  new_x <- names(bdat)[x_pos]
  for (m in models()$all) {
    candidate <- substitute_x_in_formula(
      new_x, deparse1(make_hu_block(m, dpar)$nlf[[3]])
    )
    if (identical(candidate, target)) {
      return(m)
    }
  }
  NA_character_
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
#' @details Read from the draws, not from \code{\link[brms]{fixef}}, because
#' \code{xform} need not be monotone: a quantile of transformed draws is then
#' not the transform of the quantile. \code{summary = FALSE} also returns the
#' draws \code{xform} was applied to.
#'
#' Column names are matched in full. A prefix match would make
#' \code{b_top_Intercept} also match \code{b_hutop_Intercept} in a hurdle fit.
#' The \code{sd_} and \code{r_} entries of a group-level term are not curve
#' parameters and are not matched.
#'
#' @return A draws-by-parameter \code{\link[base]{matrix}}, with zero columns
#' where the block estimates none of them.
#'
#' @importFrom brms as_draws_df
#' @noRd
curve_param_draws <- function(fit, prefix = "") {
  select_curve_cols(as.data.frame(as_draws_df(fit)), prefix)
}

#' The curve parameter columns of a draws data frame
#'
#' Separated from \code{curve_param_draws()} so that the name matching can be
#' exercised on a constructed frame rather than on a fitted model, which needs
#' minutes of sampling to produce.
#'
#' @param draws A \code{\link[base]{data.frame}} of posterior draws.
#' @param prefix The \pkg{brms} distributional parameter prefix.
#'
#' @return A draws-by-parameter \code{\link[base]{matrix}}.
#' @noRd
select_curve_cols <- function(draws, prefix = "") {
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
#' @param models A named \code{\link[base]{character}} vector of equation
#' names, one per distributional parameter.
#' @param wi A \code{\link[base]{numeric}} model weight.
#' @param summary A \code{\link[base]{logical}} value.
#' @param xform A function, from \code{recorded_xform()}.
#'
#' @return A \code{\link[base]{data.frame}}, or a
#' \code{\link[base]{list}} of one draws matrix.
#'
#' @noRd
one_fit_params <- function(fit, models, wi, summary, xform) {
  dpars <- fit_dpars(fit$family)
  links <- fit_links(fit)
  blocks <- lapply(names(dpars), function(d) {
    draws <- curve_param_draws(fit, dpars[[d]])
    report_absent_params(colnames(draws), models[[d]], d)
    # A draw a grid-read inverse cannot place is NA, with a message, so that
    # one equation's ec50 beyond the grid does not stop the whole table.
    for (p in intersect(colnames(draws), x_scale_par_names())) {
      draws[, p] <- map_draws(draws[, p], xform,
                              what = paste(p, "of", models[[d]]),
                              refuse = FALSE)
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
    out <- setNames(list(out), models[["mu"]])
    # The summary form records the link in a column. A matrix of draws has no
    # column to put it in, so the draws form keeps it as an attribute; without
    # it a saved set of draws would hold no record of the scale at all.
    attr(out, "link") <- links
    return(out)
  }
  out <- do.call(rbind, lapply(names(blocks), function(d) {
    b <- blocks[[d]]
    if (ncol(b) == 0) {
      return(NULL)
    }
    # A sampled parameter has no NA draw except one map_draws() could not
    # place, and a summary that dropped it would be biased.
    ests <- t(vapply(colnames(b), function(p) {
      if (anyNA(b[, p])) {
        c(Estimate = NA_real_, Q2.5 = NA_real_, Q97.5 = NA_real_)
      } else {
        estimates_summary(b[, p])
      }
    }, numeric(3)))
    data.frame(model = unname(models[[d]]), wi = wi, dpar = d,
               link = unname(links[[d]]), parameter = colnames(b), ests,
               row.names = NULL, stringsAsFactors = FALSE,
               check.names = FALSE)
  }))
  if (is.null(out)) {
    # Reached only for a fit holding none of the eight parameters, which no
    # equation bnec() fits produces. Returned as an empty frame of the right
    # shape rather than NULL so that rbind over a set does not drop a column.
    out <- empty_params_table()
  }
  out
}

#' @noRd
empty_params_table <- function() {
  data.frame(model = character(0), wi = numeric(0), dpar = character(0),
             link = character(0), parameter = character(0),
             Estimate = numeric(0), Q2.5 = numeric(0), Q97.5 = numeric(0),
             stringsAsFactors = FALSE, check.names = FALSE)
}

#' Say so where an equation's parameter is not in the draws
#'
#' A parameter absent from the posterior would otherwise be left out of the
#' table with no message. No route through \code{\link{bnec}} is known to
#' produce such a fit, so this is a guard: it names what is missing without
#' guessing why.
#'
#' @param found A \code{\link[base]{character}} vector of parameters in the
#' draws.
#' @param model A \code{\link[base]{character}} naming the equation, or
#' \code{NA}.
#' @param dpar A \code{\link[base]{character}} naming the block.
#'
#' @return \code{NULL}, invisibly. Called for the message.
#' @noRd
report_absent_params <- function(found, model, dpar) {
  absent <- setdiff(equation_par_names(model), found)
  if (length(absent) == 0) {
    return(invisible(NULL))
  }
  message("The ", model, " equation estimates ",
          paste(absent, collapse = ", "),
          ", which the posterior of the ", dpar, " block does not hold, so ",
          if (length(absent) == 1) "that parameter is " else
            "those parameters are ",
          "absent from the table rather than reported as missing.")
  invisible(NULL)
}

#' Report the scale the parameters are on
#'
#' The link is reported under the option \code{bayesnec.link_reported},
#' because a model set and a group share one family while the two components
#' of a \code{\link{bayesnechurdlefit}} do not. A method that dispatches
#' \code{\link{curve_params}} more than once reports for itself and then sets
#' the option, so that the message prints once.
#'
#' @param object A \code{\link{bayesnecfit}} or \code{prebayesnecfit}.
#' @param what A \code{\link[base]{character}} noun phrase naming what was
#' fitted, used as the subject of the message.
#'
#' @return \code{NULL}, invisibly. Called for the message.
#' @noRd
report_scales <- function(object, what) {
  if (!isTRUE(getOption("bayesnec.link_reported", FALSE))) {
    report_link(fit_links(object$fit), what)
  }
  invisible(NULL)
}

#' The fit a per-fit property should be read from
#'
#' @param object A \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @return An object with a \code{fit} and a \code{bayesnecformula}.
#' @noRd
representative_fit <- function(object) {
  if (inherits(object, "bayesmanecfit")) {
    object$mod_fits[[1]]
  } else {
    object
  }
}

#' Say so where a parameter is not on the response scale
#'
#' \code{\link{bnec}} assigns \code{link = "identity"} to every family it
#' accepts, so the usual case is silent. Where the caller named a link,
#' \pkg{brms} applies its inverse to the whole non-linear expression and
#' \code{top} and \code{bot} are on the link scale --- a \code{top} of 2.5 on a
#' \code{log} link is a control response of 12.2.
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
