#' One level of a joint refit, seen as an ordinary fit
#'
#' The estimators are not rewritten for a joint refit. Everything
#' \code{\link{ecx}}, \code{\link{nsec}} and \code{\link{nec}} read off a
#' \code{\link{bayesnecfit}} --- the \code{\link[brms]{brmsfit}}, the
#' \code{\link{bayesnecformula}}, the equation name and the prediction grid ---
#' is the same for a joint refit once the grid names a level, so a level is
#' presented to them as a fit in its own right and the existing methods run
#' unchanged. A second implementation of the crossing search, the control
#' posterior and the censoring warnings would drift from the first, and a
#' toxicity estimate that drifts has no symptom at the point of use.
#'
#' The class is internal and is never returned to a user. It inherits from
#' \code{\link{bayesnecfit}} so that the existing methods dispatch, and adds
#' only what tells the grid and the draws which level is meant.
#'
#' @param object An object of class \code{\link{bayesnecjointfit}}.
#' @param level One level of \code{object$group_var}.
#'
#' @return An object of class \code{bayesnecjointlevel}, inheriting from
#' \code{\link{bayesnecfit}}.
#'
#' @noRd
joint_level_fit <- function(object, level) {
  model <- joint_level_model(object, level)
  out <- list(fit = object$fit, bayesnecformula = object$bayesnecformula,
              model = model, group_var = object$group_var,
              level = level, levels = object$levels,
              level_spec = object$level_spec, retained_data = NULL)
  out <- allot_class(out, c("bayesnecjointlevel", "bayesnecfit", "bnecfit"))
  # Only a threshold equation has a nec parameter to read. nec.bayesnecfit()
  # stops on the equation name before it reaches ne_posterior for a smooth one,
  # so leaving it NULL there is what makes a joint refit refuse nec() with the
  # same message every other class refuses it with. Read off this level's own
  # equation, because a composed refit can have a threshold equation at one
  # level and a smooth one at the next.
  if (length(grep("ecx", model)) == 0) {
    out$ne_posterior <- joint_level_draws(object, "nec", level)
    out$ne_type <- "NEC"
  }
  out
}

#' The equation fitted at one level of a joint refit
#'
#' @param object An object of class \code{\link{bayesnecjointfit}}, or an
#' internal \code{bayesnecjointlevel}.
#' @param level The level, defaulting to the one named on a
#' \code{bayesnecjointlevel}.
#'
#' @details Where the levels of the grouped fit favoured different equations
#' the joint refit composes them, so there is no one equation for the fit and
#' \code{object$model} is \code{NA}. \code{models} carries one per level and is
#' what every per-level reader goes to.
#'
#' @return A \code{\link[base]{character}} string.
#'
#' @noRd
joint_level_model <- function(object, level = object$level) {
  if (!is.null(object$models)) {
    return(unname(object$models[[level]]))
  }
  object$model
}

#' The level each fitted row belongs to
#'
#' @param x An object of class \code{\link{bayesnecjointfit}}.
#'
#' @details Read from the indicator columns where the levels have different
#' equations, and from the factor column otherwise. The factor is not always
#' there to read: a composed refit with \code{disp_by_level = FALSE} never
#' writes the grouping variable into the formula, so \pkg{brms} does not keep
#' the column in \code{fit$data}, and the indicators are then the only record
#' of which rows are whose.
#'
#' @return A \code{\link[base]{character}} vector, one level name per row of
#' \code{x$fit$data}.
#'
#' @noRd
joint_row_levels <- function(x) {
  d <- x$fit$data
  if (is.null(x$level_spec$inds)) {
    return(as.character(d[[x$group_var]]))
  }
  out <- rep(NA_character_, nrow(d))
  for (l in x$levels) {
    ind <- x$level_spec$inds[[l]]
    if (!is.null(d[[ind]])) {
      out[d[[ind]] == 1] <- l
    }
  }
  out
}

#' Draws of one curve parameter at one level of a joint refit
#'
#' A level term names its coefficients \code{b_<parameter>_<group_var><level>}
#' rather than \code{b_<parameter>_Intercept}, which is why the existing readers
#' of \code{b_nec_Intercept} and \code{b_bot_Intercept} find nothing on a joint
#' refit.
#'
#' @param object An object of class \code{\link{bayesnecjointfit}}, or an
#' internal \code{bayesnecjointlevel}.
#' @param par The curve parameter, e.g. \code{"nec"} or \code{"bot"}.
#' @param level The level, defaulting to the one named on a
#' \code{bayesnecjointlevel}.
#'
#' @return A \code{\link[base]{numeric}} vector of draws, or \code{NULL} where
#' the equation has no such parameter.
#'
#' @importFrom brms as_draws_df variables
#' @noRd
joint_level_draws <- function(object, par, level = object$level) {
  # Two namings, one per branch. Dummy coded, the level is a coefficient of the
  # parameter and the name carries the factor and the level. Composed, the
  # level's parameter is a non-linear parameter in its own right with an
  # intercept, so the level is in the parameter name and the coefficient is
  # named Intercept like any other.
  tag <- object$level_spec$tags[[level]]
  var <- if (is.null(tag)) {
    paste0("b_", par, "_", object$group_var, level)
  } else {
    paste0("b_", par, tag, "_Intercept")
  }
  if (!var %in% variables(object$fit)) {
    return(NULL)
  }
  as.numeric(as_draws_df(object$fit, variable = var)[[var]])
}

#' The level structure of a joint refit, as prediction_grid takes it
#'
#' @param object An object of class \code{\link{bayesnecjointfit}}, or an
#' internal \code{bayesnecjointlevel}.
#' @param predict_levels The levels to build grid rows for. Defaults to every
#' level, which is what the whole-object methods want; a
#' \code{bayesnecjointlevel} predicts its own level alone.
#'
#' @return A \code{\link[base]{list}}, see \code{prediction_grid}.
#'
#' @noRd
joint_level_spec <- function(object, predict_levels = NULL) {
  levels <- object$levels
  if (!is.null(object$level)) {
    # A bayesnecjointlevel names one level and predicts that one alone. The
    # factor still has to be built with every level the fit was given, because
    # brms builds the design matrix against those. Carried on the object rather
    # than recovered from fit$data: a composed refit with a shared dispersion
    # never puts the factor in the formula, so brms does not keep the column.
    predict_levels <- object$level
  }
  if (is.null(levels)) {
    levels <- levels(object$fit$data[[object$group_var]])
  }
  # inds is NULL on a dummy-coded refit, and add_grid_levels() then adds the
  # factor column alone, which is the whole of what that branch's mean reads.
  list(group_var = object$group_var, levels = levels,
       predict_levels = predict_levels, inds = object$level_spec$inds)
}

#' @noRd
#' @method bnec_newdata bayesnecjointfit
#' @export
bnec_newdata.bayesnecjointfit <- function(x, resolution = 100, x_range = NA) {
  check_args_newdata(resolution, x_range)
  prediction_grid(x$fit, x$bayesnecformula, x_range = x_range,
                  resolution = resolution,
                  level_spec = joint_level_spec(x))$newdata
}

#' @noRd
#' @method bnec_newdata bayesnecjointlevel
#' @export
bnec_newdata.bayesnecjointlevel <- function(x, resolution = 100,
                                            x_range = NA) {
  check_args_newdata(resolution, x_range)
  prediction_grid(x$fit, x$bayesnecformula, x_range = x_range,
                  resolution = resolution,
                  level_spec = joint_level_spec(x))$newdata
}

#' Per-level toxicity estimates from a bayesnecjointfit
#'
#' The \code{\link{bayesnecgroupfit}} analogue, over the levels of one
#' posterior rather than over separate fits, and sharing
#' \code{estimate_table()} with it so that the two report the same columns in
#' the same order.
#'
#' \code{posterior = TRUE} returns a named \code{\link[base]{list}} of draws
#' rather than a table. The grouped method refuses it because a table of one
#' row per level cannot hold a posterior and the user has the per-level fits to
#' go to instead; a joint refit has no such fits, so refusing would leave no
#' route to the per-level draws at all.
#'
#' @param object An object of class \code{\link{bayesnecjointfit}}.
#' @param what The name of the calling method, for the error message.
#' @param fun A function taking a fit as its first argument.
#' @param ... Passed to the underlying method.
#'
#' @return A \code{\link[base]{data.frame}} with one row per level, or a named
#' \code{\link[base]{list}} under \code{posterior = TRUE}.
#'
#' @noRd
joint_estimate_table <- function(object, what, fun, ...) {
  est <- lapply(object$levels, function(lev) {
    fun(joint_level_fit(object, lev), ...)
  })
  names(est) <- object$levels
  if (isTRUE(list(...)$posterior)) {
    return(est)
  }
  estimate_table(est, object$levels, what)
}

#' @noRd
#' @method nec bayesnecjointfit
#' @export
nec.bayesnecjointfit <- function(object, ...) {
  joint_estimate_table(object, "nec", function(f, ...) nec(f, ...), ...)
}

#' @noRd
#' @method ecx bayesnecjointfit
#' @export
ecx.bayesnecjointfit <- function(object, ...) {
  joint_estimate_table(object, "ecx", function(f, ...) ecx(f, ...), ...)
}

#' @noRd
#' @method nsec bayesnecjointfit
#' @export
nsec.bayesnecjointfit <- function(object, ...) {
  joint_estimate_table(object, "nsec", function(f, ...) nsec(f, ...), ...)
}

#' @noRd
#' @method ecnsec bayesnecjointfit
#' @export
ecnsec.bayesnecjointfit <- function(object, nsec, ...) {
  # Needed to stop ecnsec.bnecfit() answering. A bayesnecjointfit inherits from
  # bnecfit, so before this method existed the inherited one ran on the
  # multi-level grid: its reference was column 2 of a two-point grid, which is
  # the first level's second point, and its type = "range" floor was the
  # minimum over every level's curve. Both are numbers, neither is an estimate
  # of anything, and nothing warned.
  # ecnsec() is the one estimator that leaves quantile() 's own "50%" names on
  # its return, and a column called 50% is not a syntactic name, so
  # as.data.frame() renames it X50. -- which reads as neither a quantile nor a
  # percentage. Renamed to the Q50 that ecx(), nsec() and nec() already use, so
  # the four tables have one set of column names between them.
  tabulate <- !isTRUE(list(...)$posterior)
  joint_estimate_table(object, "ecnsec", function(f, ...) {
    out <- ecnsec(f, nsec = nsec, ...)
    if (tabulate) {
      names(out) <- clean_names(out)
    }
    out
  }, ...)
}

#' The fitted curve at one level of a joint refit
#'
#' Built here rather than read off the object, because a joint refit stores no
#' \code{pred_vals}: one curve per level over a grid the user can move with
#' \code{x_range} is not a fixed quantity of the fit.
#'
#' @param object An object of class \code{\link{bayesnecjointfit}}.
#' @param level One level of \code{object$group_var}.
#' @param resolution The number of grid points.
#' @param x_range An optional predictor range.
#'
#' @return A \code{\link[base]{data.frame}} of \code{x}, \code{Estimate},
#' \code{Q2.5} and \code{Q97.5}.
#'
#' @importFrom stats fitted
#' @noRd
joint_level_curve <- function(object, level, resolution = 1000,
                              x_range = NA) {
  lvl_fit <- joint_level_fit(object, level)
  grid <- prediction_grid(object$fit, object$bayesnecformula,
                          x_range = x_range, resolution = resolution,
                          level_spec = joint_level_spec(object, level))
  y <- fitted(lvl_fit, newdata = grid$newdata, robust = TRUE,
              re_formula = NA, scale = "response")
  data.frame(x = grid$x_seq, Estimate = y[, "Estimate"],
             Q2.5 = y[, "Q2.5"], Q97.5 = y[, "Q97.5"])
}

#' The no-effect estimate at one level of a joint refit
#'
#' What \code{ggbnec_data()} annotates. A threshold equation has the estimate
#' as a parameter, so it is summarised from the draws exactly as
#' \code{expand_nec()} summarises \code{b_nec_Intercept}. A smooth equation has
#' no such parameter and its no-effect estimate is the NSEC read off the level's
#' own curve, which is what \code{nsec()} on the level returns.
#'
#' @param object An object of class \code{\link{bayesnecjointfit}}.
#' @param level One level of \code{object$group_var}.
#'
#' @return A \code{\link[base]{numeric}} vector of three, on the fitted scale.
#'
#' @noRd
joint_level_ne <- function(object, level) {
  lvl_fit <- joint_level_fit(object, level)
  if (!is.null(lvl_fit$ne_posterior)) {
    return(estimates_summary(lvl_fit$ne_posterior))
  }
  out <- suppressWarnings(suppressMessages(nsec(lvl_fit)))
  as.numeric(out)
}

#' Creates the data.frame for plotting a joint refit
#'
#' @inheritParams ggbnec_data
#'
#' @param x An object of class \code{\link{bayesnecjointfit}}, as returned by
#' \code{\link{bnec_joint}}.
#'
#' @inherit ggbnec_data return examples
#'
#' @method ggbnec_data bayesnecjointfit
#'
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom chk chk_lgl
#' @importFrom stats model.frame
#'
#' @export
ggbnec_data.bayesnecjointfit <- function(x, add_nec = TRUE, add_ecx = FALSE,
                                         xform = identity, ..., group = NULL) {
  chk_lgl(add_nec)
  chk_lgl(add_ecx)
  if (!inherits(xform, "function")) {
    stop("xform must be a function.")
  }
  panel_group <- is.null(group) || identical(group, x$group_var)
  raw_group <- if (panel_group) NULL else group
  r_all <- prep_raw_data(x$fit, x$bayesnecformula, group = raw_group)
  row_level <- joint_row_levels(x)
  bdat <- model.frame(x$bayesnecformula, data = x$fit$data,
                      run_par_checks = TRUE)
  pieces <- lapply(x$levels, function(lev) {
    curve <- joint_level_curve(x, lev)
    e_df <- data.frame(x_e = c(curve$x, rev(curve$x)),
                       y_e = c(curve$Estimate, rep(NA, nrow(curve))),
                       y_ci = c(curve$Q2.5, rev(curve$Q97.5)),
                       x_r = NA, y_r = NA)
    r_df <- r_all[row_level == lev, , drop = FALSE]
    if (!is.null(raw_group)) {
      e_df$group <- r_df$group[rep(NA_integer_, nrow(e_df))]
    }
    out <- rbind(e_df, r_df)
    if (!pop_var_is_transformed(bdat, "x_var")) {
      out <- out |>
        mutate(x_e = xform(.data$x_e), x_r = xform(.data$x_r))
    }
    if (add_nec) {
      out <- bind_nec(out, to_axis_scale(joint_level_ne(x, lev), bdat,
                                         x$bayesnecformula, curve$x, xform))
    }
    if (add_ecx) {
      ecx_vals <- to_axis_scale(
        plot_ecx(joint_level_fit(x, lev), x$fit$family$family, list(...)),
        bdat, x$bayesnecformula, curve$x, xform
      )
      out <- bind_ecx(out, ecx_vals)
    }
    out$panel <- factor(rep(lev, nrow(out)), levels = x$levels)
    if (panel_group) {
      out$group <- out$panel
    }
    out
  })
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  attr(out, "group_var") <- if (panel_group) x$group_var else group
  # TRUE whichever grouping was asked for when it is the level term: that term
  # is population-level, so the model estimates a curve for each of its levels,
  # which is exactly what group_fitted asserts.
  attr(out, "group_fitted") <- if (panel_group) {
    TRUE
  } else {
    attr(r_all, "group_fitted")
  }
  attr(out, "panel_var") <- x$group_var
  out
}

#' @rdname autoplot
#' @order 5
#'
#' @method autoplot bayesnecjointfit
#'
#' @inherit autoplot description return examples
#'
#' @importFrom chk chk_lgl
#'
#' @export
autoplot.bayesnecjointfit <- function(object, ..., nec = TRUE, ecx = FALSE,
                                      xform = identity, group = NULL,
                                      group_aes = c("line", "colour")) {
  chk_lgl(nec)
  chk_lgl(ecx)
  group_aes <- match.arg(group_aes)
  if (!inherits(xform, "function")) {
    stop("xform must be a function.")
  }
  dat <- ggbnec_data(object, add_nec = nec, add_ecx = ecx,
                     xform = xform, group = group, ...)
  panel_labs <- paste0(object$group_var, " = ", object$levels)
  names(panel_labs) <- object$levels
  level <- as.character(dat$panel)
  dat$model <- factor(unname(panel_labs[level]), levels = unname(panel_labs))
  # Per level, not per fit: a composed refit can have a threshold equation in
  # one panel and a smooth one in the next, and the annotation names what was
  # read off that panel's curve.
  lev_tag <- vapply(object$levels, function(l) {
    if (length(grep("ecx", joint_level_model(object, l))) > 0) "NSEC" else "NEC"
  }, character(1))
  dat$tag <- unname(lev_tag[level])
  show_group <- !is.null(group) && !identical(group, object$group_var)
  group_label <- if (show_group) {
    plot_group_label(dat, group, group_aes)
  } else {
    NULL
  }
  ggbnec(dat, nec = nec, ecx = ecx, group = show_group,
         group_aes = group_aes, group_label = group_label,
         group_fitted = !identical(attr(dat, "group_fitted"), FALSE))
}
