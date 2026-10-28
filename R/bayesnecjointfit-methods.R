#' One level of a joint refit, seen as an ordinary fit
#'
#' Presents one level of a joint refit as a \code{\link{bayesnecfit}}, so that
#' the existing \code{\link{ecx}}, \code{\link{nsec}} and \code{\link{nec}}
#' methods run on it unchanged rather than being written again. The class is
#' internal and adds only the fields that name the level.
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
              level_spec = object$level_spec, retained_data = NULL,
              level_x = joint_level_x(object, level))
  out <- allot_class(out, c("bayesnecjointlevel", "bayesnecfit", "bnecfit"))
  # A NULL ne_posterior is the record that this level has no nec parameter.
  # Decide per level: a composed refit can mix threshold and smooth equations.
  if (joint_level_is_threshold(object, level)) {
    out$ne_posterior <- joint_level_draws(object, "nec", level)
    out$ne_type <- "NEC"
  } else {
    out$ne_type <- "NSEC"
  }
  out
}

#' The recorded predictor values of one level of a joint refit
#'
#' The refit's data span every level's concentrations, so a level's grid and
#' control are read from its own rows instead, as \code{\link{bnec_group}}
#' gives each level.
#'
#' @param object An object of class \code{\link{bayesnecjointfit}}.
#' @param level One level of \code{object$group_var}.
#'
#' @return A \code{\link[base]{numeric}} vector on the recorded scale, or
#' \code{NULL} where the refit holds no data column to read the level from,
#' in which case the whole data are used as before.
#'
#' @noRd
joint_level_x <- function(object, level) {
  dat <- object$data
  group_var <- object$group_var
  if (is.null(dat) || is.null(group_var) || !group_var %in% names(dat)) {
    return(NULL)
  }
  # The predictor's column name, found as prediction_grid() finds it.
  x_var <- attr(model.frame(object$bayesnecformula, data = object$fit$data),
                "bnec_pop")[["x_var"]]
  if (is.null(x_var) || !x_var %in% names(dat)) {
    return(NULL)
  }
  x <- dat[[x_var]][as.character(dat[[group_var]]) == level]
  x <- x[is.finite(x)]
  if (!length(x)) NULL else x
}

#' Whether the equation fitted at one level estimates a nec parameter
#'
#' Tests membership of \code{mod_groups$nec}, not \code{"ecx"} in the equation
#' name, which holds only by naming convention.
#'
#' @param object An object of class \code{\link{bayesnecjointfit}}, or an
#' internal \code{bayesnecjointlevel}.
#' @param level The level, defaulting to the one named on a
#' \code{bayesnecjointlevel}.
#'
#' @return \code{TRUE} or \code{FALSE}.
#'
#' @noRd
joint_level_is_threshold <- function(object, level = object$level) {
  joint_level_model(object, level) %in% mod_groups$nec
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
#' equations, and from the factor column otherwise. A composed refit with
#' \code{disp_by_level = FALSE} has no factor column in \code{fit$data}.
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
#' A level term names its coefficients \code{b_<parameter>_<group_var><level>},
#' not \code{b_<parameter>_Intercept}, so the usual readers find nothing on a
#' joint refit. \pkg{brms} can rename the level; see \code{joint_level_coef()}.
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
  # A dummy-coded refit names the coefficient after the factor and level, and
  # joint_level_coef() finds it. A composed refit gives each level a parameter
  # of its own, named with the tag from level_par_tags(), with an Intercept.
  tag <- object$level_spec$tags[[level]]
  vars <- variables(object$fit)
  var <- if (is.null(tag)) {
    joint_level_coef(vars, par, object$group_var, object$levels, level)
  } else {
    paste0("b_", par, tag, "_Intercept")
  }
  if (is.null(var) || !var %in% vars) {
    return(NULL)
  }
  as.numeric(as_draws_df(object$fit, variable = var)[[var]])
}

#' The name of one level's coefficient on a dummy-coded joint refit
#'
#' @param vars The variable names of the fit, as \code{brms::variables()}
#' returns them.
#' @param par The curve parameter, e.g. \code{"nec"} or \code{"bot"}.
#' @param group_var The name of the grouping column.
#' @param levels The levels of the refit, in the order of its factor.
#' @param level The level wanted.
#'
#' @details \pkg{brms} names the coefficient after the design-matrix column and
#' renames the label: \code{"Site A"} becomes \code{b_top_siteSiteA}. The level
#' is therefore found by position. The columns follow the factor's levels,
#' which \code{\link{bnec_joint}} pins to \code{levels}, so the coefficient of
#' the \emph{k}th level is the \emph{k}th variable with the prefix. A
#' \pkg{brms} parameter name has no underscore, so the prefix matches only this
#' parameter's level coefficients.
#'
#' @return A \code{\link[base]{character}} string, or \code{NULL} where the
#' parameter has no coefficient, which is where the equation has no such
#' parameter.
#'
#' @noRd
joint_level_coef <- function(vars, par, group_var, levels, level) {
  prefix <- paste0("b_", par, "_", group_var)
  coefs <- vars[startsWith(vars, prefix)]
  if (length(coefs) == 0) {
    return(NULL)
  }
  # Refuse a count mismatch: position would then attribute one level's draws
  # to another.
  if (length(coefs) != length(levels) || !level %in% levels) {
    stop("The ", par, " coefficients of the joint refit (",
         paste0(coefs, collapse = ", "), ") cannot be matched to its levels (",
         paste0("\"", levels, "\"", collapse = ", "), "). This should not",
         " happen; please report it.", call. = FALSE)
  }
  coefs[match(level, levels)]
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
    # A bayesnecjointlevel predicts its own level, but the factor still needs
    # every level, against which brms builds the design matrix. The levels come
    # from the object, because fit$data may not hold the factor column.
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
  # Default to the level's own range, as bnec_group() gives each level.
  if (all(is.na(x_range)) && length(x$level_x)) {
    x_range <- range(x$level_x)
  }
  prediction_grid(x$fit, x$bayesnecformula, x_range = x_range,
                  resolution = resolution,
                  level_spec = joint_level_spec(x))$newdata
}

#' Per-level toxicity estimates from a bayesnecjointfit
#'
#' The \code{\link{bayesnecgroupfit}} analogue, sharing \code{estimate_table()}
#' with it. Unlike the grouped method it returns a named list of draws under
#' \code{posterior = TRUE}, since a joint refit has no per-level fits to go to.
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

#' The no-effect estimate at every level of a composed joint refit
#'
#' Each level fits one equation, so its estimate is a NEC or an NSEC, never a
#' mixture. Under \code{no_effect = FALSE} a level without a \code{nec}
#' parameter returns \code{NA}; under \code{no_effect = TRUE} it returns
#' \code{\link{nsec}} on that level. Do not use \code{expand_nec()}'s
#' \code{nsec_off_curve()} here: it reads the control at the first grid column,
#' so its answer would differ from \code{nsec()}.
#'
#' @param object An object of class \code{\link{bayesnecjointfit}}.
#' @param no_effect A \code{\link[base]{logical}} value, see
#' \code{\link{nec}}.
#' @param ... Passed to \code{\link{nec}} or \code{\link{nsec}} on each level.
#'
#' @return A \code{\link[base]{data.frame}} with one row per level, or a named
#' \code{\link[base]{list}} of draws under \code{posterior = TRUE}.
#'
#' @importFrom brms ndraws
#' @importFrom stats quantile setNames
#'
#' @noRd
joint_ne_table <- function(object, no_effect, ...) {
  dots <- list(...)
  prob_vals <- if (is.null(dots$prob_vals)) {
    c(0.5, 0.025, 0.975)
  } else {
    dots$prob_vals
  }
  # Validated here as well as in the per-level methods because a fit whose
  # levels are all smooth reaches neither under no_effect = FALSE: every level
  # takes the NA row built below, whose names come from prob_vals.
  if (length(prob_vals) < 3 || prob_vals[1] < prob_vals[2] ||
      prob_vals[1] > prob_vals[3] || prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order.", call. = FALSE)
  }
  posterior <- isTRUE(dots$posterior)
  levels <- object$levels
  models <- vapply(levels, joint_level_model, character(1), object = object,
                   USE.NAMES = FALSE)
  threshold <- vapply(levels, joint_level_is_threshold, logical(1),
                      object = object, USE.NAMES = FALSE)
  ne_type <- rep(NA_character_, length(levels))
  ne_type[threshold] <- "NEC"
  if (no_effect) {
    ne_type[!threshold] <- "NSEC"
  }
  na_row <- quantile(NA_real_, probs = prob_vals, na.rm = TRUE)
  names(na_row) <- clean_names(na_row)
  est <- lapply(seq_along(levels), function(i) {
    lvl_fit <- joint_level_fit(object, levels[i])
    if (threshold[i]) {
      return(nec(lvl_fit, ...))
    }
    if (no_effect) {
      return(nsec(lvl_fit, ...))
    }
    # NA per draw, not NULL or zeros: a zero-length element drops out of a
    # vapply(), and zeros would read as an estimate at the origin.
    if (posterior) rep(NA_real_, ndraws(object$fit)) else na_row
  })
  names(est) <- levels
  if (any(!threshold) && !no_effect) {
    # Worded to read the same for one level or for every level.
    message("No NEC is defined at ",
            paste0("\"", levels[!threshold], "\" (", models[!threshold], ")",
                   collapse = ", "),
            ", where the equation fitted has no nec parameter, so those ",
            "estimates are NA. nec(x, no_effect = TRUE) returns the no-effect ",
            "estimate each level's own equation does support -- the nec ",
            "parameter for a threshold equation, the NSEC of the fitted curve ",
            "for a smooth one -- labelled by type.")
  }
  if (posterior) {
    attr(est, "model") <- setNames(models, levels)
    attr(est, "ne_type") <- setNames(ne_type, levels)
    return(est)
  }
  out <- estimate_table(est, levels, "nec")
  data.frame(level = out$level, model = models, ne_type = ne_type,
             out[, -1, drop = FALSE], stringsAsFactors = FALSE)
}

#' @noRd
#' @method nec bayesnecjointfit
#' @importFrom chk chk_lgl
#' @export
nec.bayesnecjointfit <- function(object, ..., no_effect = FALSE) {
  chk_lgl(no_effect)
  # Report the scale once for the refit, not once per level. See
  # report_fitted_scale().
  quiet <- report_fitted_scale(object, dots_xform(nec, list(...)), "nec")
  on.exit(options(quiet), add = TRUE)
  joint_ne_table(object, no_effect = no_effect, ...)
}

#' @noRd
#' @method ecx bayesnecjointfit
#' @export
ecx.bayesnecjointfit <- function(object, ...) {
  # Once for the refit, as in nec.bayesnecjointfit().
  quiet <- report_fitted_scale(object, dots_xform(ecx, list(...)), "ecx")
  on.exit(options(quiet), add = TRUE)
  joint_estimate_table(object, "ecx", function(f, ...) ecx(f, ...), ...)
}

#' @noRd
#' @method nsec bayesnecjointfit
#' @export
nsec.bayesnecjointfit <- function(object, ...) {
  # Once for the refit, as in nec.bayesnecjointfit().
  quiet <- report_fitted_scale(object, dots_xform(nsec, list(...)), "nsec")
  on.exit(options(quiet), add = TRUE)
  joint_estimate_table(object, "nsec", function(f, ...) nsec(f, ...), ...)
}

#' @noRd
#' @method ecnsec bayesnecjointfit
#' @export
ecnsec.bayesnecjointfit <- function(object, nsec, ...) {
  # Without this method, the inherited ecnsec.bnecfit() would run on the grid
  # of every level together. clean_names() below renames quantile()'s "50%" to
  # Q50, as the other estimators name it. `nsec` goes back in front of the dots
  # so that a positional xform is matched as it is on a single fit.
  quiet <- report_fitted_scale(
    object, dots_xform(ecnsec, c(list(nsec = nsec), list(...))), "ecnsec"
  )
  on.exit(options(quiet), add = TRUE)
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
#' What \code{ggbnec_data()} annotates: the \code{nec} draws summarised for a
#' threshold equation, and \code{nsec()} on the level for a smooth one.
#'
#' @param object An object of class \code{\link{bayesnecjointfit}}.
#' @param level One level of \code{object$group_var}.
#'
#' @return A named \code{\link[base]{numeric}} vector of three, on the fitted
#' scale, with attribute \code{"censored_summary"} where any entry is the end
#' of the prediction range rather than a quantile.
#'
#' @noRd
joint_level_ne <- function(object, level) {
  lvl_fit <- joint_level_fit(object, level)
  if (!is.null(lvl_fit$ne_posterior)) {
    return(estimates_summary(lvl_fit$ne_posterior))
  }
  out <- suppressWarnings(suppressMessages(nsec(lvl_fit)))
  # Keep the censoring record, which as.numeric() drops; without it a censored
  # NSEC's bound is drawn as a quantile.
  vals <- as.numeric(out)
  names(vals) <- c("Estimate", "Q2.5", "Q97.5")
  attr(vals, "censored_summary") <- attr(out, "censored_summary")
  vals
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
    if (joint_level_is_threshold(object, l)) "NEC" else "NSEC"
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
