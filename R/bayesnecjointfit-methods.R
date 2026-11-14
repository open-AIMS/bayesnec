#' One level of a joint refit for toxicity estimators
#'
#' Holds the fields that toxicity estimators read for one level. The internal
#' class exposes only those estimators and its predictor grid.
#'
#' @param object An object of class \code{\link{bayesnecjointfit}}.
#' @param level One level of \code{object$group_var}.
#'
#' @return An internal \code{bayesnecjointlevel} adapter.
#'
#' @noRd
joint_level_fit <- function(object, level) {
  model <- joint_level_model(object, level)
  out <- list(fit = object$fit, bayesnecformula = object$bayesnecformula,
              model = model, group_var = object$group_var,
              level = level, levels = object$levels,
              level_spec = object$level_spec, retained_data = NULL,
              level_x = joint_level_x(object, level),
              x_control = recorded_x_control(object))
  out <- allot_class(out, "bayesnecjointlevel")
  # Keep the sampled breakpoint only where it is also the no-effect estimate.
  if (sampled_nec_is_no_effect(model)) {
    out$ne_posterior <- joint_level_draws(object, "nec", level)
    out$ne_type <- "NEC"
  } else {
    out$ne_type <- "NSEC"
  }
  out
}

#' Label the equations selected for a joint refit
#'
#' @param fit A \code{bayesnecjointfit}.
#' @return A character label pairing each level with its equation.
#' @noRd
#' @export
group_model_label.bayesnecjointfit <- function(fit) {
  paste(paste(fit$levels, unname(fit$models), sep = " = "),
        collapse = ", ")
}

#' NEC draws or summaries for one joint level
#'
#' @param object A \code{bayesnecjointlevel} adapter.
#' @param ... Estimator settings.
#' @return A numeric summary or posterior vector for this level.
#' @method nec bayesnecjointlevel
#' @export
nec.bayesnecjointlevel <- function(object, ...) nec.bayesnecfit(object, ...)

#' NSEC draws or summaries for one joint level
#'
#' @inheritParams nec.bayesnecjointlevel
#' @return A numeric summary or posterior vector for this level.
#' @method nsec bayesnecjointlevel
#' @export
nsec.bayesnecjointlevel <- function(object, ...) nsec.bayesnecfit(object, ...)

#' ECx draws or summaries for one joint level
#'
#' @inheritParams nec.bayesnecjointlevel
#' @return A numeric summary or posterior vector for this level.
#' @method ecx bayesnecjointlevel
#' @export
ecx.bayesnecjointlevel <- function(object, ...) ecx.bayesnecfit(object, ...)

#' ECNSEC draws or summaries for one joint level
#'
#' @inheritParams nec.bayesnecjointlevel
#' @return A numeric summary or posterior vector for this level.
#' @method ecnsec bayesnecjointlevel
#' @export
ecnsec.bayesnecjointlevel <- function(object, ...) ecnsec.bnecfit(object, ...)

#' Posterior predictions used inside toxicity estimators
#'
#' @param object A fit or one joint level.
#' @param newdata Predictor data.
#' @param ... Passed to posterior prediction.
#' @return A draws-by-observation matrix.
#' @noRd
estimator_epred <- function(object, newdata, ...) {
  if (!inherits(object, "bayesnecjointlevel")) {
    return(posterior_epred(object, newdata = newdata, ...))
  }
  check_newdata_disp(object$bayesnecformula, newdata)
  factorised_count_epred(object$fit, object$bayesnecformula,
                         newdata = newdata, ...)
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
#' @details \pkg{brms} can rename level labels in coefficient names, so find
#' a level by its position in the factor levels pinned by \code{bnec_joint()}.
#' The prefix matches only this parameter, whose name has no underscore.
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

#' @return A predictor grid with rows for every joint level.
#' @noRd
#' @method bnec_newdata bayesnecjointfit
#' @export
bnec_newdata.bayesnecjointfit <- function(x, resolution = 100, x_range = NA,
                                          ...) {
  check_unused_dots(list(...), "bnec_newdata")
  check_args_newdata(resolution, x_range)
  prediction_grid(x$fit, x$bayesnecformula, x_range = x_range,
                  resolution = resolution,
                  level_spec = joint_level_spec(x),
                  x_control = recorded_x_control(x))$newdata
}

#' @return A predictor grid containing only this adapter's level.
#' @noRd
#' @method bnec_newdata bayesnecjointlevel
#' @export
bnec_newdata.bayesnecjointlevel <- function(x, resolution = 100,
                                            x_range = NA, ...) {
  check_unused_dots(list(...), "bnec_newdata")
  check_args_newdata(resolution, x_range)
  # Default to the level's own range, as bnec_group() gives each level.
  if (all(is.na(x_range)) && length(x$level_x)) {
    x_range <- c(control_x(x), max(x$level_x))
  }
  prediction_grid(x$fit, x$bayesnecformula, x_range = x_range,
                  resolution = resolution,
                  level_spec = joint_level_spec(x),
                  x_control = recorded_x_control(x))$newdata
}

#' Refuse `which` on a joint refit
#'
#' Checked on the joint refit, so that the message names its class rather than
#' the internal class of one level.
#'
#' @param dots The \code{...} of the calling method, as a list.
#' @param object An object of class \code{\link{bayesnecjointfit}}.
#'
#' @return Invisibly \code{TRUE}, or an error.
#'
#' @noRd
check_joint_which <- function(dots, object) {
  if ("which" %in% names(dots)) {
    stop("`which` selects a component of a bayesnechurdlefit, as returned by",
         " bnec_hurdle(). A bayesnecjointfit has no components, and its",
         " estimates are returned one row per level of \"", object$group_var,
         "\".", call. = FALSE)
  }
  invisible(TRUE)
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
joint_estimate_table <- function(object, what, fun, ..., pool = "none",
                                  nsec = NULL) {
  check_joint_which(list(...), object)
  if (is.null(pool)) pool <- "none"
  pool <- validate_pool(pool, what)
  fits <- lapply(object$levels, function(lev) joint_level_fit(object, lev))
  names(fits) <- object$levels
  dots <- shared_level_control(fits, list(...))
  posterior <- isTRUE(dots$posterior)
  finish <- if (identical(what, "nec")) identity else function(x) {
    record_level_controls(x, level_control_values(fits, dots$x_control))
  }
  if (identical(pool, "mean_curve")) {
    if (identical(what, "ecnsec")) dots$nsec <- nsec
    return(finish(mean_curve_estimate(fits, as.numeric(table(factor(
      joint_row_levels(object), levels = object$levels))), what, dots)))
  }
  if (!identical(pool, "none")) dots$posterior <- TRUE
  est <- lapply(fits, function(f) {
    do.call(fun, c(list(f), dots))
  })
  names(est) <- object$levels
  if (!identical(pool, "none")) {
    weights <- as.numeric(table(factor(joint_row_levels(object),
                                       levels = object$levels)))
    pooled <- pool_estimate_draws(est, weights, pool)
    if (posterior) return(finish(pooled))
    probs <- if (is.null(dots$prob_vals)) c(0.5, 0.025, 0.975) else
      dots$prob_vals
    out <- summarise_censored(pooled, probs)
    names(out) <- clean_names(out)
    if (identical(what, "nsec") &&
        !is.null(attr(pooled, "ecnsec_relativeP"))) {
      attr(out, "ecnsec_relativeP") <- stats::quantile(
        attr(pooled, "ecnsec_relativeP"), probs = probs, na.rm = TRUE
      )
    }
    return(finish(out))
  }
  if (posterior) {
    return(finish(est))
  }
  finish(estimate_table(est, object$levels, what))
}

#' The no-effect estimate at every level of a composed joint refit
#'
#' Each level fits one equation, so its estimate is a NEC or an NSEC, never a
#' mixture. Under \code{no_effect = FALSE} a level without a \code{nec}
#' parameter returns \code{NA}, while a hormesis threshold equation returns
#' its curve-read NSEC. Under \code{no_effect = TRUE}, a smooth equation also
#' returns \code{\link{nsec}} on that level. Do not use \code{expand_nec()}'s
#' \code{nsec_off_curve()} here: it reads the control at the lowest
#' concentration of every level together, and \code{nsec()} at the level's own
#' control concentration.
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
joint_ne_table <- function(object, no_effect, ..., pool = "none") {
  check_joint_which(list(...), object)
  if (is.null(pool)) pool <- "none"
  pool <- validate_pool(pool, "nec")
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
  if (!identical(pool, "none")) dots$posterior <- TRUE
  levels <- object$levels
  models <- vapply(levels, joint_level_model, character(1), object = object,
                   USE.NAMES = FALSE)
  threshold <- vapply(levels, joint_level_is_threshold, logical(1),
                      object = object, USE.NAMES = FALSE)
  sampled <- vapply(models, sampled_nec_is_no_effect, logical(1))
  ne_type <- rep(NA_character_, length(levels))
  ne_type[sampled] <- "NEC"
  ne_type[threshold & !sampled] <- "NSEC"
  if (no_effect) {
    ne_type[!threshold] <- "NSEC"
  }
  na_row <- quantile(NA_real_, probs = prob_vals, na.rm = TRUE)
  names(na_row) <- clean_names(na_row)
  est <- lapply(seq_along(levels), function(i) {
    lvl_fit <- joint_level_fit(object, levels[i])
    if (sampled[i]) {
      return(do.call(nec, c(list(lvl_fit), dots)))
    }
    if (threshold[i] || no_effect) {
      return(do.call(nsec, c(list(lvl_fit), dots)))
    }
    # NA per draw, not NULL or zeros: a zero-length element drops out of a
    # vapply(), and zeros would read as an estimate at the origin.
    if (posterior) rep(NA_real_, ndraws(object$fit)) else na_row
  })
  names(est) <- levels
  if (!identical(pool, "none")) {
    if (any(!threshold) && !no_effect) {
      stop("Pooling NEC values needs `no_effect = TRUE` when a level fits",
           " a smooth equation.", call. = FALSE)
    }
    weights <- as.numeric(table(factor(joint_row_levels(object),
                                       levels = levels)))
    pooled <- pool_estimate_draws(est, weights, pool)
    if (posterior) return(pooled)
    out <- summarise_censored(pooled, prob_vals)
    names(out) <- clean_names(out)
    return(out)
  }
  if (any(!threshold) && !no_effect) {
    # Worded to read the same for one level or for every level.
    message("No NEC is defined at ",
            paste0("\"", levels[!threshold], "\" (", models[!threshold], ")",
                   collapse = ", "),
            ", where the equation fitted has no nec parameter, so those ",
            "estimates are NA. nec(x, no_effect = TRUE) returns the no-effect ",
            "estimate each level's equation supports -- a sampled NEC for a ",
            "monotone threshold equation, or the NSEC of a hormesis threshold ",
            "or smooth curve -- labelled by type.")
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
  joint_ne_table(object, no_effect = no_effect, ...)
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
  check_nsec_no_type(list(...))
  joint_estimate_table(object, "nsec", function(f, ...) nsec(f, ...), ...)
}

#' @noRd
#' @method ecnsec bayesnecjointfit
#' @export
ecnsec.bayesnecjointfit <- function(object, nsec, ...) {
  # Without this method, the inherited ecnsec.bnecfit() would run on the grid
  # of every level together. clean_names() below renames quantile()'s "50%" to
  # Q50, as the other estimators name it.
  tabulate <- !isTRUE(list(...)$posterior)
  joint_estimate_table(object, "ecnsec", function(f, ...) {
    out <- ecnsec(f, nsec = nsec, ...)
    if (tabulate) {
      names(out) <- clean_names(out)
    }
    out
  }, ..., nsec = nsec)
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
                              x_range = NA,
                              x_scale = c("recorded", "fitted")) {
  lvl_fit <- joint_level_fit(object, level)
  if (is.na(x_range[1]) && length(lvl_fit$level_x)) {
    x_range <- c(control_x(lvl_fit), max(lvl_fit$level_x))
  }
  grid <- prediction_grid(object$fit, object$bayesnecformula,
                          x_range = x_range, resolution = resolution,
                          level_spec = joint_level_spec(object, level),
                          x_control = recorded_x_control(object))
  if (match.arg(x_scale) == "fitted") {
    fitted_x <- as.numeric(sub_x_transformation(grid$x_seq,
                                                object$bayesnecformula))
    if (any(!is.finite(fitted_x)) ||
        !(all(diff(fitted_x) > 0) || all(diff(fitted_x) < 0))) {
      stop("The fitted predictor axis must be finite and strictly monotone.",
           call. = FALSE)
    }
    x_seq <- seq(min(fitted_x), max(fitted_x), length.out = resolution)
    inverse <- recorded_x_map(object$bayesnecformula, grid$x_seq,
                              inline_x_transform(object))
    grid$newdata[[grid$x_var]] <- as.numeric(inverse(x_seq))
    grid$x_seq <- x_seq
  }
  y <- fitted.bayesnecfit(lvl_fit, newdata = grid$newdata, robust = TRUE,
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
#' @param xform \code{NULL} for the recorded scale, or \code{identity} for the
#' fitted scale.
#'
#' @return A named \code{\link[base]{numeric}} vector of three, with attribute
#' \code{"censored_summary"} where any entry is the end of the prediction
#' range rather than a quantile.
#'
#' @noRd
joint_level_ne <- function(object, level, xform = identity) {
  lvl_fit <- joint_level_fit(object, level)
  if (!is.null(observed_level_spec(lvl_fit)) &&
      sampled_nec_is_no_effect(lvl_fit$model)) {
    out <- suppressWarnings(suppressMessages(nec(lvl_fit, xform = xform)))
    vals <- as.numeric(out)
    names(vals) <- c("Estimate", "Q2.5", "Q97.5")
    attr(vals, "censored_summary") <- attr(out, "censored_summary")
    return(vals)
  }
  if (!is.null(lvl_fit$ne_posterior)) {
    map <- recorded_xform(lvl_fit, xform)
    return(estimates_summary(map_draws(lvl_fit$ne_posterior, map,
                                       what = lvl_fit$ne_type)))
  }
  out <- suppressWarnings(suppressMessages(nsec(lvl_fit, xform = xform)))
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
                                         xform = NULL, ..., group = NULL,
                                         x_scale = c("recorded", "fitted"),
                                         which = "combined") {
  chk_lgl(add_nec)
  chk_lgl(add_ecx)
  check_xform(xform)
  check_plot_single_curve(which, x)
  check_plot_dpar(list(...), x)
  if (add_ecx) {
    check_plot_relative(list(...), x)
  }
  x_scale <- plot_x_scale(x, x_scale, xform)
  panel_group <- is.null(group) || identical(group, x$group_var)
  raw_group <- if (panel_group) NULL else group
  r_all <- prep_raw_data(x$fit, x$bayesnecformula, group = raw_group)
  row_level <- joint_row_levels(x)
  bdat <- model.frame(x$bayesnecformula, data = x$fit$data,
                      run_par_checks = TRUE)
  # On the recorded axis with no xform, draw what the estimators return.
  recorded <- x_scale == "recorded" && plot_reads_recorded(bdat, xform)
  est_xform <- if (recorded) NULL else identity
  axis_xform <- if (is.null(xform)) identity else xform
  pieces <- lapply(x$levels, function(lev) {
    curve <- joint_level_curve(x, lev, x_scale = x_scale)
    e_df <- data.frame(x_e = c(curve$x, rev(curve$x)),
                       y_e = c(curve$Estimate, rep(NA, nrow(curve))),
                       y_ci = c(curve$Q2.5, rev(curve$Q97.5)),
                       x_r = NA, y_r = NA)
    r_df <- r_all[row_level == lev, , drop = FALSE]
    if (!is.null(raw_group)) {
      e_df$group <- r_df$group[rep(NA_integer_, nrow(e_df))]
    }
    out <- rbind(e_df, r_df)
    if (x_scale == "fitted") {
      out$x_r <- as.numeric(sub_x_transformation(out$x_r,
                                                 x$bayesnecformula))
    }
    if (!pop_var_is_transformed(bdat, "x_var")) {
      out <- out |>
        mutate(x_e = axis_xform(.data$x_e), x_r = axis_xform(.data$x_r))
    }
    if (add_nec) {
      ne_vals <- joint_level_ne(x, lev, est_xform)
      if (!recorded && x_scale != "fitted") {
        ne_vals <- to_axis_scale(ne_vals, bdat, x$bayesnecformula, curve$x,
                                 axis_xform)
      }
      out <- bind_nec(out, ne_vals, if (x_scale == "fitted") {
        fitted_axis_labels(ne_vals, x, r_all$x_r)
      } else {
        ne_vals
      })
    }
    if (add_ecx) {
      ecx_vals <- plot_ecx(joint_level_fit(x, lev), x$fit$family$family,
                           list(...), est_xform)
      if (!recorded && x_scale != "fitted") {
        ecx_vals <- to_axis_scale(ecx_vals, bdat, x$bayesnecformula, curve$x,
                                  axis_xform)
      }
      out <- bind_ecx(out, ecx_vals, if (x_scale == "fitted") {
        fitted_axis_labels(ecx_vals, x, r_all$x_r)
      } else {
        ecx_vals
      })
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
  if (x_scale == "fitted") {
    attr(out, "x_axis") <- plot_x_axis(r_all$x_r, x$bayesnecformula)
  }
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
                                      xform = NULL, group = NULL,
                                      group_aes = c("line", "colour"),
                                      point_alpha = 1,
                                      x_scale = c("recorded", "fitted"),
                                      xlab = "Predictor", ylab = NULL) {
  chk_lgl(nec)
  chk_lgl(ecx)
  group_aes <- match.arg(group_aes)
  check_xform(xform)
  check_point_alpha(point_alpha)
  x_scale <- plot_x_scale(object, x_scale, xform)
  dat <- ggbnec_data(object, add_nec = nec, add_ecx = ecx,
                     xform = xform, x_scale = x_scale, group = group, ...)
  x_axis <- attr(dat, "x_axis")
  panel_labs <- paste0(object$group_var, " = ", object$levels)
  names(panel_labs) <- object$levels
  level <- as.character(dat$panel)
  dat$model <- factor(unname(panel_labs[level]), levels = unname(panel_labs))
  # Per level, not per fit: a composed refit can have a threshold equation in
  # one panel and a smooth one in the next, and the annotation names what was
  # read off that panel's curve.
  lev_tag <- vapply(object$levels, function(l) {
    if (sampled_nec_is_no_effect(joint_level_model(object, l))) {
      "NEC"
    } else {
      "NSEC"
    }
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
         group_fitted = !identical(attr(dat, "group_fitted"), FALSE),
         point_alpha = point_alpha, x_axis = x_axis, xlab = xlab,
         ylab = plot_ylab(ylab, object))
}

#' @noRd
#' @method pull_brmsfit bayesnecjointfit
#' @export
pull_brmsfit.bayesnecjointfit <- function(object, ...) {
  object$fit
}

#' Refuse a method that a joint refit cannot define
#'
#' Stops with a message naming what to use instead.
#'
#' @param x An object of class \code{\link{bayesnecjointfit}}.
#' @param what The call refused, as \code{"amend()"}.
#' @param remedy A sentence naming what to use instead.
#'
#' @return Nothing; always an error.
#'
#' @noRd
refuse_joint_method <- function(x, what, remedy) {
  levels_of <- if (is.null(x$group_var)) "" else
    paste0(" of \"", x$group_var, "\"")
  stop(what, " is not defined for a bayesnecjointfit, which fits every level",
       levels_of, " in one posterior. ", remedy, call. = FALSE)
}

#' The coefficients that belong to one level of a joint refit alone
#'
#' @param object An object of class \code{\link{bayesnecjointfit}}.
#' @param level One level of \code{object$group_var}.
#' @param vars The variable names of the fit, as \code{brms::variables()}
#' returns them.
#'
#' @details The level's curve parameters and, where the dispersion takes a
#' value per level, its dispersion parameter. A parameter the levels share,
#' such as a dispersion fitted once or a group-level term, belongs to no level.
#'
#' @return A \code{\link[base]{character}} vector of names in \code{vars},
#' each with its \code{b_} prefix.
#'
#' @importFrom brms variables
#' @noRd
joint_level_vars <- function(object, level, vars = variables(object$fit)) {
  model <- joint_level_model(object, level)
  curve_pars <- names(get(paste0("bf_", model))$pforms)
  pars <- curve_pars
  if (isTRUE(object$disp_by_level) && !is.null(object$family)) {
    pars <- c(pars, disp_dpar(object$family))
  }
  tag <- object$level_spec$tags[[level]]
  out <- vapply(pars, function(p) {
    # A composed refit names each level's curve parameters with its tag, but
    # writes the dispersion on the factor, as a dummy-coded refit does.
    v <- if (!is.null(tag) && p %in% curve_pars) {
      paste0("b_", p, tag, "_Intercept")
    } else {
      joint_level_coef(vars, p, object$group_var, object$levels, level)
    }
    if (is.null(v)) NA_character_ else v
  }, character(1), USE.NAMES = FALSE)
  out[!is.na(out) & out %in% vars]
}

#' The population-level parameters of a joint refit, one page per level
#'
#' @param object An object of class \code{\link{bayesnecjointfit}}.
#'
#' @return A named \code{\link[base]{list}}, one element per level that owns
#' a row of \code{\link[brms]{fixef}}, and a last element \code{shared} where
#' some row belongs to no level. Each element has a \code{title} and the
#' row names in \code{pars}.
#'
#' @importFrom brms fixef variables
#' @noRd
joint_fixef_pages <- function(object) {
  rows <- rownames(fixef(object$fit))
  vars <- variables(object$fit)
  pages <- lapply(object$levels, function(l) {
    own <- sub("^b_", "", joint_level_vars(object, l, vars))
    list(title = paste0(object$group_var, " = ", l, ": ",
                        joint_level_model(object, l)),
         pars = rows[rows %in% own])
  })
  names(pages) <- object$levels
  shared <- setdiff(rows, unlist(lapply(pages, `[[`, "pars")))
  if (length(shared) > 0) {
    pages$shared <- list(title = "Parameters shared by every level",
                         pars = shared)
  }
  pages[vapply(pages, function(p) length(p$pars) > 0, logical(1))]
}

#' Complete a newdata for a joint refit
#'
#' @param object An object of class \code{\link{bayesnecjointfit}}.
#' @param newdata A \code{\link[base]{data.frame}}, or \code{NULL}.
#'
#' @details Each row names its level in the grouping column. Where the levels
#' were fitted different equations, the indicator columns the composed mean
#' reads are written from that column, so a grid written by hand needs only
#' the predictor and the level.
#'
#' @return \code{NULL} where \code{newdata} is, and otherwise \code{newdata}
#' with the grouping column a factor over the refit's levels and any
#' indicator column added.
#'
#' @noRd
joint_newdata <- function(object, newdata) {
  if (is.null(newdata)) {
    return(NULL)
  }
  if (!is.data.frame(newdata)) {
    stop("`newdata` must be a data.frame.", call. = FALSE)
  }
  group_var <- object$group_var
  if (!group_var %in% names(newdata)) {
    stop("`newdata` has no column \"", group_var, "\". A joint refit",
         " predicts each level from its own coefficients, so each row must",
         " name its level; bnec_newdata(x) builds such a grid.",
         call. = FALSE)
  }
  lev <- as.character(newdata[[group_var]])
  unknown <- setdiff(unique(lev[!is.na(lev)]), object$levels)
  if (anyNA(lev) || length(unknown) > 0) {
    stop("Column \"", group_var, "\" of `newdata` must hold a level of the",
         " joint refit (", paste0("\"", object$levels, "\"", collapse = ", "),
         ") in every row", if (length(unknown) > 0) {
           paste0("; it holds ", paste0("\"", unknown, "\"", collapse = ", "))
         }, ".", call. = FALSE)
  }
  newdata[[group_var]] <- factor(lev, levels = object$levels)
  inds <- object$level_spec$inds
  for (l in names(inds)) {
    newdata[[inds[[l]]]] <- as.numeric(lev == l)
  }
  newdata
}

#' @noRd
#' @method fitted bayesnecjointfit
#' @importFrom stats fitted
#' @export
fitted.bayesnecjointfit <- function(object, newdata = NULL, ...) {
  fitted.bayesnecfit(object, newdata = joint_newdata(object, newdata), ...)
}

#' @noRd
#' @method predict bayesnecjointfit
#' @importFrom stats predict
#' @export
predict.bayesnecjointfit <- function(object, newdata = NULL, ...) {
  predict.bayesnecfit(object, newdata = joint_newdata(object, newdata), ...)
}

#' @noRd
#' @method posterior_epred bayesnecjointfit
#' @importFrom brms posterior_epred
#' @export
posterior_epred.bayesnecjointfit <- function(object, newdata = NULL, ...) {
  posterior_epred.bayesnecfit(object,
                              newdata = joint_newdata(object, newdata), ...)
}

#' @noRd
#' @method posterior_predict bayesnecjointfit
#' @importFrom brms posterior_predict
#' @export
posterior_predict.bayesnecjointfit <- function(object, newdata = NULL, ...) {
  posterior_predict.bayesnecfit(object,
                                newdata = joint_newdata(object, newdata), ...)
}

#' The fitted rows of each level of a joint refit
#'
#' @param object An object of class \code{\link{bayesnecjointfit}}.
#'
#' @return A named \code{\link[base]{list}} of row indices into
#' \code{object$fit$data}, one element per level.
#'
#' @noRd
joint_level_rows <- function(object) {
  row_level <- joint_row_levels(object)
  out <- lapply(object$levels, function(l) which(row_level == l))
  names(out) <- object$levels
  out
}

#' @noRd
#' @method pp_check bayesnecjointfit
#' @importFrom brms pp_check
#' @importFrom ggplot2 ggtitle
#' @export
pp_check.bayesnecjointfit <- function(object, ...) {
  if ("newdata" %in% ...names()) {
    stop("pp_check() on a bayesnecjointfit checks each level against its own",
         " rows, so it takes no `newdata`. Call brms::pp_check() on the",
         " brmsfit that pull_brmsfit(x) returns to check other data.",
         call. = FALSE)
  }
  d <- object$fit$data
  rows <- joint_level_rows(object)
  out <- lapply(object$levels, function(l) {
    pp_check(object$fit, newdata = d[rows[[l]], , drop = FALSE], ...) +
      ggtitle(paste0(object$group_var, " = ", l))
  })
  names(out) <- object$levels
  out
}

#' @noRd
#' @method rhat bayesnecjointfit
#' @importFrom brms rhat variables
#' @importFrom chk chk_number
#' @export
rhat.bayesnecjointfit <- function(x, rhat_cutoff = 1.01, ...) {
  chk_number(rhat_cutoff)
  vals <- screenable_pars(rhat(x$fit))
  vars <- variables(x$fit)
  own <- lapply(x$levels, joint_level_vars, object = x, vars = vars)
  # Give every level the parameters no level owns, lp__ among them.
  shared <- setdiff(names(vals), unlist(own))
  out <- lapply(own, function(v) {
    level_vals <- clean_rhat_names(vals[c(intersect(v, names(vals)), shared)])
    list(rhat_vals = level_vals,
         failed = any(level_vals > rhat_cutoff, na.rm = TRUE))
  })
  names(out) <- x$levels
  out
}

#' @noRd
#' @method check_chains bayesnecjointfit
#' @importFrom grDevices pdf dev.cur dev.list dev.off
#' @importFrom chk chk_character
#' @export
check_chains.bayesnecjointfit <- function(x, filename = NA, ...) {
  if (!is.na(filename)) {
    chk_character(filename)
    pdf(file = paste(filename, ".pdf", sep = ""), onefile = TRUE)
    pdf_dev <- dev.cur()
    on.exit(if (pdf_dev %in% dev.list()) dev.off(pdf_dev), add = TRUE)
  }
  for (page in joint_fixef_pages(x)) {
    plot_chains(x$fit, page$pars, page$title)
  }
  if (!is.na(filename)) {
    dev.off(pdf_dev)
    message("Chain plots saved to file ", filename, ".pdf")
  }
  invisible(NULL)
}

#' @return Invisibly, a named list of plots for the joint levels.
#' @noRd
#' @method check_priors bayesnecjointfit
#' @importFrom ggplot2 ggtitle
#' @importFrom grDevices devAskNewPage pdf dev.cur dev.list dev.off
#' @importFrom chk chk_character
#' @export
check_priors.bayesnecjointfit <- function(object, filename = NA, ask = TRUE,
                                         ...) {
  check_unused_dots(list(...), "check_priors")
  if (!is.na(filename)) {
    chk_character(filename)
  }
  pages <- joint_fixef_pages(object)
  plots <- lapply(pages, function(page) {
    prior_posterior_plot(object$fit, page$pars) + ggtitle(page$title)
  })
  # Leave the devices as they were found, as the bayesmanecfit method does.
  if (!is.na(filename)) {
    pdf(file = paste(filename, ".pdf", sep = ""), onefile = TRUE,
        width = 12, height = 4)
    pdf_dev <- dev.cur()
    on.exit(if (pdf_dev %in% dev.list()) dev.off(pdf_dev), add = TRUE)
  } else {
    default_ask <- devAskNewPage()
    on.exit(devAskNewPage(default_ask), add = TRUE)
    devAskNewPage(ask = ask)
  }
  for (p in plots) {
    print(p)
  }
  if (!is.na(filename)) {
    dev.off(pdf_dev)
    message("Probability density plots saved to file ", filename, ".pdf")
  }
  invisible(plots)
}

#' @noRd
#' @method check_fit bayesnecjointfit
#' @importFrom chk chk_number
#' @importFrom stats model.frame
#' @export
check_fit.bayesnecjointfit <- function(x, group = NULL, ndraws = 1000,
                                       seed = 10, ...) {
  chk_number(seed)
  d <- x$fit$data
  if (!is.null(group) && length(group) > 1L) {
    if (!is.atomic(group) || !is.null(dim(group)) ||
        length(group) != nrow(d) || anyNA(group)) {
      stop("`group` must have one non-missing value per row of the data the",
           " joint refit was fitted to (", nrow(d), " rows), in its order.",
           call. = FALSE)
    }
  } else if (!is.null(group) &&
             (length(group) != 1L || !is.numeric(group) || anyNA(group))) {
    stop("`group` must be a numeric bin count or one value per fitted row.",
         call. = FALSE)
  }
  pop <- attr(model.frame(x$bayesnecformula, data = d), "bnec_pop")
  y <- d[[pop[["y_var"]]]]
  predictor <- d[[pop[["x_var"]]]]
  # One simulation for the whole fit, split by level below, so every level is
  # read from the same draws.
  sims <- check_fit_draws(x$fit, x$bayesnecformula, ndraws, seed)
  cens <- check_fit_cens(x$fit)
  mixture <- is_hurdle_family(x$fit$family)
  rows <- joint_level_rows(x)
  out <- lapply(x$levels, function(l) {
    r <- rows[[l]]
    with_level_label(l, {
      grp <- check_fit_groups(predictor[r],
                              if (length(group) > 1L) group[r] else group)
      tab <- check_fit_stats(y[r], sims$yrep[, r, drop = FALSE], sims$mu[r],
                             grp, mixture,
                             cens = if (is.null(cens)) NULL else {
                               lapply(cens, `[`, r)
                             })
      ctrl <- check_fit_control_groups(predictor[r], grp,
                                       recorded_x_control(x))
      tab$control <- tab$group %in% ctrl
      attr(tab, "model") <- joint_level_model(x, l)
      allot_class(tab, c("checkfit", "data.frame"))
    })
  })
  names(out) <- x$levels
  structure(list(fits = out, group_var = x$group_var),
            class = "checkfit_group")
}

#' Draws of the curve parameters of one level of a joint refit
#'
#' @param object An object of class \code{\link{bayesnecjointfit}}.
#' @param level One level of \code{object$group_var}.
#' @param prefix The \pkg{brms} distributional parameter prefix, \code{""}
#' for the mean block.
#'
#' @return A draws-by-parameter \code{\link[base]{matrix}} with columns named
#' as \code{curve_par_names()} names them, as \code{curve_param_draws()}
#' returns for an ordinary fit.
#'
#' @noRd
joint_curve_draws <- function(object, level, prefix = "") {
  pars <- curve_par_names()
  cols <- lapply(pars, function(p) {
    joint_level_draws(object, paste0(prefix, p), level)
  })
  keep <- !vapply(cols, is.null, logical(1))
  out <- matrix(unlist(cols[keep]), ncol = sum(keep))
  colnames(out) <- pars[keep]
  out
}

#' @noRd
#' @method curve_params bayesnecjointfit
#' @importFrom chk chk_lgl
#' @export
curve_params.bayesnecjointfit <- function(object, summary = TRUE,
                                          xform = NULL, ...) {
  chk_lgl(summary)
  check_xform(xform)
  report_scales(object, "joint refit")
  out <- lapply(object$levels, function(l) {
    lvl <- joint_level_fit(object, l)
    tag <- object$level_spec$tags[[l]]
    with_prior_bound_label(paste("Level", l), {
      res <- one_fit_params(
        object$fit, models = dpar_models(lvl), wi = 1, summary = summary,
        xform = recorded_xform(lvl, xform),
        grid_span = function() fitted_grid_span(lvl),
        draws_for = function(prefix) joint_curve_draws(object, l, prefix),
        nlpar_for = function(prefix, par) paste0(prefix, par, tag)
      )
      if (summary) {
        report_param_prior_boundary(attr(res, "prior_bound_summary"))
      }
      res
    })
  })
  names(out) <- object$levels
  if (!summary) {
    return(out)
  }
  record <- bind_prior_records(lapply(out, attr, "prior_bound_summary"),
                               object$levels)
  out <- do.call(rbind, lapply(seq_along(out), function(i) {
    cbind(data.frame(level = object$levels[i], stringsAsFactors = FALSE),
          out[[i]])
  }))
  rownames(out) <- NULL
  attr(out, "prior_bound_summary") <- record
  out
}

#' @noRd
#' @method formula bayesnecjointfit
#' @export
formula.bayesnecjointfit <- function(x, ...) {
  out <- lapply(x$levels, function(l) {
    swap_crf_model(x$bayesnecformula, joint_level_model(x, l))
  })
  names(out) <- x$levels
  out
}

#' @noRd
#' @method model.frame bayesnecjointfit
#' @importFrom stats model.frame
#' @export
model.frame.bayesnecjointfit <- function(formula, ...) {
  d <- model.frame(formula$fit)
  rows <- joint_level_rows(formula)
  lapply(rows, function(r) d[r, , drop = FALSE])
}

#' @noRd
#' @method plot bayesnecjointfit
#' @importFrom graphics par title
#' @importFrom chk chk_lgl chk_character
#' @importFrom stats model.frame
#' @export
plot.bayesnecjointfit <- function(x, ..., CI = TRUE, add_nec = TRUE,
                                  position_legend = "topright",
                                  add_ec10 = FALSE, xform = NULL,
                                  lxform = identity, jitter_x = FALSE,
                                  jitter_y = FALSE, ylab = "Response",
                                  xlab = "Predictor", xticks = NA) {
  chk_lgl(CI)
  chk_lgl(add_nec)
  chk_lgl(add_ec10)
  check_xform(xform)
  if (!inherits(lxform, "function")) {
    stop("lxform must be a function.")
  }
  chk_lgl(jitter_x)
  chk_lgl(jitter_y)
  chk_character(ylab)
  chk_character(xlab)
  legend_pos <- check_position_legend(position_legend)
  family <- x$fit$family$family
  d <- x$fit$data
  bdat <- model.frame(x$bayesnecformula, data = d, run_par_checks = TRUE)
  pop <- attr(bdat, "bnec_pop")
  x_all <- d[[pop[["x_var"]]]]
  y_all <- plotted_response(d, pop, family)
  # As in plot.bayesnecfit(): on the recorded axis with no xform, draw what
  # the estimators return.
  recorded <- plot_reads_recorded(bdat, xform)
  est_xform <- if (recorded) NULL else identity
  axis_xform <- if (is.null(xform)) identity else xform
  rows <- joint_level_rows(x)
  n <- length(x$levels)
  old <- par(mfrow = c(ceiling(n / 2), min(2, n)))
  on.exit(par(old), add = TRUE)
  for (lev in x$levels) {
    curve <- joint_level_curve(x, lev)
    x_dat <- x_all[rows[[lev]]]
    x_vec <- curve$x
    if (!pop_var_is_transformed(bdat, "x_var")) {
      x_dat <- axis_xform(x_dat)
      x_vec <- axis_xform(x_vec)
    }
    nec <- c(NA, NA, NA)
    if (add_nec) {
      nec <- joint_level_ne(x, lev, est_xform)
      if (!recorded) {
        nec <- to_axis_scale(nec, bdat, x$bayesnecformula, curve$x,
                             axis_xform)
      }
    }
    ec10 <- c(NA, NA, NA)
    if (add_ec10) {
      ec10 <- plot_ecx(joint_level_fit(x, lev), family, list(), est_xform)
      if (!recorded) {
        ec10 <- to_axis_scale(ec10, bdat, x$bayesnecformula, curve$x,
                              axis_xform)
      }
    }
    nec_tag <- if (sampled_nec_is_no_effect(joint_level_model(x, lev))) {
      "NEC"
    } else {
      "NSEC"
    }
    draw_fit_panel(x_dat, y_all[rows[[lev]]], x_vec, curve, nec, ec10,
                   nec_tag, ..., CI = CI, add_nec = add_nec,
                   legend_pos = legend_pos, add_ec10 = add_ec10,
                   lxform = lxform, jitter_x = jitter_x, jitter_y = jitter_y,
                   ylab = ylab, xlab = xlab, xticks = xticks)
    title(main = paste0(x$group_var, " = ", lev))
  }
  invisible(x)
}

#' The ECx rows of a summary of a joint refit
#'
#' @param ... The dots of the summary method, passed to \code{\link{ecx}}.
#' @param .fit An object of class \code{\link{bayesnecjointfit}}.
#' @param .ecx_vals The ECx targets.
#'
#' @return A named \code{\link[base]{list}} of the tables \code{\link{ecx}}
#' returns, one per entry of \code{.ecx_vals}.
#'
#' @noRd
joint_summary_ecx_rows <- function(..., .fit, .ecx_vals) {
  # Dotted and after ..., so that an ecx_val meant for ecx() does not
  # partially match them.
  out <- lapply(.ecx_vals, function(v) ecx(.fit, ecx_val = v, ...))
  names(out) <- paste0("EC", .ecx_vals)
  out
}

#' @noRd
#' @method summary bayesnecjointfit
#' @importFrom brms bayes_R2
#' @importFrom chk chk_lgl chk_numeric chk_number
#' @export
summary.bayesnecjointfit <- function(object, ..., ecx = FALSE,
                                     ecx_vals = c(10, 50, 90),
                                     rhat_cutoff = 1.01,
                                     divergence_cutoff = 10) {
  chk_lgl(ecx)
  chk_numeric(ecx_vals)
  chk_number(rhat_cutoff)
  chk_number(divergence_cutoff)
  check_summary_ecx_posterior(list(...))
  ne_xform <- summary_ecx_xform(list(...))
  ecs <- NULL
  if (ecx) {
    ecs <- joint_summary_ecx_rows(..., .fit = object, .ecx_vals = ecx_vals)
  }
  ne_args <- list(object, no_effect = TRUE)
  if (!is.null(ne_xform)) {
    ne_args$xform <- ne_xform
  }
  # The table labels each row NEC or NSEC, so nec()'s message adds nothing.
  ne <- suppressMessages(do.call(nec, ne_args))
  d <- object$fit$data
  rows <- joint_level_rows(object)
  bayesr2 <- do.call(rbind, lapply(rows, function(r) {
    bayes_R2(object$fit, newdata = d[r, , drop = FALSE])
  }))
  rownames(bayesr2) <- object$levels
  fit <- object$fit
  divergences <- if (!is.null(fit$algorithm) &&
                     !identical(fit$algorithm, "sampling")) {
    NA_real_
  } else {
    count_divergent(fit)
  }
  models <- vapply(object$levels, joint_level_model, character(1),
                   object = object, USE.NAMES = FALSE)
  out <- list(
    brmssummary = cleaned_brms_summary(fit),
    models = stats::setNames(models, object$levels),
    group_var = object$group_var,
    family = object$family,
    disp_by_level = object$disp_by_level,
    ne = ne,
    ecs = ecs,
    bayesr2 = bayesr2,
    divergences = divergences,
    divergence_cutoff = divergence_cutoff,
    rhat_issues = lapply(rhat(object, rhat_cutoff = rhat_cutoff), "[[",
                         "failed"),
    rhat_cutoff = rhat_cutoff,
    bnec_version = bnec_version(object)
  )
  allot_class(out, "jointsummary")
}

#' Print one row per level of an estimator table
#'
#' @param tab A table as \code{\link{nec}} or \code{\link{ecx}} returns for a
#' \code{\link{bayesnecjointfit}}.
#' @param labels The row labels.
#'
#' @return \code{TRUE} where an entry is a bound of the prediction range
#' rather than a quantile, invisibly.
#'
#' @noRd
print_level_table <- function(tab, labels) {
  bounds <- grep("^bound_", names(tab), value = TRUE)
  values <- sub("^bound_", "", bounds)
  mat <- as.matrix(tab[, values, drop = FALSE])
  rownames(mat) <- labels
  marks <- lapply(seq_len(nrow(tab)), function(i) {
    list(bound = unname(unlist(tab[i, bounds])))
  })
  attr(mat, "censored_summary") <- marks
  print_mat(mat)
  invisible(any(nzchar(unlist(tab[, bounds]))))
}

#' @noRd
#' @method print jointsummary
#' @export
print.jointsummary <- function(x, ...) {
  cat("Object of class bayesnecjointfit\n\n")
  if (length(unique(x$models)) == 1) {
    cat("  equation          :", unname(x$models[[1]]), "\n")
  } else {
    cat("  equations         :",
        paste0(names(x$models), " = ", x$models, collapse = ", "), "\n")
  }
  cat("  grouping variable :", x$group_var, "\n")
  cat("  family            :", x$family$family,
      paste0("(link = ", x$family$link, ")"), "\n")
  cat("  dispersion        :",
      if (isTRUE(x$disp_by_level)) "per level" else "shared", "\n\n")
  print(x$brmssummary)
  cat("\nDivergent transitions after warmup: ", x$divergences, "\n", sep = "")
  cat("\n\nNo-effect toxicity estimates\n")
  bounded <- print_level_table(x$ne, paste0(x$ne$level, ": ", x$ne$model,
                                            " (", x$ne$ne_type, ")"))
  if (any(x$ne$ne_type == "NSEC")) {
    cat("NB: An NEC is the sampled nec parameter of a monotone threshold\n",
        "    equation. An NSEC is read off the fitted curve of a smooth or\n",
        "    hormesis threshold equation (Fisher et al. 2023).\n", sep = "")
  }
  if (!is.null(x$ecs)) {
    cat("\nECx estimates\n")
    for (nm in names(x$ecs)) {
      tab <- x$ecs[[nm]]
      bounded <- print_level_table(tab, paste0(tab$level, " ", nm)) ||
        bounded
    }
  }
  if (bounded) {
    cat("NB: An entry marked >= or <= is the end of that level's prediction\n",
        "    range, not a quantile.\n", sep = "")
  }
  cat("\nBayesian R2 estimates, from each level's own rows:\n")
  print_mat(x$bayesr2)
  cat("\n")
  print_version(x$bnec_version)
  rhat_bad <- names(x$rhat_issues)[which(unlist(x$rhat_issues))]
  div_bad <- isTRUE(x$divergences > x$divergence_cutoff)
  if (length(rhat_bad) > 0 || div_bad) {
    msg <- character(0)
    if (length(rhat_bad) > 0) {
      msg <- c(msg, paste0("Rhat > ", x$rhat_cutoff, " (no convergence) at ",
                           paste0("\"", rhat_bad, "\"", collapse = ", "),
                           "; see rhat()."))
    }
    if (div_bad) {
      msg <- c(msg, paste0(x$divergences, " divergent transitions after",
                           " warmup, more than ", x$divergence_cutoff, "."))
    }
    warning(paste(msg, collapse = "\n"), "\nConsider refitting with more",
            " iterations or a larger adapt_delta, passed to update() as",
            " control = list(adapt_delta = ...).", call. = FALSE)
  }
  invisible(x)
}

#' @noRd
#' @method update bayesnecjointfit
#' @importFrom stats update
#' @export
update.bayesnecjointfit <- function(object, newdata = NULL, recompile = NULL,
                                    ...) {
  dots <- list(...)
  if ("family" %in% names(dots)) {
    stop("update() on a bayesnecjointfit keeps the family it was fitted",
         " with, because its priors were built for that family. To fit",
         " another, fit the levels with bnec_group() under that family and",
         " refit with bnec_joint().", call. = FALSE)
  }
  if ("formula." %in% names(dots)) {
    stop("update() on a bayesnecjointfit keeps its formula. To fit another,",
         " pass it to bnec_joint() as `formula`.", call. = FALSE)
  }
  stored <- intersect(names(dots), c("x_range", "resolution", "sig_val",
                                     "loo_controls", "force_fit"))
  if (length(stored) > 0) {
    stop(paste0("`", stored, "`", collapse = ", "), " cannot be passed to",
         " update() on a bayesnecjointfit, which stores no estimate to",
         " rebuild and no model set to weight. ecx(), nsec() and nec() take",
         " x_range, resolution and sig_val when called.", call. = FALSE)
  }
  declared <- intersect(names(dots), c("prior_type", "asymptote_observed",
                                       "predictor_scale"))
  if (length(declared) > 0) {
    stop(paste0("`", declared, "`", collapse = ", "), " builds the default",
         " priors, which update() reuses. Pass it to bnec_joint() on the",
         " bnec_group() fit to refit with priors built from it.",
         call. = FALSE)
  }
  if (!is.null(newdata)) {
    newdata <- joint_newdata(object, newdata)
    absent <- setdiff(object$levels,
                      as.character(newdata[[object$group_var]]))
    if (length(absent) > 0) {
      stop("`newdata` has no row at ",
           paste0("\"", absent, "\"", collapse = ", "), ". The joint refit",
           " estimates coefficients for every level, and a level without",
           " data would be estimated from its prior alone.", call. = FALSE)
    }
    # Check the response against the family as bnec() does, and correct a
    # value at an excluded boundary before brms sees it.
    newdata <- check_update_data(
      list(list(fit = object$fit, bayesnecformula = object$bayesnecformula)),
      newdata, on_fit = FALSE
    )$data
  }
  fit_old <- object$fit
  # Build a call rather than forward `...`, and keep newdata a symbol, so that
  # brms records the name of the data as update.bnecfit() leaves it.
  upd_call <- as.call(c(quote(update), quote(fit_old),
                        list(formula. = NULL, newdata = quote(newdata),
                             recompile = quote(recompile)), dots))
  object$fit <- eval(upd_call)
  if (!is.null(newdata)) {
    object$data <- newdata
  }
  attr(object, "bnec_version") <- joint_version_table(object,
                                                      version_record())
  object
}

#' @noRd
#' @method amend bayesnecjointfit
#' @export
amend.bayesnecjointfit <- function(object, drop, add, loo_controls,
                                   x_range = NULL, resolution = NULL,
                                   sig_val = NULL, priors, prior_type = NULL,
                                   asymptote_observed = NULL, timeout = Inf,
                                   predictor_scale = NULL) {
  refuse_joint_method(object, "amend()", paste(
    "Each of its levels has one equation and it is not a model set, so",
    "there is no equation to add or drop and no model weight to recompute,",
    "and it stores no estimate for x_range, resolution or sig_val to",
    "rebuild: ecx(), nsec() and nec() take those when called. To change the",
    "equations, amend() the",
    "bnec_group() fit and refit it with bnec_joint(); to refit with other",
    "sampler settings or data, use update()."
  ))
}
