#' Select and apply a method to grouped level fits
#'
#' @param object A \code{bayesnecgroupfit}.
#' @param level One fitted level, or \code{NULL} for every level.
#' @param fun A function of a level fit and its level name.
#' @return One method result for a selected level, or a named list by level.
#' @noRd
group_method <- function(object, level, fun) {
  levels <- object$levels
  if (!identical(levels, names(object$fits))) {
    stop("The levels of this bayesnecgroupfit do not match its fits.",
         call. = FALSE)
  }
  if (!is.null(level)) {
    if (!is.character(level) || length(level) != 1L || is.na(level) ||
        !level %in% levels) {
      stop("The level of a bayesnecgroupfit must name one of ",
           paste(levels, collapse = ", "), ".", call. = FALSE)
    }
    return(group_method_one(object, level, fun))
  }
  out <- lapply(levels, function(lev) group_method_one(object, lev, fun))
  names(out) <- levels
  out
}

#' @noRd
group_method_one <- function(object, level, fun) {
  tryCatch(
    with_level_label(level, fun(object$fits[[level]], level)),
    error = function(e) {
      stop("Level ", level, ": ", conditionMessage(e), call. = FALSE)
    }
  )
}

#' Bind level tables with matching columns and classes
#'
#' @param tables A named list of data frames.
#' @return A data frame with a leading level column when schemas match;
#' otherwise \code{tables} unchanged.
#' @noRd
group_method_table <- function(tables) {
  if (!all(vapply(tables, is.data.frame, logical(1)))) return(tables)
  columns <- lapply(tables, names)
  if (!all(vapply(columns, identical, logical(1), columns[[1]]))) {
    return(tables)
  }
  schema <- lapply(tables, function(tab) {
    vapply(tab, function(column) paste(class(column), collapse = "/"),
           character(1))
  })
  if (!all(vapply(schema, identical, logical(1), schema[[1]]))) {
    return(tables)
  }
  out <- do.call(rbind, Map(function(tab, lev) {
    cbind(data.frame(level = rep(lev, nrow(tab)),
                     stringsAsFactors = FALSE), tab)
  }, tables, names(tables)))
  rownames(out) <- NULL
  out
}

#' Call a model-specific method on one level fit
#'
#' @param fit A \code{bayesnecfit} or \code{bayesmanecfit}.
#' @param model One equation, or \code{NULL} for every equation of a set.
#' @param fun The method to call.
#' @param ... Other method arguments.
#' @return One result for a single fit or named model, or a named list
#' of results by equation for a model set without \code{model}.
#' @noRd
group_model_method <- function(fit, model, fun, ...) {
  if (!is.null(model) &&
      (!is.character(model) || length(model) != 1L || is.na(model))) {
    stop("model must name one equation.", call. = FALSE)
  }
  if (inherits(fit, "bayesmanecfit")) {
    models <- names(fit$mod_fits)
    if (!is.null(model) && !model %in% models) {
      stop("The model must name one of ", paste(models, collapse = ", "),
           ".", call. = FALSE)
    }
    if (is.null(model)) {
      out <- lapply(models, function(m) fun(fit, model = m, ...))
      names(out) <- models
      return(out)
    }
    return(fun(fit, model = model, ...))
  }
  if (!is.null(model) && !identical(model, fit$model)) {
    stop("This bayesnecfit holds ", fit$model, ", not ", model, ".",
         call. = FALSE)
  }
  fun(fit, ...)
}

#' Match new data to one grouped level
#'
#' @param newdata A data frame or \code{NULL}.
#' @param level The selected level.
#' @param levels All fitted levels.
#' @return The rows for \code{level}, without the added level column, or
#' \code{newdata} unchanged when it has no level column.
#' @noRd
group_prediction_data <- function(newdata, level, levels) {
  if (is.null(newdata) || !is.data.frame(newdata) ||
      !"level" %in% names(newdata)) {
    return(newdata)
  }
  selected <- as.character(newdata$level)
  if (anyNA(selected) || any(!selected %in% levels)) {
    stop("The level column of newdata must name fitted levels: ",
         paste(levels, collapse = ", "), ".", call. = FALSE)
  }
  rows <- which(selected == level)
  if (!length(rows)) {
    stop("newdata has no row for level ", level, ".", call. = FALSE)
  }
  out <- newdata[rows, setdiff(names(newdata), "level"), drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Predictor grids for a grouped fit
#'
#' @param x A \code{bayesnecgroupfit}.
#' @param resolution Number of grid rows per level.
#' @param x_range Optional predictor range.
#' @param level One level, or \code{NULL} for all levels.
#' @param ... Unused; other arguments are refused.
#' @return A data frame for one level, or a data frame with a leading
#' \code{level} column when all grids share a schema; otherwise a named list.
#' @method bnec_newdata bayesnecgroupfit
#' @export
bnec_newdata.bayesnecgroupfit <- function(x, resolution = 100, x_range = NA,
                                          level = NULL, ...) {
  check_unused_dots(list(...), "bnec_newdata")
  out <- group_method(x, level, function(fit, lev) {
    bnec_newdata(fit, resolution = resolution, x_range = x_range)
  })
  if (is.null(level)) group_method_table(out) else out
}

#' Fitted summaries or draws for each grouped level
#'
#' @param object A \code{bayesnecgroupfit}.
#' @param newdata Optional predictor data.
#' @param ... Passed to each level fit's \code{fitted()} method.
#' @param level One level, or \code{NULL} for all levels.
#' @return A matrix for one level, or a named list of matrices by level.
#' @method fitted bayesnecgroupfit
#' @importFrom stats fitted
#' @export
fitted.bayesnecgroupfit <- function(object, newdata = NULL, ...,
                                   level = NULL) {
  group_method(object, level, function(fit, lev) {
    fitted(fit, newdata = group_prediction_data(newdata, lev, object$levels),
           ...)
  })
}

#' Predictions for each grouped level
#'
#' @inheritParams fitted.bayesnecgroupfit
#' @return A matrix for one level, or a named list of matrices by level.
#' @method predict bayesnecgroupfit
#' @importFrom stats predict
#' @export
predict.bayesnecgroupfit <- function(object, newdata = NULL, ...,
                                    level = NULL) {
  group_method(object, level, function(fit, lev) {
    predict(fit, newdata = group_prediction_data(newdata, lev, object$levels),
            ...)
  })
}

#' Expected posterior draws for each grouped level
#'
#' @inheritParams fitted.bayesnecgroupfit
#' @return A draws-by-observation matrix for one level, or a named list
#' of such matrices by level.
#' @method posterior_epred bayesnecgroupfit
#' @importFrom brms posterior_epred
#' @export
posterior_epred.bayesnecgroupfit <- function(object, newdata = NULL, ...,
                                             level = NULL) {
  group_method(object, level, function(fit, lev) {
    posterior_epred(fit,
                    newdata = group_prediction_data(newdata, lev,
                                                    object$levels), ...)
  })
}

#' Posterior predictive draws for each grouped level
#'
#' @inheritParams fitted.bayesnecgroupfit
#' @return A draws-by-observation matrix for one level, or a named list
#' of such matrices by level.
#' @method posterior_predict bayesnecgroupfit
#' @importFrom brms posterior_predict
#' @export
posterior_predict.bayesnecgroupfit <- function(object, newdata = NULL, ...,
                                               level = NULL) {
  group_method(object, level, function(fit, lev) {
    posterior_predict(fit,
                      newdata = group_prediction_data(newdata, lev,
                                                      object$levels), ...)
  })
}

#' Posterior predictive checks for a grouped fit
#'
#' @param object A \code{bayesnecgroupfit}.
#' @param ... Passed to the level method.
#' @param level One level, or \code{NULL} for all levels.
#' @return A plot or named list of plots for one level, or a named list
#' of those results by level.
#' @method pp_check bayesnecgroupfit
#' @importFrom brms pp_check
#' @export
pp_check.bayesnecgroupfit <- function(object, ..., level = NULL) {
  group_method(object, level, function(fit, lev) pp_check(fit, ...))
}

#' Rhat diagnostics for a grouped fit
#'
#' @param x A \code{bayesnecgroupfit}.
#' @param ... Passed to the level method.
#' @param level One level, or \code{NULL} for all levels.
#' @return A diagnostic list for one level, or a named list by level.
#' @method rhat bayesnecgroupfit
#' @importFrom brms rhat
#' @export
rhat.bayesnecgroupfit <- function(x, ..., level = NULL) {
  group_method(x, level, function(fit, lev) rhat(fit, ...))
}

#' Chain plots for a grouped fit
#'
#' @inheritParams rhat.bayesnecgroupfit
#' @return Invisibly, one plotting result for a selected level, or a named
#' list of plotting results by level.
#' @method check_chains bayesnecgroupfit
#' @export
check_chains.bayesnecgroupfit <- function(x, ..., level = NULL) {
  invisible(group_method(x, level, function(fit, lev) {
    check_chains(fit, ...)
  }))
}

#' Prior and posterior plots for a grouped fit
#'
#' @param object A \code{bayesnecgroupfit}.
#' @param filename Optional PDF name for model-set levels.
#' @param ask Whether to prompt between pages.
#' @param ... Unused.
#' @param level One level, or \code{NULL} for all levels.
#' @return A plot or named list of model plots for one level, or a named
#' list of those results by level.
#' @method check_priors bayesnecgroupfit
#' @export
check_priors.bayesnecgroupfit <- function(object, filename = NA, ask = TRUE,
                                         ..., level = NULL) {
  if (...length() > 0) {
    stop("check_priors() on a bayesnecgroupfit uses only filename, ask and",
         " level.", call. = FALSE)
  }
  group_method(object, level, function(fit, lev) {
    file <- if (is.na(filename) || !is.null(level)) filename else
      paste0(filename, "-", lev)
    check_priors(fit, filename = file, ask = ask)
  })
}

#' Brms fits for a grouped fit
#'
#' @param object A \code{bayesnecgroupfit}.
#' @param model One equation of each model-set level, or \code{NULL}.
#' @param ... Passed to the level method.
#' @param level One level, or \code{NULL} for all levels.
#' @return A \code{brmsfit} or named list by equation for one level, or a
#' named list of those results by level.
#' @method pull_brmsfit bayesnecgroupfit
#' @export
pull_brmsfit.bayesnecgroupfit <- function(object, model = NULL, ...,
                                         level = NULL) {
  group_method(object, level, function(fit, lev) {
    group_model_method(fit, model, pull_brmsfit, ...)
  })
}

#' Priors used at each grouped level
#'
#' @param object A \code{bayesnecgroupfit}.
#' @param ... Unused.
#' @param level One level, or \code{NULL} for all levels.
#' @return A prior object or named list by equation for one level, or a
#' named list of those results by level.
#' @method get_priors bayesnecgroupfit
#' @export
get_priors.bayesnecgroupfit <- function(object, ..., level = NULL) {
  refuse_fit_dots(...)
  group_method(object, level, function(fit, lev) get_priors(fit))
}

#' Highest-weighted fit at each grouped level
#'
#' @inheritParams get_priors.bayesnecgroupfit
#' @return A \code{bayesnecfit} for one level, or a named list of
#' \code{bayesnecfit} objects by level.
#' @method pull_best bayesnecgroupfit
#' @export
pull_best.bayesnecgroupfit <- function(object, ..., level = NULL) {
  if (...length() > 0) {
    stop("pull_best() on a bayesnecgroupfit uses only level.",
         call. = FALSE)
  }
  group_method(object, level, function(fit, lev) pull_best(fit))
}

#' Formulas fitted at each grouped level
#'
#' @param x A \code{bayesnecgroupfit}.
#' @param ... Passed to the level method.
#' @param level One level, or \code{NULL} for all levels.
#' @param model One equation of each model-set level, or \code{NULL}.
#' @return A \code{bayesnecformula} or named list by equation for one
#' level, or a named list of those results by level.
#' @method formula bayesnecgroupfit
#' @export
formula.bayesnecgroupfit <- function(x, ..., level = NULL, model = NULL) {
  group_method(x, level, function(fit, lev) {
    group_model_method(fit, model, stats::formula, ...)
  })
}

#' Model frames fitted at each grouped level
#'
#' @param formula A \code{bayesnecgroupfit}.
#' @param ... Passed to the level method.
#' @param level One level, or \code{NULL} for all levels.
#' @param model One equation of each model-set level, or \code{NULL}.
#' @return A data frame or named list by equation for one level. Across
#' levels, a data frame with a leading level column is returned when all
#' frames have one schema; otherwise a named list by level is returned.
#' @method model.frame bayesnecgroupfit
#' @importFrom stats model.frame
#' @export
model.frame.bayesnecgroupfit <- function(formula, ..., level = NULL,
                                         model = NULL) {
  out <- group_method(formula, level, function(fit, lev) {
    group_model_method(fit, model, stats::model.frame, ...)
  })
  if (is.null(level)) group_method_table(out) else out
}

#' Refit every grouped level
#'
#' @param object A \code{bayesnecgroupfit}.
#' @param newdata New data with the fitted grouping column and every level.
#' @param recompile Passed to each level's update method.
#' @param ... Further update settings applied to every level.
#' @return A \code{bayesnecgroupfit} with every level refitted.
#' @method update bayesnecgroupfit
#' @importFrom stats update
#' @export
update.bayesnecgroupfit <- function(object, newdata = NULL,
                                    recompile = NULL, ...) {
  if ("level" %in% ...names()) {
    stop("update() on a bayesnecgroupfit refits every level. To refit one",
         " level, call update() on its fit in the fits list.", call. = FALSE)
  }
  group_method(object, NULL, function(fit, lev) NULL)
  if (!is.null(newdata)) {
    if (!is.data.frame(newdata) || !object$group_var %in% names(newdata)) {
      stop("newdata for a bayesnecgroupfit must be a data frame with the",
           " grouping column ", object$group_var, ".", call. = FALSE)
    }
    observed <- as.character(newdata[[object$group_var]])
    if (anyNA(observed) || !setequal(unique(observed), object$levels)) {
      stop("newdata for a bayesnecgroupfit must contain every fitted level",
           " of ", object$group_var, " and no other level: ",
           paste(object$levels, collapse = ", "), ".", call. = FALSE)
    }
  }
  fits <- group_method(object, NULL, function(fit, lev) {
    rows <- if (is.null(newdata)) NULL else
      newdata[as.character(newdata[[object$group_var]]) == lev, ,
              drop = FALSE]
    update(fit, newdata = rows, recompile = recompile, ...)
  })
  out <- object
  out$fits <- fits
  if (!is.null(newdata)) {
    out$data <- newdata
    out$n <- as.integer(table(factor(as.character(newdata[[object$group_var]]),
                                     levels = object$levels)))
  }
  first <- fits[[1]]
  out$family <- if (inherits(first, "bayesmanecfit")) {
    first$mod_fits[[1]]$fit$family
  } else {
    first$fit$family
  }
  methods <- Filter(Negate(is.null), lapply(fits, fit_weights_method))
  if (length(methods)) out$weights_method <- methods[[1]]
  out$level_seeds <- NULL
  out
}

#' Check that grouped fits describe the same observations
#'
#' @param objects Grouped fits to combine.
#' @return Invisibly \code{TRUE}, or an error naming the mismatch.
#' @noRd
group_combination_check <- function(objects) {
  ordinary <- vapply(objects, inherits, logical(1), "bayesnecgroupfit")
  if (!all(ordinary)) {
    bad <- class(objects[[which(!ordinary)[1]]])[1]
    stop("A bayesnecgroupfit can combine only with another",
         " bayesnecgroupfit; the other object has class ", bad, ".",
         call. = FALSE)
  }
  first <- objects[[1]]
  for (other in objects[-1]) {
    if (!identical(first$group_var, other$group_var)) {
      stop("Grouped fits use different grouping variables. Fit both with",
           " the same group_var before combining them.", call. = FALSE)
    }
    if (!identical(first$levels, other$levels)) {
      stop("Grouped fits have different levels or level order. Fit both",
           " with the same levels before combining them.", call. = FALSE)
    }
    if (is.null(first$data) || is.null(other$data) ||
        !identical(sort(names(first$data)), sort(names(other$data))) ||
        !identical(first$data[sort(names(first$data))],
                   other$data[sort(names(other$data))])) {
      stop("Grouped fits use different data. Fit both on the same rows",
           " before combining them.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Combine grouped fits level by level
#'
#' @param x A \code{bayesnecgroupfit}.
#' @param ... Further grouped fits on the same data and levels.
#' @return A \code{bayesnecgroupfit} whose fit at each level is the
#' \code{bayesmanecfit} model set formed from that level's input fits.
#' @method c bayesnecgroupfit
#' @export
c.bayesnecgroupfit <- function(x, ...) {
  objects <- c(list(x), list(...))
  group_combination_check(objects)
  if (length(objects) == 1L) return(x)
  out <- x
  out$fits <- lapply(x$levels, function(lev) {
    do.call(c, lapply(objects, function(z) z$fits[[lev]]))
  })
  names(out$fits) <- x$levels
  methods <- Filter(Negate(is.null), lapply(out$fits, fit_weights_method))
  if (length(methods)) out$weights_method <- methods[[1]]
  out$level_seeds <- NULL
  out
}
