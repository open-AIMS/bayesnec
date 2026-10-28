#' Extracts a range of statistics from a \code{\link{prebayesnecfit}} object.
#'
#' @inheritParams bnec
#'
#' @param object An object of class \code{\link{prebayesnecfit}}.
#' @param loo_controls A named \code{\link[base]{list}} whose "fitting"
#' element holds arguments to be passed on to \code{\link[brms]{loo}}. A
#' single model is not weighted, so a "weights" element is accepted and not
#' read. See \code{\link{bnec}}.
#' @param ... Further arguments to internal function.
#'
#' @return A \code{\link[base]{list}} of model statistical output derived from
#' the input model object.
#'
#' @importFrom brms posterior_epred as_draws_df
#' @importFrom stats quantile fitted residuals terms
#' @importFrom chk chk_numeric
#'
#' @export
expand_nec <- function(object, formula, x_range = NA, resolution = 1000,
                       sig_val = 0.01, loo_controls, ...) {
  check_args_newdata(resolution, x_range)
  chk_numeric(sig_val)
  fam_tag <- object$fit$family$family
  if (missing(loo_controls)) {
    loo_controls <- list(fitting = list(), weights = list())
  } else {
    loo_controls <- validate_loo_controls(loo_controls, fam_tag)
  }
  object <- add_criteria(object, loo_controls$fitting, ...)
  fit <- object$fit
  extract_params <- extract_par_order()
  extracted_params <- lapply(extract_params, extract_pars, fit)
  names(extracted_params) <- gsub("^nec$", "ne", extract_params)
  grid <- prediction_grid(fit, formula, x_range = x_range,
                          resolution = resolution)
  new_dat <- grid$newdata
  x_seq <- grid$x_seq
  # Compute the grid posterior only where it is used, and never store it: it
  # would be by far the largest part of a fitted object.
  pred_posterior <- NULL
  get_pred_posterior <- function() {
    if (is.null(pred_posterior)) {
      pred_posterior <<- factorised_count_epred(
        fit, formula, newdata = new_dat, re_formula = NA
      )
    }
    pred_posterior
  }
  if (is_factorised_count_formula(formula, fit$family)) {
    y_pred_m <- summarise_epred(get_pred_posterior(), robust = TRUE)
  } else {
    y_pred_m <- fitted(fit, newdata = new_dat, robust = TRUE, re_formula = NA,
                       scale = "response")
  }
  pred_data <- data.frame(x = x_seq, Estimate = y_pred_m[, "Estimate"],
                          Q2.5 = y_pred_m[, "Q2.5"],
                          Q97.5 = y_pred_m[, "Q97.5"])
  if (is.na(extracted_params$ne["Estimate"])) {
    mod_class <- "ecx"
  } else {
    mod_class <- "nec"
  }
  # The bounds on both scales. A curve is searched on the recorded grid, so its
  # censoring record is built there and remapped with xform_censoring(), which
  # swaps the ends for a decreasing crf(). A sampled b_nec_Intercept is already
  # on the fitted scale. Non-finite grid points are dropped: an x_range
  # reaching 0 under crf(log(x)) gives -Inf.
  grid_recorded <- pred_data$x[is.finite(pred_data$x)]
  grid_fitted <- sub_x_transformation(pred_data$x, formula)
  grid_fitted <- grid_fitted[is.finite(grid_fitted)]
  ne_upper <- max(grid_fitted)
  ne_lower <- min(grid_fitted)
  to_fitted_scale <- function(value) sub_x_transformation(value, formula)
  # NSEC read off a fitted curve, on the predictor scale the user supplied it
  # on. Used for smooth (ecx-type) models, which carry no threshold parameter,
  # and for any two-block fit where at least one block is smooth.
  nsec_off_curve <- function(post) {
    reference <- quantile(post[, 1], sig_val)
    # The reference and the start of the search are the same grid point, so a
    # draw at or below the reference there takes that point as its NSEC. The
    # start is the first grid point, not the lowest observed predictor as in
    # nsec.bayesnecfit(); the two differ only where bnec() was given an x_range.
    out <- nsec_from_posterior(post, reference, pred_data$x, pred_data$x[1],
                               post[, 1])
    below <- attr(out, "below_range")
    above <- is.na(out) & !below
    cens <- xform_censoring(
      censoring_record(max(grid_recorded), min(grid_recorded), above, below),
      to_fitted_scale
    )
    if (sum(above) > 0) {
      # Name the equation, since bnec() calls this once per model. Give the
      # bound as a value: the top of the grid exceeds the highest concentration
      # tested wherever an x_range extends beyond the data.
      message("The fitted ", object$model, " curve does not fall to the ",
              "control's ", sig_val, " quantile within the predictor range ",
              "for ", sum(above), " of ", length(out), " draws. The NSEC ",
              "summary is censored at ",
              signif(censored_end(cens, "above"), 3),
              ": those draws keep their rank in it and are given no value.")
    }
    if (sum(below) > 0) {
      message("The fitted ", object$model, " curve is already below the ",
              "control's ", sig_val, " quantile where the predictor range ",
              "begins, for ", sum(below), " of ", length(out), " draws. The ",
              "NSEC summary is censored at ",
              signif(censored_end(cens, "below"), 3), ".")
    }
    out <- sub_x_transformation(out, formula)
    # The two attributes nsec_from_posterior() leaves for the reports above are
    # dropped, and the record the summaries read replaces them.
    attr(out, "n_below_range") <- NULL
    attr(out, "below_range") <- NULL
    attr(out, "x_searched_from") <- NULL
    attr(out, "censored") <- cens
    out
  }
  # Memoised: a smooth block on a hurdle fit reaches this twice on the same
  # posterior, and crossing_x() runs a root search per draw.
  ne_off_curve <- NULL
  get_ne_off_curve <- function() {
    if (is.null(ne_off_curve)) {
      ne_off_curve <<- nsec_off_curve(get_pred_posterior())
    }
    ne_off_curve
  }
  if (mod_class == "ecx") {
    ne_posterior <- get_ne_off_curve()
    extracted_params$ne <- estimates_summary(ne_posterior)
  } else {
    ne_posterior <- as_draws_df(fit)[["b_nec_Intercept"]]
    attr(ne_posterior, "censored") <- censoring_record(
      ne_upper, ne_lower, ne_posterior >= ne_upper, ne_posterior <= ne_lower
    )
    # Replace the summary only where a draw is beyond the grid. Otherwise keep
    # the one extract_pars() read off fixef(robust = TRUE), which is the number
    # archived analyses report.
    if (has_censoring(attr(ne_posterior, "censored"))) {
      extracted_params$ne <- estimates_summary(ne_posterior)
      report_ne_censoring(attr(extracted_params$ne, "censored_summary"),
                          object$model, "NEC")
    }
  }
  pred_vals <- list(data = pred_data)
  # A hurdle fit has a second block. Keep its threshold beside the combined
  # one, and report the combined threshold: posterior_epred() returns
  # mu * (1 - hu), so pred_vals describes the combined endpoint.
  hurdle_parts <- NULL
  if (is_hurdle_family(fit$family)) {
    hu_dpar <- hurdle_dpar(fit$family)
    hu_params <- lapply(extract_params, extract_pars, fit, prefix = hu_dpar)
    names(hu_params) <- gsub("^nec$", "ne", extract_params)
    hu_ne_posterior <- as_draws_df(fit)[[paste0("b_", hu_dpar,
                                                "nec_Intercept")]]
    # The two blocks may carry different equations (see the model_survival
    # argument of bnec), so each has its own class and the combined threshold
    # is derived differently depending on the pair.
    hu_class <- if (is.null(hu_ne_posterior)) "ecx" else "nec"
    if (mod_class == "nec" && hu_class == "nec") {
      # Below both thresholds mu sits at top and (1 - hu) at its control value,
      # so the product is flat; it leaves that plateau at whichever threshold
      # binds first. Exact for threshold models on both blocks.
      combined_ne <- pmin(ne_posterior, hu_ne_posterior)
      # Build the record from the minimum itself: it is above the grid only
      # where both draws are, and below it where either is.
      attr(combined_ne, "censored") <- censoring_record(
        ne_upper, ne_lower, combined_ne >= ne_upper, combined_ne <= ne_lower
      )
      ne_lab <- "NEC"
    } else {
      # With a smooth block there is no threshold to take the minimum of, so
      # interpolate off the combined curve. With one smooth block this is an
      # N(S)EC (Fisher et al. 2023): a threshold on one process and a
      # significant-effect point on the other.
      combined_ne <- get_ne_off_curve()
      ne_lab <- if (mod_class == "ecx" && hu_class == "ecx") {
        "NSEC"
      } else {
        "N(S)EC"
      }
    }
    hurdle_parts <- list(mu_ne_posterior = ne_posterior,
                         hu_ne_posterior = hu_ne_posterior,
                         hu_params = hu_params, hu_ne_type = hu_class,
                         ne_type = ne_lab)
    ne_posterior <- combined_ne
    extracted_params$ne <- estimates_summary(ne_posterior)
    if (identical(ne_lab, "NEC")) {
      # Only the two-threshold branch reports here. The other reaches its
      # combination through nsec_off_curve(), which has already said the same
      # thing about the same draws.
      report_ne_censoring(attr(extracted_params$ne, "censored_summary"),
                          object$model, ne_lab)
    }
  }
  od <- dispersion(object, summary = TRUE)
  if (length(od) == 0) {
    od <- c(Estimate = NA, Q2.5 = NA, Q97.5 = NA, `P(>1)` = NA)
  }
  if (is_factorised_count_formula(formula, fit$family)) {
    observed_epred <- factorised_count_epred(fit, formula, re_formula = NA)
    predicted_y <- summarise_epred(observed_epred, robust = TRUE)
    y_var <- hurdle_response_var(formula)
    residuals <- fit$data[[y_var]] - colMeans(observed_epred)
  } else {
    predicted_y <- fitted(fit, robust = TRUE, re_formula = NA,
                          scale = "response")
    residuals <- residuals(fit, method = "pp_expect")[, "Estimate"]
  }
  ne_type <- ifelse(mod_class == "nec", "NEC", "NSEC")
  if (!is.null(hurdle_parts)) {
    # A two-block fit reports the combined endpoint, whose type depends on both
    # blocks rather than on the response block alone.
    ne_type <- hurdle_parts$ne_type
  }
  out <- c(object, list(pred_vals = pred_vals), extracted_params,
           list(dispersion = od, predicted_y = predicted_y,
                residuals = residuals, ne_posterior = ne_posterior,
                ne_type = ne_type))
  # Only hurdle fits get the extra element, so that names() of every other
  # bayesnecfit is unchanged.
  if (!is.null(hurdle_parts)) {
    out <- c(out, list(hurdle = hurdle_parts))
  }
  out
}

#' The curve parameters, in the order they are appended to a bayesnecfit
#'
#' The same set as \code{curve_par_names()} in a different order. Keep the
#' order: code indexing a fit by position depends on it.
#' \code{test-curve_params.R} checks that the two hold the same parameters.
#'
#' @return A \code{\link[base]{character}} vector.
#' @noRd
extract_par_order <- function() {
  c("top", "beta", "nec", "f", "bot", "d", "slope", "ec50")
}

#' The grid predictions are made over
#'
#' The single definition of the prediction grid, used by
#' \code{\link{expand_nec}}, \code{posterior_on_grid} and
#' \code{\link{bnec_newdata}}. Takes a \code{\link[brms]{brmsfit}} and a
#' formula because inside \code{\link{expand_manec}} the object has no class
#' yet.
#'
#' Every prediction is made with \code{re_formula = NA}, which drops
#' group-level terms. A joint refit (\code{\link{bnec_joint}} on a
#' \code{\link{bayesnecgroupfit}}) puts the factor in the population-level part
#' of the curve, so the grid must then name a level through \code{level_spec}.
#'
#' @param fit An object of class \code{\link[brms]{brmsfit}}.
#' @param formula An object of class \code{\link{bayesnecformula}}.
#' @param level_spec \code{NULL}, or a \code{\link[base]{list}} with
#' \code{group_var}, the factor column of the level term; \code{levels}, every
#' level in coefficient order; and optionally \code{predict_levels}, the levels
#' to build rows for. The grid then has \code{resolution} rows per predicted
#' level, the level varying slowest. The factor always holds every level,
#' because \pkg{brms} builds the design matrix against the fitted levels.
#'
#' @return A \code{\link[base]{list}} of \code{newdata}, the
#' \code{\link[base]{data.frame}} to predict over; \code{x_seq}, the unique
#' predictor values it spans, which under \code{level_spec} is repeated once
#' per predicted level in \code{newdata}; and \code{x_var}, the name of the
#' predictor.
#'
#' @importFrom stats model.frame
#'
#' @noRd
prediction_grid <- function(fit, formula, x_range = NA, resolution = 1000,
                            level_spec = NULL) {
  mod_dat <- model.frame(formula, data = fit$data)
  x_var <- attr(mod_dat, "bnec_pop")[["x_var"]]
  if (is.na(x_range[1])) {
    x <- fit$data[[x_var]]
    x_seq <- seq(min(x), max(x), length = resolution)
  } else {
    x_seq <- seq(min(x_range), max(x_range), length = resolution)
  }
  newdata <- data.frame(x_seq)
  names(newdata) <- x_var
  fam_tag <- fit$family$family
  if (fam_tag == "binomial" || fam_tag == "beta_binomial") {
    newdata[[attr(mod_dat, "bnec_pop")[["trials_var"]]]] <- 1
  }
  # Set rate() exposure to 1 so predictions report the rate, the scale of
  # response parameters such as top and bot. nec stays on the predictor scale.
  # Single bracket, not [[: bnec_pop is a named character vector, so [[ on a
  # missing name is a subscript error rather than NULL.
  rate_var <- unname(attr(mod_dat, "bnec_pop")["rate_var"])
  if (!is.na(rate_var)) {
    newdata[[rate_var]] <- 1
  }
  if (!is.null(level_spec)) {
    newdata <- add_grid_levels(newdata, level_spec, length(x_seq))
  }
  # brms evaluates the dispersion for posterior_epred() too, so a disp(~z)
  # column other than the predictor must be on the grid. Add it after the
  # level term, so that a column the grid already sets is left alone. See
  # add_disp_placeholders().
  newdata <- add_disp_placeholders(newdata, fit$data, formula, x_var)
  list(newdata = newdata, x_seq = x_seq, x_var = x_var)
}

#' Repeat a prediction grid once per level of a level term
#'
#' The level varies slowest, so the matrix \code{\link[brms]{posterior_epred}}
#' returns splits into contiguous blocks of \code{n_x} columns, one per
#' predicted level.
#'
#' @param newdata The single-level grid.
#' @param level_spec See \code{prediction_grid}.
#' @param n_x The number of grid points per level.
#'
#' @return A \code{\link[base]{data.frame}} with \code{n_x} rows per predicted
#' level.
#'
#' @noRd
add_grid_levels <- function(newdata, level_spec, n_x) {
  all_levels <- level_spec$levels
  predict_levels <- level_spec$predict_levels
  if (is.null(predict_levels)) {
    predict_levels <- all_levels
  }
  out <- newdata[rep(seq_len(n_x), times = length(predict_levels)), ,
                 drop = FALSE]
  out[[level_spec$group_var]] <- factor(rep(predict_levels, each = n_x),
                                        levels = all_levels)
  # Where the levels chose different equations, the mean reads a 0/1 indicator
  # column per level. Write every level's column, predicted or not, because the
  # composed mean reads all of them on every row.
  if (!is.null(level_spec$inds)) {
    row_level <- rep(predict_levels, each = n_x)
    for (l in all_levels) {
      out[[level_spec$inds[[l]]]] <- as.numeric(row_level == l)
    }
  }
  rownames(out) <- NULL
  out
}

#' Posterior expectation over the prediction grid
#'
#' The posterior on the grid \code{\link{expand_nec}} predicts over, used by
#' \code{\link{expand_manec}} for the model-averaged draws.
#'
#' @inheritParams prediction_grid
#'
#' @return A \code{\link[base]{matrix}} with draws as rows and grid points as
#' columns.
#'
#' @importFrom brms posterior_epred
#'
#' @noRd
posterior_on_grid <- function(fit, formula, x_range = NA, resolution = 1000) {
  new_dat <- prediction_grid(fit, formula, x_range = x_range,
                             resolution = resolution)$newdata
  factorised_count_epred(fit, formula, newdata = new_dat, re_formula = NA)
}

#' Extracts a range of statistics from a list of \code{\link{prebayesnecfit}}
#' objects.
#'
#' @inheritParams bnec
#'
#' @param object A \code{\link[base]{list}} of objects of class
#' \code{\link{prebayesnecfit}}.
#' @param formula Either a \code{\link[base]{character}} string defining an
#' R formula or an actual \code{\link[stats]{formula}} object. See
#' \code{\link{bayesnecformula}} and \code{\link{check_formula}}. It could also
#' be a \code{\link[base]{list}} of formulas if multiple objects are passed to
#' \code{object}.
#'
#' @details When recovered input objects contain \code{retained_data}, the
#' returned model set stores that data frame once if every available copy is
#' identical. Different retained frames are reported and omitted because one
#' frame cannot align observations for the whole set.
#'
#' @return A \code{\link[base]{list}} of model statistical output derived from
#' the input model list. This includes \code{retained_data} when compatible
#' retained columns were present on the input objects.
#'
#' @importFrom loo loo_model_weights
#' @importFrom stats quantile
#' @importFrom chk chk_numeric
#'
#' @export
expand_manec <- function(object, formula, x_range = NA, resolution = 1000,
                         sig_val = 0.01, loo_controls) {
  check_args_newdata(resolution, x_range)
  chk_numeric(sig_val)
  model_set <- names(object)
  success_models <- model_set[sapply(object, is_prebayesnecfit)]
  if (length(success_models) == 0) {
    stop("None of the models fit successfully, ",
         "try using bnec with a single model (e.g. ecxexp) ",
         "using the default settings as a starting point ",
         "for trouble shooting, or check ?show_params to ",
         "make sure you have the correct parameter names ",
         "for your priors.")
  } else if (length(success_models) == 1) {
    message("Only ", success_models, " is fitted, ",
            "no model averaging done. Perhaps try setting better ",
            "priors, or check ?show_params to make sure you have ",
            "the correct parameter names for your priors.\n",
            "Returning ", success_models)
    return(object[success_models])
  } else {
    message(paste0("Fitted models are: ",
                   paste0(success_models, collapse = " ")))
  }
  retained_sources <- attr(object, "retained_data_sources", exact = TRUE)
  object <- object[success_models]
  if (is.null(retained_sources)) {
    retained_sources <- object
  }
  # Retained columns belong once on the completed set. Recovered single-model
  # fits include them only so they survive transport to this function; keeping
  # them in mod_fits as well would duplicate the same frame for every equation.
  mod_fits <- lapply(object, function(fit) {
    fit$retained_data <- NULL
    fit
  })
  object <- mod_fits
  formula <- formula[success_models]
  # Classify by each equation's parameters, as nec() does, not by group
  # membership: ecxflat is in neither mod_groups$ecx nor mod_groups$nec.
  has_nec <- vapply(names(object), function(m) {
    has_nec_parameter(m, object[[m]]$fit)
  }, logical(1))
  ne_lab <- "NEC"
  if (!any(has_nec)) {
    ne_lab <- "NSEC"
  } else if (!all(has_nec)) {
    ne_lab <- "N(S)EC"
  }
  # define_loo_controls(), not validate_loo_controls(), on both branches.
  # Validation leaves `method` NULL, and loo::loo_model_weights() then
  # defaults to stacking. Every route that assembles a set reaches this line,
  # so the pseudo-BMA default is supplied here.
  fam_tag <- object[[1]]$fit$family$family
  loo_controls <- if (missing(loo_controls)) {
    define_loo_controls(family_str = fam_tag)
  } else {
    define_loo_controls(loo_controls, fam_tag)
  }
  loo_w_controls <- loo_controls$weights
  for (i in seq_along(object)) {
    object[[i]] <- expand_nec(object[[i]], formula = formula[[i]],
                              x_range = x_range, resolution = resolution,
                              sig_val = sig_val, loo_controls = loo_controls,
                              model = success_models[i])
  }
  mod_dat <- model.frame(formula[[1]], data = object[[1]]$fit$data)
  y_var <- attr(mod_dat, "bnec_pop")[["y_var"]]
  disp <-  do_wrapper(object, extract_dispersion, fct = "rbind")
  colnames(disp) <- c("dispersion_Estimate", "dispersion_Q2.5",
                      "dispersion_Q97.5", "dispersion_P_over_1")
  mod_stats <- data.frame(model = success_models)
  mod_stats$waic <- sapply(object, extract_waic_estimate)
  loo_mw_args <- c(list(x = lapply(object, extract_loo)), loo_w_controls)
  mod_stats$wi <- do.call(loo_model_weights, loo_mw_args)
  attr(mod_stats$wi, "method") <- loo_w_controls$method
  mod_stats <- cbind(mod_stats, disp)
  sample_size <- set_draw_count(object)
  # Realise the weighted draw once and store both the index and its seed.
  # Later calls reuse the index, so repeated calls agree, and a stored index
  # cannot change if sample() changes between R versions. The seed covers a
  # caller thinning to a different number of draws.
  draw_seed <- sample.int(.Machine$integer.max, 1)
  draw_index <- weighted_draw_index(success_models, sample_size, mod_stats,
                                    draw_seed)
  ne_posterior <- unlist(lapply(success_models, w_nec_calc,
                                 object, draw_index))
  # Build the combined record under the same draw index as the mixture, so
  # that the censored fraction is weighted by model weight.
  attr(ne_posterior, "censored") <- concat_censoring(
    lapply(success_models, function(m) {
      subset_censoring(attr(object[[m]]$ne_posterior, "censored"),
                       draw_index[[m]])
    }),
    vapply(success_models, function(m) length(draw_index[[m]]), integer(1))
  )
  y_pred <- rowSums(do_wrapper(success_models, w_pred_calc,
                               object, mod_stats))
  # Thin each model's grid posterior to the rows the weighting keeps as soon
  # as it is computed. Building them all first would hold every model's full
  # draws-by-grid matrix at once.
  post_pred <- do_wrapper(success_models, w_grid_pred_calc, object, formula,
                          x_range, resolution, draw_index,
                          fct = "rbind")
  x <- object[[success_models[1]]]$pred_vals$data$x
  pred_data <- cbind(x = x,
                     data.frame(t(apply(post_pred, 2,
                                        estimates_summary))))
  nec <- estimates_summary(ne_posterior)
  # Report once for the set what censoring leaves of the model-averaged
  # estimate; the per-equation reports above name the curves.
  report_ne_censoring(attr(nec, "censored_summary"), "model-averaged", ne_lab)
  # Do not store post_pred: only pred_data, built from it, is read.
  out <- list(mod_fits = mod_fits, success_models = success_models,
              mod_stats = mod_stats, sample_size = sample_size,
              w_draw_seed = draw_seed, w_draw_index = draw_index,
              w_ne_posterior = ne_posterior, w_predicted_y = y_pred,
              w_residuals = mod_dat[[y_var]] - y_pred,
              w_pred_vals = list(data = pred_data),
              w_ne = nec, ne_type = ne_lab)
  retain_shared_data(out, retained_sources)
}

#' Report the censoring of a stored no-effect summary, once
#'
#' Raised from \code{\link{expand_nec}} and \code{expand_manec()}. A message,
#' not a warning: the estimate is usable, and the estimators already raise
#' their own warnings per draw.
#'
#' @param cens The \code{"censored_summary"} attribute of a summary.
#' @param what A \code{\link[base]{character}} naming the fit or equation.
#' @param label A \code{\link[base]{character}} naming the estimate type.
#'
#' @return \code{NULL}, invisibly. Called for the message.
#' @noRd
report_ne_censoring <- function(cens, what, label = "N(S)EC") {
  if (is.null(cens)) {
    return(invisible(NULL))
  }
  if (cens$n_above > 0) {
    message("The ", what, " ", label, " is censored above ",
            signif(cens$upper, 3), ", the upper bound of the prediction ",
            "range: ", cens$n_above, " of ", cens$n_draws, " draws lie at or ",
            "beyond it. They keep their rank in the summary and are given no ",
            "value, so any reported quantile falling among them is a bound.")
  }
  if (cens$n_below > 0) {
    message("The ", what, " ", label, " is censored below ",
            signif(cens$lower, 3), ", the lower bound of the prediction ",
            "range: ", cens$n_below, " of ", cens$n_draws, " draws lie at or ",
            "beyond it.")
  }
  invisible(NULL)
}
