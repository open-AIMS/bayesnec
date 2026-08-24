#' Extracts a range of statistics from a \code{\link{prebayesnecfit}} object.
#'
#' @inheritParams bnec
#'
#' @param object An object of class \code{\link{prebayesnecfit}}.
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
  extract_params <- c("top", "beta", "nec", "f",
                      "bot", "d", "slope", "ec50")
  extracted_params <- lapply(extract_params, extract_pars, fit)
  names(extracted_params) <- gsub("^nec$", "ne", extract_params)
  grid <- prediction_grid(fit, formula, x_range = x_range,
                          resolution = resolution)
  new_dat <- grid$newdata
  x_seq <- grid$x_seq
  # Computed only where it is used, and never stored. The full n_draws x
  # resolution matrix dominated the size of a fitted object -- 30.5 MB against a
  # 1.2 MB brmsfit at the defaults -- and had exactly one reader, the
  # model-averaging path in expand_manec(), which now builds its own. A
  # threshold model with a single block needs it for nothing at all, so this is
  # a saving rather than a trade. See #180.
  pred_posterior <- NULL
  get_pred_posterior <- function() {
    if (is.null(pred_posterior)) {
      pred_posterior <<- posterior_epred(fit, newdata = new_dat,
                                         re_formula = NA)
    }
    pred_posterior
  }
  y_pred_m <- fitted(fit, newdata = new_dat, robust = TRUE, re_formula = NA,
                     scale = "response")
  pred_data <- data.frame(x = x_seq, Estimate = y_pred_m[, "Estimate"],
                          Q2.5 = y_pred_m[, "Q2.5"],
                          Q97.5 = y_pred_m[, "Q97.5"])
  if (is.na(extracted_params$ne["Estimate"])) {
    mod_class <- "ecx"
  } else {
    mod_class <- "nec"
  }
  # NSEC read off a fitted curve, on the predictor scale the user supplied it
  # on. Used for smooth (ecx-type) models, which carry no threshold parameter,
  # and for any two-block fit where at least one block is smooth.
  nsec_off_curve <- function(post) {
    reference <- quantile(post[, 1], sig_val)
    out <- apply(post, 1, nsec_fct, reference = reference, x_vec = pred_data$x)
    x_str <- grep("crf(", labels(terms(formula)), fixed = TRUE, value = TRUE)
    x_call <- str2lang(eval(parse(text = x_str)))
    if (inherits(x_call, "call")) {
      x_call[[2]] <- str2lang("out")
      out <- eval(x_call)
    }
    out
  }
  if (mod_class == "ecx") {
    ne_posterior <- nsec_off_curve(get_pred_posterior())
    extracted_params$ne <- estimates_summary(ne_posterior)
  } else {
    ne_posterior <- as_draws_df(fit)[["b_nec_Intercept"]]
  }
  pred_vals <- list(data = pred_data)
  # Hurdle fits carry a second block. Keep its threshold alongside the combined
  # one, and make the *combined* threshold the headline value --
  # posterior_epred() already returns mu * (1 - hu), so pred_vals and everything
  # downstream of it describe the combined endpoint, and the NEC should describe
  # the same curve. The two component curves used to be stored here as well;
  # nothing ever read them, so they are no longer computed (#214).
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
      ne_lab <- "NEC"
    } else {
      # With a smooth block on either side there is no threshold to take the
      # minimum of, so the no-effect estimate is interpolated off the combined
      # curve itself rather than combined from the parts. When only one block
      # is smooth this is an N(S)EC in the sense of Fisher et al. (2023): a
      # threshold on one process and a significant-effect point on the other.
      combined_ne <- nsec_off_curve(get_pred_posterior())
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
  }
  od <- dispersion(object, summary = TRUE)
  if (length(od) == 0) {
    od <- c(NA, NA, NA)
  }
  predicted_y <- fitted(fit, robust = TRUE, re_formula = NA, scale = "response")
  residuals <-  residuals(fit, method = "pp_expect")[, "Estimate"]
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
  # Only hurdle fits carry the extra element. Appending it unconditionally
  # would change names() on every bayesnecfit, which is a gratuitous break for
  # anything indexing the object positionally or checking its structure.
  if (!is.null(hurdle_parts)) {
    out <- c(out, list(hurdle = hurdle_parts))
  }
  out
}

#' The grid predictions are made over
#'
#' The single definition of the prediction grid. \code{\link{expand_nec}},
#' \code{posterior_on_grid} and the exported \code{\link{bnec_newdata}} all
#' build the same grid from the same code, so they cannot drift apart. Takes a
#' \code{\link[brms]{brmsfit}} and a formula rather than a
#' \code{\link{bayesnecfit}} because inside \code{\link{expand_manec}} the
#' object has not been given its class yet. See #211.
#'
#' @param fit An object of class \code{\link[brms]{brmsfit}}.
#' @param formula An object of class \code{\link{bayesnecformula}}.
#'
#' @inheritParams bnec
#'
#' @return A \code{\link[base]{list}} of \code{newdata}, the
#' \code{\link[base]{data.frame}} to predict over; \code{x_seq}, the predictor
#' values it spans; and \code{x_var}, the name of the predictor.
#'
#' @importFrom stats model.frame
#'
#' @noRd
prediction_grid <- function(fit, formula, x_range = NA, resolution = 1000) {
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
  list(newdata = newdata, x_seq = x_seq, x_var = x_var)
}

#' Posterior expectation over the prediction grid
#'
#' The posterior on the grid \code{\link{expand_nec}} predicts over. Factored
#' out because \code{\link{expand_manec}} needs the same matrix for the
#' model-averaged draws and no longer finds it stored on the object.
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
  posterior_epred(fit, newdata = new_dat, re_formula = NA)
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
#' @return A \code{\link[base]{list}} of model statistical output derived from
#' the input model list.
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
  mod_fits <- object[success_models]
  object <- object[success_models]
  formula <- formula[success_models]
  ne_lab <- "NEC"
  if (all(success_models %in% mod_groups$ecx)) {
    ne_lab <- "NSEC"
  } else if (any(success_models %in% mod_groups$ecx) & any(success_models %in% mod_groups$nec)) {
    ne_lab <- "N(S)EC"
  }
  if (missing(loo_controls)) {
    loo_controls <- list(fitting = list(), weights = list())
  } else {
    fam_tag <- object[[1]]$fit$family$family
    loo_controls <- validate_loo_controls(loo_controls, fam_tag)
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
  colnames(disp) <- c("dispersion_Estimate",
                      "dispersion_Q2.5", "dispersion_Q97.5")
  mod_stats <- data.frame(model = success_models)
  mod_stats$waic <- sapply(object, extract_waic_estimate)
  loo_mw_args <- c(list(x = lapply(object, extract_loo)), loo_w_controls)
  mod_stats$wi <- do.call(loo_model_weights, loo_mw_args)
  attr(mod_stats$wi, "method") <- loo_w_controls$method
  mod_stats <- cbind(mod_stats, disp)
  sample_size <- extract_simdat(object[[1]])$n_samples
  # The weighted draw is realised once, here, and the seed that produced it is
  # kept on the object. Every later call -- posterior_epred(), fitted() and the
  # weighting inside predict() -- rebuilds the same index from that seed, so
  # repeated calls agree with each other and with the summaries below.
  # Previously each site drew its own unseeded sample(), so no two calls
  # returned the same answer. Drawing the seed rather than fixing one keeps the
  # realisation genuinely random per object, and responsive to a set.seed() in
  # the caller's session. Both the index and the seed behind it are kept: the
  # index is what later calls use, because a stored index cannot drift the way
  # a regenerated one can if sample()'s algorithm changes again as it did in
  # R 3.6.0, and these objects are archived and reopened years later. The seed
  # is kept for the cases the stored index cannot cover -- a caller thinning to
  # a different number of draws. See #216.
  draw_seed <- sample.int(.Machine$integer.max, 1)
  draw_index <- weighted_draw_index(success_models, sample_size, mod_stats,
                                    draw_seed)
  ne_posterior <- unlist(lapply(success_models, w_nec_calc,
                                 object, draw_index))
  y_pred <- rowSums(do_wrapper(success_models, w_pred_calc,
                               object, mod_stats))
  # Each model's posterior over the prediction grid is computed here and
  # immediately thinned to the round(sample_size * wi) rows the weighting keeps,
  # one model at a time. Accumulating them into a list first would hold every
  # model's full n_draws x resolution matrix at once -- the same peak #180 is
  # about, merely not retained afterwards. The weights do not depend on the
  # posteriors, so nothing forces them to be built together. See #180.
  post_pred <- do_wrapper(success_models, w_grid_pred_calc, object, formula,
                          x_range, resolution, draw_index,
                          fct = "rbind")
  x <- object[[success_models[1]]]$pred_vals$data$x
  pred_data <- cbind(x = x,
                     data.frame(t(apply(post_pred, 2,
                                        estimates_summary))))
  nec <- estimates_summary(ne_posterior)
  # post_pred itself is not kept: it was w_pred_vals$posterior, which nothing in
  # the package read and which became the dominant cost once the per-model
  # matrices went. pred_data, the summary the plot methods use, is built from it
  # here and is what survives. See #213.
  list(mod_fits = mod_fits, success_models = success_models,
       mod_stats = mod_stats, sample_size = sample_size,
       w_draw_seed = draw_seed, w_draw_index = draw_index,
       w_ne_posterior = ne_posterior, w_predicted_y = y_pred,
       w_residuals = mod_dat[[y_var]] - y_pred,
       w_pred_vals = list(data = pred_data),
       w_ne = nec, ne_type = ne_lab)
}
