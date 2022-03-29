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
#'
#' @export
expand_nec <- function(object, formula, x_range = NA, precision = 1000,
                       sig_val = 0.01, loo_controls, ...) {
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
  names(extracted_params) <- extract_params
  mod_dat <- model.frame(formula, data = fit$data)
  x_var <- attr(mod_dat, "bnec_pop")[["x_var"]]
  x <- fit$data[[x_var]]
  if (any(is.na(x_range))) {
    x_seq <- seq(min(x), max(x), length = precision)
  } else {
    x_seq <- seq(min(x_range), max(x_range), length = precision)
  }
  new_dat <- data.frame(x_seq)
  names(new_dat) <- x_var
  custom_name <- check_custom_name(fit$family)
  if (fam_tag == "binomial" || custom_name == "beta_binomial2") {
    trials_col_name <- attr(mod_dat, "bnec_pop")[["trials_var"]]
    new_dat[[trials_col_name]] <- 1
  }
  pred_posterior <- posterior_epred(fit, newdata = new_dat, re_formula = NA)
  y_pred_m <- fitted(fit, newdata = new_dat, robust = TRUE, re_formula = NA,
                     scale = "response")
  pred_data <- data.frame(x = x_seq, Estimate = y_pred_m[, "Estimate"],
                          Q2.5 = y_pred_m[, "Q2.5"],
                          Q97.5 = y_pred_m[, "Q97.5"])
  if (is.na(extracted_params$nec["Estimate"])) {
    mod_class <- "ecx"
  } else {
    mod_class <- "nec"
  }
  if (mod_class == "ecx") {
    reference <- quantile(pred_posterior[, 1], sig_val)
    grab <- apply(pred_posterior - reference, 1, min_abs)
    nec_posterior <- pred_data$x[grab]
    x_str <- grep("crf(", labels(terms(formula)), fixed = TRUE, value = TRUE)
    x_call <- str2lang(eval(parse(text = x_str)))
    if (inherits(x_call, "call")) {
      x_call[[2]] <- str2lang("nec_posterior")
      nec_posterior <- eval(x_call)
    }
    extracted_params$nec <- estimates_summary(nec_posterior)
  } else {
    nec_posterior <- as_draws_df(fit)[["b_nec_Intercept"]]
  }
  pred_vals <- list(data = pred_data, posterior = pred_posterior)
  od <- dispersion(object, summary = TRUE)
  if (length(od) == 0) {
    od <- c(NA, NA, NA)
  }
  predicted_y <- fitted(fit, robust = TRUE, re_formula = NA, scale = "response")
  residuals <-  residuals(fit, method = "pp_expect")[, "Estimate"]
  c(object, list(pred_vals = pred_vals), extracted_params,
    list(dispersion = od, predicted_y = predicted_y, residuals = residuals,
         nec_posterior = nec_posterior))
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
#'
#' @export
expand_manec <- function(object, formula, x_range = NA, precision = 1000,
                         sig_val = 0.01, loo_controls) {
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
  if (missing(loo_controls)) {
    loo_controls <- list(fitting = list(), weights = list())
  } else {
    fam_tag <- object[[1]]$fit$family$family
    loo_controls <- validate_loo_controls(loo_controls, fam_tag)
  }
  loo_w_controls <- loo_controls$weights
  for (i in seq_along(object)) {
    object[[i]] <- expand_nec(object[[i]], formula = formula[[i]],
                              x_range = x_range, precision = precision,
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
  nec_posterior <- unlist(lapply(success_models, w_nec_calc,
                                 object, sample_size, mod_stats))
  y_pred <- rowSums(do_wrapper(success_models, w_pred_calc,
                               object, mod_stats))
  post_pred <- do_wrapper(success_models, w_post_pred_calc,
                          object, sample_size, mod_stats,
                          fct = "rbind")
  x <- object[[success_models[1]]]$pred_vals$data$x
  pred_data <- cbind(x = x,
                     data.frame(t(apply(post_pred, 2,
                                        estimates_summary))))
  nec <- estimates_summary(nec_posterior)
  list(mod_fits = mod_fits, success_models = success_models,
       mod_stats = mod_stats, sample_size = sample_size,
       w_nec_posterior = nec_posterior, w_predicted_y = y_pred,
       w_residuals = mod_dat[[y_var]] - y_pred,
       w_pred_vals = list(data = pred_data, posterior = post_pred),
       w_nec = nec)
}
