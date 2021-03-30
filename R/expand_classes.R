#' expand_nec
#'
#' Assigns class to a prebayesnecfit object
#'
#' @inheritParams bnec
#'
#' @param object An object of class prebayesnecfit.
#'
#' @return An object of class bayesnecfit.
#' @importFrom brms posterior_epred posterior_samples
#' @importFrom stats quantile fitted residuals
expand_nec <- function(object, x_range = NA, precision = 1000,
                       sig_val = 0.01) {
  fit <- object$fit
  mod_dat <- fit$data
  extract_params <- c("top", "beta", "nec", "alpha",
                      "bot", "d", "slope", "ec50")
  extracted_params <- lapply(extract_params, extract_pars, fit)
  names(extracted_params) <- extract_params

  if (any(is.na(x_range))) {
    x_seq <- seq(min(mod_dat$x), max(mod_dat$x), length = precision)
  } else {
    x_seq <- seq(min(x_range), max(x_range), length = precision)
  }
  new_dat <- data.frame(x = x_seq)
  fam_tag <- fit$family$family
  custom_name <- check_custom_name(fit$family)
  if (fam_tag == "binomial" | custom_name == "beta_binomial2") {
    new_dat$trials <- 1
  }
  pred_posterior <- posterior_epred(fit, newdata = new_dat,
                                    re_formula = NA)
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
    extracted_params$nec <- estimates_summary(nec_posterior)
  } else {
    nec_posterior <- unlist(posterior_samples(fit, pars = "nec_Intercept"))
  }
  pred_vals <- list(data = pred_data,
                    posterior = pred_posterior)
  od <- dispersion(fit, summary = TRUE)
  if (is.null(od)) {
    od <- c(NA, NA, NA)
  }
  predicted_y <- fitted(fit, robust = TRUE, re_formula = NA, scale = "response")
  residuals <-  residuals(fit, method = "pp_expect")[, "Estimate"]
  c(object, list(pred_vals = pred_vals), extracted_params,
    list(dispersion = od, predicted_y = predicted_y,
    residuals = residuals, nec_posterior = nec_posterior))
}

#' expand_manec
#'
#' Extracts a range of statistics from a list of bayesnecfit model fits.
#'
#' @inheritParams bnec
#'
#' @param object a bayesmanecfit mod_fits output list, as returned by
#' \code{\link{bnec}} when more than one model is supplied.
#'
#' @return A list of model statistical output derived from the input model list
#' @importFrom loo loo_model_weights
#' @importFrom stats quantile
expand_manec <- function(object, x_range = NA, precision = 1000,
                         sig_val = 0.01,
                         loo_controls = list(method = "pseudobma")) {
  model_set <- names(object)
  success_models <- model_set[sapply(object, class) == "prebayesnecfit"]
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
    return(object[[success_models]])
  } else {
    message(paste("Fitted models are: ",
                  paste(success_models, collapse = " ")))
  }
  mod_fits <- object[success_models]
  object <- object[success_models]
  for (i in seq_along(object)) {
    object[[i]] <- expand_nec(object[[i]], x_range = x_range,
                              precision = precision,
                              sig_val = sig_val)
  }
  mod_dat <- object[[1]]$fit$data
  disp <-  do_wrapper(object, extract_dispersion, fct = "rbind")
  colnames(disp) <- c("dispersion_Estimate",
                      "dispersion_Q2.5", "dispersion_Q97.5")
  mod_stats <- data.frame(model = success_models)
  mod_stats$waic <- sapply(object, extract_waic)
  loo_mw_args <- c(list(x = lapply(object, extract_loo)), loo_controls)
  mod_stats$wi <- do.call(loo_model_weights, loo_mw_args)
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
       w_residuals = mod_dat$y - y_pred,
       w_pred_vals = list(data = pred_data,
                          posterior = post_pred),
       w_nec = nec)
}
