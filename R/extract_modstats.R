#'extract_modstats
#'
#' Extracts a range of statistics from a list of bayesnecfit model fits.
#'
#' @param mod_fits a bayesmanecfit mod_fits output list, as returned by
#' \code{\link{bnec}} when more than one model is supplied.
#'
#' @return A list of model statistical output derived from the input model list
#' @importFrom loo loo_model_weights
#' @importFrom stats quantile
extract_modstats <- function(mod_fits) {
  model_set <- names(mod_fits)
  success_models <- model_set[sapply(mod_fits, class) == "bayesnecfit"]
  if (length(success_models) == 0) {
    stop("None of the models fit successfully, ",
         "try using bnec with a single model (e.g. ecxexp) using the default ",
         "settings as a starting point for trouble shooting.")
  } else if (length(success_models) == 1) {
    message(paste("Only", success_models, " was successfully fitted, "),
                  "no model averaging done. Perhaps try setting better priors.\n",
                  paste("returning", success_models))
    return(mod_fits[[success_models]])
  } else {
    message(paste("successfully fitted the models: ",
                  paste(success_models, collapse = " ")))
  }
  mod_fits <- mod_fits[success_models]
  family <- mod_fits[[1]]$family
  x_type <- mod_fits[[1]]$x_type
  mod_dat <- mod_fits[[1]]$mod_dat
  disp <-  do_wrapper(mod_fits, extract_dispersion, fct = "rbind")
  colnames(disp) <- c("dispersion_Estimate",
                      "dispersion_Q2.5", "dispersion_Q97.5")
  mod_stats <- data.frame(model = success_models)
  mod_stats$waic <- sapply(mod_fits, extract_waic)
  mod_stats$wi <- loo_model_weights(lapply(mod_fits, extract_loo))
  mod_stats <- cbind(mod_stats, disp)

  sample_size <- extract_simdat(mod_fits[[1]])$n_samples

  nec_posterior <- unlist(lapply(success_models, w_nec_calc,
                                 mod_fits, sample_size, mod_stats))

  y_pred <- rowSums(do_wrapper(success_models, w_pred_calc,
                               mod_fits, mod_stats))

  y_m <- rowSums(do_wrapper(success_models, w_y_calc,
                            mod_fits, mod_stats))

  post_pred <- do_wrapper(success_models, w_post_pred_calc,
                          mod_fits, sample_size, mod_stats,
                          fct = "rbind")

  x <- mod_fits[[success_models[1]]]$pred_vals$data$x
  pred_data <- cbind(x = x,
                     data.frame(t(apply(post_pred, 2,
                                        estimates_summary))))

  nec <- estimates_summary(nec_posterior)

  list(mod_fits = mod_fits, success_models = success_models,
       mod_stats = mod_stats, sample_size = sample_size,
       w_nec_posterior = nec_posterior, w_predicted_y = y_pred,
       w_residuals = mod_dat$y - y_pred,
       w_pred_vals = list(data = pred_data,
                          posterior = post_pred,
                          y_m = y_m),
       w_nec = nec)
}
