#'extract_modstats
#'
#' Extracts a range of statistics from a list of bayesnecfit model fits.
#'
#' @param mod_fits a bayesmanecfit mod_fits output list, as returned by
#' \code{\link{bnec}} when more than one model is supplied.
#'
#' @export
#' @return A list of model statistical output derived from the input model list
#' @importFrom brms loo_model_weights waic
#' @importFrom stats quantile predict
extract_modstats <- function(mod_fits) {
  model_set <- names(mod_fits)
  success_models <- model_set[sapply(mod_fits, class) == "bayesnecfit"]
  if (length(success_models) == 0) {
    stop("None of the models fit successfully, ",
         "try using bnec with a single model (e.g. ecxexp) using the default ",
         "settings as a starting point for trouble shooting.")
  } else {
    message(paste("successfully fitted the models: ",
                  paste(success_models, collapse = " ")))
  }
  mod_fits <- mod_fits[success_models]
  family <- mod_fits[[1]]$family
  x_type <- mod_fits[[1]]$x_type
  mod_dat <- mod_fits[[1]]$mod_dat
  disp <-  do.call("rbind", lapply(mod_fits, function(x) x$over_disp))
  colnames(disp) <- c("dispersion_mean", "dispersion_median",
                      "dispersion_lw", "dispersion_up")
  mod_stats <- data.frame(model = success_models)
  mod_stats$waic <- sapply(mod_fits, function(x) {
    waic(x$fit)$estimates["waic", "Estimate"]
  })
  mod_stats$wi <- loo_model_weights(lapply(mod_fits, function(x) x$fit$loo))
  mod_stats <- cbind(mod_stats, disp)

  sample_size <- nrow(predict(mod_fits[[1]]$fit, summary = FALSE))

  # model averaged nec posterior
  nec_posterior <- unlist(lapply(seq_len(length(success_models)),
                                 function(x, mod_fits, sample_size, mod_stats) {
    sample(mod_fits[[x]]$nec_posterior,
           size = as.integer(round(sample_size * mod_stats[x, "wi"])))
  }, mod_fits, sample_size, mod_stats))

  # model averaged predicted y
  predicted_y <- rowSums(do.call("cbind",
                                 lapply(seq_len(length(success_models)),
                                        function(x, mod_fits, mod_stats) {
    mod_fits[[x]]$predicted_y * mod_stats[x, "wi"]
  }, mod_fits, mod_stats)))

  # model averaged pred_vals
  x <- mod_fits[[success_models[1]]]$pred_vals$x

  y_m <- rowSums(do.call("cbind", lapply(success_models,
                                         function(x, mod_fits, mod_stats) {
    mod_fits[[x]]$pred_vals$y_m * mod_stats[x, "wi"]
  }, mod_fits, mod_stats)))

  # model weighted posterior
  posterior_predicted <- do.call("cbind",
                                 lapply(seq_len(length(success_models)),
                                        function(x, mod_fits, sample_size,
                                                 mod_stats) {
    mod_fits[[x]]$pred_vals$posterior[, sample(seq_len(sample_size),
                                               round(sample_size *
                                                       mod_stats[x, "wi"]))]
  }, mod_fits, sample_size, mod_stats))

  y <- apply(posterior_predicted, 1, median)
  up <- apply(posterior_predicted, 1, quantile, probs = 0.975)
  lw <- apply(posterior_predicted, 1, quantile, probs = 0.025)

  nec <- quantile(nec_posterior, c(0.5, 0.025,  0.975))
  names(nec) <- c("Estimate", "Q2.5", "Q97.5")

  list(mod_fits = mod_fits,
       success_models = success_models,
       mod_dat = mod_dat,
       family = family,
       x_type = x_type,
       mod_stats = mod_stats,
       sample_size = sample_size,
       nec_posterior = nec_posterior,
       predicted_y = predicted_y,
       residuals = mod_dat$y - predicted_y,
       pred_vals = list(x = x, y = y, up = up,
                        lw = lw, posterior = posterior_predicted,
                        y_m = y_m),
       nec = nec)
}
