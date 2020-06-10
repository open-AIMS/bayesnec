#' fit_bayesnec
#'
#' Fits a concentration(dose)-response model using brms
#'
#' @inheritParams bnec
#'
#' @importFrom brms brm posterior_epred posterior_samples loo waic
#' @importFrom stats quantile fitted
#' @seealso \code{\link{bnec}}
#' @return The fitted \pkg{brms} model, including an estimate of the NEC
#' value and predicted posterior values.
#' A posterior sample of the NEC is also available under \code{nec_posterior}
fit_bayesnec <- function(data, x_var, y_var, trials_var = NA,
                         x_type = NA, family = NULL, priors,
                         x_range = NA, precision = 1000,
                         model = NA, sig_val = 0.01, ...) {

  data_check <- check_data(data = data, x_var = x_var, y_var = y_var,
                           trials_var = trials_var, x_type = x_type,
                           family = family,
                           model = model)
  mod_dat <- data_check$mod_dat
  family <- data_check$family
  x_type <- data_check$x_type
  response <- data_check$response
  data <- data_check$data
  x_dat <- data_check$x_dat
  y_dat <- data_check$y_dat
  if (missing(priors)) {
    priors <- data_check$priors
  }

  suffix <- ifelse(family$family == "binomial", "_binom", "_deflt")
  brms_bf <- get(paste0("bf_", model, suffix))
  fit <- brm(formula = brms_bf, data = mod_dat, family = family,
             prior = priors, ...)
  fit$loo <- loo(fit)
  fit$waic <- waic(fit)

  out <- list(fit = fit, mod_dat = mod_dat,
              family = family, x_type = x_type, model = model)

  extract_params <- c("top", "beta", "nec", "alpha",
                      "bot", "d", "slope", "ec50")
  extracted_params <- lapply(extract_params, extract_pars, fit)
  names(extracted_params) <- extract_params

  if (is.na(x_range)) {
    x_seq <- seq(min(mod_dat$x), max(mod_dat$x),
                 length = precision)
  }
  new_dat <- data.frame(x = x_seq)
  pred_posterior <- posterior_epred(fit, newdata = new_dat,
                                    re_formula = NA)
  if (family$family == "binomial") {
    pred_posterior <- pred_posterior / 10^3
  }
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
  residuals <-  response - predicted_y
  out <- c(out, list(pred_vals = pred_vals), extracted_params,
           list(dispersion = od, predicted_y = predicted_y,
                residuals = residuals, nec_posterior = nec_posterior))
  class(out) <- "bayesnecfit"

  message(paste0("Response variable ", y_var, " modelled as a ",
                 model, " model using a ", family$family, " distribution."))
  out
}
