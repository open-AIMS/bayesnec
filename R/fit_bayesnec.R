#' fit_bayesnec
#'
#' Fits a concentration(dose)-response model using brms
#'
#' @inheritParams bnec
#'
#' @importFrom brms fixef brm posterior_epred posterior_samples loo waic fitted
#' @importFrom stats quantile predict
#' @seealso \code{\link{bnec}}
#' @return The fitted \pkg{brms} model, including an estimate of the NEC
#' value and predicted posterior values.
#' A posterior sample of the NEC is also available under \code{nec_posterior}
fit_bayesnec <- function(data, x_var, y_var, trials_var = NA,
                         x_type = NA, family = NA, x_range = NA,
                         precision = 1000, over_disp = FALSE,
                         model = NA, sig_val = 0.01, ...) {

  if (inherits(data, "bayesmanecfit")) {
    response <- data$data[, y_var]
    mod_dat <- data$mod_dat
    y_dat <- data$y_dat
    x_dat <- data$x_dat
    family <- data$family
    x_type <- data$x_type
    if (family == "binomial") {
      response <- response / data$data[, trials_var]
    }
    mod_file <- define_prior(model = model, x_type = x_type,
                             family = family, mod_dat = mod_dat)
    priors <- mod_file$priors
    mod_family <- mod_file$mod_family
  } else {
    data_check <- check_data(data = data, x_var = x_var, y_var = y_var,
                             trials_var = trials_var, x_type = x_type,
                             family = family, over_disp = over_disp,
                             model = model)
    mod_dat <- data_check$mod_dat
    family <- data_check$family
    x_type <- data_check$x_type
    response <- data_check$response
    data <- data_check$data
    x_dat <- data_check$x_dat
    y_dat <- data_check$y_dat
    priors <- data_check$priors
    mod_family <- data_check$mod_family
  }
  family_code <- names(mod_fams)[mod_fams == family]
  fit <- fit_stan(model = model, family_code = family_code,
                  new_priors = priors, new_data = mod_dat,
                  chains = 4, ...)
  fit$loo <- loo(fit)
  fit$waic <- waic(fit)

  out <- list(fit = fit, mod_dat = mod_dat,
              family = family, x_type = x_type, model = model)

  extract_params <- c("top", "beta", "nec", "alpha",
                      "bot", "d", "slope", "ec50")
  extracted_params <- lapply(extract_params, function(x, model_fit) {
    fef <- fixef(model_fit)
    tt <- fef[grep(x, rownames(fef)), c("Estimate", "Q2.5", "Q97.5")]
    if (is.na(tt["Estimate"])) {
      NA
    } else {
      tt
    }
  }, model_fit = fit)
  names(extracted_params) <- extract_params

  top <- extracted_params$top
  beta <- extracted_params$beta
  nec <- extracted_params$nec
  alpha <- extracted_params$alpha
  bot <- extracted_params$bot
  d <- extracted_params$d
  slope <- extracted_params$slope
  ec50 <- extracted_params$ec50

  if (is.na(extracted_params$nec["Estimate"])) {
    mod_class <- "ecx"
  } else {
    mod_class <- "nec"
  }

  if (is.na(x_range)) {
    x_seq <- seq(min(mod_dat$x), max(mod_dat$x),
                 length = precision)
  }

  new_dat <- data.frame(x = x_seq)
  if (family == "binomial") {
    new_dat$trials <- 10^3
  }

  y_pred_m <- fitted(fit, newdata = new_dat, robust = TRUE, re_formula = NA, scale = "response")
  predicted_y <- fitted(fit, robust = TRUE, re_formula = NA, scale = "response")

  if (family == "binomial") {
    top <- top / 10^3
    predicted_y <- predicted_y / 10^3
    y_pred_m <-  y_pred_m / 10^3
  }

  residuals <-  response - predicted_y
  pred_posterior <- t(posterior_epred(fit, newdata = new_dat,
                              re_formula = NA))
  if (family == "binomial") {
    pred_posterior <- pred_posterior / 10^3
  }

  pred_vals <- c(list(x = x_seq, y = y_pred_m[, "Estimate"],
                      up = y_pred_m[, "Q97.5"], lw = y_pred_m[, "Q2.5"],
                      posterior = pred_posterior),
                 list(y_pred_m = y_pred_m[, "Estimate"]))

  od <- dispersion(fit, summary = TRUE)
  if (is.null(od)) {
    od <- c(NA, NA, NA, NA)
  }

  if (mod_class == "ecx") {
    reference <- quantile(pred_vals$posterior[1, ], sig_val)
    nec_posterior <- sapply(seq_len(ncol(pred_vals$posterior)),
                            function(x, pred_vals, reference) {
      pred_vals$x[which.min(abs(pred_vals$posterior[, x] - reference))]
    }, pred_vals = pred_vals, reference = reference)

    nec <- quantile(nec_posterior, c(0.5, 0.025,  0.975))
    names(nec) <- c("Estimate", "Q2.5", "Q97.5")
  } else {
    nec_posterior <- unlist(posterior_samples(fit, pars = "nec_Intercept"))
  }

  if (!inherits(out, "try-error")) {
    out <- c(out, list(pred_vals = pred_vals, nec = nec, top = top,
                       beta = beta, alpha = alpha, bot = bot,
                       d = d, ec50 = ec50, over_disp = od,
                       predicted_y = predicted_y, residuals = residuals,
                       nec_posterior = nec_posterior))
    class(out) <- "bayesnecfit"
  }

  message(paste0("Response variable ", y_var, " modelled as a ",
                 model, " model using a ", family, " distribution."))
  out
}
