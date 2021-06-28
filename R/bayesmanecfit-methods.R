#' plot.bayesmanecfit
#'
#' Generates a plot of a fitted \code{\link{bayesmanecfit}} object, as
#' returned by \code{\link{bnec}}.
#'
#' @inheritParams plot.bayesnecfit
#'
#' @param all_models A \code{\link[base]{logical}} value indicating if all
#' models in the model set should be plotted simultaneously, or if a model
#' average plot should be returned.
#'
#' @export
#' @importFrom graphics par plot mtext legend
#' @return a plot of the fitted model
plot.bayesmanecfit <- function(x, ..., CI = TRUE, add_nec = TRUE,
                               position_legend = "topright",
                               add_ec10 = FALSE, xform = NA,
                               lxform = NA, jitter_x = FALSE,
                               jitter_y = FALSE, ylab = "response",
                               xlab = "concentration", xticks = NA,
                               all_models = FALSE) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))            
  
  if (all_models) {
    mod_fits <- x$mod_fits
    par(mfrow = c(ceiling(length(mod_fits) / 2), 2),
        mar = c(1.5, 1.5, 1.5, 1.5), oma = c(3, 3, 0, 0))
    for (m in seq_along(mod_fits)) {
      mod_fits[[m]] <- expand_and_assign_nec(mod_fits[[m]])
      plot(x = mod_fits[[m]],
           CI = CI, add_nec = add_nec,
           position_legend = position_legend,
           add_ec10 = add_ec10,
           xform = xform, lxform = lxform,
           jitter_x = jitter_x, jitter_y = jitter_y,
           ylab = "", xlab = "",
           xticks = xticks, ...)
      mtext(xlab, side = 1, outer = TRUE, line = 2)
      mtext(ylab, side = 2, outer = TRUE, line = 2)
      legend("bottomleft", legend = names(mod_fits[m]), bty = "n")
    }
  } else {
    universal <- x$mod_fits[[1]]
    mod_dat <- universal$fit$data
    family <- universal$fit$family$family
    custom_name <- check_custom_name(universal$fit$family)
    if (family == "binomial" | custom_name == "beta_binomial2") {
      y_dat <- mod_dat$y / mod_dat$trials
    } else {
      y_dat <- mod_dat$y
    }
    ec10 <- c(NA, NA, NA)
    if (add_ec10 & family != "gaussian") {
      ec10 <- ecx(x)
    }
    if (add_ec10 & family == "gaussian") {
      ec10 <- ecx(x, type = "relative")
    }
    if (inherits(xform, "function")) {
      x_dat <- xform(mod_dat$x)
      nec <- xform(x$w_nec)
      x_vec <- xform(x$w_pred_vals$data$x)
      ec10 <- xform(ec10)
    } else {
      x_dat <- mod_dat$x
      nec <- x$w_nec
      x_vec <- x$w_pred_vals$data$x
    }
    if (jitter_x) {
      x_dat <- jitter(x_dat)
    }
    if (jitter_y) {
      y_dat <- jitter(y_dat)
    }
    if (length(xticks) == 1) {
      x_ticks <- seq(min(x_dat), max(x_dat), length = 7)
    } else {
      x_ticks <- xticks
    }
    plot(x_dat, y_dat, ylab = ylab, xlab = xlab,
         pch = 16, xaxt = "n", cex = 1.5,
         col = adjustcolor(1, alpha.f = 0.25), ...)
    if (!inherits(lxform, "function")) {
      if (length(xticks) == 1) {
        axis(side = 1)
      } else {
        axis(side = 1, at = signif(xticks, 2))
      }
      legend_nec <- paste("NEC: ", signif(nec["Estimate"], 2),
                          " (", signif(nec["Q2.5"], 2), "-",
                          signif(nec["Q97.5"], 2), ")", sep = "")
      legend_ec10 <- paste("EC10: ", signif(ec10[1], 2),
                           " (", signif(ec10[2], 2), "-",
                           signif(ec10[3], 2), ")", sep = "")
    } else {
      x_labs <- signif(lxform(x_ticks), 2)
      axis(side = 1, at = x_ticks, labels = x_labs)
      legend_nec <- paste("NEC: ", signif(lxform(nec["Estimate"]), 2),
                          " (", signif(lxform(nec["Q2.5"]), 2), "-",
                          signif(lxform(nec["Q97.5"]), 2), ")", sep = "")
      legend_ec10 <- paste("EC10: ", signif(lxform(ec10[1]), 2),
                           " (", signif(lxform(ec10[2]), 2), "-",
                           signif(lxform(ec10[3]), 2), ")", sep = "")
    }
    if (CI) {
      lines(x_vec, x$w_pred_vals$data$Q97.5, lty = 2)
      lines(x_vec, x$w_pred_vals$data$Q2.5, lty = 2)
    }
    lines(x_vec, x$w_pred_vals$data$Estimate)
    if (add_nec & !add_ec10) {
      abline(v = nec, col = "red", lty = c(1, 3, 3))
      legend(position_legend, bty = "n",
             legend = legend_nec, lty = 1, col = "red")
    }
    if (add_ec10 & !add_nec) {
      abline(v = ec10, col = "red", lty = c(1, 3, 3))
      legend(position_legend, bty = "n",
             legend = legend_ec10, lty = 1, col = "red")
    }
    if (add_ec10 & add_nec) {
      abline(v = nec, col = "red", lty = c(1, 3, 3))
      abline(v = ec10, col = "orange", lty = c(1, 3, 3))
      legend(position_legend, bty = "n",
             legend = c(legend_nec, legend_ec10),
             lty = 1, col = c("red", "orange"))
    }
  }
}

#' predict.bayesmanecfit
#'
#' @param object An object of class \code{\link{bayesmanecfit}} as
#' returned by \code{\link{bnec}}.
#' @param ... Unused.
#' @param precision the number of x values over which to predict values.
#' @param x_range The range of x values over which to make predictions.
#'
#' @return A list containing x and fitted y, with up and lw values
#'
#' @importFrom dplyr %>%
#' @importFrom brms posterior_epred
#'
#' @export
predict.bayesmanecfit <- function(object, ..., precision = 100,
                                x_range = NA) {
  mod_fits <- object$mod_fits
  model_set <- names(mod_fits)
  mod_dat <- object$mod_fits[[1]]$fit$data
  mod_stats <- object$mod_stats
  if (any(is.na(x_range))) {
    x_seq <- seq(min(mod_dat$x), max(mod_dat$x), length = precision)
  } else {
    x_seq <- seq(min(x_range), max(x_range), length = precision)
  }
  pred_list <- lapply(mod_fits, FUN = function(m) {
    fit <- m$fit
    new_dat <- data.frame(x = x_seq)
    fam_tag <- fit$family$family
    custom_name <- check_custom_name(fit$family)
    if (fam_tag == "binomial" | custom_name == "beta_binomial2") {
      new_dat$trials <- 1
    }
    posterior_epred(fit, newdata = new_dat, re_formula = NA)
  })
  sample_size <- min(sapply(pred_list, nrow))
  pred_out <- do_wrapper(model_set, w_pred_list_calc, pred_list, sample_size,
                         mod_stats, fct = "rbind")
  pred_data <- cbind(x = x_seq, apply(pred_out, 2, estimates_summary) %>%
                                  t %>%
                                  data.frame)
  list(data = pred_data, posterior = pred_out)
}

#' rhat.bayesmanecfit
#'
#' @param object An object of class \code{\link{bayesmanecfit}} as
#' returned by \code{\link{bnec}}.
#' @param ... Unused.
#' @param rhat_cutoff A numeric vector indicating the rhat criteria used to
#' test for model convergence.
#'
#' @return A list containing a vector or rhat values as returned for a brm fit
#' for each parameter, for each of the fitted models.
#'
#' @importFrom brms rhat
#'
#' @export
rhat.bayesmanecfit <- function(object, rhat_cutoff = 1.05, ... ) {
  rhat_vals <- lapply(object$mod_fits, function(x) rhat(x$fit))
  check <- lapply(rhat_vals, function(x, rhat_cutoff) max(x > rhat_cutoff),
                  rhat_cutoff)
  failed <- names(rhat_vals)[check == 1]
  if (length(failed) == length(rhat_vals)) {
    message(paste("All models failed the rhat_cutoff of", rhat_cutoff))
  }
  list(rhat_vals = rhat_vals, failed = failed)
}

#' summary.bayesmanecfit
#'
#' @param object An object of class \code{\link{bayesmanecfit}} as
#' returned by \code{\link{bnec}}.
#' @param ecx Should summary EC values be calculated? Defaults to FALSE.
#' @param ecx_vals EC targets (between 1 and 99). Only relevant if ecx = TRUE.
#' If no value is specified by the user, returns calculations for EC10, EC50,
#' and EC90.
#' @param ... Unused.
#'
#' @return A list containing a summary of the model fit as returned a
#' brmsfit for each model.
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map
#' @export
summary.bayesmanecfit <- function(object, ..., ecx = FALSE,
                                  ecx_vals = c(10, 50, 90)) {
  x <- object
  ecs <- NULL
  if (ecx) {
    message("ECX calculation takes a few seconds per model, calculating...\n")
    ecs <- list()
    for (i in seq_along(ecx_vals)) {
      ecs[[i]] <- ecx(x, ecx_val = ecx_vals[i])
    }
    names(ecs) <- paste0("ECx (", ecx_vals, "%) estimate:")
  }
  ecx_mods <- NULL
  if (any(x$success_models %in% mod_groups$ecx)) {
    ecx_mods <- x$success_models[x$success_models %in% mod_groups$ecx]
  }
  out <- list(
    models = x$success_models,
    family = x$mod_fits[[1]]$fit$family$family,
    sample_size = x$sample_size,
    mod_weights = clean_mod_weights(x),
    mod_weights_method = class(x$mod_stats$wi),
    ecx_mods = ecx_mods,
    nec_vals = clean_nec_vals(x),
    ecs = ecs,
    rhat_issues = map(x$mod_fits, "fit") %>%
      map(has_r_hat_warnings)
  )
  allot_class(out, "manecsummary")
}

#' print.manecsummary
#'
#' @param x An object of class \code{\link{manecsummary}} as
#' returned by \code{\link{summary.bayesmanecfit}}.
#' @param ... Unused.
#'
#' @return A list containing a summary of model features and statistics.
#'
#' @export
print.manecsummary <- function(x, ...) {
  cat("Object of class bayesmanecfit containing the following",
      " non-linear models:\n",
      paste0("  -  ", x$models, collapse = "\n"), sep = "")
  cat("\n\n")
  cat("Distribution family:", x$family)
  cat("\n")
  cat("Number of posterior draws per model: ", x$sample_size)
  cat("\n\n")
  cat("Model weights (Method: ", x$mod_weights_method, "):\n", sep = "")
  print_mat(x$mod_weights)
  cat("\n\n")
  cat("Summary of weighted NEC posterior estimates:\n")
  if (!is.null(x$ecx_mods)) {
    cat("NB: Model set contains the ECX models: ",
        paste0(x$ecx_mods, collapse = ";"),
        "; weighted NEC estimates include NSEC surrogates for NEC\n", sep = "")
  }
  print_mat(x$nec_vals)
  cat("\n\n")
  if (!is.null(x$ecs)) {
    for (i in seq_along(x$ecs)) {
      nice_ecx_out(x$ecs[[i]], names(x$ecs)[i])
      "\n\n"
    }
  }
  with_issues <- names(x$rhat_issues[unlist(x$rhat_issues)])
  if (length(with_issues) > 0) {
      warning("The following model had Rhats > 1.05 (no convergence):\n",
              paste0("  -  ", with_issues, collapse = "\n"), "\n",
              "Consider dropping them (see ?amend)\n", sep = "")
  }
  invisible(x)
}

#' print.bayesmanecfit
#'
#' @param x An object of class \code{\link{bayesmanecfit}} as
#' returned by \code{\link{bnec}}.
#' @param ... Further arguments to function summary.
#'
#' @return A list containing a summary of the model fit as returned a
#' brmsfit for each model.
#'
#' @export
print.bayesmanecfit <- function(x, ...) {
  print(summary(x, ...))
}
