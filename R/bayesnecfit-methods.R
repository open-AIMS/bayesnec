#' plot.bayesnecfit
#'
#' Generates a plot of a fitted "bayesnecfit" model, as returned by
#' \code{\link{bnec}}.
#'
#' @param x An object of class \code{\link{bayesnecfit}} as returned by
#' \code{\link{bnec}}.
#' @param ... Additional arguments to \code{\link[graphics]{plot}}.
#' @param CI A \code{\link[base]{logical}} value indicating if credibility
#' intervals on the model fit should be plotted, calculated as the upper and
#' lower bounds of the individual predicted values from all posterior samples.
#' @param add_nec A \code{\link[base]{logical}} value indicating if the
#' estimated NEC value and 95% credible intervals should be added to the plot.
#' @param add_ec10 A \code{\link[base]{logical}} value indicating if an
#' estimated EC10 value and 95% credible intervals should be added to the plot.
#' @param position_legend A \code{\link[base]{numeric}} vector indicating the
#' location of the NEC or EC10 legend, as per a call to legend.
#' @param xform A function to be applied as a transformation of the x data.
#' @param lxform A function to be applied as a transformation only to axis
#' labels and the annotated NEC / EC10 values.
#' @param jitter_x A \code{\link[base]{logical}} value indicating if the x
#' data points on the plot should be jittered.
#' @param jitter_y A \code{\link[base]{logical}} value indicating if the y
#' data points on the plot should be jittered.
#' @param xlab A \code{\link[base]{character}} vector to use for the x-axis
#' label.
#' @param ylab A \code{\link[base]{character}} vector to use for the y-axis
#' label.
#' @param xticks A numeric vector indicate where to place the tick marks of
#' the x-axis.
#' @export
#' @return a plot of the fitted model
#' @importFrom graphics plot axis lines abline legend
#' @importFrom stats quantile
#' @importFrom grDevices adjustcolor
plot.bayesnecfit <- function(x, ..., CI = TRUE, add_nec = TRUE,
                             position_legend = "topright",
                             add_ec10 = FALSE, xform = NA,
                             lxform = NA, jitter_x = FALSE,
                             jitter_y = FALSE, ylab = "response",
                             xlab = "concentration", xticks = NA) {
  family <- x$fit$family$family
  custom_name <- check_custom_name(x$fit$family)
  if (family == "binomial" | custom_name == "beta_binomial2") {
    y_dat <- x$fit$data$y / x$fit$data$trials
  } else {
    y_dat <- x$fit$data$y
  }
  ec10 <- c(NA, NA, NA)
  if (add_ec10 & family != "gaussian") {
    ec10 <- ecx(x)
  }
  if (add_ec10 & family == "gaussian") {
    ec10 <- ecx(x, type = "relative")
  }
  if (inherits(xform, "function")) {
    x_dat <- xform(x$fit$data$x)
    nec <- xform(x$nec)
    x_vec <- xform(x$pred_vals$data$x)
    ec10 <- xform(ec10)
  } else {
    x_dat <- x$fit$data$x
    nec <- x$nec
    x_vec <- x$pred_vals$data$x
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
    lines(x_vec, x$pred_vals$data$Q97.5, lty = 2)
    lines(x_vec, x$pred_vals$data$Q2.5, lty = 2)
  }
  lines(x_vec, x$pred_vals$data$Estimate)
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

#' predict.bayesnecfit
#'
#' @param object An object of class \code{\link{bayesnecfit}} as returned
#' by \code{\link{bnec}}.
#'
#' @param ... Unused.
#' @param precision the number of x values over which to predict values.
#' @param x_range The range of x values over which to make predictions.
#'
#' @return A list containing x and fitted y, with up and lw values
#'
#' @importFrom brms posterior_epred
#'
#' @export
predict.bayesnecfit <- function(object, ..., precision = 100,
                                x_range = NA) {
  mod_dat <- object$fit$data
  fit <- object$fit
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
  pred_out <- brms::posterior_epred(fit, newdata = new_dat,
                                    re_formula = NA)
  pred_d <- cbind(x = x_seq, data.frame(t(apply(pred_out, 2,
                                                estimates_summary))))
  list(data = pred_d, posterior = pred_out)
}

#' rhat.bayesnecfit
#'
#' @param object An object of class \code{\link{bayesnecfit}} as returned
#' by \code{\link{bnec}}.
#' @param ... Unused.
#'
#' @return A named vector containing rhat values as returned for a brmsfit
#' for each of the estimated parameters.
#'
#' @importFrom brms rhat
#'
#' @export
rhat.bayesnecfit <- function(object, ... ) {
  rhat(object$fit)
}

#' summary.bayesnecfit
#'
#' @param object An object of class \code{\link{bayesnecfit}} as returned
#' by \code{\link{bnec}}.
#' @param ecx Should summary EC values be calculated? Defaults to FALSE.
#' @param ecx_vals EC targets (between 1 and 99). Only relevant if ecx = TRUE.
#' If no value is specified by the user, returns calculations for EC10, EC50,
#' and EC90.
#' @param ... Unused.
#'
#' @return A summary of the fitted model as returned for a brmsfit
#'
#' @export
summary.bayesnecfit <- function(object, ..., ecx = FALSE,
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
  out <- list(
    brmssummary = summary(x$fit, robust = TRUE),
    model = x$model,
    is_ecx = x$model %in% mod_groups$ecx,
    ecs = ecs
  )
  allot_class(out, "necsummary")
}

#' print.necsummary
#'
#' @param x An object of class \code{\link{necsummary}} as
#' returned by \code{\link{summary.bayesnecfit}}.
#' @param ... Unused.
#'
#' @return A list containing a summary of model features and statistics.
#'
#' @export
print.necsummary <- function(x, ...) {
  cat("Object of class bayesnecfit containing the following",
      " non-linear model: ", x$model, "\n\n", sep = "")
  print(x$brmssummary)

  if (x$is_ecx) {
    cat("\nNB: Model ", x$model, " is an ECX model and so ",
        "the NEC estimate is an NSEC surrogate.\n", sep = "")
  }
  if (!is.null(x$ecs)) {
    cat("\n\n")
    for (i in seq_along(x$ecs)) {
      nice_ecx_out(x$ecs[[i]], names(x$ecs)[i])
      "\n\n"
    }
  }
  invisible(x)
}

#' print.bayesnecfit
#'
#' @param x An object of class \code{\link{bayesnecfit}} as
#' returned by \code{\link{bnec}}.
#' @param ... Further arguments to function summary.
#'
#' @return A list containing a summary of the model fit as returned a
#' brmsfit for each model.
#'
#' @export
print.bayesnecfit <- function(x, ...) {
  print(summary(x, ...))
}
