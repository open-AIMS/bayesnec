#' plot.bayesnecfit
#'
#' Generates a plot of a fitted \code{\link{bayesnecfit}} model, as returned by
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
#' @param force_x A \code{\link[base]{logical}} value indicating if the argument
#' \code{xform} should be forced on the predictor values. This is useful when
#' the user transforms the predictor beforehand
#' (e.g. when using a non-standard base function).
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
#' @importFrom stats quantile model.frame
#' @importFrom grDevices adjustcolor
plot.bayesnecfit <- function(x, ..., CI = TRUE, add_nec = TRUE,
                             position_legend = "topright", add_ec10 = FALSE,
                             xform = NA, lxform = NA, force_x = FALSE,
                             jitter_x = FALSE, jitter_y = FALSE,
                             ylab = "Response", xlab = "Predictor",
                             xticks = NA) {
  family <- x$fit$family$family
  custom_name <- check_custom_name(x$fit$family)
  mod_dat <- model.frame(x$bayesnecformula, data = x$fit$data)
  y_var <- attr(mod_dat, "bnec_pop")[["y_var"]]
  x_var <- attr(mod_dat, "bnec_pop")[["x_var"]]
  if (family == "binomial" | custom_name == "beta_binomial2") {
    trials_var <- attr(mod_dat, "bnec_pop")[["trials_var"]]
    y_dat <- x$fit$data[[y_var]] / x$fit$data[[trials_var]]
  } else {
    y_dat <- x$fit$data[[y_var]]
  }
  ec10 <- c(NA, NA, NA)
  if (add_ec10 & family != "gaussian") {
    ec10 <- ecx(x)
  }
  if (add_ec10 & family == "gaussian") {
    ec10 <- ecx(x, type = "relative")
  }
  if (inherits(xform, "function")) {
    x_dat <- x$fit$data[[x_var]]
    x_vec <- x$pred_vals$data$x
    if (force_x) {
      x_dat <- xform(x_dat)
      x_vec <- xform(x_vec)
    }
    nec <- xform(x$nec)
    ec10 <- xform(ec10)
  } else {
    x_dat <- x$fit$data[[x_var]]
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
#' @param precision A \code{\link[base]{numeric}} vector of length 1 indicating
#' the number of x values over which to predict values.
#' @param x_range A \code{\link[base]{numeric}} vector of length 2 indicating
#' the range of x values over which to make predictions.
#'
#' @return A \code{\link[base]{list}} containing two elements: a
#' \code{\link[base]{data.frame}} with predictor x and fitted y values plus
#' lower and upper credible intervals; a \code{\link[base]{matrix}} of M x N,
#' with M being the number of posterior draws and N being the number of
#' observations in the input data.
#'
#' @importFrom brms posterior_epred
#'
#' @export
predict.bayesnecfit <- function(object, ..., precision = 100, x_range = NA) {
  data <- model.frame(object$bayesnecformula, data = object$fit$data)
  x_var <- attr(data, "bnec_pop")[["x_var"]]
  fit <- object$fit
  x <- fit$data[[x_var]]
  if (any(is.na(x_range))) {
    x_seq <- seq(min(x), max(x), length = precision)
  } else {
    x_seq <- seq(min(x_range), max(x_range), length = precision)
  }
  new_dat <- data.frame(x_seq)
  names(new_dat) <- x_var
  fam_tag <- fit$family$family
  custom_name <- check_custom_name(fit$family)
  if (fam_tag == "binomial" || custom_name == "beta_binomial2") {
    trials_var <- attr(data, "bnec_pop")[["trials_var"]]
    new_dat[[trials_var]] <- 1
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
#' @return A named \code{\link[base]{numeric}} vector containing Rhat values as
#' returned for a \code{\link[brms]{brmsfit}} object for each of the estimated
#' parameters.
#'
#' @importFrom brms rhat
#'
#' @export
rhat.bayesnecfit <- function(object, ...) {
  rhat(object$fit)
}

#' summary.bayesnecfit
#'
#' @param object An object of class \code{\link{bayesnecfit}} as returned
#' by \code{\link{bnec}}.
#' @param ecx Should summary ECx values be calculated? Defaults to FALSE.
#' @param ecx_vals ECx targets (between 1 and 99). Only relevant if ecx = TRUE.
#' If no value is specified by the user, returns calculations for EC10, EC50,
#' and EC90.
#' @param ... Unused.
#'
#' @return A summary of the fitted model as returned for a
#' \code{\link[brms]{brmsfit}} object.
#'
#' @importFrom brms bayes_R2
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
    ecs = ecs,
    bayesr2 = bayes_R2(x$fit)
  )
  allot_class(out, "necsummary")
}

#' print.necsummary
#'
#' @param x An object of class \code{\link{necsummary}} as
#' returned by \code{\link{summary.bayesnecfit}}.
#' @param ... Unused.
#'
#' @return A \code{\link[base]{list}} containing a summary of model features
#' and statistics.
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
      if (i < length(x$ecs)) {
        cat("\n")
      }
    }
  }
  cat("\n\nBayesian R2 estimates:\n")
  print_mat(x$bayesr2)
  cat("\n\n")
  invisible(x)
}

#' print.bayesnecfit
#'
#' @param x An object of class \code{\link{bayesnecfit}} as
#' returned by \code{\link{bnec}}.
#' @param ... Further arguments to function summary.
#'
#' @return A \code{\link[base]{list}} containing a summary of the model fit as
#' returned for a \code{\link[brms]{brmsfit}} object.
#'
#' @export
print.bayesnecfit <- function(x, ...) {
  print(summary(x, ...))
}

#' formula.bayesnecfit
#'
#' @param x An object of class \code{\link{bayesnecfit}} as
#' returned by \code{\link{bnec}}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return An object of class \code{\link[stats]{formula}}.
#'
#' @importFrom stats formula
#' @export
formula.bayesnecfit <- function(x, ...) {
  formula(x$fit, ...)
}

#' model.frame.bayesnecfit
#'
#' @param formula An object of class \code{\link{bayesnecfit}} as
#' returned by \code{\link{bnec}}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A \code{\link[base]{data.frame}} containing the data used to fit
#' the model.
#'
#' @importFrom stats model.frame
#' @export
model.frame.bayesnecfit <- function(formula, ...) {
  model.frame(formula$fit, ...)
}
