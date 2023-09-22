#' Generates a plot for objects fitted by \code{\link{bnec}}
#'
#' Generates a plot for objects fitted by \code{\link{bnec}}.
#' \code{x} should be of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#'
#' @name plot
#' @order 1
#'
#' @param x An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#' @param ... Additional arguments to \code{\link[graphics]{plot}}.
#'
#' @return A \code{\link[graphics]{plot}} of the fitted model.
#'
#' @examples
#' \donttest{
#' library(bayesnec)
#' nec4param <- pull_out(manec_example, "nec4param")
#' # plot single models (bayesnecfit)
#' plot(nec4param)
#' plot(nec4param, add_nec = FALSE)
#' plot(nec4param, add_ec10 = TRUE)
#'
#' # plot model averaged predictions (bayesmanecfit)
#' plot(manec_example)
#' # plot all panels together
#' plot(manec_example, add_ec10 = TRUE, all_models = TRUE)
#' }
NULL

#' @rdname plot
#' @order 2
#'
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
#' 
#' @method plot bayesnecfit
#'
#' @inherit plot description return examples
#'
#' @importFrom graphics plot axis lines abline legend
#' @importFrom stats quantile model.frame
#' @importFrom grDevices adjustcolor
#' @importFrom chk chk_lgl chk_character
#'
#' @export
plot.bayesnecfit <- function(x, ..., CI = TRUE, add_nec = TRUE,
                             position_legend = "topright", add_ec10 = FALSE,
                             xform = identity, lxform = identity,
                             jitter_x = FALSE, jitter_y = FALSE,
                             ylab = "Response", xlab = "Predictor",
                             xticks = NA) {
  chk_lgl(CI)
  chk_lgl(add_nec)
  chk_lgl(add_ec10)
  if (!inherits(xform, "function")) {
    stop("xform must be a function.")
  }
  if (!inherits(lxform, "function")) { 
    stop("lxform must be a function.")
  }
  chk_lgl(jitter_x)
  chk_lgl(jitter_y)
  chk_character(ylab)
  chk_character(xlab)
  legend_positions <- c("left", "topleft", "top", "topright", 
                        "right", "bottomright", "bottom","bottomleft")
  if (length(na.omit(match(legend_positions, position_legend)))==0) {
    stop(paste("legend positions must be one of ", paste0(legend_positions, collapse = ", " )))
  }
  family <- x$fit$family$family
  custom_name <- check_custom_name(x$fit$family)
  mod_dat <- model.frame(x$bayesnecformula, data = x$fit$data)
  y_var <- attr(mod_dat, "bnec_pop")[["y_var"]]
  x_var <- attr(mod_dat, "bnec_pop")[["x_var"]]
  if (family == "binomial" | family == "beta_binomial") {
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

  bdat <- model.frame(x$bayesnecformula, data = x$fit$data, run_par_checks = TRUE)
  trans_vars <- find_transformations(bdat)
  x_dat <- x$fit$data[[x_var]]
  x_vec <- x$pred_vals$data$x  
  
  # if no transformations are applied via formula (including on trials),
  # use xform on axis
  if (length(trans_vars) == 0) {
      x_dat <- xform(x_dat)
      x_vec <- xform(x_vec)
  }
      
  ec10 <- xform(ec10)  
  nec <- xform(x$ne)

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
  nec_tag <- summary(x, ecx = FALSE) |>
    (`[[`)("nec_vals") |>
    rownames() |>
    suppressWarnings() |>
    suppressMessages()
  if (!inherits(lxform, "function")) {
    if (length(xticks) == 1) {
      axis(side = 1)
    } else {
      axis(side = 1, at = signif(xticks, 2))
    }
    legend_nec <- paste(nec_tag, ": ", signif(nec["Estimate"], 2),
                        " (", signif(nec["Q2.5"], 2), "-",
                        signif(nec["Q97.5"], 2), ")", sep = "")
    legend_ec10 <- paste("EC10: ", signif(ec10[1], 2),
                         " (", signif(ec10[2], 2), "-",
                         signif(ec10[3], 2), ")", sep = "")
  } else {
    x_labs <- signif(lxform(x_ticks), 2)
    axis(side = 1, at = x_ticks, labels = x_labs)
    legend_nec <- paste(nec_tag, ": ", signif(lxform(nec["Estimate"]), 2),
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

#' @rdname plot
#' @order 3
#'
#' @param all_models A \code{\link[base]{logical}} value indicating if all
#' models in the model set should be plotted simultaneously, or if a model
#' average plot should be returned.
#'
#' @method plot bayesmanecfit
#' @inherit plot description return examples
#' @importFrom graphics par plot mtext legend
#' @importFrom chk chk_lgl chk_character
#' @export
plot.bayesmanecfit <- function(x, ..., CI = TRUE, add_nec = TRUE,
                               position_legend = "topright", add_ec10 = FALSE,
                               xform = identity, lxform = identity,
                               jitter_x = FALSE, jitter_y = FALSE,
                               ylab = "Response", xlab = "Predictor",
                               xticks = NA, all_models = FALSE) {
  chk_lgl(CI)
  chk_lgl(add_nec)
  chk_lgl(add_ec10)
  if (!inherits(xform, "function")) {
    stop("xform must be a function.")
  }
  if (!inherits(lxform, "function")) { 
    stop("lxform must be a function.")
  }
  chk_lgl(jitter_x)
  chk_lgl(jitter_y)
  chk_character(ylab)
  chk_character(xlab)
  chk_lgl(all_models)
  legend_positions <- c("left", "topleft", "top", "topright", 
          "right", "bottomright", "bottom","bottomleft")
  if (length(na.omit(match(legend_positions, position_legend))) == 0) {
    stop("Legend positions must be one of ",
         paste0(legend_positions, collapse = ", " ), ".")
  }
  if (all_models) {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    mod_fits <- x$mod_fits
    par(mfrow = c(ceiling(length(mod_fits) / 2), 2),
        mar = c(1.5, 1.5, 1.5, 1.5), oma = c(3, 3, 0, 0))
    for (m in seq_along(mod_fits)) {
      mod_fits[[m]] <- pull_out(x, model = names(mod_fits)[m]) |>
        suppressWarnings() |>
        suppressMessages()
      plot(x = mod_fits[[m]], CI = CI, add_nec = add_nec,
           position_legend = position_legend, add_ec10 = add_ec10,
           xform = xform, lxform = lxform,
           jitter_x = jitter_x, jitter_y = jitter_y, ylab = "", xlab = "",
           xticks = xticks, ...)
      mtext(xlab, side = 1, outer = TRUE, line = 2)
      mtext(ylab, side = 2, outer = TRUE, line = 2)
      legend("bottomleft", legend = names(mod_fits[m]), bty = "n")
    }
  } else {
    universal <- x$mod_fits[[1]]
    mod_dat <- universal$fit$data
    bdat <- model.frame(x$mod_fits[[1]]$bayesnecformula, data = mod_dat)
    trans_vars <- find_transformations(bdat)
    y_var <- attr(bdat, "bnec_pop")[["y_var"]]
    x_var <- attr(bdat, "bnec_pop")[["x_var"]]
    family <- universal$fit$family$family
    custom_name <- check_custom_name(universal$fit$family)
    if (family == "binomial" | family == "beta_binomial") {
      trials_var <- attr(bdat, "bnec_pop")[["trials_var"]]
      y_dat <- mod_dat[[y_var]] / mod_dat[[trials_var]]
    } else {
      y_dat <- mod_dat[[y_var]]
    }
    ec10 <- c(NA, NA, NA)
    if (add_ec10 & family != "gaussian") {
      ec10 <- ecx(x)
    }
    if (add_ec10 & family == "gaussian") {
      ec10 <- ecx(x, type = "relative")
    }
    x_dat <- mod_dat[[x_var]]
    x_vec <- x$w_pred_vals$data$x
    if (length(trans_vars) == 0) {
      x_dat <- xform(x_dat)
      x_vec <- xform(x_vec)
    }
    nec <- xform(x$w_ne)
    ec10 <- xform(ec10)
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
    nec_tag <- summary(x, ecx = FALSE) |>
      (`[[`)("nec_vals") |>
      rownames() |>
      suppressWarnings() |>
      suppressMessages()
    if (!inherits(lxform, "function")) {
      if (length(xticks) == 1) {
        axis(side = 1)
      } else {
        axis(side = 1, at = signif(xticks, 2))
      }
      legend_nec <- paste(nec_tag, ": ", signif(nec["Estimate"], 2),
                          " (", signif(nec["Q2.5"], 2), "-",
                          signif(nec["Q97.5"], 2), ")", sep = "")
      legend_ec10 <- paste("EC10: ", signif(ec10[1], 2),
                           " (", signif(ec10[2], 2), "-",
                           signif(ec10[3], 2), ")", sep = "")
    } else {
      x_labs <- signif(lxform(x_ticks), 2)
      axis(side = 1, at = x_ticks, labels = x_labs)
      legend_nec <- paste(nec_tag, ": ", signif(lxform(nec["Estimate"]), 2),
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
