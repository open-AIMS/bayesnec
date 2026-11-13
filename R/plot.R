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
#' # plot the model average and one equation of the set, a panel each
#' plot(manec_example, model = "nec4param")
#' # plot every equation of the set without the model average
#' plot(manec_example, add_ec10 = TRUE, model = manec_example$success_models,
#'      average = FALSE)
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
#' @param position_legend Where to draw the NEC or EC10 legend, in either the
#' keyword form or the coordinate form \code{\link[graphics]{legend}} accepts:
#' a single keyword out of \code{"left"}, \code{"topleft"}, \code{"top"},
#' \code{"topright"}, \code{"right"}, \code{"bottomright"},
#' \code{"bottom"} and \code{"bottomleft"}, or a \code{\link[base]{numeric}}
#' vector of length two giving the x and y coordinates of the legend in user
#' units. \code{legend}'s ninth keyword, \code{"center"}, is not accepted;
#' give the coordinates instead.
#' @param xform \code{NULL}, the default, or a function. Where \code{crf()}
#' does not transform the predictor inline, a function is applied to the x data
#' and to the annotated estimates. Where it does, the x data are drawn on the
#' recorded scale, and the annotation is the estimate \code{\link{nec}} or
#' \code{\link{ecx}} returns by default, which is on that scale. A function
#' other than \code{identity} is then applied to the estimates on the
#' transformed scale instead, as the estimators apply it.
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
                             xform = NULL, lxform = identity,
                             jitter_x = FALSE, jitter_y = FALSE,
                             ylab = "Response", xlab = "Predictor",
                             xticks = NA) {
  warn_stale_observed_display(x)
  warn_stale_ne_type(x)
  chk_lgl(CI)
  chk_lgl(add_nec)
  chk_lgl(add_ec10)
  check_xform(xform)
  if (!inherits(lxform, "function")) { 
    stop("lxform must be a function.")
  }
  chk_lgl(jitter_x)
  chk_lgl(jitter_y)
  chk_character(ylab)
  chk_character(xlab)
  legend_pos <- check_position_legend(position_legend)
  family <- x$fit$family$family
  mod_dat <- model.frame(x$bayesnecformula, data = x$fit$data)
  x_var <- attr(mod_dat, "bnec_pop")[["x_var"]]
  y_dat <- plotted_response(x$fit$data, attr(mod_dat, "bnec_pop"), family)
  bdat <- model.frame(x$bayesnecformula, data = x$fit$data, run_par_checks = TRUE)
  # On the recorded axis with no xform, draw what the estimators return.
  recorded <- plot_reads_recorded(bdat, xform)
  est_xform <- if (recorded) NULL else identity
  axis_xform <- if (is.null(xform)) identity else xform
  ec10 <- c(NA, NA, NA)
  if (add_ec10 & family != "gaussian") {
    ec10 <- ecx(x, xform = est_xform)
  }
  if (add_ec10 & family == "gaussian") {
    # "range", not "relative": a gaussian response can go negative, so the
    # annotation measures the span from the control to the minimum, not to
    # bot. "relative" would also fail for a bot-free equation.
    ec10 <- ecx(x, type = "range", xform = est_xform)
  }

  x_dat <- x$fit$data[[x_var]]
  x_vec <- x$pred_vals$data$x  
  
  # Ask about the predictor alone: a transformation on the response must not
  # stop xform being applied to the predictor axis.
  if (!pop_var_is_transformed(bdat, "x_var")) {
      x_dat <- axis_xform(x_dat)
      x_vec <- axis_xform(x_vec)
  }
      
  x_grid_raw <- x$pred_vals$data$x
  nec <- c(NA, NA, NA)
  if (add_nec) {
    nec <- if (recorded) {
      summary_ne_vals(x, NULL)
    } else {
      to_axis_scale(x$ne, bdat, x$bayesnecformula, x_grid_raw, axis_xform)
    }
  }
  if (add_ec10 && !recorded) {
    ec10 <- to_axis_scale(ec10, bdat, x$bayesnecformula, x_grid_raw,
                          axis_xform)
  }

  nec_tag <- summary(x, ecx = FALSE, xform = identity) |>
    (`[[`)("nec_vals") |>
    rownames() |>
    suppressWarnings() |>
    suppressMessages()
  draw_fit_panel(x_dat, y_dat, x_vec, x$pred_vals$data, nec, ec10, nec_tag,
                 ..., CI = CI, add_nec = add_nec, legend_pos = legend_pos,
                 add_ec10 = add_ec10, lxform = lxform, jitter_x = jitter_x,
                 jitter_y = jitter_y, ylab = ylab, xlab = xlab,
                 xticks = xticks)
}

#' @rdname plot
#' @order 3
#'
#' @param model \code{NULL} (the default), or a \code{\link[base]{character}}
#' vector naming equations of the model set. Each named equation is plotted in
#' a panel of its own, which shows the same curve and annotations as
#' \code{plot()} gives for the \code{\link{bayesnecfit}} that
#' \code{\link{pull_out}} returns for it. A name that is not an equation of
#' the set is refused.
#' @param average A \code{\link[base]{logical}} value indicating if the model
#' averaged predictions should be plotted. Defaults to \code{TRUE}. Where
#' \code{model} is also given, the model average takes the first panel and the
#' named equations follow in the order given. \code{average = FALSE} requires
#' \code{model}.
#' @param all_models Deprecated, and to be removed in a later release; use
#' \code{model} and \code{average}. \code{all_models = TRUE} is
#' \code{model = x$success_models, average = FALSE}, and
#' \code{all_models = FALSE} is the default. Supplying it gives a warning, and
#' supplying it together with \code{model} or \code{average} is an error.
#'
#' @method plot bayesmanecfit
#' @inherit plot description return examples
#' @importFrom graphics par plot mtext legend
#' @importFrom chk chk_lgl chk_character
#' @export
plot.bayesmanecfit <- function(x, ..., CI = TRUE, add_nec = TRUE,
                               position_legend = "topright", add_ec10 = FALSE,
                               xform = NULL, lxform = identity,
                               jitter_x = FALSE, jitter_y = FALSE,
                               ylab = "Response", xlab = "Predictor",
                               xticks = NA, model = NULL, average = TRUE,
                               all_models = NULL) {
  warn_stale_observed_display(x)
  warn_stale_ne_type(x)
  chk_lgl(CI)
  chk_lgl(add_nec)
  chk_lgl(add_ec10)
  check_xform(xform)
  if (!inherits(lxform, "function")) { 
    stop("lxform must be a function.")
  }
  chk_lgl(jitter_x)
  chk_lgl(jitter_y)
  chk_character(ylab)
  chk_character(xlab)
  shown <- resolve_model_average(
    x, model, average, all_models,
    new_supplied = !missing(model) || !missing(average)
  )
  legend_pos <- check_position_legend(position_legend)
  if (is.null(shown$model)) {
    plot_manec_average(x, ..., CI = CI, add_nec = add_nec,
                       legend_pos = legend_pos, add_ec10 = add_ec10,
                       xform = xform, lxform = lxform, jitter_x = jitter_x,
                       jitter_y = jitter_y, ylab = ylab, xlab = xlab,
                       xticks = xticks)
  } else {
    n_panels <- length(shown$model) + shown$average
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    # Two columns, except that a single panel takes the whole device.
    par(mfrow = c(ceiling(n_panels / 2), min(n_panels, 2)),
        mar = c(1.5, 1.5, 1.5, 1.5), oma = c(3, 3, 0, 0))
    if (shown$average) {
      plot_manec_average(x, ..., CI = CI, add_nec = add_nec,
                         legend_pos = legend_pos, add_ec10 = add_ec10,
                         xform = xform, lxform = lxform, jitter_x = jitter_x,
                         jitter_y = jitter_y, ylab = "", xlab = "",
                         xticks = xticks)
      mtext(xlab, side = 1, outer = TRUE, line = 2)
      mtext(ylab, side = 2, outer = TRUE, line = 2)
      legend("bottomleft", legend = manec_average_label, bty = "n")
    }
    for (m in shown$model) {
      fit_m <- pull_out(x, model = m) |>
        suppressWarnings() |>
        suppressMessages()
      plot(x = fit_m, CI = CI, add_nec = add_nec,
           position_legend = position_legend, add_ec10 = add_ec10,
           xform = xform, lxform = lxform,
           jitter_x = jitter_x, jitter_y = jitter_y, ylab = "", xlab = "",
           xticks = xticks, ...)
      mtext(xlab, side = 1, outer = TRUE, line = 2)
      mtext(ylab, side = 2, outer = TRUE, line = 2)
      legend("bottomleft", legend = m, bty = "n")
    }
    invisible(NULL)
  }
}

#' The observed response on the scale of the fitted curve
#'
#' @param data The data the fit was made from.
#' @param bnec_pop The \code{"bnec_pop"} attribute of its model frame.
#' @param family The name of the fit's family.
#'
#' @return A \code{\link[base]{numeric}} vector: the response as a
#' proportion of its trials for a binomial or beta-binomial fit, divided by the
#' \code{rate()} denominator where there is one, and as recorded otherwise.
#'
#' @noRd
plotted_response <- function(data, bnec_pop, family) {
  y <- data[[bnec_pop[["y_var"]]]]
  rate_var <- unname(bnec_pop["rate_var"])
  if (family %in% c("binomial", "beta_binomial")) {
    y / data[[bnec_pop[["trials_var"]]]]
  } else if (!is.na(rate_var)) {
    # The prediction grid sets the denominator to 1, so the curve is a rate.
    y / data[[rate_var]]
  } else {
    y
  }
}

#' Draw the model-averaged predictions of a model set on the current device
#'
#' The body of the model-average plot, separated from
#' \code{plot.bayesmanecfit()} so that the same drawing serves both the
#' single model-average plot and the first panel of a plot that also shows
#' named equations. The arguments are those of \code{plot.bayesmanecfit()},
#' already validated there, with \code{legend_pos} the value returned by
#' \code{check_position_legend()}.
#'
#' @importFrom graphics plot axis lines abline legend
#' @importFrom stats model.frame
#' @importFrom grDevices adjustcolor
#'
#' @noRd
plot_manec_average <- function(x, ..., CI, add_nec, legend_pos, add_ec10,
                               xform, lxform, jitter_x, jitter_y, ylab, xlab,
                               xticks) {
  universal <- x$mod_fits[[1]]
  mod_dat <- universal$fit$data
  bdat <- model.frame(x$mod_fits[[1]]$bayesnecformula, data = mod_dat)
  x_var <- attr(bdat, "bnec_pop")[["x_var"]]
  family <- universal$fit$family$family
  y_dat <- plotted_response(mod_dat, attr(bdat, "bnec_pop"), family)
  # As in plot.bayesnecfit().
  recorded <- plot_reads_recorded(bdat, xform)
  est_xform <- if (recorded) NULL else identity
  axis_xform <- if (is.null(xform)) identity else xform
  ec10 <- c(NA, NA, NA)
  if (add_ec10 & family != "gaussian") {
    ec10 <- ecx(x, xform = est_xform)
  }
  if (add_ec10 & family == "gaussian") {
    # "range" rather than "relative", for the reasons given in
    # plot.bayesnecfit above.
    ec10 <- ecx(x, type = "range", xform = est_xform)
  }
  x_dat <- mod_dat[[x_var]]
  x_vec <- x$w_pred_vals$data$x
  if (!pop_var_is_transformed(bdat, "x_var")) {
    x_dat <- axis_xform(x_dat)
    x_vec <- axis_xform(x_vec)
  }
  x_grid_raw <- x$w_pred_vals$data$x
  manec_formula <- x$mod_fits[[1]]$bayesnecformula
  nec <- c(NA, NA, NA)
  if (add_nec) {
    nec <- if (recorded) {
      summary_ne_vals(x, NULL)
    } else {
      to_axis_scale(x$w_ne, bdat, manec_formula, x_grid_raw, axis_xform)
    }
  }
  if (add_ec10 && !recorded) {
    ec10 <- to_axis_scale(ec10, bdat, manec_formula, x_grid_raw, axis_xform)
  }
  nec_tag <- summary(x, ecx = FALSE, xform = identity) |>
    (`[[`)("nec_vals") |>
    rownames() |>
    suppressWarnings() |>
    suppressMessages()
  draw_fit_panel(x_dat, y_dat, x_vec, x$w_pred_vals$data, nec, ec10, nec_tag,
                 ..., CI = CI, add_nec = add_nec, legend_pos = legend_pos,
                 add_ec10 = add_ec10, lxform = lxform, jitter_x = jitter_x,
                 jitter_y = jitter_y, ylab = ylab, xlab = xlab,
                 xticks = xticks)
}

#' Draw one fitted curve, its data and its annotations on the current device
#'
#' The drawing shared by \code{plot.bayesnecfit()}, \code{plot_manec_average()}
#' and each panel of \code{plot.bayesnecjointfit()}. Every value arrives on the
#' axis scale; \code{lxform} applies to the tick labels and the legend only.
#'
#' @param x_dat,y_dat The observations.
#' @param x_vec The predictor values of the curve.
#' @param curve A \code{\link[base]{data.frame}} with columns
#' \code{Estimate}, \code{Q2.5} and \code{Q97.5}, one row per entry of
#' \code{x_vec}.
#' @param nec,ec10 The annotated estimates, each a vector of three, with a
#' \code{"censored_summary"} attribute where an entry is a bound.
#' @param nec_tag The label of the no-effect estimate, such as \code{"NEC"}.
#' @param ... Passed to \code{\link[graphics]{plot}}.
#' @param CI,add_nec,legend_pos,add_ec10,lxform,jitter_x,jitter_y,ylab,xlab,xticks
#' As validated by the calling method; \code{legend_pos} is the value
#' \code{check_position_legend()} returns.
#'
#' @return No return value; draws on the current device.
#'
#' @importFrom graphics plot axis lines abline legend
#' @importFrom grDevices adjustcolor
#'
#' @noRd
draw_fit_panel <- function(x_dat, y_dat, x_vec, curve, nec, ec10, nec_tag,
                           ..., CI, add_nec, legend_pos, add_ec10, lxform,
                           jitter_x, jitter_y, ylab, xlab, xticks) {
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
  # The calling method checked that lxform is a function. With the default
  # lxform = identity these are the values on the axis.
  x_labs <- signif(lxform(x_ticks), 2)
  axis(side = 1, at = x_ticks, labels = x_labs)
  # A decreasing lxform reverses the interval and its bound marks, so remap
  # the labels as to_axis_scale() does. Index by position: an lxform that is
  # not arithmetic may drop the names.
  nec_lab <- remap_summary(nec, lxform, x_vec)
  ec10_lab <- remap_summary(ec10, lxform, x_vec)
  legend_nec <- paste(nec_tag, ": ", bound_prefix(nec_lab, 1),
                      signif(nec_lab[[1]], 2),
                      " (", bound_prefix(nec_lab, 2),
                      signif(nec_lab[[2]], 2), "-",
                      bound_prefix(nec_lab, 3),
                      signif(nec_lab[[3]], 2), ")", sep = "")
  legend_ec10 <- paste("EC10: ", bound_prefix(ec10_lab, 1),
                       signif(ec10_lab[[1]], 2),
                       " (", bound_prefix(ec10_lab, 2),
                       signif(ec10_lab[[2]], 2), "-",
                       bound_prefix(ec10_lab, 3),
                       signif(ec10_lab[[3]], 2), ")", sep = "")
  if (CI) {
    lines(x_vec, curve$Q97.5, lty = 2)
    lines(x_vec, curve$Q2.5, lty = 2)
  }
  lines(x_vec, curve$Estimate)
  if (add_nec & !add_ec10) {
    abline(v = nec, col = "red", lty = c(1, 3, 3))
    legend(legend_pos$x, legend_pos$y, bty = "n",
           legend = legend_nec, lty = 1, col = "red")
  }
  if (add_ec10 & !add_nec) {
    abline(v = ec10, col = "red", lty = c(1, 3, 3))
    legend(legend_pos$x, legend_pos$y, bty = "n",
           legend = legend_ec10, lty = 1, col = "red")
  }
  if (add_ec10 & add_nec) {
    abline(v = nec, col = "red", lty = c(1, 3, 3))
    abline(v = ec10, col = "orange", lty = c(1, 3, 3))
    legend(legend_pos$x, legend_pos$y, bty = "n",
           legend = c(legend_nec, legend_ec10),
           lty = 1, col = c("red", "orange"))
  }
}

#' Validate and normalise the position_legend argument of the plot methods
#'
#' @param position_legend Either a single keyword out of the eight listed in
#' \code{?plot}, or a \code{\link[base]{numeric}} vector of length two giving
#' the x and y coordinates of the legend.
#'
#' @details Both forms are accepted because \code{legend} accepts both. The
#' keywords are \code{legend}'s nine less \code{"center"}.
#'
#' A numeric pair is returned as \code{x} and \code{y}: \code{legend()} reads
#' a length-two numeric \code{x} with \code{y = NULL} as the corners of a
#' rectangle, not as a point.
#'
#' @return A \code{\link[base]{list}} with elements \code{x} and \code{y},
#' suitable as the first two arguments of \code{\link[graphics]{legend}}.
#'
#' @noRd
check_position_legend <- function(position_legend) {
  legend_positions <- c("left", "topleft", "top", "topright", "right",
                        "bottomright", "bottom", "bottomleft")
  if (is.numeric(position_legend) && length(position_legend) == 2 &&
        all(is.finite(position_legend))) {
    return(list(x = position_legend[1], y = position_legend[2]))
  }
  if (is.character(position_legend) && length(position_legend) == 1 &&
        position_legend %in% legend_positions) {
    return(list(x = position_legend, y = NULL))
  }
  stop("position_legend must be one of ",
       paste0("\"", legend_positions, "\"", collapse = ", "),
       ", or a numeric vector of length two giving the x and y coordinates of",
       " the legend.", call. = FALSE)
}
