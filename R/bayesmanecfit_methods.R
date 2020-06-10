#' plot.bayesmanecfit
#'
#' Generates a plot of a fitted "bayesmanecfit" model, as returned by \code{\link{bnec}}.
#'
#' @inheritParams plot.bayesnecfit
#'
#' @param all_models A \code{\link[base]{logical}} value indicating if all models in the
#' model set should be plotted simultaneously, or if a model average plot should be returned.
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
                               all_models = TRUE) {
  if (all_models) {
    mod_fits <- x$mod_fits
    par(mfrow = c(ceiling(length(mod_fits) / 2), 2),
        mar = c(1.5, 1.5, 1.5, 1.5), oma = c(3, 3, 0, 0))

    for (m in seq_along(mod_fits)) {
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
      legend("top", legend = names(mod_fits[m]), bty = "n")
    }
  } else {
    plot.bayesnecfit(x = x, CI = CI, add_nec = add_nec,
                     position_legend = position_legend,
                     add_ec10 = add_ec10,
                     xform = xform, lxform = lxform,
                     jitter_x = jitter_x, jitter_y = jitter_y,
                     ylab = ylab, xlab = xlab,
                     xticks = xticks, ...)
  }
}
