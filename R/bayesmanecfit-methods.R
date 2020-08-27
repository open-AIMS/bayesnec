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
                               all_models = FALSE) {
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
      legend("top", legend = names(mod_fits[m]), bty = "n")
    }
  } else {
    universal <- x$mod_fits[[1]]
    family <- universal$fit$family$family
    mod_dat <- universal$fit$data
    if (family == "binomial") {
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
      legend_nec <- paste("nec: ", signif(nec["Estimate"], 2),
                          " (", signif(nec["Q2.5"], 2), "-",
                          signif(nec["Q97.5"], 2), ")", sep = "")
      legend_ec10 <- paste("ec10: ", signif(ec10[1], 2),
                           " (", signif(ec10[2], 2), "-",
                           signif(ec10[3], 2), ")", sep = "")
    } else {
      x_labs <- signif(lxform(x_ticks), 2)
      axis(side = 1, at = x_ticks, labels = x_labs)
      legend_nec <- paste("nec: ", signif(lxform(nec["Estimate"]), 2),
                          " (", signif(lxform(nec["Q2.5"]), 2), "-",
                          signif(lxform(nec["Q97.5"]), 2), ")", sep = "")
      legend_ec10 <- paste("ec10: ", signif(lxform(ec10[1]), 2),
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

#' amend
#'
#' Modifies an existing bayesmanecfit object, for example, by adding or removing fitted models.
#' 
#' @inheritParams amend.bayesmanecfit
#' 
#' @return All successfully fitted "bayesmanecfit" model fits.
#' @export
amend <- function(object, ...) {
  UseMethod("amend", object)
}

#' amend.bayesmanecfit
#'
#' Modifies an existing bayesmanecfit object, for example, by adding or removing fitted models.
#' 
#' @param object An object of class "bayesmanecfit" output list, as returned by \code{\link{bnec}}.
#' @param drop A \code{\link[base]{character}} vector containing the names of model types you which to drop for the modified fit.
#' @param add A \code{\link[base]{character}} vector containing the names of model types to add to the modified fit.
#' 
#' @return All successfully fitted "bayesmanecfit" model fits.
#' @export
amend.bayesmanecfit <- function(object, drop, add, x_range = NA,
                                precision = 1000, sig_val = 0.01) {
  if (missing(drop) && missing(add)) {
    message("Nothing to amend, please specify a model to ",
            "either add or drop;\n",
            "Returning original model set")
    return(object)
  }
  model_set <- names(object$mod_fits)
  if (!missing(drop)) {
    model_set <- handle_set(model_set, drop = drop)
  }
  if (!missing(add)) {
    model_set <- handle_set(model_set, add = add)
  }
  if (is.logical(model_set)) {
    message("Returning original model set")
    return(object)
  }
  simdat <- extract_simdat(object$mod_fits[[1]])
  data <- object$mod_fits[[1]]$fit$data
  family <- object$mod_fits[[1]]$fit$family
  mod_fits <- vector(mode = "list", length = length(model_set))
  names(mod_fits) <- model_set

  for (m in seq_along(model_set)) {
    model <- model_set[m]
    mod_m <- try(object$mod_fits[[model]], silent = TRUE)
    if (!inherits(mod_m, "prebayesnecfit")) {
      fit_m <- try(
        fit_bayesnec(data = data,
                     family = family,
                     model = model,
                     skip_check = TRUE,
                     iter = simdat$iter,
                     thin = simdat$thin,
                     warmup = simdat$warmup,
                     inits = simdat$inits,
                     chains = simdat$chains),
        silent = TRUE)
      if (!inherits(fit_m, "try-error")) {
        mod_fits[[model]] <- fit_m
      } else {
        mod_fits[[model]] <- NA
      }
    } else {
      mod_fits[[m]] <- mod_m
    }
  }
  mod_fits <- expand_manec(mod_fits, x_range = x_range,
                           precision = precision, sig_val = sig_val)
  if (!inherits(mod_fits, "prebayesnecfit")) {
    allot_class(mod_fits, "bayesmanecfit")
  } else {
    mod_fits <- expand_nec(mod_fits, x_range = x_range,
                           precision = precision,
                           sig_val = sig_val)
    allot_class(mod_fits, "bayesnecfit")
  }
}
