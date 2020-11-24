#' check_chains
#'
#' Plots mcmc chains for a \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} model fit as returned by \code{\link{bnec}}
#'
#' @param x An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} as returned by \code{\link{bnec}}.
#' @param name Label to add outside of plot.
#' @param filename An optional character vector to be used as a pdf filename
#' in the case of a \code{\link{bayesmanecfit}}. Any non empty character
#' string will indicate the user wants to save the plots.
#'
#' @importFrom brms fixef posterior_samples
#' @importFrom stats acf
#' @importFrom graphics axis lines mtext
#' @importFrom grDevices pdf dev.off
#'
#' @export
check_chains <- function(x, name = "", filename = "") {
  if (class(x) == "bayesnecfit") {
    params <- gsub("_Intercept", "", rownames(fixef(x$fit)))
    sims_array <- posterior_samples(x$fit, pars = params, as.array = TRUE)
    num_chains <- ncol(sims_array[, , 1])
    
    par(mfrow = c(length(params), 2), mar = c(0, 5, 0.5, 0.5),
        oma = c(4, 0, 2, 0))
    for (i in seq_len(length(params))) {
      x1 <- as.vector(sims_array[, , i])
      chain_id <- rep(seq_len(num_chains), each = nrow(sims_array[, , i]))
      num_lags <- length(acf(sims_array[, , i][, 1], plot = FALSE)$lag)
      # plot the chains
      plot(seq_len(nrow(sims_array[, , i])), rep(NA, nrow(sims_array[, , i])),
           xaxt = "n", ylim = range(x1), main = "", xlab = "",
           ylab = params[i])
      if (i == length(params)) {
        axis(side = 1)
      }
      for (k in seq_len(num_chains)) {
        lines(seq_len(nrow(sims_array[, , i])),
              x1[which(chain_id == k)], col = k)
      }
      # plot the acf
      plot(seq_len(num_lags), rep(NA, num_lags),
           xaxt = "n", ylim = c(0, 1), xlab = "lag",
           ylab = "correlation", main = "")
      if (i == length(params)) {
        axis(side = 1)
      }
      for (j in seq_len(num_chains)) {
        acf_j <- acf(sims_array[, , i][, j], plot = FALSE)
        lines(acf_j$lag, acf_j$acf, col = j)
      }
    }
    mtext(name, side = 3, outer = TRUE)
  } else if (class(x) == "bayesmanecfit") {
    if (nchar(filename) > 0) {
      pdf(file = paste(filename, ".pdf", sep = ""), onefile = TRUE)
    }
    for (m in seq_len(length(x$mod_fits))) {
      x_m <- x$mod_fits[[m]]
      params <-  gsub("_Intercept", "", rownames(fixef(x_m$fit)))
      sims_array <- posterior_samples(x_m$fit, pars = params, as.array = TRUE)
      num_chains <- ncol(sims_array[, , 1])
      if (nchar(filename) == 0) {
        #
      }
      par(mfrow = c(length(params), 2), mar = c(0, 5, 0.5, 0.5),
          oma = c(4, 0, 2, 0))
      for (i in seq_len(length(params))) {
        x1 <- as.vector(sims_array[, , i])
        chain_id <- rep(seq_len(num_chains), each = nrow(sims_array[, , i]))
        num_lags <- length(acf(sims_array[, , i][, 1], plot = FALSE)$lag)
        # plot the chains
        plot(seq_len(nrow(sims_array[, , i])),
             rep(NA, nrow(sims_array[, , i])), xaxt = "n", ylim = range(x1),
             main = "", xlab = "", ylab = params[i])
        if (i == length(params)) {
          axis(side = 1)
        }
        for (k in 1:num_chains) {
          lines(seq_len(nrow(sims_array[, , i])), x1[which(chain_id == k)],
                col = k)
        }
        # plot the acf
        plot(seq_len(num_lags), rep(NA, num_lags), xaxt = "n",
             ylim = c(0, 1), xlab = "lag", ylab = "correlation", main = "")
        if (i == length(params)) {
          axis(side = 1)
        }
        for (j in 1:num_chains) {
          acf_j <- acf(sims_array[, , i][, j], plot = FALSE)
          lines(acf_j$lag, acf_j$acf, col = j)
        }
      }
      mtext(names(x$mod_fits)[m], side = 3, outer = TRUE)
    }
    if (nchar(filename) > 0) {
      dev.off()
      message(paste("Chain plots saved to file ", filename, ".pdf, in your working directory.", sep=""))
    }
  } else {
    stop("No option available for class ", class(x))
  }
}
