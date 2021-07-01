#' check_chains.default
#'
#' Plots mcmc chains for a \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} model fit as returned by \code{\link{bnec}}.
#'
#' @param x An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} as returned by \code{\link{bnec}}.
#' @param ... arguments used when class is \code{\link{bayesmanecfit}}.
#'
#' @importFrom brms fixef posterior_samples
#' @importFrom stats acf
#' @importFrom graphics axis lines mtext
#' 
#' @return No return value, generates a plot or writes a pdf to file.
#'
#' @examples
#' library(bayesnec)
#' data(manec_example)
#'
#' # print to device
#' check_chains(manec_example)
#'
#' @export
check_chains.default <- function(x, ...) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))      
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
  mtext(x$model, side = 3, outer = TRUE)
}

#' check_chains
#'
#' Plots mcmc chains for a \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} model fit as returned by \code{\link{bnec}}.
#'
#' @inheritParams check_chains.default
#'
#' @inherit check_chains.default return examples
#'
#' @export
check_chains <- function(x, ...) {
  UseMethod("check_chains")
}

#' check_chains.bayesnecfit
#'
#' Plots mcmc chains for a \code{\link{bayesnecfit}} model fit as returned
#' by \code{\link{bnec}}.
#'
#' @inheritParams check_chains.default
#'
#' @inherit check_chains.default return examples
#'
#' @export
check_chains.bayesnecfit <- function(x, ...) {
  check_chains.default(x, ...)
}

#' check_chains.bayesmanecfit
#'
#' Plots mcmc chains for a \code{\link{bayesnecfit}} model fit as returned
#' by \code{\link{bnec}}.
#'
#' @inheritParams check_chains.default
#'
#' @param filename An optional character vector to be used as a pdf filename
#' in the case of a \code{\link{bayesmanecfit}}. Any non empty character
#' string will indicate the user wants to save the plots.
#'
#' @importFrom grDevices pdf dev.off
#'
#' @inherit check_chains.default return examples
#'
#' @export
check_chains.bayesmanecfit <- function(x, ..., filename = NA) {
  if (!is.na(filename)) {
    pdf(file = paste(filename, ".pdf", sep = ""), onefile = TRUE)
  }
  for (m in seq_len(length(x$mod_fits))) {
    check_chains.default(x = x$mod_fits[[m]], ...)
  }
  if (!is.na(filename)) {
    dev.off()
    message("Chain plots saved to file ", filename, ".pdf")
  }
}
