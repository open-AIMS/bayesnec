#' Checking chain convergence
#'
#' Plots HMC chains for a \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} model fit as returned by \code{\link{bnec}}.
#'
#' @name check_chains
#' @order 1
#'
#' @param x An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} as returned by \code{\link{bnec}}.
#' @param ... Unused.
#'
#' @return No return value, generates a plot or writes a pdf to file.
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' check_chains(manec_example)
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' check_chains(nec4param)
#' }
#' @export
check_chains <- function(x, ...) {
  UseMethod("check_chains")
}


#' @rdname check_chains
#' @order 2
#'
#' @method check_chains bayesnecfit
#'
#' @inherit check_chains description return examples
#'
#' @importFrom brms fixef as_draws_array
#' @importFrom stats acf
#' @importFrom graphics axis lines mtext
#'
#' @export
check_chains.bayesnecfit <- function(x, ...) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  params <- gsub("_Intercept", "", rownames(fixef(x$fit)))
  a_params <- paste0("b_", rownames(fixef(x$fit)))
  sims_array <- as_draws_array(x$fit, variable = a_params)
  num_chains <- ncol(sims_array[, , 1])
  par(mfrow = c(length(params), 2), mar = c(0, 5, 0.5, 0.5),
      oma = c(4, 2, 2, 0))
  for (i in seq_len(length(params))) {
    x1 <- as.vector(sims_array[, , i])
    chain_id <- rep(seq_len(num_chains), each = nrow(sims_array[, , i]))
    num_lags <- length(acf(sims_array[, , i][, 1, 1], plot = FALSE)$lag)
    
    # plot the acf
    plot(seq_len(num_lags), rep(NA, num_lags),
         xaxt = "n", ylim = c(0, 1), xlab = "",
         ylab = "", main = "")
    if (i == length(params)) {
      axis(side = 1)
      mtext("lag", side=1, line = 2)
    }
    for (j in seq_len(num_chains)) {
      acf_j <- acf(sims_array[, , i][, j, 1], plot = FALSE)
      lines(acf_j$lag, acf_j$acf, col = j)
    }
    # plot the chains
    plot(seq_len(nrow(sims_array[, , i])), rep(NA, nrow(sims_array[, , i])),
         xaxt = "n", ylim = range(x1), main = "", xlab = "",
         ylab = params[i])
    if (i == length(params)) {
      axis(side = 1)
      mtext("iteration", side=1, line =2)     
    }
    for (k in seq_len(num_chains)) {
      lines(seq_len(nrow(sims_array[, , i])),
            x1[which(chain_id == k)], col = k)
    } 
    
  }
  mtext("correlation", side = 2, outer = TRUE, line = -2)   
  mtext(x$model, side = 3, outer = TRUE)
}

#' @rdname check_chains
#' @order 3
#'
#' @param filename An optional \code{\link[base]{character}} vector to be used
#' as a pdf filename in the case of a \code{\link{bayesmanecfit}}. Any non
#' empty character string will indicate the user wants to save the plots.
#'
#' @method check_chains bayesmanecfit
#'
#' @inherit check_chains description return examples
#'
#' @importFrom grDevices pdf dev.off
#' @importFrom chk chk_character
#'
#' @export
check_chains.bayesmanecfit <- function(x, filename = NA, ...) {
  if (!is.na(filename)) {
    chk_character(filename)
  }
  if (!is.na(filename)) {
    pdf(file = paste(filename, ".pdf", sep = ""), onefile = TRUE)
  }
  for (m in seq_len(length(x$mod_fits))) {
    check_chains(x = pull_out(x, model = names(x$mod_fits)[m]))
  }
  if (!is.na(filename)) {
    dev.off()
    message("Chain plots saved to file ", filename, ".pdf")
  }
}
