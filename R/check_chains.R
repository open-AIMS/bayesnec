#' check_chains
#' 
#' Plots mcmc chains for a \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}} model fit as returned by \code{\link{bnec}}
#' 
#' @param x An object of class \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}} as returned by \code{\link{bnec}}.
#' @param filename An optional character vector to be used as a pdf filename in the case of a \code{\link{bayesmanecfit}}. Any non empty character string will indicate the user wants to save the plots.
#' @export
check_chains <- function(x, name = "", filename = "") {
  if (class(x) == "bayesnecfit") {
    params <-gsub("_Intercept", "", rownames(fixef(x$fit)))
    sims.array <- posterior_samples(x$fit, pars = params, as.array = TRUE)

    num.chains <- ncol(sims.array[, , 1])
    
    par(mfrow = c(length(params), 2), mar = c(0, 5, 0.5, 0.5), oma = c(4, 0, 2, 0))
    for (i in 1:length(params)) {
      x1 <- as.vector(sims.array[, , i])
      chain.id <- rep(1:num.chains, each = nrow(sims.array[, , i]))
      num.lags <- length(acf(sims.array[, , i][, 1], plot = FALSE)$lag)
      
      # plot the chains
      plot(1:nrow(sims.array[, , i]), rep(NA, nrow(sims.array[, , i])),
           xaxt = "n",
           ylim = range(x1), main = "", xlab = "", ylab = params[i]
      )
      if (i == length(params)) {
        axis(side = 1)
      }
      for (k in 1:num.chains) {
        lines(1:nrow(sims.array[, , i]), x1[which(chain.id == k)], col = k)
      }
      
      # plot the acf
      plot(1:num.lags, rep(NA, num.lags),
           xaxt = "n",
           ylim = c(0, 1), xlab = "lag", ylab = "correlation", main = ""
      )
      if (i == length(params)) {
        axis(side = 1)
      }
      for (j in 1:num.chains) {
        acf.j <- acf(sims.array[, , i][, j], plot = FALSE)
        lines(acf.j$lag, acf.j$acf, col = j)
      }
    }
    mtext(name, side = 3, outer = T)
  } else if (class(x) == "bayesmanecfit") {
    if (nchar(filename)>0) {
      pdf(file = paste(filename, ".pdf", sep = ""), onefile = TRUE)
    }
    
    for (m in 1:length(x$mod_fits)) {
      x.m <- x$mod_fits[[m]]
      params <-  gsub("_Intercept", "", rownames(fixef(x.m$fit)))
      sims.array <- posterior_samples(x.m$fit, pars = params, as.array = TRUE)
      num.chains <- ncol(sims.array[, , 1])
      if (nchar(filename) == 0) {
        
      }
        
      par(mfrow = c(length(params), 2), mar = c(0, 5, 0.5, 0.5), oma = c(4, 0, 2, 0))
      for (i in 1:length(params)) {
        x1 <- as.vector(sims.array[, , i])
        chain.id <- rep(1:num.chains, each = nrow(sims.array[, , i]))
        num.lags <- length(acf(sims.array[, , i][, 1], plot = FALSE)$lag)
        
        # plot the chains
        plot(1:nrow(sims.array[, , i]), rep(NA, nrow(sims.array[, , i])),
             xaxt = "n",
             ylim = range(x1), main = "", xlab = "", ylab = params[i]
        )
        if (i == length(params)) {
          axis(side = 1)
        }
        for (k in 1:num.chains) {
          lines(1:nrow(sims.array[, , i]), x1[which(chain.id == k)], col = k)
        }
        
        # plot the acf
        plot(1:num.lags, rep(NA, num.lags),
             xaxt = "n",
             ylim = c(0, 1), xlab = "lag", ylab = "correlation", main = ""
        )
        if (i == length(params)) {
          axis(side = 1)
        }
        for (j in 1:num.chains) {
          acf.j <- acf(sims.array[, , i][, j], plot = FALSE)
          lines(acf.j$lag, acf.j$acf, col = j)
        }
      }
      mtext(names(x$mod_fits)[m], side = 3, outer = T)
    }
    if (nchar(filename)>0) {
      dev.off()
      message(paste("Chain plots saved to file ", filename, ".pdf, in your working directory.", sep=""))
    }
  
  } else {
    stop(paste("No option available for class", class(x)))
  }
}

