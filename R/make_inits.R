#' make_inits
#'
#' Creates list of initialisation values
#'
#' @param priors an object of class "brmsprior" from package \pkg{brms}.
#' @param chains Number of chains to be passed to brms model.
#'
#' @seealso \code{\link{bnec}}
#' @return A \code{\link[base]{list}} containing the initialisation values.
make_inits <- function(priors, chains) {
  set.seed(10)
  fcts <- c(gamma = rgamma,
            normal = rnorm,
            beta = rbeta,
            uniform = runif)
  priors <- as.data.frame(priors)
  priors <- priors[priors$prior != "", ]
  par_names <- character(length = nrow(priors))
  for (j in seq_along(par_names)) {
    sep <- ifelse(priors$class[j] == "b", "_", "")
    par_names[j] <- paste(priors$class[j],
                          priors$nlpar[j],
                          sep = sep)
  }
  out <- vector(mode = "list", length = chains)
  for (i in seq_along(out)) {
    out[[i]] <- vector(mode = "list", length = nrow(priors))
    names(out[[i]]) <- par_names
    for (j in seq_len(nrow(priors))) {
      bits <- strsplit(priors$prior[j], "[^[:alnum:]\\-\\.\\s]")[[1]]
      fct_i <- bits[1]
      v1 <- as.numeric(bits[2])
      v2 <- as.numeric(bits[4])
      out[[i]][[j]] <- fcts[[fct_i]](1, v1, v2)
      if (priors$class[j] == "b") {
        dim(out[[i]][[j]]) <- 1
      }
    }
  }
  out
}
