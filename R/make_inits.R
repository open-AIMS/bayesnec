#' make_inits
#'
#' Creates list of initialisation values
#'
#' @param priors an object of class "brmsprior" from package \pkg{brms}.
#' @param chains Number of chains to be passed to brms model.
#' @param stan_like Should initial values be drawn in a similar
#' fashion to how they are drawn by stan? Defaults to FALSE.
#'
#' @seealso \code{\link{bnec}}
#' @return A \code{\link[base]{list}} containing the initialisation values.
make_inits <- function(priors, chains, stan_like = FALSE) {
  fcts <- c(gamma = rgamma,
            normal = rnorm,
            beta = rbeta,
            uniform = runif)
  fcts_st <- c(gamma = function() runif(1, 0.01, 2),
               normal = function() runif(1, -2, 2),
               beta = function() runif(1, 0.01, 0.99),
               uniform = function() runif(1, -2, 2))

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
      if (stan_like) {
        out[[i]][[j]] <- fcts_st[[fct_i]]()
      } else {
        out[[i]][[j]] <- fcts[[fct_i]](1, v1, v2)
      }
      if (priors$class[j] == "b") {
        dim(out[[i]][[j]]) <- 1
      }
    }
  }
  out
}
