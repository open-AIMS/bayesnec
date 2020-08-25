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

#' make_good_inits
#'
#' Creates list of initialisation values that generate
#' data within the natural range of data
#'
#' @inheritParams bnec
#' 
#' @param x A \code{\link[base]{numeric}} vector containing the x predictor.
#' @param y A \code{\link[base]{numeric}} vector containing the y response.
#' @param n_trials A \code{\link[base]{numeric}} vector indicating
#' how many attempts the function should run before giving up.
#' @param ... Additional arguments to \code{\link{make_inits}}.
#'
#' @seealso \code{\link{make_inits}}
#' @return A \code{\link[base]{list}} containing the initialisation values.
make_good_inits <- function(model, x, y, n_trials = 1e3, ...) {
  limits <- range(y, na.rm = TRUE)
  pred_fct <- get(paste0("pred_", model))
  fct_args <- names(unlist(as.list(args(pred_fct))))
  fct_args <- setdiff(fct_args, "x")
  inits <- make_inits(...)
  init_ranges <- lapply(inits, get_init_ranges,
                        x, pred_fct, fct_args)
  are_good <- all(sapply(init_ranges, check_limits, limits))
  n <- 1
  while (!are_good & n <= n_trials) {
    inits <- make_inits(...)
    init_ranges <- lapply(inits, get_init_ranges,
                          x, pred_fct, fct_args)
    are_good <- all(sapply(init_ranges,
                           check_limits,
                           limits))
    n <- n + 1
  }
  inits
}
