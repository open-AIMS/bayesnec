#' make_inits
#'
#' Creates list of initialisation values
#'
#' @inheritParams bnec
#'
#' @param fct_args A \code{\link[base]{character}} string containing
#' the expected argument names to be used.
#' @param priors an object of class "brmsprior" from package \pkg{brms}.
#' @param chains Number of chains to be passed to brms model.
#'
#' @importFrom stats rgamma rnorm rbeta runif
#'
#' @seealso \code{\link{bnec}}
#' @return A \code{\link[base]{list}} containing the initialisation values.
make_inits <- function(model, fct_args, priors, chains) {
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
  check_args <- identical(sort(par_names), sort(fct_args))
  if (!check_args) {
    out_args <- gsub("^b_", "", fct_args)
    out_pars <- gsub("^b_", "", par_names)
    stop("In model ", model, ", user-specific parameter ",
         "prior names (",
         paste0(out_pars, collapse = ", "), ") do not ",
         "match expectation (",
         paste0(out_args, collapse = ", "),
         "). Consider ",
         "reconstructing your priors; check necessary ",
         "parameters with show_params(\"", model, "\")")
  }
  out <- vector(mode = "list", length = chains)
  for (i in seq_along(out)) {
    out[[i]] <- vector(mode = "list", length = nrow(priors))
    names(out[[i]]) <- par_names
    for (j in seq_len(nrow(priors))) {
      bits <- gsub("\\(|\\)", ",", priors$prior[j])
      bits <- strsplit(bits, ",", fixed = TRUE)[[1]]
      fct_i <- bits[1]
      v1 <- as.numeric(bits[2])
      v2 <- as.numeric(bits[3])
      out[[i]][[j]] <- fcts[[fct_i]](1, v1, v2)
      if (priors$bound[j] != "") {
        to_keep <- "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?"
        bounds <- regmatches(priors$bound[j],
                             gregexpr(to_keep, priors$bound[j]))[[1]]
        bounds <- as.numeric(bounds)
        if (length(bounds) == 2) {
          while (out[[i]][[j]] <= min(bounds) |
                   out[[i]][[j]] >= max(bounds)) {
            out[[i]][[j]] <- fcts[[fct_i]](1, v1, v2)
          }
        } else if (length(bounds) == 1) {
          bound_fct <- ifelse(grepl("lower", priors$bound[j]), `<=`, `>=`)
          while (bound_fct(out[[i]][[j]], bounds)) {
            out[[i]][[j]] <- fcts[[fct_i]](1, v1, v2)
          }
        }
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
make_good_inits <- function(model, x, y, n_trials = 1e5, ...) {
  limits <- range(y, na.rm = TRUE)
  pred_fct <- get(paste0("pred_", model))
  fct_args <- names(unlist(as.list(args(pred_fct))))
  fct_args <- setdiff(fct_args, "x")
  inits <- make_inits(model, fct_args, ...)
  init_ranges <- lapply(inits, get_init_ranges, x, pred_fct, fct_args)
  are_good <- all(sapply(init_ranges, check_limits, limits))
  n_t <- 1
  while (!are_good & n_t <= n_trials) {
    inits <- make_inits(model, fct_args, ...)
    init_ranges <- lapply(inits, get_init_ranges, x, pred_fct, fct_args)
    are_good <- all(sapply(init_ranges, check_limits, limits))
    n_t <- n_t + 1
  }
  if (!are_good) {
    message("bayesnec failed to find initial values within the",
            " range of the response. Using Stan's default",
            " initialisation process.")
    "random"
  } else {
    inits
  }
}
