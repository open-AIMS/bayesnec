#' make_inits
#'
#' Creates list of initialisation values
#'
#' @inheritParams bnec
#'
#' @param fct_args A \code{\link[base]{character}} string containing
#' the expected argument names to be used.
#' @param priors an object of class \code{\link[brms]{brmsprior}} from package
#' \pkg{brms}.
#' @param chains Number of chains to be passed to \pkg{brms} model.
#'
#' @importFrom stats rgamma rnorm rbeta runif
#'
#' @seealso \code{\link{bnec}}
#' @return A \code{\link[base]{list}} containing the initialisation values.
#'
#' @noRd
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
      if (any(!is.na(priors[j, c("lb", "ub")]))) {
        n_bounds <- sum(!is.na(priors[j, c("lb", "ub")]))
        if (n_bounds == 2) {
          bounds <- as.numeric(priors[j, c("lb", "ub")])
          while (out[[i]][[j]] <= min(bounds) |
                   out[[i]][[j]] >= max(bounds)) {
            out[[i]][[j]] <- fcts[[fct_i]](1, v1, v2)
          }
        } else if (n_bounds == 1) {
          direction <- c("lb", "ub")[!is.na(priors[j, c("lb", "ub")])]
          bound_fct <- ifelse(direction == "lb", `<=`, `>=`)
          bounds <- as.numeric(priors[j, direction])
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

#' refine_inits
#'
#' For a single chain's init list that fails prediction checks, attempt to
#' fix it by re-drawing one parameter at a time while holding the others
#' fixed.  Targets \code{slope}, \code{d} and \code{beta} -- the parameters
#' most likely to push hormesis / sigmoidal predictions out of range.
#'
#' @param init A named \code{\link[base]{list}} of init values for one chain.
#' @param x Sorted predictor values.
#' @param pred_fct The prediction function for the model.
#' @param fct_args Parameter names expected by the prediction function.
#' @param limits A length-2 \code{\link[base]{numeric}} vector (response range).
#' @param priors A \code{\link[base]{data.frame}} of priors (already filtered
#'   to non-empty rows).
#' @param n_sub Maximum number of single-parameter re-draws per parameter.
#'
#' @return The (possibly improved) init list.
#' @noRd
refine_inits <- function(init, x, pred_fct, fct_args, limits,
                         priors, n_sub = 500) {
  fcts <- c(gamma = rgamma,
            normal = rnorm,
            beta = rbeta,
            uniform = runif)
  preds <- get_init_predictions(init, x, pred_fct, fct_args)
  if (check_init_predictions(preds, limits)) {
    return(init)
  }
  # refine_inits nudges a finite-but-out-of-range curve back into range by
  # re-drawing slope/d/beta. NaN predictions signal a structural problem
  # instead -- e.g. models that raise the predictor to a fractional power
  # (nechormepwr, nechorme4pwr, ecxsigm) return NaN wherever x < 0, for every
  # parameter draw. No re-draw fixes that, so bail early rather than burn the
  # full n_sub search on a hopeless case. We test NaN specifically rather than
  # all non-finite values: Inf from numerical overflow can sometimes be cured
  # by re-drawing a parameter smaller, so those are left for the search below.
  if (any(is.nan(preds))) {
    return(init)
  }
  # Identify tunable parameters (slope, d, beta -- those most likely
  # to push predictions out of range in hormesis / sigmoidal models).
  tunable <- intersect(names(init), c("b_slope", "b_d", "b_beta"))
  for (par in tunable) {
    pr_row <- which(priors$nlpar == gsub("^b_", "", par))
    if (length(pr_row) != 1) next
    bits <- gsub("\\(|\\)", ",", priors$prior[pr_row])
    bits <- strsplit(bits, ",", fixed = TRUE)[[1]]
    fct_i <- bits[1]
    v1 <- as.numeric(bits[2])
    v2 <- as.numeric(bits[3])
    for (k in seq_len(n_sub)) {
      candidate <- init
      new_val <- fcts[[fct_i]](1, v1, v2)
      if (priors$class[pr_row] == "b") {
        dim(new_val) <- 1
      }
      candidate[[par]] <- new_val
      preds <- get_init_predictions(candidate, x, pred_fct, fct_args)
      if (check_init_predictions(preds, limits)) {
        return(candidate)
      }
    }
  }
  init
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
#' @param seed seed number for reproducible random number generation. Defaults
#' to \code{NULL}.
#' @param ... Additional arguments to \code{\link{make_inits}}.
#'
#' @seealso \code{\link{make_inits}}
#' @return A \code{\link[base]{list}} containing the initialisation values.
#'
#' @noRd
make_good_inits <- function(model, x, y, n_trials = 1e4, seed = NULL, ...) {
  limits <- range(y, na.rm = TRUE)
  pred_fct <- get(paste0("pred_", model))
  fct_args <- names(unlist(as.list(args(pred_fct))))
  fct_args <- setdiff(fct_args, "x")
  dots <- list(...)
  priors_df <- as.data.frame(dots$priors)
  priors_df <- priors_df[priors_df$prior != "", ]
  set.seed(seed)
  inits <- make_inits(model, fct_args, ...)
  init_ranges <- lapply(inits, get_init_predictions, sort(x), pred_fct, fct_args)
  are_good <- all(sapply(init_ranges, check_init_predictions, limits))
  n_t <- 1
  while (!are_good && n_t <= n_trials) {
    inits <- make_inits(model, fct_args, ...)
    init_ranges <- lapply(inits, get_init_predictions, sort(x), pred_fct, fct_args)
    are_good <- all(sapply(init_ranges, check_init_predictions, limits))
    # If the full draw failed, try to fix each chain by re-drawing
    # one problematic parameter at a time (slope, d, beta).
    if (!are_good) {
      inits <- lapply(inits, refine_inits, sort(x), pred_fct, fct_args,
                      limits, priors_df)
      init_ranges <- lapply(inits, get_init_predictions, sort(x),
                            pred_fct, fct_args)
      are_good <- all(sapply(init_ranges, check_init_predictions, limits))
    }
    n_t <- n_t + 1
  }
  if (!are_good) {
    message("bayesnec failed to find initial values within the",
            " range of the response. Using Stan's default",
            " initialisation process.")
    list(random = "random")
  } else {
    inits
  }
}

#' make_good_hurdle_inits
#'
#' Initial values for both parameter blocks of a joint hurdle fit.
#'
#' @inheritParams make_good_inits
#'
#' @param predictor A \code{\link[base]{numeric}} vector containing the full
#' predictor, including the rows where the response is zero.
#' @param response A \code{\link[base]{numeric}} vector; zero denotes a
#' non-survivor.
#' @param priors An object of class \code{\link[brms]{brmsprior}} covering both
#' blocks, i.e. containing both \code{top} and \code{hutop} and so on.
#'
#' @details Each block is primed from the view of the data it actually models,
#' then the two are merged chain-wise. The mu block sees survivors only; the hu
#' block sees the proportion surviving at each unique predictor value, because
#' the sub-model is written as \code{1 - survival} and so the curve being
#' initialised is survival.
#'
#' Both passes reuse the same \code{pred_<model>()} prediction function, which
#' expects unprefixed parameter names -- the \code{hu} prefix is stripped before
#' the search and restored afterwards.
#'
#' @seealso \code{\link{make_good_inits}}
#' @return A \code{\link[base]{list}} of initial values, or
#' \code{list(random = "random")} if either block could not be initialised.
#'
#' @noRd
make_good_hurdle_inits <- function(model, predictor, response, priors, chains,
                                   seed = NULL, ...) {
  parts <- split_hurdle_response(predictor, response)
  pr <- as.data.frame(priors)
  is_hu <- nzchar(pr$nlpar) & grepl("^hu", pr$nlpar)
  mu_pr <- pr[!is_hu, , drop = FALSE]
  hu_pr <- pr[is_hu, , drop = FALSE]
  hu_pr$nlpar <- sub("^hu", "", hu_pr$nlpar)
  mu_inits <- make_good_inits(model, parts$mu$x, parts$mu$y, priors = mu_pr,
                              chains = chains, seed = seed, ...)
  hu_inits <- make_good_inits(model, parts$hu$x, parts$hu$y, priors = hu_pr,
                              chains = chains, seed = seed, ...)
  # If either block fell back to Stan's random initialisation there is nothing
  # coherent to merge -- hand the whole fit to Stan rather than half-priming it.
  fell_back <- function(x) length(x) == 1 && "random" %in% names(x)
  if (fell_back(mu_inits) || fell_back(hu_inits)) {
    return(list(random = "random"))
  }
  lapply(seq_len(chains), function(i) {
    hu_i <- hu_inits[[i]]
    names(hu_i) <- sub("^b_", "b_hu", names(hu_i))
    c(mu_inits[[i]], hu_i)
  })
}
