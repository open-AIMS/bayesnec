#' extract_pars
#' @param x A \code{\link[base]{character}} vector.
#' @param model_fit An object of class \code{\link[brms]{brmsfit}}.
#' @return A named \code{\link[base]{numeric}} vector or NA.
#' @importFrom brms fixef
extract_pars <- function(x, model_fit) {
  fef <- fixef(model_fit, robust = TRUE)
  tt <- fef[grep(x, rownames(fef)), c("Estimate", "Q2.5", "Q97.5")]
  if (is.na(tt["Estimate"])) {
    NA
  } else {
    tt
  }
}

#' min_abs
#' @param x A \code{\link[base]{numeric}} vector.
#' @return A \code{\link[base]{numeric}} vector.
min_abs <- function(x) {
  which.min(abs(x))
}

#' paste_normal_prior
#'
#' Creates prior string given a number
#'
#' @param mean A \code{\link[base]{numeric}} vector.
#' @param param A \code{\link[base]{character}} vector indicating the
#' target non-linear parameter.
#' @param sd A \code{\link[base]{numeric}} vector indicating the
#' standard deviation.
#' @param ... Additional arguments of \code{\link[brms]{prior_string}}.
#'
#' @return A \code{\link[base]{character}} vector.
#' @importFrom brms prior_string
paste_normal_prior <- function(mean, param, sd = 1, ...) {
    prior_string(paste0("normal(", mean, ", ", sd, ")"), nlpar = param, ...)
}

extract_dispersion <- function(x) {
  x$dispersion
}

extract_loo <- function(x) {
  x$fit$loo
}

extract_waic <- function(x) {
  x$fit$waic$estimates["waic", "Estimate"]
}

w_nec_calc <- function(index, mod_fits, sample_size, mod_stats) {
  sample(mod_fits[[index]]$nec_posterior,
         as.integer(round(sample_size * mod_stats[index, "wi"])))
}

w_pred_calc <- function(index, mod_fits, mod_stats) {
  mod_fits[[index]]$predicted_y * mod_stats[index, "wi"]
}

w_post_pred_calc <- function(index, mod_fits, sample_size, mod_stats) {
  x <- seq_len(sample_size)
  size <- round(sample_size * mod_stats[index, "wi"])
  mod_fits[[index]]$pred_vals$posterior[sample(x, size), ]
}

do_wrapper <- function(..., fct = "cbind") {
  do.call(fct, lapply(...))
}

#' @importFrom stats median quantile
estimates_summary <- function(x) {
  x <- c(median(x), quantile(x, c(0.025, 0.975)))
  names(x) <- c("Estimate", "Q2.5", "Q97.5")
  x
}

handle_set <- function(x, add, drop) {
  msets <- names(mod_groups)
  tmp <- x
  if (!missing(add)) {
    y <- add
    if (any(add %in% msets)) {
      y <- unname(unlist(mod_groups[intersect(add, msets)]))
      y <- setdiff(union(y, add), msets)
    }
    tmp <- union(tmp, y)
  }
  if (!missing(drop)) {
    y <- drop
    if (any(drop %in% msets)) {
      y <- unname(unlist(mod_groups[intersect(drop, msets)]))
    }
    tmp <- setdiff(tmp, y)
    if (length(tmp) == 0) {
      stop("All models removed, nothing to return;\n",
           "Perhaps try calling function bnec with another ",
           "model set")
    }
  }
  if (identical(sort(x), sort(tmp))) {
    message("Nothing to modify, please specify a model to ",
            "either add or drop that differs from the original set")
    FALSE
  } else {
    tmp
  }
}

#' allot_class
#'
#' Assigns class to an object.
#'
#' @param x An object.
#' @param new_class The new object class.
#'
#' @return An object of class new_class.
allot_class <- function(x, new_class) {
  class(x) <- new_class
  x
}

expand_and_assign_nec <- function(x, ...) {
  allot_class(expand_nec(x, ...), "bayesnecfit")
}

#' are_chains_correct
#'
#' Checks if number of chains in brmsfit object is correct
#'
#' @param brms_fit An object of class \code{\link[brms]{brmsfit}}.
#' @param chains The expected number of correct chains.
#'
#' @return A \code{\link[base]{logical}} vector.
are_chains_correct <- function(brms_fit, chains) {
  fit_chs <- brms_fit$fit@sim$chains
  if (is.null(fit_chs)) {
    FALSE
  } else {
    fit_chs == chains
  }
}

pred_nec3param <- function(b_beta, b_nec, b_top, x) {
  b_top * exp(-b_beta * (x - b_nec) *
    ifelse(x - b_nec < 0, 0, 1))
}

pred_nec4param <- function(b_beta, b_bot, b_nec, b_top, x) {
  b_bot + (b_top - b_bot) * exp(-b_beta * (x - b_nec) *
    ifelse(x - b_nec < 0, 0, 1))
}

pred_nechorme <- function(b_top, b_slope, b_beta, b_nec, x) {
  (b_top + b_slope * x) * exp(-b_beta * (x - b_nec) *
    ifelse(x - b_nec < 0, 0, 1))
}

pred_necsigm <- function(b_beta, b_top, b_nec, b_d, x) {
  b_top * exp(-b_beta * (x - b_nec) ^ b_d *
    ifelse(x - b_nec < 0, 0, 1))
}

pred_ecxlin <- function(b_top, b_slope, x) {
  b_top - b_slope * x
}

pred_ecxexp <- function(b_top, b_beta, x) {
  b_top * exp(-b_beta * x)
}

pred_ecxsigm <- function(b_top, b_beta, b_d, x) {
  b_top * exp(-b_beta * x)^b_d
}

pred_ecx4param <- function(b_top, b_bot, b_ec50, b_beta, x) {
  b_top + (b_bot - b_top) /
    (1 + exp((b_ec50 - x) * b_beta))
}

pred_ecxwb1 <- function(x) {
  b_bot + (b_top - b_bot) *
    exp(-exp(b_beta * (x - b_ec50)))
}

pred_ecxwb2 <- function(x) {
  b_bot + (b_top - b_bot) *
    (1 - exp(-exp(b_beta * (x - b_ec50))))
}

get_init_ranges <- function(y, x, fct, .args) {
  y <- y[match(.args, names(y))]
  y <- lapply(y, as.numeric)
  y[["x"]] <- x
  range(do.call("fct", y))
}

check_limits <- function(x, limits) {
  min(x) >= min(limits) & max(x) <= max(limits)
}

make_good_inits <- function(family, model, x, ...) {
  limits <- list(gaussian = c(-Inf, Inf),
                 poisson = c(0, Inf),
                 negbinomial = c(0, Inf),
                 Gamma = c(1e-100, Inf),
                 beta = c(1e-100, 0.999999999),
                 binomial = c(0, 1))
  limits <- limits[[family$family]]
  pred_fct <- get(paste0("pred_", model))
  fct_args <- names(unlist(as.list(args(pred_fct))))
  fct_args <- setdiff(fct_args, "x")
  inits <- make_inits(...)
  init_ranges <- lapply(inits, get_init_ranges,
                        x, pred_fct, fct_args)
  are_good <- all(sapply(init_ranges, check_limits, limits))
  while (!are_good) {
    inits <- make_inits(...)
    init_ranges <- lapply(inits, get_init_ranges,
                          x, pred_fct, fct_args)
    are_good <- all(sapply(init_ranges,
                           check_limits,
                           limits))
  }
  inits
}
