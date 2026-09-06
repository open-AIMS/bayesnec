#' average_estimates
#'
#' Extracts posterior predicted estimate values from a list of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}} model fits and
#' calculates a geometric mean.
#'
#' @inheritParams compare_posterior
#' @inheritParams ecx
#' @inheritParams nsec
#'
#' @param estimate The type of estimate to use in the mean. Takes values
#' "nec", "ecx" or "nsec".
#'
#' @details The geometric mean of values are simply the mean calculated on a
#' log scale and back transformed through \code{\link[base]{exp}}, although we
#' have added the capacity to accommodate zero values. Note that the function
  #' assumes that \code{x} has been modelled on the natural scale. Often CR
#' models are more stable on a log-transformed or sqrt scaling. If the input
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}} model fits are
#' already based on a re-scaling of the x (concentration) axis, it is important
#' to pass an appropriate xform argument to ensure these are back transformed
#' before the the geometric mean calculation is applied.
#'
#' @seealso \code{\link{bnec}}
#'
#' @return The geometric mean of the estimates estimate values
#' of the \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}
#' model fits contained in \code{x}. See Details.
#'
#' @importFrom stats quantile
#' @importFrom chk chk_lgl chk_character chk_numeric
#'
#' @examples
#' \dontrun{
#' library(brms)
#' library(bayesnec)
#' data(manec_example)
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' ecx4param <- pull_out(manec_example, model = "ecx4param")
#' average_estimates(list("nec" = ecx4param, "ecx" = nec4param), ecx_val = 50)
#' }
#'
#' @section Reproducibility:
#' The draws of each posterior are paired by an independent random permutation,
#' so \code{prob_diff} and the difference intervals change between identical
#' calls. Use \code{\link[base]{set.seed}} before the call for a reproducible
#' result. This is a Monte Carlo approximation to the difference of two
#' \bold{independent} posteriors, using \emph{n} of the \emph{n}^2 available
#' pairs.
#'
#' @section The independence assumption:
#' The pairing is valid only where the posteriors being compared come from
#' \bold{separate fits}. Two levels of one fit share draws --- draw \emph{i}
#' of each comes from the same sweep of the sampler --- and permuting them
#' destroys that pairing, which discards the correlation between the levels and
#' widens the difference posterior. \code{prob_diff} is then pulled toward 0.5
#' and a real difference is under-detected, which is the wrong direction to err
#' in. A within-fit contrast needs draw-wise differencing and must not be routed
#' through this function. See #218 and #33.
#'
#' @export
average_estimates <- function(x, estimate = "nec", ecx_val = 10,
                              posterior = FALSE, type = "absolute",
                              sig_val = 0.01,
                              resolution = 1000, x_range = NA, xform = identity,
                              prob_vals = c(0.5, 0.025, 0.975)) {
  if (!is.list(x) | is.null(names(x))) {
    stop("Argument x must be a named list")
  }
  if (!is.character(estimate)) {
    stop("Argument estimate must be a character vector")
  }
  chk_lgl(posterior)
  chk_character(type)
  chk_numeric(ecx_val)
  chk_numeric(sig_val)
  chk_numeric(resolution)
  if (!inherits(xform, "function")) {
    stop("xform must be a function.")
  }
  chk_numeric(prob_vals)
  if (is.na(x_range[1])) {
    x_range <- return_x_range(x)
  }
  if (estimate == "nec") {
    posterior_list <- lapply(x, return_nec_post, xform = xform)
  }
  if (estimate == "ecx") {
    posterior_list <- lapply(x, ecx, ecx_val = ecx_val, resolution = resolution,
                             posterior = TRUE, type = type,
                             x_range = x_range,
                             xform = xform)
  }
  if (estimate == "nsec") {
    posterior_list <- lapply(x, nsec, sig_val = sig_val, resolution = resolution,
                             posterior = TRUE,
                             x_range = x_range, xform = xform)
  }
  names(posterior_list) <- names(x)
  n_samples <- min(sapply(posterior_list, length))
  r_posterior_list <- lapply(posterior_list, FUN = function(m, n_samples) {
    # A random subset of a longer posterior, not its first n_samples draws.
    # sample(seq_len(n_samples)) permuted only the head of the vector, so where
    # components had unequal draw counts the tail of the longer one was never
    # used -- systematic rather than random thinning. Harmless when the counts
    # are equal, which is the normal case. See #218.
    m[sample(seq_along(m), n_samples, replace = FALSE)]
  }, n_samples = n_samples)
  posterior_data <- do.call("cbind", r_posterior_list) |>
      data.frame()
  post_mean <- apply(posterior_data, MARGIN = 1, FUN = gm_mean)
  mean_estimate <- quantile(unlist(post_mean), na.rm = TRUE, probs = prob_vals)
  names(mean_estimate) <- clean_names(mean_estimate)
  if (!posterior) {
    mean_estimate
  } else {
    post_mean
  }
}
