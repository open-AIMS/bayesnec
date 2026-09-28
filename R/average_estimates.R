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
#' before the the geometric mean calculation is applied. Where \code{crf()}
#' transforms the predictor inline and \code{xform} is left at its default, a
#' message says so once per call, naming the transformation and the
#' \code{xform} that inverts it.
#'
#' @seealso \code{\link{bnec}}
#'
#' @return The geometric mean of the estimates estimate values
#' of the \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}
#' model fits contained in \code{x}. See Details.
#'
#' @importFrom stats quantile
#' @importFrom chk chk_lgl chk_numeric chk_number
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
#' The draws of each posterior are paired by a random permutation, so the mean
#' is a Monte Carlo approximation using randomly paired posterior draws.
#' The permutation is drawn under \code{seed}, so two calls on the same
#' fits return the same estimate and a \code{\link[base]{set.seed}} in the
#' session has no effect on it. A different \code{seed} gives another
#' realisation of the same approximation; where the approximation matters,
#' compare a few and report the spread. The caller's random number state is
#' restored afterwards.
#'
#' @section The independence assumption:
#' The pairing is valid only where the posteriors being combined come from
#' \bold{separate fits}. Two levels of one fit share draws --- draw \emph{i}
#' of each comes from the same sweep of the sampler --- and permuting them
#' destroys that pairing and discards the correlation between the levels.
#' A within-fit combination needs draw-wise arithmetic and must not be routed
#' through this function. See #218 and #33.
#'
#' @export
average_estimates <- function(x, estimate = "nec", ecx_val = 10,
                              posterior = FALSE, type = "absolute",
                              sig_val = 0.01,
                              resolution = 200, x_range = NA, xform = identity,
                              prob_vals = c(0.5, 0.025, 0.975), seed = 10) {
  if (!is.list(x) | is.null(names(x))) {
    stop("Argument x must be a named list")
  }
  if (!is.character(estimate)) {
    stop("Argument estimate must be a character vector")
  }
  chk_lgl(posterior)
  # Validated against the four-value vocabulary here rather than left to the
  # per-fit ecx() calls below, so that an invalid type is refused before any
  # posterior is drawn, and the rename warning is issued once for the call
  # rather than once per fit in x. Same reasoning as ecx.bayesmanecfit and
  # compare_estimates(). See D15 ruling 8.
  type <- validate_ecx_type(type, match.call())
  warned <- options(bayesnec.relative_warned = TRUE)
  on.exit(options(warned), add = TRUE)
  chk_numeric(ecx_val)
  chk_numeric(sig_val)
  chk_numeric(resolution)
  if (!inherits(xform, "function")) {
    stop("xform must be a function.")
  }
  chk_numeric(prob_vals)
  chk_number(seed)
  # Once for the call rather than once per fit in x, and decided here because
  # return_nec_post() reads the stored posteriors without calling nec(). See
  # report_fitted_scale().
  quiet <- report_fitted_scale(first_transformed_fit(x), xform,
                               "average_estimates")
  on.exit(options(quiet), add = TRUE)
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
  # Random pairing affects the reported estimates. Seed it locally so calls
  # repeat without changing the caller's RNG state (#343).
  with_preserved_rng_state({
    set.seed(seed, sample.kind = "Rejection")
    r_posterior_list <- lapply(posterior_list, FUN = function(m, n_samples) {
      # A random subset of a longer posterior, not its first n_samples draws.
      # sample(seq_len(n_samples)) permuted only the head of the vector, so
      # where components had unequal draw counts the tail of the longer one was
      # never used -- systematic rather than random thinning. Harmless when the
      # counts are equal, which is the normal case. See #218.
      m[sample(seq_along(m), n_samples, replace = FALSE)]
    }, n_samples = n_samples)
  })
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
