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
#' @export
average_estimates <- function(x, estimate = "nec", ecx_val = 10,
                              posterior = FALSE, type = "absolute",
                              hormesis_def = "control", sig_val = 0.01,
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
  chk_character(hormesis_def)
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
                             hormesis_def = hormesis_def, x_range = x_range,
                             xform = xform)
  }
  if (estimate == "nsec") {
    posterior_list <- lapply(x, nsec, sig_val = sig_val, resolution = resolution,
                             posterior = TRUE, hormesis_def = hormesis_def,
                             x_range = x_range, xform = xform)
  }
  names(posterior_list) <- names(x)
  n_samples <- min(sapply(posterior_list, length))
  r_posterior_list <- lapply(posterior_list, FUN = function(m, n_samples) {
    m[sample(seq_len(n_samples), replace = FALSE)]
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
