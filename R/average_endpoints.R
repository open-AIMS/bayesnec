#' average_endpoints
#'
#' Extracts posterior predicted endpoint values from a list of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}} model fits and
#' calculates a geometric mean.
#'
#' @inheritParams compare_posterior
#' @inheritParams ecx
#' @inheritParams nsec
#'
#' @param endpoint The type of endpoint to use in the mean. Takes values
#' "nec", "ecx" or "nsec".
#'
#' @details The geometric mean of values are simply the mean calculated on a
#' log scale and back transformed through \code{\link[base]{exp}}, although we
#' have added the capacity to accommodate zero values. Note that the function
#' assumes that \code{x} has been modelled on the natural scale. Often C-R
#' models are more stable on a log transformed or sqrt scaling. If the input
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}} model fits are
#' already based on a re-scaling of the x (concentration) axis, it is important
#' to pass an appropriate xform argument to ensure these are back transformed
#' before the the geometric mean calculation is applied.
#'
#' @seealso \code{\link{bnec}}
#'
#' @return The geometric mean of the endpoints estimate values
#' of the \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}
#' model fits contained in \code{x}. See Details.
#'
#' @importFrom stats quantile predict
#' @importFrom dplyr %>% mutate bind_rows arrange
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect everything
#' @importFrom utils combn
#'
#' @examples
#' \dontrun{
#' library(brms)
#' library(bayesnec)
#' data(manec_example)
#' data(nec4param)
#' ecx4param <- pull_out(manec_example, model = "ecx4param")
#' average_endpoints(list("nec" = ecx4param, "ecx" = nec4param), ecx_val = 50)
#' }
#'
#' @export
average_endpoints <- function(x, endpoint = "nec", ecx_val = 10,
                              posterior = FALSE, type = "absolute",
                              hormesis_def = "control", sig_val = 0.01,
                              precision = 1000, x_range = NA, xform = NA,
                              prob_vals = c(0.5, 0.025, 0.975)) {
  if (!is.list(x) | is.null(names(x))) {
    stop("Argument x must be a named list")
  }
  if (!is.character(endpoint)) {
    stop("Argument endpoint must be a character vector")
  }
  if (is.na(x_range)) {
    x_range <- return_x_range(x)
  }
  if (endpoint == "nec") {
    posterior_list <- lapply(x, return_nec_post, xform = xform)
  }
  if (endpoint == "ecx") {
    posterior_list <- lapply(x, ecx, ecx_val = ecx_val, precision = precision,
                             posterior = TRUE, type = type,
                             hormesis_def = hormesis_def, x_range = x_range,
                             xform = xform)
  }
  if (endpoint == "nsec") {
    posterior_list <- lapply(x, nsec, sig_val = sig_val, precision = precision,
                             posterior = TRUE, hormesis_def = hormesis_def,
                             x_range = x_range, xform = xform)
  }
  names(posterior_list) <- names(x)
  n_samples <- min(sapply(posterior_list, length))
  r_posterior_list <- lapply(posterior_list, FUN = function(m, n_samples) {
    m[sample(seq_len(n_samples), replace = FALSE)]
  }, n_samples = n_samples)
  posterior_data <- do.call("cbind", r_posterior_list) %>%
      data.frame
  post_mean <- apply(posterior_data, MARGIN = 1, FUN = gm_mean)
  mean_estimate <- quantile(unlist(post_mean), na.rm = TRUE, probs = prob_vals)
  if (!posterior) {
    mean_estimate
  } else {
    post_mean
  }
}
