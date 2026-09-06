#' compare_estimates
#'
#' Extracts posterior predicted values from a list of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}} model fits and
#' compares these via bootstrap re sampling.
#'
#' @inheritParams compare_posterior
#' 
#' @importFrom chk chk_numeric
#'
#' @seealso \code{\link{bnec}}
#'
#' @return A named \code{\link[base]{list}} containing bootstrapped differences
#' in posterior predictions of the \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} model fits contained in \code{x}. See Details.
#'
#' @importFrom dplyr bind_rows arrange
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect everything
#' @importFrom utils combn
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' data(manec_example)
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' ecx4param <- pull_out(manec_example, model = "ecx4param")
#' compare_estimates(list("nec" = ecx4param, "ecx" = nec4param), ecx_val = 50,
#' comparison="ecx")
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
compare_estimates <- function(x, comparison = "n(s)ec", ecx_val = 10,
                              type = "absolute",
                              sig_val = 0.01, resolution = 100, x_range = NA) {
  if ((comparison %in% c("nec", "n(s)ec", "ecx", "nsec")) == FALSE) {
    stop("comparison must be one of nec, n(s)ec, ecx or nsec.")
  }
  chk_numeric(ecx_val)
  if ((type %in% c("relative", "absolute", "direct")) == FALSE) {
    stop("type must be one of \"relative\", \"absolute\" (the default) or",
         "\"direct\". Please see ?ecx for more details.")
  }
  chk_numeric(sig_val)
  chk_numeric(resolution)
  if (is.na(x_range[1])) {
    x_range <- return_x_range(x)
  } else {
    chk_numeric(x_range)    
  }
  if (comparison == "nec") {
    posterior_list <- lapply(x, nec, posterior = TRUE, xform = identity)
  }
  if (comparison == "n(s)ec") {
    posterior_list <- lapply(x, return_nec_post, xform = identity)
  }
  if (comparison == "ecx") {
    posterior_list <- lapply(x, ecx, ecx_val = ecx_val, resolution = resolution,
                             posterior = TRUE, type = type,
                             x_range = x_range)
  }
  if (comparison == "nsec") {
    posterior_list <- lapply(x, nsec, sig_val = sig_val, resolution = resolution,
                             posterior = TRUE,
                             x_range = x_range)
  }
  names(posterior_list) <- names(x)
  n_samples <- min(sapply(posterior_list, length))
  r_posterior_list <- lapply(posterior_list, function(m, n_samples) {
    # A random subset of a longer posterior, not its first n_samples draws.
    # sample(seq_len(n_samples)) permuted only the head of the vector, so where
    # components had unequal draw counts the tail of the longer one was never
    # used -- systematic rather than random thinning. Harmless when the counts
    # are equal, which is the normal case. See #218.
    m[sample(seq_along(m), n_samples, replace = FALSE)]
  }, n_samples = n_samples)
  posterior_data <- do.call("cbind", r_posterior_list) |>
    data.frame() |>
    pivot_longer(cols = everything(), names_to = "model") |>
    arrange(.data$model) |>
    data.frame()
  all_combn <- combn(names(x), 2, simplify = FALSE)
  diff_list <- lapply(all_combn, function(a, r_list) {
    r_list[[a[1]]] - r_list[[a[2]]]
  }, r_list = r_posterior_list)
  names(diff_list) <- sapply(all_combn, function(m) paste0(m[1], "-", m[2]))
  diff_data_out <- bind_rows(diff_list, .id = "comparison") |>
    pivot_longer(everything(), names_to = "comparison", values_to = "diff") |>
    data.frame()
  prob_diff <- lapply(diff_list, function(m) {
    m[m > 0] <- 1
    m[m <= 0] <- 0
    data.frame(prob = mean(m))
  })
  prob_diff_out <- bind_rows(prob_diff, .id = "comparison") |>
    data.frame()
  list(posterior_list = posterior_list, posterior_data = posterior_data,
       diff_list = diff_list, diff_data = diff_data_out,
       prob_diff = prob_diff_out)
}
