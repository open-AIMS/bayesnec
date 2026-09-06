#' compare_fitted
#'
#' Extracts posterior predicted values from a list of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}} model fits and
#' compares these across a vector of fitted values.
#'
#' @inheritParams compare_posterior
#' @param make_newdata Should the
#' user allow the package to create \code{newdata} for predictions?
#' If so, arguments \code{resolution} and \code{x_range} will be used. Defaults
#' to TRUE. See details.
#'
#' @details The argument \code{make_newdata} is relevant to those who want the
#' package to create a data.frame from which to make predictions. This is done
#' via \code{\link{bnec_newdata}} and uses arguments \code{resolution} and
#' \code{x_range}. If \code{make_newdata = FALSE} and no additional
#' \code{newdata} argument is provided (via \code{...}), then the predictions
#' are made for the raw data. Else, to generate predictions for a specific
#' user-specific data.frame, set \code{make_newdata = FALSE} and provide
#' an additional data.frame via the \code{newdata} argument. For guidance
#' on how to structure \code{newdata}, see for example
#' \code{\link[brms]{posterior_epred}}.
#' 
#' @seealso \code{\link{bnec}}
#'
#' @return A named \code{\link[base]{list}} containing bootstrapped differences
#' in posterior predictions of the \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} model fits contained in \code{x}. See Details.
#'
#' @importFrom stats quantile
#' @importFrom dplyr mutate bind_rows
#' @importFrom utils combn
#' @importFrom brms posterior_epred
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' data(manec_example)
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' ecx4param <- pull_out(manec_example, model = "ecx4param")
#' compare_fitted(list("nec" = ecx4param, "ecx" = nec4param))
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
compare_fitted <- function(x, resolution = 50, x_range = NA,
                           make_newdata = TRUE, ...) {
  if (is.na(x_range[1])) {
    x_range <- return_x_range(x)
  }
  dot_list <- list(...)
  posterior_list <- vector(mode = "list", length = length(x))
  names(posterior_list) <- names(x)
  for (i in seq_along(posterior_list)) {
    newdata_list <- newdata_eval_fitted(
      x[[i]], resolution = resolution, x_range = x_range,
      make_newdata = make_newdata, fct_eval = "compare_fitted", ...
    )
    x_vec <- newdata_list$x_vec
    resolution <- newdata_list$resolution
    dot_list$newdata <- newdata_list$newdata
    dot_list$re_formula <- newdata_list$re_formula
    dot_list$object <- x[[i]]
    posterior_list[[i]] <- do.call(posterior_epred, dot_list)
  }
  n_samples <- min(sapply(posterior_list, nrow))
  r_posterior_list <- lapply(posterior_list, function(m, n_samples) {
    # A random subset of a longer posterior, not its first n_samples rows.
    # See #218.
    m[sample(seq_len(nrow(m)), n_samples, replace = FALSE), ]
  }, n_samples = n_samples)
  posterior_data <- posterior_list |>
      lapply(summarise_posterior, x_vec = x_vec) |>
      bind_rows(.id = "model") |>
      data.frame()
  all_combn <- combn(names(x), 2, simplify = FALSE)
  diff_list <- lapply(all_combn, function(a, r_list) {
    r_list[[a[1]]] - r_list[[a[2]]]
  }, r_list = r_posterior_list)
  names(diff_list) <- sapply(all_combn, function(m) paste0(m[1], "-", m[2]))
  diff_data <- lapply(diff_list, function(m, x_vec) {
   out_diffs <- apply(m, 2, quantile, probs = c(0.5, 0.025, 0.975)) |>
     t() |>
     data.frame() |>
     mutate(x = x_vec)
  }, x_vec)
  diff_data_out <- bind_rows(diff_data, .id = "comparison")
  colnames(diff_data_out) <- c("comparison", "diff.Estimate",
                               "diff.Q2.5", "diff.Q97.5", "x")
  prob_diff <- lapply(diff_list, function(m, x_vec) {
    m[m > 0] <- 1
    m[m <= 0] <- 0
    cbind(x = x_vec, prob = colMeans(m)) |>
      data.frame()
  }, x_vec)
  prob_diff_out <- bind_rows(prob_diff, .id = "comparison")
  list(posterior_list = posterior_list, posterior_data = posterior_data,
       diff_list = diff_list, diff_data = diff_data_out,
       prob_diff = prob_diff_out)
}
