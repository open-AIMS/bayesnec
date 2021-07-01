#' compare_fitted
#'
#' Extracts posterior predicted values from a list of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}} model fits and
#' compares these across a vector of fitted values.
#'
#' @inheritParams compare_posterior
#'
#' @seealso \code{\link{bnec}}
#'
#' @return A named list containing bootstrapped differences in posterior
#' predictions of the \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}}
#' model fits contained in x. See Details.
#'
#' @importFrom stats quantile predict
#' @importFrom dplyr %>% mutate bind_rows
#' @importFrom utils combn
#'
#' @examples
#' \donttest{
#' library(bayesnec)
#' data(manec_example)
#' ecx4param <- pull_out(manec_example, model = "ecx4param")
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' compare_fitted(list("nec" = ecx4param, "ecx" = nec4param))
#' }
#'
#' @export
compare_fitted <- function(x, precision = 50, x_range = NA) {
  if (is.na(x_range)) {
    x_range <- return_x_range(x)
  }
  posterior_list <- lapply(x, function(m, ...) {
    predict(m, ...)$posterior
  }, precision = precision, x_range = x_range)
  x_vec <- seq(min(x_range), max(x_range), length = precision)
  names(posterior_list) <- names(x)
  n_samples <- min(sapply(posterior_list, nrow))
  r_posterior_list <- lapply(posterior_list, function(m, n_samples) {
    m[sample(seq_len(n_samples), replace = FALSE), ]
  }, n_samples = n_samples)
  posterior_data <- posterior_list %>%
      lapply(summarise_posterior, x_vec = x_vec) %>%
      bind_rows(.id = "model") %>%
      data.frame
  all_combn <- combn(names(x), 2, simplify = FALSE)
  diff_list <- lapply(all_combn, function(a, r_list) {
    r_list[[a[1]]] - r_list[[a[2]]]
  }, r_list = r_posterior_list)
  names(diff_list) <- sapply(all_combn, function(m) paste0(m[1], "-", m[2]))
  diff_data <- lapply(diff_list, function(m, x_vec) {
   out_diffs <- apply(m, 2, quantile, probs = c(0.5, 0.025, 0.975)) %>%
     t %>%
     data.frame %>%
     mutate(x = x_vec)
  }, x_vec)
  diff_data_out <- bind_rows(diff_data, .id = "comparison")
  colnames(diff_data_out) <- c("comparison", "diff.Estimate",
                               "diff.Q2.5", "diff.Q97.5", "x")
  prob_diff <- lapply(diff_list, function(m, x_vec) {
    m[m > 0] <- 1
    m[m <= 0] <- 0
    cbind(x = x_vec, prob = colMeans(m)) %>%
      data.frame
  }, x_vec)
  prob_diff_out <- bind_rows(prob_diff, .id = "comparison")
  list(posterior_list = posterior_list, posterior_data = posterior_data,
       diff_list = diff_list, diff_data = diff_data_out,
       prob_diff = prob_diff_out)
}
