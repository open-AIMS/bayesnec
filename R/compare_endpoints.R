#' compare_endpoints
#'
#' Extracts posterior predicted values from a list of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}} model fits and
#' compares these via bootstrap re sampling.
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
#' @importFrom dplyr %>% mutate bind_rows arrange
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect everything
#' @importFrom utils combn
#' @importFrom rlang .data
#'
#' @examples
#' library(bayesnec)
#' data(manec_example)
#' ecx4param <- pull_out(manec_example, model = "ecx4param")
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' compare_endpoints(list("nec" = ecx4param, "ecx" = nec4param), ecx_val = 50)
#'
#' @export
compare_endpoints <- function(x, comparison = "nec", ecx_val = 10,
                              type = "absolute", hormesis_def = "control",
                              sig_val = 0.01, precision, x_range = NA) {
  if (is.na(x_range)) {
    x_range <- return_x_range(x)
  }
  if (comparison == "nec") {
    posterior_list <- lapply(x, return_nec_post, xform = NA)
  }
  if (comparison == "ecx") {
    posterior_list <- lapply(x, ecx, ecx_val = ecx_val, precision = precision,
                             posterior = TRUE, type = type,
                             hormesis_def = hormesis_def, x_range = x_range)
  }
  if (comparison == "nsec") {
    posterior_list <- lapply(x, nsec, sig_val = sig_val, precision = precision,
                             posterior = TRUE, hormesis_def = hormesis_def,
                             x_range = x_range)
  }
  names(posterior_list) <- names(x)
  n_samples <- min(sapply(posterior_list, length))
  r_posterior_list <- lapply(posterior_list, function(m, n_samples) {
    m[sample(seq_len(n_samples), replace = FALSE)]
  }, n_samples = n_samples)
  posterior_data <- do.call("cbind", r_posterior_list) %>%
    data.frame %>%
    pivot_longer(cols = everything(), names_to = "model") %>%
    arrange(.data$model) %>%
    data.frame
  all_combn <- combn(names(x), 2, simplify = FALSE)
  diff_list <- lapply(all_combn, function(a, r_list) {
    r_list[[a[1]]] - r_list[[a[2]]]
  }, r_list = r_posterior_list)
  names(diff_list) <- sapply(all_combn, function(m) paste0(m[1], "-", m[2]))
  diff_data_out <- bind_rows(diff_list, .id = "comparison") %>%
    pivot_longer(everything(), names_to = "comparison", values_to = "diff") %>%
    data.frame
  prob_diff <- lapply(diff_list, function(m) {
    m[m > 0] <- 1
    m[m <= 0] <- 0
    data.frame(prob = mean(m))
  })
  prob_diff_out <- bind_rows(prob_diff, .id = "comparison") %>%
    data.frame
  list(posterior_list = posterior_list, posterior_data = posterior_data,
       diff_list = diff_list, diff_data = diff_data_out,
       prob_diff = prob_diff_out)
}
