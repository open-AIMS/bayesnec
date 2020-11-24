#' compare_posterior
#'
#' Extracts posterior predicted values from a list of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}} model fits and
#' compares these via bootstrap re sampling.
#'
#' @param x A named list \code{\link[base]{list}} of objects of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}} returned by
#' \code{\link{bnec}}.
#' @param comparison The posterior predictions to compare, takes values of
#' "nec", "nsec", "ecx" or "fitted".
#' @param n_samples The number of bootstrap iterations to perform.
#'
#' @inheritParams ecx
#' @inheritParams nsec
#'
#' @seealso \code{\link{bnec}}
#'
#' @return A named list containing bootstrapped differences in posterior
#' predictions of the \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}}
#' model fits contained in x. See Details.
#'
#' @importFrom stats quantile predict
#' @importFrom dplyr %>% mutate bind_rows group_by summarise ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect everything
#'
#' @examples
#' \dontrun{
#' library(brms)
#' library(bayesnec)
#' options(mc.cores = parallel::detectCores())
#' data(nec_data)
#'
#' exmp <- bnec(data = nec_data, x_var = "x", y_var = "y",
#'              model = c("ecx4param", "nec4param"),
#'              family = Beta(link = "identity"), priors = my_priors,
#'              iter = 1e4, control = list(adapt_delta = 0.99))
#' exmp_2 <- pull_out(exmp, "nec4param")
#' exmp_3 <- pull_out(exmp, "ecx4param")
#' compare_posterior(list(exmp_2, exmp_3), ecx_val = 50)
#' }
#'
#' @export
compare_posterior <- function(x, comparison = "nec", ecx_val = 10,
                              type = "absolute", hormesis_def = "control",
                              sig_val = 0.01, precision = 1000, x_range = NA,
                              n_samples = 2000) {
  if (comparison == "nec") {
    posterior_list <- lapply(x, function(m) {
      if (class(m) == "bayesnecfit") {
        out <- unname(m$nec_posterior)
      }
      if (class(m) == "bayesmanecfit") {
        out <- unname(m$w_nec_posterior)
      }
      return(out)
    })
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
  if (comparison == "fitted") {
    posterior_list <- lapply(x, function(m, ...) {
      predict(m, ...)$posterior
    }, precision = precision, x_range = x_range)
    x_vec <- predict(x[[1]], precision = precision, x_range = x_range)$data$x
  }
  names(posterior_list) <- names(x)
  if (comparison != "fitted") {
    posterior_data <- do.call("cbind", posterior_list) %>%
      data.frame %>%
      pivot_longer(cols = everything(), names_to = "model")
    diff_list <- boot_sample(posterior_list, n_samples)
    prob_diff <- extract_probs(diff_list)
    diff_data <- extract_diffs(diff_list)
  } else {
    posterior_data <- posterior_list %>%
      lapply(summarise_posterior, x_vec = x_vec) %>%
      bind_rows(.id = "model") %>%
      data.frame
    diff_list <- boot_sample_fitted(posterior_list, n_samples, x_vec)
    prob_diff <- extract_probs_fitted(diff_list)
    diff_data <- extract_diffs_fitted(diff_list) %>%
      mutate(index = 1) %>%
      group_by(x, comparison) %>%
      summarise(prob = sum(gr_0) / sum(index),
                diff.Estimate = quantile(diff, probs = 0.5),
                diff.Q2.5 = quantile(diff, probs = 0.025),
                diff.Q97.5 = quantile(diff, probs = 0.975),
                .groups = "keep") %>%
      ungroup %>%
      mutate(x = as.numeric(as.character(x))) %>%
      data.frame
  }
  list(posterior_list = posterior_list,
       posterior_data = posterior_data,
       diff_list = diff_list,
       diff_data = diff_data,
       prob_diff = prob_diff)
}

boot_sample <- function(posterior_list, n_samples = 1000) {
  lapply(seq_len(n_samples), function(n, posterior_list) {
    v <- sapply(posterior_list, sample, size = 1)
    outer(v, v, `-`)
  }, posterior_list = posterior_list)
}

boot_sample_fitted <- function(posterior_list, n_samples = 1000, x_vec) {
  all_diff_list <- lapply(seq_len(length(x_vec)), function(p, pl, ns) {
    posterior_list_p <- lapply(pl, function(h, p) h[, p], p = p)
    boot_sample(posterior_list_p, ns)
  }, pl = posterior_list, ns = n_samples)
  names(all_diff_list) <- x_vec
  all_diff_list
}

extract_probs <- function(diff_list) {
  tmp <- lapply(diff_list, function(x) {
    x[x >= 0] <- 1
    x[x < 0] <- 0
    x
  })
  Reduce("+", tmp) / length(diff_list)
}

extract_probs_fitted <- function(all_diff_list) {
  lapply(all_diff_list, extract_probs)
}

extract_diffs <- function(diff_list) {
  do.call("rbind", lapply(diff_list, melt_diff))
}

#' @importFrom dplyr %>% if_else mutate
#' @importFrom plyr ldply
extract_diffs_fitted <- function(all_diff_list) {
  ldply(all_diff_list, extract_diffs, .id = "x") %>%
    mutate(gr_0 = if_else(diff >= 0, 1, 0))
}

#' @importFrom dplyr %>%
#' @importFrom tidyr gather_ unite
#' @importFrom lazyeval interp
melt_diff <- function(diff, order = NULL, diff_name = "diff") {
  if (!is.null(order)) {
    diff <- diff[order, order]
  } else {
    order <- row.names(diff)
  }
  diag(diff) <- NA
  diff[upper.tri(diff)] <- NA
  diff_df <- as.data.frame(diff)
  diff_df$rows <- row.names(diff)
  diff_df %>%
    gather_(key_col = "cols",
            value_col = interp("diff_name", diff_name = as.name(diff_name)),
            order, na.rm = TRUE) %>%
    unite(comparison, rows, cols, sep = "-") %>%
    data.frame
}

summarise_posterior <- function(mat, x_vec) {
  cbind(x = x_vec, data.frame(t(apply(mat, 2, estimates_summary))))
}

#' @importFrom dplyr %>% rename_all mutate
#' @importFrom tidyr pivot_longer
pivot_posterior <- function(mat, vec) {
  data.frame(t(mat)) %>%
    rename_all(~ gsub("X", "col_", .x, fixed = TRUE)) %>%
    mutate(x_var = vec) %>%
    pivot_longer(cols = -x_var,
                 names_to = "sample",
                 values_to = "y_pred")
}
