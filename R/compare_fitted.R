#' compare_fitted
#'
#' Extracts posterior predicted values from a list of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}} model fits and
#' compares these across a vector of fitted values.
#'
#' @inheritParams compare_posterior
#' @param x A named \code{\link[base]{list}} of
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}} objects.
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
#' @return A named \code{\link[base]{list}} containing paired differences
#' in posterior predictions of the \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} model fits contained in \code{x}. See Details.
#'
#' @importFrom stats quantile
#' @importFrom dplyr mutate bind_rows
#' @importFrom utils combn
#' @importFrom brms posterior_epred
#' @importFrom chk chk_number
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' data(manec_example)
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' ecx4param <- pull_out(manec_example, model = "ecx4param")
#' compare_fitted(list("nec4param" = nec4param, "ecx4param" = ecx4param))
#' }
#'
#' @section Reproducibility:
#' The draws of each posterior are put in a random order, a longer posterior
#' being randomly subsampled to the length of the shortest, and are then paired
#' by position. \code{prob_diff} and the difference intervals are therefore a
#' Monte Carlo approximation that uses \emph{n} of the \emph{n}^2 available
#' pairs. The permutation is drawn under
#' \code{seed}, so two calls on the same fits return the same comparison and a
#' \code{\link[base]{set.seed}} in the session has no effect on it. A different
#' \code{seed} gives another realisation of the same approximation;
#' where the approximation matters, compare a few and report the spread. The
#' caller's random number state is restored afterwards.
#'
#' @inheritSection compare_estimates What the comparison estimates
#'
#' @export
compare_fitted <- function(x, resolution = 50, x_range = NA,
                           make_newdata = TRUE, seed = 10, ...) {
  if (is.na(x_range[1])) {
    x_range <- return_x_range(x)
  }
  chk_number(seed)
  dot_list <- list(...)
  posterior_list <- vector(mode = "list", length = length(x))
  names(posterior_list) <- names(x)
  # Random pairing affects the reported estimates. Seed it locally so that
  # calls repeat without changing the caller's random number stream.
  # Prediction also samples when ndraws or new group levels are requested.
  with_preserved_rng_state({
    set.seed(seed, sample.kind = "Rejection")
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
      m[sample(seq_len(nrow(m)), n_samples, replace = FALSE), ]
    }, n_samples = n_samples)
  })
  compare_fitted_draws(posterior_list, r_posterior_list, x_vec)
}

#' Compare fitted curves after their posterior rows have been paired
#' @param posterior_list Named posterior matrices.
#' @param paired_draws Matrices in the order used for each comparison.
#' @param x_vec The common prediction concentrations.
#' @return The comparison output of \code{compare_fitted()}.
#' @noRd
compare_fitted_draws <- function(posterior_list, paired_draws, x_vec) {
  posterior_data <- posterior_list |>
      lapply(summarise_posterior, x_vec = x_vec) |>
      bind_rows(.id = "model") |>
      data.frame()
  all_combn <- combn(names(posterior_list), 2, simplify = FALSE)
  diff_list <- lapply(all_combn, function(a, r_list) {
    r_list[[a[1]]] - r_list[[a[2]]]
  }, r_list = paired_draws)
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
