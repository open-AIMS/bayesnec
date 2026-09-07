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
#' @export
compare_estimates <- function(x, comparison = "n(s)ec", ecx_val = 10,
                              type = "absolute",
                              sig_val = 0.01, resolution = 100, x_range = NA) {
  if ((comparison %in% c("nec", "n(s)ec", "ecx", "nsec")) == FALSE) {
    stop("comparison must be one of nec, n(s)ec, ecx or nsec.")
  }
  chk_numeric(ecx_val)
  # The same validator ecx() uses, rather than a second copy of the vocabulary.
  # The copy that stood here still listed the 2.1.3 three-value set, so it
  # refused type = "range" -- which is the name the rename warning gives users
  # for the behaviour they had, making the migration it names impossible from
  # here and from compare_posterior(), which forwards to this function.
  type <- validate_ecx_type(type, match.call())
  # Warned once for the call rather than once per fit in x: the ecx() calls
  # below name type explicitly, so each would otherwise repeat the message.
  # Same reasoning as ecx.bayesmanecfit. See D15 ruling 8.
  warned <- options(bayesnec.relative_warned = TRUE)
  on.exit(options(warned), add = TRUE)
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
    m[sample(seq_len(n_samples), replace = FALSE)]
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
