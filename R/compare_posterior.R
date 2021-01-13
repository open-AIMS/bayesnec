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
                              sig_val = 0.01, precision, x_range = NA) {
  if (comparison != "fitted") {
    if (missing(precision)) { precision <- 500 }
    out <- compare_endpoints(x = x, comparison = comparison, ecx_val = ecx_val,
                             type = type, hormesis_def = hormesis_def,
                             sig_val = sig_val, precision = precision, x_range = x_range)
     } else {
       if (missing(precision)) { precision <- 50 }
       out <- compare_fitted(x = x, precision = precision, x_range = x_range)

  }
 return(out)
}

summarise_posterior <- function(mat, x_vec) {
  cbind(x = x_vec, data.frame(t(apply(mat, 2, estimates_summary))))
}

