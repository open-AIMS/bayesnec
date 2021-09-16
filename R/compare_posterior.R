#' compare_posterior
#'
#' Extracts posterior predicted values from a list of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}} model fits and
#' compares these via bootstrap re sampling.
#'
#' @param x A named \code{\link[base]{list}} of objects of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}} returned by
#' \code{\link{bnec}}.
#' @param comparison The posterior predictions to compare, takes values of
#' "nec", "nsec", "ecx" or "fitted".
#'
#' @inheritParams ecx
#' @inheritParams nsec
#'
#' @seealso \code{\link{bnec}}
#'
#' @return A named \code{\link[base]{list}} containing bootstrapped differences
#' in posterior predictions of the \code{\link{bayesnecfit}} or
#' \code{\link{bayesnecfit}} model fits contained in \code{x}. See Details.
#'
#' @importFrom stats quantile predict
#' @importFrom dplyr %>% mutate bind_rows group_by summarise ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect everything
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' data(manec_example)
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' ecx4param <- pull_out(manec_example, model = "ecx4param")
#' compare_posterior(list("nec" = ecx4param, "ecx" = nec4param), ecx_val = 50)
#' }
#'
#' @export
compare_posterior <- function(x, comparison = "nec", ecx_val = 10,
                              type = "absolute", hormesis_def = "control",
                              sig_val = 0.01, precision, x_range = NA) {
  if (!is.list(x) | is.null(names(x))) {
    stop("Argument x must be a named list.")
  }
  if (!is.character(comparison)) {
    stop("Argument comparison must be a character vector.")
  }
  if (comparison != "fitted") {
    if (missing(precision)) {
      precision <- 500
    }
    out <- compare_endpoints(x = x, comparison = comparison, ecx_val = ecx_val,
                             type = type, hormesis_def = hormesis_def,
                             sig_val = sig_val, precision = precision,
                             x_range = x_range)
  } else {
    if (missing(precision)) {
      precision <- 50
    }
    out <- compare_fitted(x = x, precision = precision, x_range = x_range)
  }
  out
}
