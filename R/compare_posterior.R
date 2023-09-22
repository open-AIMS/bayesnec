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
#' "nec", "n(s)ec", "nsec", "ecx" or "fitted".
#' @param make_newdata Only used if \code{comparison = "fitted"}. Should the
#' user allow the package to create \code{newdata} for predictions?
#' If so, arguments \code{resolution} and \code{x_range} will be used. Defaults
#' to TRUE. See details.
#' @param ... Further arguments that control posterior predictions via
#' \code{\link[brms]{posterior_epred}}.
#' 
#' @inheritParams ecx
#' @inheritParams nsec
#'
#' @details \code{type} "relative" is calculated as the percentage decrease
#' from the maximum predicted value of the response (top) to the minimum
#' predicted value of the response. Type "absolute" (the default) is
#' calculated as the percentage decrease from the maximum value of the
#' response (top) to 0 (or bot for a 4 parameter model fit). Type "direct"
#' provides a direct estimate of the x value for a given y.
#' Note that for the current version, ECx for an "nechorme" (NEC Hormesis)
#' model is estimated at a percent decline from the control.
#' 
#' For \code{hormesis_def}, if "max", then ECx or NSEC values -- i.e.,
#' depending on argument \code{comparison} -- are calculated
#' as a decline from the maximum estimates (i.e. the peak at NEC);
#' if "control", then ECx or NSEC values are calculated relative to the
#' control, which is assumed to be the lowest observed concentration.
#' 
#' The argument \code{make_newdata} is only used if
#' \code{comparison = "fitted"}. It is relevant to those who want the package
#' to create a data.frame from which to make predictions. This is done via
#' \code{\link{bnec_newdata}} and uses arguments \code{resolution} and
#' \code{x_range}. If \code{make_newdata = FALSE} and no additional
#' \code{newdata} argument is provided (via \code{...}), then the predictions
#' are made for the raw data. Else, to generate predictions for a specific
#' user-specific data.frame, set \code{make_newdata = FALSE} and provide
#' an additional data.frame via the \code{newdata} argument. For guidance
#' on how to structure \code{newdata}, see for example
#' \code{\link[brms]{posterior_epred}}.
#' 
#' @seealso \code{\link{bnec}} \code{\link{ecx}} \code{\link{nsec}}
#' \code{\link{nec}} \code{\link{bnec_newdata}}
#'
#' @return A named \code{\link[base]{list}} containing bootstrapped differences
#' in posterior predictions of the \code{\link{bayesnecfit}} or
#' \code{\link{bayesnecfit}} model fits contained in \code{x}. See Details.
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' data(manec_example)
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' ecx4param <- pull_out(manec_example, model = "ecx4param")
#' compare_posterior(list("n(s)ec" = ecx4param, "ecx" = nec4param), ecx_val = 50)
#' }
#'
#' @export
compare_posterior <- function(x, comparison = "n(s)ec", ecx_val = 10,
                              type = "absolute", hormesis_def = "control",
                              sig_val = 0.01, resolution, x_range = NA,
                              make_newdata = TRUE, ...) {
  if (!is.list(x) | is.null(names(x))) {
    stop("Argument x must be a named list.")
  }
  if (!is.character(comparison)) {
    stop("Argument comparison must be a character vector.")
  }
  if (comparison != "fitted") {
    if (missing(resolution)) {
      resolution <- 500
    }
    out <- compare_estimates(x = x, comparison = comparison, ecx_val = ecx_val,
                             type = type, hormesis_def = hormesis_def,
                             sig_val = sig_val, resolution = resolution,
                             x_range = x_range)
  } else {
    if (missing(resolution)) {
      resolution <- 50
    }
    out <- compare_fitted(x = x, resolution = resolution, x_range = x_range,
                          make_newdata = make_newdata, ...)
  }
  out
}
