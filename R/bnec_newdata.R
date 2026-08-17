#' bnec_newdata
#' 
#' Create a dataset for predictions
#'
#' @param x An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} as returned by \code{\link{bnec}}.
#' @param resolution A \code{\link[base]{numeric}} vector of length 1 indicating
#' the number of x values over which to predict values.
#' @param x_range A \code{\link[base]{numeric}} vector of length 2 indicating
#' the range of x values over which to make predictions.
#'
#' @return A \code{\link[base]{data.frame}} to be used in predictions.
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' # Make fine resolution, predict out of range
#' newdata <- bnec_newdata(nec4param, resolution = 200, x_range = c(0, 4))
#' nrow(newdata) == 200
#' all(range(newdata$x) == c(0, 4))
#' newdata2 <- bnec_newdata(manec_example) # default size
#' nrow(newdata2) == 100
#' }
#' 
#' @export
bnec_newdata <- function(x, resolution = 100, x_range = NA) {
  UseMethod("bnec_newdata")
}

#' bnec_newdata.bayesnecfit
#' 
#' Create a dataset for predictions
#'
#' @inheritParams bnec_newdata
#' @inherit bnec_newdata description return examples
#' 
#' @importFrom stats model.frame
#' @noRd
#' @export
bnec_newdata.bayesnecfit <- function(x, resolution = 100, x_range = NA) {
  check_args_newdata(resolution, x_range)
  # Delegates so this, expand_nec() and posterior_on_grid() cannot build
  # different grids. This function used to test any(is.na(x_range)) while the
  # other two tested is.na(x_range[1]); those disagreed for a partially
  # specified range, which check_args_newdata() now rejects outright. See #211.
  prediction_grid(x$fit, x$bayesnecformula, x_range = x_range,
                  resolution = resolution)$newdata
}

#' bnec_newdata.bayesmanecfit
#' 
#' Create a dataset for predictions
#'
#' @inheritParams bnec_newdata
#' @inherit bnec_newdata description return examples
#' @noRd
#' @export
bnec_newdata.bayesmanecfit <- function(x, resolution = 100, x_range = NA) {
  model_set <- names(x$mod_fits)
  bayesnecfit_x <- pull_out(x, model = model_set[1]) |>
    suppressMessages()
  bnec_newdata(bayesnecfit_x, resolution, x_range)
}
