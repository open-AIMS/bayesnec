#' bnec_newdata
#' 
#' Create a dataset for predictions
#'
#' @param x An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} as returned by \code{\link{bnec}}, or of class
#' \code{\link{bayesnecjointfit}} as returned by \code{\link{bnec_joint}}.
#' @param resolution A \code{\link[base]{numeric}} vector of length 1 indicating
#' the number of x values over which to predict values.
#' @param x_range A \code{\link[base]{numeric}} vector of length 2 indicating
#' the range of x values over which to make predictions.
#'
#' @return A \code{\link[base]{data.frame}} to be used in predictions. For a
#' \code{\link{bayesnecjointfit}} it holds \code{resolution} rows for each
#' level of the grouping factor, in a column of that name, with the level
#' varying slowest; every other class gives \code{resolution} rows.
#'
#' Where the formula has a \code{disp(~...)} term naming a column other than
#' the predictor, the data frame holds that column at one fixed value, the
#' first level of a factor or the median of a numeric column, and names it in
#' the attribute \code{"bnec_disp_placeholders"}. The value is the one the
#' fitted curve and the estimates are read at, and the mean does not depend on
#' it. \code{\link{posterior_predict}} and \code{\link{predict}} treat a
#' column named there as a placeholder, and draw its value for each posterior
#' draw from the data the fit was made from; see
#' \code{\link{posterior_predict}}. A column whose values have been changed
#' from the placeholder is used as given.
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
  # Delegate, so that this, expand_nec() and posterior_on_grid() build the
  # same grid.
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
