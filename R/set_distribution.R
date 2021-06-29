#' Sets distribution based on vector
#'
#' @param x A \code{\link[base]{numeric}} vector.
#' @param support_integer Logical. Should \code{\link[base]{integer}} vectors
#' be supported? Defaults to FALSE.
#' @param trials A \code{\link[base]{numeric}} vector containing the number of
#' trials. Defaults to NULL.
#'
#' @details Checks a vector and recommends a family distribution.
#'
#' @return A \code{\link[base]{character}} vector.
#'
set_distribution <- function(x, support_integer = FALSE, trials = NULL) {
  if (inherits(x, "numeric")) {
    if (!is.null(trials)) {
      stop("You have supplied a \"trials\" argument, suggesting you ",
           "wish to model a binomial. Please ensure \"y_var\" is an ",
           "integer representing the number of successes.")
    }
    if (min(x) >= 0) {
      if (max(x) > 1) {
        "Gamma"
      } else {
        "Beta"
      }
    } else {
      "gaussian"
    }
  } else if (inherits(x, "integer")) {
    if (!support_integer) {
      stop("bayesnec does not currently support integer concentration ",
           "data. Please provide a numeric x_var")
    } else {
      if (min(x) >= 0) {
        if (is.null(trials)) {
          "poisson"
        } else {
          "binomial"
        }
      }
    }
  }
}
