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
#' @noRd
set_distribution <- function(x, support_integer = FALSE, trials = NULL) {
  if (inherits(x, "numeric")) {
    if (!is.null(trials)) {
      stop("You have supplied a \"trials\" argument, suggesting you ",
           "wish to model a binomial. Please ensure \"y_var\" is an ",
           "integer representing the number of successes.")
    }
    if (min(x) >= 0) {
      if (max(x) > 1) {
        if (all(x %% 1 == 0)) {
          message("The input \"y_var\" is made up of whole numbers only,",
                  "\neven though the class is numeric. The data will be ",
                  "\nmodelled with a Gamma distribution. Change \"y_var\"",
                  "\nclass to integer if the intention is to use a Poisson or",
                  "\nNegative binomial distribution.")
        }
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
