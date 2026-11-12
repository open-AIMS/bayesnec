#' Sets distribution based on vector
#'
#' @param x A \code{\link[base]{numeric}} vector.
#' @param support_integer Logical. Should \code{\link[base]{integer}} vectors
#' be supported? Defaults to FALSE.
#' @param trials A \code{\link[base]{numeric}} vector containing the number of
#' trials. Defaults to NULL.
#' @param silence_y_msgs Should messages pertaining to the response be
#' silenced? Default is \code{FALSE.}
#' @param silence_x_msgs Should messages pertaining to the predictor be
#' silenced? Default is \code{FALSE.}
#'
#' @details Checks a vector and recommends a family distribution.
#'
#' @return A \code{\link[base]{character}} vector.
#'
#' @noRd
set_distribution <- function(x, support_integer = FALSE, trials = NULL,
                             silence_y_msgs = FALSE,
                             silence_x_msgs = TRUE) {
  if (inherits(x, "numeric")) {
    if (!is.null(trials) && !silence_y_msgs) {
      stop("The formula has a trials() term, which implies a binomial",
           " response, but the response is not stored as an integer. Store",
           " the number of successes as an integer, for example with",
           " as.integer().")
    }
    if (min(x) >= 0) {
      if (max(x) > 1) {
        if (all(x %% 1 == 0) && !silence_y_msgs) {
          message("The response is made up of whole numbers only, but is",
                  " stored as numeric, so it will be modelled with a Gamma",
                  " distribution. Store it as an integer, for example with",
                  " as.integer(), to model it with a Poisson or negative",
                  " binomial distribution.")
        }
        "Gamma"
      } else {
        "Beta"
      }
    } else {
      "gaussian"
    }
  } else if (inherits(x, "integer")) {
    if (!support_integer && !silence_x_msgs) {
      stop("bayesnec does not currently support integer concentration ",
           "data. Store the predictor as numeric, for example with ",
           "as.numeric().")
    } else {
      if (min(x) >= 0) {
        if (is.null(trials)) {
          "poisson"
        } else {
          "binomial"
        }
      } else {
        # An integer response with negative values, such as a change in a
        # count, is ordinary input and gets gaussian, as the numeric vector
        # does. Without this branch the function returns NULL.
        "gaussian"
      }
    }
  }
}
