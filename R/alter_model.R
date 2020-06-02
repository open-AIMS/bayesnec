#' Alter a package default brmsfit object
#'
#' @param brmodel An object of class \code{\link[brms]{brmsfit}}.
#' @param new_priors An object of class \code{\link[brms]{brmsprior}}.
#' @param new_data A \code{\link[base]{data.frame}}.
#' @return An \code{\link[base]{list}} containing an object of class
#' \code{\link[brms]{brmsfit}}, an object of class
#' \code{\link[brms]{brmsprior}}, and a \code{\link[base]{data.frame}}.
#' @importFrom brms prior_summary make_stancode
alter_model <- function(brmodel, new_priors = NULL, new_data = NULL) {
  brpriors <- prior_summary(brmodel)
  brdata <- brmodel$data
  if (!is.null(new_priors)) {
    match_condition <- vector(mode = "logical", length = nrow(new_priors))
    for (i in seq_len(nrow(new_priors))) {
      z <- intersect(which(brpriors$nlpar == new_priors$nlpar[i]),
                     which(brpriors$coef == new_priors$coef[i]))
      match_condition[i] <- length(z) == 1
      if (match_condition[i]) {
        brpriors[z, ] <- new_priors[i, ]
      }
    }
    if (!all(match_condition)) {
      stop("New prior does not match any prior required to run this function")
    }
    brmodel$prior <- brpriors
  }
  if (!is.null(new_data)) {
    if (!all(names(brdata) %in% names(new_data))) {
      stop("New dataset names do not match required to run this function")
    } else {
      brdata <- new_data[, names(brdata)]
    }
  }
  if (!is.null(new_priors) | !is.null(new_data)) {
    brformula <- brmodel$formula
    brfamily <- brmodel$family
    new_code <- make_stancode(brformula, data = brdata,
      family = brfamily, prior = brpriors)
    brmodel$model <- new_code
    brmodel$fit@stanmodel@model_code[[1]] <- new_code[[1]]
  }
  list(brmodel = brmodel,
       brdata = brdata,
       brpriors = brpriors)
}
