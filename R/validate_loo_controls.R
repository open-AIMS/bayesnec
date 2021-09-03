#' validate_loo_controls
#'
#' Checks if argument loo_controls has correct format
#'
#' @inheritParams bnec
#'
#' @param fam_tag A \code{\link[base]{character}} string defining the family
#' distribution.
#'
#' @return A named \code{\link[base]{list}}.
#'
#' @noRd
validate_loo_controls <- function(loo_controls, fam_tag) {
  if (!is.list(loo_controls)) {
    stop("loo_controls should be a named list of two elements. See ?bnec")
  } else if (!all(names(loo_controls) %in% c("fitting", "weights"))) {
    stop("loo_controls list names are incorrect. See ?bnec")
  }
  if ("fitting" %in% names(loo_controls)) {
    if (!is.list(loo_controls$fitting)) {
      stop("Element \"fitting\" of loo_controls should be a list. See ?bnec")
    }
    if ("pointwise" %in% names(loo_controls$fitting)) {
      if (loo_controls$fitting$pointwise == TRUE & fam_tag == "custom") {
        stop("You cannot currently set loo_controls$fitting$pointwise = TRUE",
             " for custom families.")
      }
    }
  } else {
    loo_controls$fitting <- list()
  }
  if ("weights" %in% names(loo_controls)) {
    if (!is.list(loo_controls$weights)) {
      stop("Element \"weights\" of loo_controls should be a list. See ?bnec")
    }
  } else {
    loo_controls$weights <- list()
  }
  loo_controls
}
