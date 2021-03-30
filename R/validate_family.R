#' validate_family
#'
#' Checks if family is allowed
#'
#' @inheritParams bnec
#'
#' @return An object of class \code{\link[stats]{family}}.
validate_family <- function(family) {
  if (inherits(family, "function")) {
    family <- family()
  } else if (is.character(family)) {
    if (family == "beta_binomial2") {
      family <- get(family)
    } else if (family == "Gamma") {
      family <- get(family)(link = "log")
    } else {
      family <- get(family)() # i.e (link = "identity")
    }
  }
  if (!inherits(family, "family")) {
    stop("argument family either is not an actual family, ",
         "or is of incorrect class")
  }
  fam_tag <- family$family
  if (!fam_tag %in% names(mod_fams)) {
    stop("You have specified family as ",
         fam_tag,
         ", which is not currently implemented. ",
         "bnec only allows: ",
         paste0(mod_fams, collapse = ", "))
  }
  family
}
