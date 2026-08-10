#' validate_family
#'
#' Checks if family is allowed
#'
#' @param family A family \code{\link[base]{character}} string or a 
#' \code{\link[stats]{family}} function.
#'
#' @return An object of class \code{\link[stats]{family}}.
#'
#' @noRd
validate_family <- function(family) {
  if (inherits(family, "function")) {
    family <- family()
  } else if (is.character(family)) {
    # Hurdle families carry a second link for the hurdle probability. bayesnec
    # keeps every parameter on the natural response scale, so both links must
    # be set to identity, not just the one.
    if (is_hurdle_family(family)) {
      family <- get(family)(link = "identity", link_hu = "identity")
    } else {
      family <- get(family)(link = "identity")
    }
  }
  if (!inherits(family, "family")) {
    stop("Argument \"family\" either is not an actual family, ",
         "or is of incorrect class.")
  }
  fam_tag <- family$family
  if (is_hurdle_family(fam_tag) && !identical(family$link_hu, "identity")) {
    # The hu sub-model is written as `1 - <survival curve>` on the link scale,
    # so anything other than an identity link_hu would pass that expression
    # through inv_logit and silently model something else entirely.
    stop("For the ", fam_tag, " family bayesnec requires",
         " link_hu = \"identity\", but you supplied \"", family$link_hu,
         "\". Use family = \"", fam_tag, "\" to get the supported links, or",
         " ", fam_tag, "(link = \"identity\", link_hu = \"identity\").",
         call. = FALSE)
  }
  if (!fam_tag %in% names(mod_fams)) {
    stop("You have specified family as ", fam_tag, ", which is not currently",
         " implemented. bnec only allows: ", paste0(mod_fams, collapse = ", "),
         ".")
  }
  family
}
