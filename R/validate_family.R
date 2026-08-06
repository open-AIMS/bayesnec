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
      args <- list(link = "identity")
      args[[paste0("link_", hurdle_dpar(family))]] <- "identity"
      family <- do.call(get(family), args)
    } else {
      family <- get(family)(link = "identity")
    }
  }
  if (!inherits(family, "family")) {
    stop("Argument \"family\" either is not an actual family, ",
         "or is of incorrect class.")
  }
  # brms reports every custom family as "custom", so the tag has to come from
  # family$name for those. Unchanged for the built-in families.
  fam_tag <- family_tag(family)
  if (is_hurdle_family(fam_tag)) {
    link_arg <- paste0("link_", hurdle_dpar(fam_tag))
    # The second block is written as `1 - <non-zero probability>` on the link
    # scale, so anything other than an identity link there would pass that
    # expression through inv_logit and silently model something else entirely.
    if (!identical(family[[link_arg]], "identity")) {
      stop("For the ", fam_tag, " family bayesnec requires ", link_arg,
           " = \"identity\", but you supplied \"", family[[link_arg]],
           "\". Use family = \"", fam_tag, "\" to get the supported links,",
           " or ", fam_tag, "(link = \"identity\", ", link_arg,
           " = \"identity\").", call. = FALSE)
    }
  }
  if (!fam_tag %in% names(mod_fams)) {
    stop("You have specified family as ", fam_tag, ", which is not currently",
         " implemented. bnec only allows: ", paste0(mod_fams, collapse = ", "),
         ".")
  }
  family
}
