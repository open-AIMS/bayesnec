#' Did the caller choose a link, or only a family?
#'
#' @param expr The \strong{unevaluated} expression a caller supplied as
#' \code{family}, from \code{substitute()} or \code{match.call()}.
#'
#' @details \code{\link{bnec}} fits every parameter on the identity link so
#' that \code{top}, \code{bot} and \code{nec} stay on the response scale, and
#' assigns that link itself unless the caller asked for another. Telling the two
#' apart needs the expression rather than the evaluated object, because
#' \code{Beta()} and \code{Beta(link = "logit")} produce identical family
#' objects and mean different things.
#'
#' \itemize{
#'   \item \code{"Beta"}, \code{Beta}, \code{Beta()} --- \code{"none"}. A
#'     family was named and nothing more.
#'   \item \code{Beta(link = "logit")} --- \code{"chosen"}.
#'   \item a symbol holding a family object, as in
#'     \code{fam <- Beta(link = "logit"); bnec(family = fam)} ---
#'     \code{"symbol"}. Not knowable from the expression, so the object's own
#'     link is honoured rather than silently replaced, and
#'     \code{\link{validate_family}} says which link it took.
#' }
#'
#' A symbol holding a \emph{constructor} rather than an object, which is the
#' plain \code{Beta} case, is separated from that by
#' \code{\link{validate_family}}, which has the value.
#'
#' @return A \code{\link[base]{character}} string.
#'
#' @noRd
family_link_source <- function(expr) {
  if (missing(expr) || is.null(expr)) {
    return("none")
  }
  if (is.character(expr)) {
    return("none")
  }
  if (is.name(expr)) {
    return("symbol")
  }
  if (is.call(expr) && "link" %in% names(as.list(expr))) {
    return("chosen")
  }
  "none"
}

#' Links bayesnec will fit on
#'
#' @details The identity link is what \code{\link{bnec}} assigns and what the
#' JSS article describes. \code{log} and \code{logit} are honoured when asked
#' for explicitly, because \code{?models} and \code{vignette("example2b")}
#' describe them as supported and \code{\link{check_models}} has a gate for
#' them. Every other link is refused: those are reachable at present only by
#' accident, they put \code{top}, \code{bot} and \code{nec} on a scale the
#' package does not document, and no part of bayesnec has been tested against
#' them.
#'
#' @return A \code{\link[base]{character}} vector.
#'
#' @noRd
supported_links <- function() {
  c("identity", "log", "logit")
}

#' Rebuild a family with the links bayesnec fits on
#'
#' @param fam A \code{\link[base]{character}} string naming the family, either
#' as the tag brms reports or as the constructor.
#'
#' @details Identity on the mean, and identity again on the second block of a
#' two-block family, whose \code{1 - <probability>} expression would otherwise
#' be passed through an inverse link and model something else entirely.
#'
#' \code{mod_fams} maps the tag to the constructor, and the two differ for one
#' family: the tag is \code{beta} and the constructor is \code{Beta}, so
#' \code{get("beta")} resolves to \code{base::beta()}. Mapping here rather
#' than at each call site is what makes \code{validate_family("beta")} work,
#' which it previously did not --- a user reading the family off a fitted object
#' and passing it back got \code{unused argument (link = "identity")}.
#'
#' @return An object of class \code{\link[stats]{family}}.
#'
#' @noRd
bnec_default_family <- function(fam) {
  ctor <- if (fam %in% names(mod_fams)) unname(mod_fams[[fam]]) else fam
  if (!exists(ctor, mode = "function")) {
    stop("You have specified family as ", fam, ", which is not currently",
         " implemented. bnec only allows: ", paste0(mod_fams, collapse = ", "),
         ".", call. = FALSE)
  }
  args <- list(link = "identity")
  if (is_hurdle_family(fam)) {
    args[[paste0("link_", hurdle_dpar(fam))]] <- "identity"
  }
  do.call(get(ctor, mode = "function"), args)
}

#' validate_family
#'
#' Checks that a family is allowed, and assigns the link bayesnec fits on
#' unless the caller chose one.
#'
#' @param family A family \code{\link[base]{character}} string, a
#' \code{\link[stats]{family}} function, or a family object.
#' @param link_source The output of \code{\link{family_link_source}} for the
#' expression the caller supplied. The default, \code{"none"}, assigns the
#' identity link.
#'
#' @details Before this, which link a fit used depended on how the family was
#' written, and the difference was silent. \code{"Beta"} gave identity;
#' \code{Beta} and \code{Beta()} gave \strong{logit}; \code{Gamma()} gave
#' \strong{inverse}. In the latter cases the curve was fitted to a transform of
#' the mean while \code{top}, \code{bot} and \code{nec} were reported as
#' though they were on the response scale, which is the property the identity
#' link exists to preserve. See #256.
#'
#' @return An object of class \code{\link[stats]{family}}.
#'
#' @noRd
validate_family <- function(family, link_source = "none") {
  if (is.character(family)) {
    family <- bnec_default_family(family)
  } else if (inherits(family, "function")) {
    # An unevaluated constructor names a family and nothing more, so the link
    # is bayesnec's to assign.
    family <- bnec_default_family(family()$family)
  } else if (inherits(family, "family") &&
             !identical(link_source, "chosen") &&
             !identical(link_source, "symbol")) {
    # An evaluated call carrying no link argument: Beta(), Gamma(). Same case.
    family <- bnec_default_family(family$family)
  }
  if (!inherits(family, "family")) {
    stop("Argument \"family\" either is not an actual family, ",
         "or is of incorrect class.")
  }
  if (!family$link %in% supported_links()) {
    stop("bayesnec fits on the ", paste(supported_links(), collapse = ", "),
         " links, but you supplied \"", family$link, "\" for the ",
         family$family, " family. Pass family = \"", family$family,
         "\" to fit on the identity link, which is what bayesnec assigns and",
         " what keeps top, bot and nec on the response scale.", call. = FALSE)
  }
  if (identical(link_source, "symbol") && !identical(family$link, "identity")) {
    message("Fitting on the \"", family$link, "\" link, taken from the family",
            " object supplied. bayesnec assigns the identity link where it",
            " chooses one; pass family = \"", family$family, "\" for that.")
  }
  fam_tag <- family$family
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
