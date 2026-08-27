#' Did the caller choose a link, or only a family?
#'
#' @param expr The \strong{unevaluated} expression a caller supplied as
#' \code{family}, from \code{substitute()} or \code{match.call()}.
#' @param env The \code{\link[base]{environment}} the expression came from,
#' used only to resolve the constructor so that a call can be matched against
#' its formals.
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
#'   \item \code{Beta(link = "logit")}, \code{Beta("logit")},
#'     \code{hurdle_gamma(link_hu = "logit")} --- \code{"chosen"}.
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
family_link_source <- function(expr, env = parent.frame()) {
  if (missing(expr) || is.null(expr)) {
    return("none")
  }
  if (is.character(expr)) {
    return("none")
  }
  if (is.name(expr)) {
    return("symbol")
  }
  if (is.call(expr)) {
    # `link` is the first formal of every family constructor, so `Beta("logit")`
    # and `binomial("probit")` state a link as plainly as the named form does.
    # The call is matched against the constructor's own formals before its
    # argument names are read, so that a positional or partially matched link
    # is not read as naming a family and nothing more. Resolving the
    # constructor can fail (it need not be on the search path here), and the
    # raw names are the fallback.
    args <- as.list(expr)[-1]
    ctor <- tryCatch(eval(expr[[1]], envir = env), error = function(e) NULL)
    if (is.function(ctor)) {
      matched <- tryCatch(as.list(match.call(ctor, expr))[-1],
                          error = function(e) NULL)
      if (!is.null(matched)) {
        args <- matched
      }
    }
    if (any(names(args) %in% mean_link_args())) {
      return("chosen")
    }
  }
  "none"
}

#' Constructor arguments that state a link bayesnec fits a curve on
#'
#' @details \code{link} is the mean. \code{link_hu} and \code{link_zi} are the
#' second block of a two-block family, which \code{\link{bnec}} writes as
#' \code{1 - <probability>} and so also fits a curve expression on. Writing any
#' of these is a statement about a scale \code{top}, \code{bot} and \code{nec}
#' are reported on.
#'
#' \code{link_phi} and \code{link_shape} are deliberately excluded: they are
#' dispersion links, bayesnec puts no curve on them, and reading one as a
#' choice of \emph{mean} link would reintroduce exactly the silent
#' misspecification #256 removes --- \code{Beta(link_phi = "log")} would be
#' fitted on beta's default logit.
#'
#' @return A \code{\link[base]{character}} vector.
#'
#' @noRd
mean_link_args <- function() {
  c("link", "link_hu", "link_zi")
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
  # Membership is checked before get(): `exists(ctor, mode = "function")` alone
  # admits any string naming any function, so validate_family("t") reached
  # do.call() and failed with `unused argument (link = "identity")` rather than
  # saying the family is not implemented -- the same error validate_family()
  # used to give for "beta".
  if (!ctor %in% mod_fams || !exists(ctor, mode = "function")) {
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
  # Idempotent. bnec_group() and bnec_hurdle() validate the family and then
  # pass the object on to bnec(), where the expression is the symbol holding it
  # and the link would be read a second time -- announcing, once per group
  # level, a link the caller had already chosen and advising them to drop it.
  if (isTRUE(attr(family, "bayesnec_validated"))) {
    return(family)
  }
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
  attr(family, "bayesnec_validated") <- TRUE
  family
}
