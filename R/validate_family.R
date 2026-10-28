#' Did the caller choose a link, or only a family?
#'
#' @param expr The \strong{unevaluated} expression a caller supplied as
#' \code{family}, from \code{substitute()} or \code{match.call()}.
#' @param env The \code{\link[base]{environment}} the expression came from,
#' used only to resolve the constructor so that a call can be matched against
#' its formals.
#'
#' @details \code{\link{bnec}} assigns the identity link unless the caller
#' asked for another. Telling the two apart needs the expression, because
#' \code{Beta()} and \code{Beta(link = "logit")} give identical objects.
#'
#' \code{"Beta"} and \code{Beta()} return \code{"none"}.
#' \code{Beta(link = "logit")} returns \code{"link"}, and
#' \code{hurdle_gamma(link_hu = "logit")} returns \code{"link_hu"}: a block the
#' caller left alone is still bayesnec's to assign. Anything else returns
#' \code{"symbol"}, because its link cannot be read: a variable holding a
#' family, the evaluated family \code{do.call()} inlines, or \code{fit$family}.
#' The object's own link is then honoured, not replaced.
#' \code{\link{validate_family}} separates a symbol holding a constructor, such
#' as \code{Beta}.
#'
#' @return A \code{\link[base]{character}} vector: \code{"none"},
#' \code{"symbol"}, or the link arguments the caller wrote.
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
  if (!is.call(expr)) {
    # An already-evaluated family, which is what do.call() puts in the call it
    # builds. Not readable, so it is treated as a variable holding one.
    return("symbol")
  }
  # Match the call against the constructor's formals before reading names, so
  # that a positional link, as in Beta("logit"), is read as a link. Look the
  # constructor up in function mode, as R does, so that an unrelated
  # `Beta <- 0.5` in the caller's workspace does not hide it.
  ctor <- if (is.name(expr[[1]])) {
    tryCatch(get(as.character(expr[[1]]), envir = env, mode = "function"),
             error = function(e) NULL)
  } else {
    tryCatch(eval(expr[[1]], envir = env), error = function(e) NULL)
  }
  matched <- if (is.function(ctor)) {
    tryCatch(as.list(match.call(ctor, expr))[-1], error = function(e) NULL)
  }
  # Only the callee is evaluated, never the call. A `link` formal separates a
  # family constructor from an ordinary call that returns a family, such as
  # fit$family, whose link cannot be read.
  if (is.null(matched) || !"link" %in% names(formals(ctor))) {
    return("symbol")
  }
  chosen <- intersect(names(matched), mean_link_args())
  if (length(chosen) > 0) {
    chosen
  } else {
    "none"
  }
}

#' The link arguments a link_source says the caller wrote
#'
#' @param link_source The output of \code{\link{family_link_source}}, or the
#' shorthand \code{"chosen"} for a family whose links are all the caller's.
#'
#' @return A \code{\link[base]{character}} vector.
#'
#' @noRd
chosen_link_args <- function(link_source) {
  if (identical(link_source, "none")) {
    return(character(0))
  }
  if (identical(link_source, "chosen") || identical(link_source, "symbol")) {
    return(mean_link_args())
  }
  intersect(link_source, mean_link_args())
}

#' The links of a supplied family that survive the rebuild
#'
#' @param family An object of class \code{\link[stats]{family}}.
#' @param link_source The output of \code{\link{family_link_source}}.
#'
#' @details The links the caller wrote, and the dispersion links, which
#' bayesnec does not assign: rebuilding the family without them would return a
#' dispersion sub-model to the family's default link. Where one cannot be
#' carried, \code{\link{bnec_default_family}} refuses rather than drops it.
#'
#' @return A named \code{\link[base]{list}}.
#'
#' @noRd
kept_links <- function(family, link_source) {
  present <- grep("^link", names(family), value = TRUE)
  dispersion <- setdiff(present, c("link", mean_link_args()))
  wanted <- intersect(present, c(dispersion, chosen_link_args(link_source)))
  out <- unclass(family)[wanted]
  out[vapply(out, function(x) is.character(x) && length(x) == 1, logical(1))]
}

#' Constructor arguments that state a link bayesnec fits a curve on
#'
#' @details \code{link} is the mean, and \code{link_hu} and \code{link_zi} the
#' second block of a two-block family. A curve on that block is written as
#' \code{1 - <probability>}, so its link must be the identity.
#'
#' \code{link_phi} and \code{link_shape} are excluded on purpose: reading a
#' dispersion link as a choice of mean link would fit
#' \code{Beta(link_phi = "log")} on beta's default logit.
#'
#' @return A \code{\link[base]{character}} vector.
#'
#' @noRd
mean_link_args <- function() {
  c("link", "link_hu", "link_zi")
}

#' Links bayesnec will fit on
#'
#' @details \code{\link{bnec}} assigns the identity link. \code{log} and
#' \code{logit} are honoured when asked for, as \code{?models} documents them.
#' Every other link is refused, since no part of bayesnec is tested on it.
#'
#' @return A \code{\link[base]{character}} vector.
#'
#' @noRd
supported_links <- function() {
  c("identity", "log", "logit")
}

#' The tag bayesnec keys on, for a family brms reports under another
#'
#' @param family A \code{\link[stats]{family}} object, or a
#' \code{\link[base]{character}} string naming a family.
#'
#' @details \pkg{brms} reports \code{gamma} where \code{stats::Gamma} reports
#' \code{Gamma}, and bayesnec keys its tables and checks on the latter. Pass a
#' family read off a \code{brmsfit} through here before comparing it or keying
#' anything on it; otherwise \code{check_models} keeps the linear-decay
#' equations it drops for Gamma.
#'
#' @return A \code{\link[base]{character}} string, or \code{NA_character_}
#' where \code{family} names none.
#'
#' @noRd
canonical_fam_tag <- function(family) {
  fam <- if (inherits(family, "family")) family$family else family
  if (is.null(fam)) {
    return(NA_character_)
  }
  aliases <- c(gamma = "Gamma")
  if (fam %in% names(aliases)) unname(aliases[[fam]]) else fam
}

#' Rebuild a family with the links bayesnec fits on
#'
#' @param fam A \code{\link[base]{character}} string naming the family, either
#' as the tag brms reports or as the constructor.
#' @param keep A named \code{\link[base]{list}} of link arguments to carry
#' through rather than assign, from \code{\link{kept_links}}.
#'
#' @details Identity on the mean, and on the second block of a two-block
#' family, whose \code{1 - <probability>} expression would otherwise pass
#' through an inverse link.
#'
#' The tag and the constructor differ in case for two families: \code{beta}
#' against \code{Beta}, where \code{get("beta")} finds \code{base::beta()},
#' and \pkg{brms}'s \code{gamma} against \code{stats::Gamma}. Map them here,
#' through \code{mod_fams} and \code{\link{canonical_fam_tag}}, not at each
#' call site.
#'
#' @return An object of class \code{\link[stats]{family}}.
#'
#' @importFrom brms brmsfamily
#'
#' @noRd
bnec_default_family <- function(fam, keep = list()) {
  fam <- canonical_fam_tag(fam)
  ctor <- if (fam %in% names(mod_fams)) unname(mod_fams[[fam]]) else fam
  # Check membership before get(): exists() alone admits any function, so
  # validate_family("t") would fail on an unused `link` argument.
  if (!ctor %in% mod_fams || !exists(ctor, mode = "function")) {
    stop("You have specified family as ", fam, ", which is not currently",
         " implemented. bnec only allows: ", paste0(mod_fams, collapse = ", "),
         ".", call. = FALSE)
  }
  ctor_fn <- get(ctor, mode = "function")
  args <- list(link = "identity")
  if (is_hurdle_family(fam)) {
    args[[paste0("link_", hurdle_dpar(fam))]] <- "identity"
  }
  # What the caller wrote, and the dispersion links, override the assignment.
  takeable <- names(keep) %in% names(formals(ctor_fn))
  args[names(keep)[takeable]] <- keep[takeable]
  # Drop a link the constructor cannot take where it is only the family's
  # default: kept_links() reports every link on the object, written or not.
  spare <- keep[!takeable]
  if (length(spare) > 0) {
    stock <- do.call(brmsfamily, list(family = fam))
    spare <- spare[!vapply(names(spare),
                           function(nm) identical(spare[[nm]], stock[[nm]]),
                           logical(1))]
  }
  if (length(spare) == 0) {
    return(do.call(ctor_fn, args))
  }
  # The stats constructors for gaussian and Gamma take only `link`, and brms
  # rebuilds the family from its own constructor, so a field set afterwards is
  # lost. Use brmsfamily() to carry a dispersion link the caller wrote.
  out <- do.call(brmsfamily, c(list(family = fam), args, spare))
  if (!out$family %in% names(mod_fams)) {
    # brmsfamily() reports `gamma`, which brms dispatches on and bayesnec does
    # not key on. Refuse rather than return a tag that would keep the
    # linear-decay equations in the set.
    stop("bayesnec cannot carry ", paste(names(spare), collapse = ", "),
         " on the ", fam,
         " family. Model dispersion with disp(~x) in the formula instead,",
         " which is valid under any link. See ?bayesnecformula.",
         call. = FALSE)
  }
  out
}

#' Remove the marker validate_family() uses to stay idempotent
#'
#' @param family An object of class \code{\link[stats]{family}}.
#'
#' @details The marker is private. It is dropped wherever the family leaves
#' bayesnec --- into a \code{brmsfit}, or into a returned fit object --- so
#' that it is never serialised into anything a user holds or compares.
#'
#' @return An object of class \code{\link[stats]{family}}.
#'
#' @noRd
unmark_family <- function(family) {
  attr(family, "bayesnec_validated") <- NULL
  family
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
#' @details The link must not depend on how the family was written:
#' \code{"Beta"}, \code{Beta} and \code{Beta()} all fit on the identity link,
#' so that \code{top}, \code{bot} and \code{nec} stay on the response scale.
#'
#' @return An object of class \code{\link[stats]{family}}.
#'
#' @noRd
validate_family <- function(family, link_source = "none") {
  # Idempotent: bnec_group() and bnec_hurdle() validate the family and pass
  # the object to bnec(), which would otherwise read its link again and report
  # it. Only the rebuild and the message are skipped; the checks below run.
  validated <- isTRUE(attr(family, "bayesnec_validated"))
  if (!validated) {
    if (is.character(family)) {
      family <- bnec_default_family(family)
    } else if (inherits(family, "function")) {
      # A bare constructor names a family and nothing more, so bayesnec assigns
      # the link. A function that is not a constructor is refused as an
      # unimplemented family.
      tag <- tryCatch(family()$family, error = function(e) NULL)
      if (!is.character(tag) || length(tag) != 1) {
        stop("Argument \"family\" is a function, but calling it does not",
             " produce a family. bnec only allows: ",
             paste0(mod_fams, collapse = ", "), ".", call. = FALSE)
      }
      family <- bnec_default_family(tag)
    } else if (inherits(family, "family")) {
      # Rebuild every evaluated family: a constructor's closure can hold the
      # environment it was called in, which would then be sent to every worker.
      # Keep each link the caller wrote, per argument, and assign the rest.
      # Dispersion links are always kept; see kept_links().
      family <- bnec_default_family(family$family,
                                    keep = kept_links(family, link_source))
    }
  }
  if (!inherits(family, "family")) {
    stop("Argument \"family\" either is not an actual family, ",
         "or is of incorrect class.")
  }
  # Rebuild a family under a tag only brms uses, such as one read off a fitted
  # object, under bayesnec's tag, keeping every link it holds.
  if (!family$family %in% names(mod_fams) &&
        !identical(canonical_fam_tag(family$family), family$family)) {
    family <- bnec_default_family(family$family,
                                  keep = kept_links(family, "chosen"))
  }
  # Ordered before the link check: for a family bayesnec does not implement at
  # all, naming the link it was given is the less useful of the two errors.
  if (!family$family %in% names(mod_fams)) {
    stop("You have specified family as ", family$family, ", which is not",
         " currently implemented. bnec only allows: ",
         paste0(mod_fams, collapse = ", "), ".", call. = FALSE)
  }
  if (!family$link %in% supported_links()) {
    stop("bayesnec fits on the ", paste(supported_links(), collapse = ", "),
         " links, but you supplied \"", family$link, "\" for the ",
         family$family, " family. Pass family = \"", family$family,
         "\" to fit on the identity link, which is what bayesnec assigns and",
         " what keeps top, bot and nec on the response scale.", call. = FALSE)
  }
  if (!validated && identical(link_source, "symbol") &&
        !identical(family$link, "identity")) {
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
  attr(family, "bayesnec_validated") <- TRUE
  family
}
