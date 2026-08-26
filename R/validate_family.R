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

#' Does the response distribution restrict the range of the mean?
#'
#' @param family A \code{\link[stats]{family}} object, already through
#' \code{\link{validate_family}}.
#'
#' @details Returns \code{TRUE} when a value of \code{mu} outside some interval
#' would make the likelihood undefined, so that a proposal reaching it is
#' rejected by Stan rather than merely improbable.
#'
#' Under a \code{log} or \code{logit} link \code{mu} is the linear predictor and
#' spans the whole real line, so nothing is constrained. Under any other link
#' every family \pkg{bayesnec} accepts except \code{gaussian} restricts the
#' mean: to (0, 1) for bernoulli, beta, binomial, beta_binomial and the
#' \code{hu} block of the hurdle families, and to (0, Inf) for Gamma, poisson,
#' negbinomial, the zero-inflated counts and the \code{mu} block of
#' \code{hurdle_gamma}. The distinction that matters here is only whether the
#' interval is bounded at all, not where its bounds are.
#'
#' \strong{This is the interim form of the predicate proposed in #256}, written
#' inline so that #245 does not wait on it. It is deliberately the coarsest
#' correct test: it answers only the question \code{\link{add_brm_defaults}}
#' asks, and #256 should replace it with the full predicate rather than adding a
#' second copy beside it.
#'
#' @return A \code{\link[base]{logical}} of length one.
#'
#' @noRd
mu_is_constrained <- function(family) {
  if (is.null(family) || is.null(family$link)) {
    return(FALSE)
  }
  if (family$link %in% c("log", "logit")) {
    return(FALSE)
  }
  !identical(family$family, "gaussian")
}
