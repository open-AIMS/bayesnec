#' Class \code{bayesnecgroupfit} of models fitted separately at each level of a
#' factor
#'
#' Returned by \code{\link{bnec_group}}. Each element of \code{fits} is an
#' ordinary \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}, so
#' everything that works on those works per level.
#'
#' @name bayesnecgroupfit-class
#' @aliases bayesnecgroupfit bayesnecgroupfit-class
#' @docType class
#'
#' @details See \code{methods(class = "bayesnecgroupfit")} for an overview of
#' available methods.
#'
#' @slot fits A named \code{\link[base]{list}} of fits, one per level.
#' @slot group_var The name of the grouping column.
#' @slot levels The factor levels, in order.
#' @slot formula The \code{\link{bayesnecformula}} fitted at every level.
#' @slot data The full \code{\link[base]{data.frame}}, all levels.
#' @slot family The family, chosen once and shared by every level.
#' @slot n The number of observations per level.
#'
#' @seealso \code{\link{bnec_group}}, \code{\link{crossed_group_weights}},
#' \code{\link{bayesnecfit}}, \code{\link{bayesmanecfit}}
NULL

#' @noRd
is_bayesnecgroupfit <- function(x) {
  inherits(x, "bayesnecgroupfit")
}

#' Apply a per-fit function across the levels of a bayesnecgroupfit
#'
#' Every element of \code{fits} is an ordinary bayesnec object, so the
#' level-aware forms of \code{nec}, \code{ecx} and friends are a map rather than
#' a reimplementation. Keeping it that way is deliberate: stage 1 of #33 must
#' not need changes inside \code{nec()}/\code{ecx()}/\code{bnec_newdata()},
#' because those are the files the toxval migration moves.
#'
#' @param object An object of class \code{\link{bayesnecgroupfit}}.
#' @param fun A function taking a fit as its first argument.
#' @param ... Passed to \code{fun}.
#'
#' @return A named \code{\link[base]{list}}.
#'
#' @noRd
group_lapply <- function(object, fun, ...) {
  out <- lapply(object$fits, fun, ...)
  names(out) <- object$levels
  out
}

#' Per-level toxicity estimates from a bayesnecgroupfit
#'
#' @param object An object of class \code{\link{bayesnecgroupfit}}.
#' @param what The name of the calling method, for the error message.
#' @param fun A function taking a fit as its first argument.
#' @param ... Passed to the underlying method.
#'
#' @details The columns are taken from the \emph{names} of the vector each
#' underlying method returns, not from its positions. \code{nec()} and
#' \code{ecx()} both take \code{prob_vals}, so the returned vector is
#' \code{length(prob_vals)} long and named for the quantiles actually asked
#' for; reading positions 1 to 3 silently truncated a longer request and
#' mislabelled a reordered one.
#'
#' \code{posterior = TRUE} is refused rather than accommodated. It makes the
#' underlying methods return the full draw vector instead of a summary, and a
#' positional table then reported draws 1, 2 and 3 as an estimate and its
#' credible interval --- wrong, plausible-looking and silent. There is no
#' sensible one-row-per-level table of posteriors, so the user is sent to the
#' per-level fits, where the posteriors are exactly what they already were.
#'
#' @return A \code{\link[base]{data.frame}} with one row per level.
#'
#' @noRd
group_estimate_table <- function(object, what, fun, ...) {
  dots <- list(...)
  if (isTRUE(dots$posterior)) {
    stop(what, " on a bayesnecgroupfit returns one row per level, which a",
         " posterior sample is not. Use lapply(x$fits, ", what,
         ", posterior = TRUE) for the per-level posteriors: the levels are",
         " fitted independently, so each element is an ordinary fit and its",
         " posterior is unchanged by being part of a group.", call. = FALSE)
  }
  est <- group_lapply(object, fun, ...)
  nms <- names(est[[1]])
  if (is.null(nms) || anyDuplicated(nms) > 0) {
    stop("The per-level ", what, " estimates are not uniquely named, so they",
         " cannot be tabulated. This should not happen; please report it.",
         call. = FALSE)
  }
  out <- do.call(rbind, lapply(seq_along(est), function(i) {
    e <- est[[i]]
    # Matched by name, so a level whose method returned the quantiles in a
    # different order cannot silently land in the wrong column.
    if (!identical(names(e), nms)) {
      stop("Level \"", object$levels[i], "\" returned ", what, " estimates",
           " named differently from level \"", object$levels[1], "\".",
           call. = FALSE)
    }
    cbind(data.frame(level = object$levels[i], stringsAsFactors = FALSE),
          as.data.frame(as.list(unclass(e)[nms])))
  }))
  rownames(out) <- NULL
  out
}

#' @noRd
#' @method print bayesnecgroupfit
#' @export
print.bayesnecgroupfit <- function(x, ...) {
  cat("Object of class bayesnecgroupfit\n\n")
  cat("  grouping variable :", x$group_var, "\n")
  cat("  family            :", x$family$family,
      paste0("(link = ", x$family$link, ")"), "\n\n")
  mods <- function(f) {
    if (inherits(f, "bayesmanecfit")) {
      paste0(length(f$mod_fits), " models: ",
             paste0(names(f$mod_fits), collapse = ", "))
    } else {
      f$model
    }
  }
  for (i in seq_along(x$levels)) {
    cat(sprintf("  %-14s (n = %3d) : %s\n", x$levels[i], x$n[i],
                mods(x$fits[[i]])))
  }
  cat("\nEach level is a complete fit in its own right; use nec(), ecx() for\n",
      "per-level estimates and crossed_group_weights() for the model weights\n",
      "across levels.\n", sep = "")
  invisible(x)
}

#' @noRd
#' @method nec bayesnecgroupfit
#' @export
nec.bayesnecgroupfit <- function(object, ...) {
  group_estimate_table(object, "nec", function(f, ...) nec(f, ...), ...)
}

#' @noRd
#' @method ecx bayesnecgroupfit
#' @export
ecx.bayesnecgroupfit <- function(object, ...) {
  group_estimate_table(object, "ecx", function(f, ...) ecx(f, ...), ...)
}

#' @noRd
#' @method nsec bayesnecgroupfit
#' @export
nsec.bayesnecgroupfit <- function(object, ...) {
  group_estimate_table(object, "nsec", function(f, ...) nsec(f, ...), ...)
}

#' @noRd
#' @method summary bayesnecgroupfit
#' @export
summary.bayesnecgroupfit <- function(object, ...) {
  group_lapply(object, summary, ...)
}

#' @noRd
#' @method plot bayesnecgroupfit
#' @importFrom graphics par title
#' @export
plot.bayesnecgroupfit <- function(x, ...) {
  n <- length(x$levels)
  old <- par(mfrow = c(ceiling(n / 2), min(2, n)))
  on.exit(par(old), add = TRUE)
  for (i in seq_along(x$levels)) {
    plot(x$fits[[i]], ...)
    title(main = paste0(x$group_var, " = ", x$levels[i]))
  }
  invisible(x)
}
