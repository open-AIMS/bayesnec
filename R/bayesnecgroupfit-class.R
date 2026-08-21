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
#' @param ... Passed to the underlying method.
#'
#' @return A \code{\link[base]{data.frame}} with one row per level.
#'
#' @noRd
group_estimate_table <- function(object, fun, ...) {
  est <- group_lapply(object, fun, ...)
  out <- do.call(rbind, lapply(seq_along(est), function(i) {
    e <- est[[i]]
    data.frame(level = object$levels[i],
               Estimate = unname(e[1]),
               Q2.5 = unname(e[2]),
               Q97.5 = unname(e[3]),
               stringsAsFactors = FALSE)
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
  group_estimate_table(object, function(f, ...) nec(f, ...), ...)
}

#' @noRd
#' @method ecx bayesnecgroupfit
#' @export
ecx.bayesnecgroupfit <- function(object, ...) {
  group_estimate_table(object, function(f, ...) ecx(f, ...), ...)
}

#' @noRd
#' @method nsec bayesnecgroupfit
#' @export
nsec.bayesnecgroupfit <- function(object, ...) {
  group_estimate_table(object, function(f, ...) nsec(f, ...), ...)
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
