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
#' \code{\link{nec}}, \code{\link{ecx}} and \code{\link{nsec}} on a
#' \code{bayesnecgroupfit} return a \code{\link[base]{data.frame}} with one row
#' per level: a \code{level} column; one numeric column per quantile, named as
#' the same method names them on a single fit; and, after those, one character
#' column per quantile, named \code{bound_} followed by that quantile's column
#' name, for example \code{bound_Q97.5}. A \code{bound_} entry is \code{">="} or
#' \code{"<="} where the matching number is the end of the prediction range
#' rather than a quantile, and \code{""} where it is a quantile. These are the
#' marks the level's own fit returns in attribute \code{"censored_summary"} and
#' prints beside the number.
#'
#' @slot fits A named \code{\link[base]{list}} of fits, one per level.
#' @slot group_var The name of the grouping column.
#' @slot levels The factor levels, in order.
#' @slot formula The \code{\link{bayesnecformula}} fitted at every level.
#' @slot data The full \code{\link[base]{data.frame}}, all levels.
#' @slot family The family, chosen once and shared by every level.
#' @slot n The number of observations per level.
#' @slot weights_method The model-averaging weighting requested of
#' \code{\link{bnec_group}}, which \code{\link{crossed_group_weights}} checks
#' against what the fits themselves carry.
#' @slot level_seeds The seed each level was fitted under, realised in the
#' calling session before the levels were dispatched. Kept so that the
#' realisation is recoverable from the fit rather than regenerated, for the
#' reason \code{expand_manec()} stores \code{w_draw_index} beside
#' \code{w_draw_seed}.
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
#' Applies \code{fun} to the fit of each level, since every element of
#' \code{fits} is an ordinary bayesnec object.
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
#' @details Refuses \code{posterior = TRUE}: a table with one row per level
#' cannot hold a vector of draws.
#'
#' @return A \code{\link[base]{data.frame}} with one row per level: the level,
#' one numeric column per entry of the estimate, and one \code{bound_} column
#' per entry from \code{estimate_marks()}.
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
  estimate_table(group_lapply(object, fun, ...), object$levels, what)
}

#' Stack per-level estimates into one row per level
#'
#' Shared with \code{\link{bayesnecjointfit}}, so that the grouped and joint
#' routes return the same columns.
#'
#' @param est A named \code{\link[base]{list}} of estimate vectors, one per
#' level.
#' @param levels The level names, in order.
#' @param what The name of the calling method, for the error message.
#'
#' @return A \code{\link[base]{data.frame}} with one row per level: the level,
#' one numeric column per entry of the estimate, and one \code{bound_} column
#' per entry from \code{estimate_marks()}.
#'
#' @noRd
estimate_table <- function(est, levels, what) {
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
      stop("Level \"", levels[i], "\" returned ", what, " estimates",
           " named differently from level \"", levels[1], "\".",
           call. = FALSE)
    }
    cbind(data.frame(level = levels[i], stringsAsFactors = FALSE),
          as.data.frame(as.list(unclass(e)[nms])))
  }))
  # Building the numeric columns drops each level's "censored_summary"
  # attribute, so add the marks as separate columns. Writing them into the
  # numbers, as print() does, would turn the numeric columns into text.
  out <- cbind(out, estimate_marks(est, nms, levels, what))
  rownames(out) <- NULL
  out
}

#' The censoring marks of per-level estimates, as columns of a table
#'
#' One character column per entry of the estimate, named \code{bound_} followed
#' by the entry's own column name, holding the \code{bound} vector of that
#' level's \code{"censored_summary"} record: \code{">="} or \code{"<="} where
#' the entry is the end of the prediction range rather than a quantile, and
#' \code{""} where it is a quantile. A level whose estimate has no record
#' has \code{""} throughout, which is what its own \code{print()} shows.
#'
#' The marks are columns because an attribute does not print, and
#' \code{rbind()} and subsetting drop it. They follow the numeric columns, so
#' those keep their positions.
#'
#' @param est A \code{\link[base]{list}} of summarised estimates, one per
#' level, each named \code{nms}.
#' @param nms The names of the entries, which are the names of the numeric
#' columns.
#' @param levels The level names, in order, for the error message.
#' @param what The name of the calling method, for the error message.
#'
#' @return A \code{\link[base]{data.frame}} with one row per level.
#'
#' @noRd
estimate_marks <- function(est, nms, levels, what) {
  marks <- lapply(seq_along(est), function(i) {
    bound <- attr(est[[i]], "censored_summary")$bound
    if (is.null(bound)) {
      return(rep("", length(nms)))
    }
    # summarise_censored() writes one mark per entry. Refuse any other length:
    # recycling or truncating it would label a bound as a quantile.
    if (length(bound) != length(nms)) {
      stop("The censoring record of level \"", levels[i], "\" marks ",
           length(bound), " entries and its ", what, " estimate has ",
           length(nms), ". This should not happen; please report it.",
           call. = FALSE)
    }
    bound
  })
  out <- as.data.frame(do.call(rbind, marks), stringsAsFactors = FALSE)
  names(out) <- paste0("bound_", nms)
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
  # Once for the group rather than once per level: every level is fitted with
  # the one formula. See report_fitted_scale().
  quiet <- report_fitted_scale(object, dots_xform(nec, list(...)), "nec")
  on.exit(options(quiet), add = TRUE)
  # Check here, so that the error names this class rather than a level's.
  check_no_effect_arg(list(...), object)
  group_estimate_table(object, "nec", function(f, ...) nec(f, ...), ...)
}

#' @noRd
#' @method ecx bayesnecgroupfit
#' @export
ecx.bayesnecgroupfit <- function(object, ...) {
  # Once for the group, as in nec.bayesnecgroupfit().
  quiet <- report_fitted_scale(object, dots_xform(ecx, list(...)), "ecx")
  on.exit(options(quiet), add = TRUE)
  group_estimate_table(object, "ecx", function(f, ...) ecx(f, ...), ...)
}

#' @noRd
#' @method nsec bayesnecgroupfit
#' @export
nsec.bayesnecgroupfit <- function(object, ...) {
  # Once for the group, as in nec.bayesnecgroupfit().
  quiet <- report_fitted_scale(object, dots_xform(nsec, list(...)), "nsec")
  on.exit(options(quiet), add = TRUE)
  group_estimate_table(object, "nsec", function(f, ...) nsec(f, ...), ...)
}

#' @noRd
#' @method summary bayesnecgroupfit
#' @export
summary.bayesnecgroupfit <- function(object, ...) {
  # Report the scale of the ECx rows once for the group. Read `xform` from the
  # dots that the level summaries pass on to ecx(), or the message would name
  # the wrong scale. See summary_ecx_xform().
  dots <- list(...)
  if (isTRUE(dots[["ecx"]])) {
    rest <- dots[!names(dots) %in% c("ecx", "ecx_vals")]
    quiet <- report_fitted_scale(object, summary_ecx_xform(rest), "ecx")
    on.exit(options(quiet), add = TRUE)
  }
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
  # Let through only the first all_models deprecation warning. Each level maps
  # the argument itself, because the levels' equation sets can differ.
  deprecation_seen <- FALSE
  for (i in seq_along(x$levels)) {
    withCallingHandlers(
      plot(x$fits[[i]], ...),
      bayesnec_all_models_deprecated = function(w) {
        if (deprecation_seen) {
          invokeRestart("muffleWarning")
        }
        deprecation_seen <<- TRUE
      }
    )
    title(main = paste0(x$group_var, " = ", x$levels[i]))
  }
  invisible(x)
}
