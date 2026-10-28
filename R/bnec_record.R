#' What \code{bnec()} did to your request before fitting
#'
#' Reports the candidate set as requested, the candidate set attempted, the
#' equations excluded and why, and any substitution made in the response.
#'
#' @details \code{\link{bnec}} decides which of the requested equations it will
#' not attempt, and \code{check_data()} substitutes values that a family cannot
#' represent --- a zero under \code{Gamma} or \code{Beta}, a one under
#' \code{Beta}. Both were reported by \code{\link[base]{message}} and then
#' discarded, so neither could be recovered from the returned object: only from
#' console output, which a knitted document or a call wrapped in
#' \code{\link[base]{suppressMessages}} does not keep. The set as requested, the
#' set attempted, the reason for the difference, and what was altered in the
#' data are exactly what a methods section has to state.
#'
#' Two of the three substitutions were silent even on the console, so a user
#' comparing \pkg{bayesnec} against another engine had no way to see that the
#' data had been changed at all.
#'
#' @param x An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#'
#' @return A \code{\link[base]{list}} with elements \code{requested} and
#' \code{attempted}, both \code{\link[base]{character}} vectors of equation
#' names; \code{excluded}, a \code{\link[base]{data.frame}} of the equations
#' \code{\link{bnec}} declined to attempt with the reason for each; and
#' \code{substitutions}, a \code{\link[base]{data.frame}} of the changes made to
#' the response, or \code{NULL} where none were made. \code{requested} is
#' partitioned exactly by \code{attempted} and \code{excluded$model}, with one
#' exception. Where the response does not vary, \code{\link{bnec}} fits the
#' constant equation "ecxflat" alone whatever was requested: \code{attempted}
#' is then "ecxflat", which need not have been requested, and every other
#' requested equation is in \code{excluded} with that reason. See the section
#' of \code{?bnec} on a response that does not vary.
#'
#' \code{attempted} is the set \code{\link{bnec}} tried to fit, not the set
#' that sampled: an equation that was attempted and failed appears here, and
#' for \code{\link{bnec}} and \code{\link{amend}} also in
#' \code{\link{failed_models}}. The name was chosen over \code{fitted} for
#' that reason. \code{\link[stats]{update}} does not record failures, so an
#' equation whose refit failed stays in \code{attempted} and is not reported
#' by \code{\link{failed_models}}.
#'
#' The record is kept through \code{\link[stats]{update}},
#' \code{\link{amend}}, \code{\link{pull_out}}, \code{\link[base]{c}} and
#' \code{+}. \code{\link{pull_out}} moves each equation it does not keep to
#' \code{excluded}, with the reason "not kept by pull_out()", as
#' \code{\link{amend}} does with the reason "dropped by amend()" for an
#' equation it drops. An equation whose failure \code{\link{failed_models}}
#' still reports stays in \code{attempted}. \code{\link[base]{c}} and \code{+}
#' join the records of the objects they combine. \code{requested} and
#' \code{attempted} are the unions of theirs, and an equation attempted in any
#' of them is not in \code{excluded}. An equation excluded by more than one of
#' them keeps the reason the first gives, as \code{\link[base]{c}} keeps the
#' first of two fits of one equation. \code{substitutions} holds the distinct
#' rows of theirs. Where any of the objects combined has no record, the result
#' has none, because what was done to its part of the request is not known.
#'
#' \code{NULL} for an object fitted by a version that did not record it.
#'
#' @seealso \code{\link{bnec}}, \code{\link{models}},
#' \code{\link{check_data}}, \code{\link{bnec_version}}
#'
#' @examples
#' \donttest{
#' library(bayesnec)
#' data(manec_example)
#' bnec_record(manec_example)
#' }
#'
#' @export
bnec_record <- function(x) {
  if (!inherits(x, "bnecfit")) {
    stop("Object is not a bayesnecfit or bayesmanecfit.", call. = FALSE)
  }
  attr(x, "bnec_record")
}

#' Attach the record of what bnec() changed
#'
#' Follows \code{attach_failed_models()}, which stores the models that were
#' attempted and failed. This stores the ones that were never attempted, and
#' what was substituted in the data before any of them were.
#'
#' @param out The fitted object.
#' @param requested The equations the user asked for.
#' @param attempted The equations that were attempted.
#' @param excluded The \code{excluded} attribute from \code{check_models()}.
#' @param substitutions The \code{substitutions} element of
#' \code{check_data()}'s return.
#'
#' @return \code{out}, with the record attached.
#' @noRd
attach_bnec_record <- function(out, requested, attempted, excluded,
                               substitutions) {
  attr(out, "bnec_record") <- list(
    requested = as.character(requested),
    attempted = as.character(attempted),
    excluded = if (is.null(excluded)) {
      data.frame(model = character(), reason = character(),
                 stringsAsFactors = FALSE)
    } else {
      excluded
    },
    substitutions = substitutions
  )
  out
}

#' Copy the records of the objects a fitted set was built from
#'
#' For the functions that build a set from fitted objects: \code{c()},
#' \code{pull_out()} and \code{amend()}. Each attribute \pkg{bayesnec}
#' attaches to a fit is derived here, so give a new one its rule here too.
#' \code{amend()} then rebuilds the request record for the equations it
#' fitted. \code{bnec()} and \code{update()} attach their records themselves.
#'
#' @param out The set built. Attach its failures first: an equation that
#' failed stays attempted.
#' @param inputs A \code{\link[base]{list}} of the objects \code{out} was
#' built from.
#' @param reason The reason recorded for an equation that was attempted and
#' that \code{out} does not keep.
#'
#' @return \code{out}, with attributes \code{"bnec_version"} and
#' \code{"bnec_record"}.
#' @noRd
copy_fit_records <- function(out, inputs, reason) {
  out <- attach_version(out, do.call(rbind, lapply(inputs, stored_version)))
  kept <- c(fit_equations(out), names(out[["failed_models"]]))
  attr(out, "bnec_record") <- joined_record(
    lapply(inputs, attr, "bnec_record"), kept, reason
  )
  out
}

#' The request record of a set built from fitted objects
#'
#' @param records A \code{\link[base]{list}} of records, one per object the
#' set was built from.
#' @param kept The equations the set holds or reports as failed.
#' @param reason As for \code{copy_fit_records()}.
#'
#' @return A record, or \code{NULL} where any element of \code{records} is.
#' @noRd
joined_record <- function(records, kept, reason) {
  if (length(records) == 0 ||
        any(vapply(records, is.null, logical(1)))) {
    return(NULL)
  }
  field <- function(name) unique(unlist(lapply(records, `[[`, name)))
  requested <- field("requested")
  attempted <- intersect(field("attempted"), kept)
  # Drop the exclusions of an equation another record attempted: if the set
  # no longer keeps it, `reason` says why, not the earlier exclusion.
  excluded <- do.call(rbind, lapply(records, `[[`, "excluded"))
  if (!is.null(excluded)) {
    excluded <- excluded[!excluded$model %in% field("attempted"), ,
                         drop = FALSE]
  }
  substitutions <- do.call(rbind, lapply(records, `[[`, "substitutions"))
  if (!is.null(substitutions)) {
    substitutions <- unique(substitutions)
    rownames(substitutions) <- NULL
  }
  list(requested = requested, attempted = attempted,
       excluded = amend_exclusions(
         list(requested = requested, excluded = excluded), NULL, attempted,
         NULL, reason = reason
       ),
       substitutions = substitutions)
}
