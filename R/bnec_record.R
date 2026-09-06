#' What \code{bnec()} did to your request before fitting
#'
#' Reports the candidate set as requested, the candidate set as fitted, the
#' equations excluded and why, and any substitution made in the response.
#'
#' @details \code{\link{bnec}} decides which of the requested equations it will
#' not attempt, and \code{check_data()} substitutes values that a family cannot
#' represent --- a zero under \code{Gamma} or \code{Beta}, a one under
#' \code{Beta}. Both were reported by \code{\link[base]{message}} and then
#' discarded, so neither could be recovered from the returned object: only from
#' console output, which a knitted document or a call wrapped in
#' \code{\link[base]{suppressMessages}} does not keep. The set as requested, the
#' set as fitted, the reason for the difference, and what was altered in the
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
#' \code{fitted}, both \code{\link[base]{character}} vectors of equation names;
#' \code{excluded}, a \code{\link[base]{data.frame}} of the equations
#' \code{\link{bnec}} declined to attempt with the reason for each; and
#' \code{substitutions}, a \code{\link[base]{data.frame}} of the changes made to
#' the response, or \code{NULL} where none were made. \code{NULL} for an object
#' fitted by a version that did not record it.
#'
#' @seealso \code{\link{bnec}}, \code{\link{models}}, \code{\link{check_data}}
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
#' @param fitted The equations that were attempted.
#' @param excluded The \code{excluded} attribute from \code{check_models()}.
#' @param substitutions The \code{substitutions} element of
#' \code{check_data()}'s return.
#'
#' @return \code{out}, with the record attached.
#' @noRd
attach_bnec_record <- function(out, requested, fitted, excluded,
                               substitutions) {
  attr(out, "bnec_record") <- list(
    requested = as.character(requested),
    fitted = as.character(fitted),
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
