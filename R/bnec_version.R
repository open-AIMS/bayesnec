#' The installation of bayesnec that fitted an object
#'
#' Reports, for each equation of a fitted object, the version of
#' \pkg{bayesnec} that fitted it, where that installation came from, and the
#' commit it was built from where the installation records one.
#'
#' @details A fit's estimates depend on the code that produced it as well as on
#' the data. The version number does not identify that code on its own: it is
#' raised by hand at intervals, so builds from many commits report the same
#' number. The commit names one state of the code, and the version identifies a
#' CRAN release, which records no commit.
#'
#' The record is read from the \code{DESCRIPTION} of the installed package,
#' \code{utils::packageDescription("bayesnec")}, when the equation is fitted,
#' and stored as an attribute of the fitted object.
#'
#' \code{source} is \code{"local"} where the package was loaded from a source
#' directory by \code{devtools::load_all()}, which leaves no \code{Built}
#' field, or installed from one by \code{R CMD INSTALL},
#' \code{devtools::install()} or \code{remotes::install_local()}, which leave
#' no \code{Repository} field and no \code{RemoteType} other than
#' \code{"local"}. Otherwise \code{source} is the
#' \code{RemoteType} that \pkg{remotes} or \pkg{pak} wrote, such as
#' \code{"github"}. Where there is no \code{RemoteType}, or it is
#' \code{"standard"} or \code{"cran"}, which \pkg{pak} writes for a package
#' from a repository, \code{source} is the \code{Repository} field:
#' \code{"CRAN"}, or the address of an R-universe repository.
#'
#' \code{commit} is \code{RemoteSha}, which \pkg{remotes} and \pkg{pak} write
#' for an installation from GitHub and R-universe writes for its builds. It is
#' read only where it is a git object name of 7 to 40 hexadecimal characters,
#' because \pkg{pak} writes the version number there for a package from a
#' repository. \code{commit} is \code{NA} for a local installation, whose
#' installed files do not record the commit of the source directory, and for a
#' CRAN installation. \code{ref} and \code{repo} name the branch or tag and the
#' repository the commit was installed from, and are \code{NA} where
#' \code{commit} is.
#'
#' Each equation keeps its own record. \code{\link{bnec}} gives every equation
#' it fits the record of the installation running it, and
#' \code{\link[stats]{update}} does the same for every equation it refits.
#' \code{\link{amend}} keeps the record of each equation it retains and gives
#' the equations it adds the current one. \code{\link[base]{c}}, \code{+} and
#' \code{\link{pull_out}} keep each equation's own record, so a set combined
#' from fits made by different installations lists each, and \code{c()},
#' \code{+} and \code{amend()} say so in a message. \code{\link{summary}}
#' prints the record, as one line where every equation shares it and one line
#' per equation where they differ. A \code{\link{bayesnechurdlefit}} and a
#' \code{\link{bayesnecgroupfit}} hold fits made by \code{\link{bnec}}, and
#' report the record of each.
#'
#' Under a parallel plan, each equation records the installation and R version
#' of the worker that fitted it, which can differ from the calling session.
#'
#' An object fitted by a version of \pkg{bayesnec} that did not record the
#' installation, including the example fits shipped with the package such as
#' \code{\link{manec_example}}, reports \code{source} \code{"unknown"} and
#' \code{NA} for the other recorded columns. The \code{brms} and
#' \code{backend} columns are read from the \code{\link[brms]{brmsfit}} of each
#' equation, where \pkg{brms} stores them, so they are reported for such an
#' object too.
#'
#' @param x An object returned by \code{\link{bnec}},
#' \code{\link{bnec_hurdle}}, \code{\link{bnec_group}} or
#' \code{\link{bnec_joint}}.
#'
#' @return A \code{\link[base]{data.frame}} with one row per equation and the
#' columns \code{model}, the equation; \code{bayesnec}, the version;
#' \code{source}; \code{commit}; \code{ref}; \code{repo}; \code{R}, the version
#' of R; \code{brms}, the version of \pkg{brms}; and \code{backend}, the Stan
#' interface and its version. Every column is \code{\link[base]{character}}. A
#' \code{\link{bayesnechurdlefit}} adds a first column \code{component}, naming
#' the growth or survival component, and a \code{\link{bayesnecgroupfit}} a
#' first column \code{level}. A \code{\link{bayesnecjointfit}} is one model,
#' with one record, reported once per level beside the equation fitted at that
#' level.
#'
#' @seealso \code{\link{bnec_record}}, \code{\link{summary}}
#'
#' @examples
#' library(bayesnec)
#' data(manec_example)
#' # Fitted before the installation was recorded.
#' bnec_version(manec_example)
#'
#' @export
bnec_version <- function(x) {
  if (is_bayesnechurdlefit(x)) {
    return(stack_versions(list(growth = x$growth, survival = x$survival),
                          "component"))
  }
  if (is_bayesnecgroupfit(x)) {
    return(stack_versions(stats::setNames(x$fits, x$levels), "level"))
  }
  if (is_bayesnecjointfit(x)) {
    rec <- attr(x, "bnec_version")
    if (is.null(rec)) {
      rec <- joint_version_table(x, unknown_version())
    }
    return(add_fit_versions(rec, rep(list(x$fit), nrow(rec))))
  }
  if (!is_bayesnecfit(x) && !is_bayesmanecfit(x)) {
    stop("bnec_version() applies to an object returned by bnec(),",
         " bnec_hurdle(), bnec_group() or bnec_joint().", call. = FALSE)
  }
  rec <- stored_version(x)
  fits <- if (is_bayesmanecfit(x)) {
    lapply(x$mod_fits[rec$model], `[[`, "fit")
  } else {
    list(x$fit)
  }
  add_fit_versions(rec, fits)
}

#' The DESCRIPTION of the bayesnec that is running
#'
#' A wrapper, so that tests can replace it.
#'
#' @return The \code{packageDescription} object, or \code{NA} where none is
#' found.
#'
#' @noRd
bayesnec_description <- function() {
  suppressWarnings(utils::packageDescription("bayesnec"))
}

#' The record of the bayesnec installation that is running
#'
#' @param desc A \code{packageDescription} object, or a named
#' \code{\link[base]{list}} of \code{DESCRIPTION} fields.
#' @param r_version The R version of the session that fitted the equation.
#'
#' @return A one-row \code{\link[base]{data.frame}} of
#' \code{\link[base]{character}} columns \code{bayesnec}, \code{source},
#' \code{commit}, \code{ref}, \code{repo} and \code{R}. See
#' \code{?bnec_version} for the rule each follows.
#'
#' @noRd
version_record <- function(desc = bayesnec_description(),
                            r_version = as.character(getRversion())) {
  if (!is.list(desc)) {
    return(unknown_version())
  }
  field <- function(name) {
    value <- desc[[name]]
    if (is.null(value) || is.na(value[1]) || !nzchar(value[1])) {
      NA_character_
    } else {
      as.character(value[1])
    }
  }
  type <- field("RemoteType")
  repository <- field("Repository")
  local <- is.na(field("Built")) || identical(type, "local") ||
    (is.na(type) && is.na(repository))
  source <- if (local) {
    "local"
  } else if (!is.na(type) && !type %in% c("standard", "cran")) {
    type
  } else if (!is.na(repository)) {
    repository
  } else {
    type
  }
  sha <- field("RemoteSha")
  has_commit <- !local && !is.na(sha) && grepl("^[0-9a-f]{7,40}$", sha)
  repo <- if (!is.na(field("RemoteUsername")) && !is.na(field("RemoteRepo"))) {
    paste0(field("RemoteUsername"), "/", field("RemoteRepo"))
  } else {
    field("RemoteUrl")
  }
  data.frame(
    bayesnec = field("Version"),
    source = source,
    commit = if (has_commit) sha else NA_character_,
    ref = if (has_commit) field("RemoteRef") else NA_character_,
    repo = if (has_commit) repo else NA_character_,
    R = r_version,
    stringsAsFactors = FALSE
  )
}

#' The installation reported by one fitting attempt
#'
#' @param model The equation fitted.
#' @param fit A successful fitting attempt with worker installation metadata.
#'
#' @return A one-row table from \code{version_table()}. An attempt without
#' metadata reports the unknown installation.
#'
#' @noRd
attempt_version <- function(model, fit) {
  installation <- attr(fit, "bnec_installation", exact = TRUE)
  record <- if (is.null(installation)) {
    unknown_version()
  } else {
    version_record(installation$description, installation$R)
  }
  version_table(model, record)
}

#' The record of an object fitted before the installation was recorded
#'
#' @return A one-row \code{\link[base]{data.frame}} with the columns of
#' \code{version_record()}, \code{source} \code{"unknown"} and the rest
#' \code{NA}.
#'
#' @noRd
unknown_version <- function() {
  data.frame(bayesnec = NA_character_, source = "unknown",
             commit = NA_character_, ref = NA_character_,
             repo = NA_character_, R = NA_character_,
             stringsAsFactors = FALSE)
}

#' One record repeated for each of a set of equations
#'
#' @param models A \code{\link[base]{character}} vector of equation names.
#' @param record A one-row record from \code{version_record()} or
#' \code{unknown_version()}.
#'
#' @return A \code{\link[base]{data.frame}} with a \code{model} column and
#' the columns of \code{record}, one row per element of \code{models}.
#'
#' @noRd
version_table <- function(models, record) {
  out <- cbind(data.frame(model = as.character(models),
                          stringsAsFactors = FALSE),
               record[rep(1L, length(models)), , drop = FALSE])
  rownames(out) <- NULL
  out
}

#' The equations a single fit or a model set holds
#'
#' @param x An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#'
#' @return A \code{\link[base]{character}} vector.
#'
#' @noRd
fit_equations <- function(x) {
  if (is_bayesmanecfit(x)) names(x$mod_fits) else x$model
}

#' The stored record of each equation of a single fit or a model set
#'
#' @param x An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#'
#' @return The table \code{version_table()} builds, in the order of the
#' equations of \code{x}. An equation with no stored row, as in an object
#' fitted before the record existed, has the \code{unknown_version()} record.
#'
#' @noRd
stored_version <- function(x) {
  models <- fit_equations(x)
  stored <- attr(x, "bnec_version")
  out <- version_table(models, unknown_version())
  if (is.null(stored)) {
    return(out)
  }
  rows <- match(models, stored$model)
  found <- !is.na(rows)
  out[found, ] <- stored[rows[found], names(out), drop = FALSE]
  out
}

#' Attach the record of each equation to a single fit or a model set
#'
#' @param out An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#' @param previous \code{NULL}, or a table from \code{stored_version()} of the
#' objects \code{out} was built from.
#'
#' @details An equation named in \code{previous} keeps the first row naming it,
#' as \code{\link[base]{c}} keeps the first of two equations with one name.
#' Every other equation was fitted by this call and gets the current record.
#' Call it after \code{attach_failed_models()}, which rebuilds the object and
#' drops its attributes.
#'
#' @return \code{out}, with attribute \code{"bnec_version"}.
#'
#' @noRd
attach_version <- function(out, previous = NULL) {
  models <- fit_equations(out)
  rows <- if (is.null(previous)) {
    rep(NA_integer_, length(models))
  } else {
    match(models, previous$model)
  }
  new <- is.na(rows)
  rec <- version_table(models, unknown_version())
  if (any(!new)) {
    rec[!new, ] <- previous[rows[!new], names(rec), drop = FALSE]
  }
  if (any(new)) {
    rec[new, ] <- version_table(models[new], version_record())
  }
  attr(out, "bnec_version") <- rec
  out
}

#' Say where a model set mixes the records of its equations
#'
#' @param out An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}, with its record attached.
#'
#' @return \code{NULL}, invisibly. Called for its message.
#'
#' @noRd
report_mixed_versions <- function(out) {
  if (mixed_versions(attr(out, "bnec_version"))) {
    message("The equations of this set were fitted by different installations",
            " of bayesnec or R; bnec_version() lists each.")
  }
  invisible(NULL)
}

#' Whether a table of records holds more than one record
#'
#' @param rec A table from \code{version_table()}, or \code{NULL}.
#'
#' @return A \code{\link[base]{logical}} of length 1.
#'
#' @noRd
mixed_versions <- function(rec) {
  if (is.null(rec) || nrow(rec) < 2) {
    return(FALSE)
  }
  fields <- names(unknown_version())
  nrow(unique(rec[, fields, drop = FALSE])) > 1
}

#' The record of a joint refit, once for each level
#'
#' @param x An object of class \code{\link{bayesnecjointfit}}, or the list
#' \code{bnec_joint()} is building one from.
#' @param record A one-row record.
#'
#' @return The table \code{version_table()} builds, with a first column
#' \code{level}.
#'
#' @noRd
joint_version_table <- function(x, record) {
  models <- if (is.null(x$models)) {
    rep(x$model, length(x$levels))
  } else {
    unname(x$models)
  }
  cbind(data.frame(level = x$levels, stringsAsFactors = FALSE),
        version_table(models, record))
}

#' Add the brms and backend versions each equation's brmsfit stores
#'
#' @param rec A table of records, one row per equation.
#' @param fits A \code{\link[base]{list}} of \code{\link[brms]{brmsfit}}
#' objects, one per row of \code{rec}.
#'
#' @return \code{rec}, with \code{brms} and \code{backend} columns.
#'
#' @noRd
add_fit_versions <- function(rec, fits) {
  rec$brms <- vapply(fits, function(fit) {
    v <- fit$version$brms
    if (is.null(v)) NA_character_ else as.character(v)
  }, character(1), USE.NAMES = FALSE)
  rec$backend <- vapply(fits, backend_version, character(1),
                        USE.NAMES = FALSE)
  rownames(rec) <- NULL
  rec
}

#' The Stan interface a brmsfit was sampled with, and its version
#'
#' @param fit An object of class \code{\link[brms]{brmsfit}}.
#'
#' @return A \code{\link[base]{character}} of length 1, such as
#' \code{"rstan 2.32.6"} or \code{"CmdStan 2.36.0"}, or \code{NA}.
#'
#' @noRd
backend_version <- function(fit) {
  backend <- if (is.null(fit$backend)) "rstan" else fit$backend
  v <- fit$version
  if (identical(backend, "cmdstanr") && !is.null(v$cmdstan)) {
    paste("CmdStan", as.character(v$cmdstan))
  } else if (!is.null(v[[backend]])) {
    paste(backend, as.character(v[[backend]]))
  } else {
    NA_character_
  }
}

#' Stack the records of the fits a hurdle or grouped fit holds
#'
#' @param parts A named \code{\link[base]{list}} of fits.
#' @param id The name of the column that holds the names of \code{parts}.
#'
#' @return A \code{\link[base]{data.frame}}.
#'
#' @noRd
stack_versions <- function(parts, id) {
  tabs <- lapply(names(parts), function(nm) {
    tab <- bnec_version(parts[[nm]])
    cbind(stats::setNames(data.frame(rep(nm, nrow(tab)),
                                     stringsAsFactors = FALSE), id),
          tab)
  })
  out <- do.call(rbind, tabs)
  rownames(out) <- NULL
  out
}

#' One record as a phrase that follows "Fitted by"
#'
#' @param rec One row of a table from \code{bnec_version()}.
#'
#' @return A \code{\link[base]{character}} of length 1.
#'
#' @noRd
describe_version <- function(rec) {
  if (identical(rec$source, "unknown")) {
    return("a version of bayesnec that did not record its installation")
  }
  where <- if (identical(rec$source, "local")) {
    "a local source, which records no commit"
  } else if (is.na(rec$commit)) {
    rec$source
  } else {
    paste0(rec$source,
           if (is.na(rec$repo)) "" else paste0(" ", rec$repo),
           if (is.na(rec$ref)) "" else paste0("@", rec$ref),
           " at commit ", substr(rec$commit, 1, 7))
  }
  paste0("bayesnec ", rec$bayesnec, ", installed from ", where, ", under R ",
         rec$R)
}

#' Print the record of a summary
#'
#' @param rec A table from \code{bnec_version()}, or \code{NULL} for a summary
#' stored before the record existed.
#'
#' @return \code{NULL}, invisibly. Called for its output.
#'
#' @noRd
print_version <- function(rec) {
  if (is.null(rec) || nrow(rec) == 0) {
    return(invisible(NULL))
  }
  if (!mixed_versions(rec)) {
    cat("Fitted by ", describe_version(rec[1, , drop = FALSE]), ".\n\n",
        sep = "")
    return(invisible(NULL))
  }
  ids <- intersect(c("component", "level", "model"), names(rec))
  labels <- do.call(paste, unname(as.list(rec[ids])))
  cat("Fitted by different installations; see bnec_version():\n")
  for (i in seq_len(nrow(rec))) {
    cat("  ", labels[i], ": ", describe_version(rec[i, , drop = FALSE]), "\n",
        sep = "")
  }
  cat("\n")
  invisible(NULL)
}
