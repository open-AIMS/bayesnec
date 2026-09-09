# Adapted from
# https://github.com/bcgov/bcdata/blob/master/vignettes/precompile.R

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0

library(knitr)
library(tools)
library(purrr)

# produce theoretical curves first in example 2b
# source("vignettes/exmp2b_theoretical_curves.R")
rm(list = ls())

# Convert *.orig to *.Rmd -------------------------------------------------
orig_files <- dir(path = "vignettes/", pattern = "*\\.Rmd\\.orig",
                  full.names = TRUE)
# need to set system variable locally first -------------------------------
Sys.setenv("NOT_CRAN" = "true")

# Optional local fit cache ------------------------------------------------
# A full vignette is hours of sampling, so a prose-only correction otherwise
# costs a complete refit -- example9 took 3902 s on 2026-09-08, and was rendered
# twice in two days for changes that touched no chunk. With the cache on, knitr
# reloads each chunk's objects and only re-runs the chunks whose code changed.
#
# OFF unless BAYESNEC_VIGNETTE_CACHE=true, and deliberately so. Two reasons:
#
# 1. A release render must fit from scratch. #190 ships the rendered .Rmd as the
#    published record of what the package does, and a cached chunk is a fit made
#    by whatever version was installed when the cache was written. The gate makes
#    the release path the default and caching the thing you opt into.
# 2. Invalidation is by chunk-code hash. `autodep` tracks which cached chunks
#    read objects another chunk created and invalidates downstream, but it reads
#    only the chunks it has seen, so a change reaching a fit indirectly -- an
#    edited helper, a new package version, a different seed set outside a chunk
#    -- is not detected. The failure is silent and looks like a normal render.
#
# So: use it while iterating on prose, and delete the cache before any render
# whose numbers will be quoted. `unlink("cache/vignettes", recursive = TRUE)`.
use_cache <- identical(Sys.getenv("BAYESNEC_VIGNETTE_CACHE"), "true")
cache_root <- "cache/vignettes"
if (use_cache) {
  message("Vignette fit cache is ON, under ", cache_root, "/. Chunks whose code",
          " is unchanged will NOT be refitted. Do not ship this render without",
          " clearing the cache and re-running.")
}

knit_one <- function(f) {
  if (use_cache) {
    base <- file_path_sans_ext(file_path_sans_ext(basename(f)))
    knitr::opts_chunk$set(cache = TRUE, autodep = TRUE,
                          cache.path = file.path(cache_root, base, ""))
  }
  knitr::knit(f, file_path_sans_ext(f))
}
purrr::walk(orig_files, knit_one)
# Move figures into correct directory so they render ----------------------
# Every vignette is an html_vignette and so uses the png device: an embedded
# pdf is rendered by the browser's pdf plugin rather than as an image.
images <- dir(".", pattern = "vignette-fig.*\\.png$")
success <- file.copy(from = images, to = file.path("vignettes", images),
                     overwrite = TRUE)
# Clean up if successful --------------------------------------------------
if (!all(success)) {
  stop("Image files were not successfully transferred to vignettes directory")
} else {
  unlink(images)
}

# Fail on a vignette whose chunks errored ---------------------------------
# knitr renders a failed chunk as `#> Error...` and carries on, so a run can
# report success while emitting a vignette that is a cascade of errors --
# which is exactly what happened to example8 on 2026-08-24, unnoticed until
# someone read the output. R CMD check does not catch this either: the error
# text is just text in a rendered .Rmd. Check it here, where it is produced.
#
# Scoped to what this run actually knitted, not to every rendered vignette in
# the directory. The precompile workflow rebuilds one vignette per job by
# holding back the other `.Rmd.orig` files -- it cannot hide the rendered
# `.Rmd` files, which have to stay in place for the partial diff to make sense.
# Globbing the directory therefore judged a partial rebuild against vignettes
# it had not touched and was not shipping, so one known-bad vignette sitting in
# the tree would fail every partial rebuild, at the last step, after the
# compute had been spent. See #251.
rendered <- file_path_sans_ext(orig_files)
errored <- Filter(function(f) any(grepl("^#> Error", readLines(f, warn = FALSE))),
                  rendered)
if (length(errored)) {
  detail <- vapply(errored, function(f) {
    hits <- grep("^#> Error", readLines(f, warn = FALSE), value = TRUE)
    paste0("  ", basename(f), " (", length(hits), "): ", hits[1])
  }, character(1))
  stop("Chunks errored while knitting:\n", paste(detail, collapse = "\n"),
       "\nFix the vignette source and re-run; do not ship this output.",
       call. = FALSE)
}
