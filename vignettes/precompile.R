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
purrr::walk(orig_files, ~knitr::knit(.x, file_path_sans_ext(.x)))
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
rendered <- dir("vignettes", pattern = "^example.*\\.Rmd$", full.names = TRUE)
errored <- Filter(function(f) any(grepl("^#> Error", readLines(f, warn = FALSE))),
                  rendered)
if (length(errored)) {
  detail <- vapply(errored, function(f) {
    ln <- readLines(f, warn = FALSE)
    i <- grep("^#> Error", ln)
    # knitr wraps a long message onto following `#>` lines, so reporting only
    # the matched line yields a bare "#> Error:" that says nothing. Carry the
    # continuation lines of the first error through to the message.
    first <- ln[i[1]]
    j <- i[1] + 1
    while (j <= length(ln) && grepl("^#>", ln[j]) && !grepl("^#> Error", ln[j])) {
      first <- paste(first, sub("^#>\\s*", "", ln[j])); j <- j + 1
    }
    paste0("  ", basename(f), " (", length(i), " errored chunks): ", first)
  }, character(1))
  stop("Chunks errored while knitting:\n", paste(detail, collapse = "\n"),
       "\nFix the vignette source and re-run; do not ship this output.",
       call. = FALSE)
}
