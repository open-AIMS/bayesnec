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
# Move .png files into correct directory so they render -------------------
images <- dir(".", pattern = "vignette-fig.*\\.png$")
success <- file.copy(from = images, to = file.path("vignettes", images),
                     overwrite = TRUE)
# Clean up if successful --------------------------------------------------
if (!all(success)) {
  stop("Image files were not successfully transferred to vignettes directory")
} else {
  unlink(images)
}
