# Adapted from
# https://github.com/bcgov/bcdata/blob/master/vignettes/precompile.R

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0

# Precompile the vignettes.
#
#   Rscript vignettes/precompile.R                  # every vignette
#   Rscript vignettes/precompile.R example7         # one
#   Rscript vignettes/precompile.R example2 example7
#   source("vignettes/precompile.R")                # every vignette
#
# and, for the routes that cannot pass arguments -- an interactive source(),
# `R CMD BATCH`, a container `%runscript` -- the same selection is read from
# BAYESNEC_VIGNETTES as a comma- or space-separated list.
#
# Run from the repository root. Errors are not caught: under Rscript an
# uncaught error exits non-zero, which is what lets the HPC job fail rather
# than return a plausible-looking .Rmd. See hpc/README.md and #306.

library(knitr)
library(tools)
library(purrr)

# produce theoretical curves first in example 2b
# source("vignettes/exmp2b_theoretical_curves.R")

# Read the selection before the workspace is cleared. The name is dotted so
# that the `rm(list = ls())` below leaves it alone -- ls() omits dotted names --
# but that is too quiet to rely on, so it is also named in the setdiff.
.bayesnec_selection <- local({
  # commandArgs() is only consulted when this is not an interactive session:
  # an IDE may start R with --args of its own, and a stray argument there
  # should not silently narrow a precompile to one vignette.
  args <- if (interactive()) character(0) else commandArgs(trailingOnly = TRUE)
  if (!length(args)) {
    env <- Sys.getenv("BAYESNEC_VIGNETTES")
    args <- if (nzchar(env)) strsplit(trimws(env), "[,[:space:]]+")[[1]] else character(0)
  }
  args[nzchar(args)]
})
rm(list = setdiff(ls(), ".bayesnec_selection"))

# Convert *.orig to *.Rmd -------------------------------------------------
available <- dir(path = "vignettes/", pattern = "\\.Rmd\\.orig$", full.names = TRUE)
if (!length(available)) {
  stop("no vignettes/*.Rmd.orig found. Run this from the repository root.",
       call. = FALSE)
}
if (length(.bayesnec_selection)) {
  # A vignette may be named as `example7`, `example7.Rmd`, `example7.Rmd.orig`
  # or with the directory attached, because all four are what a person has in
  # front of them when they come to rebuild one.
  stem <- function(x) file_path_sans_ext(file_path_sans_ext(basename(x)))
  wanted <- stem(.bayesnec_selection)
  unknown <- setdiff(wanted, stem(available))
  if (length(unknown)) {
    stop("no such vignette source: ", paste(unknown, collapse = ", "),
         "\nAvailable: ", paste(sort(stem(available)), collapse = ", "),
         call. = FALSE)
  }
  orig_files <- available[stem(available) %in% wanted]
} else {
  orig_files <- available
}
message("Precompiling: ", paste(file_path_sans_ext(basename(orig_files)),
                                collapse = ", "))

# need to set system variable locally first -------------------------------
Sys.setenv("NOT_CRAN" = "true")

# Stan backend ------------------------------------------------------------
# cmdstanr for every vignette, not brms's rstan default. cmdstan compiles each
# Stan program once and caches the executable under a name derived from a hash
# of the Stan source, so the cache is reusable between runs, between vignettes
# and between branches wherever the data and priors are unchanged. rstan has no
# equivalent cross-session cache. What that saves depends on how much sampling
# the vignette does: on example4, 14 programs, a cold run took 13 m 58 s and a
# warm one 13 m 06 s; example7 compiled 304 programs cold and none warm (#306).
#
# This changes the sampler that produces the committed vignette output, so the
# next full precompile is expected to change numbers in every vignette. That is
# a deliberate decision recorded on #306, not a side effect.
#
# BAYESNEC_BACKEND overrides it, and .github/workflows/precompile-vignettes.yaml
# sets it to rstan: that runner has neither cmdstanr, which is not on CRAN and
# not in DESCRIPTION, nor a cmdstan installation, so every fit would fail
# require_backend() there.
options(brms.backend = Sys.getenv("BAYESNEC_BACKEND", "cmdstanr"))

# Chains run in parallel across the cores this run has been given. Without
# this, brms takes cores from getOption("mc.cores"), whose default is 1, and
# four chains run one after another: example1 and example4 were sampling
# serially on a four-core allocation. SLURM_CPUS_PER_TASK is the allocation
# rather than the node; the fallback is used off the cluster.
#
# example2, example3 and example6 set mc.cores themselves, to
# parallel::detectCores(), in a chunk, and so override this. That reports the
# node and not the allocation, but brms runs at most `chains` in parallel and
# every one of those vignettes takes the default of four, so it oversubscribes
# nothing as they stand. Those chunks are echo = FALSE, so removing the lines
# would change nothing a reader sees; they are left alone here because #190
# re-renders the whole set and is the place to remove them.
.cpus <- suppressWarnings(as.integer(Sys.getenv("SLURM_CPUS_PER_TASK")))
if (is.na(.cpus) || .cpus < 1) {
  .cpus <- max(1L, parallel::detectCores(logical = FALSE))
}
options(mc.cores = .cpus)
message("Chains run on ", .cpus, " core(s)")

# Where cmdstanr writes the .stan files it names by hash, and therefore where
# the compiled executables live. Unset, it is the session tempdir and nothing
# survives the run. The HPC job points it at shared scratch; see hpc/README.md.
.stan_cache <- Sys.getenv("BAYESNEC_STAN_CACHE")
if (nzchar(.stan_cache)) {
  dir.create(.stan_cache, recursive = TRUE, showWarnings = FALSE)
  options(cmdstanr_write_stan_file_dir = .stan_cache)
  message("Stan program cache: ", .stan_cache)
}

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
#
# Distinct from BAYESNEC_STAN_CACHE above, which caches compiled Stan programs
# and not fits. That one is always safe: a program is reused only when its Stan
# source hashes the same, and the sampling is re-run either way.
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
  started <- Sys.time()
  knitr::knit(f, file_path_sans_ext(f))
  message(basename(f), " knitted in ",
          format(round(difftime(Sys.time(), started, units = "mins"), 1)))
}

# Figures written to the working directory by this run, so that a stale figure
# left behind by an interrupted run is neither copied over a good committed one
# nor deleted. Recorded before knitting and compared after: a partial rebuild
# must not touch the figures of the vignettes it is not rebuilding.
# Contents, not modification time. A figure rewritten within the same second to
# the same size would be missed by an mtime-and-size comparison, and the
# filesystems this runs on include one with one-second mtime resolution. These
# are a handful of small PNGs, so hashing them costs nothing worth measuring.
fig_state <- function() {
  f <- dir(".", pattern = "^vignette-fig.*\\.png$")
  tools::md5sum(f)
}
before <- fig_state()

purrr::walk(orig_files, knit_one)

# Move figures into correct directory so they render ----------------------
# Every vignette is an html_vignette and so uses the png device: an embedded
# pdf is rendered by the browser's pdf plugin rather than as an image.
after <- fig_state()
is_new <- !(names(after) %in% names(before))
changed <- names(after)[is_new |
                          (!is_new & after != before[names(after)])]
if (length(changed)) {
  success <- file.copy(from = changed, to = file.path("vignettes", changed),
                       overwrite = TRUE)
  # Clean up if successful ------------------------------------------------
  if (!all(success)) {
    stop("Image files were not successfully transferred to vignettes ",
         "directory: ", paste(changed[!success], collapse = ", "),
         call. = FALSE)
  }
  unlink(changed)
  message("Copied ", length(changed), " figure(s) into vignettes/")
}

# Anything left here was written by an earlier run that did not finish, and is
# deliberately neither copied nor deleted: it may belong to a vignette this run
# was not asked to rebuild. Reported, because otherwise it accumulates silently
# -- the working directory is the repository root and these names are not
# covered by vignettes/.gitignore.
stale <- setdiff(names(fig_state()), changed)
if (length(stale)) {
  message("Left in place, not written by this run: ", paste(stale, collapse = ", "),
          "\n  They are from an interrupted run. Delete them once you have",
          " checked which vignette they belong to.")
}

# Fail on a vignette whose chunks errored ---------------------------------
# knitr renders a failed chunk as `#> Error...` and carries on, so a run can
# report success while emitting a vignette that is a cascade of errors --
# which is exactly what happened to example8 on 2026-08-24, unnoticed until
# someone read the output. R CMD check does not catch this either: the error
# text is just text in a rendered .Rmd. Check it here, where it is produced.
#
# Scoped to what this run actually knitted, not to every rendered vignette in
# the directory. The precompile workflow rebuilds one vignette per job -- now
# by naming it rather than by holding back the other `.Rmd.orig` files -- and
# it cannot hide the rendered `.Rmd` files, which have to stay in place for the
# partial diff to make sense. Globbing the directory therefore judged a partial
# rebuild against vignettes it had not touched and was not shipping, so one
# known-bad vignette sitting in the tree would fail every partial rebuild, at
# the last step, after the compute had been spent. See #251.
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
message("Precompiled without error: ",
        paste(basename(rendered), collapse = ", "))
