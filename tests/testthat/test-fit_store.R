# Source the vignette-only fit-store tool into an isolated environment.
# Try source-tree and R CMD check paths; skip if absent, as in installed
# coverage runs that omit vignettes/.
fit_store_env <- function() {
  candidates <- c(
    test_path("..", "..", "vignettes", "fit_store.R"),
    test_path("..", "..", "00_pkg_src", "bayesnec", "vignettes", "fit_store.R")
  )
  path <- candidates[file.exists(candidates)]
  skip_if(length(path) == 0L, "vignettes/fit_store.R is not reachable")
  env <- new.env(parent = globalenv())
  sys.source(path[[1L]], envir = env)
  env
}

# A store directory holding a MANIFEST laid out as the compendium's
# analysis/assemble_store.R writes it. `bayesnec = NULL` leaves the version
# line out; `manifest = FALSE` writes no MANIFEST at all.
fit_store_dir <- function(bayesnec = NULL, manifest = TRUE) {
  store <- tempfile("fit-store-")
  dir.create(store)
  if (manifest) {
    writeLines(c("vignette: /scratch/bayesnec-src/vignettes/example8.Rmd.orig",
                 if (!is.null(bayesnec)) paste("bayesnec:", bayesnec),
                 "manifest built: 2026-09-17 22:57:45",
                 "assembled: 2026-09-18 01:15:19",
                 "assembly seed: 228"),
               file.path(store, "MANIFEST"))
  }
  store
}

test_that("a store fitted with an older bayesnec is refused", {
  fs <- fit_store_env()
  # Version 2.1.3.39 also labels stores fitted before the prior change;
  # reject it even though the package version initially stayed unchanged.
  store <- fit_store_dir("2.1.3.39")
  shims <- new.env()
  err <- expect_error(
    suppressMessages(fs$fit_store_install(store, envir = shims))
  )
  msg <- conditionMessage(err)
  expect_match(msg, "fitted with bayesnec 2.1.3.39", fixed = TRUE)
  expect_match(msg, paste(fs$FIT_STORE_MIN_BAYESNEC, "or later"), fixed = TRUE)
  expect_match(msg, "rename the cluster's old units/ and store/", fixed = TRUE)
  expect_match(msg, "./hpc/deploy.sh --ref", fixed = TRUE)
  expect_match(msg, "BAYESNEC_FIT_STORE", fixed = TRUE)
  # Refused before any shim was assigned.
  expect_length(ls(shims), 0L)
  unlink(store, recursive = TRUE)
})

test_that("a version line with leading space is still checked", {
  fs <- fit_store_env()
  # Read as a missing line, this store would be installed with a message
  # rather than refused.
  store <- fit_store_dir()
  writeLines(c("vignette: example8.Rmd.orig", "  bayesnec: 2.1.3.39"),
             file.path(store, "MANIFEST"))
  shims <- new.env()
  expect_error(
    suppressMessages(fs$fit_store_install(store, envir = shims)),
    "fitted with bayesnec 2.1.3.39", fixed = TRUE
  )
  expect_length(ls(shims), 0L)
  unlink(store, recursive = TRUE)
})

test_that("a store at or above the minimum is installed", {
  fs <- fit_store_env()
  for (v in c(fs$FIT_STORE_MIN_BAYESNEC, "2.1.3.41", "2.2.0")) {
    store <- fit_store_dir(v)
    shims <- new.env()
    expect_true(suppressMessages(fs$fit_store_install(store, envir = shims)))
    expect_setequal(ls(shims), fs$FIT_FUNS)
    unlink(store, recursive = TRUE)
  }
})

test_that("versions are compared as numbers, not as strings", {
  fs <- fit_store_env()
  # As strings "2.1.3.100" sorts before "2.1.3.40", so a string comparison
  # would refuse a store that is newer than the minimum.
  expect_true("2.1.3.100" < "2.1.3.40")
  store <- fit_store_dir("2.1.3.100")
  shims <- new.env()
  expect_true(suppressMessages(fs$fit_store_install(store, envir = shims)))
  expect_setequal(ls(shims), fs$FIT_FUNS)
  unlink(store, recursive = TRUE)
})

test_that("a store that records no version is installed and reported", {
  fs <- fit_store_env()
  # The compendium's key check installs the shim against a store of empty
  # files with no MANIFEST, so a missing version cannot be a refusal.
  for (store in list(fit_store_dir(manifest = FALSE), fit_store_dir(NULL))) {
    shims <- new.env()
    msgs <- capture_messages(
      res <- fs$fit_store_install(store, envir = shims)
    )
    expect_true(res)
    expect_match(msgs, "records no bayesnec version", all = FALSE)
    expect_setequal(ls(shims), fs$FIT_FUNS)
    unlink(store, recursive = TRUE)
  }
})

test_that("a version that cannot be read is refused", {
  fs <- fit_store_env()
  store <- fit_store_dir("unknown")
  shims <- new.env()
  expect_error(
    suppressMessages(fs$fit_store_install(store, envir = shims)),
    "is not a version number"
  )
  expect_length(ls(shims), 0L)
  unlink(store, recursive = TRUE)
})

test_that("an unset BAYESNEC_FIT_STORE installs nothing", {
  fs <- fit_store_env()
  shims <- new.env()
  expect_false(fs$fit_store_install("", envir = shims))
  expect_length(ls(shims), 0L)
})
