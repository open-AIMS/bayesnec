## Loading precompiled fits from a store, instead of fitting them.
##
## GENERATED FILE -- do not edit here.
## Source: grouping-structures/R/keys.R + shim/fit_store_body.R
## Regenerate with: Rscript analysis/build_shim.R <path to this file>
##
## example8 fits 189 models. Run in sequence at the cluster's four cores that
## is the better part of a day, which is longer than the walltime and far
## longer than a vignette should take to rebuild after a prose edit. The
## compendium at open-AIMS/grouping-structures runs every one of those models
## as its own array task and writes the assembled fits to a store; this file
## is how the store reaches the render.
##
## Set BAYESNEC_FIT_STORE to the directory. Leave it unset and nothing here
## does anything, so an ordinary precompile is unaffected.
## keys_sha256: 25db16bf6152ac6c

## The key a stored fit is filed under.
##
## The same key is computed in two places that never see each other's code: here,
## from the call this compendium extracted out of the vignette source, and in
## bayesnec's shim/fit_store.R, from the call the vignette actually makes while
## it is being knitted. Agreement between the two is what makes the store
## trustworthy, so the key is derived from the call and the data rather than from
## a name either side chooses. A vignette edit that changes a fit call changes
## its key, the render finds nothing under it, and the render stops -- which is
## the whole point. Keying by name instead would have loaded the old fit under
## the new code and said nothing.
##
## Kept free of every dependency but digest, because this file is copied verbatim
## into bayesnec and has to behave identically there.

FIT_FUNS <- c("bnec", "bnec_group", "bnec_hurdle", "bnec_joint")

## The fitting functions whose first argument is a fitted object rather than a
## data frame. bnec_joint() refits a bayesnecgroupfit or a bayesnechurdlefit as
## one model, so what it is handed is the result of an earlier call in the same
## vignette. There is no `data` to digest, and keying on the call alone would
## file two joint refits of two different grouped fits under one key.
FIT_FUNS_ON_FIT <- c("bnec_joint")

## bayesnec::bnec -> "bnec"; bnec -> "bnec"; anything else -> NA.
fit_fun_name <- function(e) {
  if (!is.call(e)) return(NA_character_)
  nm <- e[[1L]]
  if (is.call(nm) && as.character(nm[[1L]]) %in% c("::", ":::")) {
    nm <- nm[[3L]]
  }
  if (!is.name(nm)) return(NA_character_)
  nm <- as.character(nm)
  if (nm %in% FIT_FUNS) nm else NA_character_
}

## A call reduced to text that does not depend on how it was written.
##
## match.call() against the real definition names every argument and puts them in
## the formals' order, so `bnec(f, d)` and `bnec(data = d, formula = f)` normalise
## to the same string. The arguments are then sorted by name, which the formals'
## order already almost gives but which `...` arguments do not follow. `data` is
## dropped and digested separately: it is a whole data frame, and deparsing one
## would be both enormous and dependent on print width.
##
## `object` is dropped for the same reason and on the same terms, but only for
## the functions that take one: it names the fit being refitted, and what
## identifies that fit is its own key rather than the symbol the vignette
## happened to bind it to. joint_key() digests the key in its place.
##
## bnec_joint() is a generic, and the definition matched against here is the
## generic rather than the method, so only `object` is named by match.call() and
## everything else keeps whatever name the call gave it. Matching against the
## method would normalise `model` and `formula` as well, but it would mean the
## two sides choosing a method -- by class during the render and from the
## prerequisite's own function here -- and the whole value of this file is that
## both sides reach the same string without consulting each other.
##
## deparse() with a wide cutoff rather than deparse1(), because the file has to
## run under the R in the container as well as here.
normalise_fit_call <- function(cl, fn_name = fit_fun_name(cl)) {
  if (is.na(fn_name)) {
    stop("not a bayesnec fitting call: ", paste(deparse(cl), collapse = " "))
  }
  def <- get(fn_name, envir = asNamespace("bayesnec"))
  cl <- match.call(def, cl, expand.dots = TRUE)
  cl$data <- NULL
  if (fn_name %in% FIT_FUNS_ON_FIT) {
    cl$object <- NULL
  }
  args <- as.list(cl)[-1L]
  if (length(args)) {
    args <- args[order(names(args))]
  }
  parts <- vapply(seq_along(args), function(i) {
    paste0(names(args)[[i]], "=",
           paste(deparse(args[[i]], width.cutoff = 500L), collapse = " "))
  }, character(1L))
  paste(c(fn_name, parts), collapse = ";")
}

## The data frame is digested rather than deparsed. Both sides build it by
## running the vignette's own preparation code, so the objects are identical
## rather than merely equivalent, and digest() on the object itself is then both
## exact and cheap. It is exact about the things that decide a fit and are easy
## to change by accident: row order, factor levels and their order, and the
## numeric values to full precision.
fit_key <- function(cl, data, fn_name = fit_fun_name(cl)) {
  substr(digest::digest(list(call = normalise_fit_call(cl, fn_name),
                             data = data),
                        algo = "sha256"), 1L, 16L)
}

## A joint refit is keyed on its call and on the key of the fit it refits.
##
## The dependency is part of the identity rather than beside it: change the
## bnec_group() call and its key changes, so the joint refit's key changes with
## it and the render stops at the joint chunk instead of loading a refit of the
## previous draft. That is the same contract the data digest keeps for every
## other call in the store, reached the only way it can be reached for a call
## that is handed a fit rather than data.
##
## The list element is named `depends_on` rather than `data`, which keeps a
## joint refit's key out of the space fit_key() addresses even where a
## prerequisite key and a data frame happened to digest alike.
joint_key <- function(cl, depends_on, fn_name = fit_fun_name(cl)) {
  substr(digest::digest(list(call = normalise_fit_call(cl, fn_name),
                             depends_on = depends_on),
                        algo = "sha256"), 1L, 16L)
}

## ---------------------------------------------------------------------------
## The shim
## ---------------------------------------------------------------------------
##
## Replaces bnec(), bnec_group() and bnec_hurdle() for the duration of a
## precompile with functions that load a fit from the store instead of running
## one. Installed only when BAYESNEC_FIT_STORE names a directory, so an ordinary
## precompile is untouched.
##
## What the reader sees is unaffected: knitr echoes the chunk's source, so the
## vignette still shows the bnec() call the analysis was actually run from. What
## changes is only that the call is answered from a file rather than by sampling
## for the twenty minutes it took on the cluster.
##
## A key that is not in the store stops the render. That is the design and not a
## limitation. The alternative -- falling back to fitting -- would turn a
## vignette edit that nobody noticed into a twenty-two hour render, or worse,
## into a render where one chunk was fitted fresh and the rest came from a store
## built against a different draft.

fit_store_install <- function(store = Sys.getenv("BAYESNEC_FIT_STORE"),
                              envir = globalenv()) {
  if (!nzchar(store)) return(invisible(FALSE))
  if (!dir.exists(store)) {
    stop("BAYESNEC_FIT_STORE is set to \"", store, "\", which is not a ",
         "directory.", call. = FALSE)
  }
  manifest <- file.path(store, "MANIFEST")
  message("Fit store: ", normalizePath(store))
  if (file.exists(manifest)) {
    message(paste0("  ", readLines(manifest, warn = FALSE), collapse = "\n"))
  }
  for (nm in FIT_FUNS) {
    assign(nm, fit_store_shim(nm, store), envir = envir)
  }
  invisible(TRUE)
}

fit_store_shim <- function(fn_name, store) {
  force(fn_name); force(store)
  function(...) {
    cl <- sys.call()
    caller <- parent.frame()
    def <- get(fn_name, envir = asNamespace("bayesnec"))
    mcl <- match.call(def, cl, expand.dots = TRUE)
    key <- if (fn_name %in% FIT_FUNS_ON_FIT) {
      joint_key(cl, fit_store_prereq_key(mcl, fn_name, caller), fn_name)
    } else {
      if (is.null(mcl$data)) {
        stop("the fit store needs `data` named or matched in the call to ",
             fn_name, "().", call. = FALSE)
      }
      fit_key(cl, eval(mcl$data, caller), fn_name)
    }
    path <- file.path(store, paste0(key, ".rds"))
    if (!file.exists(path)) {
      fit_store_miss(key, cl, fn_name, store)
    }
    message("fit store: ", key, " <- ", basename(path))
    # Every object the store hands back is stamped with the key it came from, so
    # that a later call which refits it can name its prerequisite without being
    # told. The compendium reaches the same key by following the variable back to
    # the earlier fit call in the vignette source, which is a different route to
    # the same string -- and two routes that agree is the property the whole
    # store rests on.
    obj <- readRDS(path)
    attr(obj, "fit_store_key") <- key
    obj
  }
}

## The key of the fit a joint refit was handed.
##
## Read off the object rather than off the symbol, because the symbol is not
## evidence: a variable rebound between the grouped fit and the joint refit would
## still be called `fits_herb`. The stamp is only on an object this shim
## returned, so a missing one means the fit reaching bnec_joint() is not the one
## the compendium planned against, and that is reported rather than guessed
## around.
fit_store_prereq_key <- function(mcl, fn_name, caller) {
  if (is.null(mcl$object)) {
    stop("the fit store needs `object` named or matched in the call to ",
         fn_name, "().", call. = FALSE)
  }
  obj <- eval(mcl$object, caller)
  dep <- attr(obj, "fit_store_key")
  if (is.null(dep)) {
    stop("the fit handed to ", fn_name, "() did not come from the fit store.\n",
         "  object: ", paste(deparse(mcl$object, width.cutoff = 500L),
                             collapse = " "), "\n",
         "  A joint refit is keyed on the key of the fit it refits, so that fit\n",
         "  has to be one the store answered. It is not, which means something\n",
         "  between the two calls returned a new object -- screen_models() and\n",
         "  amend() both do. Refit jointly from the object the fitting call\n",
         "  returned, or teach the compendium about the step in between.",
         call. = FALSE)
  }
  dep
}

## A miss is reported with everything needed to find the cause: the key, the
## normalised form of the call the key was computed from, and what the store does
## hold. Without the normalised form the only information is that two hashes
## differ, which says nothing about which argument moved.
fit_store_miss <- function(key, cl, fn_name, store) {
  idx <- file.path(store, "index.csv")
  held <- if (file.exists(idx)) {
    d <- utils::read.csv(idx, stringsAsFactors = FALSE)
    paste0("  ", d$key, "  ", d$target, "  (", d$n_units, " units)",
           collapse = "\n")
  } else {
    "  (no index.csv in the store)"
  }
  stop("no stored fit under key ", key, "\n",
       "  call: ", paste(deparse(cl, width.cutoff = 500L), collapse = " "), "\n",
       "  normalised: ", normalise_fit_call(cl, fn_name), "\n",
       "  the store holds:\n", held, "\n",
       "  This call has changed since the store was built. Rebuild it in the\n",
       "  grouping-structures compendium: analysis/build_manifest.R, then the\n",
       "  array, then analysis/assemble_store.R.", call. = FALSE)
}
