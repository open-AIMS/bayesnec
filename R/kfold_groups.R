#' Exact K-fold cross-validation over grouping levels
#'
#' Refits each equation once without each fold of the levels of a grouping,
#' and scores every held-out level from that refit with all of its own
#' deviations integrated out.
#'
#' @param x A \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#' @param group The grouping whose levels are held out, as a column name or
#' an interaction such as \code{"plate:conc_rank"}. By default, the one
#' grouping with a deviation in the fit whose levels are each observed at a
#' single concentration. Name it where its levels have no deviation in the
#' equation, such as a cell identifier in an equation with only a plate term.
#' Its column may then be one kept in \code{x$retained_data} rather than in
#' the fitted data.
#' @param K The number of folds. Ignored when \code{folds} is supplied.
#' @param folds A named integer vector assigning every level of the grouping
#' to a fold, numbered consecutively from one. By default the levels at each
#' concentration are assigned to the folds in turn, in random order, so that
#' each fold spans the concentration series.
#' @param fold One fold to compute in this process. By default every fold is
#' computed in sequence.
#' @param iter,warmup The iterations and warm-up of each refit. By default,
#' those of the fit.
#' @param seed The seed of the fold assignment and of the draws that start
#' the integration over each held-out level's deviations. The refits use the
#' seed the fit recorded, and this one only where it recorded none.
#'
#' @details Each level of the grouping must be observed at a single
#' concentration. Each equation may have up to two deviations on the grouping.
#' Each must be a normal intercept, independent of the other, as for an
#' \code{ogl()} term together with one \code{(par | group)} term. Every other
#' grouping must keep all its levels in each training set, so a fold that
#' removes a whole plate is refused.
#'
#' Each refit takes the fit's formula, priors, family, backend, chains,
#' sampler control and seed, with the fit's initial values and its
#' group-level deviations started at zero. As in \code{brms::update()}, its
#' chains run on \code{getOption("mc.cores", 1)} cores. A held-out level is
#' scored by the log of the mean, over the refit's draws, of the likelihood of
#' its observations with its deviations integrated out by Gauss--Legendre
#' quadrature, nested where there are two. The other group-level deviations
#' are taken at each draw. The likelihood is evaluated by
#' \code{brms::log_lik()}, so censoring, the transform of an \code{ogl()} term
#' and a \code{disp()} model are taken from the fit.
#'
#' To compute the folds in separate R processes, read the same saved fit in
#' each, pass the same \code{folds} (or \code{K} and \code{seed}), \code{iter}
#' and \code{warmup}, and one \code{fold}, and save the returned part.
#' \code{\link{kfold_combine}} joins the parts.
#'
#' The result records each refit's largest R-hat, number of divergent
#' transitions, share of transitions at the maximum tree depth, elapsed
#' seconds and any error. A fold whose refit fails, or which gives a held-out
#' level no finite log predictive density, leaves its levels without a score
#' and gives a warning.
#'
#' \code{loo::loo_compare()} compares the criteria of two fits to the same
#' data and levels. The pointwise rows are named by level, so that the
#' differences can be summed within a coarser grouping, such as plate, for
#' its standard error. The \code{p_kfold} column is \code{NA}, because the log
#' predictive density of the full fit is not computed.
#'
#' @return Where every fold is computed and scored, a criterion of class
#' \code{c("kfold", "loo")} with one pointwise row per level, or for a model
#' set a named list of them, one per equation. Its attributes \code{group},
#' \code{folds}, \code{refit_settings} and \code{refit_diagnostics} record the
#' grouping, the realised assignment, the refits' \code{iter}, \code{warmup}
#' and \code{seed}, and each refit's diagnostics. Where \code{fold} is given,
#' or any fold has no score, a \code{kfold_groups_part} for
#' \code{\link{kfold_combine}}.
#'
#' @seealso \code{\link{kfold_combine}}, \code{\link{kfold_weights}}
#'
#' @export
kfold_groups <- function(x, group = NULL, K = 10, folds = NULL, fold = NULL,
                         iter = NULL, warmup = NULL, seed = 1L) {
  if (!inherits(x, c("bayesnecfit", "bayesmanecfit"))) {
    stop("x must be a bayesnecfit or bayesmanecfit.", call. = FALSE)
  }
  if (!is.null(iter) && (!is.numeric(iter) || length(iter) != 1L ||
                        is.na(iter) || iter < 2 ||
                        iter > .Machine$integer.max ||
                        iter != as.integer(iter))) {
    stop("iter must be a positive integer greater than one.", call. = FALSE)
  }
  if (!is.null(warmup) && (!is.numeric(warmup) || length(warmup) != 1L ||
                          is.na(warmup) || warmup < 1 ||
                          warmup > .Machine$integer.max ||
                          warmup != as.integer(warmup))) {
    stop("warmup must be a positive integer.", call. = FALSE)
  }
  if (!is.null(iter) && !is.null(warmup) && warmup >= iter) {
    stop("warmup must be less than iter.", call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
      seed < 1 || seed > .Machine$integer.max ||
      seed != as.integer(seed)) {
    stop("seed must be a positive integer.", call. = FALSE)
  }
  fits <- if (inherits(x, "bayesmanecfit")) x$mod_fits else
    stats::setNames(list(x), x$model)
  f <- fits[[1]]$fit
  data <- kfold_set_data(x, fits)
  dat <- model.frame(fits[[1]]$bayesnecformula, data = f$data)
  predictor <- retrieve_var(dat, "x_var", error = TRUE)
  if (is.null(group)) {
    by_group <- logo_grouping(fits, predictor)
    if (is.null(by_group$group)) {
      stop("No suitable grouping can be left out", if (!is.null(by_group$reason))
        paste0(": ", by_group$reason), ".", call. = FALSE)
    }
    group <- by_group$group
  } else {
    if (!is.character(group) || length(group) != 1L || is.na(group) ||
        !all(strsplit(group, ":", fixed = TRUE)[[1]] %in% names(data))) {
      stop("group must name a grouping in the fitted or retained data.",
           call. = FALSE)
    }
    for (m in names(fits)) {
      reason <- logo_slot(fits[[m]]$fit, group)$reason
      if (!is.null(reason)) stop(reason, " in ", m, ".", call. = FALSE)
    }
  }
  group_fit <- f
  group_fit$data <- data
  level <- logo_levels(group_fit, group)
  if (!all(tapply(predictor, level, function(v) length(unique(v))) == 1)) {
    stop("Each level of group must occur at one concentration.",
         call. = FALSE)
  }
  fold_map <- kfold_assign(level, predictor, K, folds, seed)
  K <- max(fold_map)
  for (m in names(fits)) {
    f_m <- fits[[m]]$fit
    for (other in setdiff(unique(f_m$ranef$group), group)) {
      other_levels <- logo_levels(f_m, other)
      for (k in seq_len(K)) {
        kept <- fold_map[as.character(level)] != k
        if (any(!levels(other_levels) %in%
                levels(droplevels(other_levels[kept])))) {
          stop("Fold ", k, " removes a whole level of ", other,
               " in ", m, "; its deviation would be unknown.",
               call. = FALSE)
        }
      }
    }
  }
  if (!is.null(fold) && (!is.numeric(fold) || length(fold) != 1L ||
                        is.na(fold) || !(fold %in% seq_len(K)))) {
    stop("fold must name one of the assigned folds.", call. = FALSE)
  }
  original_iter <- vapply(fits, function(o) kfold_fit_settings(o$fit)[["iter"]],
                          numeric(1))
  original_warmup <- vapply(fits, function(o)
                            kfold_fit_settings(o$fit)[["warmup"]], numeric(1))
  if (is.null(iter) && length(unique(original_iter)) != 1L ||
      is.null(warmup) && length(unique(original_warmup)) != 1L) {
    stop("The equations used different iteration settings; supply iter and",
         " warmup for their refits.", call. = FALSE)
  }
  settings <- list(iter = as.integer(if (is.null(iter)) original_iter[[1]]
                                    else iter),
                   warmup = as.integer(if (is.null(warmup))
                                       original_warmup[[1]] else warmup),
                   seed = as.integer(seed))
  if (settings$warmup >= settings$iter) {
    stop("The refit warmup must be less than its iterations.",
         call. = FALSE)
  }
  signature <- lapply(fits, function(o) kfold_fit_key(o$fit, data))
  fold_ids <- if (is.null(fold)) seq_len(K) else as.integer(fold)
  parts <- with_preserved_rng_state(lapply(fold_ids, function(k) {
    result <- lapply(names(fits), function(m) {
      o <- fits[[m]]
      started <- proc.time()[["elapsed"]]
      one <- tryCatch(kfold_refit(o, data, group, level, fold_map, k,
                                 settings),
                      error = function(e) list(
                        error = conditionMessage(e),
                        diagnostics = list(
                          max_rhat = NA_real_, divergent = NA_real_,
                          share_max_treedepth = NA_real_,
                          seconds = proc.time()[["elapsed"]] - started,
                          error = conditionMessage(e))))
      if (!is.null(one$error)) {
        warning("Fold ", k, " of ", m, " failed: ", one$error,
                call. = FALSE)
      }
      one
    })
    names(result) <- names(fits)
    structure(list(group = group, folds = fold_map, settings = settings,
                   signature = signature, fold = k, results = result),
              class = "kfold_groups_part")
  }))
  if (!is.null(fold)) return(parts[[1]])
  do.call(kfold_combine, parts)
}

#' Combine separately computed folds
#'
#' Joins the folds that \code{\link{kfold_groups}} computed in separate calls
#' into one criterion per equation.
#'
#' @param ... \code{kfold_groups_part} objects from the same fit, grouping,
#' fold assignment and refit settings, or one list of them. A part returned
#' by an earlier call with folds still unscored may be among them.
#'
#' @details The parts may be given in any order, and may repeat a fold. For
#' each fold and equation the first scored result is used, so that a part
#' recomputing a failed fold replaces it whether it comes before or after it.
#'
#' @return Where every level of every equation has a score, one criterion of
#' class \code{c("kfold", "loo")} per equation, or a named list of them for a
#' model set, as \code{\link{kfold_groups}} returns. Otherwise a warning names
#' the folds without a score, and a \code{kfold_groups_part} holds the scores
#' and diagnostics so far. Pass it to \code{kfold_combine()} again with the
#' parts that recompute those folds.
#'
#' @export
kfold_combine <- function(...) {
  parts <- list(...)
  if (length(parts) == 1L && is.list(parts[[1]]) &&
      !inherits(parts[[1]], "kfold_groups_part")) parts <- parts[[1]]
  if (!length(parts) || !all(vapply(parts, inherits, logical(1),
                                   "kfold_groups_part"))) {
    stop("Supply kfold_groups_part objects.", call. = FALSE)
  }
  # An incomplete result holds the parts it was combined from.
  parts <- unlist(lapply(parts, function(p) {
    if (is.null(p$parts)) list(p) else p$parts
  }), recursive = FALSE)
  first <- parts[[1]]
  for (part in parts[-1]) {
    for (field in c("group", "folds", "settings", "signature")) {
      if (!identical(first[[field]], part[[field]])) {
        stop("The fold parts have different ", field, ".", call. = FALSE)
      }
    }
  }
  all_ids <- as.character(seq_len(max(first$folds)))
  scores <- lapply(names(first$signature), function(m) {
    val <- stats::setNames(rep(NA_real_, length(first$folds)),
                           names(first$folds))
    diagnostics <- stats::setNames(vector("list", length(all_ids)), all_ids)
    for (part in parts) {
      k <- as.character(part$fold)
      held <- diagnostics[[k]]
      if (!is.null(held) && is.null(held$error)) next
      one <- part$results[[m]]
      if (is.null(one$error)) val[names(one$elpd)] <- one$elpd
      diagnostics[[k]] <- if (is.null(one$diagnostics))
        list(error = one$error) else one$diagnostics
    }
    list(elpd = val, diagnostics = diagnostics)
  })
  names(scores) <- names(first$signature)
  unscored <- lapply(scores, function(s) {
    sort(unique(first$folds[names(s$elpd)[!is.finite(s$elpd)]]))
  })
  unscored <- unscored[lengths(unscored) > 0]
  if (length(unscored) > 0) {
    warning("Not every fold has a score (",
            paste0("fold", ifelse(lengths(unscored) > 1, "s ", " "),
                   vapply(unscored, paste0, character(1), collapse = ", "),
                   " of ", names(unscored), collapse = "; "),
            "). Pass the parts that recompute ",
            if (length(unlist(unscored)) > 1) "them" else "it",
            " to kfold_combine() with this result before comparing or",
            " weighting equations.", call. = FALSE)
    return(structure(c(first[c("group", "folds", "settings", "signature")],
                       list(fold = NA_integer_, results = scores,
                            parts = parts)), class = "kfold_groups_part"))
  }
  out <- lapply(names(scores), function(m) {
    kfold_criterion(scores[[m]]$elpd, first$group, first$folds,
                    first$settings, scores[[m]]$diagnostics,
                    first$signature[[m]])
  })
  names(out) <- names(scores)
  if (length(out) == 1L) out[[1]] else out
}

#' Assign the levels of a grouping to folds
#'
#' @param level The level of every observation.
#' @param predictor The concentration of every observation.
#' @param K,folds,seed As in \code{kfold_groups()}.
#'
#' @return A named integer vector, one element per level.
#'
#' @noRd
kfold_assign <- function(level, predictor, K, folds, seed) {
  lev <- levels(level)
  if (!is.null(folds)) {
    if (!is.numeric(folds) || is.null(names(folds)) ||
        anyDuplicated(names(folds)) || !setequal(names(folds), lev) ||
        anyNA(folds) || any(folds < 1) ||
        any(folds > .Machine$integer.max) ||
        any(folds != as.integer(folds)) ||
        max(folds) < 2 ||
        !identical(sort(unique(as.integer(folds))),
                   seq_len(max(folds)))) {
      stop("folds must assign every level once to consecutive positive",
           " integer folds.", call. = FALSE)
    }
    return(as.integer(folds[lev]) |> stats::setNames(lev))
  }
  if (!is.numeric(K) || length(K) != 1L || is.na(K) ||
      K < 2 || K > length(lev) || K != as.integer(K)) {
    stop("K must be an integer from two to the number of levels.",
         call. = FALSE)
  }
  concentration <- vapply(lev, function(g) unique(predictor[level == g]),
                          numeric(1))
  with_preserved_rng_state({
    kfold_set_seed(seed)
    out <- stats::setNames(integer(length(lev)), lev)
    offset <- 0L
    for (value in sort(unique(concentration))) {
      at <- sample(lev[concentration == value])
      out[at] <- (seq_along(at) - 1L + offset) %% K + 1L
      offset <- offset + length(at)
    }
    out
  })
}

#' Seed R's generator independently of the session's generator kinds
#' @param seed An integer seed.
#' @noRd
kfold_set_seed <- function(seed) {
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion",
           sample.kind = "Rejection")
}

#' A fingerprint of a fit and the data its folds were drawn from
#'
#' @param fit A \code{\link[brms]{brmsfit}}.
#' @param data The data, with any retained columns.
#'
#' @details Hashes the data and the last draw of every parameter in every
#' chain. It omits \code{fit$criteria}, which \pkg{bayesnec} rewrites when it
#' weights a set. Write no \code{serialize()} output into the hash: its header
#' records the R version and the native encoding.
#'
#' @return An MD5 hash.
#'
#' @noRd
kfold_fit_key <- function(fit, data = fit$data) {
  path <- tempfile("bayesnec-kfold-")
  on.exit(unlink(path))
  con <- file(path, open = "wb")
  put <- function(v) {
    v <- as.vector(v)
    if (is.numeric(v) || is.logical(v)) {
      writeBin(as.double(v), con, size = 8L, endian = "big")
    } else {
      writeBin(enc2utf8(as.character(v)), con)
    }
  }
  put(names(data))
  for (v in data) put(v)
  for (chain in fit$fit@sim$samples) {
    put(names(chain))
    put(vapply(chain, function(v) if (length(v)) v[[length(v)]] else NA_real_,
               numeric(1)))
  }
  close(con)
  unname(tools::md5sum(path))
}

#' @noRd
kfold_fit_settings <- function(fit) {
  c(iter = fit$fit@sim$iter, warmup = fit$fit@sim$warmup)
}

#' @noRd
kfold_data <- function(x, fit) {
  data <- fit$data
  retained <- x$retained_data
  if (!is.null(retained)) {
    if (nrow(retained) != nrow(data)) {
      stop("The retained data have a different number of rows.",
           call. = FALSE)
    }
    data <- cbind(data, retained[setdiff(names(retained), names(data))])
  }
  data
}

#' @noRd
kfold_set_data <- function(x, fits) {
  data <- kfold_data(x, fits[[1]]$fit)
  for (m in names(fits)[-1]) {
    other_data <- fits[[m]]$fit$data
    shared <- intersect(names(other_data), names(data))
    if (nrow(other_data) != nrow(data) ||
        !all(vapply(shared, function(v) {
          isTRUE(all.equal(other_data[[v]], data[[v]],
                           check.attributes = FALSE))
        }, logical(1)))) {
      stop("The equations must use the same rows in the same order.",
           call. = FALSE)
    }
    missing <- setdiff(names(other_data), names(data))
    data <- cbind(data, other_data[missing])
  }
  data
}

#' Refit an equation without one fold and score its held-out levels
#'
#' @param o The equation's \code{\link{prebayesnecfit}} or
#' \code{\link{bayesnecfit}}.
#' @param data The data, with any retained columns.
#' @param group,level The grouping and the level of every observation.
#' @param fold_map The fold of every level.
#' @param fold The fold to hold out.
#' @param settings The refits' \code{iter}, \code{warmup} and \code{seed}.
#'
#' @return From \code{kfold_result()}. A refit that fails is an error.
#'
#' @noRd
kfold_refit <- function(o, data, group, level, fold_map, fold, settings) {
  f <- o$fit
  held <- fold_map[as.character(level)] == fold
  train <- droplevels(data[!held, , drop = FALSE])
  test <- data[held, , drop = FALSE]
  args <- list(f, newdata = train, init = refit_inits(o$init, f, train),
               refresh = 0, silent = 2)
  if (!is.null(settings$iter)) args$iter <- settings$iter
  if (!is.null(settings$warmup)) args$warmup <- settings$warmup
  if (!is.null(o$call_args$seed)) args$seed <- o$call_args$seed else
    args$seed <- settings$seed
  start <- proc.time()[["elapsed"]]
  refit <- suppressWarnings(suppressMessages(do.call(stats::update, args)))
  seconds <- proc.time()[["elapsed"]] - start
  if (brms::ndraws(refit) == 0L) stop("the refit returned no draws.",
                                    call. = FALSE)
  kfold_set_seed((as.double(settings$seed) + fold) %% .Machine$integer.max)
  lev <- droplevels(level[held])
  chunks <- split(seq_len(brms::ndraws(refit)),
                  ceiling(seq_len(brms::ndraws(refit)) / 250L))
  ll <- do.call(rbind, lapply(chunks, function(ids) {
    prep <- brms::prepare_predictions(refit, newdata = test,
              draw_ids = ids, allow_new_levels = TRUE,
              sample_new_levels = "gaussian")
    logo_integrate_slots(refit, prep, group, lev, ids)
  }))
  colnames(ll) <- levels(lev)
  elpd <- apply(ll, 2, function(v) {
    m <- max(v)
    m + log(mean(exp(v - m)))
  })
  np <- brms::nuts_params(refit)
  depth <- np$Value[np$Parameter == "treedepth__"]
  max_depth <- refit$stan_args$control$max_treedepth
  if (!is.numeric(max_depth) || length(max_depth) != 1L) max_depth <- 10
  rh <- suppressWarnings(max(brms::rhat(refit), na.rm = TRUE))
  kfold_result(elpd, list(
    max_rhat = if (is.finite(rh)) rh else NA_real_,
    divergent = sum(np$Value[np$Parameter == "divergent__"]),
    share_max_treedepth = if (length(depth)) mean(depth >= max_depth) else NA_real_,
    seconds = seconds, error = NULL))
}

#' The result of one fold for one equation
#'
#' @param elpd The held-out levels' log predictive densities, named by level.
#' @param diagnostics The refit's diagnostics.
#'
#' @return A \code{\link[base]{list}} of \code{elpd} and \code{diagnostics}.
#' Where a level's density is not finite, \code{error} names the levels, and
#' \code{kfold_combine()} leaves the fold unscored but keeps the diagnostics.
#'
#' @noRd
kfold_result <- function(elpd, diagnostics) {
  bad <- names(elpd)[!is.finite(elpd)]
  if (length(bad) == 0L) {
    return(list(elpd = elpd, diagnostics = diagnostics))
  }
  error <- paste0("no finite log predictive density for ",
                  paste0(bad, collapse = ", "))
  diagnostics$error <- error
  list(elpd = elpd, error = error, diagnostics = diagnostics)
}

#' @noRd
kfold_criterion <- function(elpd, group, folds, settings, diagnostics,
                            signature) {
  n <- length(elpd)
  pointwise <- cbind(elpd_kfold = elpd, p_kfold = rep(NA_real_, n),
                     kfoldic = -2 * elpd)
  rownames(pointwise) <- names(elpd)
  estimates <- rbind(elpd_kfold = c(sum(elpd), sqrt(n * stats::var(elpd))),
                     p_kfold = c(NA_real_, NA_real_),
                     kfoldic = c(-2 * sum(elpd),
                                 2 * sqrt(n * stats::var(elpd))))
  colnames(estimates) <- c("Estimate", "SE")
  structure(list(estimates = estimates, pointwise = pointwise),
            class = c("kfold", "loo"), K = max(folds), group = group,
            folds = folds, refit_settings = settings,
            refit_diagnostics = diagnostics, fit_key = signature)
}

#' Weight a model set by exact K-fold scores
#'
#' Stores each equation's criterion from \code{\link{kfold_groups}} with the
#' equation and reweights the set by them, recording
#' \code{loo_controls$leave_out = "kfold"} so that \code{\link{amend}},
#' \code{\link{pull_out}} and \code{c()} keep the scores.
#'
#' @param x A \code{\link{bayesmanecfit}}.
#' @param criteria The named list returned by \code{\link{kfold_groups}} or
#' \code{\link{kfold_combine}} for the same set.
#' @param method \code{"pseudobma"} or \code{"stacking"}. By default the
#' set's current weighting method is retained.
#'
#' @details Each criterion must come from the same fit and data as the
#' equation it is given to, which is checked against a fingerprint of the
#' posterior draws and the data. \code{update()} and an \code{amend()} that
#' adds an equation refuse a set weighted this way unless
#' \code{loo_controls} names another \code{leave_out}, because the refitted
#' or added equations have no such score.
#'
#' @return The model set reweighted by the exact K-fold scores.
#'
#' @seealso \code{\link{kfold_groups}}
#'
#' @export
kfold_weights <- function(x, criteria, method = NULL) {
  if (!inherits(x, "bayesmanecfit") || !is.list(criteria) ||
      !setequal(names(criteria), names(x$mod_fits)) ||
      !all(vapply(criteria, inherits, logical(1), "kfold"))) {
    stop("Supply complete K-fold criteria for every equation in the set.",
         call. = FALSE)
  }
  data <- kfold_set_data(x, x$mod_fits)
  for (m in names(x$mod_fits)) {
    if (!identical(attr(criteria[[m]], "fit_key"),
                   kfold_fit_key(x$mod_fits[[m]]$fit, data))) {
      stop("The criterion for ", m, " comes from a different fit.",
           call. = FALSE)
    }
  }
  groups <- vapply(criteria, function(y) attr(y, "group"), character(1))
  folds <- lapply(criteria, function(y) attr(y, "folds"))
  if (length(unique(groups)) != 1L ||
      !all(vapply(folds, identical, logical(1), folds[[1]]))) {
    stop("The criteria have different groupings or fold assignments.",
         call. = FALSE)
  }
  if (is.null(method)) method <- fit_weights_method(x)
  fits <- recover_prebayesnecfit(x)
  for (m in names(fits)) fits[[m]]$fit$criteria$kfold <- criteria[[m]]
  controls <- list(leave_out = "kfold")
  if (!identical(method, fit_weights_method(x))) {
    controls$weights <- list(method = method)
  }
  amend_model_set(x, fits, old_method = fit_weights_method(x),
                  loo_controls = controls)
}
