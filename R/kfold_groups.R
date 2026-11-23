#' Exact K-fold cross-validation over grouping levels
#'
#' @param x A \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#' @param group The grouping whose levels are held out. Required when the
#' grouping has no deviation in the equation, such as a cell identifier in an
#' equation with only a plate term. Its column may be retained outside the
#' fitted model frame.
#' @param K Number of folds. Ignored when \code{folds} is supplied.
#' @param folds An optional named integer vector assigning every level of the
#' selected grouping to a fold numbered from one.
#' @param fold One fold number to compute in this process. By default every
#' fold is computed in sequence.
#' @param iter,warmup Optional iteration and warm-up counts for the refits.
#' The original fit's settings are used when these are omitted.
#' @param seed Seed used to assign levels to folds and to prepare the held-out
#' predictions. It seeds a refit if the original fit recorded no seed. The
#' realised assignment is stored in the result.
#'
#' @return For a complete run, a \code{\link[loo]{loo}} object of class
#' \code{kfold} for one equation, or a named list of them for a model set.
#' Each pointwise row is one level. With \code{fold} specified, a
#' \code{kfold_groups_part} is returned; combine the parts with
#' \code{\link{kfold_combine}}.
#'
#' @details The grouping must have one concentration per level and at most two
#' independent normal intercept deviations per equation. The deviations of a
#' held-out level are integrated jointly through \code{brms::log_lik()}.
#' Other groupings must retain all their levels in every training fold.
#' A separate process can read the same saved fit, pass \code{fold = 1} and
#' save the returned part. Pass its \code{folds} vector to the calls for the
#' remaining folds, then pass all parts to \code{kfold_combine()}.
#' \code{loo::loo_compare()} accepts the complete criteria. The result records
#' each refit's largest R-hat, divergent transitions, share at maximum tree
#' depth, elapsed seconds and any error. The \code{p_kfold} column is
#' \code{NA}: the full-data log predictive density needed for that difference
#' is not computed. The \code{elpd_kfold} and \code{kfoldic} columns are exact
#' cross-validation estimates.
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
  parts <- lapply(fold_ids, function(k) {
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
  })
  if (!is.null(fold)) return(parts[[1]])
  do.call(kfold_combine, parts)
}

#' Combine separately computed folds
#'
#' @param ... \code{kfold_groups_part} objects from the same original fit,
#' assignment and refit settings. A list of parts may also be supplied.
#'
#' @return One \code{kfold} criterion per equation, or a named list for a set.
#' If a fold failed or is absent, a \code{kfold_groups_part} is returned with
#' the available scores and diagnostic records. It can be combined later with
#' replacement parts.
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
  bundled <- any(vapply(parts, function(p) !is.null(p$parts), logical(1)))
  if (bundled) {
    parts <- unlist(lapply(parts, function(p) {
      if (is.null(p$parts)) list(p) else p$parts
    }), recursive = FALSE)
  }
  first <- parts[[1]]
  for (part in parts[-1]) {
    for (field in c("group", "folds", "settings", "signature")) {
      if (!identical(first[[field]], part[[field]])) {
        stop("The fold parts have different ", field, ".", call. = FALSE)
      }
    }
  }
  ids <- vapply(parts, `[[`, integer(1), "fold")
  if (anyDuplicated(ids)) {
    if (!bundled) {
      stop("The fold parts contain a duplicate fold.", call. = FALSE)
    }
    keep <- !duplicated(ids, fromLast = TRUE)
    parts <- parts[keep]
    ids <- ids[keep]
  }
  parts <- parts[order(ids)]
  names(parts) <- as.character(sort(ids))
  all_ids <- seq_len(max(first$folds))
  scores <- lapply(names(first$signature), function(m) {
    val <- stats::setNames(rep(NA_real_, length(first$folds)),
                           names(first$folds))
    diagnostics <- vector("list", length(all_ids))
    names(diagnostics) <- as.character(all_ids)
    for (part in parts) {
      one <- part$results[[m]]
      if (is.null(one$error)) val[names(one$elpd)] <- one$elpd
      diagnostics[[as.character(part$fold)]] <- if (is.null(one$diagnostics))
        list(error = one$error) else one$diagnostics
    }
    list(elpd = val, diagnostics = diagnostics)
  })
  names(scores) <- names(first$signature)
  if (!setequal(ids, all_ids) || any(vapply(scores, function(s)
                                          any(!is.finite(s$elpd)), logical(1)))) {
    warning("Some folds have no score; combine the missing or replacement",
            " fold parts before comparing or weighting equations.",
            call. = FALSE)
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
  old <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (is.null(old)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", old, envir = .GlobalEnv)
  })
  set.seed(seed)
  out <- stats::setNames(integer(length(lev)), lev)
  offset <- 0L
  for (value in sort(unique(concentration))) {
    at <- sample(lev[concentration == value])
    out[at] <- (seq_along(at) - 1L + offset) %% K + 1L
    offset <- offset + length(at)
  }
  out
}

#' @noRd
kfold_fit_key <- function(fit, data = fit$data) {
  path <- tempfile("bayesnec-kfold-")
  on.exit(unlink(path))
  saveRDS(list(fit = fit, data = data), path, compress = FALSE)
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
  set.seed(as.integer((as.double(settings$seed) + fold) %%
                      .Machine$integer.max))
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
  if (!all(is.finite(elpd))) {
    stop("A held-out level had no finite log predictive density.",
         call. = FALSE)
  }
  np <- brms::nuts_params(refit)
  depth <- np$Value[np$Parameter == "treedepth__"]
  max_depth <- refit$stan_args$control$max_treedepth
  if (!is.numeric(max_depth) || length(max_depth) != 1L) max_depth <- 10
  rh <- suppressWarnings(max(brms::rhat(refit), na.rm = TRUE))
  list(elpd = elpd, diagnostics = list(
    max_rhat = if (is.finite(rh)) rh else NA_real_,
    divergent = sum(np$Value[np$Parameter == "divergent__"]),
    share_max_treedepth = if (length(depth)) mean(depth >= max_depth) else NA_real_,
    seconds = seconds, error = NULL))
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

#' Apply exact groupwise scores as model weights
#'
#' @param x A \code{\link{bayesmanecfit}}.
#' @param criteria The named list returned by \code{kfold_groups()} or
#' \code{kfold_combine()} for the same fit.
#' @param method \code{"pseudobma"} or \code{"stacking"}. By default the
#' set's current weighting method is retained.
#'
#' @return The model set reweighted by the exact K-fold scores.
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
