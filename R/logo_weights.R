#' The grouping whose levels the model weights leave out
#'
#' @param fits A named \code{\link[base]{list}} of
#' \code{\link{prebayesnecfit}} objects forming one model set.
#' @param x The predictor values of the data the set was fitted to.
#'
#' @details A grouping qualifies when each of its levels holds a single
#' predictor value, so that its deviations are estimated from one
#' concentration only. Leaving out an observation of such a group leaves its
#' deviation to be estimated from the rest of the group, and the score then
#' barely depends on the curve.
#'
#' @return A \code{\link[base]{list}} with \code{group}, the grouping to leave
#' out or \code{NULL}, and \code{reason}, \code{NULL} or a sentence saying why
#' a qualifying grouping cannot be left out.
#'
#' @noRd
logo_grouping <- function(fits, x) {
  groups <- unique(unlist(lapply(fits, function(f) f$fit$ranef$group)))
  nested <- groups[vapply(groups, function(g) {
    with_g <- Find(function(f) g %in% f$fit$ranef$group, fits)
    level <- logo_levels(with_g$fit, g)
    all(tapply(x, level, function(v) length(unique(v))) == 1)
  }, logical(1))]
  if (length(nested) == 0) {
    return(list(group = NULL, reason = NULL))
  }
  if (length(nested) > 1) {
    return(list(group = NULL, reason = paste0(
      "more than one grouping (", paste0(nested, collapse = ", "),
      ") has levels at a single concentration"
    )))
  }
  for (m in names(fits)) {
    reason <- logo_slot(fits[[m]]$fit, nested)$reason
    if (!is.null(reason)) {
      return(list(group = NULL, reason = paste0(reason, " in ", m)))
    }
  }
  list(group = nested, reason = NULL)
}

#' The level of every observation in a brms grouping
#'
#' @param fit A \code{\link[brms]{brmsfit}}.
#' @param group A grouping as \code{fit$ranef$group} names it, such as
#' \code{"plate:conc_rank"}.
#'
#' @return A \code{\link[base]{factor}} with one element per row of
#' \code{fit$data}.
#'
#' @noRd
logo_levels <- function(fit, group) {
  vars <- strsplit(group, ":", fixed = TRUE)[[1]]
  interaction(fit$data[vars], drop = TRUE, sep = "_")
}

#' Where a grouping's deviations enter a fitted equation
#'
#' @param fit A \code{\link[brms]{brmsfit}}.
#' @param group A grouping as \code{fit$ranef$group} names it.
#' @param prep Optionally, the fit's \code{brms::prepare_predictions()}
#' output.
#'
#' @return A \code{\link[base]{list}}. \code{path} is \code{NULL} where the
#' equation has no deviation on \code{group}, and otherwise locates its draws
#' in \code{prep}. \code{slots} holds one entry per independent deviation.
#' \code{reason} names an unsupported term or correlation.
#'
#' @noRd
logo_slot <- function(fit, group, prep = NULL) {
  re <- fit$ranef[fit$ranef$group == group, , drop = FALSE]
  if (nrow(re) == 0) {
    return(list(path = NULL, reason = NULL))
  }
  unsupported <- if (nrow(re) > 2) {
    "more than two deviations"
  } else if (anyDuplicated(re$id)) {
    "correlated deviations"
  } else if (anyDuplicated(paste(re$nlpar, re$dpar))) {
    "more than one deviation on one parameter"
  } else NULL
  if (!is.null(unsupported)) {
    return(list(path = NULL,
                reason = paste0("the grouping ", group, " has ", unsupported)))
  }
  if (!all(re$coef == "Intercept") ||
      ("dist" %in% names(re) && !all(re$dist == "gaussian"))) {
    return(list(path = NULL, reason = paste0(
      "the grouping ", group, " has a term other than a normal intercept"
    )))
  }
  slots <- lapply(seq_len(nrow(re)), function(i) {
    path <- if (nzchar(re$nlpar[i])) c("nlpars", re$nlpar[i]) else
      c("dpars", re$dpar[i])
    par <- if (nzchar(re$nlpar[i])) re$nlpar[i] else re$dpar[i]
    slot <- list(path = path, sd_name = paste0("sd_", group, "__",
      if (par == "mu") "" else paste0(par, "_"), "Intercept"))
    if (!is.null(prep)) {
      part <- prep[[path[1]]][[path[2]]]$re
      slot$r <- part$r[[group]]
      z <- which(as.matrix(part$Z[[group]]) != 0, arr.ind = TRUE)
      slot$col_of_obs <- z[order(z[, "row"]), "col"]
    }
    slot
  })
  c(slots[[1]], list(slots = slots, reason = NULL))
}

#' Integrate every independent deviation on a held-out grouping
#' @noRd
logo_integrate_slots <- function(fit, prep, group, level, draw_ids = NULL,
                                 starts = NULL) {
  slot <- logo_slot(fit, group, prep)
  if (!is.null(slot$reason)) stop(slot$reason, call. = FALSE)
  likelihood <- function() t(rowsum(t(log_lik(prep)), level, reorder = TRUE))
  if (is.null(slot$path)) return(likelihood())
  slots <- slot$slots
  columns <- slots[[1]]$col_of_obs
  if (!all(vapply(slots, function(s) identical(s$col_of_obs, columns),
                  logical(1)))) {
    stop("The grouping's deviations use different level orders.",
         call. = FALSE)
  }
  likelihood <- function() t(rowsum(t(log_lik(prep)), columns,
                                reorder = TRUE))
  sds <- lapply(slots, function(part) {
    sd <- as.vector(as_draws_matrix(fit, variable = part$sd_name))
    if (is.null(draw_ids)) sd else sd[draw_ids]
  })
  integrate <- function(i) {
    if (i > length(slots)) return(likelihood())
    part <- slots[[i]]
    h <- function(z) {
      # Update the prepared predictions shared by both integration levels.
      prep[[part$path[1]]][[part$path[2]]]$re$r[[group]] <<- z
      integrate(i + 1L)
    }
    logo_integrate(h, if (is.null(starts)) part$r else starts[[i]], sds[[i]])
  }
  out <- integrate(1L)
  col_level <- tapply(as.integer(level), columns, unique)
  out[, order(col_level), drop = FALSE]
}

#' The log predictive density of each group with its deviation integrated out
#'
#' @param h A function of an \code{S x L} matrix of deviations, one per draw
#' and level, returning the \code{S x L} matrix of each level's joint
#' log-likelihood under those deviations.
#' @param r0 The \code{S x L} posterior draws of the deviations.
#' @param sd The \code{S} posterior draws of the deviations' standard
#' deviation.
#' @param nodes Number of Gauss--Legendre nodes on each side of the mode.
#' @param newton_steps Number of Newton steps that locate each integrand's mode.
#' @param cutoff How far the log integrand falls before the range ends.
#'
#' @details Separately for every draw and level, Newton steps from \code{r0},
#' a draw from the normalised integrand itself, locate its mode and curvature.
#' A ladder of probes on each side then finds where the log integrand has
#' fallen by \code{cutoff}, and each side is integrated by Gauss--Legendre
#' quadrature over that range. A symmetric rule is not used: where every
#' observation of a group is censored, the integrand has a plateau on one side
#' of its mode and a cliff on the other.
#'
#' A deviation for which \code{h} returns \code{NaN} is given zero density.
#'
#' @return The \code{S x L} matrix of \code{log p(y_g | theta_s)}.
#'
#' @noRd
logo_integrate <- function(h, r0, sd, nodes = 10, newton_steps = 2,
                           cutoff = 14) {
  sd <- matrix(sd, nrow(r0), ncol(r0))
  hp <- function(z) {
    out <- suppressWarnings(h(z)) + stats::dnorm(z, 0, sd, log = TRUE)
    # log_lik() returns NaN where a deviation takes the mean outside the
    # family's support, such as above 1 for an identity-linked Beta. The model
    # gives that deviation zero density.
    out[is.nan(out)] <- -Inf
    out
  }
  spread <- matrix(apply(r0, 2, stats::sd), nrow(r0), ncol(r0), byrow = TRUE)
  mu <- r0
  tau <- pmin(sd, spread)
  tau[!is.finite(tau) | tau <= 0] <- sd[!is.finite(tau) | tau <= 0]
  for (i in seq_len(newton_steps)) {
    # Keep the differences narrow: a wide one can straddle the edge of the
    # censored region and send the step away from the mode.
    d <- tau / 4
    h0 <- hp(mu)
    up <- hp(mu + d)
    down <- hp(mu - d)
    g1 <- (up - down) / (2 * d)
    g2 <- (up - 2 * h0 + down) / d^2
    ok <- is.finite(g1) & is.finite(g2) & g2 < 0
    step <- ifelse(ok, -g1 / g2, 0)
    mu <- mu + pmax(pmin(step, 3 * tau), -3 * tau)
    tau <- ifelse(ok, pmin(1 / sqrt(pmax(-g2, 0)), sd), sd)
  }
  h0 <- hp(mu)
  ladder <- 2^(-1:4)
  gl <- gauss_legendre(nodes)
  out <- NULL
  for (dir in c(-1, 1)) {
    extent <- matrix(NA_real_, nrow(mu), ncol(mu))
    prev_t <- 0 * tau
    prev_drop <- 0 * tau
    for (j in ladder) {
      t <- j * tau
      drop <- h0 - hp(mu + dir * t)
      drop[is.na(drop)] <- Inf
      hit <- is.na(extent) & drop >= cutoff
      frac <- ifelse(is.finite(drop), (cutoff - prev_drop) / (drop - prev_drop),
                     1)
      extent[hit] <- (prev_t + pmin(pmax(frac, 0), 1) * (t - prev_t))[hit]
      prev_t <- t
      prev_drop <- drop
    }
    wide <- is.na(extent)
    extent[wide] <- pmax(2 * max(ladder) * tau, 6 * sd)[wide]
    for (k in seq_along(gl$x)) {
      x <- extent * (gl$x[k] + 1) / 2
      term <- hp(mu + dir * x) + log(gl$w[k] * extent / 2)
      out <- if (is.null(out)) term else log_add_exp(out, term)
    }
  }
  out
}

#' Gauss--Legendre nodes and weights on the interval -1 to 1
#'
#' @param n Number of nodes.
#'
#' @return A \code{\link[base]{list}} of \code{x} and \code{w}.
#'
#' @noRd
gauss_legendre <- function(n) {
  i <- seq_len(n - 1)
  b <- i / sqrt(4 * i^2 - 1)
  jacobi <- matrix(0, n, n)
  jacobi[cbind(i, i + 1)] <- b
  jacobi[cbind(i + 1, i)] <- b
  e <- eigen(jacobi, symmetric = TRUE)
  list(x = rev(e$values), w = rev(2 * e$vectors[1, ]^2))
}

#' @noRd
log_add_exp <- function(a, b) {
  m <- pmax(a, b)
  out <- m + log(exp(a - m) + exp(b - m))
  out[m == -Inf] <- -Inf
  out
}

#' Leave-one-group-out criterion of one fitted equation
#'
#' @param fit A \code{\link[brms]{brmsfit}}.
#' @param group The grouping to leave out, as \code{fit$ranef$group} names it.
#' @param level The level of every observation, from \code{logo_levels()}, in
#' the order the result's columns follow.
#' @param chunk Number of draws evaluated at once, which bounds the memory
#' used.
#'
#' @details Every evaluation goes through \code{brms::log_lik()} on the fit's
#' prepared predictions, with the draws of the left-out grouping's deviations
#' replaced, so censoring, the transform an \code{ogl()} term applies and a
#' \code{disp()} model are all taken from the fit. The other group-level
#' deviations are held at their drawn values.
#'
#' @return A \code{psis_loo} object from \code{\link[loo]{loo}}, with one
#' pointwise row per level and attributes \code{"group"} and \code{"levels"}.
#'
#' @importFrom brms prepare_predictions log_lik as_draws_matrix nchains ndraws rhat
#' @importFrom loo loo relative_eff
#'
#' @noRd
logo_criterion <- function(fit, group, level, chunk = 1000) {
  if (length(logo_slot(fit, group)$slots) > 1L) chunk <- min(chunk, 250L)
  draws <- seq_len(ndraws(fit))
  blocks <- split(draws, ceiling(draws / chunk))
  ll <- do.call(rbind, lapply(blocks, function(ids) {
    prep <- prepare_predictions(fit, draw_ids = ids)
    logo_integrate_slots(fit, prep, group, level, ids)
  }))
  colnames(ll) <- levels(level)
  # Shift each column by its maximum before exponentiating: a level's joint
  # likelihood underflows, and the relative efficiency does not depend on scale.
  lik <- exp(sweep(ll, 2, apply(ll, 2, max)))
  chain <- rep(seq_len(nchains(fit)), each = ndraws(fit) / nchains(fit))
  out <- without_pareto_k_warning(
    loo(ll, r_eff = relative_eff(lik, chain_id = chain))
  )
  attr(out, "group") <- group
  attr(out, "levels") <- levels(level)
  out
}

#' The criteria a model set is weighted by
#'
#' @param object A named \code{\link[base]{list}} of
#' \code{\link{prebayesnecfit}} objects, after \code{\link{expand_nec}}.
#' @param x The predictor values of the data.
#' @param leave_out \code{"group"} or \code{"observation"}, from
#' \code{loo_controls}.
#' @param refit Whether to refit each equation without each level whose
#' Pareto k is at or above 0.7, from \code{loo_controls}.
#'
#' @details A criterion a fit already holds for the same grouping is reused,
#' because the set is weighted again by every function that rebuilds it. Its
#' refitted levels are reused with it.
#'
#' @return A \code{\link[base]{list}} of \code{criteria}, one per equation,
#' \code{logo}, the leave-one-group-out criteria or \code{NULL}, and
#' \code{leave_out}, the grouping left out or \code{"observation"}.
#'
#' @noRd
weight_criteria <- function(object, x, leave_out = "group", refit = FALSE) {
  pointwise <- list(criteria = lapply(object, extract_loo), logo = NULL,
                    kfold = NULL, leave_out = "observation")
  if (identical(leave_out, "kfold")) {
    criterion <- lapply(object, function(o) o$fit$criteria$kfold)
    if (!all(vapply(criterion, inherits, logical(1), "kfold"))) {
      stop("Complete K-fold scores are required for every equation. Run",
           " kfold_groups() and kfold_weights() first.", call. = FALSE)
    }
    groups <- vapply(criterion, function(c) attr(c, "group"), character(1))
    folds <- lapply(criterion, function(c) attr(c, "folds"))
    if (length(unique(groups)) != 1L ||
        !all(vapply(folds, identical, logical(1), folds[[1]]))) {
      stop("K-fold criteria have different groupings or assignments.",
           call. = FALSE)
    }
    return(list(criteria = criterion, logo = NULL, kfold = criterion,
                leave_out = "kfold"))
  }
  if (!identical(leave_out, "group")) {
    return(pointwise)
  }
  by_group <- logo_grouping(object, x)
  if (!is.null(by_group$reason)) {
    warning("The model weights are computed by leaving out each observation,",
            " because ",
            by_group$reason, ". Leaving out each group whole is supported",
            " for one grouping whose levels are each observed at a single",
            " concentration, with up to two independent normal intercepts",
            " per level.",
            if (isTRUE(refit)) " loo_controls$refit = TRUE has no effect.",
            " See ?bnec, argument loo_controls.", call. = FALSE)
  }
  group <- by_group$group
  if (is.null(group)) {
    if (isTRUE(refit) && is.null(by_group$reason)) {
      message("No grouping has its levels each observed at a single",
              " concentration, so the models are scored by leaving out each",
              " observation and loo_controls$refit = TRUE has no effect.")
    }
    return(pointwise)
  }
  has_group <- vapply(object, function(o) group %in% o$fit$ranef$group,
                      logical(1))
  level <- logo_levels(object[[which(has_group)[1]]]$fit, group)
  logo <- lapply(object, function(o) {
    held <- o$fit$criteria$logo
    if (identical(attr(held, "group"), group) &&
          nrow(held$pointwise) == nlevels(level)) {
      return(held)
    }
    logo_criterion(o$fit, group, level)
  })
  if (isTRUE(refit)) {
    logo <- Map(logo_reloo, object, logo, list(level), names(object))
  }
  list(criteria = logo, logo = logo, kfold = NULL, leave_out = group)
}

#' Replace the approximate scores of a criterion by refits
#'
#' @param o A \code{\link{prebayesnecfit}}, which supplies the fit and its
#' initial values.
#' @param logo Its leave-one-group-out criterion.
#' @param level The level of every observation, from \code{logo_levels()}.
#' @param model The name of the equation, for messages.
#'
#' @details Each level whose Pareto k is at or above 0.7 is left out of the
#' data, the equation is refitted under the same priors, and the level is scored
#' from the refit with its deviation integrated out. Its k is then set to 0.
#' The \code{"refitted"} and \code{"refit_failed"} attributes name the levels
#' done, so that a rebuilt set does not refit them again.
#'
#' @return The criterion, with the refitted levels' scores replaced.
#'
#' @noRd
logo_reloo <- function(o, logo, level, model) {
  done <- c(attr(logo, "refitted"), attr(logo, "refit_failed"))
  todo <- setdiff(levels(level)[logo$diagnostics$pareto_k >= 0.7], done)
  if (length(todo) == 0) {
    return(logo)
  }
  group <- attr(logo, "group")
  message("Refitting ", model, " ", length(todo), " time",
          if (length(todo) > 1) "s", ", leaving out each level of ", group,
          " whose Pareto k is at or above 0.7.")
  elpd <- stats::setNames(rep(NA_real_, length(todo)), todo)
  rhat_max <- elpd
  base <- o$fit
  first_error <- NULL
  for (g in todo) {
    out <- tryCatch(logo_refit_level(base, o, group, level, g),
                    error = function(e) {
                      if (is.null(first_error)) {
                        first_error <<- trimws(conditionMessage(e))
                      }
                      NULL
                    })
    if (is.null(out)) {
      next
    }
    elpd[g] <- out$elpd
    rhat_max[g] <- out$rhat
    # Refit from the last refit: under cmdstanr it holds a compiled program
    # even where the stored fit's has been deleted.
    base <- out$fit
  }
  ok <- is.finite(elpd)
  i <- match(todo[ok], levels(level))
  lpd <- logo$pointwise[i, "elpd_loo"] + logo$pointwise[i, "p_loo"]
  logo$pointwise[i, "elpd_loo"] <- elpd[ok]
  logo$pointwise[i, "p_loo"] <- lpd - elpd[ok]
  logo$pointwise[i, "looic"] <- -2 * elpd[ok]
  logo$pointwise[i, "mcse_elpd_loo"] <- 0
  logo$pointwise[i, "influence_pareto_k"] <- 0
  logo$diagnostics$pareto_k[i] <- 0
  sel <- c("elpd_loo", "p_loo", "looic")
  logo$estimates[sel, "Estimate"] <- colSums(logo$pointwise[, sel])
  logo$estimates[sel, "SE"] <- sqrt(nrow(logo$pointwise) *
                                      apply(logo$pointwise[, sel], 2, stats::var))
  attr(logo, "refitted") <- c(attr(logo, "refitted"), todo[ok])
  attr(logo, "refit_failed") <- c(attr(logo, "refit_failed"), todo[!ok])
  if (any(!ok)) {
    warning("Refitting ", model, " failed without ", sum(!ok), " level",
            if (sum(!ok) > 1) "s", " of ", group, " (",
            paste0(todo[!ok], collapse = ", "), "), so ",
            if (sum(!ok) > 1) "their scores remain" else "its score remains",
            " approximate.",
            if (!is.null(first_error)) {
              paste0(" The first error was: ", first_error)
            }, call. = FALSE)
  }
  slow <- todo[ok][rhat_max[ok] > 1.01]
  if (length(slow) > 0) {
    warning("Refits of ", model, " without ", length(slow), " level",
            if (length(slow) > 1) "s", " of ", group,
            " have an Rhat above 1.01 (", paste0(slow, collapse = ", "),
            "), so the chains of those refits may not have converged.",
            call. = FALSE)
  }
  logo
}

#' Refit an equation without one level and score that level
#'
#' @param base The \code{\link[brms]{brmsfit}} to update, either the
#' equation's fit or an earlier refit of it.
#' @param o The equation's \code{\link{prebayesnecfit}}.
#' @param group The grouping left out.
#' @param level The level of every observation, from \code{logo_levels()}.
#' @param g The level to leave out.
#'
#' @details The refit takes the priors, sampler settings and initial values of
#' the fit. The initial deviations are reset to zero at the refit's number of
#' levels, because brms would otherwise reuse the fit's, one of which no longer
#' exists. The left-out level is a new level of the refit, and its deviation is
#' integrated out as in \code{logo_criterion()}, starting from the fit's draws
#' of it.
#'
#' @return A \code{\link[base]{list}} of \code{elpd}, the level's log
#' predictive density, \code{rhat}, the refit's largest Rhat, and \code{fit},
#' the refit. A refit that fails, or returns no draws, is an error, which
#' \code{logo_reloo()} reports.
#'
#' @importFrom stats update
#'
#' @noRd
logo_refit_level <- function(base, o, group, level, g) {
  f <- o$fit
  held <- level == g
  train <- droplevels(f$data[!held, , drop = FALSE])
  test <- f$data[held, , drop = FALSE]
  args <- list(base, newdata = train, init = refit_inits(o$init, f, train),
               refresh = 0, silent = 2)
  if (!is.null(o$call_args$seed)) {
    args$seed <- o$call_args$seed
  }
  refit <- suppressWarnings(suppressMessages(do.call(update, args)))
  if (ndraws(refit) == 0) {
    stop("the refit returned no draws.", call. = FALSE)
  }
  prep <- prepare_predictions(refit, newdata = test, allow_new_levels = TRUE,
                              sample_new_levels = "gaussian")
  refit_slot <- logo_slot(refit, group, prep)
  starts <- NULL
  if (!is.null(refit_slot$path)) {
    full_slot <- logo_slot(f, group, prepare_predictions(f, newdata = test))
    starts <- Map(function(full, current) {
      matrix(rep_len(as.vector(full$r), nrow(current$r)),
             nrow = nrow(current$r), ncol = ncol(current$r))
    }, full_slot$slots, refit_slot$slots)
  }
  ll <- logo_integrate_slots(refit, prep, group,
                              factor(rep(g, nrow(test))),
                              starts = starts)[, 1]
  m <- max(ll)
  list(elpd = m + log(mean(exp(ll - m))),
       rhat = max(rhat(refit), na.rm = TRUE), fit = refit)
}

#' Initial values for a refit with fewer levels
#'
#' @param init The fit's initial values: a list with one list per chain, or a
#' keyword.
#' @param fit The \code{\link[brms]{brmsfit}}.
#' @param train The data of the refit.
#'
#' @return \code{init} with each \code{z_k} replaced by zeros at the number of
#' levels in \code{train}, or \code{"random"} where \code{init} is not a list.
#'
#' @noRd
refit_inits <- function(init, fit, train) {
  if (!is.list(init)) {
    return("random")
  }
  re <- fit$ranef
  dims <- lapply(split(re, re$id), function(r) {
    vars <- strsplit(r$group[1], ":", fixed = TRUE)[[1]]
    c(nrow(r), nlevels(interaction(train[vars], drop = TRUE)))
  })
  lapply(init, function(chain) {
    for (k in names(dims)) {
      z <- paste0("z_", k)
      if (z %in% names(chain)) {
        chain[[z]] <- matrix(0, dims[[k]][1], dims[[k]][2])
      }
    }
    chain
  })
}

#' Suppress an intermediate Pareto k warning
#'
#' @param expr An expression that computes a LOO criterion.
#'
#' @return The value of \code{expr}.
#' @noRd
without_pareto_k_warning <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("pareto[_ -]?k", conditionMessage(w), ignore.case = TRUE)) {
      invokeRestart("muffleWarning")
    }
  })
}

#' Report high Pareto k in the criteria used for model weights
#'
#' @param criteria The selected LOO criteria, one per equation.
#' @param wi The set's model weights.
#' @param leave_out The grouping left out, or \code{"observation"}.
#' @param refit Whether the set was refitted where k is at or above 0.7, in which
#' case the levels still above it are those whose refits failed.
#' @param report_k Whether to report the diagnostic.
#'
#' @details Only equations given at least 0.01 of the weight are named: an
#' unreliable score for an equation given no weight does not change the
#' average. With fewer posterior draws, loo may flag values below 0.7 at its
#' draw-dependent reliability threshold; report those separately.
#'
#' @return \code{NULL}, invisibly. Called for the message.
#' @noRd
report_weight_k <- function(criteria, wi, leave_out, refit = FALSE,
                            report_k = TRUE) {
  if (!report_k) {
    return(invisible(NULL))
  }
  high <- vapply(criteria, function(l) sum(l$diagnostics$pareto_k >= 0.7,
                                           na.rm = TRUE),
                 numeric(1))
  named <- names(criteria)[high > 0 & wi >= 0.01]
  n_units <- nrow(criteria[[1]]$pointwise)
  groupwise <- !identical(leave_out, "observation")
  # message() adds no newline to a condition object, so add it here or the
  # next message continues this line.
  emit <- function(msg) {
    condition <- simpleMessage(paste0(msg, "\n"))
    class(condition) <- c("bayesnec_pareto_k_message", class(condition))
    message(condition)
  }
  if (length(named) > 0) {
    named <- named[order(wi[match(named, names(criteria))], decreasing = TRUE)]
    msg <- paste0("The leave-one-", if (groupwise) "group" else "observation",
            "-out score has Pareto k at or above 0.7 for ",
            paste0(high[named], " of ", n_units,
                   if (groupwise) paste0(" levels of ", leave_out) else
                     " observations",
                   " in ", named, " (weight ",
                   signif(wi[match(named, names(criteria))], 2), ")",
                   collapse = "; "),
            ". The affected scores are approximate",
            if (refit || !groupwise) {
              "."
            } else {
              paste0("; loo_controls = list(refit = TRUE) replaces them by",
                     " refitting the equation without each level.")
            })
    emit(msg)
  }
  draws <- vapply(criteria, function(l) {
    s <- attr(l, "dims", exact = TRUE)[1]
    if (length(s) == 0 || !is.finite(s)) NA_real_ else s
  }, numeric(1))
  threshold <- ifelse(draws > 10, pmin(1 - 1 / log10(draws), 0.7), 0.7)
  lower <- vapply(seq_along(criteria), function(i) {
    k <- criteria[[i]]$diagnostics$pareto_k
    sum(k >= threshold[i] & k < 0.7, na.rm = TRUE)
  }, numeric(1))
  named <- names(criteria)[lower > 0 & wi >= 0.01]
  if (length(named) > 0) {
    named <- named[order(wi[match(named, names(criteria))], decreasing = TRUE)]
    i <- match(named, names(criteria))
    emit(paste0(
      "The selected LOO score has Pareto k below 0.7 but at or above loo's ",
      "draw-dependent reliability threshold for ",
      paste0(lower[i], " of ", n_units, " ",
             if (groupwise) paste0("levels of ", leave_out) else "observations",
             " in ", named, " (weight ", signif(wi[i], 2),
             "; threshold ", signif(threshold[i], 2), ")",
             collapse = "; "),
      ". Increase the number of posterior draws and reassess."
    ))
  }
  invisible(NULL)
}

#' What the criterion behind a set's weights left out
#'
#' @param x A \code{\link{bayesmanecfit}}.
#'
#' @return A grouping name, or \code{"observation"}, which is also returned
#' for a set weighted before the record existed.
#'
#' @noRd
weights_leave_out <- function(x) {
  out <- attr(x$mod_stats$wi, "leave_out")
  if (is.null(out)) "observation" else out
}

#' @noRd
leave_out_label <- function(leave_out) {
  if (is.null(leave_out) || identical(leave_out, "observation")) {
    "; each observation left out"
  } else if (identical(leave_out, "kfold")) {
    "; exact K-fold over grouping levels"
  } else {
    paste0("; each level of ", leave_out, " left out")
  }
}
