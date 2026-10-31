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
#' in \code{prep}. \code{reason} is \code{NULL} unless the deviations cannot be
#' integrated out one level at a time by \code{logo_integrate()}.
#'
#' @noRd
logo_slot <- function(fit, group, prep = NULL) {
  re <- fit$ranef[fit$ranef$group == group, , drop = FALSE]
  if (nrow(re) == 0) {
    return(list(path = NULL, reason = NULL))
  }
  if (nrow(re) > 1) {
    return(list(path = NULL, reason = paste0(
      "the grouping ", group, " has a deviation on more than one parameter"
    )))
  }
  if (!identical(re$coef, "Intercept") || !identical(re$dist, "gaussian")) {
    return(list(path = NULL, reason = paste0(
      "the grouping ", group, " has a term other than a normal intercept"
    )))
  }
  path <- if (nzchar(re$nlpar)) c("nlpars", re$nlpar) else c("dpars", re$dpar)
  out <- list(path = path, reason = NULL)
  if (!is.null(prep)) {
    out$r <- prep[[path[1]]][[path[2]]]$re$r[[group]]
    # brms names the parameter, not the deviation, in the standard deviation.
    par <- if (nzchar(re$nlpar)) re$nlpar else re$dpar
    out$sd_name <- paste0("sd_", group, "__",
                          if (par == "mu") "" else paste0(par, "_"),
                          "Intercept")
    z <- which(as.matrix(prep[[path[1]]][[path[2]]]$re$Z[[group]]) != 0,
               arr.ind = TRUE)
    out$col_of_obs <- z[order(z[, "row"]), "col"]
  }
  out
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
#' @return The \code{S x L} matrix of \code{log p(y_g | theta_s)}.
#'
#' @noRd
logo_integrate <- function(h, r0, sd, nodes = 10, newton_steps = 2,
                           cutoff = 14) {
  sd <- matrix(sd, nrow(r0), ncol(r0))
  hp <- function(z) h(z) + stats::dnorm(z, 0, sd, log = TRUE)
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

#' Gauss--Legendre nodes and weights on [-1, 1]
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
#' @importFrom brms prepare_predictions log_lik as_draws_matrix nchains ndraws
#' rhat
#' @importFrom loo loo relative_eff
#'
#' @noRd
logo_criterion <- function(fit, group, level, chunk = 1000) {
  draws <- seq_len(ndraws(fit))
  blocks <- split(draws, ceiling(draws / chunk))
  ll <- do.call(rbind, lapply(blocks, function(ids) {
    prep <- prepare_predictions(fit, draw_ids = ids)
    slot <- logo_slot(fit, group, prep)
    if (is.null(slot$path)) {
      # No deviation to integrate: a level's joint log-likelihood is the sum
      # over its observations.
      return(t(rowsum(t(log_lik(prep)), level, reorder = TRUE)))
    }
    sd <- as.vector(as_draws_matrix(fit, variable = slot$sd_name))[ids]
    col_level <- tapply(as.integer(level), slot$col_of_obs, unique)
    h <- function(z) {
      prep[[slot$path[1]]][[slot$path[2]]]$re$r[[group]] <- z
      t(rowsum(t(log_lik(prep)), slot$col_of_obs, reorder = TRUE))
    }
    out <- logo_integrate(h, slot$r, sd)
    out[, order(col_level), drop = FALSE]
  }))
  colnames(ll) <- levels(level)
  # Shift each column by its maximum before exponentiating: a level's joint
  # likelihood underflows, and the relative efficiency does not depend on scale.
  lik <- exp(sweep(ll, 2, apply(ll, 2, max)))
  chain <- rep(seq_len(nchains(fit)), each = ndraws(fit) / nchains(fit))
  out <- suppressWarnings(loo(ll, r_eff = relative_eff(lik, chain_id = chain)))
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
#' Pareto k exceeds 0.7, from \code{loo_controls}.
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
                    leave_out = "observation")
  if (!identical(leave_out, "group")) {
    return(pointwise)
  }
  by_group <- logo_grouping(object, x)
  if (!is.null(by_group$reason)) {
    warning("The model weights are computed by leaving out each observation,",
            " because ",
            by_group$reason, ". Leaving out each group whole is supported",
            " for one grouping whose levels are each observed at a single",
            " concentration, with one normal intercept per level. See ?bnec,",
            " argument loo_controls.", call. = FALSE)
  }
  group <- by_group$group
  if (is.null(group)) {
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
  list(criteria = logo, logo = logo, leave_out = group)
}

#' Replace the approximate scores of a criterion by refits
#'
#' @param o A \code{\link{prebayesnecfit}}, which supplies the fit and its
#' initial values.
#' @param logo Its leave-one-group-out criterion.
#' @param level The level of every observation, from \code{logo_levels()}.
#' @param model The name of the equation, for messages.
#'
#' @details Each level whose Pareto k exceeds 0.7 is left out of the data, the
#' equation is refitted under the same priors, and the level is scored from the
#' refit with its deviation integrated out. Its k is then set to 0. Attributes
#' \code{"refitted"} and \code{"refit_failed"} name the levels done, so that a
#' rebuilt set does not refit them again.
#'
#' @return The criterion, with the refitted levels' scores replaced.
#'
#' @noRd
logo_reloo <- function(o, logo, level, model) {
  done <- c(attr(logo, "refitted"), attr(logo, "refit_failed"))
  todo <- setdiff(levels(level)[logo$diagnostics$pareto_k > 0.7], done)
  if (length(todo) == 0) {
    return(logo)
  }
  group <- attr(logo, "group")
  message("Refitting ", model, " ", length(todo), " time",
          if (length(todo) > 1) "s", ", leaving out each level of ", group,
          " whose Pareto k exceeds 0.7.")
  elpd <- stats::setNames(rep(NA_real_, length(todo)), todo)
  rhat_max <- elpd
  base <- o$fit
  for (g in todo) {
    out <- tryCatch(logo_refit_level(base, o, group, level, g),
                    error = function(e) NULL)
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
            " approximate.", call. = FALSE)
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
#' @return \code{NULL} where the refit failed, and otherwise a
#' \code{\link[base]{list}} of \code{elpd}, the level's log predictive
#' density, \code{rhat}, the refit's largest Rhat, and \code{fit}, the
#' refit.
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
  refit <- try(suppressWarnings(suppressMessages(do.call(update, args))),
               silent = TRUE)
  if (inherits(refit, "try-error") || ndraws(refit) == 0) {
    return(NULL)
  }
  prep <- prepare_predictions(refit, newdata = test, allow_new_levels = TRUE,
                              sample_new_levels = "gaussian")
  slot <- logo_slot(refit, group, prep)
  if (is.null(slot$path)) {
    ll <- rowSums(log_lik(prep))
  } else {
    sd <- as.vector(as_draws_matrix(refit, variable = slot$sd_name))
    r_full <- logo_slot(f, group, prepare_predictions(f, newdata = test))$r
    r0 <- matrix(rep_len(as.vector(r_full), nrow(slot$r)), ncol = 1)
    h <- function(z) {
      prep[[slot$path[1]]][[slot$path[2]]]$re$r[[group]] <- z
      matrix(rowSums(log_lik(prep)), ncol = 1)
    }
    ll <- logo_integrate(h, r0, sd)[, 1]
  }
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

#' Report the levels whose left-out score is unreliable
#'
#' @param logo The leave-one-group-out criteria of a set, or \code{NULL}.
#' @param wi The set's model weights.
#' @param refit Whether the set was refitted where k exceeds 0.7, in which
#' case the levels still above it are those whose refits failed.
#'
#' @details Only equations given at least 0.01 of the weight are named: an
#' unreliable score for an equation given no weight does not change the
#' average.
#'
#' @return \code{NULL}, invisibly. Called for the message.
#' @noRd
report_logo_k <- function(logo, wi, refit = FALSE) {
  if (is.null(logo)) {
    return(invisible(NULL))
  }
  high <- vapply(logo, function(l) sum(l$diagnostics$pareto_k > 0.7),
                 numeric(1))
  named <- names(logo)[high > 0 & wi >= 0.01]
  if (length(named) == 0) {
    return(invisible(NULL))
  }
  named <- named[order(wi[match(named, names(logo))], decreasing = TRUE)]
  n_levels <- nrow(logo[[1]]$pointwise)
  message("The leave-one-group-out score has Pareto k above 0.7 for ",
          paste0(high[named], " of ", n_levels, " levels of ",
                 attr(logo[[1]], "group"), " in ", named, " (weight ",
                 signif(wi[match(named, names(logo))], 2), ")",
                 collapse = "; "),
          ". The scores of those levels are approximate",
          if (refit) {
            "."
          } else {
            paste0("; loo_controls = list(refit = TRUE) replaces them by",
                   " refitting the equation without each level.")
          })
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
  } else {
    paste0("; each level of ", leave_out, " left out")
  }
}
