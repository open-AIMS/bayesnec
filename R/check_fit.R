#' Check a fit against the data it was fitted to
#'
#' Reports, per group of the predictor, the observed location and scale of the
#' response against what the fitted model simulates, with a posterior predictive
#' p-value for each. Sits alongside \code{\link{check_chains}}, which checks the
#' sampler, and \code{\link{check_priors}}, which checks the priors: this checks
#' the fit against the data.
#'
#' @name check_fit
#' @order 1
#'
#' @param x An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} as returned by \code{\link{bnec}}, or a
#' \code{\link{bayesnecgroupfit}} returned by \code{\link{bnec_group}}.
#' @param group A numeric, character or factor vector of the same length as the
#' data giving the group each row belongs to, or a single numeric value giving
#' the number of bins to cut the predictor
#' into. If \code{NULL} (the default) the distinct values of the predictor are
#' used where they are replicated, and the predictor is binned otherwise, with a
#' warning.
#' For a \code{bayesnecgroupfit}, an explicit vector has one value per row of
#' the original data passed to \code{bnec_group()}, in its original order. It
#' is split by fitted level before the checks. The fitted-level factor is not
#' the predictor grouping used within each level.
#' For a \code{bayesnechurdlefit}, an explicit vector has one value per exposed
#' row; the growth check uses the values for surviving rows.
#' @param ndraws A \code{\link[base]{numeric}} vector of length 1, the number of
#' posterior draws to simulate from. Reduced silently to the number the fit
#' actually holds where that is smaller.
#' @param seed A \code{\link[base]{numeric}} vector of length 1. Passed to
#' \code{\link[base]{set.seed}} before simulating, so the result is
#' reproducible. The caller's random number stream is restored before the
#' check returns, so running it does not change what the next random operation
#' in the session returns. \code{NULL} is refused rather than passed on,
#' because \code{set.seed(NULL)} re-initialises the stream from the clock.
#' @param ... Unused.
#'
#' @details \bold{Why this is not \code{\link{dispersion}}}
#'
#' \code{\link{dispersion}} reports one global statistic, and for any family
#' with a free dispersion parameter that parameter absorbs exactly the
#' discrepancy the global statistic measures. On the packaged
#' \code{manec_example} the global Pearson ratio is 1.011, with a 95% interval
#' of 0.71 to 1.44 --- a
#' clean bill of health --- while the same fit simulates about 27% more
#' variability than the data show in the control region. A global summary is
#' structurally incapable of showing that, which is why this check is
#' \emph{local}.
#'
#' \bold{Why the control row matters}
#'
#' \code{\link{nsec}} sets its reference from the \code{sig_val} quantile of the
#' posterior of the control mean, so the width of that posterior sets the
#' reference. Overstate control variability and the reference falls, the curve
#' crosses it later, and \emph{NSEC} moves right --- a less protective number.
#' Understate it and \emph{NSEC} moves left. A curve pulled away from the
#' control data biases the same reference in location rather than spread. The
#' control row is flagged for that reason.
#'
#' \bold{Residuals, not raw values}
#'
#' The scale statistic is computed on residuals. Within a group the raw standard
#' deviation of the response mixes residual variability with the slope of the
#' curve across the group, which would make every steep-region group look
#' overdispersed.
#'
#' \bold{Model averaging does not protect you here}
#'
#' Model weights come from a global \code{elpd}, whether they are the
#' pseudo-BMA weights \code{\link{bnec}} computes by default or stacking
#' weights requested through \code{loo_controls}. The control is a handful of
#' rows out of many, so a candidate model can hold high weight while fitting the
#' control badly --- it wins on the bulk of the curve and pays almost nothing
#' for the control. For a \code{\link{bayesmanecfit}} the per-model rows are
#' therefore reported alongside the averaged rows, or the table could not say
#' which model is doing the damage.
#'
#' Posterior predictive p-values give half weight to simulated statistics that
#' tie the observed statistic. Ties are common for count families because their
#' statistics take few distinct values.
#' When both an observed statistic and the median simulated statistic are zero,
#' their ratio is reported as 1 to denote agreement in the control check.
#'
#' \bold{Censored readings}
#'
#' Where the formula includes a \code{cens()} term, a censored reading is
#' recorded at its bound, whereas the responses simulated from the fit are not
#' censored. In each group that includes a censored reading, the observed and
#' the simulated responses are therefore both held at one bound before the
#' statistics are computed: values below the largest left-censoring bound in
#' the group are raised to it, and values above the smallest right-censoring
#' bound are lowered to it. An interval-censored reading is treated as
#' left-censored at the upper end of its interval. A group whose lower holding
#' bound exceeds its upper holding bound cannot be compared this way and is
#' refused. The true value of every censored reading is known to be beyond the
#' bound, so its held value is known exactly, and the observed and simulated
#' statistics are computed from the same quantity. Where the observed statistic
#' equals the median simulated one, as when every value is held, its ratio is 1. A
#' structural zero of a hurdle or zero-inflated family stays at zero, unless a
#' left-censored reading in the group could itself be a zero; the zeros are
#' then held with the other values and the group's zero proportions are
#' \code{NA}.
#'
#' Holding the values discards what the data show beyond the bound. The column
#' \code{n_cens} gives the number of censored readings in each group, and where
#' it approaches \code{n} the statistics of that group show little more than
#' whether the fitted model predicts readings beyond the bound.
#' \code{\link{pp_check}} instead leaves censored readings out, which in a
#' partly censored group compares the uncensored readings, all on the measured
#' side of the bound, with simulations that fall on either side.
#'
#' @return A \code{\link[base]{data.frame}} with one row per group (per model,
#' for a \code{\link{bayesmanecfit}}), carrying the group, the number of
#' observations, the observed and simulated mean and standard deviation, their
#' ratios, a posterior predictive p-value for each, and a logical flag marking
#' the control group. For the mixture families it also carries the observed and
#' simulated proportion of zeros. A group with one observation has no standard
#' deviation; its standard deviation statistics are \code{NA}, while its mean
#' statistics remain available. Where the formula includes a \code{cens()}
#' term, the column \code{n_cens} gives the number of censored readings in each
#' group.
#' A \code{bayesnecgroupfit} returns a \code{checkfit_group} object containing
#' the original \code{checkfit} table for each fitted level in \code{$fits}.
#' Each table retains its attributes, including any stored Pareto diagnostics.
#' \code{print()} labels and prints each level; \code{plot()} returns a named
#' list of plots with one plot per fitted level.
#'
#' The separate \code{"pareto"} attribute is a
#' data frame with one row per equation. It names the stored criterion used for
#' model weights, its Pareto cutoff, the count and IDs of held-out units at or
#' above that cutoff, the largest k, and any successful or failed group refits.
#' \code{flagged_ids}, \code{refitted_ids}, and \code{refit_failed_ids} are list
#' columns of character vectors. A missing criterion has \code{status =
#' "unavailable"} and missing counts, not a clean result. Single-equation fits
#' have no model-weight comparison. For hurdle fits the growth and survival
#' tables have their own attributes; the combined predictive table has no
#' leave-one-out criterion.
#'
#' Pareto k assesses the approximation to a leave-one-out score. It does not
#' assess sampler convergence or the posterior predictive discrepancies in the
#' concentration-level table. Grouped scores use the package's 0.7 cutoff;
#' observation-wise scores use the applicable \pkg{loo} cutoff.
#'
#' @seealso \code{\link{pp_check}}, \code{\link{pull_best}},
#' \code{\link{dispersion}},
#' \code{\link{check_chains}}, \code{\link{check_priors}}
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' check_fit(nec4param)
#' data(nec_data)
#' nec_data$site <- rep(c("a", "b"), length.out = nrow(nec_data))
#' grouped <- bnec_group(y ~ crf(x, "nec3param"), data = nec_data,
#'                       group_var = "site")
#' checks <- check_fit(grouped)
#' plot(checks)
#' }
#'
#' @export
check_fit <- function(x, group = NULL, ndraws = 1000, seed = 10, ...) {
  UseMethod("check_fit")
}

#' Read Pareto diagnostics from the criterion used for weights
#' @param fit A \code{bayesnecfit}.
#' @param criterion \code{"logo"} or \code{"loo"}.
#' @param weight The model weight, or \code{NA} for a single equation.
#' @return A one-row data frame.
#' @noRd
check_fit_pareto_row <- function(fit, criterion, weight = NA_real_) {
  z <- fit$fit$criteria[[criterion]]
  k <- if (is.null(z)) NULL else z$diagnostics$pareto_k
  available <- !is.null(k) && length(k) > 0 && !anyNA(k)
  cutoff <- if (!available) NA_real_ else if (criterion == "logo") 0.7 else
    attr(loo::pareto_k_table(z), "k_threshold")
  ids <- if (is.null(z)) NULL else if (criterion == "logo")
    attr(z, "levels") else rownames(z$pointwise)
  if (available && (is.null(ids) || length(ids) != length(k))) {
    ids <- as.character(seq_along(k))
  }
  flagged <- if (available) ids[k >= cutoff] else character()
  row <- data.frame(model = fit$model, criterion = criterion,
                    weight = weight, cutoff = cutoff,
                    n_high_k = if (available) sum(k >= cutoff) else NA_integer_,
                    max_k = if (available) max(k) else NA_real_,
                    status = if (available) "available" else "unavailable")
  row$flagged_ids <- I(list(flagged))
  row$refitted_ids <- I(list(if (is.null(z)) character() else
                               as.character(attr(z, "refitted"))))
  row$refit_failed_ids <- I(list(if (is.null(z)) character() else
                                   as.character(attr(z, "refit_failed"))))
  row
}

#' The stored criterion selected when a model set was weighted
#' @param x A \code{bayesmanecfit}.
#' @return \code{"logo"} or \code{"loo"}.
#' @noRd
check_fit_weight_criterion <- function(x) {
  leave_out <- attr(x$mod_stats$wi, "leave_out")
  if (!is.null(leave_out)) {
    return(if (identical(leave_out, "observation")) "loo" else "logo")
  }
  # Older sets did not record leave_out. Only choose logo if every equation
  # holds one; otherwise the stored observation-wise criterion is unambiguous.
  if (all(vapply(x$mod_fits, function(f) !is.null(f$fit$criteria$logo),
                 logical(1)))) "logo" else "loo"
}

#' Assign each observation to a group of the predictor
#'
#' Groups by the replicated predictor values where every value is replicated,
#' since those are the design points. Otherwise cuts the predictor into bins,
#' with a warning, because a bin is not a design point.
#'
#' @param predictor A \code{\link[base]{numeric}} vector.
#' @param group The user's \code{group} argument.
#'
#' @return A \code{\link[base]{factor}}.
#'
#' @importFrom stats quantile
#'
#' @noRd
check_fit_groups <- function(predictor, group = NULL) {
  if (!is.null(group) && length(group) == length(predictor)) {
    return(factor(group))
  }
  n_bins <- if (!is.null(group) && length(group) == 1) group else NULL
  reps <- table(predictor)
  if (is.null(n_bins) && length(reps) > 0 && min(reps) > 1) {
    return(factor(predictor))
  }
  if (is.null(n_bins)) {
    # Aim for roughly 10 per bin, bounded so a small dataset does not end up
    # with one bin and a large one with hundreds.
    n_bins <- max(3, min(10, floor(length(predictor) / 10)))
    warning("The predictor has unreplicated values, so it has been cut into ",
            n_bins, " bins to give check_fit something to group by. A bin is",
            " not a design point: pass `group` explicitly if the binning is",
            " not what you want, and treat any single row as indicative.",
            call. = FALSE)
  }
  breaks <- unique(quantile(predictor, probs = seq(0, 1, length.out = n_bins + 1)))
  factor(cut(predictor, breaks = breaks, include.lowest = TRUE))
}

#' Groups containing the stated control concentration
#'
#' @param predictor The observed predictor values.
#' @param grp Their diagnostic groups.
#' @param x_control A recorded explicit control, or \code{NULL}.
#' @return Character group labels. Empty when an explicit control was absent.
#' @noRd
check_fit_control_groups <- function(predictor, grp, x_control = NULL) {
  if (is.null(x_control)) {
    return(levels(grp)[which.min(tapply(predictor, grp, min))])
  }
  unique(as.character(grp[predictor == x_control]))
}

#' Posterior predictive p-value
#'
#' The proportion above the observed statistic plus half the proportion tied
#' with it. Values near 0 or 1 both indicate misfit, in opposite directions;
#' 0.5 is perfect agreement.
#'
#' @param sim A \code{\link[base]{numeric}} vector of simulated statistics.
#' @param obs A \code{\link[base]{numeric}} vector of length 1.
#' @return A \code{\link[base]{numeric}} vector of length 1.
#'
#' @noRd
ppp_value <- function(sim, obs) {
  sim <- sim[is.finite(sim)]
  if (!length(sim) || !is.finite(obs)) {
    return(NA_real_)
  }
  tied <- is_tie(sim, obs)
  mean(sim > obs & !tied) + mean(tied) / 2
}

#' Whether two statistics are equal up to rounding
#'
#' @param a,b \code{\link[base]{numeric}} vectors.
#'
#' @details Values held at one bound give equal statistics on both sides, but
#' \code{mean()} and \code{rowMeans()} round differently where the platform's
#' \code{long double} is a \code{double}, as on macOS arm64. Exact equality
#' then misses the tie.
#'
#' @return A \code{\link[base]{logical}} vector.
#'
#' @noRd
is_tie <- function(a, b) {
  abs(a - b) <= 64 * .Machine$double.eps * pmax(abs(a), abs(b))
}

#' The censoring of each fitted row, as the model treats it
#'
#' @param fit An object of class \code{\link[brms]{brmsfit}}.
#'
#' @details The codes are read from the data \pkg{brms} fitted, not from the
#' \code{cens()} column. A joint refit fits a left-censored survivor as an
#' interval from zero, and a literal indicator such as \code{cens("left")} has
#' no column.
#'
#' @return \code{NULL} where the model has no \code{cens()} term. Otherwise a
#' \code{\link[base]{list}} with one element per row in each of \code{code},
#' the \pkg{brms} code (-1 left, 0 none, 1 right, 2 interval); \code{bound},
#' the recorded value, or the upper end of an interval; and
#' \code{may_be_zero}, whether a censored reading could be a structural zero
#' of a hurdle or zero-inflated family, which \pkg{brms} left censoring
#' includes.
#'
#' @importFrom brms standata
#'
#' @noRd
check_fit_cens <- function(fit) {
  if (!"cens" %in% all.names(fit$formula$formula[[2]])) {
    return(NULL)
  }
  sdata <- standata(fit)
  if (is.null(sdata$cens)) {
    return(NULL)
  }
  code <- as.integer(sdata$cens)
  bound <- as.vector(sdata$Y)
  interval <- code == 2L
  bound[interval] <- as.vector(sdata$rcens)[interval]
  list(code = code, bound = bound, may_be_zero = code == -1L)
}

#' Hold observed or simulated responses at a group's censoring bound
#'
#' @param v A \code{\link[base]{numeric}} vector or matrix.
#' @param lower,upper The bounds; values beyond them are set to them.
#' @param keep_zero A \code{\link[base]{logical}}. Whether an exact zero stays
#' at zero.
#'
#' @return \code{v}, with its dimensions.
#'
#' @noRd
hold_at_bound <- function(v, lower, upper, keep_zero) {
  out <- pmin(pmax(v, lower), upper)
  if (keep_zero) {
    out[v == 0] <- 0
  }
  out
}

#' The per-group table for one brmsfit
#'
#' @param fit An object of class \code{\link[brms]{brmsfit}}.
#' @param formula The bayesnec formula used for the fit.
#' @param y A \code{\link[base]{numeric}} vector, the response.
#' @param grp A \code{\link[base]{factor}}.
#' @param ndraws,seed As in \code{\link{check_fit}}.
#' @param is_mixture A \code{\link[base]{logical}}.
#'
#' @return A \code{\link[base]{data.frame}}.
#'
#' @importFrom brms posterior_predict posterior_epred ndraws
#' @importFrom stats sd
#'
#' @noRd
check_fit_table <- function(fit, formula, y, grp, ndraws, seed, is_mixture) {
  # brms errors when ndraws exceeds the draws a fit holds, so cap it.
  available <- brms::ndraws(fit)
  if (ndraws > available) {
    ndraws <- available
  }
  # Leave the caller's random number stream where it was: a diagnostic must
  # not change it.
  with_preserved_rng_state({
    set.seed(seed)
    yrep <- posterior_predict(fit, ndraws = ndraws)
    # Take the residuals of both the observed and the simulated response
    # against one fitted mean. Each draw's own mean would remove the
    # discrepancy being measured.
    mu <- apply(factorised_count_epred(fit, formula, ndraws = ndraws),
                2, median)
  })
  check_fit_stats(y, yrep, mu, grp, is_mixture, cens = check_fit_cens(fit))
}

#' The per-group statistics, given a response and a simulated response
#'
#' Split out from \code{check_fit_table()} so that the combined hurdle check
#' can pass a \code{yrep} and \code{mu} it built itself.
#'
#' @param y The observed response.
#' @param yrep A draws-by-observations matrix of simulated responses.
#' @param mu The fitted mean per observation.
#' @param grp A \code{\link[base]{factor}} grouping the observations.
#' @param is_mixture A \code{\link[base]{logical}}.
#' @param cens \code{NULL}, or the censoring of each row in the form
#' \code{check_fit_cens()} returns. Where supplied, the table gains an
#' \code{n_cens} column, and a group with a censored reading is compared
#' with both sides held at its bound (see \code{\link{check_fit}}).
#'
#' @return A \code{\link[base]{data.frame}}, one row per group.
#'
#' @noRd
check_fit_stats <- function(y, yrep, mu, grp, is_mixture, cens = NULL) {
  levs <- levels(grp)
  out <- lapply(levs, function(lv) {
    idx <- which(grp == lv)
    obs_y <- y[idx]
    sim_y <- yrep[, idx, drop = FALSE]
    n_cens <- if (is.null(cens)) 0L else sum(cens$code[idx] != 0L)
    held <- n_cens > 0L
    zero_known <- TRUE
    if (held) {
      code <- cens$code[idx]
      bound <- cens$bound[idx]
      lower <- max(-Inf, bound[code %in% c(-1L, 2L)])
      upper <- min(Inf, bound[code == 1L])
      if (lower > upper) {
        stop("Group ", lv, " has a left or interval censoring bound above a",
             " right censoring bound, so check_fit() cannot hold its",
             " responses at a common bound.", call. = FALSE)
      }
      zero_known <- !any(cens$may_be_zero[idx] & code != 0L)
      keep_zero <- is_mixture && zero_known
      obs_y <- hold_at_bound(obs_y, lower, upper, keep_zero)
      sim_y <- hold_at_bound(sim_y, lower, upper, keep_zero)
    }
    res_obs <- obs_y - mu[idx]
    res_sim <- sweep(sim_y, 2, mu[idx], "-")
    obs_mean <- mean(obs_y)
    sim_mean <- rowMeans(sim_y)
    has_sd <- length(idx) > 1L
    obs_sd <- if (has_sd) sd(res_obs) else NA_real_
    sim_sd <- if (has_sd) apply(res_sim, 1, sd) else NA_real_
    sim_sd_median <- if (has_sd) median(sim_sd) else NA_real_
    # Report two zero statistics as agreement; 0/0 would flag the control.
    # A held group can also differ by rounding alone.
    ratio <- function(o, s) {
      if ((held && isTRUE(is_tie(o, s))) ||
          (isTRUE(o == 0) && isTRUE(s == 0))) 1 else o / s
    }
    row <- data.frame(
      group = lv,
      n = length(idx),
      obs_mean = obs_mean,
      sim_mean = median(sim_mean),
      mean_ratio = ratio(obs_mean, median(sim_mean)),
      ppp_mean = ppp_value(sim_mean, obs_mean),
      obs_sd = obs_sd,
      sim_sd = sim_sd_median,
      sd_ratio = ratio(obs_sd, sim_sd_median),
      ppp_sd = ppp_value(sim_sd, obs_sd),
      # The 95% span of each simulated statistic, for plot(). print() hides
      # it to keep the table within one line width.
      sim_mean_lo = unname(quantile(sim_mean, 0.025)),
      sim_mean_hi = unname(quantile(sim_mean, 0.975)),
      sim_sd_lo = if (has_sd) unname(quantile(sim_sd, 0.025)) else NA_real_,
      sim_sd_hi = if (has_sd) unname(quantile(sim_sd, 0.975)) else NA_real_,
      stringsAsFactors = FALSE
    )
    if (!is.null(cens)) {
      row <- data.frame(row[1:2], n_cens = n_cens, row[-(1:2)])
    }
    if (is_mixture) {
      # Callers enable this zero-fraction check for hurdle fits and the
      # combined hurdle check. Zero-inflated count fits do not reach it.
      obs_zero <- mean(obs_y == 0)
      sim_zero <- rowMeans(sim_y == 0)
      if (!zero_known) {
        # A censored reading here could be a zero, so the held values hide
        # the zeros on both sides.
        obs_zero <- NA_real_
        sim_zero <- NA_real_
      }
      row$obs_zero <- obs_zero
      row$sim_zero <- median(sim_zero)
      row$ppp_zero <- ppp_value(sim_zero, obs_zero)
    }
    row
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

#' @rdname check_fit
#' @order 2
#'
#' @method check_fit bayesnecfit
#'
#' @inherit check_fit description return examples
#'
#' @importFrom stats model.frame
#' @importFrom chk chk_number
#'
#' @export
check_fit.bayesnecfit <- function(x, group = NULL, ndraws = 1000, seed = 10,
                                  ...) {
  # Validate the seed: set.seed(NULL) is not an error but reseeds from the
  # clock, so the diagnostic would not repeat.
  chk_number(seed)
  mod_dat <- model.frame(x$bayesnecformula, data = x$fit$data)
  y_var <- attr(mod_dat, "bnec_pop")[["y_var"]]
  x_var <- attr(mod_dat, "bnec_pop")[["x_var"]]
  y <- x$fit$data[[y_var]]
  predictor <- x$fit$data[[x_var]]
  grp <- check_fit_groups(predictor, group)
  out <- check_fit_table(x$fit, x$bayesnecformula, y, grp, ndraws, seed,
                         is_hurdle_family(x$fit$family))
  ctrl <- check_fit_control_groups(predictor, grp, recorded_x_control(x))
  out$control <- out$group %in% ctrl
  attr(out, "model") <- x$model
  attr(out, "pareto") <- check_fit_pareto_row(
    x, if (!is.null(x$fit$criteria$logo)) "logo" else "loo"
  )
  allot_class(out, c("checkfit", "data.frame"))
}

#' @rdname check_fit
#' @order 3
#'
#' @method check_fit bayesmanecfit
#'
#' @inherit check_fit description return examples
#'
#' @export
check_fit.bayesmanecfit <- function(x, group = NULL, ndraws = 1000, seed = 10,
                                    ...) {
  # Validated here as well as in the bayesnecfit method it delegates to, so
  # that a bad seed is refused before the first pull_out() rather than after.
  chk_number(seed)
  out <- lapply(names(x$mod_fits), function(m) {
    # pull_out() expands a fit and recalculates its criteria. This check only
    # needs the stored brms fit, formula, and equation name.
    fit_m <- allot_class(x$mod_fits[[m]], c("bayesnecfit", "bnecfit"))
    res <- check_fit(fit_m, group = group, ndraws = ndraws, seed = seed)
    res$model <- m
    res$wi <- x$mod_stats[m, "wi"]
    res
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out <- out[, c("model", "wi",
                 setdiff(names(out), c("model", "wi"))), drop = FALSE]
  criterion <- check_fit_weight_criterion(x)
  attr(out, "pareto") <- do.call(rbind, lapply(names(x$mod_fits), function(m) {
    check_fit_pareto_row(x$mod_fits[[m]], criterion,
                         unname(x$mod_stats[m, "wi"]))
  }))
  allot_class(out, c("checkfit", "data.frame"))
}

#' @rdname check_fit
#' @order 4
#' @method check_fit bayesnecgroupfit
#' @inherit check_fit description return examples
#' @export
check_fit.bayesnecgroupfit <- function(x, group = NULL, ndraws = 1000,
                                      seed = 10, ...) {
  chk_number(seed)
  levels <- x$levels
  if (is.null(levels)) levels <- names(x$fits)
  if (is.null(levels) || !identical(levels, names(x$fits))) {
    stop("The fitted levels do not match the component fits.", call. = FALSE)
  }
  groups <- rep(list(group), length(levels))
  if (!is.null(group) && length(group) > 1L) {
    if (!is.atomic(group) || !is.null(dim(group)) ||
        length(group) != nrow(x$data) || anyNA(group)) {
      stop("`group` must have one non-missing value per row of the original",
           " data, in its original order.", call. = FALSE)
    }
    if (is.null(x$group_var) || !x$group_var %in% names(x$data)) {
      stop("The fitted-level column is missing from the original data.",
           call. = FALSE)
    }
    groups <- lapply(levels, function(level) {
      group[as.character(x$data[[x$group_var]]) == level]
    })
    for (i in seq_along(levels)) {
      fit <- x$fits[[i]]
      n_rows <- if (inherits(fit, "bayesmanecfit")) {
        vapply(fit$mod_fits, function(model) nrow(model$fit$data), integer(1))
      } else {
        nrow(fit$fit$data)
      }
      if (any(length(groups[[i]]) != n_rows)) {
        stop("The original rows for fitted level \"", levels[[i]],
             "\" do not match its fitted data.", call. = FALSE)
      }
    }
  } else if (!is.null(group) &&
             (length(group) != 1L || !is.numeric(group) || anyNA(group))) {
    stop("`group` must be a numeric bin count or a full-input grouping vector.",
         call. = FALSE)
  }
  out <- setNames(lapply(seq_along(levels), function(i) {
    check_fit(x$fits[[i]], group = groups[[i]], ndraws = ndraws, seed = seed)
  }), levels)
  structure(list(fits = out, group_var = x$group_var),
            class = "checkfit_group")
}

#' @rdname check_fit
#' @export
print.checkfit_group <- function(x, ...) {
  for (level in names(x$fits)) {
    cat("\n", x$group_var, " = ", level, "\n", sep = "")
    print(x$fits[[level]])
  }
  invisible(x)
}

#' @rdname check_fit
#' @export
plot.checkfit_group <- function(x, ...) {
  setNames(lapply(names(x$fits), function(level) {
    plot(x$fits[[level]]) +
      ggplot2::labs(title = paste0(x$group_var, ": ", level))
  }), names(x$fits))
}

#' Print a check_fit table
#'
#' @param x An object of class \code{checkfit}.
#' @param ... Unused.
#'
#' @return No return value, prints to the console.
#'
#' @noRd
#' @export
print.checkfit <- function(x, ...) {
  y <- as.data.frame(x)
  # The simulated intervals are for plot(), not for reading: including them
  # takes the table past a readable line width. They stay on the object.
  y <- y[, !grepl("_(lo|hi)$", names(y)), drop = FALSE]
  num <- vapply(y, is.numeric, logical(1))
  y[num] <- lapply(y[num], function(z) round(z, 3))
  print(y)
  if (!any(y$control)) {
    cat("\nNo observed row is at the stated control concentration, so",
        " no group is labelled as the control.\n")
  }
  flagged <- y$ppp_mean < 0.05 | y$ppp_mean > 0.95 |
    y$ppp_sd < 0.05 | y$ppp_sd > 0.95
  flagged[is.na(flagged)] <- FALSE
  if (any(flagged & y$control)) {
    cat("\nThe control group is flagged. nsec() reads its reference from the",
        "\ncontrol, so this is the row most likely to move a reported",
        "\nno-effect concentration. See ?check_fit.\n")
  } else if (any(flagged)) {
    cat("\nOne or more groups show a posterior predictive p-value beyond",
        "\n[0.05, 0.95]. See ?check_fit.\n")
  }
  if (any(y$n_cens > 0)) {
    cat("\nn_cens is the number of censored readings in a group. The",
        "\nstatistics of such a group are computed on observed and simulated",
        "\nvalues held at its censoring bound, and show nothing of the fit",
        "\nbeyond that bound. See ?check_fit.\n")
  }
  p <- attr(x, "pareto")
  if (!is.null(p)) {
    cat("\nPareto k for stored leave-one-out scores:", p$criterion[1], "\n")
    if (all(p$status == "unavailable")) {
      cat("No stored criterion with Pareto diagnostics is available.\n")
    } else {
      shown <- seq_len(nrow(p))
      if (nrow(p) > 10 && all(!is.na(p$weight))) {
        shown <- which(p$weight >= 0.01)
        if (length(shown) == 0) shown <- order(p$weight, decreasing = TRUE)[1]
      }
      compact <- p[shown, c("model", "weight", "n_high_k", "max_k", "cutoff",
                             "status"), drop = FALSE]
      if (all(is.na(compact$weight))) compact$weight <- NULL
      num <- vapply(compact, is.numeric, logical(1))
      compact[num] <- lapply(compact[num], function(z) round(z, 3))
      print(compact, row.names = FALSE)
      for (i in shown) {
        if (length(p$flagged_ids[[i]])) {
          cat(p$model[i], "at or above cutoff:",
              paste(p$flagged_ids[[i]], collapse = ", "), "\n")
        }
        if (length(p$refit_failed_ids[[i]])) {
          cat(p$model[i], "refit failed:",
              paste(p$refit_failed_ids[[i]], collapse = ", "), "\n")
        }
      }
      if (length(shown) < nrow(p)) {
        cat(nrow(p) - length(shown), "lower-weight equations omitted from",
            " print; see attr(x, \"pareto\") for all equations.\n")
      }
      if (all(p$status == "available" & p$n_high_k == 0)) {
        cat("No held-out unit is at or above the Pareto cutoff.\n")
      }
      cat("High k concerns the leave-one-out approximation, not sampler",
          " convergence or the posterior predictive check.\n")
    }
  }
  invisible(x)
}

#' @rdname check_fit
#' @order 4
#'
#' @method check_fit bayesnechurdlefit
#'
#' @inherit check_fit description return examples
#'
#' @details For a \code{\link{bayesnechurdlefit}} one table is returned per
#' component, following the precedent \code{\link{dispersion}} sets. The two
#' carry different response vectors --- growth is fitted on survivors only,
#' survival on every individual --- so there is no single table that describes
#' both.
#'
#' @param combined A \code{\link[base]{logical}} vector of length 1. If
#' \code{TRUE} (the default) a third table is returned checking the two
#' components against the response they jointly predict, which is what the user
#' actually observed. The growth component is predicted onto the full exposed
#' set rather than onto its own data, since it is fitted on survivors only.
#' Where either component is a model set, the third table has one block of rows
#' per pair of growth and survival equations, named \code{"growth / survival"}
#' in the column \code{model}. When one component has one equation, \code{wi}
#' is the other component's model weight. With model sets in both components,
#' \code{wi} is available under pseudo-BMA through
#' \code{\link{crossed_weights}}; it is omitted for other weighting methods.
#'
#' @export
check_fit.bayesnechurdlefit <- function(x, group = NULL, ndraws = 1000,
                                        seed = 10, combined = TRUE, ...) {
  chk_lgl(combined)
  chk_number(seed)
  growth_group <- group
  if (!is.null(group) && length(group) > 1L) {
    n_exposed <- nrow(component_fits(x$survival)[[1]]$fit$data)
    if (!is.atomic(group) || !is.null(dim(group)) ||
        length(group) != n_exposed || anyNA(group)) {
      stop("`group` must have one non-missing value per exposed row, in",
           " its original order.", call. = FALSE)
    }
    growth_group <- group[hurdle_observed_response(x) > 0]
  }
  out <- list(growth = check_fit(x$growth, group = growth_group,
                                 ndraws = ndraws,
                                 seed = seed),
              survival = check_fit(x$survival, group = group, ndraws = ndraws,
                                   seed = seed))
  if (combined) {
    out$combined <- check_fit_combined(x, group = group, ndraws = ndraws,
                                       seed = seed)
  }
  out
}

#' The hurdle fit checked against the response as measured
#'
#' The per-component tables check growth against the survivors and survival
#' against everyone, and neither against the observed response with its
#' zeros. This draws alive or dead from the survival fit, draws growth for
#' every exposed row, and multiplies. Posterior draws are paired by row and
#' truncated to the smaller count; the hurdle likelihood factorises, so any
#' pairing is valid.
#'
#' \code{newdata} must be the full exposed set, not the growth fit's own data,
#' which holds survivors only and would give a vector of the wrong length.
#' Growth is then predicted beyond the largest predictor value with a
#' survivor. Those predictions still affect the combined check wherever the
#' survival component predicts a nonzero response.
#'
#' Where either component is a model set, every pair of equations is checked,
#' as \code{\link{check_fit}} checks every equation of a set.
#'
#' @inheritParams check_fit
#'
#' @return A \code{\link[base]{data.frame}} of class \code{checkfit}.
#'
#' @importFrom brms posterior_predict posterior_epred
#' @importFrom stats median
#'
#' @noRd
check_fit_combined <- function(x, group = NULL, ndraws = 1000, seed = 10) {
  g_fits <- component_fits(x$growth)
  s_fits <- component_fits(x$survival)
  # Every equation of a component is fitted to the same rows.
  gb <- g_fits[[1]]$fit
  sb <- s_fits[[1]]$fit
  s_dat <- model.frame(s_fits[[1]]$bayesnecformula, data = sb$data)
  predictor <- retrieve_var(s_dat, "x_var", error = TRUE)
  y_obs <- hurdle_observed_response(x)
  grp <- check_fit_groups(predictor, group)
  n_av <- min(vapply(c(g_fits, s_fits), function(f) brms::ndraws(f$fit),
                     numeric(1)))
  nd <- min(ndraws, n_av)
  g_rows <- hurdle_growth_rows(x, sb$data, growth_data = gb$data)
  # Draw each equation's predictions once, before the pairs are formed, so
  # that every pair sharing an equation shares its draws.
  with_preserved_rng_state({
    set.seed(seed)
    alive <- lapply(s_fits, function(f) posterior_predict(f$fit, ndraws = nd))
    grow <- lapply(g_fits, function(f) {
      posterior_predict(f$fit, newdata = g_rows, ndraws = nd)
    })
    mu_alive <- lapply(s_fits, function(f) {
      apply(posterior_epred(f$fit), 2, median)
    })
    mu_grow <- lapply(g_fits, function(f) {
      apply(factorised_count_epred(
        f$fit, f$bayesnecformula, newdata = g_rows, ndraws = nd
      ), 2, median)
    })
  })
  cens <- hurdle_exposed_cens(gb, y_obs)
  ctrl <- check_fit_control_groups(predictor, grp, recorded_x_control(x))
  pairs <- expand.grid(growth = names(g_fits), survival = names(s_fits),
                       stringsAsFactors = FALSE)
  methods <- unique(unlist(lapply(list(x$growth, x$survival),
                                  fit_weights_method)))
  show_weights <- nrow(pairs) > 1L &&
    (length(g_fits) == 1L || length(s_fits) == 1L ||
       length(methods) == 0L || identical(methods, "pseudobma"))
  if (nrow(pairs) > 1L && !show_weights) {
    warning("Combined model-pair weights are unavailable when both",
            " components have model sets and either uses weights other than",
            " pseudo-BMA; the table omits `wi`.",
            call. = FALSE)
  }
  weights <- if (show_weights) crossed_weights(x) else NULL
  out <- lapply(seq_len(nrow(pairs)), function(i) {
    g <- pairs$growth[i]
    s <- pairs$survival[i]
    n <- min(nrow(alive[[s]]), nrow(grow[[g]]))
    yrep <- alive[[s]][seq_len(n), , drop = FALSE] *
      grow[[g]][seq_len(n), , drop = FALSE]
    res <- check_fit_stats(y_obs, yrep, mu_alive[[s]] * mu_grow[[g]], grp,
                           is_mixture = TRUE, cens = cens)
    res$control <- res$group %in% ctrl
    if (nrow(pairs) > 1) {
      if (show_weights) {
        res <- data.frame(model = paste(g, s, sep = " / "),
                          wi = weights[g, s], res)
      } else {
        res <- data.frame(model = paste(g, s, sep = " / "), res)
      }
    }
    res
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  allot_class(out, c("checkfit", "data.frame"))
}

#' The equations of one component of a hurdle fit
#'
#' @param x A \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @return A named \code{\link[base]{list}} with one element per equation,
#' each holding \code{fit} and \code{bayesnecformula}.
#'
#' @noRd
component_fits <- function(x) {
  if (inherits(x, "bayesmanecfit")) {
    return(x$mod_fits)
  }
  setNames(list(x), x$model)
}

#' The growth component's censoring, placed on the exposed rows of a hurdle fit
#'
#' @param gb The growth component's \code{\link[brms]{brmsfit}}.
#' @param y_obs The observed response of every exposed row, zeros included.
#'
#' @details The growth rows are the survivors, in the order of the exposed
#' rows, so they are the rows where \code{y_obs} is positive. A censored
#' survivor is known to be alive, so none of its readings can be a zero.
#'
#' @return \code{NULL} where the growth component has no \code{cens()} term,
#' otherwise a \code{\link[base]{list}} in the form \code{check_fit_cens()}
#' returns, one element per exposed row.
#'
#' @noRd
hurdle_exposed_cens <- function(gb, y_obs) {
  g_cens <- check_fit_cens(gb)
  if (is.null(g_cens)) {
    return(NULL)
  }
  alive <- y_obs > 0
  if (sum(alive) != length(g_cens$code)) {
    stop("The growth component of this hurdle fit has ", length(g_cens$code),
         " rows and the data ", sum(alive), " non-zero responses, so its",
         " censoring cannot be matched to the exposed rows.", call. = FALSE)
  }
  code <- integer(length(y_obs))
  bound <- numeric(length(y_obs))
  code[alive] <- g_cens$code
  bound[alive] <- g_cens$bound
  list(code = code, bound = bound, may_be_zero = logical(length(y_obs)))
}

#' The exposed rows of a hurdle fit, with the growth component's disp() columns
#'
#' @param x A \code{\link{bayesnechurdlefit}}.
#' @param frame The survival component's fitted data, one row per exposed
#' individual.
#' @param growth_data The rows fitted by the growth component. Where supplied,
#' categorical dispersion values absent from those rows are refused.
#'
#' @details The survival component's data lacks the columns a growth
#' \code{disp(~z)} term names, since the survival formula has no \code{disp()}
#' term. Each row takes its recorded value from the data \code{bnec_hurdle()}
#' was given, matched by position: both components keep the order of those
#' rows. A category found only among non-survivors has no fitted growth
#' dispersion, so it is refused before \pkg{brms} reports an unknown level.
#'
#' @return \code{frame}, unchanged where the growth formula has no such
#' column.
#'
#' @noRd
hurdle_growth_rows <- function(x, frame, growth_data = NULL) {
  required <- disp_covariates(x$formula)
  if (length(required) == 0) {
    return(frame)
  }
  columns <- setdiff(required, names(frame))
  if (length(columns) > 0 && nrow(x$data) != nrow(frame)) {
    stop("The survival component of this hurdle fit has ", nrow(frame),
         " rows and its data ", nrow(x$data), ", so the growth component's",
         " disp() ", if (length(columns) == 1) "column" else "columns", " ",
         paste0("\"", columns, "\"", collapse = ", "), " cannot be matched",
         " to the rows it is simulated for.", call. = FALSE)
  }
  for (v in columns) {
    frame[[v]] <- x$data[[v]]
  }
  if (!is.null(growth_data)) {
    for (v in required) {
      if (!(is.factor(frame[[v]]) || is.character(frame[[v]]) ||
            is.logical(frame[[v]]))) {
        next
      }
      absent <- setdiff(unique(as.character(frame[[v]])),
                        unique(as.character(growth_data[[v]])))
      if (length(absent) > 0) {
        stop("check_fit() cannot simulate the growth component for disp()",
             " column \"", v, "\" at ",
             if (length(absent) == 1) "value " else "values ",
             paste0("\"", absent, "\"", collapse = ", "),
             ": no surviving response with ",
             if (length(absent) == 1) "that value was" else "those values were",
             " used to fit the growth component.", call. = FALSE)
      }
    }
  }
  frame
}

#' The observed response of a hurdle fit, zeros included
#'
#' \code{bnec_hurdle()} splits the data into a survival frame carrying every
#' exposed row and a growth frame carrying the survivors, so neither component's
#' own data is the response as measured. Reconstructed here by zeroing the
#' non-survivors.
#'
#' @param x A \code{\link{bayesnechurdlefit}}.
#'
#' @return A \code{\link[base]{numeric}} vector, one element per exposed row.
#'
#' @noRd
hurdle_observed_response <- function(x) {
  s_fit <- component_fits(x$survival)[[1]]
  s_dat <- model.frame(s_fit$bayesnecformula, data = s_fit$fit$data)
  alive <- retrieve_var(s_dat, "y_var", error = TRUE)
  g_fit <- component_fits(x$growth)[[1]]
  g_dat <- model.frame(g_fit$bayesnecformula, data = g_fit$fit$data)
  g_y <- retrieve_var(g_dat, "y_var", error = TRUE)
  out <- numeric(length(alive))
  # The survivors appear in the growth frame in the same order they appear in
  # the survival frame -- bnec_hurdle() subsets rather than reorders -- so the
  # growth response drops straight into the alive positions.
  out[alive > 0] <- g_y
  out
}

#' Plot a check_fit table
#'
#' The numeric table answers "is any group off?"; the plot answers "by how much,
#' and in which direction?", which is the question that decides whether it
#' matters. Each panel shows, per group of the predictor, the observed statistic
#' against the 95% span of what the fitted model simulates. A point outside its
#' interval is a group the model does not reproduce. A group with one
#' observation appears in the location panel only, because its residual
#' standard deviation is undefined.
#'
#' Two panels rather than one: the location and the scale fail independently and
#' for different reasons. A model can get the mean of every group right while
#' simulating far too much spread, which is the case
#' \code{\link{check_fit}} exists to catch and which a single combined panel
#' would hide.
#'
#' The control is drawn differently because it is not just another group:
#' \code{\link{nsec}} reads its reference from the control, so a discrepancy
#' there moves a reported no-effect concentration in a way that a discrepancy at
#' the top of the curve does not.
#'
#' Built directly in \pkg{ggplot2}, which is already in \code{Depends}. No
#' \pkg{bayesplot} dependency -- see #148.
#'
#' @param x An object of class \code{checkfit}, from \code{\link{check_fit}}.
#' @param ... Unused.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @importFrom ggplot2 ggplot aes geom_linerange geom_point facet_wrap labs
#' @importFrom ggplot2 scale_shape_manual scale_colour_manual theme_bw
#' @importFrom ggplot2 element_text theme vars
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' plot(check_fit(manec_example))
#' }
#'
#' @method plot checkfit
#' @export
plot.checkfit <- function(x, ...) {
  d <- as.data.frame(x)
  # Reshaped by rbind rather than a tidyr call: R/ is base R by convention, and
  # two statistics do not justify a reshape dependency in package code.
  long <- rbind(
    data.frame(group = d$group, control = d$control,
               statistic = "location (mean)",
               observed = d$obs_mean, lo = d$sim_mean_lo, hi = d$sim_mean_hi,
               stringsAsFactors = FALSE),
    data.frame(group = d$group, control = d$control,
               statistic = "scale (residual SD)",
               observed = d$obs_sd, lo = d$sim_sd_lo, hi = d$sim_sd_hi,
               stringsAsFactors = FALSE)
  )
  if (!is.null(d$model)) {
    long$model <- rep(d$model, times = 2)
  }
  long <- long[is.finite(long$observed) & is.finite(long$lo) &
                 is.finite(long$hi), , drop = FALSE]
  long$role <- ifelse(long$control, "control", "exposed")
  long$group <- factor(long$group, levels = unique(d$group))
  caption <- paste("Bars are the 95% span of the statistic simulated",
                   "from the fit.\nA point outside its bar is a group",
                   "the model does not reproduce.")
  if (any(d$n_cens > 0)) {
    caption <- paste0(caption, "\nGroups with censored readings are compared",
                      " with values held at their censoring bound.")
  }
  p <- ggplot(long, aes(x = .data$group)) +
    geom_linerange(aes(ymin = .data$lo, ymax = .data$hi)) +
    geom_point(aes(y = .data$observed, colour = .data$role,
                   shape = .data$role), size = 2.4) +
    scale_colour_manual(values = c(control = "#b2182b", exposed = "black")) +
    scale_shape_manual(values = c(control = 17, exposed = 16)) +
    labs(x = NULL, y = "observed against simulated (95%)",
         colour = NULL, shape = NULL, caption = caption) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  if (!is.null(long$model)) {
    p + facet_wrap(vars(.data$model, .data$statistic), scales = "free_y")
  } else {
    p + facet_wrap(vars(.data$statistic), scales = "free_y")
  }
}
