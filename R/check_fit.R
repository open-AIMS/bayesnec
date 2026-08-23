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
#' \code{\link{bayesmanecfit}} as returned by \code{\link{bnec}}.
#' @param group A \code{\link[base]{numeric}} vector of the same length as the
#' data giving the group each row belongs to, or a single
#' \code{\link[base]{numeric}} giving the number of bins to cut the predictor
#' into. If \code{NULL} (the default) the distinct values of the predictor are
#' used where they are replicated, and the predictor is binned otherwise, with a
#' warning.
#' @param ndraws A \code{\link[base]{numeric}} vector of length 1, the number of
#' posterior draws to simulate from. Reduced silently to the number the fit
#' actually holds where that is smaller.
#' @param seed A \code{\link[base]{numeric}} vector of length 1. Passed to
#' \code{\link[base]{set.seed}} before simulating, so the result is
#' reproducible.
#' @param ... Unused.
#'
#' @details \bold{Why this is not \code{\link{dispersion}}}
#'
#' \code{\link{dispersion}} reports one global statistic, and for any family
#' with a free dispersion parameter that parameter absorbs exactly the
#' discrepancy the global statistic measures. On the packaged
#' \code{manec_example} the global Pearson ratio is 1.011, with a 95\% interval
#' of 0.71 to 1.44 --- a
#' clean bill of health --- while the same fit simulates about 27\% more
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
#' Stacking weights come from a global \code{elpd}. The control is a handful of
#' rows out of many, so a candidate model can hold high weight while fitting the
#' control badly --- it wins on the bulk of the curve and pays almost nothing
#' for the control. For a \code{\link{bayesmanecfit}} the per-model rows are
#' therefore reported alongside the averaged rows, or the table could not say
#' which model is doing the damage.
#'
#' @return A \code{\link[base]{data.frame}} with one row per group (per model,
#' for a \code{\link{bayesmanecfit}}), carrying the group, the number of
#' observations, the observed and simulated mean and standard deviation, their
#' ratios, a posterior predictive p-value for each, and a logical flag marking
#' the control group. For the mixture families it also carries the observed and
#' simulated proportion of zeros.
#'
#' @seealso \code{\link{pp_check}}, \code{\link{dispersion}},
#' \code{\link{check_chains}}, \code{\link{check_priors}}
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' check_fit(nec4param)
#' }
#'
#' @export
check_fit <- function(x, group = NULL, ndraws = 1000, seed = 10, ...) {
  UseMethod("check_fit")
}

#' Assign each observation to a group of the predictor
#'
#' Replication is preferred over binning because it is what the design actually
#' provides: a concentration-response experiment has replicates at each
#' concentration, and grouping by them asks a question about the design rather
#' than about an arbitrary cut. Binning always returns something, including
#' where the answer is meaningless, so it warns.
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

#' Posterior predictive p-value
#'
#' \code{P(T(yrep) >= T(y))}. Values near 0 or 1 both indicate misfit, in
#' opposite directions; 0.5 is perfect agreement.
#'
#' @param sim A \code{\link[base]{numeric}} vector of simulated statistics.
#' @param obs A \code{\link[base]{numeric}} vector of length 1.
#'
#' @return A \code{\link[base]{numeric}} vector of length 1.
#'
#' @noRd
ppp_value <- function(sim, obs) {
  sim <- sim[is.finite(sim)]
  if (!length(sim) || !is.finite(obs)) {
    return(NA_real_)
  }
  mean(sim >= obs)
}

#' The per-group table for one brmsfit
#'
#' @param fit An object of class \code{\link[brms]{brmsfit}}.
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
check_fit_table <- function(fit, y, grp, ndraws, seed, is_mixture) {
  # brms errors rather than truncating when ndraws exceeds what the fit holds,
  # so the default has to bend to the object. A short fit -- manec_example has
  # 100 draws -- is exactly what someone runs a diagnostic on first.
  available <- brms::ndraws(fit)
  if (ndraws > available) {
    ndraws <- available
  }
  set.seed(seed)
  yrep <- posterior_predict(fit, ndraws = ndraws)
  # The fitted mean is what residuals are taken against, for both the observed
  # and the simulated response, so that the two are comparable. Using each
  # draw's own mean instead would remove the very discrepancy being measured.
  mu <- apply(posterior_epred(fit), 2, median)
  levs <- levels(grp)
  out <- lapply(levs, function(lv) {
    idx <- which(grp == lv)
    obs_y <- y[idx]
    sim_y <- yrep[, idx, drop = FALSE]
    res_obs <- obs_y - mu[idx]
    res_sim <- sweep(sim_y, 2, mu[idx], "-")
    obs_mean <- mean(obs_y)
    sim_mean <- rowMeans(sim_y)
    obs_sd <- sd(res_obs)
    sim_sd <- apply(res_sim, 1, sd)
    row <- data.frame(
      group = lv,
      n = length(idx),
      obs_mean = obs_mean,
      sim_mean = median(sim_mean),
      mean_ratio = obs_mean / median(sim_mean),
      ppp_mean = ppp_value(sim_mean, obs_mean),
      obs_sd = obs_sd,
      sim_sd = median(sim_sd),
      sd_ratio = obs_sd / median(sim_sd),
      ppp_sd = ppp_value(sim_sd, obs_sd),
      # The 95% span of the simulated statistic, carried so plot() can show the
      # observed value against what the model actually simulates rather than
      # against a point summary of it. A posterior predictive p-value says a
      # group is off; the interval says by how much and in which direction,
      # which is what decides whether it matters. Hidden by print() -- see
      # print.checkfit() -- because it would take the console table past what
      # fits on a line. See #148.
      sim_mean_lo = unname(quantile(sim_mean, 0.025)),
      sim_mean_hi = unname(quantile(sim_mean, 0.975)),
      sim_sd_lo = unname(quantile(sim_sd, 0.025)),
      sim_sd_hi = unname(quantile(sim_sd, 0.975)),
      stringsAsFactors = FALSE
    )
    if (is_mixture) {
      # Only these families can emit a structural zero, and whether the zero
      # fraction is right is the whole question they were added for (#104).
      # Nothing else in the package reports it.
      obs_zero <- mean(obs_y == 0)
      sim_zero <- rowMeans(sim_y == 0)
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
#'
#' @export
check_fit.bayesnecfit <- function(x, group = NULL, ndraws = 1000, seed = 10,
                                  ...) {
  mod_dat <- model.frame(x$bayesnecformula, data = x$fit$data)
  y_var <- attr(mod_dat, "bnec_pop")[["y_var"]]
  x_var <- attr(mod_dat, "bnec_pop")[["x_var"]]
  y <- x$fit$data[[y_var]]
  predictor <- x$fit$data[[x_var]]
  grp <- check_fit_groups(predictor, group)
  out <- check_fit_table(x$fit, y, grp, ndraws, seed,
                         is_hurdle_family(x$fit$family))
  # The control is the lowest predictor value, which is the package's own
  # convention (check_data()) and is the column nsec() reads its reference from.
  ctrl <- levels(grp)[which.min(tapply(predictor, grp, min))]
  out$control <- out$group == ctrl
  attr(out, "model") <- x$model
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
  out <- lapply(names(x$mod_fits), function(m) {
    fit_m <- suppressMessages(pull_out(x, model = m))
    res <- check_fit(fit_m, group = group, ndraws = ndraws, seed = seed)
    res$model <- m
    res$wi <- x$mod_stats[m, "wi"]
    res
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out <- out[, c("model", "wi",
                 setdiff(names(out), c("model", "wi"))), drop = FALSE]
  allot_class(out, c("checkfit", "data.frame"))
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
#' @export
check_fit.bayesnechurdlefit <- function(x, group = NULL, ndraws = 1000,
                                        seed = 10, ...) {
  list(growth = check_fit(x$growth, group = group, ndraws = ndraws,
                          seed = seed),
       survival = check_fit(x$survival, group = group, ndraws = ndraws,
                            seed = seed))
}

#' Plot a check_fit table
#'
#' The numeric table answers "is any group off?"; the plot answers "by how much,
#' and in which direction?", which is the question that decides whether it
#' matters. Each panel shows, per group of the predictor, the observed statistic
#' against the 95% span of what the fitted model simulates. A point outside its
#' interval is a group the model does not reproduce.
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
  long$role <- ifelse(long$control, "control", "exposed")
  long$group <- factor(long$group, levels = unique(d$group))
  p <- ggplot(long, aes(x = .data$group)) +
    geom_linerange(aes(ymin = .data$lo, ymax = .data$hi)) +
    geom_point(aes(y = .data$observed, colour = .data$role,
                   shape = .data$role), size = 2.4) +
    scale_colour_manual(values = c(control = "#b2182b", exposed = "black")) +
    scale_shape_manual(values = c(control = 17, exposed = 16)) +
    labs(x = NULL, y = "observed against simulated (95%)",
         colour = NULL, shape = NULL,
         caption = paste("Bars are the 95% span of the statistic simulated",
                         "from the fit.\nA point outside its bar is a group",
                         "the model does not reproduce.")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  if (!is.null(long$model)) {
    p + facet_wrap(vars(.data$model, .data$statistic), scales = "free_y")
  } else {
    p + facet_wrap(vars(.data$statistic), scales = "free_y")
  }
}
