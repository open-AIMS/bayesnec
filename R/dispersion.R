#' Posterior dispersion
#'
#' Calculates a posterior dispersion metric.
#'
#' @param model An object of class \code{\link{bayesnecfit}} whose distribution
#' family is either \code{\link[stats]{poisson}} or
#' \code{\link[stats]{binomial}}.
#' @param summary Logical. Should summary stats be returned instead of full
#' vector? Defaults to FALSE.
#' @param seed Change seed for reproducible purposes.
#'
#' @details This function calculates a dispersion metric which takes the ratio
#' between the observed relative to simulated Pearson residuals sums of
#' squares.
#'
#' @return A \code{\link[base]{numeric}} vector. If \code{summary} is FALSE, an
#' n-long vector containing the dispersion metric, where n is the number of post
#' warm-up posterior draws from the \code{\link[brms]{brmsfit}} object. An
#' element of that vector is \code{Inf} for a draw containing an observation of
#' zero fitted variance whose response is not the fitted value, and \code{NA}
#' for a draw in which no observation contributes a residual; both cases are
#' described under Details and both are reported when they arise. If
#' TRUE, then a \code{\link[base]{data.frame}} containing the summary stats
#' (median, 95% credible interval, and the posterior probability of
#' over-dispersion) of the dispersion metric.
#'
#' @details The statistic is the ratio of the observed to the simulated Pearson
#' residual sum of squares, whose null value is 1. With \code{summary = TRUE}
#' the returned vector reports \code{P(>1)}, the posterior probability that the
#' ratio exceeds 1. It uses the whole posterior rather than a point estimate or
#' a single tail quantile, and it is symmetric: \code{1 - P(>1)} is the
#' posterior probability of under-dispersion, which no other summary here
#' addresses.
#'
#' \bold{An observation the model reproduces exactly is excluded.} The Pearson
#' denominator is the fitted standard deviation, which is exactly zero wherever
#' the fitted mean underflows --- \code{mu (1 - mu) n} for a binomial and
#' \code{mu} for a Poisson both do so for a curve that decays fast enough. An
#' observation whose response equals that fitted value contributes \code{0/0}
#' to both sums and says nothing about dispersion in either direction, so it is
#' excluded from both and every draw is retained. The exclusion is the limit
#' rather than an approximation to it: as the fitted mean tends to zero with a
#' response of zero the observed term tends to zero, and the simulated term
#' does the same, because a predictive distribution of vanishing variance
#' returns its mean with probability approaching one. Excluding a term is
#' arithmetically identical to contributing zero to both sums, so it introduces
#' no bias in either direction, and the ratio is self-normalising --- under the
#' null each retained observation contributes approximately 1 to the
#' denominator, so the null value of 1 holds whatever the size of the retained
#' set. The exclusion is made draw by draw, since an observation whose fitted
#' mean underflows in one draw gives an ordinary residual in another, and the
#' number of observations and of draws affected is reported. Where instead the response differs from a fitted value
#' of zero variance, the model has assigned zero variance to a value it did not
#' predict; the statistic is reported as \code{Inf} and a warning names the
#' observations. The infinity is the underflow --- in exact arithmetic the
#' residual there is large and finite --- but the misfit it reports is real. An empty vector is
#' returned only where every observation has zero variance and is reproduced
#' exactly, in which case there are no residuals to compare.
#'
#' \bold{A beta-binomial fit does not address under-dispersion.}
#' \code{beta_binomial} adds a variance component to the binomial, so it can
#' represent a variance above the binomial's and not one below it. Where
#' \code{P(>1)} is near 0 --- the data vary less than the fitted model implies
#' --- moving from \code{binomial} to \code{beta_binomial} cannot help, and
#' the usual causes are a mis-specified mean curve or non-independent
#' observations that make the effective sample size smaller than the nominal
#' one.
#'
#' @importFrom brms standata posterior_linpred posterior_epred posterior_predict
#' @importFrom chk chk_lgl
#'
#' @references
#' Zuur, A. F., Hilbe, J. M., & Ieno, E. N. (2013). A Beginner's Guide to GLM
#' and GLMM with R: A Frequentist and Bayesian Perspective for Ecologists.
#' Highland Statistics Limited.
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' data(nec_data)
#' # A Poisson mean following the curve, so the counts are a genuine count
#' # process; rounding a scaled proportion gives a variance that does not change
#' # with the mean, which no count distribution can represent.
#' mu <- 5 + (85 - 5) * exp(-exp(0.3) * (nec_data$x - 1.5) * (nec_data$x > 1.5))
#' nec_data$y <- as.integer(rpois(length(mu), mu))
#' nec4param <- bnec(y ~ crf(x, "nec4param"), data = nec_data, chains = 2)
#' dispersion(nec4param, summary = TRUE)
#' }
#' @export
dispersion <- function(model, summary = FALSE, seed = 10) {
  # Not an S3 generic, so a hurdle fit has to be handled here. It has two
  # underlying brmsfits and no combined analogue, so one result per component.
  if (is_bayesnechurdlefit(model)) {
    return(list(
      growth = dispersion(model$growth, summary = summary, seed = seed),
      survival = dispersion(model$survival, summary = summary, seed = seed)
    ))
  }
  chk_lgl(summary)
  chk_number(seed)
  # Captured before `model` is replaced by the brmsfit. expand_nec() calls this
  # once per equation, so an unnamed message on a model set says only that
  # something somewhere was excluded, which is not enough to act on.
  mod_name <- model$model
  mod_label <- if (is.null(mod_name)) "fitted" else mod_name
  formula <- model$bayesnecformula
  model <- model$fit
  mod_dat <- model.frame(formula, data = model$data)
  allowed_fams <- c("poisson", "binomial")
  fam <- model$family$family
  if (fam %in% allowed_fams) {
    # The link is taken from the fit rather than left at the family default.
    # get("poisson")() is a log link and get("binomial")() a logit one, but
    # The fit is on link = "identity" -- assigned by bnec() unless the caller
  # wrote a link -- so posterior_linpred() below is already
    # on the response scale and linkinv() would transform it a second time. For
    # a Poisson that means exp() of a mean of ~90, giving variance weights of
    # ~1e39; they do not cancel out of the ratio, because rowSums() weights the
    # two sums over observations separately, so the statistic ends up dominated
    # by the lowest-mean observations and understates dispersion. Reading the
    # link off the fit rather than hard-coding "identity" keeps this correct if
    # a future path stops forcing it. See #247.
    fam_fcts <- get(fam)(link = model$family$link)
    obs_y <- standata(model)$Y
    lpd_out <- posterior_linpred(model)
    prd_out <- posterior_epred(model)
    set.seed(seed)
    ppd_out <- posterior_predict(model)
    # The per-observation fitted variance is collected first and the residual
    # arithmetic done in one place, because an observation with zero variance
    # has to be handled differently depending on whether the response agrees
    # with the fitted value. See pearson_dispersion() and #298.
    var_out <- matrix(0, nrow(prd_out), ncol(prd_out))
    for (i in seq_len(nrow(prd_out))) {
      prd_mu <- fam_fcts$linkinv(lpd_out[i, ])
      prd_var_y <- fam_fcts$variance(prd_mu)
      if (fam == "binomial") {
        trials_var <- attr(mod_dat, "bnec_pop")[["trials_var"]]
        prd_var_y <- prd_var_y * model$data[[trials_var]]
      }
      rate_var <- unname(attr(mod_dat, "bnec_pop")["rate_var"])
      if (fam == "poisson" && !is.na(rate_var)) {
        # Exactly parallel to the binomial branch, and exact for Poisson:
        # prd_mu is the rate, the observations are counts over an exposure, and
        # Var(count) = mu * denom. Note this does NOT generalise to
        # negbinomial, where brms scales the shape by the denominator too, so
        # the count-scale variance is mu_c + mu_c^2 / (shape * denom) rather
        # than a plain multiple. dispersion() does not accept negbinomial at
        # all -- see allowed_fams above -- so there is nothing to get wrong
        # today, but whoever widens that list must derive the negbinomial case
        # rather than copying this line. See #136. Such a change also reaches
        # pearson_dispersion(), whose classification of a zero denominator
        # assumes the variance and the mean underflow together; that holds for
        # binomial and Poisson and not for a Gamma-style variance.
        prd_var_y <- prd_var_y * model$data[[rate_var]]
      }
      var_out[i, ] <- prd_var_y
    }
    disp <- pearson_dispersion(obs_y, prd_out, ppd_out, var_out,
                               labels = rownames(model$data),
                               model_name = mod_name)
    if (all(is.na(disp))) {
      # Reached when no draw retains an observation. The usual cause is that
      # every observation has zero variance and is reproduced exactly, but the
      # message states the condition tested rather than assuming the cause.
      message("No observation contributes a residual to compare in any draw of",
              " the ", mod_label, " model, so the dispersion statistic is not",
              " defined for this fit. Where the fitted mean underflows this is",
              " because the model reproduces every observation exactly.")
      numeric()
    } else {
      if (summary) {
        # P(dispersion > 1) is added to the median and the equal-tailed
        # interval. The two rules the interval supports are both poor: a
        # threshold on the point estimate discards the uncertainty the
        # statistic was computed to express, and requiring Q2.5 > 1 is blunt,
        # because each draw compares one observed residual sum against a single
        # simulated replicate and the interval's width at a typical design is
        # dominated by replicate-to-replicate simulation noise. The posterior
        # probability uses the whole posterior, is directly interpretable, and
        # is symmetric: 1 - p answers the under-dispersion question, which
        # nothing else here addresses. See #262.
        c(estimates_summary(disp), "P(>1)" = mean(disp > 1, na.rm = TRUE))
      } else {
        disp
      }      
    }
  } else {
    numeric()
  }
}

#' Pearson residual dispersion ratio, with degenerate observations handled
#'
#' Computes, for each posterior draw, the ratio of the observed to the
#' simulated Pearson residual sum of squares.
#'
#' @param obs_y A \code{\link[base]{numeric}} vector of observed responses.
#' @param prd_out A draws-by-observations \code{\link[base]{matrix}} of fitted
#' means, from \code{\link[brms]{posterior_epred}}.
#' @param ppd_out A draws-by-observations \code{\link[base]{matrix}} of
#' simulated responses, from \code{\link[brms]{posterior_predict}}.
#' @param var_out A draws-by-observations \code{\link[base]{matrix}} of fitted
#' variances on the scale of \code{obs_y}.
#' @param labels A \code{\link[base]{character}} vector naming the
#' observations, used in the message and the warning. Defaults to the column
#' positions.
#' @param model_name A \code{\link[base]{character}} naming the equation, used
#' in the message and the warning.
#'
#' @details The Pearson denominator is \code{sqrt(var_out)}, which is exactly
#' zero wherever the fitted mean underflows --- \code{mu (1 - mu) n} for a
#' binomial and \code{mu} for a Poisson both do so for a curve that decays fast
#' enough. Two cases reach that zero and they need opposite treatment.
#'
#' Where the response equals the fitted value, the model is degenerate at that
#' observation and the data agree with it exactly. Both the observed and the
#' simulated term are \code{0/0}: the observation says nothing about dispersion
#' in either direction, so it is excluded from both sums and every draw is
#' retained. Discarding the whole statistic instead, as this function did
#' before #298, lost it precisely for the equations describing the data best,
#' since underflow requires a fast decay.
#'
#' Where the response differs from the fitted value, the model has assigned zero
#' variance to a value it did not predict. The infinity is produced by the
#' underflow, since in exact arithmetic the residual there is large and finite,
#' but the misfit it reports is real, so the observed term is kept and the
#' statistic is reported as \code{Inf}. Only the simulated term is excluded, because a
#' degenerate predictive distribution cannot deviate from its mean and is
#' \code{0/0} whichever case holds. A warning names the observations. It is a
#' warning rather than an error because \code{dispersion()} is called once per
#' equation from \code{expand_nec()}, so stopping would abandon construction of
#' a whole \code{\link{bayesmanecfit}} for one candidate whose shape is wrong.
#'
#' The classification of a zero denominator assumes that the variance and the
#' mean underflow together, which holds for the two families
#' \code{dispersion()} accepts: \code{1 - mu} is exactly 1 for any \code{mu}
#' below machine epsilon, so \code{mu (1 - mu) n} and \code{mu n} are the same
#' product. It does not hold for a family whose variance underflows first --- a
#' Gamma-style \code{mu^2 / shape} is zero at \code{mu = 1e-170} --- where a
#' response of zero would be classified as a disagreement and the statistic
#' reported as \code{Inf}. See the note on \code{allowed_fams} in
#' \code{dispersion()}.
#'
#' @return A \code{\link[base]{numeric}} vector with one element per draw.
#' \code{NA} for a draw in which no observation contributes a residual.
#'
#' @noRd
pearson_dispersion <- function(obs_y, prd_out, ppd_out, var_out,
                               labels = NULL, model_name = NULL) {
  who_short <- if (is.null(model_name)) "fitted model" else model_name
  who <- if (is.null(model_name)) {
    "The fitted mean"
  } else {
    paste0("The fitted ", model_name, " mean")
  }
  if (length(labels) != ncol(prd_out)) {
    labels <- as.character(seq_len(ncol(prd_out)))
  }
  obs_mat <- matrix(obs_y, nrow(prd_out), ncol(prd_out), byrow = TRUE)
  # A variance that is negative or not finite is neither of the two cases below
  # and no accepted family should produce one. It is excluded from both sums and
  # reported, rather than left to disappear into the na.rm below. The negative
  # test is needed as well as is.finite(): a negative variance is finite, and
  # sqrt() would otherwise turn it into a NaN term that na.rm drops in silence,
  # under base R's unattributed "NaNs produced" warning. Setting it missing
  # before the square root keeps that warning from being raised at all.
  unusable <- !is.finite(var_out) | var_out < 0
  var_out[unusable] <- NA_real_
  degenerate <- var_out == 0
  degenerate[unusable] <- FALSE
  disagreeing <- degenerate & obs_mat != prd_out
  denom <- sqrt(var_out)
  prd_sr <- ((obs_mat - prd_out) / denom)^2
  sim_sr <- ((ppd_out - prd_out) / denom)^2
  prd_sr[degenerate & !disagreeing] <- NA
  # Defensive rather than load-bearing: a predictive distribution with zero
  # variance is a point mass at its mean, so ppd_out equals prd_out and the
  # term is already 0/0 whichever of the two cases holds.
  sim_sr[degenerate] <- NA
  # na.rm drops the excluded terms rather than the draw. A draw in which every
  # term is excluded sums to 0/0, and the NaN that produces is replaced with NA
  # so that the returned vector reads as missing rather than as an arithmetic
  # accident; is.na() treats the two alike, so nothing downstream depends on
  # the substitution. estimates_summary() then excludes such a draw as it does
  # a censored ECx draw. A draw with a disagreeing observation divides Inf by
  # the sum of what is left and stays Inf, which is the verdict that case
  # earns.
  disp <- rowSums(prd_sr, na.rm = TRUE) / rowSums(sim_sr, na.rm = TRUE)
  disp[is.nan(disp)] <- NA_real_
  if (any(unusable)) {
    warning(who, "'s variance is negative or not finite at ",
            n_obs_phrase(sum(colSums(unusable) > 0)), " (",
            paste(labels[which(colSums(unusable) > 0)], collapse = ", "),
            "). Those observations are excluded from the dispersion ",
            "statistic.", call. = FALSE)
  }
  reproduced <- degenerate & !disagreeing
  excluded <- which(colSums(reproduced) > 0)
  if (length(excluded) > 0) {
    # The count of draws is reported alongside the count of observations
    # because the exclusion is elementwise and not a fixed set: an observation
    # whose fitted mean underflows in a steep draw gives an ordinary residual
    # in a shallower one. Naming only the observations would describe a set
    # that is dropped from the whole posterior, which is a different and more
    # damaging operation than the one performed. See #300.
    message(who, " has zero variance at ", length(excluded), " of ",
            ncol(prd_out), " observations (", paste(labels[excluded],
            collapse = ", "), ") which the model reproduces exactly, in ",
            sum(rowSums(reproduced) > 0), " of ", nrow(prd_out), " draws. ",
            "Such a term says nothing about dispersion in either direction, ",
            "and is excluded from both sums in the draws where it arises; ",
            "every draw is retained.")
  }
  n_missing <- sum(is.na(disp))
  if (n_missing > 0 && n_missing < length(disp)) {
    # The same account nsec_off_curve() gives of a censored draw: the draws
    # that return NA are named rather than left to be dropped by the na.rm in
    # estimates_summary() and in P(>1). See #39.
    message("No observation contributes a residual in ", n_missing, " of ",
            length(disp), " draws of the ", who_short, ". Those draws return ",
            "NA and are excluded from the summary, which is computed from the ",
            "remainder.")
  }
  infinite <- which(colSums(disagreeing) > 0)
  if (length(infinite) > 0) {
    warning(who, " has zero variance at ",
            n_obs_phrase(length(infinite)), " (",
            paste(labels[infinite], collapse = ", "),
            ") whose response is not the fitted value. The statistic is ",
            "reported as Inf. The infinity itself is the underflow --- in ",
            "exact arithmetic the Pearson residual there is large and finite ",
            "--- but the misfit it reports is real. For a converged binomial ",
            "or Poisson fit this is close to unreachable, because the ",
            "likelihood rejects a draw that assigns zero probability to an ",
            "observed value, so the first thing to check is that the response ",
            "and the fitted mean are on the same scale.", call. = FALSE)
  }
  disp
}

#' @noRd
n_obs_phrase <- function(n) {
  paste(n, if (n == 1) "observation" else "observations")
}
