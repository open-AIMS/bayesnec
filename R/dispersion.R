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
#' warm-up posterior draws from the \code{\link[brms]{brmsfit}} object. If
#' TRUE, then a \code{\link[base]{data.frame}} containing the summary stats
#' (median, 95% credible interval, and the posterior probability of
#' over-dispersion) of the dispersion metric.
#'
#' @details The statistic is the ratio of the observed to the simulated Pearson
#' residual sum of squares, whose null value is 1. With \code{summary = TRUE}
#' the returned vector carries \code{P(>1)}, the posterior probability that the
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
#' excluded from both and every draw is retained. The number of observations
#' excluded is reported. Where instead the response differs from a fitted value
#' of zero variance, the model has assigned zero variance to a value it did not
#' predict; the Pearson residual is infinite, the statistic is reported as
#' \code{Inf}, and a warning names the observations. An empty vector is
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
  if (is.null(mod_name)) {
    mod_name <- "fitted"
  }
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
        # rather than copying this line. See #136.
        prd_var_y <- prd_var_y * model$data[[rate_var]]
      }
      var_out[i, ] <- prd_var_y
    }
    disp <- pearson_dispersion(obs_y, prd_out, ppd_out, var_out,
                               labels = rownames(model$data),
                               model_name = mod_name)
    if (all(is.na(disp))) {
      message("Every observation has a fitted variance of zero and is",
              " reproduced exactly by the ", mod_name, " model, so there are",
              " no residuals to compare. The dispersion statistic is not",
              " defined for this fit.")
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
#' variance to a value it did not predict. That is a severe misfit rather than a
#' numerical artefact, so its infinite observed term is kept and the statistic
#' is reported as \code{Inf}. Only the simulated term is excluded, because a
#' degenerate predictive distribution cannot deviate from its mean and is
#' \code{0/0} whichever case holds. A warning names the observations. It is a
#' warning rather than an error because \code{dispersion()} is called once per
#' equation from \code{expand_nec()}, so stopping would abandon construction of
#' a whole \code{\link{bayesmanecfit}} for one candidate whose shape is wrong.
#'
#' @return A \code{\link[base]{numeric}} vector with one element per draw.
#' \code{NA} for a draw in which every observation is degenerate and agreeing.
#'
#' @noRd
pearson_dispersion <- function(obs_y, prd_out, ppd_out, var_out,
                               labels = NULL, model_name = NULL) {
  who <- if (is.null(model_name)) {
    "The fitted mean"
  } else {
    paste0("The fitted ", model_name, " mean")
  }
  if (length(labels) != ncol(prd_out)) {
    labels <- as.character(seq_len(ncol(prd_out)))
  }
  denom <- sqrt(var_out)
  obs_mat <- matrix(obs_y, nrow(prd_out), ncol(prd_out), byrow = TRUE)
  prd_sr <- ((obs_mat - prd_out) / denom)^2
  sim_sr <- ((ppd_out - prd_out) / denom)^2
  # A variance that is not finite is neither of the two cases below and no
  # accepted family should produce one. It is excluded from both sums and
  # reported, rather than left to disappear into the na.rm below.
  unusable <- !is.finite(var_out)
  degenerate <- var_out == 0
  degenerate[unusable] <- FALSE
  disagreeing <- degenerate & obs_mat != prd_out
  prd_sr[unusable | (degenerate & !disagreeing)] <- NA
  sim_sr[unusable | degenerate] <- NA
  # na.rm drops the excluded terms rather than the draw. A draw in which every
  # observation is excluded and none of them disagrees sums to 0/0, and is
  # returned as NA so that estimates_summary() excludes it as it does a
  # censored ECx draw. Where such a draw does have a disagreeing observation
  # the observed sum is Inf and the ratio stays Inf, which is the verdict that
  # case earns.
  n_used <- rowSums(!degenerate & !unusable)
  disp <- rowSums(prd_sr, na.rm = TRUE) / rowSums(sim_sr, na.rm = TRUE)
  disp[n_used == 0 & rowSums(disagreeing) == 0] <- NA_real_
  if (any(unusable)) {
    warning(who, "'s variance is not finite at ",
            n_obs_phrase(sum(colSums(unusable) > 0)), " (",
            paste(labels[which(colSums(unusable) > 0)], collapse = ", "),
            "). Those observations are excluded from the dispersion ",
            "statistic.", call. = FALSE)
  }
  excluded <- which(colSums(degenerate & !disagreeing) > 0)
  if (length(excluded) > 0) {
    message(who, " has zero variance at ", length(excluded), " of ",
            ncol(prd_out), " observations (", paste(labels[excluded],
            collapse = ", "), ") which the model reproduces exactly. Those ",
            "observations carry no information about dispersion and are ",
            "excluded from the statistic; all draws are retained.")
  }
  infinite <- which(colSums(disagreeing) > 0)
  if (length(infinite) > 0) {
    warning(who, " has zero variance at ",
            n_obs_phrase(length(infinite)), " (",
            paste(labels[infinite], collapse = ", "),
            ") whose response is not the fitted value. The Pearson residual ",
            "is infinite there and the dispersion statistic is reported as ",
            "Inf. This is a property of the fit rather than a numerical ",
            "limit: the model has assigned zero variance to a value it did ",
            "not predict.", call. = FALSE)
  }
  disp
}

#' @noRd
n_obs_phrase <- function(n) {
  paste(n, if (n == 1) "observation" else "observations")
}
