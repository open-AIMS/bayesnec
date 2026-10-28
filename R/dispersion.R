#' Posterior dispersion
#'
#' Calculates a posterior dispersion metric.
#'
#' @param model An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} whose distribution family is either
#' \code{\link[stats]{poisson}} or \code{\link[stats]{binomial}}.
#' @param summary Logical. Should summary stats be returned instead of full
#' vector? Defaults to FALSE.
#' @param seed A \code{\link[base]{numeric}} vector of length 1. Passed to
#' \code{\link[base]{set.seed}} before simulating, so the result is
#' reproducible. The caller's random number stream is restored before the
#' statistic is returned, so running it does not change what the next random
#' operation in the session returns. \code{NULL} is refused rather than passed
#' on, because \code{set.seed(NULL)} re-initialises the stream from the clock.
#'
#' @details This function calculates a dispersion metric which takes the ratio
#' between the observed relative to simulated Pearson residuals sums of
#' squares.
#'
#' @return For a \code{\link{bayesnecfit}}, a \code{\link[base]{numeric}}
#' vector. If \code{summary} is FALSE, it contains the dispersion metric for
#' each post-warm-up posterior draw from the \code{\link[brms]{brmsfit}}
#' object. An element is \code{Inf} for a draw containing an observation of
#' zero fitted variance whose response is not the fitted value, and \code{NA}
#' for a draw in which no observation contributes a residual; both cases are
#' described under Details and reported when they arise. If \code{summary} is
#' TRUE, the vector contains the median, 95% credible interval, and posterior
#' probability of over-dispersion. For a \code{\link{bayesmanecfit}},
#' \code{summary = FALSE} returns a named list of these vectors and
#' \code{summary = TRUE} returns a data frame with one row per equation.
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
  chk_lgl(summary)
  chk_number(seed)
  # Not an S3 generic, so a hurdle fit has to be handled here. It has two
  # underlying brmsfits and no combined analogue, so one result per component.
  if (is_bayesnechurdlefit(model)) {
    return(list(
      growth = dispersion(model$growth, summary = summary, seed = seed),
      survival = dispersion(model$survival, summary = summary, seed = seed)
    ))
  }
  # A model set has no single dispersion statistic, so return one per stored
  # fit. An empty result becomes an NA row, as in expand_nec() and summary().
  if (is_bayesmanecfit(model)) {
    out <- lapply(model$mod_fits, dispersion, summary = summary, seed = seed)
    if (!summary) {
      return(out)
    }
    summary_names <- c("Estimate", "Q2.5", "Q97.5", "P(>1)")
    out <- lapply(out, function(x) {
      if (length(x) == 0) {
        stats::setNames(rep(NA_real_, length(summary_names)), summary_names)
      } else {
        x
      }
    })
    return(as.data.frame(do.call(rbind, out), check.names = FALSE))
  }
  # Keep the equation's name for the messages, since expand_nec() calls this
  # once per equation.
  mod_name <- model$model
  mod_label <- if (is.null(mod_name)) "fitted" else mod_name
  formula <- model$bayesnecformula
  factorised_count <- is_factorised_count_formula(formula, model$fit$family)
  model <- model$fit
  mod_dat <- model.frame(formula, data = model$data)
  allowed_fams <- c("poisson", "binomial")
  fam <- model$family$family
  if (fam %in% allowed_fams) {
    # Take the link from the fit, not the family default (log for poisson,
    # logit for binomial). bnec() fits on the identity link, so
    # posterior_linpred() is already on the response scale and the default
    # linkinv() would transform it a second time.
    fam_fcts <- get(fam)(link = model$family$link)
    obs_y <- standata(model)$Y
    lpd_out <- posterior_linpred(model)
    prd_out <- factorised_count_epred(model, formula)
    # Leave the caller's random number stream where it was. summary() reaches
    # this for every equation of a model set.
    with_preserved_rng_state({
      set.seed(seed)
      ppd_out <- posterior_predict(model)
    })
    # Collect the fitted variances first: pearson_dispersion() treats an
    # observation with zero variance differently depending on whether the
    # response equals the fitted value.
    var_out <- matrix(0, nrow(prd_out), ncol(prd_out))
    for (i in seq_len(nrow(prd_out))) {
      prd_mu <- fam_fcts$linkinv(lpd_out[i, ])
      prd_var_y <- if (factorised_count && fam == "poisson") {
        hurdle_positive_variance(prd_mu, fam)
      } else {
        fam_fcts$variance(prd_mu)
      }
      if (fam == "binomial") {
        trials_var <- attr(mod_dat, "bnec_pop")[["trials_var"]]
        prd_var_y <- prd_var_y * model$data[[trials_var]]
      }
      rate_var <- unname(attr(mod_dat, "bnec_pop")["rate_var"])
      if (fam == "poisson" && !is.na(rate_var)) {
        # Exact for Poisson: prd_mu is the rate and Var(count) = mu * denom.
        # It does not carry over to negbinomial, whose shape brms also scales
        # by the denominator; derive that case before adding negbinomial to
        # allowed_fams.
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
        # Add P(dispersion > 1) to the median and interval. It uses the whole
        # posterior, and 1 - p answers the under-dispersion question.
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
#' @details \code{sqrt(var_out)} is exactly zero wherever the fitted mean
#' underflows, as for a fast-decaying binomial or Poisson curve. Where the
#' response equals the fitted value, both terms are \code{0/0} and the
#' observation is excluded from both sums. Where it differs, the misfit is
#' real: the observed term is kept and the statistic is \code{Inf}, with a
#' warning rather than an error, since \code{expand_nec()} calls
#' \code{dispersion()} for every equation of a set. The simulated term is
#' \code{0/0} in both cases and always excluded.
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
  # A negative or non-finite variance should not arise. Exclude it from both
  # sums and report it. Set it NA before sqrt(), which would otherwise turn a
  # negative value into a NaN that na.rm drops in silence.
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
  # na.rm drops the excluded terms, not the draw. A draw with every term
  # excluded gives 0/0, returned as NA. A draw with a disagreeing observation
  # stays Inf.
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
    # Report the number of draws as well as observations: an observation is
    # excluded only in the draws where its fitted mean underflows, not from
    # the whole posterior.
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
    # Name the NA draws rather than leave estimates_summary() and P(>1) to
    # drop them silently, as nsec_off_curve() does for a censored draw.
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
