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
#' TRUE, a named vector of length 3 --- \code{Estimate}, \code{Q2.5} and
#' \code{Q97.5} --- holding the median of the dispersion metric and an
#' equal-tailed 95% interval. Returns an empty vector for any family other than
#' \code{poisson} or \code{binomial}, whose variance is fixed by the mean: a
#' family carrying a free dispersion parameter poses no over-dispersion
#' question.
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
  formula <- model$bayesnecformula
  model <- model$fit
  mod_dat <- model.frame(formula, data = model$data)
  allowed_fams <- c("poisson", "binomial")
  fam <- model$family$family
  if (fam %in% allowed_fams) {
    # The link is taken from the fit rather than left at the family default.
    # get("poisson")() is a log link and get("binomial")() a logit one, but
    # bnec() forces link = "identity", so posterior_linpred() below is already
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
    prd_sr <- matrix(0, nrow(prd_out), ncol(prd_out))
    sim_sr <- matrix(0, nrow(prd_out), ncol(prd_out))
    for (i in seq_len(nrow(prd_out))) {
      prd_y <- prd_out[i, ]
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
      prd_res <- (obs_y - prd_y) / sqrt(prd_var_y)
      sim_y <- ppd_out[i, ]
      sim_res <- (sim_y - prd_y) / sqrt(prd_var_y)
      prd_sr[i, ] <- prd_res^2
      sim_sr[i, ] <- sim_res^2
    }
    disp <- rowSums(prd_sr) / rowSums(sim_sr)
    if (any(is.na(disp))) {
      message("Your model predictions have generated no residuals; this is",
              " most likely cause by a bad model fit. Ignoring dispersion",
              " calculation.")
      numeric()
    } else {
      if (summary) {
        estimates_summary(disp)
      } else {
        disp
      }      
    }
  } else {
    numeric()
  }
}
