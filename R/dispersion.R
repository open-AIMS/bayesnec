#' Posterior dispersion
#'
#' Calculates posterior dispersion metric
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
#' (mean, median, 95% highest density intervals) of the dispersion metric.
#'
#' @importFrom brms standata posterior_linpred posterior_epred posterior_predict
#' @importFrom stats median
#'
#' @references
#' Zuur, A. F., Hilbe, J. M., & Ieno, E. N. (2013). A Beginner's Guide to GLM
#' and GLMM with R: A Frequentist and Bayesian Perspective for Ecologists.
#' Highland Statistics Limited.
#'
#' @examples
#' \donttest{
#' library(bayesnec)
#' data(nec_data)
#' nec_data$y <- as.integer(round(nec_data$y * 100))
#' nec4param <- bnec(y ~ crf(x, "nec4param"), data = nec_data, chains = 2)
#' dispersion(nec4param, summary = TRUE)
#' }
#' @export
dispersion <- function(model, summary = FALSE, seed = 10) {
  formula <- model$bayesnecformula
  model <- model$fit
  mod_dat <- model.frame(formula, data = model$data)
  allowed_fams <- c("poisson", "binomial")
  fam <- model$family$family
  if (fam %in% allowed_fams) {
    fam_fcts <- get(fam)()
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
