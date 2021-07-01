#' Posterior dispersion
#'
#' Calculates posterior dispersion metric
#'
#' @param model An object of class \code{\link[brms]{brmsfit}} whose
#' distribution family is either \code{\link[stats]{gaussian}},
#' \code{\link[stats]{poisson}} or \code{\link[stats]{binomial}}.
#' @param summary Logical. Should summary stats be returned instead of full
#' vector? Defaults to FALSE.
#'
#' @details This function calculates a dispersion metric which takes the ratio
#' between the observed relative to simulated Pearson residuals sums of
#' squares.
#'
#' @return If \code{summary} is FALSE, an n-long \code{\link[base]{numeric}}
#' vector containing the dispersion metric, where n is the number of post
#' warm-up posterior draws from the \code{\link[brms]{brmsfit}} object. If
#' TRUE, then a \code{\link[base]{data.frame}} containing the summary stats
#' (mean, median, 95% highest density intervals). of the dispersion metric.
#'
#' @importFrom brms standata posterior_linpred posterior_epred posterior_predict
#' @importFrom stats median
#' @examples
#' library(bayesnec)
#' data(manec_example)
#' exmp_brmsfit <- manec_example$mod_fits$nec4param$fit
#' dispersion(exmp_brmsfit, summary = TRUE)
#' 
#' @export
dispersion <- function(model, summary = FALSE) {
  allowed_fams <- c("gaussian", "binomial", "poisson")
  fam <- model$family$family
  if (fam %in% c(allowed_fams)) {
   fam_fcts <- get(fam)()
    obs_y <- standata(model)$Y
    lpd_out <- posterior_linpred(model)
    prd_out <- posterior_epred(model)
    ppd_out <- posterior_predict(model)
    prd_sr <- matrix(0, nrow(prd_out), ncol(prd_out))
    sim_sr <- matrix(0, nrow(prd_out), ncol(prd_out))
    for (i in seq_len(nrow(prd_out))) {
      prd_y <- prd_out[i, ]
      prd_mu <- fam_fcts$linkinv(lpd_out[i, ])
      prd_var_y <- fam_fcts$variance(prd_mu)
      prd_res <- (obs_y - prd_y) / sqrt(prd_var_y)
      sim_y <- ppd_out[i, ]
      sim_res <- (sim_y - prd_y) / sqrt(prd_var_y)
      prd_sr[i, ] <- prd_res^2
      sim_sr[i, ] <- sim_res^2
    }
    disp <- rowSums(prd_sr) / rowSums(sim_sr)
    if (summary) {
      estimates_summary(disp)
    } else {
      disp
    }
  } else {
    NULL
  }
}
