params <-
list(EVAL = FALSE)

## ----setup, include = FALSE---------------------------------------------------
stopifnot(require(knitr))
knitr::opts_chunk$set(
  warning = FALSE, message = FALSE,
  eval = ifelse(isTRUE(exists("params")), params$EVAL, FALSE)
)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("remotes")
#  remotes::install_github("open-AIMS/bayesnec")

## ---- results = "hide", eval = FALSE------------------------------------------
#  library(bayesnec)
#  data("nec_data")
#  
#  set.seed(333)
#  exp_5 <- bnec(data = nec_data, x_var = "x", y_var = "y", model = "all")

## ---- fig.width = 7, fig.height = 7, eval=FALSE-------------------------------
#  plot(exp_5)

## ----echo = FALSE, out.width = "100%"-----------------------------------------
#  knitr::include_graphics("example2a.jpeg")

## ---- eval=FALSE--------------------------------------------------------------
#  exp_5$mod_stats
#  #                     model      waic           wi dispersion_Estimate dispersion_Q2.5 dispersion_Q97.5
#  # nec4param       nec4param -332.7474 3.747495e-01                  NA              NA               NA
#  # nechorme4       nechorme4 -330.3145 1.275786e-01                  NA              NA               NA
#  # neclin             neclin -332.0511 2.903177e-01                  NA              NA               NA
#  # neclinhorme   neclinhorme -329.7996 1.448851e-01                  NA              NA               NA
#  # nechorme4pwr nechorme4pwr -328.3958 6.005012e-02                  NA              NA               NA
#  # ecxlin             ecxlin -188.0340 1.468296e-24                  NA              NA               NA
#  # ecx4param       ecx4param -302.3491 2.512591e-06                  NA              NA               NA
#  # ecxwb1             ecxwb1 -294.3010 7.113960e-08                  NA              NA               NA
#  # ecxwb2             ecxwb2 -317.1199 6.432912e-04                  NA              NA               NA
#  # ecxll5             ecxll5 -317.7844 1.723062e-03                  NA              NA               NA
#  # ecxll4             ecxll4 -302.9171 3.615674e-06                  NA              NA               NA
#  # ecxhormebc5   ecxhormebc5 -310.2534 4.644824e-05                  NA              NA               NA

## ---- eval=FALSE--------------------------------------------------------------
#  summary(exp_5)
#  # Object of class bayesmanecfit containing the following non-linear models:
#  #   -  nec4param
#  #   -  nechorme4
#  #   -  neclin
#  #   -  neclinhorme
#  #   -  nechorme4pwr
#  #   -  ecxlin
#  #   -  ecx4param
#  #   -  ecxwb1
#  #   -  ecxwb2
#  #   -  ecxll5
#  #   -  ecxll4
#  #   -  ecxhormebc5
#  #
#  # Distribution family: beta
#  # Number of posterior draws per model:  4000
#  #
#  # Model weights (Method: pseudobma_bb_weights):
#  #                 waic   wi
#  # nec4param    -332.75 0.37
#  # nechorme4    -330.31 0.13
#  # neclin       -332.05 0.29
#  # neclinhorme  -329.80 0.14
#  # nechorme4pwr -328.40 0.06
#  # ecxlin       -188.03 0.00
#  # ecx4param    -302.35 0.00
#  # ecxwb1       -294.30 0.00
#  # ecxwb2       -317.12 0.00
#  # ecxll5       -317.78 0.00
#  # ecxll4       -302.92 0.00
#  # ecxhormebc5  -310.25 0.00
#  #
#  #
#  # Summary of weighted NEC posterior estimates:
#  # NB: Model set contains the ECX models: ecxlin;ecx4param;ecxwb1;ecxwb2;ecxll5;ecxll4;ecxhormebc5; weighted NEC estimates include NSEC surrogates for NEC
#  #     Estimate Q2.5 Q97.5
#  # NEC     1.39 1.26  1.48
#  #
#  # Warning message:
#  # In print.manecsummary(x) :
#  #   The following model had Rhats > 1.05 (no convergence):
#  #   -  ecxll5
#  # Consider dropping them (see ?amend)
#  

## ---- fig.width = 7, fig.height = 7, eval=FALSE-------------------------------
#  exp_5_nec4param <- pull_out(exp_5, model = "nec4param")
#  plot(exp_5_nec4param)

## ----echo = FALSE, out.width = "100%"-----------------------------------------
#  knitr::include_graphics("example2b.jpeg")

## ---- fig.width = 7, fig.height = 10, eval=FALSE------------------------------
#  plot(exp_5, all_models = TRUE)

## ----echo = FALSE, out.width = "100%"-----------------------------------------
#  knitr::include_graphics("example2c.jpeg")

## ---- fig.width = 7, fig.height = 7, eval=FALSE-------------------------------
#  plot(exp_5$mod_fits$nec4param$fit)

## ----echo = FALSE, out.width = "100%"-----------------------------------------
#  knitr::include_graphics("example2d.jpeg")

## ---- fig.width = 7, fig.height = 7, eval=FALSE-------------------------------
#  check_chains(exp_5, filename = "example_5_all_chains")
#  #Chain plots saved to file example_5_all_chains.pdf, in your working directory.

## ---- fig.width = 7, fig.height = 7, eval=FALSE-------------------------------
#  check_priors(exp_5$mod_fits$nec4param)

## ----echo = FALSE, out.width = "100%"-----------------------------------------
#  knitr::include_graphics("example2f.jpeg")

## ---- fig.width = 7, fig.height = 7, eval=FALSE-------------------------------
#  check_priors(exp_5, filename = "example_5_all_priors")
#  #Probability density plots saved to file example_5_all_priors.pdf

## ---- eval=FALSE--------------------------------------------------------------
#  exp_5_new <- amend(exp_5, drop = rhat(exp_5)$failed)
#  #Fitted models are:  nec4param nechorme4 neclin neclinhorme nechorme4pwr ecxlin ecx4param ecxwb1 ecxwb2 ecxll4 ecxhormebc5

## ---- eval=FALSE--------------------------------------------------------------
#  exp_5_nec <- pull_out(exp_5, model = "nec")
#  # Model(s) nec3param, nechorme, necsigm, nechormepwr, nechormepwr01 non-existent in current set of models: nec4param, nechorme4, neclin, neclinhorme, nechorme4pwr, ecxlin, ecx4param, ecxwb1, ecxwb2, ecxll5, ecxll4, ecxhormebc5.
#  # If needed, add desired model(s) via function amend (see ?amend)
#  # Pulling out model(s): nec4param, nechorme4, neclin, neclinhorme, nechorme4pwr

## ---- eval=FALSE--------------------------------------------------------------
#  exp_5_nec <- amend(exp_5_nec, drop = "neclinhorme")
#  #Fitted models are:  nec4param nechorme4 neclin nechorme4pwr

## ---- eval=FALSE--------------------------------------------------------------
#  ECx10 <- ecx(exp_5, ecx_val = 10)
#  ECx50 <- ecx(exp_5, ecx_val = 50)
#  ECx10
#  #    ec_10 ec_10_lw ec_10_up
#  # 1.567394 1.503566 1.621647
#  ECx50
#  #    ec_50 ec_50_lw ec_50_up
#  # 2.004610 1.956740 2.052481

## ---- eval=FALSE--------------------------------------------------------------
#  NECvals <- exp_5_nec$w_nec
#  NECvals
#  # Estimate     Q2.5    Q97.5
#  # 1.382239 1.274046 1.466245

## ---- fig.width = 7, fig.height = 7, eval=FALSE-------------------------------
#  preds <- exp_5_nec$w_pred_vals$data
#  
#  par(mfrow=c(1,1))
#  plot(exp_5, add_nec = FALSE)
#  abline(v = ECx10, col = "orange", lty = c(1, 3, 3))
#  abline(v = ECx50, col = "blue", lty = c(1, 3, 3))
#  abline(v = NECvals, col = "darkgrey", lty = c(3, 1, 3))
#  lines(preds$x, preds$Estimate, col = "darkgrey")
#  lines(preds$x, preds$Q2.5, col = "darkgrey", lty = 3)
#  lines(preds$x, preds$Q97.5, col = "darkgrey", lty = 3)
#  legend("bottomleft",
#    legend = c("Complete averaged model", "ec10", "ec50", "NEC"),
#    col = c("black", "orange", "blue", "darkgrey"), lty = 1, bty = "n"
#  )

## ----echo = FALSE, out.width = "100%"-----------------------------------------
#  knitr::include_graphics("example2e.jpeg")

