## ----setup, include=FALSE--------------------------------------------------------------
options(prompt = 'R> ', continue = '+ ')
knitr::opts_chunk$set(purl = TRUE, warning = FALSE, message = FALSE, echo = TRUE, cache = TRUE, include = TRUE, eval = TRUE)


## ----install, results="hide", eval=FALSE-----------------------------------------------
## devtools::install_github("https://github.com/open-AIMS/bayesnec")
## 


## ----fit, results="hide"---------------------------------------------------------------
library(bayesnec)
data(nec_data)
exmp_fit <- bnec(data = nec_data, 
                 x_var = "x", 
                 y_var = "y", 
                 model = "decline")


## ----brms-plot, fig.height=7, fig.cap="Default \\pkg{brms} plot of the \\texttt{nec4param} model showing the posterior probability densities and chain mixing for each of the included parameters.", dependson="fit"----
plot(exmp_fit$mod_fits$nec4param$fit)


## ----summary, dependson="fit"----------------------------------------------------------
summary(exmp_fit)


## ----base-plot, fig.height=5, fig.cap="Base plot of the example fit model averaged curve. The solid black line is the fitted median of the posterior prediction, dashed black lines are the 95\\% credible intervals, and the red vertical lines show the estimated \\textit{NEC} value.", dependson="fit"----
plot(exmp_fit)


## ----sample-prior, fig.cap="Frequency histograms of samples of the default priors used by bnec for fitting the \\texttt{nec4param} model to the example data.", dependson="fit"----
sample_priors(exmp_fit$mod_fits$nec4param$fit$prior)


## ----check-priorsingle, fig.cap="A comparison of the prior and posterior parameter probability densities for the \\texttt{nec4param} model fit to the example data.", dependson="fit"----
exmp_fit_nec4param <- pull_out(exmp_fit, model = "nec4param")
check_priors(exmp_fit_nec4param)


## ----check-priorall, dependson="fit"---------------------------------------------------
check_priors(exmp_fit, filename = "Check_priors_plots")


## ----sessioninfo, eval=TRUE, include=FALSE---------------------------------------------
sessionInfo()

