params <-
list(EVAL = FALSE)

## ----setup, include = FALSE---------------------------------------------------
stopifnot(require(knitr))
knitr::opts_chunk$set(
  warning = FALSE, message = FALSE,
  eval = ifelse(isTRUE(exists("params")), params$EVAL, FALSE)
)
library(bayesnec)

## ----echo = FALSE, warning = FALSE, message = FALSE, results = "hide"---------
#  plat <- Sys.info()["sysname"]
#  if (plat == "Windows") {
#    library(rstan)
#    example(stan_model, run.dontrun = TRUE, verbose = TRUE)
#  }

## ---- eval = FALSE------------------------------------------------------------
#  library(bayesnec)
#  models()

## ---- echo = FALSE------------------------------------------------------------
#  show_params("ecxlin")

## ---- echo = FALSE------------------------------------------------------------
#  show_params("ecxexp")

## ---- echo = FALSE------------------------------------------------------------
#  show_params("ecxsigm")

## ---- echo = FALSE------------------------------------------------------------
#  show_params("ecx4param")

## ---- echo = FALSE------------------------------------------------------------
#  show_params("ecxwb1")

## ---- echo = FALSE------------------------------------------------------------
#  show_params("ecxwb1p3")

## ---- echo = FALSE------------------------------------------------------------
#  show_params("ecxwb2")

## ---- echo = FALSE------------------------------------------------------------
#  show_params("ecxwb2p3")

## ---- echo = FALSE------------------------------------------------------------
#  show_params("ecxll5")

## ---- echo = FALSE------------------------------------------------------------
#  show_params("ecxll4")

## ---- echo = FALSE------------------------------------------------------------
#  show_params("ecxll3")

## ---- echo = FALSE------------------------------------------------------------
#  show_params("ecxhormebc5")

## ---- echo = FALSE------------------------------------------------------------
#  show_params("ecxhormebc4")

## ---- echo = FALSE------------------------------------------------------------
#  show_params("neclin")

## ---- echo = FALSE------------------------------------------------------------
#  show_params("nec3param")

## ---- echo = FALSE------------------------------------------------------------
#  show_params("nec4param")

## ---- echo = FALSE------------------------------------------------------------
#  show_params("nechorme")

## ---- echo = FALSE------------------------------------------------------------
#  show_params("nechormepwr")

## ---- echo = FALSE------------------------------------------------------------
#  show_params("neclinhorme")

## ---- echo = FALSE------------------------------------------------------------
#  show_params("nechorme4")

## ---- echo = FALSE------------------------------------------------------------
#  show_params("nechorme4pwr")

## ---- echo = FALSE------------------------------------------------------------
#  show_params("nechormepwr01")

## ---- echo = FALSE------------------------------------------------------------
#  show_params("necsigm")

