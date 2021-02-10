library(bayesnec)
library(dplyr)
library(testthat)

suppress_bnec <- function(...) {
  bnec(...) %>%
    suppressWarnings %>%
    suppressMessages
}

logit <- function(x) {
  log(x / (1 - x))
}

manec_gausian_identity <- nec_data %>%
  mutate(y = logit(y)) %>%
  suppress_bnec("x", "y", model = c("nec4param", "ecx4param"),
                iter = 50, chains = 2)
nec_gausian_identity <- pull_out(manec_gausian_identity, "nec4param")
