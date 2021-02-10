library(brms)
require(bayesnec)
require(tidyverse)

set.seed(50)
##########
#TEST FITS
##########
logit <- function(p) {
  log(p / (1 - p))
}
# gaussian
manec_gausian_identity <- nec_data %>%
  mutate(y = logit(y)) %>%
  bnec("x", "y", model = c("nec4param", "ecx4param"),
       iter = 50, chains = 2)
# beta
manec_beta_logit <- nec_data %>%
  bnec("x", "y", model = c("nec4param", "ecx4param"),
       iter = 50, chains = 2)
manec_beta_identity <- nec_data %>%
  bnec("x", "y", model = c("nec4param", "ecx4param"),
       iter = 50, chains = 2, family = Beta(link = "identity"))
# binomial
manec_binomial_logit <- nec_data %>%
  mutate(trials = 10, y = as.integer(round(y * trials))) %>%
  bnec("x", "y", trials_var = "trials",
       model = c("nec4param", "ecx4param"), iter = 50, chains = 2)
manec_binomial_identity <- nec_data %>%
  mutate(trials = 10, y = round(y * trials)) %>%
  bnec("x", "y", trials_var = "trials",
       model = c("nec4param", "ecx4param"), iter = 50, chains = 2,
       family = binomial(link = "identity"))
# betabinomial
manec_betabinomial <- nec_data %>%
  mutate(trials = 10, y = round(y * trials)) %>%
  bnec("x", "y", trials_var = "trials",
       model = c("nec4param", "ecx4param"), iter = 50, chains = 2,
       family = "beta_binomial2")
# poisson
manec_poisson_log <- nec_data %>%
  mutate(y = as.integer(round(exp(y * 3)))) %>%
  bnec("x", "y", model = c("nec4param", "ecx4param"),
       iter = 50, chains = 2)
manec_poisson_identity <- nec_data %>%
  mutate(y = as.integer(round(exp(y * 3)))) %>%
  bnec("x", "y", model = c("nec4param", "ecx4param"),
       iter = 50, chains = 2, family = poisson(link = "identity"))
# negative binomial
manec_negbinomial_log <- nec_data %>%
  mutate(y = as.integer(round(exp(y * 3)))) %>%
  bnec("x", "y", model = c("nec4param", "ecx4param"), iter = 50,
       chains = 2, family = negbinomial)
manec_negbinomial_identity <- nec_data %>%
  mutate(y = as.integer(round(exp(y * 3)))) %>%
  bnec("x", "y", model = c("nec4param", "ecx4param"), iter = 50,
       chains = 2, family = negbinomial(link = "identity"))
# gamma
manec_gamma_log <- nec_data %>%
  mutate(y = exp(y * 3)) %>%
  bnec("x", "y", model = c("nec4param", "ecx4param"),
       iter = 50, chains = 2)
manec_gamma_identity <- nec_data %>%
  mutate(y = exp(y * 3)) %>%
  bnec("x", "y", model = c("nec4param", "ecx4param"), iter = 50,
       chains = 2, family = Gamma(link = "identity"))
# collate output
manec_fits <- list(manec_gausian_identity = manec_gausian_identity,
                   manec_beta_logit = manec_beta_logit,
                   manec_beta_identity = manec_beta_identity,
                   manec_betabinomial = manec_betabinomial,
                   manec_poisson_log = manec_poisson_log,
                   manec_poisson_identity = manec_poisson_identity,
                   manec_negbinomial_log = manec_negbinomial_log,
                   manec_negbinomial_identity = manec_negbinomial_identity,
                   manec_gamma_log = manec_gamma_log,
                   manec_gamma_identity = manec_gamma_identity)

usethis::use_data(
  manec_fits,
  internal = TRUE, overwrite = TRUE
)
load("R/sysdata.rda")
