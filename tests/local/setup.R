library(bayesnec)
library(dplyr)
library(testthat)

muted_bnec <- function(...) {
  bnec(...) %>%
    suppressWarnings %>%
    suppressMessages
}

logit <- function(x) {
  log(x / (1 - x))
}

expect_range <- function(object, lower = -Inf, upper = Inf, ...) {
  expect_true(all(object >= lower & object <= upper), ...)
}

message("\n\n\nTemporarily caching models for local tests\n\n\n")

message("\n# Gaussian\n")
manec_gausian_identity <- nec_data %>%
  mutate(y = logit(y)) %>%
  muted_bnec("x", "y", model = c("nec4param", "ecx4param"), chains = 2)

# message("\n# Beta\n")
# manec_beta_logit <- nec_data %>%
#   muted_bnec("x", "y", model = c("nec4param", "ecx4param"),
#              iter = 50, chains = 2)
# manec_beta_identity <- nec_data %>%
#   muted_bnec("x", "y", model = c("nec4param", "ecx4param"),
#              iter = 50, chains = 2, family = Beta(link = "identity"))
# message("\n# Binomial\n")
# manec_binomial_logit <- nec_data %>%
#   mutate(trials = 10, y = as.integer(round(y * trials))) %>%
#   muted_bnec("x", "y", trials_var = "trials",
#              model = c("nec4param", "ecx4param"), iter = 50, chains = 2)
# manec_binomial_identity <- nec_data %>%
#   mutate(trials = 10, y = round(y * trials)) %>%
#   muted_bnec("x", "y", trials_var = "trials",
#              model = c("nec4param", "ecx4param"), iter = 50, chains = 2,
#              family = binomial(link = "identity"))
# message("\n# Betabinomial\n")
# manec_betabinomial <- nec_data %>%
#   mutate(trials = 10, y = round(y * trials)) %>%
#   muted_bnec("x", "y", trials_var = "trials",
#              model = c("nec4param", "ecx4param"), iter = 50, chains = 2,
#              family = "beta_binomial2")
# message("\n# Poisson\n")
# manec_poisson_log <- nec_data %>%
#   mutate(y = as.integer(round(exp(y * 3)))) %>%
#   muted_bnec("x", "y", model = c("nec4param", "ecx4param"),
#              iter = 50, chains = 2)
# manec_poisson_identity <- nec_data %>%
#   mutate(y = as.integer(round(exp(y * 3)))) %>%
#   muted_bnec("x", "y", model = c("nec4param", "ecx4param"),
#              iter = 50, chains = 2, family = poisson(link = "identity"))
# message("\n# Negative binomial\n")
# manec_negbinomial_log <- nec_data %>%
#   mutate(y = as.integer(round(exp(y * 3)))) %>%
#   muted_bnec("x", "y", model = c("nec4param", "ecx4param"), iter = 50,
#              chains = 2, family = negbinomial)
# manec_negbinomial_identity <- nec_data %>%
#   mutate(y = as.integer(round(exp(y * 3)))) %>%
#   muted_bnec("x", "y", model = c("nec4param", "ecx4param"), iter = 50,
#              chains = 2, family = negbinomial(link = "identity"))
# message("\n# Gamma\n")
# manec_gamma_log <- nec_data %>%
#   mutate(y = exp(y * 3)) %>%
#   muted_bnec("x", "y", model = c("nec4param", "ecx4param"),
#              iter = 50, chains = 2)
# manec_gamma_identity <- nec_data %>%
#   mutate(y = exp(y * 3)) %>%
#   muted_bnec("x", "y", model = c("nec4param", "ecx4param"), iter = 50,
#              chains = 2, family = Gamma(link = "identity"))
