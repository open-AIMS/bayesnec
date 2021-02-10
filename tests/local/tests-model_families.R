source("setup_local_tests.R")

test_that("gaussian model with identity works correctly", ){
  manec_gausian_identity <- nec_data %>%
  mutate(y = logit(y)) %>%
  suppress_bnec("x", "y", model = c("nec4param", "ecx4param"),
                iter = 50, chains = 2)
} 


# # beta
# manec_beta_logit <- nec_data %>%
#   suppress_bnec("x", "y", model = c("nec4param", "ecx4param"),
#                 iter = 50, chains = 2)
# manec_beta_identity <- nec_data %>%
#   suppress_bnec("x", "y", model = c("nec4param", "ecx4param"),
#                 iter = 50, chains = 2, family = Beta(link = "identity"))
# # binomial
# manec_binomial_logit <- nec_data %>%
#   mutate(trials = 10, y = as.integer(round(y * trials))) %>%
#   suppress_bnec("x", "y", trials_var = "trials",
#                 model = c("nec4param", "ecx4param"), iter = 50, chains = 2)
# manec_binomial_identity <- nec_data %>%
#   mutate(trials = 10, y = round(y * trials)) %>%
#   suppress_bnec("x", "y", trials_var = "trials",
#                 model = c("nec4param", "ecx4param"), iter = 50, chains = 2,
#                 family = binomial(link = "identity"))
# # betabinomial
# manec_betabinomial <- nec_data %>%
#   mutate(trials = 10, y = round(y * trials)) %>%
#   suppress_bnec("x", "y", trials_var = "trials",
#                 model = c("nec4param", "ecx4param"), iter = 50, chains = 2,
#                 family = "beta_binomial2")
# # poisson
# manec_poisson_log <- nec_data %>%
#   mutate(y = as.integer(round(exp(y * 3)))) %>%
#   suppress_bnec("x", "y", model = c("nec4param", "ecx4param"),
#                 iter = 50, chains = 2)
# manec_poisson_identity <- nec_data %>%
#   mutate(y = as.integer(round(exp(y * 3)))) %>%
#   suppress_bnec("x", "y", model = c("nec4param", "ecx4param"),
#                 iter = 50, chains = 2, family = poisson(link = "identity"))
# # negative binomial
# manec_negbinomial_log <- nec_data %>%
#   mutate(y = as.integer(round(exp(y * 3)))) %>%
#   suppress_bnec("x", "y", model = c("nec4param", "ecx4param"), iter = 50,
#                 chains = 2, family = negbinomial)
# manec_negbinomial_identity <- nec_data %>%
#   mutate(y = as.integer(round(exp(y * 3)))) %>%
#   suppress_bnec("x", "y", model = c("nec4param", "ecx4param"), iter = 50,
#                 chains = 2, family = negbinomial(link = "identity"))
# # gamma
# manec_gamma_log <- nec_data %>%
#   mutate(y = exp(y * 3)) %>%
#   suppress_bnec("x", "y", model = c("nec4param", "ecx4param"),
#                 iter = 50, chains = 2)
# manec_gamma_identity <- nec_data %>%
#   mutate(y = exp(y * 3)) %>%
#   suppress_bnec("x", "y", model = c("nec4param", "ecx4param"), iter = 50,
#                 chains = 2, family = Gamma(link = "identity"))
# }
