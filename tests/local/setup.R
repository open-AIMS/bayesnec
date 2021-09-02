library(bayesnec)
library(dplyr)
library(testthat)
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

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
  muted_bnec(y ~ crf(x, model = c("nec4param", "ecx4param")), data = .,
             chains = 2)

message("\n# Beta\n")
manec_beta_logit <- nec_data %>%
  muted_bnec(y ~ crf(x, model = c("nec4param", "ecx4param")), data = .,
             chains = 2, family = Beta(link = "logit"))
manec_beta_identity <- nec_data %>%
  muted_bnec(y ~ crf(x, model = c("nec4param", "ecx4param")), data = .,
             chains = 2)

message("\n# Binomial\n")
manec_binomial_logit <- nec_data %>%
  mutate(trials = 10, y = as.integer(round(y * trials))) %>%
  muted_bnec(y | trials(trials) ~ crf(x, model = c("nec4param", "ecx4param")),
             data = ., chains = 2, family = binomial(link = "logit"))
manec_binomial_identity <- nec_data %>%
  mutate(trials = 10, y = as.integer(round(y * trials))) %>%
  muted_bnec(y | trials(trials) ~ crf(x, model = c("nec4param", "ecx4param")),
             data = ., chains = 2)

message("\n# Betabinomial\n")
manec_betabinomial <- nec_data %>%
  mutate(trials = 10, y = round(y * trials)) %>%
  muted_bnec(y | trials(trials) ~ crf(x, model = c("nec4param", "ecx4param")),
             data = ., chains = 2, family = "beta_binomial2")

message("\n# Poisson\n")
manec_poisson_log <- nec_data %>%
  mutate(y = as.integer(round(exp(y * 3)))) %>%
  muted_bnec(y ~ crf(x, model = c("nec4param", "ecx4param")), data = .,
             chains = 2, family = poisson(link = "log"))
manec_poisson_identity <- nec_data %>%
  mutate(y = as.integer(round(exp(y * 3)))) %>%
  muted_bnec(y ~ crf(x, model = c("nec4param", "ecx4param")), data =.,
             chains = 2)

message("\n# Negative binomial\n")
manec_negbinomial_log <- nec_data %>%
  mutate(y = as.integer(round(exp(y * 3)))) %>%
  muted_bnec(y ~ crf(x, model = c("nec4param", "ecx4param")), data = .,
             chains = 2, family = negbinomial(link = "log"))
manec_negbinomial_identity <- nec_data %>%
  mutate(y = as.integer(round(exp(y * 3)))) %>%
  muted_bnec(y ~ crf(x, model = c("nec4param", "ecx4param")), data = .,
             chains = 2, family = "negbinomial")

message("\n# Gamma\n")
manec_gamma_log <- nec_data %>%
  mutate(y = exp(y * 3)) %>%
  muted_bnec(y ~ crf(x, model = c("nec4param", "ecx4param")), data = .,
             chains = 2, family = Gamma(link = "log"))
manec_gamma_identity <- nec_data %>%
  mutate(y = exp(y * 3)) %>%
  muted_bnec(y ~ crf(x, model = c("nec4param", "ecx4param")), data = .,
             chains = 2)

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

nec_fits <- lapply(manec_fits, pull_out, model = "nec4param")

data("nec_data")
other_data <- nec_data
colnames(other_data) <- c("a", "b")
nec_data <- nec_data %>% 
  mutate(count = as.integer(round(y * 20)),
         trials = as.integer(20))
a <- nec_data$x
b <- nec_data$y
j <- nec_data$trials
