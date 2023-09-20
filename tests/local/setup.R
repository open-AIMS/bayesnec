library(bayesnec)
library(dplyr)
library(testthat)
options(mc.cores = 1)

muted_bnec <- function(...) {
  bnec(... , refresh = 0, silent = 2) |>
    suppressWarnings() |>
    suppressMessages()
}

retry_mb <- function(..., n_mod = 2) {
  out <- muted_bnec(...)
  n_ <- 1
  while (n_ <= 10 && n_mod == 2 &&
          (inherits(out, "try-error") || inherits(out, "bayesnecfit"))) {
    message("\n\n\nTwo models should have worked but only one returned, ",
           "retrying... (attempt #", n_, ")\n\n\n")
    out <- muted_bnec(..., seed = n_)
    n_ <- n_ + 1
    if (n_ > 10) {
      stop("10 failed attempts...")
    }
  }
  n_ <- 1
  while (n_ <= 10 && n_mod == 1 && inherits(out, "try-error")) {
    message("\n\n\nOne model should have worked but none returned, ",
           "retrying... (attempt #", n_, ")\n\n\n")
    out <- muted_bnec(..., seed = n_)
    n_ <- n_ + 1
    if (n_ > 10) {
      stop("10 failed attempts...")
    }
  }
  out
}

logit <- function(x) {
  log(x / (1 - x))
}

expect_range <- function(object, lower = -Inf, upper = Inf, ...) {
  expect_true(all(object >= lower & object <= upper), ...)
}

message("\n\n\nTemporarily caching models for local tests\n\n\n")

message("\n# Gaussian\n")
manec_gausian_identity <- nec_data |>
  dplyr::mutate(y = logit(y)) |>
  (\(x)retry_mb(y ~ crf(x, model = c("nec4param", "ecx4param")), data = x,
                chains = 2))()

message("\n# Beta\n")
manec_beta_logit <- nec_data |>
  (\(x)retry_mb(y ~ crf(x, model = c("nec4param", "ecx4param")), data = x,
                chains = 2, family = Beta(link = "logit")))()
manec_beta_identity <- nec_data |>
  (\(x)retry_mb(y ~ crf(x, model = c("nec4param", "ecx4param")), data = x,
                chains = 2))()

message("\n# Binomial\n")
manec_binomial_logit <- nec_data |>
  dplyr::mutate(trials = 10, y = as.integer(round(y * trials))) |>
  (\(x)retry_mb(y | trials(trials) ~
                  crf(x, model = c("nec4param", "ecx4param")),
                data = x, chains = 2, family = binomial(link = "logit")))()
manec_binomial_identity <- nec_data |>
  dplyr::mutate(trials = 10, y = as.integer(round(y * trials))) |>
  (\(x)retry_mb(y | trials(trials) ~
                  crf(x, model = c("nec4param", "ecx4param")),
                data = x, chains = 2))()

message("\n# Betabinomial\n")
manec_betabinomial <- nec_data |>
  dplyr::mutate(trials = 10, y = round(y * trials)) |>
  (\(x)retry_mb(y | trials(trials) ~
                  crf(x, model = c("nec4param", "ecx4param")),
                data = x, chains = 2, family = "beta_binomial"))()

message("\n# Poisson\n")
manec_poisson_log <- nec_data |>
  dplyr::mutate(y = as.integer(round(exp(y * 3)))) |>
  (\(x)retry_mb(y ~ crf(x, model = c("nec4param", "ecx4param")), data = x,
                chains = 2, family = poisson(link = "log")))()
manec_poisson_identity <- nec_data |>
  dplyr::mutate(y = as.integer(round(exp(y * 3)))) |>
  (\(x)retry_mb(y ~ crf(x, model = c("nec4param", "ecx4param")), data = x,
                chains = 2))()

message("\n# Negative binomial\n")
manec_negbinomial_log <- nec_data |>
  dplyr::mutate(y = as.integer(round(exp(y * 3)))) |>
  (\(x)retry_mb(y ~ crf(x, model = c("nec4param", "ecx4param")), data = x,
                chains = 2, family = negbinomial(link = "log")))()
manec_negbinomial_identity <- nec_data |>
  dplyr::mutate(y = as.integer(round(exp(y * 3)))) |>
  (\(x)retry_mb(y ~ crf(x, model = c("nec4param", "ecx4param")), data = x,
                chains = 2, family = "negbinomial"))()

message("\n# Gamma\n")
manec_gamma_log <- nec_data |>
  dplyr::mutate(y = exp(y * 3)) |>
  (\(x)retry_mb(y ~ crf(x, model = c("nec4param", "ecx4param")), data = x,
                chains = 2, family = Gamma(link = "log")))()
manec_gamma_identity <- nec_data |>
  dplyr::mutate(y = exp(y * 3)) |>
  (\(x)retry_mb(y ~ crf(x, model = c("nec4param", "ecx4param")), data = x,
                chains = 2))()

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

mod_1 <- nec_data |>
  dplyr::mutate(y = qlogis(y)) |>
  (\(x)retry_mb(formula = y ~ crf(x, model = "nec4param"),
                data = x, chains = 2, n_mod = 1))()
mod_2 <- nec_data |>
  dplyr::mutate(y = qlogis(y)) |>
  (\(x)retry_mb(formula = y ~ crf(x, model = "ecx4param"),
                data = x, chains = 2, n_mod = 1))()
mod_3 <- nec_data |>
  dplyr::mutate(y = qlogis(y)) |>
  (\(x)retry_mb(formula = y ~ crf(x, model = "nechorme4"),
                data = x, chains = 2, n_mod = 1))()
mod_4 <- nec_data |>
  dplyr::mutate(y = qlogis(y), log_x = log(x)) |>
  (\(x)retry_mb(formula = y ~ crf(log_x, model = "nechorme4"),
                data = x, chains = 2, n_mod = 1))()
mod_5 <- nec_data |>
  dplyr::mutate(y = qlogis(y), log_x = log(x)) |>
  (\(x)retry_mb(formula = y ~ crf(log(x), model = "nechorme4"),
                data = x, chains = 2, n_mod = 1))()
mod_6 <- nec_data |>
  (\(x)retry_mb(formula = qlogis(y) ~ crf(x, model = "nechorme4"),
                data = x, chains = 2, n_mod = 1))()

other_data <- nec_data
colnames(other_data) <- c("a", "b")
nec_data <- nec_data |>
  dplyr::mutate(count = as.integer(round(y * 20)), trials = as.integer(20))
a <- nec_data$x
b <- nec_data$y
j <- nec_data$trials

data(herbicide)
herbicide <- herbicide |> 
  dplyr::mutate(x = log(concentration),
                y = (fvfm+0.001)*0.999)
fit <-  bnec(y ~ crf(x, model = "ecx4param"), data = herbicide, 
             family = Beta(), seed = 17)

fit_brms <- pull_brmsfit(fit)

priors.fit <- prior_summary(fit_brms)
priors.fit
bf_fitInt <- brms::bf(y ~ top + (bot - top)/(1 + exp((ec50 - x) * exp(beta))),
                      top + bot + beta + ec50 ~ herbicide,
                      nl = TRUE)
fit_brmsInt <- brm(bf_fitInt, data = herbicide, family = Beta(),
                   prior = priors.fit, iter = 5000,  save_pars = save_pars(all=TRUE),
                   seed = 700, init = 0)



