library(testthat)
library(bayesnec)
library(tidyverse)

options(mc.cores = 2)
ggplot2::theme_set(theme_default())
# prevents rstan related crashes
#options(brms.backend = "cmdstanr")
set.seed(50)

expect_ggplot <- function(object, ...) {
  testthat::expect_true(is(object, "ggplot"), ...)
}

expect_range <- function(object, lower = -Inf, upper = Inf, ...) {
  testthat::expect_true(all(object >= lower & object <= upper), ...)
}

suppress_bnec <- function(...) {
  bnec(...) %>%
    suppressWarnings %>%
    suppressMessages
}
logit <- function(p) {
  log(p / (1 - p))
}

context("local tests")