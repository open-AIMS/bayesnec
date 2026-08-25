test_that("dispersion fails because family is gaussian", {
  expect_length(dispersion(ecx4param), 0)
  expect_length(dispersion(ecx4param, summary = TRUE), 0)
})

# The regression tests below all concern #247: dispersion() rebuilt the family
# with get(fam)(), which carries that family's DEFAULT link -- log for Poisson,
# logit for Binomial -- and applied its linkinv() to posterior_linpred(). Since
# bnec() forces link = "identity", the linear predictor is already on the
# response scale, so the value was transformed a second time.

poisson_fit <- function() {
  set.seed(247)
  x <- runif(60, 0, 3.2)
  mu <- 5 + (85 - 5) * exp(-exp(0.3) * (x - 1.5) * (x > 1.5))
  d <- data.frame(x = x, y = as.integer(rpois(length(mu), mu)))
  bnec(y ~ crf(x, model = "nec4param"), data = d, family = "poisson",
       iter = 400, warmup = 200, chains = 2, seed = 247, refresh = 0,
       open_progress = FALSE) |>
    suppressMessages() |>
    suppressWarnings()
}

test_that("dispersion uses the link the model was fitted with", {
  fit <- poisson_fit()
  m <- pull_brmsfit(fit)
  expect_identical(m$family$link, "identity")

  obs <- brms::standata(m)$Y
  epr <- brms::posterior_epred(m)
  set.seed(10)  # the default `seed` argument, so the draws match exactly
  sim <- brms::posterior_predict(m)

  # Var(count) = mu for a Poisson, and posterior_linpred() IS mu here, so the
  # weights are the fitted means themselves.
  wanted <- rowSums(sweep(-epr, 2, -obs)^2 / epr) /
    rowSums((sim - epr)^2 / epr)
  expect_equal(unname(dispersion(fit)), unname(wanted), tolerance = 1e-8)
})

test_that("dispersion recovers a value near one for Poisson-simulated data", {
  # The whole point of the statistic. Before #247 the exponentiated weights
  # spanned tens of orders of magnitude and the estimate wandered far from one
  # with a credible interval two orders of magnitude wide.
  fit <- poisson_fit()
  disp <- dispersion(fit, summary = TRUE)
  expect_gt(disp[["Q2.5"]], 0.4)
  expect_lt(disp[["Q97.5"]], 2.5)
})

test_that("dispersion detects genuine overdispersion", {
  set.seed(247)
  x <- runif(60, 0, 3.2)
  mu <- 5 + (85 - 5) * exp(-exp(0.3) * (x - 1.5) * (x > 1.5))
  d <- data.frame(x = x, y = as.integer(rnbinom(length(mu), mu = mu, size = 5)))
  fit <- bnec(y ~ crf(x, model = "nec4param"), data = d, family = "poisson",
              iter = 400, warmup = 200, chains = 2, seed = 247, refresh = 0,
              open_progress = FALSE) |>
    suppressMessages() |>
    suppressWarnings()
  disp <- dispersion(fit, summary = TRUE)
  expect_gt(disp[["Q2.5"]], 2)
})
