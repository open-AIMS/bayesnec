# Fitting tests for the beta_ub family. These live under tests/local because
# they sample; the fast checks on the family object, registration, data checks,
# priors and inits are in tests/testthat/test-family_beta_ub.R.

bub_sim <- function(n_conc = 8, reps = 5, top = 0.8, u = 1, phi = 30,
                    seed = 173) {
  set.seed(seed)
  x <- rep(seq(0, 6, length.out = n_conc), each = reps)
  mu <- top * exp(-pmax(x - 2, 0))
  data.frame(x = x, y = u * rbeta(length(mu), (mu / u) * phi,
                                  (1 - mu / u) * phi))
}

bub_dat <- bub_sim()
bub_ymax <- max(bub_dat$y)

bub_fit <- muted_bnec(
  y ~ crf(x, model = "nec3param"), data = bub_dat, family = beta_ub(),
  U_loc = 1, U_scale = 0.1, chains = 2, iter = 2000, warmup = 1000,
  seed = 173, control = list(adapt_delta = 0.95)
)

# ---- Phase 5: bnec() end to end --------------------------------------------

test_that("bnec() fits a beta_ub model and converges", {
  expect_s3_class(bub_fit, "bayesnecfit")
  expect_lt(max(brms::rhat(bub_fit$fit), na.rm = TRUE), 1.05)
  np <- brms::nuts_params(bub_fit$fit)
  expect_equal(sum(np$Value[np$Parameter == "divergent__"]), 0)
})

test_that("the response reaches brms untouched", {
  # no rescaling anywhere: the whole point of the family is that the ceiling
  # is a parameter rather than a divisor applied to the data
  expect_equal(max(bub_fit$fit$data$y), max(bub_dat$y))
  expect_equal(min(bub_fit$fit$data$y), min(bub_dat$y))
  expect_equal(sort(bub_fit$fit$data$y), sort(bub_dat$y))
})

test_that("ymax reaches the Stan data and matches the observed maximum", {
  expect_equal(brms::standata(bub_fit$fit)$ymax, bub_ymax)
})

test_that("the fit recovers its generating values", {
  fe <- brms::fixef(bub_fit$fit)
  expect_gt(fe["top_Intercept", "Q97.5"], 0.8 - 0.1)
  expect_lt(fe["top_Intercept", "Q2.5"], 0.8)
  expect_true(fe["nec_Intercept", "Q2.5"] < 2 && fe["nec_Intercept", "Q97.5"] > 2)
  dr <- brms::as_draws_df(bub_fit$fit)
  u <- bub_ymax + dr$delta
  expect_true(quantile(u, 0.975) > 0.95)   # truth U = 1
  expect_true(all(u > bub_ymax))           # the ceiling is above every datum
})

test_that("top stays the control-level response rather than the support bound", {
  # the failure this family exists to prevent: `top` doubling as the ceiling
  dr <- brms::as_draws_df(bub_fit$fit)
  top <- median(dr$b_top_Intercept)
  u <- median(bub_ymax + dr$delta)
  expect_lt(abs(top - mean(bub_dat$y[bub_dat$x == 0])), 0.1)
  expect_lt(top, u)
})

test_that("summary() renders, including phi and delta", {
  out <- capture.output(print(summary(bub_fit)))
  expect_true(any(grepl("beta_ub", out)))
  expect_true(any(grepl("^phi", out)))
  expect_true(any(grepl("^delta", out)))
  expect_true(any(grepl("^NEC", out)))
})

test_that("a prior-driven ceiling is announced when U_loc is omitted", {
  expect_message(
    suppressWarnings(bnec(
      y ~ crf(x, model = "nec3param"), data = bub_dat, family = beta_ub(),
      chains = 1, iter = 400, warmup = 200, seed = 1, refresh = 0, silent = 2
    )),
    "prior-driven"
  )
})
