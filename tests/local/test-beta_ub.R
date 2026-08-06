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

# ---- Phase 6: predictions, ECx, NSEC, plotting -----------------------------

test_that("the toxicity estimates come back finite and correctly ordered", {
  e10 <- ecx(bub_fit, ecx_val = 10)
  e50 <- ecx(bub_fit, ecx_val = 50)
  ns <- nsec(bub_fit)
  ne <- nec(bub_fit)
  for (z in list(e10, e50, ns, ne)) {
    expect_length(z, 3)
    expect_true(all(is.finite(z)))
    expect_lt(z[2], z[1])
    expect_gt(z[3], z[1])
  }
  # a larger effect must take a larger concentration, and the no-effect
  # estimates must sit below the 10% effect concentration
  expect_lt(e10[1], e50[1])
  expect_lt(ne[1], e10[1])
  expect_lt(ns[1], e10[1])
  # nec3param has no bot, so relative and absolute coincide
  expect_equal(unname(ecx(bub_fit, ecx_val = 10, type = "relative")[1]),
               unname(e10[1]), tolerance = 1e-8)
})

test_that("predictions delegate to the custom-family methods", {
  ep <- posterior_epred(bub_fit)
  pp <- posterior_predict(bub_fit)
  expect_equal(dim(ep), dim(pp))
  expect_equal(ncol(ep), nrow(bub_fit$fit$data))
  expect_true(all(is.finite(ep)))
  expect_true(all(ep > 0))
  # E[y] = mu exactly for this family, so epred and the predictive mean agree
  expect_equal(colMeans(ep), colMeans(pp), tolerance = 0.05)
  ft <- fitted(bub_fit)
  pr <- predict(bub_fit)
  expect_equal(colnames(ft), c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
  expect_equal(colnames(pr), c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
  # the predictive interval must be wider than the interval on the mean
  expect_gt(mean(pr[, "Est.Error"]), mean(ft[, "Est.Error"]))
})

test_that("plot() and autoplot() render", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_silent(plot(bub_fit))
  g <- suppressMessages(autoplot(bub_fit))
  expect_s3_class(g, "ggplot")
  expect_gt(nrow(suppressMessages(ggbnec_data(bub_fit))), 0)
})

test_that("ECx is invariant to a divisor fixed in advance", {
  # the claim made in ?ecx and in issue #173: dividing by a constant changes
  # what `top` means, not the toxicity estimate. Only true for a constant --
  # the whole problem with normalising is that the divisor is random.
  k <- 2.5
  d2 <- data.frame(x = bub_dat$x, y = bub_dat$y / k)
  f2 <- muted_bnec(y ~ crf(x, model = "nec3param"), data = d2,
                   family = beta_ub(), U_loc = 1 / k, U_scale = 0.1 / k,
                   chains = 2, iter = 2000, warmup = 1000, seed = 173,
                   control = list(adapt_delta = 0.95))
  expect_equal(unname(ecx(f2, ecx_val = 10)[1]),
               unname(ecx(bub_fit, ecx_val = 10)[1]), tolerance = 1e-6)
  expect_equal(unname(nec(f2)[1]), unname(nec(bub_fit)[1]), tolerance = 0.01)
  # `top`, by contrast, moves by exactly the divisor
  t1 <- median(brms::as_draws_df(bub_fit$fit)$b_top_Intercept)
  t2 <- median(brms::as_draws_df(f2$fit)$b_top_Intercept)
  expect_equal(t1 / t2, k, tolerance = 0.02)
})

# ---- Phase 7: model averaging and QC ---------------------------------------

bub_manec <- muted_bnec(
  y ~ crf(x, model = c("nec3param", "nec4param", "ecx4param")),
  data = bub_dat, family = beta_ub(), U_loc = 1, U_scale = 0.1,
  chains = 2, iter = 2000, warmup = 1000, seed = 173,
  control = list(adapt_delta = 0.95)
)

test_that("a beta_ub model set averages", {
  expect_s3_class(bub_manec, "bayesmanecfit")
  expect_gte(length(bub_manec$success_models), 2)
  expect_equal(sum(bub_manec$mod_stats$wi), 1, tolerance = 1e-6)
  # the generating model should carry real weight
  expect_gt(bub_manec$mod_stats["nec3param", "wi"], 0.1)
})

test_that("averaged toxicity estimates are finite and ordered", {
  e10 <- ecx(bub_manec, ecx_val = 10)
  e50 <- ecx(bub_manec, ecx_val = 50)
  ne <- suppressMessages(nec(bub_manec))
  for (z in list(e10, e50, ne)) {
    expect_length(z, 3)
    expect_true(all(is.finite(z)))
  }
  expect_lt(e10[1], e50[1])
  expect_lt(ne[1], e10[1])
  ae <- suppressMessages(average_estimates(list(a = bub_manec)))
  expect_true(all(is.finite(ae)))
})

test_that("amend(drop) and the QC screens run on a beta_ub set", {
  dropped <- suppressMessages(suppressWarnings(
    amend(bub_manec, drop = "ecx4param")
  ))
  expect_false("ecx4param" %in% dropped$success_models)
  expect_gte(length(dropped$success_models), 2)
  r <- rhat(bub_manec)
  expect_true(all(vapply(r, function(z) all(z$rhat_vals < 1.05), logical(1))))
  out <- capture.output(print(summary(bub_manec)))
  expect_true(any(grepl("beta_ub", out)))
  expect_true(any(grepl("Model weights", out)))
})
