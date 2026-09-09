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

# The tests below concern #298: an observation whose fitted variance is exactly
# zero made the ratio NaN for every draw, and the whole statistic was discarded
# with a message diagnosing a bad model fit. They exercise pearson_dispersion()
# directly rather than through a fitted model, because an underflowed fitted
# mean needs a contrived dataset and a long fit to produce, while the
# arithmetic it triggers is deterministic.

degenerate_input <- function(obs_y = c(5, 3, 0)) {
  list(obs_y = obs_y,
       prd_out = rbind(c(4.5, 2.5, 0), c(5.5, 3.5, 0)),
       ppd_out = rbind(c(6, 2, 0), c(4, 4, 0)),
       var_out = rbind(c(4, 2, 0), c(5, 3, 0)))
}

test_that("an exactly reproduced observation is excluded and all draws kept", {
  d <- degenerate_input()
  expect_message(
    disp <- pearson_dispersion(d$obs_y, d$prd_out, d$ppd_out, d$var_out),
    "reproduces exactly"
  )
  expect_length(disp, 2)
  expect_true(all(is.finite(disp)))
  # The value is the statistic computed on the two usable observations.
  obs_mat <- matrix(d$obs_y, 2, 3, byrow = TRUE)
  keep <- 1:2
  wanted <- rowSums(((obs_mat - d$prd_out) / sqrt(d$var_out))[, keep]^2) /
    rowSums(((d$ppd_out - d$prd_out) / sqrt(d$var_out))[, keep]^2)
  expect_equal(unname(disp), unname(wanted))
})

test_that("a zero-variance disagreement is reported rather than dropped", {
  # Case 2: the model has assigned zero variance to a value it did not predict.
  # The Pearson residual is infinite and that is a result, not an artefact.
  d <- degenerate_input(obs_y = c(5, 3, 2))
  expect_warning(
    disp <- pearson_dispersion(d$obs_y, d$prd_out, d$ppd_out, d$var_out),
    "not the fitted value"
  )
  expect_equal(unname(disp), c(Inf, Inf))
})

test_that("the observations are named in the message and the warning", {
  d <- degenerate_input()
  labs <- c("a", "b", "c")
  expect_message(
    pearson_dispersion(d$obs_y, d$prd_out, d$ppd_out, d$var_out, labels = labs),
    "\\(c\\)"
  )
  d2 <- degenerate_input(obs_y = c(5, 3, 2))
  expect_warning(
    pearson_dispersion(d2$obs_y, d2$prd_out, d2$ppd_out, d2$var_out,
                       labels = labs),
    "\\(c\\)"
  )
})

test_that("a draw with nothing left to compare returns NA", {
  zero <- matrix(0, 2, 3)
  disp <- suppressMessages(
    pearson_dispersion(c(0, 0, 0), zero, zero, zero)
  )
  expect_true(all(is.na(disp)))
})

test_that("a variance that is not finite is excluded and reported", {
  d <- degenerate_input()
  d$var_out[1, 3] <- NA_real_
  d$var_out[2, 3] <- 5
  expect_warning(
    disp <- pearson_dispersion(d$obs_y, d$prd_out, d$ppd_out, d$var_out),
    "negative or not finite"
  )
  expect_true(all(is.finite(disp)))
})

test_that("a negative variance is excluded rather than dropped by na.rm", {
  # is.finite(-1) is TRUE, so the guard tests the sign as well. Without it
  # sqrt() returns NaN, na.rm drops the term, and the observation is still
  # counted as contributing.
  d <- degenerate_input()
  d$var_out[, 3] <- c(-1, -1)
  expect_warning(
    disp <- pearson_dispersion(d$obs_y, d$prd_out, d$ppd_out, d$var_out),
    "negative or not finite"
  )
  keep <- 1:2
  obs_mat <- matrix(d$obs_y, 2, 3, byrow = TRUE)[, keep]
  wanted <- rowSums(((obs_mat - d$prd_out[, keep]) /
                       sqrt(d$var_out[, keep]))^2) /
    rowSums(((d$ppd_out[, keep] - d$prd_out[, keep]) /
               sqrt(d$var_out[, keep]))^2)
  expect_equal(unname(disp), unname(wanted))
})

test_that("the exclusion is reported per draw, not as a fixed set", {
  # The exclusion is elementwise. Reporting only the observations would
  # describe a set dropped from the whole posterior, which is a different
  # operation: measured on the fixture below, 229 of 400 draws exclude nothing.
  d <- degenerate_input()
  d$var_out[2, 3] <- 5
  expect_message(
    pearson_dispersion(d$obs_y, d$prd_out, d$ppd_out, d$var_out),
    "1 of 3 observations \\(3\\) which the model reproduces exactly, in 1 of 2 draws"
  )
})

test_that("draws that return NA are named rather than silently dropped", {
  # #39's account of a censored draw, applied here: a draw in which no
  # observation contributes a residual is excluded from the summary by the
  # na.rm in estimates_summary(), so the count is reported.
  d <- degenerate_input()
  d$var_out[1, ] <- 0
  d$prd_out[1, ] <- d$obs_y
  d$ppd_out[1, ] <- d$obs_y
  # capture_messages() rather than expect_message(), which lets the exclusion
  # message this input also raises through to the console.
  msgs <- capture_messages(
    disp <- pearson_dispersion(d$obs_y, d$prd_out, d$ppd_out, d$var_out)
  )
  expect_match(paste(msgs, collapse = " "),
               "No observation contributes a residual in 1 of 2 draws")
  expect_true(is.na(disp[1]))
  expect_true(is.finite(disp[2]))
})

test_that("the report reads correctly with no equation name", {
  # The name was pre-filled with "fitted" and then interpolated into "The
  # fitted <name> mean", which printed "The fitted fitted mean".
  d <- degenerate_input()
  msg <- capture_messages(
    pearson_dispersion(d$obs_y, d$prd_out, d$ppd_out, d$var_out)
  )
  expect_match(msg[1], "^The fitted mean has zero variance")
  expect_no_match(msg[1], "fitted fitted")
})

test_that("the statistic is unchanged where no observation is degenerate", {
  d <- degenerate_input()
  d$var_out[, 3] <- c(1, 1)
  obs_mat <- matrix(d$obs_y, 2, 3, byrow = TRUE)
  wanted <- rowSums(((obs_mat - d$prd_out) / sqrt(d$var_out))^2) /
    rowSums(((d$ppd_out - d$prd_out) / sqrt(d$var_out))^2)
  expect_silent(
    disp <- pearson_dispersion(d$obs_y, d$prd_out, d$ppd_out, d$var_out)
  )
  expect_equal(unname(disp), unname(wanted))
})

# Fitted once and reused: two tests need the same fit and it takes minutes to
# sample. At the two highest doses nothing survived, and nec3param's fitted mean
# underflows to exactly zero there.
degenerate_fit <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      d <- data.frame(x = rep(c(0, 1, 2, 3, 4), each = 3), trials = 10,
                      y = c(10, 10, 10, 10, 9, 10, 9, 8, 9, 0, 0, 0, 0, 0, 0))
      cached <<- bnec(y | trials(trials) ~ crf(x, model = "nec3param"),
                      data = d, family = "binomial", iter = 400, warmup = 200,
                      chains = 2, seed = 298, refresh = 0,
                      open_progress = FALSE) |>
        suppressMessages() |>
        suppressWarnings()
    }
    cached
  }
})

test_that("a fit that reproduces a group exactly still reports a statistic", {
  # The same failure end to end. Six of the fifteen observations have a fitted
  # variance of exactly zero in at least one draw. Before #298 every draw was
  # NaN, the returned vector was empty, and expand_nec() wrote NA into the
  # dispersion columns of the weights table.
  fit <- degenerate_fit()
  expect_true(any(brms::posterior_linpred(pull_brmsfit(fit)) == 0))
  expect_message(disp <- dispersion(fit, summary = TRUE),
                 "6 of 15 observations .* in [0-9]+ of 400 draws")
  expect_length(disp, 4)
  expect_true(all(is.finite(disp)))
  expect_false(is.na(fit$dispersion[["Estimate"]]))
})

test_that("a fit with nothing left to compare returns an empty vector", {
  # The remaining branch: every draw is NA, so there is no statistic. Reaching
  # it from a real fit needs a model degenerate at every observation, which is
  # not a fit anyone would produce, so the return of pearson_dispersion() is
  # substituted instead.
  fit <- degenerate_fit()
  local_mocked_bindings(
    pearson_dispersion = function(obs_y, prd_out, ...) {
      rep(NA_real_, nrow(prd_out))
    }
  )
  expect_message(disp <- dispersion(fit), "not\\s+defined for this fit")
  expect_length(disp, 0)
  expect_length(suppressMessages(dispersion(fit, summary = TRUE)), 0)
})
