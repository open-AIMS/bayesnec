test_that("ecxll5 predictor retains finite values and handles a powered overflow", {
  x <- seq(0, 1, length.out = 41)
  bot <- 0.2
  top <- 0.9
  beta <- log(4)
  ec50 <- 0.5

  for (f in c(-2, 0, 2)) {
    powered <- bot + (top - bot) /
      (1 + exp(exp(beta) * (x - ec50))) ^ exp(f)
    stable <- bayesnec:::pred_ecxll5(bot, top, beta, ec50, f, x)
    expect_true(all(is.finite(powered)))
    expect_equal(stable, powered, tolerance = 1e-7)
  }

  beta <- 3.231
  f <- 7.703
  x <- 1
  expect_true(is.infinite(
    (1 + exp(exp(beta) * (x - 0.3))) ^ exp(f)
  ))
  expect_identical(
    bayesnec:::pred_ecxll5(0.849, 1.390, beta, 0.3, f, x), 0.849
  )
})

test_that("ecxll5 fits four chains from an init that overflows the powered form", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  dat <- data.frame(
    x = rep(seq(0, 1, length.out = 6), each = 3),
    y = rep(c(1.39, 1.35, 1.10, 0.90, 0.85, 0.84), each = 3)
  )
  init <- lapply(
    list(b_beta = 3.231, b_top = 1.390, b_bot = 0.849,
         b_ec50 = 0.300, b_f = 7.703),
    function(value) array(value, dim = 1)
  )
  # This short run tests initialisation, not posterior diagnostics.
  fit <- suppressWarnings(suppressMessages(bnec(
    y ~ crf(x, "ecxll5"), data = dat, family = gaussian(),
    init = rep(list(init), 4), chains = 4, iter = 120, warmup = 60,
    seed = 600, refresh = 0
  )))

  expect_s3_class(fit, "bayesnecfit")
  expect_equal(dim(as.array(fit$fit))[2], 4)
})
