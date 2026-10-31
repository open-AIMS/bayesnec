# Check documented equivalence to drc NEC.3/NEC.4 using its transcribed
# formula below, avoiding a new test dependency. The comparison fixes
# the reference formula independently of installed drc versions.

# drc:::NEC()$fct, transcribed. parmMat columns are b, c, d, e.
drc_nec <- function(b, c, d, e, dose) {
  doseDiff <- dose - e
  c + (d - c) * exp(-b * doseDiff * (doseDiff > 0))
}

test_that("nec4param is exactly drc's NEC.4", {
  x <- seq(0, 10, length.out = 25)
  pars <- list(b = 0.4, c = 0.1, d = 1, e = 3.2)
  expect_equal(
    bayesnec:::pred_nec4param(b_beta = log(pars$b), b_bot = pars$c,
                              b_nec = pars$e, b_top = pars$d, x = x),
    drc_nec(pars$b, pars$c, pars$d, pars$e, x),
    tolerance = 0
  )
})

test_that("nec3param is exactly drc's NEC.3, which fixes c = 0", {
  x <- seq(0, 10, length.out = 25)
  pars <- list(b = 0.4, d = 1, e = 3.2)
  expect_equal(
    bayesnec:::pred_nec3param(b_beta = log(pars$b), b_nec = pars$e,
                              b_top = pars$d, x = x),
    drc_nec(pars$b, c = 0, pars$d, pars$e, x),
    tolerance = 0
  )
})

test_that("the equivalence holds away from the tested parameter values", {
  # Vary every parameter and include thresholds outside and at the grid
  # boundary. Start from beta with b = exp(beta): exp(log(b)) introduces
  # round-off unrelated to equation equivalence.
  x <- seq(0, 10, length.out = 25)
  grid <- expand.grid(beta = c(-3, -0.92, 1.1), c = c(0, 0.1, 0.6),
                      d = c(0.5, 1, 40), e = c(-1, 0, 3.2, 25))
  diffs <- vapply(seq_len(nrow(grid)), function(i) {
    g <- grid[i, ]
    max(abs(
      bayesnec:::pred_nec4param(b_beta = g$beta, b_bot = g$c, b_nec = g$e,
                                b_top = g$d, x = x) -
        drc_nec(exp(g$beta), g$c, g$d, g$e, x)
    ))
  }, numeric(1))
  expect_identical(max(diffs), 0)
})

test_that("the reparameterisation round trip is where the only error lives", {
  # Starting from drc b and round-tripping through beta = log(b) can
  # introduce floating-point error even when the equations agree.
  x <- seq(0, 10, length.out = 25)
  b <- 0.05
  round_trip <- max(abs(
    bayesnec:::pred_nec3param(b_beta = log(b), b_nec = 3, b_top = 1, x = x) -
      drc_nec(b, c = 0, d = 1, e = 3, dose = x)
  ))
  expect_gt(round_trip, 0)
  expect_lt(round_trip, 1e-12)
})

test_that("bayesnec's decay rate is positive by construction", {
  # drc can estimate negative b and therefore unbounded post-threshold
  # growth. exp(beta) excludes that region for finite real beta; the
  # fixed-top NEC.2 case is also outside the documented mapping.
  x <- seq(0, 10, length.out = 25)
  betas <- c(-50, -5, 0, 5)
  for (beta in betas) {
    y <- bayesnec:::pred_nec3param(b_beta = beta, b_nec = 3, b_top = 1, x = x)
    # a decline: never above `top`, and monotone non-increasing
    expect_lte(max(y), 1)
    expect_true(all(diff(y) <= 0))
  }
})

test_that("pred_ecxflat is its top at every concentration (#419)", {
  top <- 0.7
  dim(top) <- 1
  expect_identical(bayesnec:::pred_ecxflat(top, c(0, 0.5, 10, 1e6)),
                   rep(0.7, 4))
  # The fitted formula agrees: its second term is exactly zero for any finite
  # predictor, which is what keeps the predictor in the fitted object's data.
  rhs <- bayesnec:::bf_ecxflat$formula[[3]]
  expect_identical(eval(rhs, list(top = 0.7, x = c(-3, 0, 1e6))),
                   rep(0.7, 3))
})
