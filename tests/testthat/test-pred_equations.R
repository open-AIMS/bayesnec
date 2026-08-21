# The `?models` and vignette("example2b") documentation added under #139 claims
# that drc's NEC.4 and NEC.3 are *exactly* nec4param and nec3param, not
# approximations of them. That is a numerical claim in prose, so it is asserted
# here rather than left to be re-derived by whoever next doubts it.
#
# drc is not a dependency, and adding one to test a documentation claim would be
# a poor trade. Its generator is three lines, so it is reproduced here verbatim
# from drc:::NEC()$fct -- the point of the test is that OUR equations match THAT
# formula, and an inlined copy makes the comparison explicit rather than hiding
# it behind a package version.

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
  # A single parameter set could match by coincidence of the step location. Vary
  # every parameter, including a nec outside the predictor range (so the step
  # never fires) and one at the lower boundary (so it fires everywhere).
  #
  # The grid is over `beta` with drc's b = exp(beta), not over b with our
  # beta = log(b). That is the direction the equivalence actually runs, and it
  # matters numerically: round-tripping b through exp(log(b)) is not the
  # identity in floating point, and doing it that way round produces
  # discrepancies of ~7e-15 that belong to the round trip rather than to any
  # difference between the two models.
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
  # Documented so nobody re-derives it: given b = exp(beta) the two forms are
  # bit-identical, but a user starting from a drc `b` and setting
  # beta = log(b) will not recover it exactly. The size of that is worth
  # knowing -- it is round-off, not a modelling difference.
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
  # The one substantive difference documented in ?models: drc estimates b
  # directly and can return b < 0, a threshold followed by unbounded growth.
  # exp(beta) cannot be negative for any real beta, so that region is
  # unreachable here -- which is the reason NEC.2's fixed-top case and drc's
  # negative-b case are both documented as deliberate omissions.
  x <- seq(0, 10, length.out = 25)
  betas <- c(-50, -5, 0, 5)
  for (beta in betas) {
    y <- bayesnec:::pred_nec3param(b_beta = beta, b_nec = 3, b_top = 1, x = x)
    # a decline: never above `top`, and monotone non-increasing
    expect_lte(max(y), 1)
    expect_true(all(diff(y) <= 0))
  }
})
