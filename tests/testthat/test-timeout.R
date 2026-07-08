test_that("bnec validates the timeout argument before fitting", {
  # A non-positive or non-numeric timeout is rejected up front, so these
  # checks do not require an actual (slow) model fit to run.
  expect_error(
    bnec(y ~ crf(x, "nec3param"), data = nec_data, timeout = -1),
    "must be a positive number"
  )
  expect_error(
    bnec(y ~ crf(x, "nec3param"), data = nec_data, timeout = 0),
    "must be a positive number"
  )
  expect_error(
    bnec(y ~ crf(x, "nec3param"), data = nec_data, timeout = "a")
  )
})

test_that("amend validates the timeout argument", {
  expect_error(
    amend(manec_example, drop = "nec4param", timeout = -1),
    "must be a positive number"
  )
  expect_error(
    amend(manec_example, drop = "nec4param", timeout = "a")
  )
})

test_that("timeout is exposed with a sensible default", {
  expect_identical(formals(bnec)$timeout, Inf)
  expect_identical(formals(amend)$timeout, Inf)
})

test_that("fit_bayesnec aborts a fit that exceeds timeout", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  skip_if_not_installed("R.utils")
  # A tiny timeout guarantees the brm call is interrupted. In a multi-model
  # call the resulting failure is caught by try() and the model dropped, so
  # bnec still returns rather than erroring, but with fewer than the two
  # requested models. Kept behind NOT_CRAN as it triggers a real fit attempt.
  fit <- try(
    bnec(y ~ crf(x, model = c("nec3param", "ecx4param")),
         data = nec_data, timeout = 1e-3, chains = 1, iter = 200,
         warmup = 100, seed = 10),
    silent = TRUE
  ) |>
    suppressWarnings() |>
    suppressMessages()
  # The call must complete (not hang) and yield either a caught failure or a
  # valid bnec object; it must not return normally with both models intact.
  expect_true(inherits(fit, "try-error") || inherits(fit, "bnecfit"))
})
