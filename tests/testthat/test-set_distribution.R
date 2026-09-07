r_pois <- rpois(1000, lambda = 10)
r_norm <- rnorm(1000)
r_gamm <- rgamma(1000, 2)
r_beta <- rbeta(1000, 1, 2)
r_binm <- rbinom(1000, 10, 0.5)

test_that("expect correct distribution", {
  expect_identical(set_distribution(r_pois, support_integer = TRUE), "poisson")
  expect_identical(set_distribution(r_norm), "gaussian")
  expect_error(expect_identical(set_distribution(r_gamm), "gamma"))
  expect_identical(set_distribution(r_gamm), "Gamma")
  expect_identical(set_distribution(r_beta), "Beta")
  expect_identical(set_distribution(r_binm, support_integer = TRUE, 10),
                   "binomial")
  expect_error(set_distribution(r_norm, trials = TRUE))
  expect_identical(set_distribution(r_pois, support_integer = TRUE, 10),
                   "binomial")
})

r_pois_b <- add_na(r_pois, n = 10)
r_norm_b <- add_na(r_norm, n = 10)
test_that("does not support NA", {
  expect_error(set_distribution(r_pois_b, support_integer = TRUE))
  expect_error(set_distribution(r_norm_b))
})


test_that("an integer response with negative values gets gaussian (#272)", {
  # The integer branch tested min(x) >= 0 and had no else, so the function fell
  # off the end and returned NULL. Automatic family selection read that NULL
  # and the call failed reporting a `family` argument the user had not
  # supplied. An integer response with negative values -- a difference, an
  # increment, a change in a count between two times -- is ordinary input.
  expect_equal(set_distribution(c(-3L, 0L, 4L), support_integer = TRUE),
               "gaussian")
  expect_equal(set_distribution(c(-1L, -5L), support_integer = TRUE),
               "gaussian")
  # The non-negative cases are unchanged.
  expect_equal(set_distribution(c(0L, 3L, 4L), support_integer = TRUE),
               "poisson")
  expect_equal(set_distribution(c(0L, 3L, 4L), support_integer = TRUE,
                                trials = 10L), "binomial")
  # And the equivalent numeric vector already behaved this way, which is what
  # makes the two consistent rather than merely both defined.
  expect_equal(set_distribution(c(-3, 0, 4)), "gaussian")
})

test_that("a negative integer response reaches get_priors (#272)", {
  # The user-visible symptom: no family argument was supplied, and the error
  # named one.
  d <- data.frame(x = rep(c(0.5, 1, 10, 100), each = 5),
                  y = as.integer(rep(c(5, 2, 0, -3), each = 5)))
  expect_s3_class(
    suppressMessages(get_priors(y ~ crf(x, model = "nec3param"), data = d)),
    "brmsprior"
  )
})
