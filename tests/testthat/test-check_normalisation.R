set.seed(173)

# Helper: build the model.frame that check_normalisation() expects, in the same
# way bnec() does.
norm_bdat <- function(x, y) {
  d <- data.frame(x = x, y = y)
  model.frame(bayesnecformula(y ~ crf(x, "nec3param")), data = d,
              run_par_checks = TRUE)
}

norm_design <- function(k = 6) rep(c(0, 1, 2, 4, 8), each = k)

norm_response <- function(x) {
  p <- 0.9 / (1 + (x / 3)^2)
  rgamma(length(p), 10, 10 / (p * 30))
}

test_that("on_rational_grid recognises count-derived proportions", {
  expect_true(on_rational_grid(c(0, 1, 2, 3) / 4))
  expect_true(on_rational_grid(c(19, 20, 17, 20) / 20))
  expect_true(on_rational_grid(c(3, 7, 12)))
  # a continuous response divided by a continuous maximum is not on a grid
  y <- norm_response(norm_design())
  expect_false(on_rational_grid(y / max(y)))
  expect_false(on_rational_grid(c(0.123456789, 0.5, 1)))
  expect_false(on_rational_grid(numeric(0)))
})

test_that("no message for a raw, unnormalised response", {
  x <- norm_design()
  expect_silent(check_normalisation(norm_bdat(x, norm_response(x))))
})

test_that("no message for genuine count proportions with a unique maximum", {
  # The case the rational-grid guard exists for: one replicate recorded every
  # individual as alive, so max(y) == 1 and only one observation attains it.
  x <- norm_design()
  y <- c(1, 19, 18, 17, 18, 19, rep(c(15, 12, 8, 3), each = 6)) / 20
  y[1] <- 1
  expect_length(y, length(x))
  expect_true(max(y) == 1 && sum(y == 1) == 1)
  expect_silent(check_normalisation(norm_bdat(x, y)))
})

test_that("dividing by the observed maximum is detected", {
  x <- norm_design()
  y <- norm_response(x)
  expect_message(check_normalisation(norm_bdat(x, y / max(y))),
                 "divided by its own observed maximum")
})

test_that("dividing by the control mean is detected", {
  x <- norm_design()
  y <- norm_response(x)
  y <- y / mean(y[x == min(x)])
  expect_message(check_normalisation(norm_bdat(x, y)),
                 "divided by the control mean")
})

test_that("the control-mean check needs a real control group", {
  # Two controls, or a control group whose values are all identical, are not
  # evidence of anything and must not fire.
  x <- c(0, 0, 1, 1, 2, 2, 4, 4, 8, 8)
  y <- norm_response(x)
  expect_silent(check_normalisation(norm_bdat(x, y / mean(y[x == 0]))))

  x2 <- norm_design()
  y2 <- norm_response(x2)
  y2[x2 == 0] <- 1
  expect_silent(check_normalisation(norm_bdat(x2, y2)))
})

test_that("the two checks do not fire for each other", {
  x <- norm_design()
  y <- norm_response(x)
  # divided by the maximum: the control mean is not 1
  expect_message(check_normalisation(norm_bdat(x, y / max(y))),
                 "observed maximum")
  # divided by the control mean: the maximum is not exactly 1
  y_cm <- y / mean(y[x == min(x)])
  expect_false(max(y_cm) == 1 && sum(y_cm == 1) == 1)
})

test_that("too few observations are not screened", {
  expect_silent(check_normalisation(norm_bdat(c(0, 0, 1, 1), c(2, 1, 0.5, 1))))
})

test_that("bnec() emits the message once, not once per model", {
  # check_data() runs per model, so the check must not live there.
  x <- norm_design(10)
  y <- norm_response(x)
  bdat <- norm_bdat(x, y / max(y))
  msgs <- capture_messages(check_normalisation(bdat))
  expect_length(msgs, 1)
  expect_match(msgs, "observed maximum")
})
