library(bayesnec)
library(dplyr)

nec3param <- function(beta, nec, top, x) {
  top * exp(-exp(beta) * (x - nec) *
    ifelse(x - nec < 0, 0, 1))
}

data <- data.frame(x = seq(1, 20, length.out = 10), tr = 100, wght = c(1, 2),
                   group_1 = sample(c("a", "b"), 10, replace = TRUE),
                   group_2 = sample(c("c", "d"), 10, replace = TRUE))
data$y <- nec3param(beta = -0.2, nec = 4, top = 100, data$x)

test_that("correct classes", {
  f_0 <- y ~ crf(x, "nec3param")
  expect_s3_class(check_formula(bnf(f_0), data), "bayesnecformula")
  expect_error(check_formula(f_0, data), "must be of class bayesnecformula")
  expect_error(check_formula(bnf(f_0), as.matrix(data)))
  data_x_char <- data
  data_x_char$x <- as.character(data_x_char$x)
  expect_s3_class(check_formula(bnf(f_0), data_x_char), "bayesnecformula")
})

test_that("all variables actually exist in the data frame", {
  # population-level covariates are not allowed
  f_1 <- y ~ crf(x, "nec3param") + z
  expect_error(check_formula(bnf(f_1), data),
               "not allowed in a bayesnec formula")
})

test_that("Parameter checks", {
  # expect a series of messages for because not all
  # nec models have the "bot" parameter
  f_2 <- y | trials(tr) ~ crf(x, "nec") + (nec + bot | group_1)
  expect_s3_class(check_formula(bnf(f_2), data, run_par_checks = FALSE),
                  "bayesnecformula")
  check_formula(bnf(f_2), data, run_par_checks = TRUE) %>%
    expect_message("Performing single parameter checks on all models...") %>%
    expect_message("\"bot\" not valid parameters") %>%
    expect_message("\"bot\" not valid parameters") %>%
    expect_message("\"bot\" not valid parameters") %>%
    expect_message("\"bot\" not valid parameters") %>%
    expect_message("\"bot\" not valid parameters") %>%
    expect_message("\"bot\" not valid parameters") %>%
    expect_message("\"bot\" not valid parameters")
  f_3 <- "log(y) | trials(tr) ~ crf(sqrt(x), \"nec3param\")"
  expect_s3_class(check_formula(bnf(f_3), data), "bayesnecformula")
  f_4 <- y | trials(tr) ~ crf(x, "nec3param") + ogl(group_1) + pgl(group_2)
  expect_s3_class(check_formula(bnf(f_4), data), "bayesnecformula")
  # There can only be one variable inside crf
  f_5 <- y | trials(tr) ~ crf(x + tr, "nec3param")
  expect_error(check_formula(bnf(f_5), data), "can only have one variable")
  f_6 <- y | trials(tr) ~ crf(sqrt(x + tr), "nec3param")
  expect_error(check_formula(bnf(f_6), data), "can only have one variable")
  # right hand side is either predictor or random variable
  f_7 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + tr
  expect_error(check_formula(bnf(f_7), data),
               "not allowed in a bayesnec formula")
  f_8 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + ogl(group_1)
  expect_s3_class(check_formula(bnf(f_8), data), "bayesnecformula")
  f_9 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + kgl(group_1)
  expect_error(check_formula(bnf(f_9), data),
               "not allowed in a bayesnec formula")
  # group-level cannot be numeric
  f_10 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + pgl(tr)
  expect_error(check_formula(bnf(f_10), data), "variables cannot be numeric")
  f_11 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + ogl(tr)
  expect_error(check_formula(bnf(f_11), data), "variables cannot be numeric")
  f_12 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + (nec | tr)
  expect_error(check_formula(bnf(f_12), data), "variables cannot be numeric")
  f_13 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + (nec + top | tr)
  expect_error(check_formula(bnf(f_13), data), "variables cannot be numeric")
  f_14 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + (nec + top | tr) +
    (beta | x)
  expect_error(check_formula(bnf(f_14), data), "variables cannot be numeric")
  # nested/interacting random effects accepted as long as variables exist
  f_15 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + ogl(group_1 / group_2)
  f_16 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + ogl(group_1:group_2)
  f_17 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + pgl(group_1 / group_2)
  f_18 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + pgl(group_1:group_2)
  f_19 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + (nec | group_1 / group_2)
  f_20 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + (nec | group_1:group_2)
  expect_s3_class(check_formula(bnf(f_15), data), "bayesnecformula")
  expect_s3_class(check_formula(bnf(f_16), data), "bayesnecformula")
  expect_s3_class(check_formula(bnf(f_17), data), "bayesnecformula")
  expect_s3_class(check_formula(bnf(f_18), data), "bayesnecformula")
  expect_s3_class(check_formula(bnf(f_19), data), "bayesnecformula")
  expect_s3_class(check_formula(bnf(f_20), data), "bayesnecformula")
  # complex nested inline functions for x are allowed (but will fail internally)
  f_21 <- y ~ crf(scale(sqrt(x), scale = FALSE), "nec3param")
  expect_s3_class(check_formula(bnf(f_21), data), "bayesnecformula")
})
