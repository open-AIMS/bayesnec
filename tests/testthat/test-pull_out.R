library(bayesnec)
data(manec_example)

test_that("fails without model argument", {
  expect_error(pull_out(manec_example))
})

test_that("rejects wrong model names/structure, returns original", {
  msg <- "non-existent in current set of models"
  pull_out(manec_example, model = "wrong") %>%
    expect_equal(manec_example) %>%
    expect_message(msg)
  pull_out(manec_example, model = NA) %>%
    expect_equal(manec_example) %>%
    expect_message(msg)
  pull_out(manec_example, model = c("nec4param", "ecx4param")) %>%
    expect_error
})

test_that("returns correct class for proper model names", {
  expect_identical(pull_out(manec_example, model = "nec"),
                   pull_out(manec_example, model = "nec4param"))
  expect_s3_class(pull_out(manec_example, model = "nec"), "bayesnecfit")
  expect_s3_class(pull_out(manec_example, model = "ecx"), "bayesnecfit")
})

test_that("loo_controls ignore weights", {
  msg <- "You have specified a list of arguments in loo_control"
  my_ctrls <- list(weights = list(method = "stacking"))
  pull_out(manec_example, model = "nec", loo_controls = my_ctrls) %>%
    expect_warning %>%
    expect_message(msg) %>%
    expect_message
})

test_that("loo_controls work", {
  my_ctrls <- list(fitting = list(moment_match = TRUE))
  pull_out(manec_example, model = "nec", loo_controls = my_ctrls) %>%
    expect_s3_class("bayesnecfit") %>%
    expect_warning %>%
    expect_message
})
