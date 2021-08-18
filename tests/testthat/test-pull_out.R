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
                   pull_out(manec_example, model = "nec4param")) %>%
    suppressWarnings %>%
    suppressMessages
  expect_s3_class(pull_out(manec_example, model = "nec"), "bayesnecfit") %>%
    suppressWarnings %>%
    suppressMessages
  expect_s3_class(pull_out(manec_example, model = "ecx"), "bayesnecfit") %>%
    suppressWarnings %>%
    suppressMessages
})

test_that("loo_controls ignore weights", {
  msg <- "You have specified a list of arguments in loo_control"
  my_ctrls <- list(weights = list(method = "stacking"))
  pull_out(manec_example, model = "nec", loo_controls = my_ctrls) %>%
    suppressWarnings %>%
    expect_message(msg) %>%
    expect_message %>%
    expect_message
})
