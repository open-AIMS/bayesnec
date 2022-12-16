library(bayesnec)
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = 1)

test_that("loo_controls work", {
  my_ctrls <- list(fitting = list(moment_match = TRUE))
  pull_out(manec_gausian_identity, model = "nec", loo_controls = my_ctrls) |>
    expect_s3_class("bayesnecfit") |>
    suppressWarnings() |>
    expect_message() |>
    expect_message()
})

test_that("returns correct class for proper model names", {
  expect_identical(pull_out(manec_example, model = "nec"),
                   pull_out(manec_example, model = "nec4param")) |>
    suppressWarnings() |>
    suppressMessages()
  expect_s3_class(pull_out(manec_example, model = "nec"), "bayesnecfit") |>
    suppressWarnings() |>
    suppressMessages()
  expect_s3_class(pull_out(manec_example, model = "ecx"), "bayesnecfit") |>
    suppressWarnings() |>
    suppressMessages()
})

test_that("loo_controls ignore weights", {
  msg <- "You have specified a list of arguments in loo_control"
  my_ctrls <- list(weights = list(method = "stacking"))
  pull_out(manec_example, model = "nec", loo_controls = my_ctrls) |>
    suppressWarnings() |>
    expect_message(msg) |>
    expect_message() |>
    expect_message()
})
