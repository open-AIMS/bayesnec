library(bayesnec)
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = 1)

muted_amend <- function(...) {
  amend(...) |>
    suppressWarnings() |>
    suppressMessages()
}

get_new_method <- function(x) {
  attributes(x$mod_stats$wi)$method
}

manec_gauss_id_3 <- muted_amend(manec_gausian_identity, add = "ecxlin")

test_that("models drop and add work correctly", {
  amend(manec_gauss_id_3, drop = "nec4param") |>
    suppressWarnings() |>
    expect_message("Fitted models are: ecx4param ecxlin")
  expect_equal(names(manec_gauss_id_3$mod_fits),
               c("nec4param", "ecx4param", "ecxlin"))
})

test_that("new loo_controls are incorporated", {
  expect_equal(get_new_method(manec_gausian_identity), "pseudobma")
  my_ctrls <- list(weights = list(method = "stacking"))
  manec_example_stack <- muted_amend(
    manec_gausian_identity, loo_controls = my_ctrls
  )
  expect_equal(get_new_method(manec_example_stack), "stacking")
  my_ctrls <- list(fitting = list(moment_match = TRUE),
                   weights = list(method = "stacking"))
  manec_example_stack2 <- muted_amend(
    manec_gausian_identity, loo_controls = my_ctrls
  )
  expect_equal(get_new_method(manec_example_stack2), "stacking")
  # changing original to moment match alters weights
  my_ctrls <- list(fitting = list(moment_match = TRUE))
  manec_example_stack3 <- muted_amend(
    manec_gausian_identity, loo_controls = my_ctrls
  )
  expect_equal(get_new_method(manec_example_stack3), "pseudobma")
  expect_false(
    all(manec_gausian_identity$mod_stats$wi ==
          manec_example_stack$mod_stats$wi)
  )
  expect_false(
    all(manec_gausian_identity$mod_stats$wi ==
          manec_example_stack2$mod_stats$wi)
  )
  expect_false(
    all(manec_example_stack$mod_stats$wi == manec_example_stack2$mod_stats$wi)
  )
  expect_false(
    all(manec_gausian_identity$mod_stats$wi == manec_example_stack3$mod_stats$wi)
  )
})
