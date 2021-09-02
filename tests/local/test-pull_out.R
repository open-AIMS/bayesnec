library(bayesnec)

test_that("loo_controls work", {
  my_ctrls <- list(fitting = list(moment_match = TRUE))
  pull_out(manec_gausian_identity, model = "nec", loo_controls = my_ctrls) %>%
    expect_s3_class("bayesnecfit") %>%
    suppressWarnings %>%
    expect_message %>%
    expect_message
})
