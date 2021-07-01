library(bayesnec)
library(dplyr)

data(manec_example)
manec_example_stack <- amend(manec_example,
                                loo_controls = list(method = "stacking")) %>%
  suppressWarnings

test_that("input checks work correctly and return appropriate messages", {
  m_0 <- paste0("Nothing to amend, please specify a model to either add or",
                " drop, or a weighting method via loo_controls;")
  m_1 <- paste0("Nothing to amend, please specify a model to either add or",
                " drop that differs from the original set")
  m_2 <- "Returning original model set."
  m_3 <- paste0("Weighting method not modified, please call amend and ",
                "specify only loo_controls if you do not need to drop or add",
                " any models and simply want to update the weighting method.")
  expect_message(amend(manec_example), m_0)
  amend(manec_example, drop = "nec3param") %>%
    expect_message(m_1) %>%
    expect_message(m_2) %>%
    expect_message(m_3)
  m_4 <- paste0("The weighting method you have supplied is invalid, it",
                " must be one of \"stacking\" or \"pseudobma\".")
  manec_example %>%
    amend(loo_controls = list(method = "somethingwrong")) %>%
    expect_error(m_4)
  expect_message(amend(manec_example, drop = "nec4param"))
  amend(manec_example, add = "nec3param") %>%
    expect_message %>%
    expect_message("Fitted models are:  nec4param ecx4param")
  amend(manec_example, add = "nec4param",
        loo_controls = list(method = "pseudobma")) %>%
    expect_message %>%
    expect_message(m_2) %>%
    expect_message(m_3)
  amend(manec_example, loo_controls = list(method = "pseudobma")) %>%
    expect_message(m_2) %>%
    expect_message("Weighting method specified is the same as the original.")
})

test_that("loo_controls pass correctly", {
  expect_equal(class(manec_example_stack$mod_stats$wi), "stacking_weights")
})
