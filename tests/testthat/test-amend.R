library(bayesnec)
library(dplyr)

manec_gausian_identity <- bayesnec:::manec_gausian_identity
a1 <- bayesnec:::a1
a2 <- bayesnec:::a2

test_that("input checks work correctly and return appropriate messages", {
  m_0 <- paste0("Nothing to amend, please specify a model to either add or",
                " drop, or a weighting method via loo_controls;")
  m_1 <- paste0("Nothing to amend, please specify a model to either add or",
                " drop that differs from the original set")
  m_2 <- "Returning original model set."
  m_3 <- paste0("Weighting method not modified, please call amend and ",
                "specify only loo_controls if you do not need to drop or add",
                " any models and simply want to update the weighting method.")
  expect_message(amend(manec_gausian_identity), m_0)
  amend(manec_gausian_identity, drop = "nec3param") %>%
    expect_message(m_1) %>%
    expect_message(m_2) %>%
    expect_message(m_3)
  m_4 <- paste0("The weighting method you have supplied is invalid, it",
                " must be one of \"stacking\" or \"pseudobma\".")
  manec_gausian_identity %>%
   amend(loo_controls = list(method = "somethingwrong")) %>%
   expect_error(m_4)
  expect_message(amend(manec_gausian_identity, drop = "nec4param"))
  amend(manec_gausian_identity, add = "nec3param") %>%
    expect_message %>%
    expect_message("Fitted models are:  nec4param ecx4param")
  amend(manec_gausian_identity, add = "nec4param",
        loo_controls = list(method = "pseudobma")) %>%
    expect_message %>%
    expect_message(m_2) %>%
    expect_message(m_3)
  amend(manec_gausian_identity, loo_controls = list(method = "pseudobma")) %>%
    expect_message(m_2) %>%
    expect_message("Weighting method specified is the same as the original.")
})

test_that("loo_controls pass correctly", {
  expect_equal(class(a1$mod_stats$wi), "stacking_weights")
})

test_that("models drop and add work correctly", {
  amend(a2, drop = "nec4param") %>%
    expect_message("Fitted models are:  ecx4param ecxlin")
  expect_equal(names(a2$mod_fits), c("nec4param", "ecx4param", "ecxlin"))
})
