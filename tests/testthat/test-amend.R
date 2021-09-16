library(bayesnec)
library(dplyr)

data(manec_example)

test_that("input checks work correctly and return appropriate messages", {
  general_error <- paste(
    "Nothing to amend, please specify a proper model to either add or drop, or",
    "changes to loo_controls;\n Returning original model set."
  )
  m_1 <- paste0("Nothing to amend, please specify a model to either add or",
                " drop that differs from the original set")
  m_2 <- "Returning original model set."
  m_3 <- paste0("No new LOO fitting/weighting arguments have been specified;",
                " ignoring argument loo_controls.")
  expect_message(amend(manec_example), general_error)
  amend(manec_example, drop = "nec3param") %>%
    expect_message(general_error) %>%
    expect_message(m_1)
  m_4 <- "loo_controls list names are incorrect. See ?bnec"
  manec_example %>%
    amend(loo_controls = list(method = "somethingwrong")) %>%
    expect_message(m_4) %>%
    expect_error
  expect_message(amend(manec_example, drop = "nec4param")) %>%
    suppressWarnings
  amend(manec_example, add = "nec3param") %>%
    expect_message %>%
    expect_message("Fitted models are: nec4param ecx4param") %>%
    suppressWarnings
  amend(manec_example, add = "nec4param",
        loo_controls = list(weights = list(method = "pseudobma"))) %>%
    expect_message(general_error) %>%
    expect_message(m_1) %>%
    expect_message(m_3)
  amend(manec_example,
        loo_controls = list(weights = list(method = "pseudobma"))) %>%
    expect_message(general_error) %>%
    expect_message(m_3)
})
