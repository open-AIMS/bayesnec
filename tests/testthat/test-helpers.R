library(bayesnec)
library(dplyr)

test_that("handle_set works", {
  m_0 <- paste0("Nothing to amend, please specify a model to either add",
                " or drop that differs from the original set.")
  handle_set(c("nec4param", "nec3param"), add = "nec4param") %>%
    expect_false %>%
    expect_message(m_0)
  expect_false(handle_set(c("nec4param", "nec3param"))) %>%
    expect_message(m_0)
  handle_set(c("nec4param", "nec3param"),
             drop = c("nec4param", "nec3param")) %>%
    expect_error("All models removed, nothing to return")
  handle_set(c("nec4param", "nec3param"), add = c("ecx4param", "ecxlin")) %>%
    expect_equal(c("nec4param", "nec3param", "ecx4param", "ecxlin"))
  handle_set(c("nec4param", "nec3param", "ecx4param",  "ecxlin"),
             drop = c("ecxlin", "nec4param", "nec3param")) %>%
    expect_equal(c("ecx4param"))
})
