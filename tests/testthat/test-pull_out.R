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
