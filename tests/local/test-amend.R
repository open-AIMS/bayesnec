library(bayesnec)
library(dplyr)

data(manec_example)
manec_gauss_id_3 <- amend(manec_example, add = "ecxlin") %>%
  suppressWarnings

test_that("models drop and add work correctly", {
  amend(manec_gauss_id_3, drop = "nec4param") %>%
    expect_message("Fitted models are:  ecx4param ecxlin")
  expect_equal(names(manec_gauss_id_3$mod_fits), c("nec4param", "ecx4param", "ecxlin"))
})
