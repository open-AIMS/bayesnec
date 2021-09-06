library(bayesnec)
library(dplyr)

data(manec_example)
nec_ <- pull_out(manec_example, "nec4param")
ecx_ <- pull_out(manec_example, "ecx4param")

test_that("Update works with regular fitting arguments", {
  expect_s3_class(update(nec_, chains = 1, iter = 50, recompile = TRUE,
                         refresh = 0, verbose = FALSE), "bnecfit") %>%
    suppressWarnings %>%
    suppressMessages
})

test_that("Different distribution triggers error or message", {
  expect_message(update(manec_example, newdata = nec_data, recompile = TRUE,
                        chains = 1, iter = 50, family = Beta(link = "identity"),
                        force_fit = TRUE, refresh = 0, verbose = FALSE)) %>%
    suppressWarnings %>%
    suppressMessages
})
