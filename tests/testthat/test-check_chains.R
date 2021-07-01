library(bayesnec)
library(dplyr)

data(manec_example)
nec4param <- pull_out(manec_example, model = "nec4param")

test_that("returns null", {
  expect_null(check_chains(nec4param))
})

test_that("returns pdf for bayesmanec objects with filename argument", {
  filename <- random_filename(15)
  check_chains(manec_example, filename = filename) %>%
    expect_invisible %>%
    expect_message
  on.exit(file.remove(paste(filename, ".pdf", sep = "")))
})
