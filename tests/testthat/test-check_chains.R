library(bayesnec)
library(dplyr)

test_that("returns null", {
  expect_null(check_chains(nec_gausian_identity))
})

test_that("returns pdf for baysmanec objects with filename argument", {
  filename <- random_filename(15)
  check_chains(manec_gausian_identity, filename = filename) %>%
    expect_invisible %>%
    expect_message
  on.exit(file.remove(paste(filename, ".pdf", sep = "")))
})
