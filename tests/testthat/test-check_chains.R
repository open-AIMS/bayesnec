library(bayesnec)
library(dplyr)

manec_gauss_id_2 <- bayesnec:::manec_gauss_id_2
nec4param <- pull_out(manec_gauss_id_2, model = "nec4param")

test_that("returns null", {
  expect_null(check_chains(nec4param))
})

test_that("returns pdf for bayesmanec objects with filename argument", {
  filename <- random_filename(15)
  check_chains(manec_gauss_id_2, filename = filename) %>%
    expect_invisible %>%
    expect_message
  on.exit(file.remove(paste(filename, ".pdf", sep = "")))
})
