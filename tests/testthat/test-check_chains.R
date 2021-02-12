library(bayesnec)

test_that("returns null", {
  expect_null(check_chains.default(nec_gausian_identity))
})

test_that("returns pdf for baysmanec objects with filename argument", {
  filename <- random_filename(15)
  expect_invisible(check_chains(manec_gausian_identity, filename = filename))
  on.exit(file.remove(paste(filename, ".pdf", sep = ""))) 
})

