test_that("returns null", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  expect_null(check_chains(nec4param))
})

test_that("returns pdf for bayesmanec objects with filename argument", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  filename <- random_filename(15)
  check_chains(manec_example, filename = filename) |>
    expect_invisible() |>
    expect_message()
  on.exit(file.remove(paste(filename, ".pdf", sep = "")))
})
