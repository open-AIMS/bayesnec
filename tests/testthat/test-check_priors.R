test_that("returns ggplot for bayesnecfit objects", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  p <- check_priors(nec4param)
  expect_error(print(p), NA)
  expect_silent(check_priors(nec4param))
  # Check inheritance rather than the exact class vector: ggplot2 >= 4.0 builds
  # plots as S7 objects, so class(p) gained extra entries ("ggplot2::ggplot",
  # "S7_object", ...) and an exact-equality test breaks across versions.
  expect_s3_class(p, "ggplot")
})

test_that("returns pdf for bayesmanecfit objects", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  filename <- random_filename(15)
  expect_invisible(check_priors(manec_example, filename = filename))
  on.exit(file.remove(paste(filename, ".pdf", sep = "")))
})
