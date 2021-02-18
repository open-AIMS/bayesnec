require(bayesnec)
require(ggplot2)

test_that("returns ggplot for baysnec objects", {
  p <- check_priors(nec_gausian_identity)  
  expect_error(print(p), NA)
  expect_silent(check_priors(nec_gausian_identity))
  expect_equal(class(p),c("gg", "ggplot") )
 
})

test_that("returns pdf for baysmanec objects", {
  filename <- random_filename(15)
  expect_invisible(check_priors(manec_gausian_identity, filename = filename))
  on.exit(file.remove(paste(filename, ".pdf", sep = ""))) 
})

 
