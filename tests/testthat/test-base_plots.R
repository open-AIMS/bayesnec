require(bayesnec)
require(ggplot2)
p <- plot(nec_gausian_identity)
pm <- plot(manec_gausian_identity)


test_that("returns null, is invisible, and is silent", {
  expect_null(plot(nec_gausian_identity))
  expect_null(plot(manec_gausian_identity))  
  expect_silent(plot(nec_gausian_identity))
  expect_silent(plot(manec_gausian_identity))   
  expect_invisible(plot(nec_gausian_identity))
  expect_invisible(plot(manec_gausian_identity))   
  
})


