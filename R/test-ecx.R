library(bayesnec)
library(brms)

test_that("gaussian absolute ecx values are not returned", {
expect_error(ecx(manec_gausian_identity))
})