require(bayesnec)
beta_family_default <- bayesnec:::validate_family("Beta")
binomial_family_default <- bayesnec:::validate_family("binomial")
poisson_family_default <- bayesnec:::validate_family("poisson")
negbinomial_family_default <- bayesnec:::validate_family("negbinomial")
gamma_family_default <- bayesnec:::validate_family("Gamma")
gaussian_family_default <- bayesnec:::validate_family("gaussian")

beta_family_identity <- bayesnec:::validate_family(Beta(link = "identity"))
binomial_family_identity <- bayesnec:::validate_family(binomial(link = "identity"))
poisson_family_identity <- bayesnec:::validate_family(poisson(link = "identity"))
negbinomial_family_identity <- bayesnec:::validate_family(negbinomial(link = "identity"))
gamma_family_identity <- bayesnec:::validate_family(Gamma(link = "identity"))

test_that("properly drops zero bounded models for logit and log links", {
  expect_equal(bayesnec:::check_models(c("nec3param", "nec4param", "ecxexp"), beta_family_default), "nec4param")
  expect_equal(bayesnec:::check_models(c("nec3param", "ecx4param", "ecxexp"), beta_family_default), "ecx4param") 
  expect_equal(bayesnec:::check_models(c("nec3param", "ecx4param", "ecxexp"), poisson_family_default), "ecx4param") 
  expect_equal(bayesnec:::check_models(c("nec3param", "ecx4param", "ecxexp"), beta_family_identity),  
               c("nec3param", "ecx4param", "ecxexp"))   
  #expect_warning(bayesnec:::check_models(c("nec3param", "nec4param", "ecxexp"), beta_family_default))  
})

test_that("when all models dropped an error is returned", {
  expect_error(bayesnec:::check_models(c("nec3param","ecxexp"), beta_family_default))
})

test_that("properly drops lin models for identity link for anything but gaussian", {
  expect_error(bayesnec:::check_models(c("neclin", "neclinhorme", "ecxlin"), beta_family_identity))
  expect_error(bayesnec:::check_models(c("neclin", "neclinhorme", "ecxlin"), binomial_family_identity)) 
  expect_error(bayesnec:::check_models(c("neclin", "neclinhorme", "ecxlin", "nechormepwr01"), poisson_family_identity)) 
  expect_error(bayesnec:::check_models(c("neclin", "neclinhorme", "ecxlin", "nechormepwr01"), negbinomial_family_identity)) 
  expect_error(bayesnec:::check_models(c("neclin", "neclinhorme", "ecxlin", "nechormepwr01"), gamma_family_identity)) 
  
  expect_equal(bayesnec:::check_models(c("neclin", "neclinhorme", "ecxlin"), gaussian_family_default),  
               c("neclin", "neclinhorme", "ecxlin"))   
})

test_that("properly drops zero bounded models for gaussian family", {
  expect_equal(bayesnec:::check_models(c("nec3param", "nec4param", "ecxexp"), gaussian_family_default), "nec4param")
})

