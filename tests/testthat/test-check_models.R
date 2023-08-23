library(bayesnec)

test_that("properly drops zero bounded models for logit and log links", {
  beta_family_logit <- validate_family(Beta(link="logit"))
  binomial_family_logit <- validate_family(binomial(link = "logit"))
  poisson_family_log <- validate_family(poisson(link = "log"))
  negbinomial_family_log <- validate_family(negbinomial(link = "log"))
  gamma_family_log <- validate_family(Gamma(link = "log"))
  gaussian_family_default <- validate_family("gaussian")
  
  expect_equal(check_models(c("nec3param", "nec4param", "ecxexp"),
                   beta_family_logit), "nec4param")
  expect_equal(check_models(c("nec3param", "ecx4param", "ecxexp"),
                   beta_family_logit), "ecx4param")
  expect_equal(check_models(c("nec3param", "ecx4param", "ecxexp"),
                   poisson_family_log), "ecx4param")
  expect_equal(check_models(c("nec3param", "ecx4param", "ecxexp"),
                   beta_family_logit),  "ecx4param")

  
  })

test_that("when all models dropped an error is returned", {
  beta_family_logit <- validate_family(Beta(link = "logit"))
  expect_error(check_models(c("nec3param", "ecxexp"), beta_family_logit))
})

test_that(paste0("properly drops lin models for identity link for",
                 " anything but Gaussian"), {
  beta_family_identity <- validate_family(Beta(link = "identity"))
  binomial_family_identity <- validate_family(binomial(link = "identity"))
  poisson_family_identity <- validate_family(poisson(link = "identity"))
  negbin_family_identity <- validate_family(negbinomial(link = "identity"))
  gamma_family_identity <- validate_family(Gamma(link = "identity"))
  
  expect_error(check_models(c("neclin", "neclinhorme", "ecxlin"),
                   beta_family_identity))
  expect_error(check_models(c("neclin", "neclinhorme", "ecxlin"),
                   binomial_family_identity))
  expect_error(check_models(c("neclin", "neclinhorme", "ecxlin",
                   "nechormepwr01"), poisson_family_identity))
  expect_error(check_models(c("neclin", "neclinhorme", "ecxlin",
                   "nechormepwr01"), negbin_family_identity))
  expect_error(check_models(c("neclin", "neclinhorme", "ecxlin",
                   "nechormepwr01"), gamma_family_identity))
  gaussian_family_default <- validate_family("gaussian")
  expect_equal(check_models(c("neclin", "neclinhorme", "ecxlin"),
                            gaussian_family_default),
               c("neclin", "neclinhorme", "ecxlin"))
})

test_that("properly drops zero bounded models for Gaussian family", {
  gaussian_family_default <- validate_family("gaussian")
  expect_equal(check_models(c("nec3param", "nec4param", "ecxexp"),
                            gaussian_family_default), "nec4param")
})
