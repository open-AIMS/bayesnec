test_that("properly drops zero bounded models for logit and log links", {
  beta_family_logit <- validate_family(Beta(link="logit"), link_source = "chosen")
  binomial_family_logit <- validate_family(binomial(link = "logit"), link_source = "chosen")
  poisson_family_log <- validate_family(poisson(link = "log"), link_source = "chosen")
  negbinomial_family_log <- validate_family(negbinomial(link = "log"), link_source = "chosen")
  gamma_family_log <- validate_family(Gamma(link = "log"), link_source = "chosen")
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
  beta_family_logit <- validate_family(Beta(link = "logit"), link_source = "chosen")
  expect_error(check_models(c("nec3param", "ecxexp"), beta_family_logit))
})

test_that(paste0("properly drops lin models for identity link for",
                 " anything but Gaussian"), {
  beta_family_identity <- validate_family(Beta(link = "identity"), link_source = "chosen")
  binomial_family_identity <- validate_family(binomial(link = "identity"), link_source = "chosen")
  poisson_family_identity <- validate_family(poisson(link = "identity"), link_source = "chosen")
  negbin_family_identity <- validate_family(negbinomial(link = "identity"), link_source = "chosen")
  gamma_family_identity <- validate_family(Gamma(link = "identity"), link_source = "chosen")
  
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

test_that("models() and check_models() agree for every response range", {
  # The drift-proof assertion for #170: whatever ?models advertises as available
  # for a response range must be exactly what bnec() will fit for a family with
  # that range. Every family sharing a range is listed, because the equivalence
  # is the reason one representative per range is enough in range_to_family().
  all_mod <- models()$all
  ranges <- list(
    "c(-Inf, Inf)" = list(range = c(-Inf, Inf),
                          families = list(validate_family("gaussian"))),
    "c(0, 1)" = list(
      range = c(0, 1),
      families = list(validate_family(Beta(link = "identity"), link_source = "chosen"),
                      validate_family(binomial(link = "identity"), link_source = "chosen"),
                      validate_family(beta_binomial(link = "identity"), link_source = "chosen"),
                      validate_family(bernoulli(link = "identity"), link_source = "chosen"))
    ),
    "c(0, Inf)" = list(
      range = c(0, Inf),
      families = list(validate_family(Gamma(link = "identity"), link_source = "chosen"),
                      validate_family(poisson(link = "identity"), link_source = "chosen"),
                      validate_family(negbinomial(link = "identity"), link_source = "chosen"))
    )
  )
  for (lab in names(ranges)) {
    advertised <- names(models(ranges[[lab]]$range))
    for (fam in ranges[[lab]]$families) {
      fitted_set <- suppressMessages(check_models(all_mod, fam))
      expect_setequal(advertised, fitted_set)
    }
  }
})

test_that("models() rejects ranges and names it cannot map", {
  # These used to fail with "object 'use_mods' not found".
  expect_error(models(c(0, 100)), "one of the response ranges")
  expect_error(models("nonsense"), "must be a bayesnecfit")
  # Every model group bnec(model = ) accepts is listable.
  for (grp in names(models())) {
    expect_setequal(names(models(grp)), models()[[grp]])
  }
})
