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

test_that("keeps zero bounded models for the gaussian family (#206)", {
  # The inverse of the assertion that stood here. Up to 2.1.3 every
  # zero-bounded equation was dropped under gaussian on the grounds that it
  # "cannot generate predictions of negative response values", which confuses
  # the range of the mean function with the support of the likelihood. The
  # exclusion is removed; the candidate set now decides by weight.
  gaussian_family_default <- validate_family("gaussian")
  expect_equal(check_models(c("nec3param", "nec4param", "ecxexp"),
                            gaussian_family_default),
               c("nec3param", "nec4param", "ecxexp"))
  # A single zero-bounded equation named explicitly used to be an error,
  # because the set emptied.
  expect_equal(check_models("nec3param", gaussian_family_default), "nec3param",
               ignore_attr = TRUE)
  expect_silent(check_models(mod_groups$zero_bounded, gaussian_family_default))
  # The link exclusion is a different condition and still applies.
  gaussian_family_log <- validate_family(gaussian(link = "log"),
                                         link_source = "chosen")
  expect_false(
    "nec3param" %in% check_models(c("nec3param", "nec4param"),
                                  gaussian_family_log)
  )
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

test_that("models() filters equations by curve-parameter count (#301)", {
  expected <- c(
    ecxlin = 2L, ecxexp = 2L, nec3param = 3L, neclin = 3L,
    ecxsigm = 3L, ecxwb1p3 = 3L, ecxwb2p3 = 3L, ecxll3 = 3L,
    nec4param = 4L, nechorme = 4L, necsigm = 4L, neclinhorme = 4L,
    nechormepwr = 4L, nechormepwr01 = 4L, ecx4param = 4L,
    ecxwb1 = 4L, ecxwb2 = 4L, ecxll4 = 4L, ecxhormebc4 = 4L,
    nechorme4 = 5L, nechorme4pwr = 5L, ecxll5 = 5L,
    ecxhormebc5 = 5L
  )
  all_models <- models("all")
  observed <- vapply(
    all_models, function(x) length(names(x$pforms)), integer(1)
  )

  expect_equal(observed[names(expected)], expected)
  expect_setequal(names(models(max_pars = 3)), names(expected[expected <= 3]))
  expect_setequal(
    names(models("decline", max_pars = 3)),
    intersect(mod_groups$decline, names(expected[expected <= 3]))
  )
  expect_setequal(
    names(models(c(0, 1), max_pars = 3)),
    intersect(names(models(c(0, 1))), names(expected[expected <= 3]))
  )
})

test_that("models() validates parameter-count limits (#301)", {
  expect_error(models(max_pars = 1),
               "No model equations have 1 or fewer curve parameters")
  for (bad in list(0, -1, 2.5, Inf, NA_real_, c(2, 3), "3")) {
    expect_error(models(max_pars = bad),
                 "single positive whole number")
  }
})


test_that("check_models records which equations it excluded, and why (#261)", {
  # bnec() decided which equations it would not attempt, told the user once by
  # message(), and discarded the decision, so the composition of the candidate
  # set could not be recovered from the fit -- only from console output, which
  # a knitted document or a suppressMessages() call does not keep.
  beta_identity <- validate_family(Beta(link = "identity"),
                                   link_source = "chosen")
  kept <- suppressMessages(check_models(mod_groups$all, beta_identity,
                                       record = TRUE))
  rec <- attr(kept, "excluded")
  expect_s3_class(rec, "data.frame")
  expect_named(rec, c("model", "reason"))
  # The record and the returned set partition the requested set exactly.
  expect_setequal(c(as.character(kept), rec$model), mod_groups$all)
  expect_length(intersect(as.character(kept), rec$model), 0)
  # Every excluded equation carries a non-empty reason.
  expect_true(all(nzchar(rec$reason)))
  expect_true("neclin" %in% rec$model)
  expect_match(rec$reason[rec$model == "neclin"], "unbounded below")
})

test_that("an empty exclusion record is still a data frame", {
  # The edge case: nothing dropped. A NULL here would make every downstream
  # nrow() and rbind() conditional.
  gaussian_identity <- validate_family("gaussian")
  kept <- suppressMessages(check_models(c("nec4param", "ecx4param"),
                                        gaussian_identity, record = TRUE))
  rec <- attr(kept, "excluded")
  expect_s3_class(rec, "data.frame")
  expect_equal(nrow(rec), 0)
})

test_that("ecxhormebc5 is declined only where negative x can invalidate mu", {
  negative <- data.frame(y = c(3, 2, 1), conc = exp(c(-2, 0, 2)))
  negative <- model.frame(
    bnf(y ~ crf(log(conc), "ecxhormebc5")), negative
  )
  requested <- c("ecxhormebc4", "ecxhormebc5", "nec4param")
  gamma_identity <- validate_family(Gamma(link = "identity"),
                                    link_source = "chosen")

  expect_message(
    kept <- check_models(requested, gamma_identity, negative, record = TRUE),
    "cannot be reliably initialised.*predictor \\(x\\) contains negative"
  )
  expect_equal(as.character(kept), c("ecxhormebc4", "nec4param"))
  excluded <- attr(kept, "excluded")
  expect_equal(excluded$model, "ecxhormebc5")
  expect_match(excluded$reason, "cannot be reliably initialised")
  beta_identity <- validate_family(Beta(link = "identity"),
                                   link_source = "chosen")
  expect_equal(
    suppressMessages(check_models(requested, beta_identity, negative)),
    c("ecxhormebc4", "nec4param")
  )

  non_negative <- data.frame(y = c(3, 2, 1), conc = exp(c(0, 1, 2)))
  non_negative <- model.frame(
    bnf(y ~ crf(log(conc), "ecxhormebc5")), non_negative
  )
  expect_equal(
    suppressMessages(
      check_models(c("ecxhormebc4", "ecxhormebc5"), gamma_identity,
                   non_negative)
    ),
    c("ecxhormebc4", "ecxhormebc5")
  )

  # These cases do not expose a positive-support likelihood to a negative
  # linear predictor, so the new exclusion must not broaden to them.
  expect_silent(check_models("ecxhormebc5", validate_family("gaussian"),
                             negative))
  gamma_log <- validate_family(Gamma(link = "log"), link_source = "chosen")
  expect_silent(check_models("ecxhormebc5", gamma_log, negative))
})
