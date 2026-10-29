test_that("the zero-inflated count families are accepted, on identity mu", {
  for (tag in c("zero_inflated_poisson", "zero_inflated_negbinomial")) {
    fam <- validate_family(tag)
    expect_s3_class(fam, "family")
    expect_equal(fam$family, tag)
    # A family name without a chosen link defaults to identity for mu. top and
    # bot then use the response scale; nec uses the predictor scale. Leave the
    # constant mixing parameter zi on its family link.
    expect_equal(fam$link, "identity")
    expect_equal(fam$link_zi, "logit")
  }
})

test_that("the zero-inflated count families are not two-block families", {
  # Beta cannot emit zero, so its zero-inflated likelihood factorises. Poisson
  # and negative-binomial observations can be zero in either component, so
  # these mixtures must not enter the two-block machinery.
  for (tag in c("zero_inflated_poisson", "zero_inflated_negbinomial")) {
    expect_false(bayesnec:::is_hurdle_family(tag))
    expect_false(bayesnec:::is_hurdle_family(validate_family(tag)))
  }
  expect_true(bayesnec:::is_hurdle_family("zero_inflated_beta"))
  expect_true(bayesnec:::is_hurdle_family("hurdle_gamma"))
})

test_that("model restrictions follow the count family, not the mixture", {
  # The mu block is an ordinary count mean, so it takes the same restrictions
  # poisson and negbinomial do under an identity link.
  base <- suppressMessages(
    check_models(models()$all, validate_family(poisson(link = "identity")))
  )
  for (tag in c("zero_inflated_poisson", "zero_inflated_negbinomial")) {
    got <- suppressMessages(check_models(models()$all, validate_family(tag)))
    expect_setequal(got, base)
    expect_false("nechormepwr01" %in% got)
    expect_false("neclin" %in% got)
  }
})

test_that("priors come from the base count family", {
  set.seed(104)
  y <- as.numeric(c(rep(0, 20), rpois(80, 12)))
  x <- as.numeric(rep(1:10, each = 10))
  for (tag in c("zero_inflated_poisson", "zero_inflated_negbinomial")) {
    base_tag <- sub("^zero_inflated_", "", tag)
    zi_pr <- define_prior("nec4param", validate_family(tag), x, y)
    base_pr <- define_prior("nec4param", validate_family(base_tag), x, y)
    expect_s3_class(zi_pr, "brmsprior")
    expect_equal(zi_pr$prior, base_pr$prior)
    expect_equal(zi_pr$nlpar, base_pr$nlpar)
  }
})

test_that("model_survival is refused for the zero-inflated count families", {
  # model_survival requires a separate curve for the zero-probability block.
  for (tag in c("zero_inflated_poisson", "zero_inflated_negbinomial")) {
    expect_error(
      check_model_survival("nec3param", validate_family(tag)),
      "only applies to the two-block families"
    )
  }
})

test_that("bnec_hurdle refuses the zero-inflated count families", {
  dat <- data.frame(x = rep(1:4, each = 5),
                    y = as.integer(c(rep(3, 15), rep(0, 5))))
  err <- expect_error(
    bnec_hurdle(y ~ crf(x, "nec3param"), data = dat,
                family_growth = zero_inflated_poisson()),
    "cannot use zero_inflated_poisson"
  )
  expect_match(conditionMessage(err), "does not factorise")
  expect_match(conditionMessage(err), "bnec\\(family = \"zero_inflated_poisson\"\\)")
  # An untruncated count fit is not a replacement for a count hurdle model.
  expect_match(conditionMessage(err), "zero-truncated")
  expect_false(grepl("leave family_growth unset --", conditionMessage(err)))
  expect_error(
    bnec_hurdle(y ~ crf(x, "nec3param"), data = dat,
                family_growth = zero_inflated_negbinomial()),
    "cannot use zero_inflated_negbinomial"
  )
  expect_error(
    bnec_hurdle(y ~ crf(x, "nec3param"), data = dat,
                family_growth = "hurdle_gamma"),
    "already a two-block family"
  )
  # A plain count family is the hurdle-on-counts case. bnec_hurdle adds the
  # zero truncation after it has identified the positive subset.
  expect_error(
    bayesnec:::check_hurdle_growth_family(validate_family("poisson")),
    NA
  )
})

test_that("disp() is refused, with the reason that applies", {
  # zero_inflated_negbinomial does have a shape, so the generic "no free
  # dispersion parameter" message would be wrong for it.
  for (tag in c("zero_inflated_poisson", "zero_inflated_negbinomial")) {
    err <- expect_error(
      bayesnec:::check_disp_spec(list(route = "B", value = "power"),
                                 validate_family(tag)),
      "zero-inflated count"
    )
    expect_match(conditionMessage(err), "mixture")
    expect_false(grepl("no free dispersion parameter", conditionMessage(err)))
  }
})

test_that("a zero-inflated count response fits through bnec", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  set.seed(104)
  x <- rep(c(0, 1, 2, 3, 4, 5), each = 15)
  mu <- 40 * exp(-0.5 * (x - 2) * (x > 2))
  y <- rpois(length(x), mu) * rbinom(length(x), 1, 1 - 0.25)
  dat <- data.frame(x = x, y = as.integer(y))
  fit <- bnec(y ~ crf(x, "nec3param"), data = dat,
              family = "zero_inflated_poisson", iter = 600, warmup = 300,
              chains = 2, seed = 104, refresh = 0) |>
    suppressMessages() |>
    suppressWarnings()
  expect_s3_class(fit, "bayesnecfit")
  expect_equal(fit$fit$family$family, "zero_inflated_poisson")
  pars <- rownames(fixef(fit$fit))
  # The curve describes mu; zi is a constant mixing probability, not a second
  # concentration-response equation.
  expect_setequal(pars, c("top_Intercept", "beta_Intercept", "nec_Intercept"))
  expect_false(any(grepl("^zi(top|beta|nec)", pars)))
  expect_true("zi" %in% variables(fit$fit))
  expect_true(is.numeric(nec(fit)))
  expect_equal(nrow(predict(fit)), nrow(dat))
  expect_equal(nrow(fit$pred_vals$data), 1000)
  expect_error(suppressWarnings(summary(fit)), NA)
})
