# Count hurdles use two independent blocks only when every zero is known to be
# structural. Their positive count block is therefore fitted on Y >= 1.

test_that("count hurdle families are registered as two-block", {
  for (family in c("hurdle_poisson", "hurdle_negbinomial")) {
    expect_true(bayesnec:::is_hurdle_family(family))
    expect_identical(bayesnec:::hurdle_dpar(family), "hu")
    expect_identical(validate_family(family)$link, "identity")
    expect_equal(bayesnec:::mu_support(validate_family(family)), c(0, Inf))
    expect_equal(
      bayesnec:::mu_support(validate_family(family), dpar = "hu"), c(0, 1)
    )
  }
})

test_that("count hurdle mu blocks use their base count families", {
  expect_identical(
    bayesnec:::hurdle_mu_family("hurdle_poisson")$family, "poisson"
  )
  expect_identical(
    bayesnec:::hurdle_mu_family("hurdle_negbinomial")$family, "negbinomial"
  )
  expect_identical(
    bayesnec:::hurdle_mu_family("hurdle_poisson")$link, "identity"
  )
})

test_that("count hurdle model restrictions follow the mu block", {
  for (family in c("hurdle_poisson", "hurdle_negbinomial")) {
    keep <- suppressMessages(
      check_models(
        c("nec3param", "nec4param", "neclin", "nechormepwr01"),
        validate_family(family)
      )
    )
    expect_setequal(keep, c("nec3param", "nec4param"))
  }
})

test_that("joint count growth predictions are conditional on being positive", {
  mu <- matrix(c(0, 0.5, 3, 10), nrow = 2)
  got_pois <- bayesnec:::hurdle_positive_mean(mu, "hurdle_poisson")
  expected_pois <- mu / (1 - exp(-mu))
  expected_pois[1] <- 1
  expect_equal(got_pois, expected_pois)

  shape <- matrix(c(0.8, 2, 5, 10), nrow = 2)
  got_nb <- bayesnec:::hurdle_positive_mean(
    mu, "hurdle_negbinomial", shape
  )
  p_zero <- (shape / (shape + mu))^shape
  expected_nb <- mu / (1 - p_zero)
  expected_nb[1] <- 1
  expect_equal(got_nb, expected_nb)
})

test_that("joint hurdle block predictions use the documented scales", {
  object <- list(fit = list(family = validate_family("hurdle_negbinomial")))
  mu <- matrix(c(0.5, 2), nrow = 1)
  shape <- matrix(c(3, 4), nrow = 1)
  hu <- matrix(c(0.1, 0.7), nrow = 1)
  combined <- matrix(c(1.2, 0.8), nrow = 1)
  local_mocked_bindings(
    posterior_epred = function(object, newdata, re_formula, dpar = NULL) {
      if (is.null(dpar)) combined else switch(dpar, mu = mu, shape = shape,
                                              hu = hu)
    },
    .package = "bayesnec"
  )

  expect_equal(
    bayesnec:::joint_hurdle_epred(object, data.frame(x = 1:2)), combined
  )
  expect_equal(
    bayesnec:::joint_hurdle_epred(object, data.frame(x = 1:2), "hu"), 1 - hu
  )
  expect_equal(
    bayesnec:::joint_hurdle_epred(object, data.frame(x = 1:2), "mu"),
    bayesnec:::hurdle_positive_mean(mu, "hurdle_negbinomial", shape)
  )
})

test_that("the internal count truncation preserves existing aterms", {
  dat <- data.frame(x = 1:4, y = 1:4, cens = "none")
  formula <- bnf(y | cens(cens) ~ crf(x, "nec3param"))
  out <- bayesnec:::add_hurdle_truncation(formula)

  expect_match(deparse1(formula.tools::lhs(out)), "cens\\(cens\\)", perl = TRUE)
  expect_match(deparse1(formula.tools::lhs(out)), "trunc\\(lb = 1\\)", perl = TRUE)
  expect_true(isTRUE(attr(out, "bayesnec_internal_truncation")))
  expect_no_error(check_formula(out, dat))

  one_model <- bayesnec:::single_model_formula(out, "nec3param")
  expect_true(isTRUE(attr(one_model, "bayesnec_internal_truncation")))
  expect_no_error(check_formula(one_model, dat))
})

test_that("factorised counts require corrected brms truncation semantics", {
  expect_error(
    bayesnec:::check_count_truncation_support(package_version("2.23.1")),
    "brms 2.23.2 or later.*inclusive lower bound"
  )
  expect_no_error(
    bayesnec:::check_count_truncation_support(package_version("2.23.2"))
  )
})

test_that("truncation remains internal to bnec_hurdle", {
  dat <- data.frame(x = 1:4, y = 1:4)
  expect_error(
    check_formula(bnf(y | trunc(lb = 1) ~ crf(x, "nec3param")), dat),
    "does not support.*trunc\\(lb = 1\\)"
  )
})

test_that("bnec_hurdle adds truncation on automatic and supplied count paths", {
  dat <- data.frame(
    x = rep(0:3, each = 3),
    y = as.integer(c(8, 7, 9, 5, 6, 4, 2, 1, 3, 0, 0, 0))
  )
  calls <- list()
  local_mocked_bindings(
    check_count_truncation_support = function(...) invisible(NULL),
    bnec = function(formula, data, family, ...) {
      calls[[length(calls) + 1L]] <<- list(formula = formula, family = family)
      structure(list(), class = c("bayesnecfit", "bnecfit"))
    },
    .package = "bayesnec"
  )

  suppressMessages(
    bnec_hurdle(y ~ crf(x, "nec3param"), data = dat)
  )
  expect_identical(calls[[1]]$family$family, "poisson")
  expect_match(deparse1(formula.tools::lhs(calls[[1]]$formula)),
               "trunc\\(lb = 1\\)")
  expect_false(grepl("trunc", deparse1(calls[[2]]$formula), fixed = TRUE))

  calls <- list()
  suppressMessages(
    bnec_hurdle(y ~ crf(x, "nec3param"), data = dat,
                family_growth = brms::negbinomial())
  )
  expect_identical(calls[[1]]$family$family, "negbinomial")
  expect_match(deparse1(formula.tools::lhs(calls[[1]]$formula)),
               "trunc\\(lb = 1\\)")
})

test_that("zero-inflated counts stay off the two-block path", {
  for (family in c("zero_inflated_poisson", "zero_inflated_negbinomial")) {
    expect_false(bayesnec:::is_hurdle_family(family))
    expect_error(
      bayesnec:::check_hurdle_growth_family(validate_family(family)),
      "does not factorise"
    )
  }
  expect_no_error(
    bayesnec:::check_hurdle_growth_family(validate_family("poisson"))
  )
  expect_error(
    bayesnec:::check_hurdle_growth_family(validate_family("hurdle_poisson")),
    "already a two-block family"
  )
})
