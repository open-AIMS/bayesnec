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

  # The same closed forms correct the factorised base-family predictions.
  expect_equal(
    bayesnec:::hurdle_positive_mean(mu, "poisson"), expected_pois
  )
  expect_equal(
    bayesnec:::hurdle_positive_mean(mu, "negbinomial", shape), expected_nb
  )
})

test_that("positive-count variances use the zero-truncated distribution", {
  mu <- matrix(c(0, 0.5, 3, 10), nrow = 2)
  q_pois <- 1 - exp(-mu)
  expected_pois <- (mu + mu^2) / q_pois - (mu / q_pois)^2
  expected_pois[1] <- 0
  expect_equal(
    bayesnec:::hurdle_positive_variance(mu, "poisson"), expected_pois
  )

  shape <- matrix(c(0.8, 2, 5, 10), nrow = 2)
  q_nb <- 1 - (shape / (shape + mu))^shape
  expected_nb <- (mu + mu^2 / shape + mu^2) / q_nb -
    (mu / q_nb)^2
  expected_nb[1] <- 0
  expect_equal(
    bayesnec:::hurdle_positive_variance(mu, "negbinomial", shape),
    expected_nb
  )
})

test_that("factorised count epred uses the exact positive mean", {
  formula <- bayesnec:::add_hurdle_truncation(
    bnf(y ~ crf(x, "nec3param"))
  )
  fit <- list(family = validate_family("negbinomial"))
  mu <- matrix(c(0.1, 0.5, 3, 3), nrow = 2)
  shape <- matrix(c(0.5, 0.5, 0.5, 2), nrow = 2)
  local_mocked_bindings(
    posterior_epred = function(object, dpar, ...) {
      switch(dpar, mu = mu, shape = shape)
    },
    .package = "bayesnec"
  )

  got <- bayesnec:::factorised_count_epred(fit, formula)
  expect_equal(
    got, bayesnec:::hurdle_positive_mean(mu, "negbinomial", shape)
  )
  expect_true(all(is.finite(got)))
  expect_gt(got[1, 2], 4.8)
})

test_that("factorised negative-binomial epred pairs mu and shape draws", {
  formula <- bayesnec:::add_hurdle_truncation(
    bnf(y ~ crf(x, "nec3param"))
  )
  fit <- list(family = validate_family("negbinomial"))
  seen_ids <- list()
  local_mocked_bindings(
    ndraws = function(object) 6L,
    posterior_epred = function(object, dpar, ndraws = NULL,
                               draw_ids = NULL, ...) {
      seen_ids[[dpar]] <<- draw_ids
      matrix(draw_ids, ncol = 1)
    },
    .package = "bayesnec"
  )

  set.seed(209)
  bayesnec:::factorised_count_epred(fit, formula, ndraws = 3)
  expect_length(seen_ids$mu, 3)
  expect_identical(seen_ids$mu, seen_ids$shape)
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

test_that("factorised count hurdles refuse censoring before fitting", {
  dat <- data.frame(
    x = as.numeric(rep(0:3, each = 3)),
    y = as.integer(c(8, 7, 9, 5, 6, 4, 2, 1, 3, 0, 0, 0)),
    cens = "none"
  )
  local_mocked_bindings(
    check_count_truncation_support = function(...) invisible(NULL),
    .package = "bayesnec"
  )
  expect_error(
    bnec_hurdle(y | cens(cens) ~ crf(x, "nec3param"), data = dat),
    "cannot combine cens\\(\\) with a count growth family.*does not condition"
  )
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

test_that("relative count-hurdle asymptotes use the positive-count scale", {
  object <- list(
    fit = list(family = validate_family("hurdle_poisson")),
    bayesnecformula = bnf(y ~ crf(x, "nec3param"))
  )
  expect_equal(
    bayesnec:::count_positive_asymptote(object, "mu", 0,
                                        data.frame(x = 0)),
    1
  )
  bot <- c(0.5, 2)
  expect_equal(
    bayesnec:::count_positive_asymptote(object, "mu", bot,
                                        data.frame(x = 0)),
    bayesnec:::hurdle_positive_mean(bot, "hurdle_poisson")
  )

  object$fit$family <- validate_family("hurdle_negbinomial")
  shape <- matrix(c(0.5, 2), ncol = 1)
  local_mocked_bindings(
    posterior_epred = function(object, newdata, re_formula, dpar) shape,
    .package = "bayesnec"
  )
  expect_equal(
    bayesnec:::count_positive_asymptote(object, "mu", bot,
                                        data.frame(x = 0)),
    as.numeric(bayesnec:::hurdle_positive_mean(
      bot, "hurdle_negbinomial", shape
    ))
  )

  object$fit$family <- validate_family("poisson")
  object$bayesnecformula <- bayesnec:::add_hurdle_truncation(
    bnf(y ~ crf(x, "nec3param"))
  )
  expect_equal(
    bayesnec:::count_positive_asymptote(object, NULL, bot,
                                        data.frame(x = 0)),
    as.numeric(bayesnec:::hurdle_positive_mean(bot, "poisson"))
  )
})

test_that("count asymptotes apply a non-identity mean link first", {
  bot_eta <- log(c(0.5, 2))
  expected <- as.numeric(
    bayesnec:::hurdle_positive_mean(exp(bot_eta), "poisson")
  )
  joint <- list(
    fit = list(family = brms::hurdle_poisson(link = "log")),
    bayesnecformula = bnf(y ~ crf(x, "nec3param"))
  )
  expect_equal(
    bayesnec:::count_positive_asymptote(
      joint, "mu", bot_eta, data.frame(x = 0)
    ),
    expected
  )

  factorised <- list(
    fit = list(family = stats::poisson(link = "log")),
    bayesnecformula = bayesnec:::add_hurdle_truncation(
      bnf(y ~ crf(x, "nec3param"))
    )
  )
  expect_equal(
    bayesnec:::count_positive_asymptote(
      factorised, NULL, bot_eta, data.frame(x = 0)
    ),
    expected
  )
  # An equation without bot tends to zero on the link scale, hence to one on
  # the mean scale under a log link.
  expect_equal(
    bayesnec:::count_positive_asymptote(
      factorised, NULL, 0, data.frame(x = 0)
    ),
    as.numeric(bayesnec:::hurdle_positive_mean(1, "poisson"))
  )
})

test_that("model-averaged count asymptotes retain their draw pairing", {
  count_formula <- bayesnec:::add_hurdle_truncation(
    bnf(y ~ crf(x, "nec3param"))
  )
  parts <- lapply(c("a", "b"), function(model) {
    structure(list(
      model = model,
      fit = list(family = validate_family("poisson")),
      bayesnecformula = count_formula
    ), class = c("bayesnecfit", "bnecfit"))
  })
  names(parts) <- c("a", "b")
  object <- structure(list(
    mod_fits = list(a = list(fit = 1), b = list(fit = 2))
  ), class = c("bayesmanecfit", "bnecfit"))
  asymptote <- c(0.2, 0.5, 1, 2)
  draw_index <- list(a = c(1L, 3L), b = c(2L, 4L))
  local_mocked_bindings(
    as_draws_df = function(x) data.frame(draw = seq_along(asymptote)),
    pull_draw_index = function(...) draw_index,
    pull_out = function(object, model) parts[[model]],
    ecx_asymptote = function(object, type) asymptote,
    .package = "bayesnec"
  )

  got <- bayesnec:::count_positive_asymptote(
    object, NULL, rep(0, 4), data.frame(x = 0)
  )
  transformed <- as.numeric(
    bayesnec:::hurdle_positive_mean(asymptote, "poisson")
  )
  expect_equal(got, c(transformed[draw_index$a],
                      transformed[draw_index$b]))
})

test_that("a real factorised count fit reports the exact positive mean", {
  skip_on_cran()
  skip_if(packageVersion("brms") < "2.23.2",
          "corrected brms count truncation is not installed")
  set.seed(209)
  dat <- data.frame(x = as.numeric(rep(0:4, each = 8)))
  dat$y <- as.integer(rpois(
    nrow(dat), lambda = pmax(0.25, 8 - 1.7 * dat$x)
  ))
  dat$y[dat$x == 4 & seq_len(nrow(dat)) %% 2 == 0] <- 0L
  fit <- suppressMessages(suppressWarnings(
    bnec_hurdle(
      y ~ crf(x, "nec3param"), data = dat,
      family_growth = stats::poisson(), chains = 1, iter = 120,
      warmup = 60, seed = 209, refresh = 0
    )
  ))
  mu <- brms::posterior_epred(fit$growth$fit, dpar = "mu")
  exact <- bayesnec:::hurdle_positive_mean(mu, "poisson")
  expect_equal(posterior_epred(fit$growth), exact)
  expect_equal(fitted(fit$growth, summary = FALSE), exact)
})
