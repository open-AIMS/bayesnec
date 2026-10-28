# Under the default identity link, the Poisson mean is the rate times
# exposure. Response asymptotes are rates; nec remains on the predictor
# scale. Rate terms must survive parsing, priors and prediction.

rate_data <- function(seed = 1) {
  set.seed(seed)
  x <- rep(seq(0, 10, length.out = 10), 4)
  ex <- rep(c(1, 2, 4, 8), each = 10)          # deliberately varying exposure
  nec3 <- function(beta, nec, top, x) {
    top * exp(-exp(beta) * (x - nec) * (x > nec))
  }
  data.frame(x = x, ex = ex,
             y = as.integer(rpois(40, nec3(-0.5, 4, 20, x) * ex)))
}

test_that("a rate term is parsed and carried into the model frame", {
  d <- rate_data()
  mf <- model.frame(bnf(y | rate(ex) ~ crf(x, "nec3param")), data = d)
  pop <- attr(mf, "bnec_pop")
  expect_equal(unname(pop[names(pop) == "rate_var"]), "ex")
  expect_true("ex" %in% names(mf))
})

test_that("retrieve_var finds the rate variable by position", {
  # retrieve_var() indexes by position: short_form term order and pop_vars
  # name order must agree. Include censoring, the slot immediately before rate.
  d <- rate_data()
  d$cens <- rep(c("none", "left"), 20)
  mf <- model.frame(bnf(y | rate(ex) + cens(cens) ~ crf(x, "nec3param")),
                    data = d)
  expect_identical(bayesnec:::retrieve_var(mf, "rate_var"), d$ex)
  expect_identical(bayesnec:::retrieve_var(mf, "y_var"), d$y)
  expect_identical(bayesnec:::retrieve_var(mf, "x_var"), d$x)
})

test_that("priors are built on the rate scale, not the count scale", {
  # Derive top from rates; using raw counts folds exposure into its prior.
  d <- rate_data()
  pr <- get_priors(y | rate(ex) ~ crf(x, "nec3param"), data = d,
                   family = "poisson")
  rate <- as.numeric(sub("^gamma\\([^,]+,\\s*([^)]+)\\)$", "\\1",
                         pr$prior[pr$nlpar == "top"]))
  # The true top is 20. These broad bounds on gamma mean 2 / rate
  # detect exposure contamination without pinning a noisy prior constant.
  expect_lt(2 / rate, 40)
  expect_gt(2 / rate, 2)
})

test_that("the prediction grid pins the denominator at 1", {
  # Prediction grids need denominator 1 to describe the rate curve.
  # A missing denominator breaks posterior_epred(); varying exposure
  # would change the curve used by ecx() and nsec().
  d <- rate_data()
  mf <- model.frame(bnf(y | rate(ex) ~ crf(x, "nec3param")), data = d)
  fake <- list(data = d, family = list(family = "poisson"))
  grid <- bayesnec:::prediction_grid(fake,
                                     bnf(y | rate(ex) ~ crf(x, "nec3param")),
                                     resolution = 7)
  expect_true("ex" %in% names(grid$newdata))
  expect_identical(unique(grid$newdata$ex), 1)
  expect_equal(nrow(grid$newdata), 7)
})

test_that("rate is refused for families brms cannot fit it with", {
  d <- rate_data()
  f <- bnf(y | rate(ex) ~ crf(x, "nec3param"))
  mf <- model.frame(f, data = d)
  for (fam in c("gaussian", "Gamma", "binomial")) {
    expect_error(
      bayesnec:::wrangle_model_formula("nec3param", f, mf,
                                       family = validate_family(fam)),
      "only valid for the poisson and negbinomial families"
    )
  }
})

test_that("the zero-inflated count families get their own message", {
  # They are what a user reaches for next, and brms genuinely cannot do it
  # there -- nor is there an offset workaround under an identity link.
  d <- rate_data()
  f <- bnf(y | rate(ex) ~ crf(x, "nec3param"))
  mf <- model.frame(f, data = d)
  expect_error(
    bayesnec:::wrangle_model_formula(
      "nec3param", f, mf,
      family = validate_family("zero_inflated_poisson")),
    "zero-inflated"
  )
})

test_that("poisson and negbinomial are accepted", {
  d <- rate_data()
  f <- bnf(y | rate(ex) ~ crf(x, "nec3param"))
  mf <- model.frame(f, data = d)
  for (fam in c("poisson", "negbinomial")) {
    expect_s3_class(
      bayesnec:::wrangle_model_formula("nec3param", f, mf,
                                       family = validate_family(fam)),
      "brmsformula"
    )
  }
})

test_that("bnec_hurdle refuses a rate term with a reason", {
  # Only one component is a count; the survival component is one Bernoulli
  # trial per individual and has no rate to take a denominator.
  d <- rate_data()
  expect_error(
    bayesnec:::check_hurdle_aterms(bnf(y | rate(ex) ~ crf(x, "nec3param"))),
    "rate"
  )
})

test_that("an unrecognised aterm is an error, not a message", {
  # Unsupported additional terms must error before fitting; passing them
  # through can defer failure until expensive post-processing.
  d <- rate_data()
  expect_error(
    model.frame(bnf(y | se(ex) ~ crf(x, "nec3param")), data = d),
    "aterms bayesnec does not support"
  )
  # the four validated ones must not trip it
  d$tr <- 10L
  d$cens <- "none"
  expect_no_error(
    model.frame(bnf(y | rate(ex) ~ crf(x, "nec3param")), data = d)
  )
})

test_that("a rate denominator written as an expression is refused (#389)", {
  # Require a bare rate column: retrieve_var() reduces rate(exh / 24) to
  # exh while brms retains the expression. Otherwise priors, initial values,
  # prediction grids and plotting would use inconsistent denominators.
  d <- rate_data()
  d$exh <- d$ex * 24
  expect_error(model.frame(bnf(y | rate(exh / 24) ~ crf(x, "nec3param")),
                           data = d),
               "must be a column of the data")
  expect_error(bnf(y | rate(ex * 2) ~ crf(x, "nec3param")) |>
                 model.frame(data = d),
               "rate\\(ex \\* 2\\)")
  # the bare column is untouched
  expect_no_error(
    model.frame(bnf(y | rate(exh) ~ crf(x, "nec3param")), data = d)
  )
})

# Dispersion centring on the scale of the mean

# The constants are spliced into the variance function as "- <literal>)", which
# no curve expression contains, so this reads them back in order of appearance.
centring_literals <- function(rhs) {
  m <- regmatches(rhs, gregexpr("- -?[0-9.]+\\)", rhs))[[1]]
  as.numeric(gsub("^- |\\)$", "", m))
}

rate_shape_rhs <- function(formula, d) {
  bf <- make_brmsformula(formula, d, family = "negbinomial")[[1]]
  deparse1(bf$pforms$shape[[3]])
}

test_that("a rate fit centres its variance function on the rate (#397)", {
  # With an identity link, centre rate-model dispersion on the rates.
  # Varying exposure separates these references from count-based centres.
  d <- rate_data()
  rate <- d$y / d$ex
  # The exposures differ, so the two scales give different references and the
  # assertions below can tell them apart.
  expect_false(median(log(rate[rate > 0])) == median(log(d$y[d$y > 0])))
  expect_false(median(rate) == median(d$y))
  pw <- rate_shape_rhs(y | rate(ex) ~ crf(x, "nec3param") + disp("power"), d)
  expect_equal(centring_literals(pw),
               signif(median(log(rate[rate > 0])), 6))
  # loglinear centres on the median itself rather than the geometric median,
  # so the division is asserted on both forms of the reference
  ll <- rate_shape_rhs(y | rate(ex) ~ crf(x, "nec3param") + disp("loglinear"),
                       d)
  expect_equal(centring_literals(ll), signif(median(rate), 6))
})

test_that("the rate-scale literal reaches the Stan program (#397)", {
  # bnec() and make_brmsformula() share the formula builder, so the literal
  # asserted above is the one a fit compiles.
  d <- rate_data()
  bf <- make_brmsformula(y | rate(ex) ~ crf(x, "nec3param") + disp("power"), d,
                         family = validate_family("negbinomial"))[[1]]
  sc <- brms::make_stancode(bf, data = d,
                            family = brms::negbinomial(link = "identity"))
  ref <- format(signif(median(log((d$y / d$ex)[d$y > 0])), 6),
                scientific = FALSE)
  expect_match(sc, paste0("- ", ref, ")"), fixed = TRUE)
})

test_that("without a rate term the centre is still taken from the counts", {
  # Without rate(), centre on counts as the mean scale requires.
  d <- rate_data()
  pw <- rate_shape_rhs(y ~ crf(x, "nec3param") + disp("power"), d)
  expect_equal(centring_literals(pw), signif(median(log(d$y[d$y > 0])), 6))
  ll <- rate_shape_rhs(y ~ crf(x, "nec3param") + disp("loglinear"), d)
  expect_equal(centring_literals(ll), signif(median(d$y), 6))
})
