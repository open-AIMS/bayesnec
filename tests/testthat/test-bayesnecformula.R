test_that("bayesnecformula is agnostic with respect to the formula structure", {
  expect_identical(bayesnecformula(y ~ crf(x, "nec3param")),
                   bnf(y ~ crf(x, "nec3param")))
  expect_s3_class(bnf(log(y) | trials(tr) ~ crf(sqrt(x), "nec3param")),
                  "bayesnecformula")
  expect_s3_class(bnf(log(y) | trials(tr) ~ crf(sqrt(x), "nec3param")),
                  "formula")
  expect_s3_class(bnf(y ~ x), "formula")
  expect_s3_class(bnf(y ~ x), "bayesnecformula")
  expect_s3_class(bnf(0 ~ x), "bayesnecformula")
  expect_s3_class(bnf("0 ~ x"), "bayesnecformula")
  expect_s3_class(bnf(y ~ crf(scale(x, scale = TRUE), "nec3param")),
                  "bayesnecformula")
})

test_that("bayesnecformula is equivalent to bnf", {
  expect_identical(bayesnecformula(y ~ crf(x, "nec3param")),
                   bnf(y ~ crf(x, "nec3param")))
  expect_identical(bayesnecformula(y ~ crf(x, "nec3param")),
                   bnf('y ~ crf(x, "nec3param")'), ignore_formula_env = TRUE)
})


# ---- #257, the group-level deviation reaches the brms formula ----------------

ogl_fixture <- function() {
  set.seed(1)
  d <- data.frame(x = rep(c(1, 5, 20, 100), each = 8),
                  g = rep(letters[1:4], 8))
  d$y <- pmin(pmax(stats::rnorm(32, 0.9 - 0.8 * (d$x > 5), 0.05), 0.01), 0.99)
  d
}

test_that("a constrained mean gets the deviation multiplicatively (#257)", {
  d <- ogl_fixture()
  f <- bnf(y ~ crf(x, model = "nec3param") + ogl(g))
  bdat <- model.frame(f, data = d, run_par_checks = TRUE)
  bfm <- wrangle_model_formula("nec3param", f, bdat, validate_family("Beta"))
  rhs_txt <- deparse1(bfm$formula[[3]])
  # The curve becomes an intermediate and the deviation multiplies it.
  expect_match(rhs_txt, "bnecmu", fixed = TRUE)
  expect_match(rhs_txt, "exp(ogl)", fixed = TRUE)
  expect_true("bnecmu" %in% names(bfm$pforms))
  # The intermediate is a transformation, not a parameter, so it carries the
  # nl flag brms uses to tell those apart. Without it brms would demand a
  # prior for it and the fit would not build.
  expect_true(isTRUE(attr(bfm$pforms$bnecmu, "nl")))
  # The curve itself is unchanged, just relocated.
  expect_match(deparse1(bfm$pforms$bnecmu[[3]]), "top", fixed = TRUE)
  expect_match(deparse1(bfm$pforms$bnecmu[[3]]), "nec", fixed = TRUE)
  # And the additive form is gone.
  expect_false(grepl("ogl + top", rhs_txt, fixed = TRUE))
})

test_that("an unconstrained mean keeps the additive offset (#257)", {
  # gaussian is unconstrained, so there is nothing to protect against; #245
  # measured 0% divergent for exactly this case with the identical curve and
  # grouping.
  d <- ogl_fixture()
  f <- bnf(y ~ crf(x, model = "nec3param") + ogl(g))
  bdat <- model.frame(f, data = d, run_par_checks = TRUE)
  bfm <- wrangle_model_formula("nec3param", f, bdat,
                               validate_family("gaussian"))
  rhs_txt <- deparse1(bfm$formula[[3]])
  expect_match(rhs_txt, "ogl + ", fixed = TRUE)
  expect_false("bnecmu" %in% names(bfm$pforms))
})

test_that("an equation the transform is undefined for keeps the offset", {
  # neclin is unbounded below, so log and logit of its mean are both NaN. This
  # is a blocker rather than a caveat, and the equation keeps the additive
  # offset permanently.
  d <- ogl_fixture()
  f <- bnf(y ~ crf(x, model = "neclin") + ogl(g))
  bdat <- model.frame(f, data = d, run_par_checks = TRUE)
  bfm <- wrangle_model_formula("neclin", f, bdat, validate_family("Beta"))
  expect_match(deparse1(bfm$formula[[3]]), "ogl + ", fixed = TRUE)
  expect_false("bnecmu" %in% names(bfm$pforms))
})

test_that("the transformed model is the current model at zero deviation", {
  # The claim that top, bot, nec and beta keep their meanings rests on this:
  # the deviation is zero-centred and m * exp(0) is m. Evaluated on the two
  # generated expressions rather than argued.
  d <- ogl_fixture()
  f <- bnf(y ~ crf(x, model = "nec3param") + ogl(g))
  bdat <- model.frame(f, data = d, run_par_checks = TRUE)
  tr <- wrangle_model_formula("nec3param", f, bdat, validate_family("Beta"))
  ad <- wrangle_model_formula("nec3param", f, bdat, validate_family("gaussian"))
  env <- list(top = 0.9, beta = -1.2, nec = 5, ogl = 0, x = c(1, 5, 20, 100))
  env$bnecmu <- eval(tr$pforms$bnecmu[[3]], env)
  expect_equal(eval(tr$formula[[3]], env), env$bnecmu)
  expect_equal(eval(ad$formula[[3]], env), env$bnecmu)
})
