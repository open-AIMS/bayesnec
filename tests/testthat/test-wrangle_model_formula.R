data <- data.frame(pred = 1:10, resp = 1:10)

test_that("correct classes", {
  all_models <- models("all")
  for (i in seq_along(all_models)) {
    model <- names(all_models)[i]
    form <- bnf(paste0("resp ~ crf(sqrt(pred), \"", model, "\")"))
    bdat <- model.frame(form, data)
    expect_s3_class(wrangle_model_formula(model, bnf(form), bdat),
                    "brmsformula")
  }
})


# ---- #294, a group-level deviation on a single parameter ---------------------

set.seed(294)
gdat <- data.frame(
  resp = runif(60, 0.05, 0.9),
  pred = rep(log(c(0.1, 1, 10, 100, 1000, 1e4)), 10),
  grp = factor(rep(1:5, each = 12))
)

build <- function(term, model = "nec4param", family = validate_family("Beta")) {
  form <- bnf(paste0("resp ~ crf(pred, \"", model, "\") + ", term))
  bdat <- suppressMessages(model.frame(form, gdat))
  suppressMessages(wrangle_model_formula(model, form, bdat, family = family))
}
sub_rhs <- function(bform, par) {
  deparse1(formula.tools::rhs(bform[[2]][[par]]))
}

test_that("a term on bot becomes a multiplicative deviation", {
  bform <- build("(bot | grp)")
  # bot keeps its name and stays population-level, so b_bot_Intercept and the
  # prior define_prior() builds for it are untouched.
  expect_equal(sub_rhs(bform, "bot"), "1")
  # The grouping is declared on the deviation, and the curve reads the
  # intermediate.
  expect_equal(sub_rhs(bform, "botgl"), "1 + (1 | grp)")
  expect_equal(sub_rhs(bform, "bnecbot"),
               "bot * exp(botgl)/(1 - bot + bot * exp(botgl))")
  expect_true(grepl("bnecbot", deparse1(bform[[1]][[3]]), fixed = TRUE))
  expect_false(grepl("\\bbot\\b", deparse1(bform[[1]][[3]])))
  # The intermediate is a non-linear sub-formula, not a linear one.
  expect_true(attr(bform[[2]][["bnecbot"]], "nl"))
})

test_that("a term on an unbounded parameter keeps the additive form", {
  # nec is on the predictor scale, is routinely negative on a log predictor,
  # and is not bounded by the likelihood, so there is nothing to protect.
  bform <- build("(nec | grp)")
  expect_equal(sub_rhs(bform, "nec"), "1 + (1 | grp)")
  expect_null(bform[[2]][["bnecnec"]])
  expect_equal(deparse1(bform[[1]][[3]]),
               "bot + (top - bot) * exp(-exp(beta) * (pred - nec) * step(pred - nec))")
})

test_that("gaussian keeps the additive form on every parameter", {
  # The isolating arm. Same data, same term, same settings; the only change is
  # that the mean is no longer constrained.
  bform <- build("(bot | grp)", family = validate_family("gaussian"))
  expect_equal(sub_rhs(bform, "bot"), "1 + (1 | grp)")
  expect_null(bform[[2]][["botgl"]])
  expect_null(bform[[2]][["bnecbot"]])
})

test_that("pgl expands to what the equivalent explicit terms give", {
  # pgl() is documented as a term on every parameter at once, so it must expand
  # to exactly what writing those terms out by hand gives -- top and bot
  # transformed, the rest additive.
  bform <- build("pgl(grp)")
  expect_equal(sub_rhs(bform, "botgl"), "1 + (1 | grp)")
  expect_equal(sub_rhs(bform, "topgl"), "1 + (1 | grp)")
  expect_equal(sub_rhs(bform, "bot"), "1")
  expect_equal(sub_rhs(bform, "top"), "1")
  expect_equal(sub_rhs(bform, "nec"), "1 + (1 | grp)")
  expect_equal(sub_rhs(bform, "beta"), "1 + (1 | grp)")
  expect_equal(deparse1(bform[[1]][[3]]),
               paste("bnecbot + (bnectop - bnecbot) * exp(-exp(beta) *",
                     "(pred - nec) * step(pred - nec))"))
  explicit <- build("(bot | grp) + (top | grp) + (nec | grp) + (beta | grp)")
  expect_equal(deparse1(bform[[1]][[3]]), deparse1(explicit[[1]][[3]]))
  for (p in c("bot", "top", "botgl", "topgl", "nec", "beta")) {
    expect_equal(sub_rhs(bform, p), sub_rhs(explicit, p))
  }
})

test_that("the mean transform and the parameter transform compose", {
  # ogl() makes the curve an intermediate named bnecmu, and that curve is the
  # one the parameter transform has already been applied to.
  bform <- build("ogl(grp) + (bot | grp)")
  expect_equal(deparse1(bform[[1]][[3]]),
               "bnecmu * exp(ogl)/(1 - bnecmu + bnecmu * exp(ogl))")
  expect_true(grepl("bnecbot", deparse1(formula.tools::rhs(bform[[2]][["bnecmu"]])),
                    fixed = TRUE))
  expect_equal(sub_rhs(bform, "ogl"), "1 + (1 | grp)")
  expect_equal(sub_rhs(bform, "botgl"), "1 + (1 | grp)")
})

test_that("two groupings on one transformed parameter both reach the deviation", {
  gdat2 <- transform(gdat, grp2 = factor(rep(1:2, 30)))
  form <- bnf("resp ~ crf(pred, \"nec4param\") + (bot | grp) + (bot | grp2)")
  bdat <- suppressMessages(model.frame(form, gdat2))
  bform <- suppressMessages(
    wrangle_model_formula("nec4param", form, bdat,
                          family = validate_family("Beta"))
  )
  expect_equal(sub_rhs(bform, "botgl"), "1 + (1 | grp) + (1 | grp2)")
  expect_equal(sub_rhs(bform, "bot"), "1")
  # The intermediate is built once, not once per grouping.
  expect_equal(sub_rhs(bform, "bnecbot"),
               "bot * exp(botgl)/(1 - bot + bot * exp(botgl))")
})

test_that("every equation and family builds Stan code with a group-level term", {
  # The transform is applied by string substitution into 23 different curve
  # expressions, so the check that matters is that brms still accepts all of
  # them. Gamma exercises the log branch, gaussian the untransformed one.
  skip_on_cran()
  for (fam_name in c("Beta", "Gamma", "gaussian")) {
    family <- validate_family(fam_name)
    dd <- gdat
    if (fam_name == "Gamma") dd$resp <- gdat$resp * 10
    for (model in names(models("all"))) {
      for (term in c("pgl(grp)", "(bot | grp)", "(top | grp)")) {
        form <- bnf(paste0("resp ~ crf(pred, \"", model, "\") + ", term))
        bdat <- suppressMessages(model.frame(form, dd))
        bform <- suppressMessages(
          wrangle_model_formula(model, form, bdat, family = family)
        )
        gspec <- parse_group_terms(form, model)
        priors <- define_prior(model, family, bdat$pred, bdat$resp,
                               group_spec = gspec)
        expect_s3_class(
          suppressMessages(brms::make_stancode(bform, data = bdat,
                                               family = family,
                                               prior = priors)),
          "character"
        )
      }
    }
  }
})
