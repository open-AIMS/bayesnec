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


# ---- #319, symbols resolve where the user wrote the formula -----------------

# Every test below is written inside a function on purpose. At the top level of
# a test file testthat evaluates in an environment whose parent chain reaches
# the global environment, which is the one place the defect did not show: a
# top-level test passed before the fix and would not have caught it.

test_that("a model set held in a variable resolves from inside a function", {
  fit_set <- function() {
    eqs <- c("nec3param", "ecxll3")
    get_model_from_formula(bnf(y ~ crf(x, eqs)))
  }
  expect_identical(fit_set(), c("nec3param", "ecxll3"))
})

test_that("a model set in a variable resolves for a character formula", {
  fit_set <- function() {
    eqs <- c("nec3param", "ecxll3")
    get_model_from_formula(bnf("y ~ crf(x, eqs)"))
  }
  expect_identical(fit_set(), c("nec3param", "ecxll3"))
})

test_that("a model set resolves in an environment off the global chain", {
  # baseenv() as the parent puts the global environment nowhere on the lookup
  # chain, so this fails whenever the variable is found by falling through to
  # the global environment rather than by the formula's own environment.
  env <- new.env(parent = baseenv())
  assign("eqs", c("nec3param", "ecxll3"), envir = env)
  assign("bnf", bnf, envir = env)
  assign("get_model_from_formula", get_model_from_formula, envir = env)
  expect_identical(evalq(get_model_from_formula(bnf(y ~ crf(x, eqs))), env),
                   c("nec3param", "ecxll3"))
})

test_that("a variable model set is accepted named as well as positional", {
  fit_set <- function() {
    eqs <- "nec4param"
    get_model_from_formula(bnf(y ~ crf(x, model = eqs)))
  }
  expect_identical(fit_set(), "nec4param")
})

test_that("a literal model set is unchanged", {
  expect_identical(get_model_from_formula(bnf(y ~ crf(x, "nec3param"))),
                   "nec3param")
  expect_identical(
    get_model_from_formula(bnf(y ~ crf(x, c("nec3param", "ecxll3")))),
    c("nec3param", "ecxll3")
  )
  # A model group name still expands to its members.
  expect_true(all(c("nec3param", "nec4param") %in%
                    get_model_from_formula(bnf(y ~ crf(x, "nec")))))
})

test_that("a model set returned by models() is accepted", {
  expected <- names(models("decline", max_pars = 3))
  expect_identical(
    get_model_from_formula(bnf(y ~ crf(x, models("decline", max_pars = 3)))),
    expected
  )
})

test_that("a model set variable that does not exist is still an error", {
  fit_set <- function() {
    get_model_from_formula(bnf(y ~ crf(x, no_such_object)))
  }
  expect_error(fit_set(), "no_such_object")
})

test_that("a locally defined predictor transformation resolves on the R side", {
  # The same defect on the other half of the crf() term: the reduced formula
  # model.frame() is given lost the user's environment, so a function defined
  # in the caller was found only at the console.
  build <- function() {
    squared <- function(z) z^2
    model.frame(bnf(y ~ crf(squared(x), "nec3param")), data = nec_data)
  }
  bdat <- build()
  expect_equal(bdat[["squared(x)"]], nec_data$x^2)
})

test_that("get_priors resolves a variable model set from inside a function", {
  build <- function() {
    eqs <- "nec3param"
    get_priors(bnf(y ~ crf(x, eqs)), data = nec_data, family = gaussian())
  }
  expect_s3_class(build(), "brmsprior")
})

test_that("the brms formula is built from a locally defined transformation", {
  # single_model_formula() and wrangle_model_formula() run once per model on
  # the fitting path, after the model frame is built, so the environment has to
  # survive both. Asserted here rather than by fitting, which would add a Stan
  # compilation to the suite for a defect that is fixed before brm() is
  # reached.
  build <- function() {
    # square() is defined here for R's model frame and is also a Stan built-in.
    # An arbitrary R helper would still need a Stan definition at fit time.
    square <- function(z) z^2
    eqs <- "nec3param"
    f <- bnf(y ~ crf(square(x), eqs))
    single_form <- single_model_formula(f, get_model_from_formula(f))
    make_brmsformula(single_form, nec_data)
  }
  expect_s3_class(build()$nec3param, "brmsformula")
})

test_that("the hurdle component formulas keep the user's environment", {
  # bnec_hurdle() splits one formula into two by rebuilding the right-hand side
  # from its deparsed text, which drops the environment unless it is carried
  # over.
  build <- function() {
    eqs <- c("nec3param", "ecxll3")
    get_model_from_formula(swap_response(bnf(y ~ crf(x, eqs)), "y_surv"))
  }
  expect_identical(build(), c("nec3param", "ecxll3"))
})

test_that("the fitting entry points resolve a character formula", {
  # bnec(), bnec_group(), bnec_hurdle(), bnec_joint(), get_priors.formula() and
  # get_priors.character() each pass their own caller's frame down, and that is
  # the only thing those six lines change. get_priors() is the one of them that
  # reaches a result without sampling, so it is the one asserted here.
  build <- function() {
    eqs <- "nec3param"
    get_priors("y ~ crf(x, eqs)", data = nec_data, family = gaussian())
  }
  expect_s3_class(build(), "brmsprior")
})

test_that("env must be an environment", {
  # environment(formula) <- NULL is legal, so an unchecked NULL would pass and
  # silently restore the global-environment-only behaviour.
  expect_error(bnf("y ~ crf(x, \"nec3param\")", env = NULL),
               "must be an environment")
  expect_error(bayesnecformula("y ~ crf(x, \"nec3param\")", env = "globalenv"),
               "must be an environment")
})

test_that("env is ignored when a formula already carries an environment", {
  f <- y ~ crf(x, "nec3param")
  out <- bnf(f, env = NULL)
  expect_s3_class(out, "bayesnecformula")
  expect_identical(environment(out), environment(f))
})

test_that("a model argument that is not character is named as the cause", {
  # The symbol now resolves in more environments than it did, so a name bound
  # to something else in the caller reaches expand_model_set(), which reported
  # "'match' requires vector arguments".
  build <- function() {
    get_model_from_formula(bnf(y ~ crf(x, models)))
  }
  expect_error(build(), "must be a character vector")
})

test_that("the model frame resolves trials() off the global chain", {
  # The reduced formula keeps trials() as a call and bayesnec does not export
  # it, so taking the user's environment alone broke every binomial fit under
  # an installed package. baseenv() as the parent puts both the global
  # environment and the package namespace off the lookup chain, so this holds
  # whether or not pkgload::load_all() has exported the internals -- which is
  # what hid the defect from every local run.
  d <- nec_data
  d$tr <- 100L
  d$count <- as.integer(round(d$y * 20))
  env <- new.env(parent = baseenv())
  assign("d", d, envir = env)
  assign("bnf", bnf, envir = env)
  assign("model.frame", model.frame, envir = env)
  bdat <- evalq(
    model.frame(bnf(count | trials(tr) ~ crf(x, "nec3param")), data = d), env
  )
  expect_identical(bdat[["trials(tr)"]], d$tr)
})

test_that("a formula whose environment was stripped still builds", {
  # NULL and emptyenv() are the two ways an environment is stripped, and
  # emptyenv() is the worse to pass on: nothing resolves through it, base
  # included.
  f_null <- y ~ crf(x, "nec3param")
  environment(f_null) <- NULL
  expect_s3_class(model.frame(bnf(f_null), data = nec_data), "data.frame")
  f_empty <- y ~ crf(x, "nec3param")
  environment(f_empty) <- emptyenv()
  expect_s3_class(model.frame(bnf(f_empty), data = nec_data), "data.frame")
})

test_that("a knitted chunk resolves a variable model set", {
  # The reported failure: vignettes/precompile.R calls knitr::knit() from
  # inside knit_one(), so a variable assigned in a chunk lands in that frame
  # and not in the global environment. Reproduced here by knitting from inside
  # a function for the same reason.
  skip_if_not_installed("knitr")
  # echo = FALSE so that the assertion is made on what the chunk returned and
  # not on the echoed source, which names the equations itself. knitr renders a
  # failed chunk as text and carries on, so the absence of an error is asserted
  # as well as the presence of the answer.
  txt <- c("```{r echo = FALSE}", "eqs <- c(\"nec3param\", \"ecxll3\")",
           "bayesnec:::get_model_from_formula(bayesnec::bnf(y ~ crf(x, eqs)))",
           "```")
  knit_one <- function(text) {
    knitr::knit(text = text, quiet = TRUE)
  }
  out <- knit_one(txt)
  expect_match(out, "ecxll3")
  expect_false(grepl("Error", out, fixed = TRUE))
})
