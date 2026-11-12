test_that("a joint refit conditions left censoring on known survival", {
  d <- data.frame(
    x = c(5, 5, 4, 3, 2, 1),
    y = c(0, 0.005, 0.2, 0.4, 0.7, 0.9),
    censor = c("none", "left", "interval", "none", "right", "none"),
    upper = c(0, 0.005, 0.3, 0.4, 0.7, 0.9)
  )
  f <- bnf(y | cens(censor, upper) ~ crf(x, "nec3param"))
  fam <- "hurdle_gamma"
  marked <- bayesnec:::mark_joint_survivor_cens(f, d, fam)
  single <- bayesnec:::single_model_formula(marked, "nec3param")
  expect_identical(attr(single, "bayesnec_survivor_cens"),
                   attr(marked, "bayesnec_survivor_cens"))
  frame <- model.frame(single, d)
  bf <- bayesnec:::wrangle_model_formula(
    "nec3param", single, frame, bayesnec:::validate_family(fam),
    model_survival = "nec3param"
  )
  prepared <- bayesnec:::prepare_joint_survivor_cens(single, d, bf)
  standata <- brms::make_standata(
    prepared$formula, prepared$data,
    family = bayesnec:::validate_family(fam)
  )
  expect_equal(prepared$data$y, d$y)
  expect_equal(as.vector(standata$Y), c(0, 0, 0.2, 0.4, 0.7, 0.9))
  expect_equal(as.vector(standata$cens), c(0L, 2L, 2L, 0L, 1L, 0L))
  expect_equal(as.vector(standata$rcens[c(2, 3)]), c(0.005, 0.3))
  expect_true(all(is.finite(prepared$data[[
    attr(single, "bayesnec_survivor_cens")$upper
  ]])))
  factor_data <- prepared$data
  left_name <- attr(single, "bayesnec_survivor_cens")$left
  factor_data[[left_name]] <- factor(factor_data[[left_name]])
  factor_standata <- brms::make_standata(
    prepared$formula, factor_data,
    family = bayesnec:::validate_family(fam)
  )
  expect_equal(as.vector(factor_standata$Y), as.vector(standata$Y))
})

test_that("a known Beta survivor is refused before a joint refit", {
  d <- data.frame(x = 1:4, y = c(0, 0.005, 0.3, 0.6),
                  censor = c("none", "left", "none", "none"))
  f <- bnf(y | cens(censor) ~ crf(x, "nec3param"))
  expect_error(
    bayesnec:::mark_joint_survivor_cens(f, d, "zero_inflated_beta"),
    "beta_lccdf\\(0\\).*Keep the factorised bnec_hurdle\\(\\) fit"
  )
  d$censor[2] <- "right"
  expect_null(attr(bayesnec:::mark_joint_survivor_cens(
    f, d, "zero_inflated_beta"
  ), "bayesnec_survivor_cens"))
})

test_that("direct left censoring retains the mixture likelihood", {
  d <- data.frame(x = 1:4, y = c(0, 0.005, 0.3, 0.6),
                  censor = c("none", "left", "none", "none"))
  f <- bnf(y | cens(censor) ~ crf(x, "nec3param"))
  expect_null(attr(f, "bayesnec_survivor_cens"))
  for (fam in c("hurdle_gamma", "zero_inflated_beta")) {
    bf <- bayesnec:::wrangle_model_formula(
      "nec3param", f, model.frame(f, d),
      bayesnec:::validate_family(fam), model_survival = "nec3param"
    )
    prepared <- bayesnec:::prepare_joint_survivor_cens(f, d, bf)
    standata <- brms::make_standata(
      prepared$formula, prepared$data,
      family = bayesnec:::validate_family(fam)
    )
    expect_equal(standata$Y[2], 0.005)
    expect_equal(standata$cens[2], -1L)
  }
})

test_that("joint censoring columns are rebuilt for new data", {
  d <- data.frame(x = 1:4, y = c(0, 0.005, 0.3, 0.6),
                  censor = c("none", "left", "none", "none"))
  f <- bayesnec:::mark_joint_survivor_cens(
    bnf(y | cens(censor) ~ crf(x, "nec3param")), d, "hurdle_gamma"
  )
  spec <- attr(f, "bayesnec_survivor_cens")
  old <- bayesnec:::joint_survivor_cens_data(f, d, spec)
  newer <- old
  newer$censor <- c("none", "none", "left", "none")
  newer$y[3] <- 0.03
  updated <- bayesnec:::joint_survivor_cens_data(f, newer, spec)
  expect_equal(updated[[spec$left]], c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(updated[[spec$code]], c(0L, 0L, 2L, 0L))
  expect_equal(updated[[spec$upper]], updated$y)
  newer$y[3] <- 0
  expect_error(
    bayesnec:::joint_survivor_cens_data(f, newer, spec),
    "needs a positive response bound"
  )
})

test_that("a fitted object retains the original censoring columns", {
  d <- data.frame(x = 1:4, y = c(0, 0.005, 0.3, 0.6),
                  censor = c("none", "left", "interval", "none"),
                  upper = c(0, 0.005, 0.4, 0.6))
  f <- bayesnec:::mark_joint_survivor_cens(
    bnf(y | cens(censor, upper) ~ crf(x, "nec3param")),
    d, "hurdle_gamma"
  )
  stored <- list(data = d[, c("x", "y"), drop = FALSE])
  restored <- bayesnec:::restore_joint_survivor_data(stored, d, f)
  expect_identical(restored$data$censor, d$censor)
  expect_identical(restored$data$upper, d$upper)
  expect_equal(bayesnec:::retrieve_cens(model.frame(f, restored$data)),
               c(0L, -1L, 2L, 0L))
})

test_that("bnec() hands brm() the interval for a marked survivor", {
  d <- data.frame(
    x = rep(c(0, 1, 2, 3, 4), each = 4),
    y = c(0.9, 1.1, 1.0, 0.95, 0.8, 0.9, 0.85, 1.0, 0.6, 0.7, 0.65, 0.5,
          0.3, 0, 0.35, 0.2, 0, 0, 0.05, 0),
    censor = "none"
  )
  survivor <- 19
  d$censor[survivor] <- "left"
  f <- bayesnec:::mark_joint_survivor_cens(
    bnf(y | cens(censor) ~ crf(x, "nec3param")), d, "hurdle_gamma"
  )
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    brm = function(formula, data, ...) {
      seen$formula <- formula
      seen$data <- data
      structure(list(), class = "brmsfit")
    },
    are_chains_correct = function(...) TRUE,
    make_good_inits = function(..., chains) {
      replicate(chains, list(b_top = as.array(1)), simplify = FALSE)
    },
    make_good_hurdle_inits = function(..., chains) {
      replicate(chains, list(b_top = as.array(1)), simplify = FALSE)
    },
    .package = "bayesnec"
  )
  try(suppressWarnings(suppressMessages(
    bnec(f, data = d, family = "hurdle_gamma", model_survival = "nec3param")
  )), silent = TRUE)
  expect_false(is.null(seen$formula))
  standata <- brms::make_standata(
    seen$formula, seen$data,
    family = bayesnec:::validate_family("hurdle_gamma")
  )
  expect_equal(standata$Y[survivor], 0)
  expect_equal(standata$cens[survivor], 2L)
  expect_equal(standata$rcens[survivor], 0.05)
  expect_equal(seen$data$y, d$y)
})
