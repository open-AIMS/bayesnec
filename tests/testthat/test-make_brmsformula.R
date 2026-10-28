b_top <- 100
b_beta <- -0.2
b_nec <- 4
fct <- function(...)as.integer(round(bayesnec:::pred_nec3param(...)))
# group7 is in x9's last bar term. It was absent until #466, and the test
# passed only because check_formula() looked at the first bar term alone.
dat <- data.frame(x = seq(1, 20, length.out = 10), tr = 100, wght = c(1, 2),
                  group1 = "a", group2 = "b", group3 = "b", group4 = "b",
                  group5 = "b", group6 = "c", group7 = "d", z = 19, k = 20,
                  sei = 30)
dat$y <- fct(b_beta, b_nec, b_top, dat$x)

# se() was dropped from this chain under #136: unvalidated aterms are now an
# error, and it was incidental here -- what these tests exercise is parsing a
# chain of several aterms, which trials + weights + cens still does.
x0 <- y | trials(tr) + weights(wght) +
  cens(k) ~ crf(sqrt(x), "nec") +
  ogl(group1) + pgl(group2) + (nec | group1)
x1 <- log(y) | trials(tr) + weights(wght) +
  cens(k) ~ crf(sqrt(x), "nec3param") + 
  ogl(group2) + pgl(group3)
x2 <- log(y) ~ crf(sqrt(x), "nec3param")
x3 <- log(y) ~ crf(x, "nec3param")
x4 <- log(y) | weights(wght) ~ crf(x, "nec3param")
x5 <- y | weights(wght) ~ crf(x, "nec3param")
x6 <- 'y | weights(wght) ~ crf(x, "ecx")'
x7 <- y | trials(tr) ~ crf(sqrt(x), "nec4param") + ogl(group1)
x8 <- y | trials(tr) ~ crf(sqrt(x), "nec4param") + ogl(group1) + pgl(group2)
x9 <- y | trials(tr) ~ crf(sqrt(x), "nec4param") + ogl(group1) + ogl(group2) + pgl(group3) + pgl(group4) + (nec | group6) + (nada + top | group5) + (top | group7)

test_that("correct classes", {
  expect_type(suppressMessages(make_brmsformula(x0, dat)), "list")
  expect_s3_class(suppressMessages(make_brmsformula(x0, dat)[[1]]),
                  "brmsformula")
  expect_type(suppressMessages(make_brmsformula(x1, dat)), "list")
  expect_type(suppressMessages(make_brmsformula(x2, dat)), "list")
  expect_type(suppressMessages(make_brmsformula(x3, dat)), "list")
  expect_type(suppressMessages(make_brmsformula(x4, dat)), "list")
  expect_type(suppressMessages(make_brmsformula(x5, dat)), "list")
  expect_type(suppressMessages(make_brmsformula(x6, dat)), "list")
  expect_type(suppressMessages(make_brmsformula(x7, dat)), "list")
  expect_type(suppressMessages(make_brmsformula(x8, dat)), "list")
  expect_type(suppressMessages(make_brmsformula(x9, dat)), "list")
})

test_that("an unvalidated aterm in a chain is an error", {
  # #136. The chain must be rejected as a whole rather than having the
  # unrecognised term quietly ignored, which is what the old message amounted
  # to in practice.
  expect_error(
    make_brmsformula(y | trials(tr) + se(sei) ~ crf(x, "nec3param"), dat),
    "aterms bayesnec does not support"
  )
})


# ---- #319, symbols resolve where the formula was written ---------------------

test_that("make_brmsformula resolves symbols in its caller's frame", {
  # Written inside a function on purpose: at the top level of a test file the
  # lookup falls through to the global environment, which is where the defect
  # did not show.
  build_set <- function() {
    eqs <- "nec3param"
    names(make_brmsformula("y ~ crf(x, eqs)", nec_data))
  }
  expect_identical(build_set(), "nec3param")
  build_x <- function() {
    # This checks R-side construction only. The separate Stan test below uses
    # a function name that both languages define.
    squared <- function(z) z^2
    names(make_brmsformula("y ~ crf(squared(x), \"nec3param\")", nec_data))
  }
  expect_identical(build_x(), "nec3param")
})

test_that("a local R transformation that Stan defines reaches valid code", {
  skip_if_not_installed("rstan")
  build_code <- function() {
    # Stan provides square(); this local definition supplies the R side used
    # while bayesnec builds the model frame. Other custom functions require a
    # corresponding Stan definition through stanvars.
    square <- function(z) z^2
    bform <- make_brmsformula(
      "y ~ crf(square(x), \"nec3param\")", nec_data
    )[[1]]
    brms::make_stancode(
      bform, data = nec_data, family = stats::gaussian(link = "identity")
    )
  }
  code <- build_code()
  expect_match(code, "square(C_1[n])", fixed = TRUE)
  expect_true(rstan::stanc(model_code = code, allow_undefined = FALSE)$status)
})

test_that("a missing parameter named in two terms is left out of both (#470)", {
  # match() found the first of the two terms only, so the second reached the
  # formula build and failed in rhs() before anything was sampled. ecxll3 has
  # ec50 and no nec, so it keeps both ec50 terms and drops both nec terms.
  d <- transform(dat, group8 = "e")
  f <- y | trials(tr) ~ crf(x, "ecxll3") + (nec | group6) + (nec | group8) +
    (ec50 | group6) + (ec50 | group8)
  # The messages are collected rather than asserted with expect_message(),
  # which returns the condition and not the formula.
  msgs <- character(0)
  bf <- withCallingHandlers(make_brmsformula(f, d), message = function(m) {
    msgs <<- c(msgs, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  expect_true(any(grepl("\"nec\"; \"nec\" are not valid parameters in ecxll3",
                        msgs, fixed = TRUE)))
  pforms <- bf$ecxll3$pforms
  expect_null(pforms$nec)
  expect_identical(deparse1(pforms$ec50),
                   "ec50 ~ 1 + (1 | group6) + (1 | group8)")
})

# ---- #479, the predictor as written into the brms formula --------------------

crf_data <- data.frame(x = seq(1.5, 12, length.out = 12),
                       y = seq(9, 2, length.out = 12))

test_that("an I() predictor reaches brms without I() (#479)", {
  # Stan has no I(), so the expression inside it is written, in parentheses.
  # A name or a function call is written unchanged.
  txt <- bayesnec:::brms_x_text
  expect_identical(txt("x"), "x")
  expect_identical(txt("log(x)"), "log(x)")
  expect_identical(txt("log(x + 1)"), "log(x + 1)")
  expect_identical(txt("I(log(x) + 1)"), "(log(x) + 1)")
  expect_identical(txt("I(-x)"), "(-x)")
  expect_identical(txt("I(log(x))"), "log(x)")
  expect_identical(txt("I(I(x - 1))"), "(x - 1)")
  expect_identical(txt("x > 1"), "(x > 1)")
  rhs <- deparse1(bayesnec:::bf_nec4param[[1]][[3]])
  expect_identical(
    bayesnec:::substitute_x_in_formula("I(log(x) + 1)", rhs),
    paste0("bot + (top - bot) * exp(-exp(beta) * ((log(x) + 1) - nec) *",
           " step((log(x) + 1) - nec))")
  )
  # Parenthesised, a comparison is not regrouped with the curve's own terms:
  # bare, (x > 1 - nec) would read as x > (1 - nec).
  sub <- str2lang(bayesnec:::substitute_x_in_formula("x > 1", "(x - nec)"))
  expect_identical(sub, quote(((x > 1) - nec)))
  # The model frame and the estimators keep I().
  f <- bnf(y ~ crf(I(log(x) + 1), "nec4param"))
  expect_identical(names(model.frame(f, data = crf_data))[2], "I(log(x) + 1)")
  expect_equal(bayesnec:::sub_x_transformation(2, f), log(2) + 1,
               ignore_attr = TRUE)
  bf <- make_brmsformula(f, crf_data)$nec4param
  expect_identical(
    deparse1(bf$formula[[3]]),
    paste0("bot + (top - bot) * exp(-exp(beta) * ((log(x) + 1) - nec) *",
           " step((log(x) + 1) - nec))")
  )
  # The formula built for crf(x) and crf(log(x)) is as it was.
  expect_identical(
    deparse1(make_brmsformula(y ~ crf(log(x), "ecx4param"),
                              crf_data)$ecx4param$formula[[3]]),
    "top + (bot - top)/(1 + exp((ec50 - log(x)) * exp(beta)))"
  )
  expect_identical(
    deparse1(make_brmsformula(y ~ crf(x, "nec4param"),
                              crf_data)$nec4param$formula[[3]]),
    "bot + (top - bot) * exp(-exp(beta) * (x - nec) * step(x - nec))"
  )
})

test_that("an I() predictor gives Stan code that compiles (#479)", {
  skip_if_not_installed("rstan")
  stan_ok <- function(bform, family) {
    code <- brms::make_stancode(bform, data = crf_data, family = family)
    expect_no_match(code, "I(", fixed = TRUE)
    isTRUE(rstan::stanc(model_code = code, allow_undefined = FALSE)$status)
  }
  gauss <- stats::gaussian(link = "identity")
  for (p in c("I(log(x) + 1)", "I(-x)", "log(x)", "x")) {
    for (m in c("nec4param", "ecx4param")) {
      f <- stats::as.formula(paste0("y ~ crf(", p, ", \"", m, "\")"))
      expect_true(stan_ok(make_brmsformula(f, crf_data)[[m]], gauss),
                  label = paste(p, m))
    }
  }
  # The hurdle block and a disp() variance function write the predictor
  # through the same substitution.
  hurdle <- brms::hurdle_gamma(link = "identity", link_shape = "log",
                               link_hu = "identity")
  expect_true(stan_ok(
    make_brmsformula(y ~ crf(I(-x), "nec4param"), crf_data,
                     family = hurdle)$nec4param,
    hurdle
  ))
  gamma <- stats::Gamma(link = "identity")
  expect_true(stan_ok(
    make_brmsformula(y ~ crf(I(log(x) + 1), "ecx4param") + disp("power"),
                     crf_data, family = gamma)$ecx4param,
    gamma
  ))
})
