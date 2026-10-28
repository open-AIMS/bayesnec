b_top <- 100
b_beta <- -0.2
b_nec <- 4
fct <- function(...)as.integer(round(bayesnec:::pred_nec3param(...)))
# group7 occurs in x9's last bar term and must exist to exercise parsing.
dat <- data.frame(x = seq(1, 20, length.out = 10), tr = 100, wght = c(1, 2),
                  group1 = "a", group2 = "b", group3 = "b", group4 = "b",
                  group5 = "b", group6 = "c", group7 = "d", z = 19, k = 20,
                  sei = 30)
dat$y <- fct(b_beta, b_nec, b_top, dat$x)

# Use validated trials, weights and cens terms to exercise chained aterms.
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
  # Reject the whole chain when it contains an unrecognised term.
  expect_error(
    make_brmsformula(y | trials(tr) + se(sei) ~ crf(x, "nec3param"), dat),
    "aterms bayesnec does not support"
  )
})


# Resolve symbols where the formula was written.

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
