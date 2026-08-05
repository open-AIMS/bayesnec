data <- data.frame(pred = seq(0.5, 5, length.out = 10),
                   resp = seq(0.9, 0.1, length.out = 10),
                   tr = 10, wgt = 15,
                   cen = rep(c("none", "left"), 5), ub = 1,
                   g_1 = "a", g_2 = "b")

test_that("cens() is carried into model.frame output and bnec_pop", {
  f <- bnf(resp | cens(cen) ~ crf(pred, "nec4param"))
  mf <- model.frame(f, data)
  expect_identical(names(mf), c("resp", "pred", "cen"))
  expect_identical(names(attr(mf, "bnec_pop")),
                   c("y_var", "x_var", "cens_var"))
  expect_equal(attr(mf, "bnec_pop"), c("resp", "pred", "cen"),
               ignore_attr = TRUE)
  # bnec_pop positions must index the model frame, because retrieve_var()
  # looks a variable up by position rather than by name
  expect_equal(retrieve_var(mf, "y_var"), data$resp)
  expect_equal(retrieve_var(mf, "x_var"), data$pred)
})

test_that("cens() coexists with trials, group-level terms and transformations", {
  mf <- model.frame(bnf(resp | trials(tr) + cens(cen) ~ crf(pred, "nec4param")),
                    data)
  expect_identical(names(attr(mf, "bnec_pop")),
                   c("y_var", "x_var", "trials_var", "cens_var"))
  expect_equal(retrieve_var(mf, "trials_var"), data$tr)
  expect_equal(bayesnec:::retrieve_cens(mf), c(0L, -1L, 0L, -1L, 0L, -1L, 0L,
                                               -1L, 0L, -1L))
  # the order the aterms are written in must not matter, because bnec_pop
  # positions are fixed by simplify_formula rather than by the formula
  mf_rev <- model.frame(
    bnf(resp | cens(cen) + trials(tr) ~ crf(pred, "nec4param")), data
  )
  expect_identical(names(mf_rev), names(mf))
  expect_identical(attr(mf_rev, "bnec_pop"), attr(mf, "bnec_pop"))
  mf_g <- model.frame(
    bnf(resp | cens(cen) ~ crf(pred, "nec4param") + ogl(g_2) + pgl(g_1)), data
  )
  expect_identical(names(mf_g), c("resp", "pred", "cen", "g_1", "g_2"))
  expect_identical(names(attr(mf_g, "bnec_pop")),
                   c("y_var", "x_var", "cens_var"))
  expect_equal(attr(mf_g, "bnec_group"), c("g_1", "g_2"))
  mf_t <- model.frame(bnf(resp | cens(cen) ~ crf(sqrt(pred), "nec4param")),
                      data)
  expect_identical(names(mf_t), c("resp", "sqrt(pred)", "cen"))
  expect_equal(retrieve_var(mf_t, "x_var"), sqrt(data$pred))
})

test_that("the interval-censoring form carries both variables", {
  mf <- model.frame(bnf(resp | cens(cen, ub) ~ crf(pred, "nec4param")), data)
  expect_identical(names(mf), c("resp", "pred", "cen", "ub"))
  expect_identical(names(attr(mf, "bnec_pop")),
                   c("y_var", "x_var", "cens_var", "cens_y2_var"))
  expect_equal(retrieve_var(mf, "cens_y2_var"), data$ub)
})

test_that("the brmsformula keeps the cens term", {
  bf <- make_brmsformula(resp | cens(cen) ~ crf(pred, "nec4param"), data)
  expect_true(grepl("cens(cen)", deparse1(bf$nec4param$formula), fixed = TRUE))
})

test_that("cens() no longer trips the unvalidated-aterm message", {
  expect_no_message(check_formula(bnf(resp | cens(cen) ~
                                        crf(pred, "nec4param")), data))
  expect_message(
    check_formula(bnf(resp | se(wgt) ~ crf(pred, "nec4param")), data),
    "aterms other than trials, weights and cens"
  )
})

test_that("a cens() term with no variable is flagged", {
  expect_warning(
    check_formula(bnf(resp | cens("left") ~ crf(pred, "nec4param")), data),
    "contains no variable"
  )
})

test_that("normalise_cens follows the brms encodings", {
  expect_equal(bayesnec:::normalise_cens(c("none", "left", "right",
                                           "interval")),
               c(0L, -1L, 1L, 2L))
  # brms matches these by prefix
  expect_equal(bayesnec:::normalise_cens(c("n", "l", "r", "i")),
               c(0L, -1L, 1L, 2L))
  expect_equal(bayesnec:::normalise_cens(factor(c("left", "none"))),
               c(-1L, 0L))
  expect_equal(bayesnec:::normalise_cens(c(TRUE, FALSE)), c(1L, 0L))
  expect_equal(bayesnec:::normalise_cens(c(-1, 0, 1, 2)), c(-1L, 0L, 1L, 2L))
  # anything unrecognised is left for brms to reject
  expect_equal(bayesnec:::normalise_cens(c("bogus", NA)), c(NA_integer_,
                                                            NA_integer_))
  expect_equal(bayesnec:::normalise_cens(7), NA_integer_)
})

test_that("check_data exempts censored rows from the boundary shifts", {
  build <- function(y, cen = NULL) {
    d <- data.frame(pred = seq(0.5, by = 0.5, length.out = length(y)), resp = y)
    if (is.null(cen)) {
      return(model.frame(bnf(resp ~ crf(pred, "nec4param")), d))
    }
    d$cen <- cen
    model.frame(bnf(resp | cens(cen) ~ crf(pred, "nec4param")), d)
  }
  y <- c(0.9, 0.5, 0.2, 0)
  # unchanged when no censoring is declared
  expect_message(
    out <- bayesnec:::check_data(build(y), Gamma(), "nec4param"),
    "response contains zeros"
  )
  expect_equal(out$mod_dat$y, c(0.9, 0.5, 0.2, 0.02))
  # a zero that is not censored is still shifted, one that is censored is not
  cen <- c("none", "none", "left", "none")
  out2 <- bayesnec:::check_data(build(c(0.9, 0.5, 0.005, 0), cen), Gamma(),
                               "nec4param") |>
    suppressMessages()
  expect_equal(out2$mod_dat$y, c(0.9, 0.5, 0.005, 0.0005))
})

test_that("check_data rejects a censored value on an excluded boundary", {
  d <- data.frame(pred = c(0.5, 1, 1.5, 2), resp = c(0.9, 0.5, 0.2, 0),
                  cen = c("none", "none", "none", "left"))
  mf <- model.frame(bnf(resp | cens(cen) ~ crf(pred, "nec4param")), d)
  expect_error(bayesnec:::check_data(mf, Gamma(), "nec4param"),
               "left-censored at 0")
  expect_error(bayesnec:::check_data(mf, Beta(), "nec4param"),
               "left-censored at 0")
  d2 <- d
  d2$resp <- c(1, 0.5, 0.2, 0.1)
  d2$cen <- c("right", "none", "none", "none")
  mf2 <- model.frame(bnf(resp | cens(cen) ~ crf(pred, "nec4param")), d2)
  expect_error(bayesnec:::check_data(mf2, Beta(), "nec4param"),
               "right-censored at 1")
  # gaussian has support at both, so neither is a problem
  expect_silent(bayesnec:::check_data(mf, gaussian(), "nec4param"))
})
