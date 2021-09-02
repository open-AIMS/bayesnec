library(bayesnec)

b_top <- 100
b_beta <- -0.2
b_nec <- 4
fct <- function(...)as.integer(round(bayesnec:::pred_nec3param(...)))
dat <- data.frame(x = seq(1, 20, length.out = 10), tr = 100, wght = c(1, 2),
                  group1 = "a", group2 = "b", group3 = "b", group4 = "b", 
                  group5 = "b", group6 = "c", z = 19, k = 20, sei = 30) %>%
  dplyr::mutate(y = fct(b_beta, b_nec, b_top, x))

x0 <- y | trials(tr) + weights(wght) + se(sei) +
  cens(k) ~ crf(sqrt(x), "nec") +
  ogl(group1) + pgl(group2) + (nec | group1)
x1 <- log(y) | trials(tr) + weights(wght) + se(sei) +
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
