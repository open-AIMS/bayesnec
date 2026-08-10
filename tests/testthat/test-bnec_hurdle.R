test_that("hurdle_response_var accepts a plain response", {
  expect_equal(
    bayesnec:::hurdle_response_var(bnf(y ~ crf(x, "nec3param"))), "y"
  )
})

test_that("hurdle_response_var rejects a transformed response", {
  # A transformed response makes the zero-as-death convention ambiguous:
  # log(0) is not a death, it is -Inf.
  expect_error(
    bayesnec:::hurdle_response_var(bnf(log(y) ~ crf(x, "nec3param"))),
    "plain, untransformed response"
  )
  expect_error(
    bayesnec:::hurdle_response_var(bnf(sqrt(y) ~ crf(x, "nec3param"))),
    "plain, untransformed response"
  )
})

test_that("swap_response replaces the lhs and keeps the rhs", {
  out <- bayesnec:::swap_response(bnf(y ~ crf(x, "nec3param")), ".alive")
  expect_s3_class(out, "bayesnecformula")
  expect_equal(bayesnec:::hurdle_response_var(out), ".alive")
  expect_equal(bayesnec:::get_model_from_formula(out), "nec3param")
})

test_that("swap_response preserves group-level terms", {
  out <- bayesnec:::swap_response(
    bnf(y ~ crf(x, "nec3param") + pgl(tank)), ".alive"
  )
  expect_true(grepl("pgl(tank)", deparse1(out), fixed = TRUE))
})

test_that("swap_crf_model swaps a single model and a model group", {
  f <- bnf(y ~ crf(x, "nec3param"))
  expect_equal(
    bayesnec:::get_model_from_formula(bayesnec:::swap_crf_model(f, "ecxexp")),
    "ecxexp"
  )
  expect_setequal(
    bayesnec:::get_model_from_formula(bayesnec:::swap_crf_model(f, "nec")),
    models()$nec
  )
  expect_setequal(
    bayesnec:::get_model_from_formula(
      bayesnec:::swap_crf_model(f, c("nec3param", "ecxexp"))
    ),
    c("nec3param", "ecxexp")
  )
})

test_that("swap_crf_model preserves a transformed predictor", {
  f <- bnf(y ~ crf(log(x), "nec3param"))
  out <- bayesnec:::swap_crf_model(f, "ecxexp")
  expect_true(grepl("log(x)", deparse1(out), fixed = TRUE))
})

test_that("bnec_hurdle rejects inputs that break the zero-as-death convention", {
  dat <- data.frame(x = rep(1:4, each = 5), y = c(rep(1, 15), rep(0, 5)))
  # no zeros: nothing died, so there is no survival component to fit
  expect_error(
    bnec_hurdle(y ~ crf(x, "nec3param"), data = transform(dat, y = 1)),
    "no zeros"
  )
  # all zeros
  expect_error(
    bnec_hurdle(y ~ crf(x, "nec3param"), data = transform(dat, y = 0)),
    "Every value"
  )
  # NA cannot be distinguished from a death
  na_dat <- dat; na_dat$y[1] <- NA
  expect_error(
    bnec_hurdle(y ~ crf(x, "nec3param"), data = na_dat), "contains NA"
  )
  # negatives cannot be represented by a zero-bounded growth component
  neg_dat <- dat; neg_dat$y[1] <- -3
  expect_error(
    bnec_hurdle(y ~ crf(x, "nec3param"), data = neg_dat), "negative values"
  )
  # response absent from data
  expect_error(
    bnec_hurdle(z ~ crf(x, "nec3param"), data = dat), "not a column"
  )
  # non-numeric response
  chr_dat <- dat; chr_dat$y <- as.character(chr_dat$y)
  expect_error(
    bnec_hurdle(y ~ crf(x, "nec3param"), data = chr_dat), "must be numeric"
  )
})

test_that("crossed_weights is the outer product of component weights", {
  # Mock two model-averaged fits: crossed_weights only reads mod_stats.
  mock_manec <- function(w) {
    structure(list(mod_stats = data.frame(wi = w, row.names = names(w))),
              class = "bayesmanecfit")
  }
  obj <- structure(
    list(growth = mock_manec(c(nec3param = 0.7, nec4param = 0.3)),
         survival = mock_manec(c(ecxexp = 0.25, ecxll3 = 0.75))),
    class = c("bayesnechurdlefit", "bnecfit")
  )
  w <- crossed_weights(obj)
  expect_equal(dim(w), c(2L, 2L))
  expect_equal(sum(w), 1)
  expect_equal(unname(w["nec3param", "ecxll3"]), 0.7 * 0.75)
  expect_equal(unname(w["nec4param", "ecxexp"]), 0.3 * 0.25)
  expect_equal(dimnames(w),
               list(growth = c("nec3param", "nec4param"),
                    survival = c("ecxexp", "ecxll3")))
})

test_that("crossed_weights handles single-model components", {
  obj <- structure(
    list(growth = structure(list(model = "nec3param"), class = "bayesnecfit"),
         survival = structure(list(model = "ecxexp"), class = "bayesnecfit")),
    class = c("bayesnechurdlefit", "bnecfit")
  )
  w <- crossed_weights(obj)
  expect_equal(unname(w), matrix(1, 1, 1))
  expect_equal(dimnames(w), list(growth = "nec3param", survival = "ecxexp"))
})

test_that("crossed_weights rejects other classes", {
  expect_error(crossed_weights(list()), "bayesnechurdlefit")
})

test_that("nec and ecx generics accept extra arguments", {
  # The bayesnechurdlefit methods add a `which` argument, which requires the
  # generics to carry `...`. Guards against that being removed.
  expect_true("..." %in% names(formals(nec)))
  expect_true("..." %in% names(formals(ecx)))
})
