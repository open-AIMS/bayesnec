# #33 stage 1. Levels are fitted independently and model-averaged within level;
# the crossed weights across levels follow from the per-level weights alone.
#
# The fitting itself is exercised by hand rather than here: two real fits per
# test would dominate the suite. What is tested here is everything around it --
# validation, the crossed-weight arithmetic, and the per-level dispatch -- using
# manec_example as a stand-in for a fitted level.

fake_group_fit <- function(fits = list(a = manec_example, b = manec_example)) {
  out <- list(fits = fits, group_var = "site", levels = names(fits),
              formula = manec_example$bayesnecformula,
              data = manec_example$mod_fits[[1]]$fit$data,
              family = manec_example$mod_fits[[1]]$fit$family,
              n = rep(50L, length(fits)))
  bayesnec:::allot_class(out, c("bayesnecgroupfit", "bnecfit"))
}

test_that("a grouping column must exist, be categorical, and have >1 level", {
  d <- data.frame(x = 1:20, y = runif(20), site = rep(c("a", "b"), 10),
                  one = "only", num = rep(1:2, 10))
  f <- y ~ crf(x, "nec3param")
  expect_error(bnec_group(f, d, group_var = "nope"), "not a column")
  # A numeric column is almost always a predictor that belongs in crf(), so
  # silently factorising it would be the wrong kindness.
  expect_error(bnec_group(f, d, group_var = "num"), "numeric")
  expect_error(bnec_group(f, d, group_var = "one"), "nothing to compare")
  expect_error(bnec_group(f, d, group_var = c("a", "b")), "single column name")
})

test_that("a level too small to support a fit is refused", {
  # Each level is a complete concentration-response model in its own right.
  d <- data.frame(x = 1:20, y = runif(20),
                  site = c(rep("a", 18), "b", "b"))
  expect_error(
    bnec_group(y ~ crf(x, "nec3param"), d, group_var = "site"),
    "fewer than 4 observations"
  )
})

test_that("crossed_group_weights requires the right class", {
  expect_error(crossed_group_weights(manec_example), "bayesnecgroupfit")
})

test_that("crossed weights are the outer product of the per-level weights", {
  # The identity stage 1 rests on: levels partition the data disjointly and
  # share no parameters, so elpd is additive and under pseudo-BMA the crossed
  # weight of a combination is the product of its per-level weights.
  gf <- fake_group_fit()
  cw <- crossed_group_weights(gf)
  expect_named(cw$per_level, c("a", "b"))
  w <- manec_example$mod_stats$wi
  names(w) <- rownames(manec_example$mod_stats)
  expect_equal(cw$per_level$a, w)
  # best combination is the per-level argmax, and its weight the product
  expect_equal(unname(cw$best_combination),
               rep(names(w)[which.max(w)], 2))
  expect_equal(cw$best_weight, max(w)^2)
})

test_that("the diagonal is over models common to every level and sums to 1", {
  # The diagonal answers a different question from the unrestricted maximum:
  # which single equation best describes every level.
  gf <- fake_group_fit()
  cw <- crossed_group_weights(gf)
  expect_setequal(cw$common_models, rownames(manec_example$mod_stats))
  expect_equal(sum(cw$diagonal), 1)
  # sorted decreasing, so the first entry is the best common form
  expect_false(is.unsorted(rev(cw$diagonal)))
})

test_that("the diagonal only covers models every level actually fitted", {
  # A model dropped from one level by check_models() cannot be the common form.
  one_model <- suppressMessages(pull_out(manec_example, model = "nec4param"))
  gf <- fake_group_fit(list(a = manec_example, b = one_model))
  cw <- crossed_group_weights(gf)
  expect_equal(cw$common_models, "nec4param")
  expect_equal(names(cw$diagonal), "nec4param")
})

test_that("a single-model level contributes a weight of exactly 1", {
  one_model <- suppressMessages(pull_out(manec_example, model = "nec4param"))
  gf <- fake_group_fit(list(a = one_model, b = one_model))
  cw <- crossed_group_weights(gf)
  expect_equal(unname(cw$per_level$a), 1)
  expect_equal(cw$best_weight, 1)
})

test_that("per-level estimates come back as one row per level", {
  skip_if(Sys.getenv("NOT_CRAN") == "")
  gf <- fake_group_fit()
  out <- nec(gf)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
  expect_setequal(out$level, c("a", "b"))
  expect_true(all(c("Estimate", "Q2.5", "Q97.5") %in% names(out)))
  # the same fit at both levels must give the same numbers -- the map must not
  # be reordering or recycling anything
  expect_equal(out$Estimate[1], out$Estimate[2])
})

test_that("printing reports the shared family and the per-level model sets", {
  gf <- fake_group_fit()
  out <- capture.output(print(gf))
  expect_true(any(grepl("bayesnecgroupfit", out)))
  expect_true(any(grepl("site", out)))
  # the family is shared, so it is reported once rather than per level
  expect_equal(sum(grepl("family", out)), 1)
})
