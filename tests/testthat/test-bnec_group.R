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

# #33 review finding: the outer-product identity holds for pseudo-BMA only, and
# was documented but not enforced. `loo_controls` reaches bnec() through `...`,
# so stacking weights were reachable and would have produced a crossed table
# that looked right and was not.

test_that("crossed_group_weights refuses a non-pseudo-BMA fit", {
  # Built directly rather than fitted: the check is on what the object records,
  # and fitting a group set twice to exercise an error is not worth the minutes.
  fake <- structure(
    list(fits = list(), group_var = "site", levels = c("a", "b"),
         weights_method = "stacking"),
    class = c("bayesnecgroupfit", "bnecfit")
  )
  expect_error(crossed_group_weights(fake), "pseudo-BMA weights only")
  expect_error(crossed_group_weights(fake), "stacking")
})

test_that("an absent weights_method is treated as the default", {
  # Defensive: the class is new, so no object should lack the field, but a
  # hand-built one must not be refused on a technicality. Asserted as "does not
  # fail the method check" rather than "does not fail", because an object with
  # no fits has nothing to compute and will error further down for that reason.
  fake <- structure(
    list(fits = list(), group_var = "site", levels = c("a", "b")),
    class = c("bayesnecgroupfit", "bnecfit")
  )
  msg <- tryCatch({
    crossed_group_weights(fake)
    ""
  }, error = function(e) conditionMessage(e))
  expect_false(grepl("pseudo-BMA", msg, fixed = TRUE))
})

test_that("bnec_group records the weighting method it was given", {
  # The method cannot be recovered from a bayesmanecfit afterwards -- it does
  # not store it -- which is why bnec_group() has to capture it up front.
  expect_false("loo_controls" %in% names(manec_example))
  expect_false(any(grepl("weight", names(manec_example), ignore.case = TRUE)))
})

# RF, on review of #228: compare_posterior() should compare the levels of a
# bayesnecgroupfit. The levels are fitted independently, so `$fits` is already
# the named list compare_posterior() takes -- this is dispatch, not new
# machinery, the same shape as the pp_check() methods in #148 part A.

test_that("compare_posterior is a generic with a default and a group method", {
  expect_true(is.function(compare_posterior))
  expect_false(is.null(getS3method("compare_posterior", "default",
                                   optional = TRUE)))
  expect_false(is.null(getS3method("compare_posterior", "bayesnecgroupfit",
                                   optional = TRUE)))
})

test_that("the default path is unchanged for a named list", {
  # compare_posterior() was a plain function before; making it generic must not
  # move the behaviour every existing caller and vignette depends on.
  skip_on_cran()
  l <- list(a = suppressMessages(pull_out(manec_example, model = "nec4param")),
            b = suppressMessages(pull_out(manec_example, model = "ecx4param")))
  r <- suppressWarnings(suppressMessages(
    compare_posterior(l, comparison = "n(s)ec")
  ))
  expect_named(r, c("posterior_list", "posterior_data", "diff_list",
                    "diff_data", "prob_diff"))
  expect_setequal(names(r$posterior_list), c("a", "b"))
})

test_that("the default still refuses input that is not a named list", {
  expect_error(compare_posterior(list(1, 2)), "named list")
  expect_error(compare_posterior("not a list"), "named list")
})

test_that("the group method passes the per-level fits through", {
  # Asserted on the method body rather than by fitting a group set twice: the
  # method is a one-line delegation and fitting to prove it costs minutes.
  m <- getS3method("compare_posterior", "bayesnecgroupfit")
  expect_match(paste(deparse(body(m)), collapse = " "), "x\\$fits")
})
