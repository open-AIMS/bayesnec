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

test_that("a missing value is refused before the first level is fitted", {
  # Each level is a complete fit, so without a check before the loop a missing
  # value in level "b" would be reached only after level "a" had compiled and
  # sampled. Measured on two levels of twelve: level "a" ran to completion
  # before level "b" raised. The whole data frame is checked, so the row is
  # named as the user recorded it rather than by its position within a subset.
  # See #278.
  d <- data.frame(x = rep(1:10, 2), y = rep(seq(0.9, 0.1, length = 10), 2),
                  site = rep(c("a", "b"), each = 10))
  d$y[14] <- NA
  msgs <- capture.output(
    msg <- tryCatch(bnec_group(y ~ crf(x, "nec3param"), d, group_var = "site"),
                    error = conditionMessage),
    type = "message"
  )
  expect_match(msg, "1 row\\(s\\) with missing values")
  expect_match(msg, "at row\\(s\\) 14")
  # Nothing was fitted: bnec_group() announces every level it starts, so the
  # absence of that message is what says the loop was never entered.
  expect_false(any(grepl("Fitting level", msgs)))
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
  # Columns are the names nec() itself returns, not fixed positions -- see the
  # posterior/prob_vals tests below for why that matters.
  expect_equal(names(out), c("level", "Q50", "Q2.5", "Q97.5"))
  # the same fit at both levels must give the same numbers -- the map must not
  # be reordering or recycling anything
  expect_equal(out$Q50[1], out$Q50[2])
})

# #33 second review: group_estimate_table() read positions 1:3 of whatever the
# underlying method returned, while passing `...` straight through to methods
# that take `posterior` and `prob_vals`. Both are silent-wrong-answer routes.

test_that("posterior = TRUE is refused rather than tabulated", {
  # nec(posterior = TRUE) returns the draws, not a summary, so a positional
  # table reported draws 1, 2 and 3 as an estimate and its credible interval.
  gf <- fake_group_fit()
  expect_error(nec(gf, posterior = TRUE), "one row per level")
  expect_error(ecx(gf, posterior = TRUE), "one row per level")
  expect_error(nsec(gf, posterior = TRUE), "one row per level")
  # and the message points at the route that does work
  expect_error(nec(gf, posterior = TRUE), "lapply\\(x\\$fits")
})

test_that("a non-default prob_vals is carried through, not truncated", {
  skip_if(Sys.getenv("NOT_CRAN") == "")
  gf <- fake_group_fit()
  # Five quantiles, in nec()'s required central/lower/upper order. Reading
  # positions 1:3 dropped the last two without a word.
  out <- nec(gf, prob_vals = c(0.5, 0.05, 0.95, 0.25, 0.75))
  expect_equal(names(out), c("level", "Q50", "Q5", "Q95", "Q25", "Q75"))
  expect_equal(nrow(out), 2)
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

test_that("the method is read off the fits where they still carry it", {
  # Correction to the earlier rationale: expand_manec() *does* stamp the method
  # on the weight vector, as attr(mod_stats$wi, "method"). It survives cbind but
  # not row-subsetting, so it is a reliable cross-check where present and no
  # substitute for capturing the request at fit time where it is not.
  expect_equal(attr(manec_example$mod_stats$wi, "method"), "pseudobma")
  stacked <- manec_example
  attr(stacked$mod_stats$wi, "method") <- "stacking"
  # weights_method says pseudobma; the fits say otherwise, and the fits win
  gf <- fake_group_fit(list(a = stacked, b = stacked))
  gf$weights_method <- "pseudobma"
  expect_error(crossed_group_weights(gf), "pseudo-BMA weights only")
})

test_that("levels weighted by different methods are refused", {
  # Their weights are not on a common footing, so there is no table to return.
  stacked <- manec_example
  attr(stacked$mod_stats$wi, "method") <- "stacking"
  gf <- fake_group_fit(list(a = manec_example, b = stacked))
  expect_error(crossed_group_weights(gf), "different methods")
})

test_that("a missing value in the grouping column is refused", {
  # data[grp == lev, ] is logical indexing with NA present, which puts an
  # all-NA row into *every* level's subset.
  d <- data.frame(x = 1:20, y = runif(20),
                  site = c(rep("a", 9), NA, rep("b", 10)))
  expect_error(
    bnec_group(y ~ crf(x, "nec3param"), d, group_var = "site"),
    "missing value"
  )
})

test_that("the pooled comparison is on the same observations", {
  # The third reading from the queue: does the factor matter at all? A pooled
  # fit ignoring the factor is scored on the same observations, so the WAICs
  # are directly comparable.
  gf <- fake_group_fit()
  cw <- crossed_group_weights(gf, pooled = manec_example)
  ms <- manec_example$mod_stats
  best <- rownames(ms)[which.max(ms$wi)]
  w <- ms$waic[match(best, rownames(ms))]
  expect_equal(cw$pooled$waic_grouped, 2 * w)
  expect_equal(cw$pooled$waic_pooled, w)
  expect_equal(cw$pooled$diff, w - 2 * w)
  expect_equal(cw$pooled$pooled_model, best)
  # manec_example carries no pointwise values, so the SE is NA rather than
  # silently omitted or invented
  expect_true(is.na(cw$pooled$se_diff))
  expect_true(is.na(cw$pooled$n_obs))
})

test_that("the pooled fit must be a bnec fit", {
  gf <- fake_group_fit()
  expect_error(crossed_group_weights(gf, pooled = 1:10),
               "bayesnecfit or bayesmanecfit")
})

test_that("no pooled argument leaves the result as it was", {
  gf <- fake_group_fit()
  expect_null(crossed_group_weights(gf)$pooled)
  expect_named(crossed_group_weights(gf),
               c("per_level", "best_combination", "best_weight",
                 "common_models", "diagonal"))
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

# The standard error on the pooled comparison pairs each observation's pointwise
# WAIC across the two fits, so the level-ordered grouped values have to line up
# with the pooled fit's row order. Built from synthetic pointwise values rather
# than fitted: the hazard is the reordering, and a fit would only obscure it.

fake_waic_fit <- function(pointwise) {
  structure(
    list(model = "m",
         fit = structure(
           list(criteria = list(waic = list(
             estimates = matrix(sum(pointwise), 1, 1,
                                dimnames = list("waic", "Estimate")),
             pointwise = cbind(waic = pointwise)))),
           class = "brmsfit")),
    class = c("bayesnecfit", "bnecfit")
  )
}

test_that("the pooled SE pairs observations across differently ordered data", {
  # site is deliberately NOT in level order, which is the case that would go
  # wrong if the grouped values were simply concatenated against the pooled.
  d <- data.frame(site = rep(c("b", "a"), each = 10))
  grp <- factor(d$site, levels = c("a", "b"))
  pooled_pw <- as.numeric(seq_len(20))
  # every "a" row differs by 1, every "b" row by 2
  lvl_a <- pooled_pw[grp == "a"] - 1
  lvl_b <- pooled_pw[grp == "b"] - 2
  gf <- structure(
    list(fits = list(a = fake_waic_fit(lvl_a), b = fake_waic_fit(lvl_b)),
         group_var = "site", levels = c("a", "b"), data = d,
         n = c(10L, 10L), weights_method = "pseudobma"),
    class = c("bayesnecgroupfit", "bnecfit")
  )
  res <- crossed_group_weights(gf, pooled = fake_waic_fit(pooled_pw))$pooled
  expect_equal(res$waic_grouped, sum(lvl_a) + sum(lvl_b))
  expect_equal(res$waic_pooled, sum(pooled_pw))
  expect_equal(res$diff, sum(pooled_pw) - sum(lvl_a) - sum(lvl_b))
  expect_equal(res$n_obs, 20L)
  # the paired differences are 1 for every a row and 2 for every b row, which
  # they only are if the reordering is right
  expect_equal(res$se_diff, sqrt(20) * sd(c(rep(1, 10), rep(2, 10))))
})

test_that("a pooled fit on different observations leaves the SE NA", {
  d <- data.frame(site = rep(c("a", "b"), each = 10))
  gf <- structure(
    list(fits = list(a = fake_waic_fit(as.numeric(1:10)),
                     b = fake_waic_fit(as.numeric(11:20))),
         group_var = "site", levels = c("a", "b"), data = d,
         n = c(10L, 10L), weights_method = "pseudobma"),
    class = c("bayesnecgroupfit", "bnecfit")
  )
  # 19 pooled observations against 20 grouped ones: the point estimates are
  # still returned, the SE is not invented.
  res <- crossed_group_weights(gf,
                               pooled = fake_waic_fit(as.numeric(1:19)))$pooled
  expect_false(is.na(res$diff))
  expect_true(is.na(res$se_diff))
  expect_true(is.na(res$n_obs))
})

test_that("a model-averaged level yields a WAIC difference but no SE", {
  # expand_manec() snapshots mod_fits *before* the expand_nec() loop that calls
  # add_criteria(), so a bayesmanecfit keeps each model's WAIC point estimate in
  # mod_stats and none of the pointwise values. The comparison still works; the
  # standard error is not available and must not be invented.
  expect_null(manec_example$mod_fits[[1]]$fit$criteria$waic$pointwise)
  gf <- fake_group_fit()
  res <- crossed_group_weights(gf, pooled = manec_example)$pooled
  expect_false(is.na(res$diff))
  expect_true(is.na(res$se_diff))
})
