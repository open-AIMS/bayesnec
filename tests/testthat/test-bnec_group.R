# Levels are fitted independently and model-averaged within level.
# Use manec_example as each fitted level to check validation, dispatch and
# crossed weights without repeated sampling.

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
  # Validate the whole data frame before any level compiles or samples.
  # Report the original row number, not its position within a level.
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
  # Read estimate columns by name: posterior draws and custom quantiles change
  # the returned shape. Append censoring marks after the numeric columns.
  expect_equal(names(out), c("level", "Q50", "Q2.5", "Q97.5",
                             "bound_Q50", "bound_Q2.5", "bound_Q97.5"))
  # the same fit at both levels must give the same numbers -- the map must not
  # be reordering or recycling anything
  expect_equal(out$Q50[1], out$Q50[2])
  # manec_example is identified inside its own grid, so nothing is a bound.
  expect_true(all(unlist(out[c("bound_Q50", "bound_Q2.5", "bound_Q97.5")]) ==
                    ""))
})

# Forwarded posterior and prob_vals arguments must not turn draws or
# truncated quantile vectors into apparently valid estimate tables.

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
  q <- c("Q50", "Q5", "Q95", "Q25", "Q75")
  expect_equal(names(out), c("level", q, paste0("bound_", q)))
  expect_equal(nrow(out), 2)
})

# Use manec_example on a grid ending inside its posterior to test censoring
# marks without sampling. Expand once because every equation is predicted.
censored_level <- local({
  fit <- NULL
  function() {
    if (is.null(fit)) {
      fs <- manec_example$mod_fits
      forms <- lapply(fs, function(z) z$bayesnecformula)
      x_range <- c(min(fs[["ecx4param"]]$fit$data$x), 0.9)
      fit <<- bayesnec:::allot_class(
        suppressMessages(suppressWarnings(bayesnec:::expand_manec(
          fs, formula = forms, x_range = x_range, resolution = 50
        ))),
        c("bayesmanecfit", "bnecfit")
      )
    }
    fit
  }
})

# The number and the mark a level's own fit reports, in the form the group
# table holds them: the numbers as they were, and "" where there is no record.
own_estimate <- function(e) {
  bound <- attr(e, "censored_summary")$bound
  list(values = unname(as.numeric(e)), names = names(e),
       bound = if (is.null(bound)) rep("", length(e)) else bound)
}

expect_level_agrees <- function(out, row, e) {
  own <- own_estimate(e)
  expect_identical(unname(unlist(out[row, own$names])), own$values)
  expect_identical(unname(unlist(out[row, paste0("bound_", own$names)])),
                   own$bound)
}

test_that("the table marks a censored level as its own fit does", {
  skip_if(Sys.getenv("NOT_CRAN") == "")
  cens <- censored_level()
  gf <- fake_group_fit(list(a = cens, b = manec_example))
  out <- suppressMessages(suppressWarnings(nec(gf)))
  own_a <- suppressMessages(suppressWarnings(nec(cens)))
  own_b <- suppressMessages(suppressWarnings(nec(manec_example)))
  # The fixture must be censored above, or every mark checked here is empty.
  expect_true(any(attr(own_a, "censored_summary")$bound == ">="))
  expect_level_agrees(out, 1, own_a)
  # An uncensored level is unchanged: its numbers are its own nec(), and none
  # of its entries is marked.
  expect_null(attr(own_b, "censored_summary"))
  expect_level_agrees(out, 2, own_b)
  # A column survives the subsetting and stacking that would drop an attribute.
  expect_identical(out[1, "bound_Q50"],
                   attr(own_a, "censored_summary")$bound[1])
  expect_identical(rbind(out, out)$bound_Q97.5, rep(out$bound_Q97.5, 2))
})

test_that("nsec() and ecx() on the group agree with each level's own fit", {
  skip_if(Sys.getenv("NOT_CRAN") == "")
  cens <- censored_level()
  gf <- fake_group_fit(list(a = cens, b = manec_example))
  # Above: an ECx50 the curves do not reach before 0.9.
  x_top <- c(min(manec_example$mod_fits[[1]]$fit$data$x), 0.9)
  out <- suppressWarnings(ecx(gf, ecx_val = 50, x_range = x_top))
  for (i in 1:2) {
    own <- suppressWarnings(ecx(gf$fits[[i]], ecx_val = 50, x_range = x_top))
    expect_level_agrees(out, i, own)
  }
  expect_true(any(out$bound_Q50 == ">="))
  # Below: a grid starting at 2.5 leaves the crossings before its foot, so
  # the marks are "<=" rather than ">=".
  out <- suppressWarnings(nsec(gf, x_range = c(2.5, 3.2), resolution = 50))
  for (i in 1:2) {
    own <- suppressWarnings(nsec(gf$fits[[i]], x_range = c(2.5, 3.2),
                                 resolution = 50))
    expect_level_agrees(out, i, own)
  }
  expect_true(any(out$bound_Q50 == "<="))
  expect_false(any(unlist(out[grep("^bound_", names(out))]) == ">="))
})

test_that("the marks are each record's bound vector, and empty without one", {
  plain <- c(Q50 = 1.2, Q2.5 = 0.5, Q97.5 = 1.4)
  marked <- c(Q50 = 0.9, Q2.5 = 0.5, Q97.5 = 0.9)
  attr(marked, "censored_summary") <- list(bound = c(">=", "", ">="))
  out <- bayesnec:::estimate_marks(list(marked, plain), names(plain),
                                   c("a", "b"), "nec")
  expect_identical(names(out), c("bound_Q50", "bound_Q2.5", "bound_Q97.5"))
  expect_identical(unlist(out[1, ], use.names = FALSE), c(">=", "", ">="))
  expect_identical(unlist(out[2, ], use.names = FALSE), rep("", 3))
  expect_type(out$bound_Q50, "character")
})

test_that("a record that cannot be aligned with the entries is refused", {
  # summarise_censored() writes one mark per entry, so this is a guard against a
  # record built elsewhere, not a path a fit reaches.
  e <- c(Q50 = 0.9, Q2.5 = 0.5, Q97.5 = 0.9)
  attr(e, "censored_summary") <- list(bound = c(">=", ""))
  expect_error(
    bayesnec:::estimate_marks(list(e), names(e), "a", "nec"),
    "marks 2 entries and its nec estimate has 3"
  )
})

test_that("printing reports the shared family and the per-level model sets", {
  gf <- fake_group_fit()
  out <- capture.output(print(gf))
  expect_true(any(grepl("bayesnecgroupfit", out)))
  expect_true(any(grepl("site", out)))
  # the family is shared, so it is reported once rather than per level
  expect_equal(sum(grepl("family", out)), 1)
})

test_that("plot() of a group fit gives the all_models warning once (#120)", {
  # Each level maps deprecated all_models onto its own set, but the caller
  # should receive one warning. Mock legend() to check every panel and equation.
  skip_on_cran()
  gf <- fake_group_fit(list(a = manec_example, b = manec_example,
                            c = manec_example))
  labels <- character()
  local_mocked_bindings(
    legend = function(..., legend = NULL) {
      labels <<- c(labels, as.character(legend))
    },
    .package = "bayesnec"
  )
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  first <- collect_warnings(plot(gf, all_models = TRUE))
  expect_length(first$warnings, 1)
  expect_match(first$warnings, "`all_models` is deprecated", fixed = TRUE)
  for (m in manec_example$success_models) {
    expect_equal(sum(labels == m), 3)
  }
  # The muffling lasts one call: the next call warns again, once.
  second <- collect_warnings(plot(gf, all_models = FALSE))
  expect_length(second$warnings, 1)
  # And the replacement arguments reach every level without a warning.
  labels <- character()
  expect_silent(plot(gf, model = "nec4param", average = FALSE))
  expect_equal(sum(labels == "nec4param"), 3)
})

# The crossed-weight product requires pseudo-BMA. loo_controls reaches
# bnec() through dots, so stacking must be rejected before building the table.

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
  # A hand-built object without the method field may reach the calculation. Its
  # missing fits can still fail later; assert only that method checking passes.
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
  # expand_manec() records the method on mod_stats$wi. The attribute survives
  # cbind but not row-subsetting, so cross-check it where present and keep the
  # method captured at fit time as well.
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
  # A pooled fit scores the same observations while ignoring the factor,
  # so its WAIC can be compared with the grouped fit.
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

# Each level is fitted independently, so compare_posterior() can use its
# random-permutation pairing on the named list of fits. That pairing does not
# apply to dependent levels within one fit.

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

# Pair pointwise WAIC values by original observation when comparing grouped
# and pooled fits. Synthetic values isolate ordering errors without sampling.

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
  # expand_manec() snapshots mod_fits before expand_nec() adds pointwise WAIC.
  # The stored mod_stats still gives point estimates, so omit only the SE.
  expect_null(manec_example$mod_fits[[1]]$fit$criteria$waic$pointwise)
  gf <- fake_group_fit()
  res <- crossed_group_weights(gf, pooled = manec_example)$pooled
  expect_false(is.na(res$diff))
  expect_true(is.na(res$se_diff))
})

test_that("the asymptote declaration is refused before any level (#394)", {
  # Validate the whole response before the level loop, so no earlier level
  # samples before a later refusal and the message can name the remedy.
  set.seed(394)
  x <- rep(c(0, 1, 2, 4, 8, 16), each = 4)
  mu <- 0.9 - 0.4 * (x / max(x))
  d <- data.frame(
    x = rep(x, 2),
    y = c(stats::rnorm(length(mu), mu, 0.03) - 1.5,
          stats::rnorm(length(mu), mu, 0.03) - 1.5),
    site = rep(c("a", "b"), each = length(x))
  )
  err <- expect_error(suppressWarnings(suppressMessages(bnec_group(
    y ~ crf(x, c("nec3param", "nec4param")), d, group_var = "site",
    asymptote_observed = FALSE, chains = 1, iter = 10))))
  expect_match(conditionMessage(err), "`prior` argument", fixed = TRUE)
})

test_that("bnec_group reads an abbreviated declaration (#394)", {
  # do.call() partially matches asymptote_observed. bnec_group() checks it
  # before the level loop and must read the abbreviation before setting its
  # checked marker, or a refusal can fall into each level's model loop.
  set.seed(394)
  x <- rep(c(0, 1, 2, 4, 8, 16), each = 4)
  mu <- 0.9 - 0.4 * (x / max(x))
  d <- data.frame(
    x = rep(x, 2),
    y = c(stats::rnorm(length(mu), mu, 0.03) - 1.5,
          stats::rnorm(length(mu), mu, 0.03) - 1.5),
    site = rep(c("a", "b"), each = length(x))
  )
  err <- expect_error(suppressWarnings(suppressMessages(bnec_group(
    y ~ crf(x, "nec4param"), d, group_var = "site",
    asymptote = FALSE, chains = 1, iter = 10))))
  expect_match(conditionMessage(err), "`prior` argument", fixed = TRUE)
})

test_that("bnec_group validates the declaration before any level (#394)", {
  # Read here before bnec() sees it, so an invalid value would otherwise raise
  # the floor refusal or the mixed-set report rather than naming what is wrong.
  set.seed(394)
  x <- rep(c(0, 1, 2, 4, 8, 16), each = 4)
  mu <- 0.9 - 0.4 * (x / max(x))
  d <- data.frame(
    x = rep(x, 2),
    y = c(stats::rnorm(length(mu), mu, 0.03),
          stats::rnorm(length(mu), mu, 0.03)),
    site = rep(c("a", "b"), each = length(x))
  )
  expect_error(suppressWarnings(suppressMessages(bnec_group(
    y ~ crf(x, "nec4param"), d, group_var = "site",
    asymptote_observed = NA, chains = 1, iter = 10))),
    "asymptote_observed", fixed = TRUE)
})

test_that("bnec_group resolves a dot argument the way do.call will (#394)", {
  # dots_arg() must match the value do.call() forwards, including unambiguous
  # model_survival abbreviations. Exact lookup would give the flatness report
  # and hurdle gate no survival equation.
  dots <- list(asymptote = FALSE, model_s = "nec3param", prior_type = "x")
  expect_false(bayesnec:::dots_arg(dots, "asymptote_observed", TRUE))
  expect_identical(bayesnec:::dots_arg(dots, "model_survival"), "nec3param")
  expect_null(bayesnec:::dots_arg(dots, "prior"))
  # An exact name wins over an abbreviation of the same formal.
  both <- list(asymptote = TRUE, asymptote_observed = FALSE)
  expect_false(bayesnec:::dots_arg(both, "asymptote_observed", TRUE))
  # Nothing supplied gives the default, and an empty list does not error.
  expect_true(bayesnec:::dots_arg(list(), "asymptote_observed", TRUE))
  expect_true(bayesnec:::dots_arg(list(chains = 2), "asymptote_observed",
                                  TRUE))
})

test_that("a level at a bound is fitted with ecxflat, the others as asked (#419)", {
  # A level at one bound throughout gets ecxflat; other levels keep their set.
  # Report each substitution once, before fitting; level calls do not repeat it.
  # Mocked fits check dispatch; test-bnec.R checks the fitted substitution.
  x <- rep(c(0.1, 0.5, 1, 3, 10, 30), each = 5)
  a <- rep(1L, length(x))
  a[x == max(x)] <- c(0L, 0L, 1L, 0L, 1L)
  d <- data.frame(x = rep(x, 2), alive = c(a, rep(1L, length(x))),
                  site = rep(c("a", "b"), each = length(x)))
  seen <- list()
  local_mocked_bindings(
    bnec = function(formula, data, ...) {
      seen[[length(seen) + 1L]] <<- list(
        rows = nrow(data), all_one = all(data$alive == 1L),
        reported = isTRUE(list(...)[[".bayesnec_bound_reported"]])
      )
      manec_example
    },
    .package = "bayesnec"
  )
  msgs <- character(0)
  fit <- withCallingHandlers(
    bnec_group(alive ~ crf(x, c("nec3param", "ecx4param")), d,
               group_var = "site", family = "bernoulli"),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_s3_class(fit, "bayesnecgroupfit")
  expect_length(seen, 2)
  expect_false(seen[[1]]$all_one)
  expect_true(seen[[2]]$all_one)
  expect_true(all(vapply(seen, `[[`, logical(1), "reported")))
  report <- grep("fitted with the constant equation ecxflat alone", msgs,
                 fixed = TRUE, value = TRUE)
  expect_length(report, 1)
  expect_match(report, "1 level(s) of \"site\": \"b\" (the upper bound)",
               fixed = TRUE)
  # Said before the first level is fitted.
  expect_lt(which(msgs == report),
            min(grep("Fitting level", msgs, fixed = TRUE)))
})

test_that("a group name reaches each level, ecxflat included (#485)", {
  # The level that does not vary is fitted with ecxflat alone; the other is
  # given the whole group, ecxflat included. The level fits are mocked.
  x <- rep(c(0.1, 0.5, 1, 3, 10, 30), each = 5)
  a <- rep(1L, length(x))
  a[x == max(x)] <- c(0L, 0L, 1L, 0L, 1L)
  d <- data.frame(x = rep(x, 2), alive = c(a, rep(1L, length(x))),
                  site = rep(c("a", "b"), each = length(x)))
  sets <- list()
  local_mocked_bindings(
    bnec = function(formula, data, ...) {
      sets[[length(sets) + 1L]] <<- get_model_from_formula(formula)
      manec_example
    },
    .package = "bayesnec"
  )
  msgs <- character(0)
  withCallingHandlers(
    bnec_group(alive ~ crf(x, "decline"), d, group_var = "site",
               family = "bernoulli"),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_length(sets, 2)
  expect_identical(sets[[1]], mod_groups$decline)
  expect_true("ecxflat" %in% sets[[1]])
  # The group named more than ecxflat, so the level at a bound is reported.
  expect_length(grep("fitted with the constant equation ecxflat alone", msgs,
                     fixed = TRUE), 1)
})

test_that("a beta level at 0 still refuses the whole call before any level (#419)", {
  # A beta response of zero throughout has no positive value to define its
  # boundary shift. Refuse the whole call before fitting any level.
  x <- rep(c(0.1, 0.5, 1, 3, 10, 30), each = 5)
  d <- data.frame(x = rep(x, 2),
                  cover = c(seq(0.9, 0.2, length.out = length(x)),
                            rep(0, length(x))),
                  site = rep(c("a", "b"), each = length(x)))
  calls <- 0L
  local_mocked_bindings(
    bnec = function(...) {
      calls <<- calls + 1L
      stop("level fit should not start")
    },
    .package = "bayesnec"
  )
  err <- tryCatch(
    suppressMessages(
      bnec_group(cover ~ crf(x, c("nec3param", "ecx4param")), d,
                 group_var = "site", family = "Beta")
    ),
    error = conditionMessage
  )
  expect_match(err, "1 level(s) of \"site\": \"b\", where every value is 0",
               fixed = TRUE)
  expect_match(err, "Remove that level from `data`", fixed = TRUE)
  expect_identical(calls, 0L)
})

test_that("a zero_inflated_beta level at a bound follows beta (#441)", {
  # A level of ones dispatches to ecxflat; a level of zeros has no beta-block
  # data and refuses the whole call before fitting.
  x <- rep(c(0.1, 0.5, 1, 3, 10, 30), each = 5)
  varying <- seq(0.9, 0.2, length.out = length(x))
  varying[x == max(x)] <- c(0, 0, 0.2, 0, 0.1)
  seen <- list()
  local_mocked_bindings(
    bnec = function(formula, data, ...) {
      seen[[length(seen) + 1L]] <<- list(
        all_one = all(data$cover == 1),
        reported = isTRUE(list(...)[[".bayesnec_bound_reported"]])
      )
      manec_example
    },
    .package = "bayesnec"
  )
  one <- data.frame(x = rep(x, 2), cover = c(varying, rep(1, length(x))),
                    site = rep(c("a", "b"), each = length(x)))
  msgs <- character(0)
  withCallingHandlers(
    bnec_group(cover ~ crf(x, c("nec3param", "ecx4param")), one,
               group_var = "site", family = "zero_inflated_beta"),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_length(seen, 2)
  expect_identical(vapply(seen, `[[`, logical(1), "all_one"), c(FALSE, TRUE))
  report <- grep("fitted with the constant equation ecxflat alone", msgs,
                 fixed = TRUE, value = TRUE)
  expect_length(report, 1)
  expect_match(report, "zero_inflated_beta response in every observation of 1",
               fixed = TRUE)
  expect_match(report, "\"b\" (the upper bound)", fixed = TRUE)
  seen <- list()
  zero <- one
  zero$cover[zero$site == "b"] <- 0
  err <- tryCatch(
    suppressMessages(
      bnec_group(cover ~ crf(x, c("nec3param", "ecx4param")), zero,
                 group_var = "site", family = "zero_inflated_beta")
    ),
    error = conditionMessage
  )
  expect_match(err, "1 level(s) of \"site\": \"b\", where every value is 0",
               fixed = TRUE)
  expect_match(err, "so the beta block has no data", fixed = TRUE)
  expect_length(seen, 0)
})

test_that("the group tables hold a level fitted with a single equation (#419)", {
  # A single-equation level has weight one, and common_models is the
  # intersection of the equations each level fitted. ecx4param stands in for
  # that level without sampling; nsec() must still return one row per level.
  g <- fake_group_fit(list(a = manec_example, b = ecx4param))
  cw <- crossed_group_weights(g)
  expect_identical(cw$per_level$b, c(ecx4param = 1))
  expect_identical(cw$common_models, "ecx4param")
  tab <- suppressWarnings(suppressMessages(nsec(g)))
  expect_identical(tab$level, c("a", "b"))
})

test_that("a binomial formula with no trials() term is refused before any level (#442)", {
  # The first level's bnec() call refused it with "subscript out of bounds"
  # from inside its model loop, once per equation. bnec() is replaced so that
  # starting a level fails the test.
  d <- nec_data
  d$site <- rep(c("a", "b"), length.out = nrow(d))
  calls <- 0L
  local_mocked_bindings(
    bnec = function(...) {
      calls <<- calls + 1L
      stop("level fit should not start")
    },
    .package = "bayesnec"
  )
  for (fam in c("binomial", "beta_binomial")) {
    msgs <- capture.output(
      err <- tryCatch(
        bnec_group(count ~ crf(x, c("nec3param", "nec4param")), d,
                   group_var = "site", family = fam),
        error = conditionMessage
      ),
      type = "message"
    )
    expect_match(err, paste("The", fam, "family needs the number of trials"),
                 fixed = TRUE, info = fam)
    expect_false(any(grepl("Fitting level", msgs)), info = fam)
  }
  expect_identical(calls, 0L)
})

test_that("with a trials() term every level is fitted (#442)", {
  d <- nec_data
  d$site <- rep(c("a", "b"), length.out = nrow(d))
  calls <- 0L
  local_mocked_bindings(
    bnec = function(...) {
      calls <<- calls + 1L
      manec_example
    },
    .package = "bayesnec"
  )
  for (fam in c("binomial", "beta_binomial")) {
    fit <- suppressWarnings(suppressMessages(
      bnec_group(count | trials(trials) ~ crf(x, c("nec3param", "nec4param")),
                 d, group_var = "site", family = fam)
    ))
    expect_s3_class(fit, "bayesnecgroupfit")
  }
  expect_identical(calls, 4L)
})


# Equations that keep no group-level term on a variable.

grouped_levels_data <- function() {
  d <- nec_data
  d$site <- rep(c("a", "b"), length.out = nrow(d))
  d$tank <- rep(c("t1", "t2", "t3"), length.out = nrow(d))
  d
}

test_that("bnec_group() reports the exclusion once, before any level (#470)", {
  # Each level's bnec() call still makes the exclusion and records it; the
  # private marker silences only its report. The level fits are mocked.
  seen <- list()
  local_mocked_bindings(
    bnec = function(formula, data, ...) {
      seen[[length(seen) + 1L]] <<-
        isTRUE(list(...)[[".bayesnec_group_terms_reported"]])
      manec_example
    },
    .package = "bayesnec"
  )
  msgs <- character(0)
  fit <- withCallingHandlers(
    bnec_group(y ~ crf(x, c("ecxexp", "ecxll3")) + (ec50 | tank),
               grouped_levels_data(), group_var = "site",
               family = "gaussian"),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_s3_class(fit, "bayesnecgroupfit")
  expect_length(seen, 2)
  expect_true(all(unlist(seen)))
  report <- grep("Dropping the model(s) ecxexp", msgs, fixed = TRUE,
                 value = TRUE)
  expect_length(report, 1)
  expect_lt(which(msgs == report),
            min(grep("Fitting level", msgs, fixed = TRUE)))
})

test_that("bnec_group() refuses a set excluded at every level (#470)", {
  calls <- 0L
  local_mocked_bindings(
    bnec = function(...) {
      calls <<- calls + 1L
      stop("level fit should not start")
    },
    .package = "bayesnec"
  )
  err <- tryCatch(
    suppressMessages(
      bnec_group(y ~ crf(x, "ecxexp") + (nec | tank),
                 grouped_levels_data(), group_var = "site",
                 family = "gaussian")
    ),
    error = conditionMessage
  )
  expect_match(err, "At level \"a\" of \"site\"", fixed = TRUE)
  expect_match(err, "No equation requested can be fitted", fixed = TRUE)
  expect_identical(calls, 0L)
})

# A model set weighted by leaving out each level of `group`, holding one
# equation whose score per level is `elpd`.
fake_logo_set <- function(levels, elpd, group = "cell") {
  ms <- data.frame(waic = 10, wi = 1, row.names = "m")
  attr(ms$wi, "leave_out") <- group
  logo <- structure(list(pointwise = cbind(elpd_loo = elpd)), group = group,
                    levels = levels)
  structure(list(mod_stats = ms,
                 mod_fits = list(m = list(fit = list(criteria = list(
                   logo = logo
                 ))))),
            class = c("bayesmanecfit", "bnecfit"))
}

test_that("the pooled comparison uses the left-out score where all fits hold it", {
  d <- data.frame(site = rep(c("a", "b"), each = 4))
  gf <- structure(
    list(fits = list(a = fake_logo_set(c("a1", "a2"), c(-1, -2)),
                     b = fake_logo_set(c("b1", "b2"), c(-3, -4))),
         group_var = "site", levels = c("a", "b"), data = d,
         n = c(4L, 4L), weights_method = "pseudobma"),
    class = c("bayesnecgroupfit", "bnecfit")
  )
  # The pooled levels are in another order, so pairing must be by name.
  pooled <- fake_logo_set(c("b1", "a1", "b2", "a2"),
                          c(-3.5, -1.25, -4.5, -2.25))
  res <- crossed_group_weights(gf, pooled = pooled)$pooled
  expect_identical(res$criterion, "looic")
  expect_identical(res$group, "cell")
  expect_equal(res$looic_grouped, 20)
  expect_equal(res$looic_pooled, 23)
  expect_equal(res$diff, 3)
  expect_equal(res$n_groups, 4L)
  expect_equal(res$se_diff, sqrt(4) * sd(c(0.5, 0.5, 1, 1)))
  # A pooled fit scored by observation leaves the comparison on WAIC.
  expect_warning(res <- crossed_group_weights(gf, pooled = manec_example)$pooled,
                 "compared on WAIC")
  expect_identical(res$criterion, "waic")
})

test_that("the pooled comparison requires the same held-out levels", {
  d <- data.frame(site = rep(c("a", "b"), each = 4))
  gf <- structure(
    list(fits = list(a = fake_logo_set(c("a1", "a2"), c(-1, -2)),
                     b = fake_logo_set(c("b1", "b2"), c(-3, -4))),
         group_var = "site", levels = c("a", "b"), data = d,
         n = c(4L, 4L), weights_method = "pseudobma"),
    class = c("bayesnecgroupfit", "bnecfit")
  )
  pooled <- fake_logo_set(c("a1", "a2", "b1", "other"),
                          c(-1, -2, -3, -4))
  expect_warning(res <- crossed_group_weights(gf, pooled = pooled)$pooled,
                 "do not cover the same levels")
  expect_identical(res$criterion, "waic")
  gf$fits$b <- fake_logo_set(c("a1", "b2"), c(-3, -4))
  pooled <- fake_logo_set(c("a1", "a2", "b2"), c(-1, -2, -4))
  expect_warning(res <- crossed_group_weights(gf, pooled = pooled)$pooled,
                 "do not cover the same levels")
  expect_identical(res$criterion, "waic")
})
