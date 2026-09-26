# The censored summary (#395). The estimators are exercised in their own test
# files; what is pinned here is the machinery they share and the invariants that
# hold across them.

cens_record <- function(above, below, upper = 10, lower = 0) {
  bayesnec:::censoring_record(upper, lower, above, below)
}

test_that("a posterior with no beyond-range draw is summarised as before", {
  # The regression guard for every existing analysis. estimates_summary() must
  # return the release's number to the last bit where nothing is censored,
  # whether the record is absent or present and empty.
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  plain <- bayesnec:::estimates_summary(x)
  expect_identical(
    plain,
    stats::setNames(c(stats::median(x),
                      stats::quantile(x, c(0.025, 0.975), na.rm = TRUE)),
                    c("Estimate", "Q2.5", "Q97.5"))
  )
  attr(x, "censored") <- cens_record(logical(10), logical(10))
  expect_identical(bayesnec:::estimates_summary(x), plain)
  expect_null(attr(bayesnec:::estimates_summary(x), "censored_summary"))
})

test_that("a censored draw contributes its rank and no value", {
  # Nine draws inside the range and ninety-one above it. The median of the
  # whole sample is therefore above the top of the range and is reported as a
  # bound, where deleting the censored draws would have reported the median of
  # the nine.
  x <- c(1:9, rep(NA_real_, 91))
  attr(x, "censored") <- cens_record(c(rep(FALSE, 9), rep(TRUE, 91)),
                                     logical(100))
  out <- bayesnec:::estimates_summary(x)
  cens <- attr(out, "censored_summary")
  expect_equal(unname(out[["Estimate"]]), 10)
  expect_identical(cens$bound, c(">=", "", ">="))
  expect_identical(cens$n_above, 91L)
  expect_identical(cens$n_below, 0L)
  expect_identical(cens$n_draws, 100L)
  # The lower limit falls among the nine draws that were identified, so it is a
  # number and not a bound. It is the third of them, because the censored
  # summary reports an order statistic rather than interpolating between two.
  expect_equal(unname(out[["Q2.5"]]), 3)
  # This is not the treatment #39 removed: no censored draw was given the value
  # of the bound, so the median of the uncensored draws is unchanged by how
  # many draws were censored.
  expect_lt(stats::median(1:9), out[["Estimate"]])
})

test_that("the two ends are recorded and reported separately", {
  x <- c(rep(NA_real_, 30), 4:8, rep(NA_real_, 65))
  attr(x, "censored") <- cens_record(
    above = c(rep(FALSE, 35), rep(TRUE, 65)),
    below = c(rep(TRUE, 30), rep(FALSE, 70))
  )
  out <- bayesnec:::estimates_summary(x)
  cens <- attr(out, "censored_summary")
  expect_identical(cens$bound, c(">=", "<=", ">="))
  expect_identical(c(cens$n_below, cens$n_above), c(30L, 65L))
  expect_equal(unname(out[["Q2.5"]]), 0)
  expect_equal(unname(out[["Q97.5"]]), 10)
})

test_that("summarise_censored honours prob_vals and leaves names alone", {
  x <- c(1:50, rep(NA_real_, 50))
  attr(x, "censored") <- cens_record(c(rep(FALSE, 50), rep(TRUE, 50)),
                                     logical(100))
  out <- bayesnec:::summarise_censored(x, c(0.5, 0.1, 0.9))
  expect_named(out, c("50%", "10%", "90%"))
  # Fifty draws identified and fifty censored: the median is the fiftieth order
  # statistic, which is the last draw that was identified, so it is a number.
  # Only the 90 per cent quantile falls among the censored ranks.
  expect_identical(attr(out, "censored_summary")$bound, c("", "", ">="))
  expect_equal(unname(out[["50%"]]), 50)
})

test_that("a decreasing xform censors the same draws at the other end", {
  cens <- cens_record(above = c(TRUE, FALSE, FALSE), below = c(FALSE, FALSE,
                                                              TRUE),
                      upper = 10, lower = 2)
  flipped <- bayesnec:::xform_censoring(cens, function(x) -x)
  # Which draws are beyond the range is a property of the predictor and does
  # not change; the end each is named at does.
  expect_identical(flipped$above, cens$below)
  expect_identical(flipped$below, cens$above)
  expect_equal(c(flipped$lower, flipped$upper), c(-10, -2))
  rising <- bayesnec:::xform_censoring(cens, exp)
  expect_identical(rising$above, cens$above)
  expect_equal(c(rising$lower, rising$upper), c(exp(2), exp(10)))
})

test_that("component records combine in draw order, not by component", {
  # Two components of very unequal size: one wholly censored and small, one
  # uncensored and large. Counting components would call the combination half
  # censored; counting draws calls it a tenth.
  parts <- list(cens_record(rep(TRUE, 10), logical(10), upper = 5, lower = 1),
                NULL)
  out <- bayesnec:::concat_censoring(parts, c(10L, 90L))
  expect_identical(sum(out$above), 10L)
  expect_length(out$above, 100L)
  expect_equal(out$upper, 5)
  expect_null(bayesnec:::concat_censoring(list(NULL, NULL), c(10L, 90L)))
})

test_that("concat_censoring reports the bound true of every component", {
  parts <- list(cens_record(c(TRUE, FALSE), c(FALSE, FALSE), upper = 5,
                            lower = 1),
                cens_record(c(FALSE, TRUE), c(FALSE, FALSE), upper = 8,
                            lower = 0))
  out <- bayesnec:::concat_censoring(parts, c(2L, 2L))
  # ">= 5" is true of a draw known to be above 8 as well; ">= 8" is not true of
  # a draw known only to be above 5.
  expect_equal(out$upper, 5)
  expect_equal(out$lower, 1)
})

test_that("subset_censoring takes a component's share of a record", {
  cens <- cens_record(c(TRUE, FALSE, TRUE, FALSE), rep(FALSE, 4))
  out <- bayesnec:::subset_censoring(cens, c(1L, 2L))
  expect_identical(out$above, c(TRUE, FALSE))
  expect_equal(out$upper, 10)
  expect_null(bayesnec:::subset_censoring(NULL, 1:2))
})

test_that("a censored entry prints as a bound and states its fraction", {
  x <- c(1:9, rep(NA_real_, 91))
  attr(x, "censored") <- cens_record(c(rep(FALSE, 9), rep(TRUE, 91)),
                                     logical(100))
  mat <- t(as.matrix(bayesnec:::estimates_summary(x)))
  rownames(mat) <- "NSEC"
  attr(mat, "censored_summary") <-
    attr(bayesnec:::estimates_summary(x), "censored_summary")
  printed <- utils::capture.output(bayesnec:::print_mat(mat))
  expect_true(any(grepl(">= 10.00", printed, fixed = TRUE)))
  note <- utils::capture.output(
    bayesnec:::print_censoring_note(attr(mat, "censored_summary"), "NSEC")
  )
  expect_true(any(grepl("91 of 100 draws", note, fixed = TRUE)))
  expect_true(any(grepl("lie above 10", note, fixed = TRUE)))
})

test_that("combine_censored_min keeps the block that is inside the range", {
  # One block censored above and one not. The smaller of the two is the one
  # inside the range, so the combination is not censored -- which pmin() on the
  # raw vectors cannot say, because it propagates the NA.
  g <- c(NA_real_, 3, NA_real_)
  attr(g, "censored") <- cens_record(c(TRUE, FALSE, TRUE), rep(FALSE, 3))
  s <- c(4, 5, NA_real_)
  attr(s, "censored") <- cens_record(c(FALSE, FALSE, TRUE), rep(FALSE, 3))
  out <- bayesnec:::combine_censored_min(g, s, 3)
  expect_equal(out$values, c(4, 3, NA_real_))
  expect_identical(out$censored$above, c(FALSE, FALSE, TRUE))
  # A draw below the foot of the range is the smaller whatever the other block
  # says.
  attr(g, "censored") <- cens_record(c(FALSE, FALSE, FALSE),
                                     c(TRUE, FALSE, FALSE))
  out2 <- bayesnec:::combine_censored_min(g, s, 3)
  expect_identical(out2$censored$below, c(TRUE, FALSE, FALSE))
  expect_true(is.na(out2$values[1]))
})

test_that("combine_censored_min is pmin where neither block is censored", {
  g <- c(1, 5, 3)
  s <- c(4, 2, 6)
  out <- bayesnec:::combine_censored_min(g, s, 3)
  expect_identical(out$values, pmin(g, s))
  expect_null(out$censored)
})

test_that("the censored summary recovers the quantile where it is identified", {
  # The strongest statement the rank treatment makes: where a quantile does not
  # fall among the censored draws, it is the quantile of the uncensored truth
  # and not of the draws that were identified. Checked against a posterior
  # whose beyond-range values are known, which no fit can supply.
  set.seed(1)
  truth <- sort(stats::runif(200, 0, 5))
  above <- truth > 4
  seen <- truth
  seen[above] <- NA_real_
  out <- bayesnec:::summarise_censored(
    seen, c(0.5, 0.025, 0.975), cens_record(above, logical(200), upper = 4,
                                            lower = 0)
  )
  want <- stats::quantile(truth, c(0.5, 0.025, 0.975), type = 1)
  expect_gt(sum(above), 20)
  expect_equal(unname(out[1:2]), unname(want[1:2]))
  # The upper limit does fall among them and is the bound, which is the one
  # entry the data cannot identify.
  expect_identical(attr(out, "censored_summary")$bound, c("", "", ">="))
  expect_equal(unname(out[[3]]), 4)
  # The deleted-draw summary gets the median wrong by deleting the top 17 per
  # cent of the sample.
  deleted <- stats::quantile(seen, c(0.5, 0.025, 0.975), na.rm = TRUE)
  expect_lt(deleted[[1]], want[[1]])
})

test_that("a wholly censored posterior reports the bound at every entry", {
  x <- rep(NA_real_, 5)
  up <- bayesnec:::summarise_censored(
    x, c(0.5, 0.025, 0.975), cens_record(rep(TRUE, 5), logical(5))
  )
  expect_true(all(up == 10))
  expect_identical(attr(up, "censored_summary")$bound, rep(">=", 3))
  down <- bayesnec:::summarise_censored(
    x, c(0.5, 0.025, 0.975), cens_record(logical(5), rep(TRUE, 5))
  )
  expect_true(all(down == 0))
  expect_identical(attr(down, "censored_summary")$bound, rep("<=", 3))
})

test_that("a one-draw posterior is summarised rather than refused", {
  out <- bayesnec:::summarise_censored(NA_real_, c(0.5, 0.025, 0.975),
                                       cens_record(TRUE, FALSE))
  expect_equal(as.numeric(out), rep(10, 3))
  plain <- bayesnec:::summarise_censored(3, c(0.5, 0.025, 0.975),
                                         cens_record(FALSE, FALSE))
  expect_equal(as.numeric(plain), rep(3, 3))
})

test_that("a record of the wrong length is refused rather than recycled", {
  # Recycling a shorter logical would censor a draw chosen by position in the
  # recycling rather than by where its estimate lies, and would do it silently.
  expect_error(
    bayesnec:::summarise_censored(c(1, 2), 0.5,
                                  cens_record(c(TRUE, FALSE, FALSE),
                                              logical(3))),
    "covers 3 draws and the posterior has 2"
  )
})

test_that("a quantile that only straddles the boundary is not called a bound", {
  # Ten draws, one censored at 100. The 97.5 per cent quantile sits between the
  # ninth draw and the tenth, so under an interpolating quantile it came back
  # infinite and was reported as >= 100, although the smallest value consistent
  # with the sample is 9. The censored summary reports an order statistic, so
  # the entry is either a draw that was identified or one known to be beyond an
  # end, and never a value produced by interpolating across the boundary.
  x <- c(1:9, NA_real_)
  out <- bayesnec:::summarise_censored(
    x, c(0.5, 0.025, 0.975),
    cens_record(c(rep(FALSE, 9), TRUE), logical(10), upper = 100, lower = 0)
  )
  cens <- attr(out, "censored_summary")
  expect_identical(cens$bound, c("", "", ">="))
  expect_equal(unname(out[[1]]), 5)
  # The reported upper limit is the censored draw's own rank, so the bound is
  # true: that draw is at or beyond 100.
  expect_equal(unname(out[[3]]), 100)
  # Every finite entry is a draw that was identified, not an interpolation.
  finite <- out[!nzchar(cens$bound)]
  expect_true(all(finite %in% 1:9))
})

test_that("a decreasing predictor transformation names the right end", {
  # crf(-x) maps the top of the recorded range to the foot of the fitted one, so
  # a draw the search left beyond the top of the recorded grid lies below the
  # foot of the fitted grid and must be reported there. Building the record on
  # the fitted bounds directly would label it as above the fitted top, which is
  # the one value it is known not to exceed.
  recorded <- censoring_record(above = c(TRUE, FALSE), below = c(FALSE, FALSE),
                               upper = 5, lower = 0.1)
  fitted <- bayesnec:::xform_censoring(recorded, function(v) -v)
  expect_identical(fitted$above, c(FALSE, FALSE))
  expect_identical(fitted$below, c(TRUE, FALSE))
  expect_equal(c(fitted$lower, fitted$upper), c(-5, -0.1))
  out <- bayesnec:::summarise_censored(c(NA_real_, -2), c(0.5, 0.025, 0.975),
                                       fitted)
  expect_true(any(attr(out, "censored_summary")$bound == "<="))
  expect_false(any(attr(out, "censored_summary")$bound == ">="))
})

test_that("the end a class is named at survives one and two remappings", {
  # The two classes are named by the geometry the search saw: draws the curve
  # did not reach within the range, and draws it had already passed where the
  # range began. A decreasing remapping puts those at opposite ends of the
  # reported scale, so a message naming cens$upper for the first of them would
  # name the one value those draws are known not to exceed. The record carries
  # which way round it is.
  rec <- cens_record(above = c(TRUE, FALSE, FALSE),
                     below = c(FALSE, TRUE, FALSE), upper = 10, lower = 1)
  expect_equal(bayesnec:::censored_end(rec, "above"), 10)
  expect_equal(bayesnec:::censored_end(rec, "below"), 1)

  flipped <- bayesnec:::xform_censoring(rec, function(v) -v)
  expect_true(isTRUE(attr(flipped, "swapped")))
  # Both ends are censored here, which is the case a test on cens$above alone
  # gets wrong: it would name -1 for draws that lie at or below -10.
  expect_equal(bayesnec:::censored_end(flipped, "above"), -10)
  expect_equal(bayesnec:::censored_end(flipped, "below"), -1)

  # Two decreasing remappings leave the ends where they started, which is the
  # composition a record makes when both the crf() and the caller's xform
  # reverse the predictor.
  twice <- bayesnec:::xform_censoring(flipped, function(v) -v)
  expect_false(isTRUE(attr(twice, "swapped")))
  expect_equal(bayesnec:::censored_end(twice, "above"), 10)
  expect_equal(bayesnec:::censored_end(twice, "below"), 1)

  # An increasing remapping after a decreasing one keeps the swap.
  mixed <- bayesnec:::xform_censoring(flipped, function(v) v * 2)
  expect_true(isTRUE(attr(mixed, "swapped")))
  expect_equal(bayesnec:::censored_end(mixed, "above"), -20)
})

test_that("an unexplained NA is left out of the fraction that is stated", {
  # Such a draw could not be computed at all, so na.rm drops it from the
  # quantile. Counting it in n_draws would state a fraction of a sample the
  # reported figure was not taken from.
  x <- c(1:8, NA_real_, NA_real_)
  cens <- cens_record(c(rep(FALSE, 8), FALSE, TRUE), logical(10))
  out <- bayesnec:::summarise_censored(x, c(0.5, 0.025, 0.975), cens)
  cs <- attr(out, "censored_summary")
  expect_identical(cs$n_above, 1L)
  expect_identical(cs$n_draws, 9L)
  expect_true(cs$n_above + cs$n_below <= cs$n_draws)
})

test_that("a stacked table marks the rows whose entries are bounds", {
  # print.hurdlesummary() builds one matrix from several estimates. rbind()
  # keeps the numbers and drops every attribute, so a censored row printed as a
  # bare number while nec() on the same object returned it marked.
  marked <- bayesnec:::summarise_censored(
    c(1:4, rep(NA_real_, 6)), c(0.5, 0.025, 0.975),
    cens_record(c(rep(FALSE, 4), rep(TRUE, 6)), logical(10))
  )
  plain <- bayesnec:::summarise_censored(as.numeric(1:10),
                                         c(0.5, 0.025, 0.975), NULL)
  mat <- rbind(marked, plain)
  rownames(mat) <- c("combined", "growth")
  expect_null(attr(mat, "censored_summary"))
  attr(mat, "censored_summary") <- bayesnec:::row_censoring(
    list(marked, plain)
  )
  printed <- utils::capture.output(bayesnec:::print_mat(mat))
  combined <- printed[grepl("combined", printed)]
  growth <- printed[grepl("growth", printed)]
  expect_true(grepl(">=", combined, fixed = TRUE))
  expect_false(grepl(">=", growth, fixed = TRUE))
  note <- utils::capture.output(bayesnec:::print_row_censoring_notes(
    attr(mat, "censored_summary"), rownames(mat)
  ))
  expect_true(any(grepl("combined", note, fixed = TRUE)))
  expect_false(any(grepl("growth", note, fixed = TRUE)))
})

test_that("the warning and the note name the same end for the same draws", {
  # A decreasing remapping puts the two classes of beyond-range draw at
  # opposite ends of the reported scale. Keying the warning on the geometry the
  # search saw and the note on the record's fields put the two in
  # contradiction: "censored above -10" printed over "lie below -10" about the
  # same draws. Both now read the same two fields.
  rec <- cens_record(above = c(TRUE, TRUE, FALSE), below = rep(FALSE, 3),
                     upper = 10, lower = 1)
  flipped <- bayesnec:::xform_censoring(rec, function(z) -z)
  msg <- tryCatch(
    bayesnec:::warn_censored_draws(c(NA_real_, NA_real_, -5), "NSEC",
                                   cens = flipped),
    bayesnec_censored = function(w) conditionMessage(w)
  )
  expect_true(grepl("not identified for 2 of 3 draws", msg, fixed = TRUE))
  expect_true(grepl("at or below -10", msg, fixed = TRUE))
  expect_false(grepl("above", msg, fixed = TRUE))
  # The note about the same draws, and the mark on the summary, agree with it.
  out <- bayesnec:::summarise_censored(c(NA_real_, NA_real_, -5),
                                       c(0.5, 0.025, 0.975), flipped)
  note <- utils::capture.output(
    bayesnec:::print_censoring_note(attr(out, "censored_summary"), "NSEC")
  )
  expect_true(any(grepl("lie below -10", note, fixed = TRUE)))
  expect_true(any(attr(out, "censored_summary")$bound == "<="))
})

test_that("a draw the record explains at neither end is reported apart", {
  # It could not be computed at all rather than being known to lie beyond an
  # end, so it is named on its own and left out of the denominator the other
  # reports use, which is the denominator the note states.
  msgs <- character(0)
  withCallingHandlers(
    bayesnec:::warn_censored_draws(
      c(NA_real_, 2, NA_real_), "NSEC",
      cens = cens_record(c(TRUE, FALSE, FALSE), rep(FALSE, 3))
    ),
    bayesnec_censored = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(msgs, 2)
  expect_true(grepl("not identified for 1 of 2 draws", msgs[1], fixed = TRUE))
  expect_true(grepl("could not be computed for 1 of 3 draws", msgs[2],
                    fixed = TRUE))
})

# Integration. These reuse the packaged fits and change only the prediction
# grid, so no model is compiled or sampled.

# The foot of the grid is the lowest observed predictor value. expand_nec()
# reads its control at the first grid point and nsec() reads it at the lowest
# observed one (D15 ruling 2), so the two paths agree on a number only where
# those coincide, which is the default grid and is what this uses.
censored_x_range <- function() {
  c(min(manec_example$mod_fits[["ecx4param"]]$fit$data$x), 0.9)
}

censored_ecx_fit <- function(x_range = censored_x_range(), resolution = 50) {
  f <- manec_example$mod_fits[["ecx4param"]]
  suppressMessages(suppressWarnings(bayesnec:::expand_and_assign_nec(
    f, f$bayesnecformula, model = "ecx4param",
    x_range = x_range, resolution = resolution
  )))
}

test_that("a fit with no beyond-range draw reports what the release reports", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # manec_example's grid spans its own data, every curve reaches the reference
  # within it, and its stored summary predates this change. Nothing about it
  # may move.
  expect_null(attr(manec_example$w_ne, "censored_summary"))
  expect_equal(unname(manec_example$w_ne),
               c(1.4503481, 0.7487292, 1.5273698), tolerance = 1e-6)
  out <- suppressMessages(suppressWarnings(nec(manec_example)))
  expect_equal(as.numeric(out), as.numeric(manec_example$w_ne),
               tolerance = 1e-12)
  expect_null(attr(out, "censored_summary"))
  summ <- suppressMessages(suppressWarnings(summary(manec_example,
                                                    check_fit = FALSE)))
  expect_null(attr(summ$nec_vals, "censored_summary"))
})

test_that("a censored ecx-type fit reports the bound and the fraction", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  fit <- censored_ecx_fit()
  cens <- attr(fit$ne, "censored_summary")
  expect_false(is.null(cens))
  expect_gt(cens$n_above, 0)
  expect_equal(cens$upper, 0.9)
  # The bound is reported where the quantile falls among the censored draws,
  # and the estimate is the bound rather than a quantile of what is left.
  expect_equal(unname(fit$ne[["Estimate"]]), 0.9)
  printed <- utils::capture.output(print(
    suppressMessages(suppressWarnings(summary(fit)))
  ))
  expect_true(any(grepl(">= 0.90", printed, fixed = TRUE)))
  expect_true(any(grepl(paste0(cens$n_above, " of ", cens$n_draws, " draws"),
                        printed, fixed = TRUE)))
})

test_that("nsec agrees with what summary prints for the same object", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  fit <- censored_ecx_fit()
  # nsec() recomputes on the grid it is given, so the same grid must give the
  # same answer as the one stored at fit time.
  out <- suppressWarnings(nsec(fit, x_range = censored_x_range(),
                               resolution = 50))
  expect_equal(as.numeric(out), as.numeric(fit$ne), tolerance = 1e-8)
  expect_identical(attr(out, "censored_summary")$bound,
                   attr(fit$ne, "censored_summary")$bound)
  expect_identical(attr(out, "censored_summary")$n_above,
                   attr(fit$ne, "censored_summary")$n_above)
})

test_that("ecx takes the censored summary as well", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  out <- suppressWarnings(ecx(ecx4param, ecx_val = 50,
                              x_range = c(0.03, 0.6), resolution = 50))
  cens <- attr(out, "censored_summary")
  expect_false(is.null(cens))
  expect_equal(cens$upper, 0.6)
  expect_true(all(out[nzchar(cens$bound)] == 0.6))
  expect_warning(ecx(ecx4param, ecx_val = 50, x_range = c(0.03, 0.6),
                     resolution = 50),
                 "censored at 0.6")
})

test_that("a draw beyond the lower end is recorded as below", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # A grid starting above the lowest observed concentration leaves the stretch
  # between the control and the grid unsearched, so a draw crossing there is
  # known only to lie below the foot of the grid.
  out <- suppressWarnings(nsec(ecx4param, x_range = c(2.5, 3.2),
                               resolution = 50))
  cens <- attr(out, "censored_summary")
  expect_gt(cens$n_below, 0)
  expect_true(any(cens$bound == "<="))
  expect_equal(cens$lower, 2.5)
})

test_that("the model-averaged fraction is the weighted one", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  fs <- manec_example$mod_fits
  forms <- lapply(fs, function(z) z$bayesnecformula)
  parts <- lapply(names(fs), function(nm) {
    suppressMessages(suppressWarnings(expand_nec(
      fs[[nm]], forms[[nm]], model = nm,
      x_range = censored_x_range(), resolution = 50)))
  })
  names(parts) <- names(fs)
  m <- suppressMessages(suppressWarnings(expand_manec(
    fs, formula = forms, x_range = censored_x_range(), resolution = 50)))
  idx <- m$w_draw_index
  expected <- sum(vapply(names(fs), function(nm) {
    sum(attr(parts[[nm]]$ne_posterior, "censored")$above[idx[[nm]]])
  }, numeric(1)))
  cens <- attr(m$w_ne, "censored_summary")
  expect_equal(cens$n_above, expected)
  expect_identical(cens$n_draws, length(m$w_ne_posterior))
  # A count over equations rather than over draws would be 2 here, and both
  # equations are censored, so the two answers are distinguishable.
  expect_gt(cens$n_above, length(fs))
})

test_that("pull_out does not change a censored estimate", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  fs <- manec_example$mod_fits
  forms <- lapply(fs, function(z) z$bayesnecformula)
  m <- suppressMessages(suppressWarnings(expand_manec(
    fs, formula = forms, x_range = censored_x_range(), resolution = 50)))
  m <- bayesnec:::allot_class(m, c("bayesmanecfit", "bnecfit"))
  po <- suppressMessages(suppressWarnings(
    pull_out(m, "ecx4param", x_range = censored_x_range(),
             resolution = 50)))
  direct <- censored_ecx_fit()
  expect_equal(as.numeric(po$ne), as.numeric(direct$ne), tolerance = 1e-12)
  expect_identical(attr(po$ne, "censored_summary")$n_above,
                   attr(direct$ne, "censored_summary")$n_above)
  expect_identical(attr(po$ne, "censored_summary")$bound,
                   attr(direct$ne, "censored_summary")$bound)
})

test_that("the plot annotation marks a censored estimate", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  fit <- censored_ecx_fit()
  d <- suppressMessages(suppressWarnings(ggbnec_data(fit)))
  labs <- d$nec_labs[!is.na(d$nec_labs)]
  expect_true(any(grepl("^>=", labs)))
  plain <- suppressMessages(suppressWarnings(ggbnec_data(ecx4param)))
  expect_false(any(grepl("^>=|^<=", plain$nec_labs[!is.na(plain$nec_labs)])))
})

# ---- #417, the annotation under a decreasing map -----------------------------

# The packaged nec4param fit and model set, re-expanded over grids that end
# inside their posteriors, so nothing is compiled or sampled. Each accessor
# builds once per file.
#
# Ending at 1.5 puts 14 of nec4param's 100 NEC draws above the grid, and 12 of
# the model average's, so only the upper limit is a bound: the case where the
# two interval entries can be told apart after a reversal. The ECx call's grid
# ends at 1.7, where 32 of nec4param's relative EC10 draws lie above it. Under
# an explicit xform the fit's own grid decides only the direction of the map,
# so the model set is built once, on the NEC grid. The automatic inverse clamps
# anything beyond the fit's grid to its end, so the single fit that test uses
# is built on the ECx grid as well. Expanding the model set takes about ten
# seconds, and its summary(), which plot() calls, about as long again.
reversal_x_foot <- function() {
  min(manec_example$mod_fits[["nec4param"]]$fit$data$x)
}
reversal_top <- c(nec = 1.5, ecx = 1.7)
reversal_ecx_args <- function() {
  list(ecx_val = 10, type = "relative",
       x_range = c(reversal_x_foot(), reversal_top[["ecx"]]), resolution = 50)
}

reversal_fit <- local({
  cached <- list()
  function(which = c("nec", "ecx")) {
    which <- match.arg(which)
    if (is.null(cached[[which]])) {
      f <- manec_example$mod_fits[["nec4param"]]
      cached[[which]] <<- suppressMessages(suppressWarnings(
        bayesnec:::expand_and_assign_nec(
          f, f$bayesnecformula, model = "nec4param",
          x_range = c(reversal_x_foot(), reversal_top[[which]]),
          resolution = 50
        )
      ))
    }
    cached[[which]]
  }
})

reversal_manec <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      fs <- manec_example$mod_fits
      forms <- lapply(fs, function(z) z$bayesnecformula)
      m <- suppressMessages(suppressWarnings(expand_manec(
        fs, formula = forms,
        x_range = c(reversal_x_foot(), reversal_top[["nec"]]),
        resolution = 50
      )))
      cached <<- bayesnec:::allot_class(m, c("bayesmanecfit", "bnecfit"))
    }
    cached
  }
})

negate <- function(z) -z

# A stored fit relabelled as fitted on crf(I(-x)), its stored estimate put on
# that scale draw by draw. I(-x) rather than -x, because inside a formula a bare
# minus is the operator that removes a term, so model.frame() keeps x as it is
# and the predictor is not reported as transformed. ecx() reads the formula
# when it maps its search onto the fitted scale, so its estimates come out
# negated with no further change.
negated_fit <- function(fit) {
  fit$ne <- suppressMessages(suppressWarnings(nec(fit, xform = negate)))
  fit$bayesnecformula <- bayesnecformula(y ~ crf(I(-x), model = "nec4param"))
  fit
}

negated_manec <- function(m) {
  m$w_ne <- suppressMessages(suppressWarnings(nec(m, xform = negate)))
  m$mod_fits[[1]]$bayesnecformula <- bayesnecformula(
    stats::as.formula(paste0("y ~ crf(I(-x), model = \"",
                             names(m$mod_fits)[1], "\")"))
  )
  m
}

# The three labels of one annotation, estimate first.
annotation_labels <- function(d, what = "nec") {
  cols <- paste0(what, c("_labs", "_labs_l", "_labs_u"))
  vapply(cols, function(n) unique(stats::na.omit(d[[n]])), character(1),
         USE.NAMES = FALSE)
}

# What x -> -x must make of labels on the recorded scale: each number negated,
# each mark reversed, and the two interval entries exchanged. The annotation
# remaps the stored summary rather than summarising the draws again, so this is
# exact, where comparing with nec(fit, xform = negate) is not: with 100 draws a
# censored median is a type 1 quantile, and the median of -x is then the
# negated 51st draw rather than the 50th.
reflected_labels <- function(labs) {
  reflect <- function(l) {
    if (startsWith(l, ">=")) {
      return(paste0("<=-", substring(l, 3)))
    }
    if (startsWith(l, "<=")) {
      return(paste0(">=-", substring(l, 3)))
    }
    paste0("-", l)
  }
  vapply(labs[c(1, 3, 2)], reflect, character(1), USE.NAMES = FALSE)
}

annotate <- function(obj, ...) {
  suppressMessages(suppressWarnings(ggbnec_data(obj, ...)))
}

annotate_ecx <- function(obj, ...) {
  do.call(annotate, c(list(obj, add_nec = FALSE, add_ecx = TRUE, ...),
                      reversal_ecx_args()))
}

test_that("a bound above the grid is labelled as below it once negated", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # The reproduction in #417: every NEC draw lies above a grid ending at 0.9,
  # so all three entries are the bound, and the negated axis puts it at the
  # foot. The release labelled all three ">=-0.90".
  f <- manec_example$mod_fits[["nec4param"]]
  fit <- suppressMessages(suppressWarnings(
    bayesnec:::expand_and_assign_nec(
      f, f$bayesnecformula, model = "nec4param",
      x_range = c(reversal_x_foot(), 0.9), resolution = 50
    )
  ))
  expect_identical(annotation_labels(annotate(fit, xform = negate)),
                   rep("<=-0.90", 3))
})

test_that("a decreasing xform reverses the NEC annotation", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  for (obj in list(reversal_fit("nec"), reversal_manec())) {
    plain <- annotation_labels(annotate(obj))
    expect_match(plain[3], "^>=")
    negated <- annotate(obj, xform = negate)
    expect_identical(annotation_labels(negated), reflected_labels(plain))
    # The lower limit is the bound now, and it is the smaller number.
    expect_match(annotation_labels(negated)[2], "^<=")
    vals <- negated$nec_vals[!is.na(negated$nec_vals)]
    expect_lt(vals[2], vals[3])
  }
})

test_that("a decreasing xform reverses the ECx annotation", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  for (obj in list(reversal_fit("nec"), reversal_manec())) {
    plain <- annotation_labels(annotate_ecx(obj), "ecx")
    expect_match(plain[3], "^>=")
    negated <- annotate_ecx(obj, xform = negate)
    expect_identical(annotation_labels(negated, "ecx"),
                     reflected_labels(plain))
    vals <- negated$ecx_vals[!is.na(negated$ecx_vals)]
    expect_lt(vals[2], vals[3])
  }
})

test_that("the inverse of a decreasing crf() term restores the annotation", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # No xform: the estimate is on the negated scale and is inverted on the grid
  # for the axis, which is drawn on the recorded one. The interval entries and
  # their marks must come back as the untransformed fit has them. The estimate
  # is compared for its mark only, for the type 1 reason given above.
  pairs <- list(
    list(plain = annotation_labels(annotate(reversal_fit("nec"))),
         inverted = annotation_labels(
           annotate(negated_fit(reversal_fit("nec"))))),
    list(plain = annotation_labels(annotate(reversal_manec())),
         inverted = annotation_labels(
           annotate(negated_manec(reversal_manec())))),
    list(plain = annotation_labels(annotate_ecx(reversal_fit("ecx")), "ecx"),
         inverted = annotation_labels(
           annotate_ecx(negated_fit(reversal_fit("ecx"))), "ecx"))
  )
  for (p in pairs) {
    expect_identical(p$inverted[2:3], p$plain[2:3])
    expect_match(p$inverted[3], "^>=")
    expect_no_match(p$inverted[1], "^[<>]=")
  }
})

test_that("identity and increasing maps leave the annotation as it was", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  fit <- reversal_fit("nec")
  plain <- annotation_labels(annotate(fit))
  expect_identical(plain[c(1, 2)],
                   unname(bayesnec:::rounded(fit$ne[c(1, 2)], 2)))
  expect_identical(plain[3], paste0(">=", bayesnec:::rounded(fit$ne[[3]], 2)))
  doubled <- annotation_labels(annotate(fit, xform = function(z) z * 2))
  expect_identical(doubled[3],
                   paste0(">=", bayesnec:::rounded(fit$ne[[3]] * 2, 2)))
  expect_no_match(doubled[1:2], "^[<>]=")
})

test_that("the base plot legend takes the same reversal", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # plot() returns nothing, so the legend text is read off a mocked legend().
  legend_text <- function(obj, ...) {
    got <- NULL
    local_mocked_bindings(
      legend = function(x, y = NULL, legend, ...) got <<- legend,
      .package = "bayesnec"
    )
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    suppressMessages(suppressWarnings(plot(obj, ...)))
    got
  }
  fit <- reversal_fit("nec")
  plain <- legend_text(fit)
  expect_match(plain, "->= 1.5)", fixed = TRUE)
  # lxform relabels the axis, and a decreasing one is remapped by the same
  # rule: negating the axis and then its labels gives the plain legend back.
  # Applying lxform to the numbers alone left "<=" beside 1.5. Before #417
  # this pair was right by accident, the marks reversed by neither map, which
  # is why the negated axis alone is asserted too.
  expect_identical(legend_text(fit, xform = negate, lxform = negate), plain)
  # The model-set method has its own copy of the legend code. Two calls, not
  # three, because each one spends about ten seconds in summary().
  for (obj in list(fit, reversal_manec())) {
    negated <- legend_text(obj, xform = negate)
    expect_match(negated, "(<= -1.5--", fixed = TRUE)
    expect_no_match(negated, ">=", fixed = TRUE)
  }
  relabelled <- legend_text(reversal_manec(), xform = negate, lxform = negate)
  expect_match(relabelled, "->= 1.5)", fixed = TRUE)
  expect_no_match(relabelled, "<=", fixed = TRUE)
  inverted <- legend_text(negated_fit(reversal_fit("nec")))
  expect_match(inverted, ">= 1.5)", fixed = TRUE)
  expect_no_match(inverted, "<=", fixed = TRUE)
})

test_that("a hurdle fit with one block censored reports the other block", {
  # Specification 4.11's hurdle case, on a mock rather than a fit: no packaged
  # two-block fit exists and compiling one for this costs minutes. The growth
  # block is beyond the top of the range for the first two draws and the
  # survival block is inside it for all four, so the combined no-effect
  # estimate is the survival block wherever growth is censored. pmin() on the
  # raw vectors returned NA for those draws and deleted them.
  cens <- function(above) {
    bayesnec:::censoring_record(10, 0, above, logical(length(above)))
  }
  mk <- function(post, above) {
    out <- structure(list(model = "nec3param", ne_type = "NEC",
                          ne_posterior = post),
                     class = c("bayesnecfit", "bnecfit"))
    attr(out$ne_posterior, "censored") <- cens(above)
    out
  }
  obj <- structure(
    list(growth = mk(c(NA_real_, NA_real_, 3, 9), c(TRUE, TRUE, FALSE, FALSE)),
         survival = mk(c(4, 5, 6, 2), rep(FALSE, 4)),
         data = data.frame(x = 1:4, y = c(2, 1, 0, 0)),
         formula = bnf(y ~ crf(x, "nec3param")), y_var = "y",
         n_exposed = 4L, n_dead = 2L),
    class = c("bayesnechurdlefit", "bnecfit")
  )
  post <- suppressMessages(suppressWarnings(
    nec(obj, posterior = TRUE)
  ))
  # The first two draws take the survival value; the last two take the smaller
  # of the two blocks as before.
  expect_equal(as.numeric(post), c(4, 5, 3, 2))
  expect_false(anyNA(post))
  # A record is still attached, because a component carried one, but it marks
  # no draw: the combination is inside the range at every draw.
  expect_false(bayesnec:::has_censoring(attr(post, "censored")))
  est <- suppressMessages(suppressWarnings(nec(obj)))
  expect_equal(as.numeric(est),
               as.numeric(stats::quantile(c(4, 5, 3, 2),
                                          c(0.5, 0.025, 0.975))))
  expect_null(attr(est, "censored_summary"))
})

test_that("a hurdle fit censored in both blocks stays censored", {
  cens <- function(above) {
    bayesnec:::censoring_record(10, 0, above, logical(length(above)))
  }
  mk <- function(post, above) {
    out <- structure(list(model = "nec3param", ne_type = "NEC",
                          ne_posterior = post),
                     class = c("bayesnecfit", "bnecfit"))
    attr(out$ne_posterior, "censored") <- cens(above)
    out
  }
  obj <- structure(
    list(growth = mk(c(NA_real_, 3), c(TRUE, FALSE)),
         survival = mk(c(NA_real_, 6), c(TRUE, FALSE)),
         data = data.frame(x = 1:4, y = c(2, 1, 0, 0)),
         formula = bnf(y ~ crf(x, "nec3param")), y_var = "y",
         n_exposed = 4L, n_dead = 2L),
    class = c("bayesnechurdlefit", "bnecfit")
  )
  est <- suppressMessages(suppressWarnings(nec(obj)))
  cs <- attr(est, "censored_summary")
  # The minimum of two draws is above the top of the range only where both
  # blocks are, which is the first draw here.
  expect_identical(cs$n_above, 1L)
  expect_equal(cs$upper, 10)
  expect_true(any(nzchar(cs$bound)))
})
