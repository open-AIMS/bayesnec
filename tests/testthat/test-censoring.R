# Test the shared censoring machinery; estimator-specific tests live in
# their own files.

cens_record <- function(above, below, upper = 10, lower = 0) {
  bayesnec:::censoring_record(upper, lower, above, below)
}

test_that("a posterior with no beyond-range draw is summarised as before", {
  # An absent or empty censoring record must preserve the uncensored summary
  # to the last bit.
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
  # Keep censored draws in the ranks: deleting ninety-one upper-tail draws
  # would turn a bounded median into the median of the nine retained draws.
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
  # Do not impute the bound as a draw value; the identified draws keep
  # their own median.
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

# A decreasing map with an infinite endpoint.

test_that("a decreasing xform swaps a record whose end maps to infinity", {
  # A decreasing map must reverse censoring even when an endpoint maps to Inf.
  cens <- cens_record(above = c(TRUE, FALSE, FALSE),
                      below = c(FALSE, FALSE, TRUE), upper = 100, lower = 0)
  flipped <- bayesnec:::xform_censoring(cens, function(z) -log(z))
  expect_identical(flipped$above, cens$below)
  expect_identical(flipped$below, cens$above)
  expect_equal(flipped$lower, -log(100))
  expect_identical(flipped$upper, Inf)
  expect_true(attr(flipped, "swapped"))
  # The same where the infinite end is on the record already: a fitted-scale
  # record under crf(log(x)) on a grid from 0 runs from -Inf, and a caller's
  # negation then takes that end to Inf.
  logged <- cens_record(above = c(TRUE, FALSE), below = c(FALSE, TRUE),
                        upper = log(100), lower = -Inf)
  negated <- bayesnec:::xform_censoring(logged, function(z) -z)
  expect_identical(negated$above, logged$below)
  expect_identical(negated$below, logged$above)
  expect_equal(c(negated$lower, negated$upper), c(-log(100), Inf))
  expect_true(attr(negated, "swapped"))
  # An increasing map is not swapped by an infinite image either.
  rising <- bayesnec:::xform_censoring(cens, log)
  expect_identical(rising$above, cens$above)
  expect_equal(c(rising$lower, rising$upper), c(-Inf, log(100)))
  expect_false(attr(rising, "swapped"))
})

test_that("the per-draw and summary paths agree under -log on a grid from 0", {
  # Use an infinite endpoint and upper-censored draws to compare per-draw
  # transformation with remapping a summary.
  grid <- seq(0, 100, length.out = 101)
  draws <- c(seq(10, 90, length.out = 90), rep(NA_real_, 11))
  cens <- bayesnec:::censoring_record(100, 0, is.na(draws), logical(101))
  probs <- c(0.5, 0.025, 0.975)
  neg_log <- function(z) -log(z)
  per_draw <- bayesnec:::summarise_censored(
    neg_log(draws), probs, bayesnec:::xform_censoring(cens, neg_log)
  )
  on_recorded <- bayesnec:::summarise_censored(draws, probs, cens)
  names(on_recorded) <- bayesnec:::clean_names(on_recorded)
  summary_path <- bayesnec:::remap_summary(on_recorded, neg_log, grid)
  expect_lt(per_draw[[2]], per_draw[[3]])
  expect_equal(as.numeric(per_draw), as.numeric(summary_path))
  expect_identical(attr(per_draw, "censored_summary")$bound, c("", "<=", ""))
  expect_identical(attr(per_draw, "censored_summary")$bound,
                   attr(summary_path, "censored_summary")$bound)
  expect_equal(per_draw[[2]], -log(100))
})

test_that("every increasing xform leaves a record as it was before #440", {
  # Keep the earlier finite-endpoint rule as the reference for increasing
  # maps; those maps must retain their existing behaviour.
  before_440 <- function(cens, xform) {
    new_upper <- xform(cens$upper)
    new_lower <- xform(cens$lower)
    swapped <- isTRUE(attr(cens, "swapped"))
    if (is.finite(new_upper) && is.finite(new_lower) &&
        new_upper < new_lower) {
      out <- bayesnec:::censoring_record(new_lower, new_upper, cens$below,
                                         cens$above)
      swapped <- !swapped
    } else {
      out <- bayesnec:::censoring_record(new_upper, new_lower, cens$above,
                                         cens$below)
    }
    attr(out, "swapped") <- swapped
    out
  }
  above <- c(TRUE, FALSE, FALSE, NA)
  below <- c(FALSE, FALSE, TRUE, FALSE)
  records <- list(
    finite = cens_record(above, below, upper = 3.22, lower = 0.0323),
    from_zero = cens_record(above, below, upper = 100, lower = 0),
    from_minus_inf = cens_record(above, below, upper = log(3.22),
                                 lower = -Inf),
    to_inf = cens_record(above, below, upper = Inf, lower = 1),
    one_point = cens_record(above, below, upper = 2, lower = 2),
    negative = cens_record(above, below, upper = 5, lower = -5)
  )
  swapped_before <- records$finite
  attr(swapped_before, "swapped") <- TRUE
  records$swapped_before <- swapped_before
  maps <- list(exp = exp, log = log, log1p = log1p, sqrt = sqrt,
               linear = function(z) 2 * z + 1, cube = function(z) z^3,
               plogis = stats::plogis, identity = identity)
  for (r in names(records)) {
    for (m in names(maps)) {
      # log and sqrt of a negative end warn on both paths alike.
      expect_identical(
        suppressWarnings(bayesnec:::xform_censoring(records[[r]], maps[[m]])),
        suppressWarnings(before_440(records[[r]], maps[[m]])),
        label = paste(r, "under", m)
      )
    }
  }
})

test_that("the estimators under xform = exp return what they did before #440", {
  skip_on_cran()
  # Relabel the stored nec4param fit as logarithmic without refitting it.
  # The fixed exp() results guard the increasing inverse for identified and
  # censored draws alike.
  logged <- nec4param
  logged$bayesnecformula <- bnf(y ~ crf(log(x), model = "nec4param"))
  q <- function(expr) suppressWarnings(suppressMessages(expr))
  same <- function(got, value, bound = NULL, ends = NULL, counts = NULL,
                   tolerance = 1e-8) {
    expect_equal(unname(as.numeric(got)), value, tolerance = tolerance)
    cs <- attr(got, "censored_summary")
    if (is.null(bound)) {
      expect_null(cs)
      return(invisible(NULL))
    }
    expect_identical(cs$bound, bound)
    expect_equal(c(cs$upper, cs$lower), ends, tolerance = 1e-8)
    expect_identical(c(cs$n_above, cs$n_below), counts)
  }
  same(q(ecx(logged, ecx_val = 50, xform = exp, resolution = 200)),
       c(1.6735243462, 1.6101967180, 1.7230802679))
  same(q(ecx(nec4param, ecx_val = 50, xform = exp, resolution = 200)),
       c(5.3309227862, 5.0038420117, 5.6017580711))
  same(q(ecx(logged, ecx_val = 90, xform = exp, x_range = c(0.0324, 1.5))),
       rep(1.5, 3), rep(">=", 3), c(1.5, 0.0324), c(100L, 0L))
  same(q(ecx(logged, ecx_val = 10, xform = exp, x_range = c(1.6, 3.22))),
       rep(1.6, 3), rep("<=", 3), c(3.22, 1.6), c(0L, 100L))
  same(q(nsec(logged, xform = exp, resolution = 200)),
       c(1.4847758014, 1.3936170593, 1.5506664636), tolerance = 1e-6)
  same(q(nsec(logged, xform = exp, x_range = c(1.6, 3.22))),
       rep(1.6, 3), rep("<=", 3), c(3.22, 1.6), c(0L, 99L))
  same(q(nsec(manec_example, xform = exp, x_range = c(0.0324, 1.2),
              resolution = 200)),
       c(3.3201169227, 2.0703283968, 3.3201169227), c(">=", "", ">="),
       c(3.3201169227, 1.0329305949), c(82L, 0L), tolerance = 1e-6)
  same(q(nec(logged, xform = exp)),
       c(4.3104239685, 3.8995850141, 4.6079795245))
  # The per-draw records keep their ends and are not swapped.
  records <- list(
    above = list(q(ecx(logged, ecx_val = 90, xform = exp,
                       x_range = c(0.0324, 1.5), posterior = TRUE)),
                 c(1.5, 0.0324), c(100L, 0L)),
    below = list(q(ecx(logged, ecx_val = 10, xform = exp,
                       x_range = c(1.6, 3.22), posterior = TRUE)),
                 c(3.22, 1.6), c(0L, 100L)),
    nsec = list(q(nsec(logged, xform = exp, posterior = TRUE)),
                c(3.2205196629, 0.0323480132), c(0L, 0L))
  )
  for (r in records) {
    rec <- attr(r[[1]], "censored")
    expect_equal(c(rec$upper, rec$lower), r[[2]], tolerance = 1e-8)
    expect_identical(c(sum(rec$above), sum(rec$below)), r[[3]])
    expect_false(attr(rec, "swapped"))
  }
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
  # Use simulated truth beyond the bounds to check that an identified
  # quantile uses the full draw ranks, not just the retained subset.
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
  # Use an order statistic so a censored quantile reports a known bound.
  # Interpolating across an unknown draw can produce an unjustified bound.
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
  # A decreasing predictor map exchanges the ends: an above-range recorded
  # draw becomes a below-range fitted draw.
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
  # Warnings must name the endpoint on the reported scale, accounting for
  # whether a decreasing map has reversed the search geometry.
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
  # The warning and summary note must use the same remapped endpoint; a
  # decreasing map must not make them report contradictory bounds.
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

# Start at the lowest observed predictor so the stored expansion and nsec()
# use the same control reference.
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
  # Keep the packaged uncensored summary unchanged for this existing fit.
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

# Annotations under a decreasing map.

# Reuse stored fits without sampling. Set grid ends so only one interval
# limit is censored and the two limits can be distinguished after reversal.
# Use the ECx grid for its automatic inverse, which clamps beyond that grid.
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

# Relabel a stored fit as crf(I(-x)) and negate its stored draws. A bare -x
# removes a formula term; I(-x) makes the model frame negate the predictor.
negated_fit <- function(fit) {
  fit$ne <- suppressMessages(suppressWarnings(nec(fit, xform = negate)))
  fit$ne_posterior <- suppressMessages(suppressWarnings(
    nec(fit, xform = negate, posterior = TRUE)
  ))
  fit$bayesnecformula <- bayesnecformula(y ~ crf(I(-x), model = "nec4param"))
  fit
}

negated_manec <- function(m) {
  m$w_ne <- suppressMessages(suppressWarnings(nec(m, xform = negate)))
  m$w_ne_posterior <- suppressMessages(suppressWarnings(
    nec(m, xform = negate, posterior = TRUE)
  ))
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

# Reflect stored summary labels, not newly computed quantiles. With an even
# number of draws, a type-1 median after negation need not equal the negated
# original median.
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

annotation_frame <- function(obj, ...) {
  suppressMessages(suppressWarnings(ggbnec_data(obj, ...)))
}

annotation_frame_ecx <- function(obj, ...) {
  do.call(annotation_frame,
          c(list(obj, add_nec = FALSE, add_ecx = TRUE, ...),
            reversal_ecx_args()))
}

test_that("a bound above the grid is labelled as below it once negated", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # Censor every NEC draw above 0.9 so a negated axis must mark all entries
  # at its lower end.
  f <- manec_example$mod_fits[["nec4param"]]
  fit <- suppressMessages(suppressWarnings(
    bayesnec:::expand_and_assign_nec(
      f, f$bayesnecformula, model = "nec4param",
      x_range = c(reversal_x_foot(), 0.9), resolution = 50
    )
  ))
  expect_identical(annotation_labels(annotation_frame(fit, xform = negate)),
                   rep("<=-0.90", 3))
})

test_that("a decreasing xform reverses the NEC annotation", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  for (obj in list(reversal_fit("nec"), reversal_manec())) {
    plain <- annotation_labels(annotation_frame(obj))
    expect_match(plain[3], "^>=")
    negated <- annotation_frame(obj, xform = negate)
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
    plain <- annotation_labels(annotation_frame_ecx(obj), "ecx")
    expect_match(plain[3], "^>=")
    negated <- annotation_frame_ecx(obj, xform = negate)
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
  # With no xform, invert the negated estimate on the recorded grid. Compare
  # the interval and marks with the original; compare the median's mark only.
  pairs <- list(
    list(plain = annotation_labels(annotation_frame(reversal_fit("nec"))),
         inverted = annotation_labels(
           annotation_frame(negated_fit(reversal_fit("nec"))))),
    list(plain = annotation_labels(annotation_frame(reversal_manec())),
         inverted = annotation_labels(
           annotation_frame(negated_manec(reversal_manec())))),
    list(plain = annotation_labels(
           annotation_frame_ecx(reversal_fit("ecx")), "ecx"),
         inverted = annotation_labels(
           annotation_frame_ecx(negated_fit(reversal_fit("ecx"))), "ecx"))
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
  plain <- annotation_labels(annotation_frame(fit))
  expect_identical(plain[c(1, 2)],
                   unname(bayesnec:::rounded(fit$ne[c(1, 2)], 2)))
  expect_identical(plain[3], paste0(">=", bayesnec:::rounded(fit$ne[[3]], 2)))
  doubled <- annotation_labels(annotation_frame(fit, xform = function(z) z * 2))
  expect_identical(doubled[3],
                   paste0(">=", bayesnec:::rounded(fit$ne[[3]] * 2, 2)))
  expect_no_match(doubled[1:2], "^[<>]=")
})

test_that("the annotation marks what nec() marks at a whole n times p", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # Put the grid end between draws 50 and 51. Reversal changes which median
  # rank is identified, so reversing existing marks alone is insufficient.
  f <- manec_example$mod_fits[["nec4param"]]
  ranked <- sort(nec4param$ne_posterior)
  fit <- suppressMessages(suppressWarnings(
    bayesnec:::expand_and_assign_nec(
      f, f$bayesnecformula, model = "nec4param",
      x_range = c(reversal_x_foot(), mean(ranked[50:51])), resolution = 50
    )
  ))
  expect_identical(attr(fit$ne, "censored_summary")$n_above, 50L)
  per_draw <- suppressMessages(suppressWarnings(nec(fit, xform = negate)))
  per_draw_marks <- attr(per_draw, "censored_summary")$bound
  expect_identical(per_draw_marks[1], "<=")
  labels <- annotation_labels(annotation_frame(fit, xform = negate))
  expect_identical(regmatches(labels, regexpr("^([<>]=)?", labels)),
                   per_draw_marks)
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
  # Negating the axis and its labels must restore the plain legend. Check
  # the negated axis alone too, so two omitted reversals cannot cancel.
  expect_identical(legend_text(fit, xform = negate, lxform = negate), plain)
  # Exercise both legend implementations while limiting repeated summaries.
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
  # Mock the two blocks: no packaged two-block fit supplies this case.
  # With a common upper bound, an identified survival draw below that bound
  # is smaller than a censored growth draw; raw pmin() would propagate NA.
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

# Test stored posterior combinations with unequal component ranges.
# An identified draw above the other component's censoring limit cannot
# resolve which component is smaller.
hurdle_necfit <- function(post, cens) {
  out <- structure(list(model = "nec3param", ne_type = "NEC",
                        ne_posterior = post,
                        fit = list(family = list(family = "gaussian"))),
                   class = c("bayesnecfit", "bnecfit"))
  attr(out$ne_posterior, "censored") <- cens
  out
}
hurdle_of <- function(growth, survival) {
  structure(
    list(growth = growth, survival = survival,
         data = data.frame(x = 1:4, y = c(2, 1, 0, 0)),
         formula = bnf(y ~ crf(x, "nec3param")), y_var = "y",
         n_exposed = 4L, n_dead = 2L),
    class = c("bayesnechurdlefit", "bnecfit")
  )
}
# Growth is known only to exceed 10; survival is identified within 0 to 40.
issue_415_hurdle <- function(s_value) {
  hurdle_of(
    hurdle_necfit(rep(12, 4), cens_record(rep(TRUE, 4), logical(4))),
    hurdle_necfit(rep(s_value, 4), cens_record(logical(4), logical(4),
                                               upper = 40))
  )
}

test_that("a threshold above the other component's limit is not identified", {
  obj <- issue_415_hurdle(20)
  est <- suppressWarnings(nec(obj))
  cs <- attr(est, "censored_summary")
  # The minimum of a draw above 10 and a draw at 20 is not identified at 20.
  expect_false(any(as.numeric(est) == 20))
  expect_identical(cs$bound, c(">=", ">=", ">="))
  expect_equal(cs$upper, 10)
  expect_identical(cs$n_above, 4L)
  post <- suppressWarnings(nec(obj, posterior = TRUE))
  expect_true(all(is.na(post)))
  expect_identical(attr(post, "censored")$above, rep(TRUE, 4))
  expect_warning(nec(obj), "not identified for 4 of 4 draws")
})

test_that("a threshold at or below the other component's limit is kept", {
  est <- nec(issue_415_hurdle(5))
  expect_equal(as.numeric(est), c(5, 5, 5))
  expect_null(attr(est, "censored_summary"))
  # A value at the limit itself is the minimum whatever the censored draw is,
  # so it is identified too.
  est_at <- nec(issue_415_hurdle(10))
  expect_equal(as.numeric(est_at), c(10, 10, 10))
  expect_null(attr(est_at, "censored_summary"))
})

test_that("the combined threshold does not depend on component order", {
  g <- c(NA_real_, NA_real_, NA_real_, 3, NA_real_, 7, 9)
  attr(g, "censored") <- cens_record(
    above = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE),
    below = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  s <- c(20, 5, 20, NA_real_, NA_real_, NA_real_, 30)
  attr(s, "censored") <- cens_record(
    above = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    below = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
    upper = 40
  )
  gs <- bayesnec:::combine_censored_min(g, s, 7)
  sg <- bayesnec:::combine_censored_min(s, g, 7)
  expect_identical(gs, sg)
  # Censoring at both ends in one posterior. Draw by draw: above 10 against
  # 20; above 10 against 5; below 0 against 20; 3 against above 40; above
  # both limits; 7 against below 0; 9 against 30.
  expect_equal(gs$values, c(NA, 5, NA, 3, NA, NA, 9))
  expect_identical(gs$censored$above,
                   c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_identical(gs$censored$below,
                   c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE))
  expect_equal(c(gs$censored$lower, gs$censored$upper), c(0, 10))
  # And through the public method, with the components exchanged.
  obj <- issue_415_hurdle(20)
  swapped <- hurdle_of(obj$survival, obj$growth)
  expect_identical(suppressWarnings(nec(swapped)),
                   suppressWarnings(nec(obj)))
})

test_that("the shorter range bounds the combination in either direction", {
  # Survival's range is the shorter one here, the reverse of the usual case.
  # Survival known only to exceed 10 against growth identified at 20 and at 8.
  g <- c(20, 8)
  attr(g, "censored") <- cens_record(logical(2), logical(2), upper = 40)
  s <- c(NA_real_, NA_real_)
  attr(s, "censored") <- cens_record(c(TRUE, TRUE), logical(2))
  out <- bayesnec:::combine_censored_min(g, s, 2)
  expect_equal(out$values, c(NA, 8))
  expect_identical(out$censored$above, c(TRUE, FALSE))
  # Both censored above, at unequal limits: the minimum exceeds the smaller
  # limit, which is the one bound true of it.
  attr(g, "censored") <- cens_record(c(TRUE, TRUE), logical(2), upper = 40)
  both <- bayesnec:::combine_censored_min(g, s, 2)
  expect_identical(both$censored$above, c(TRUE, TRUE))
  expect_equal(both$censored$upper, 10)
})

test_that("a draw below the foot of its range stays below it", {
  # Legacy or structural objects can have unequal lower bounds; new fits
  # predict both components on the whole concentration series. Keep this
  # check per draw because unequal-bound summaries can misrank values.
  g <- c(NA_real_, NA_real_, 7)
  attr(g, "censored") <- cens_record(logical(3), c(TRUE, TRUE, FALSE),
                                     lower = 5)
  attr(attr(g, "censored"), "swapped") <- TRUE
  s <- c(8, 3, NA_real_)
  attr(s, "censored") <- cens_record(logical(3), c(FALSE, FALSE, TRUE))
  attr(attr(s, "censored"), "swapped") <- TRUE
  out <- bayesnec:::combine_censored_min(g, s, 3)
  expect_true(all(is.na(out$values)))
  expect_identical(out$censored$below, c(TRUE, TRUE, TRUE))
  expect_false(any(out$censored$above))
  expect_equal(out$censored$lower, 5)
  # A record remapped by a decreasing crf() keeps its flag.
  expect_true(attr(out$censored, "swapped"))
  expect_identical(bayesnec:::combine_censored_min(s, g, 3), out)
})

test_that("an identified minimum keeps its value at either foot", {
  # Unequal feet with no draw below either: the minimum of growth at 7 and
  # survival at 3 is 3, although 3 is below growth's foot of 5, and nothing
  # is marked.
  g <- c(7, 9)
  attr(g, "censored") <- cens_record(logical(2), logical(2), lower = 5)
  s <- c(3, 6)
  attr(s, "censored") <- cens_record(logical(2), logical(2))
  out <- bayesnec:::combine_censored_min(g, s, 2)
  expect_equal(out$values, c(3, 6))
  expect_false(any(out$censored$below))
  expect_equal(out$censored$lower, 5)
  expect_identical(bayesnec:::combine_censored_min(s, g, 2), out)
  # Equal feet with a draw below: the identified minimum in the other draw is
  # unchanged, and every identified value is inside the recorded limits.
  attr(g, "censored") <- cens_record(logical(2), c(TRUE, FALSE))
  g[1] <- NA_real_
  equal <- bayesnec:::combine_censored_min(g, s, 2)
  expect_equal(equal$values, c(NA, 6))
  expect_identical(equal$censored$below, c(TRUE, FALSE))
  expect_equal(equal$censored$lower, 0)
})

test_that("an unexplained missing draw is not marked by the combination", {
  g <- c(NA_real_, 4)
  attr(g, "censored") <- cens_record(c(TRUE, FALSE), logical(2))
  s <- c(NA_real_, 6)
  attr(s, "censored") <- cens_record(logical(2), logical(2), upper = 40)
  out <- bayesnec:::combine_censored_min(g, s, 2)
  # An unmarked NA supplies no bound on the minimum; leave it unexplained.
  expect_equal(out$values, c(NA, 4))
  expect_false(any(out$censored$above))
})

test_that("a curve-derived NSEC with no stored value combines as censored", {
  # A model-averaged growth component holding a smooth equation, whose NSEC is
  # read off its curve and is NA with a mark wherever the curve does not reach
  # the reference within growth's range.
  growth <- structure(
    list(mod_fits = list(
      nec3param = list(fit = list(family = list(family = "gaussian"))),
      ecx4param = list()
    ),
    ne_type = "N(S)EC", w_ne_posterior = c(NA_real_, NA_real_, 3, 8),
    success_models = c("nec3param", "ecx4param")),
    class = c("bayesmanecfit", "bnecfit")
  )
  attr(growth$w_ne_posterior, "censored") <-
    cens_record(c(TRUE, TRUE, FALSE, FALSE), logical(4))
  survival <- hurdle_necfit(c(20, 5, 20, 20),
                            cens_record(logical(4), logical(4), upper = 40))
  obj <- hurdle_of(growth, survival)
  post <- suppressMessages(suppressWarnings(nec(obj, posterior = TRUE)))
  expect_equal(as.numeric(post), c(NA, 5, 3, 8))
  expect_identical(attr(post, "censored")$above, c(TRUE, FALSE, FALSE, FALSE))
  msgs <- testthat::capture_messages(suppressWarnings(nec(obj)))
  expect_true(any(grepl("mixture of NEC and NSEC draws", msgs)))
})

test_that("the hurdle summary and a decreasing xform keep the bound", {
  obj <- issue_415_hurdle(20)
  sm <- suppressWarnings(summary(obj))
  cs <- attr(sm$ne$combined, "censored_summary")
  expect_identical(cs$bound, c(">=", ">=", ">="))
  expect_equal(cs$upper, 10)
  printed <- utils::capture.output(print(sm))
  combined_row <- printed[grepl("^combined", printed)]
  expect_false(any(grepl("20.00", combined_row, fixed = TRUE)))
  expect_true(grepl(">= 10.00", combined_row, fixed = TRUE))
  # A decreasing xform names the same draws at the other end of the new scale.
  flipped <- suppressWarnings(nec(obj, xform = function(x) -x))
  fcs <- attr(flipped, "censored_summary")
  expect_identical(fcs$bound, c("<=", "<=", "<="))
  expect_equal(fcs$lower, -10)
  expect_identical(fcs$n_below, 4L)
})
