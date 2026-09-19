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
  # The lower limit is an ordinary quantile of the whole sample, which falls
  # among the nine draws that were identified, so it is a number and not a
  # bound.
  expect_equal(unname(out[["Q2.5"]]), 3.475)
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
  expect_identical(attr(out, "censored_summary")$bound, c(">=", "", ">="))
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
  want <- stats::quantile(truth, c(0.5, 0.025, 0.975))
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
                 "censored above")
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
