# extrapolate (#392). The choice of bound sits on top of the censoring record
# #395 built, so what is pinned here is the choice: which limits are accepted,
# what each does to the record, and what is refused.

bounds_of <- function(lower, upper) list(lower = lower, upper = upper)

test_that("FALSE returns no limits and leaves the estimate alone", {
  expect_null(bayesnec:::extrapolate_limits(FALSE, bounds_of(0, 10), "NEC"))
})

test_that("a number is an upper limit and a pair is both limits", {
  one <- bayesnec:::extrapolate_limits(20, bounds_of(0, 10), "NEC")
  expect_equal(c(one$lower, one$upper), c(0, 20))
  two <- bayesnec:::extrapolate_limits(c(-5, 20), bounds_of(0, 10), "NEC")
  expect_equal(c(two$lower, two$upper), c(-5, 20))
})

test_that("an infinite limit needs every component to sample a NEC", {
  out <- bayesnec:::extrapolate_limits(TRUE, bounds_of(0, 10), c("NEC", "NEC"))
  expect_equal(c(out$lower, out$upper), c(-Inf, Inf))
  # The default bnec() set mixes the two classes, so this is the common path
  # rather than an edge case. The error names the finite form and the equation
  # that cannot take an infinite one.
  expect_error(
    bayesnec:::extrapolate_limits(TRUE, bounds_of(0, 10), c("NEC", "NSEC"),
                                  c("nec4param", "ecx4param")),
    "extrapolate = c\\(<lower>, <upper>\\)"
  )
  expect_error(
    bayesnec:::extrapolate_limits(TRUE, bounds_of(0, 10), c("NEC", "NSEC"),
                                  c("nec4param", "ecx4param")),
    "ecx4param"
  )
  # A pair with one infinite entry is the same request by another spelling and
  # is refused the same way.
  expect_error(
    bayesnec:::extrapolate_limits(c(-Inf, 20), bounds_of(0, 10), "NSEC"),
    "samples a NEC"
  )
})

test_that("a limit inside the range is refused rather than applied", {
  # Accepting it would report an estimate as censored at a value the fit had no
  # trouble identifying, which is a tightening dressed as an extrapolation.
  expect_error(bayesnec:::extrapolate_limits(5, bounds_of(0, 10), "NEC"),
               "narrow it with x_range instead")
  expect_error(bayesnec:::extrapolate_limits(c(2, 20), bounds_of(0, 10),
                                             "NEC"),
               "lower limit of 2")
  # The current bounds themselves are accepted, and report what FALSE reports.
  same <- bayesnec:::extrapolate_limits(c(0, 10), bounds_of(0, 10), "NEC")
  expect_equal(c(same$lower, same$upper), c(0, 10))
})

test_that("a malformed extrapolate is refused with the forms it takes", {
  for (bad in list(NA, c(TRUE, TRUE), "10", c(1, 2, 3), c(1, NA))) {
    expect_error(bayesnec:::extrapolate_limits(bad, bounds_of(0, 10), "NEC"),
                 "FALSE, TRUE, a single upper limit")
  }
  expect_error(bayesnec:::extrapolate_limits(c(20, 5), bounds_of(0, 10),
                                             "NEC"),
               "Give the lower limit first")
})

test_that("limits are put on the fitted scale before any draw is compared", {
  # A decreasing crf() takes the top of the recorded scale to the foot of the
  # fitted one, so the pair is transformed and then ordered. Carrying the names
  # across would compare a draw against the one bound it is known not to reach.
  data(nec_data)
  rising <- bayesnecformula(y ~ crf(log(x), "nec3param"))
  out <- bayesnec:::fitted_extrapolate_limits(bounds_of(1, 20),
                                              bounds_of(1, 10), rising)
  expect_equal(c(out$lower, out$upper), log(c(1, 20)))
  falling <- bayesnecformula(y ~ crf(-x, "nec3param"))
  flipped <- bayesnec:::fitted_extrapolate_limits(bounds_of(1, 20),
                                                  bounds_of(1, 10), falling)
  expect_equal(c(flipped$lower, flipped$upper), c(-20, -1))
  # An infinite recorded limit stays infinite at the end the direction puts it.
  unbounded <- bayesnec:::fitted_extrapolate_limits(bounds_of(-Inf, Inf),
                                                    bounds_of(1, 10), falling)
  expect_equal(c(unbounded$lower, unbounded$upper), c(-Inf, Inf))
  # A limit outside the domain of the transformation is refused, rather than
  # left to put -Inf at the foot of a grid no curve can be read on.
  expect_error(
    bayesnec:::fitted_extrapolate_limits(bounds_of(0, 20), bounds_of(1, 10),
                                         rising),
    "domain of the crf"
  )
})

test_that("releasing a sampled posterior is a comparison and nothing more", {
  draws <- c(1, 5, 9, 12)
  tight <- bayesnec:::recensor_sampled(draws, bounds_of(2, 10))
  expect_identical(tight$above, c(FALSE, FALSE, FALSE, TRUE))
  expect_identical(tight$below, c(TRUE, FALSE, FALSE, FALSE))
  loose <- bayesnec:::recensor_sampled(draws, bounds_of(-Inf, Inf))
  expect_false(bayesnec:::has_censoring(loose))
})

test_that("a truncated nec prior is reported against the limit asked for", {
  priors <- list(nec3param = list(lb = 0.1, ub = 5))
  # The bound is not reached, so there is nothing to say.
  expect_silent(bayesnec:::report_bounded_ne_prior(priors,
                                                   bounds_of(0.1, 5)))
  expect_message(
    bayesnec:::report_bounded_ne_prior(priors, bounds_of(0.1, 50)),
    "bounded above at 5"
  )
  expect_message(
    bayesnec:::report_bounded_ne_prior(priors, bounds_of(0.001, 5)),
    "bounded below at 0.1"
  )
  # A bound stored as the character form of a grid value parses back a few
  # units in the last place away from it, so the comparison needs a tolerance:
  # without one, a lower limit that extends nothing was reported as blocked.
  drifted <- list(nec3param = list(lb = as.numeric("0.03234801324009"),
                                   ub = NA_real_))
  expect_silent(
    bayesnec:::report_bounded_ne_prior(drifted, bounds_of(0.03234801324009,
                                                          50))
  )
})

test_that("extrapolate is refused where no prediction range is stored", {
  expect_error(bayesnec:::check_no_extrapolate(5, "brmsfit"),
               "Set the grid with x_range instead")
  expect_true(bayesnec:::check_no_extrapolate(FALSE, "brmsfit"))
})

# Integration. These reuse the packaged fits and change only the prediction
# grid, so no model is compiled or sampled. The grid is cut to 0.9, which is
# below every nec4param draw, so the stored estimate is wholly censored and
# what a limit releases is visible.

extrapolate_x_range <- function() {
  c(min(manec_example$mod_fits[["ecx4param"]]$fit$data$x), 0.9)
}

truncated_nec_fit <- function() {
  f <- manec_example$mod_fits[["nec4param"]]
  suppressMessages(suppressWarnings(bayesnec:::expand_and_assign_nec(
    f, f$bayesnecformula, model = "nec4param",
    x_range = extrapolate_x_range(), resolution = 50
  )))
}

truncated_manec_fit <- function() {
  fs <- manec_example$mod_fits
  forms <- lapply(fs, function(z) z$bayesnecformula)
  bayesnec:::allot_class(
    suppressMessages(suppressWarnings(bayesnec:::expand_manec(
      fs, formula = forms, x_range = extrapolate_x_range(), resolution = 50
    ))),
    c("bayesmanecfit", "bnecfit")
  )
}

test_that("a fit with no censored draw reports what it reported before", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # The regression guard. manec_example's own grid spans its data and nothing
  # is censored, so no value may change under the default.
  full <- suppressMessages(suppressWarnings(pull_out(manec_example,
                                                     model = "nec4param")))
  plain <- suppressWarnings(nec(full))
  expect_equal(as.numeric(plain), as.numeric(full$ne), tolerance = 1e-12)
  expect_null(attr(plain, "censored_summary"))
  released <- suppressMessages(suppressWarnings(nec(full, extrapolate = TRUE)))
  expect_identical(as.numeric(released), as.numeric(plain))
  expect_null(attr(released, "censored_summary"))
})

test_that("TRUE returns the posterior a truncated grid had censored", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  fit <- truncated_nec_fit()
  censored <- suppressWarnings(nec(fit))
  # Every draw of this posterior lies above the cut, so the default reports the
  # bound at every entry and states the fraction.
  expect_equal(attr(censored, "censored_summary")$n_above, 100L)
  expect_true(all(as.numeric(censored) == 0.9))
  released <- suppressMessages(suppressWarnings(nec(fit, extrapolate = TRUE)))
  expect_null(attr(released, "censored_summary"))
  # A sampled nec needs no curve, so releasing it recovers the summary of the
  # untruncated fit exactly rather than approximately.
  full <- suppressMessages(suppressWarnings(pull_out(manec_example,
                                                     model = "nec4param")))
  expect_equal(as.numeric(released), as.numeric(suppressWarnings(nec(full))),
               tolerance = 1e-12)
})

test_that("a finite limit censors at the limit and not at the grid", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  fit <- truncated_nec_fit()
  part <- suppressMessages(suppressWarnings(nec(fit, extrapolate = 1.45)))
  cens <- attr(part, "censored_summary")
  expect_equal(cens$upper, 1.45)
  expect_gt(cens$n_above, 0)
  expect_lt(cens$n_above, 100)
  # The fraction falls as the limit rises, and the reported entries that are
  # bounds are the limit itself.
  expect_lt(cens$n_above, attr(suppressWarnings(nec(fit)),
                               "censored_summary")$n_above)
  expect_true(all(part[nzchar(cens$bound)] == 1.45))
  # The posterior itself carries the new record, so a caller taking draws sees
  # the same accounting as one taking the summary.
  post <- suppressMessages(suppressWarnings(
    nec(fit, extrapolate = 1.45, posterior = TRUE)
  ))
  expect_identical(sum(attr(post, "censored")$above), cens$n_above)
})

test_that("a decreasing xform censors the same draws at the other end", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  fit <- truncated_nec_fit()
  plain <- suppressMessages(suppressWarnings(nec(fit, extrapolate = 1.45)))
  flipped <- suppressMessages(suppressWarnings(
    nec(fit, extrapolate = 1.45, xform = function(x) -x)
  ))
  up <- attr(plain, "censored_summary")
  down <- attr(flipped, "censored_summary")
  # The comparison happens on the fitted scale, before xform, so the same draws
  # are censored. What changes is the end they are named at and the value the
  # bound is stated as.
  expect_identical(down$n_below, up$n_above)
  expect_identical(down$n_above, up$n_below)
  expect_equal(down$lower, -1.45)
  expect_identical(down$bound, c("<=", "<=", ""))
})

test_that("TRUE is refused on a set that mixes the two classes", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  m <- truncated_manec_fit()
  expect_error(suppressMessages(nec(m, extrapolate = TRUE)),
               "samples a NEC")
  expect_error(suppressMessages(nec(m, extrapolate = TRUE)),
               "ecx4param")
})

test_that("a finite limit extends both halves of a mixed set", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  m <- truncated_manec_fit()
  before <- attr(suppressMessages(suppressWarnings(nec(m))),
                 "censored_summary")
  expect_gt(before$n_above, 0)
  after <- suppressMessages(suppressWarnings(nec(m, extrapolate = 3)))
  # The threshold draws are released by comparison and the curve-read ones are
  # re-evaluated out to the same limit, so the combined fraction falls to none.
  expect_null(attr(after, "censored_summary"))
  expect_gt(as.numeric(after)[1], before$upper)
  # The mixture is reassembled at its own size: every equation contributes the
  # same share of draws it contributed to the stored one.
  post <- suppressMessages(suppressWarnings(
    nec(m, extrapolate = 3, posterior = TRUE)
  ))
  expect_length(as.numeric(post), length(m$w_ne_posterior))
})

test_that("a limit inside the stored grid is refused on a fit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  m <- truncated_manec_fit()
  expect_error(suppressMessages(nec(m, extrapolate = 0.5)),
               "narrow it with x_range instead")
  expect_error(suppressMessages(nec(m, extrapolate = c(0.5, 3))),
               "lower limit of 0.5")
})

test_that("the default leaves every fit exactly as it was", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  m <- truncated_manec_fit()
  default <- suppressMessages(suppressWarnings(nec(m)))
  at_bounds <- suppressMessages(suppressWarnings(
    nec(m, extrapolate = c(min(m$w_pred_vals$data$x),
                           max(m$w_pred_vals$data$x)))
  ))
  # Naming the current bounds re-evaluates nothing, so the two agree to the
  # last bit rather than to a tolerance.
  expect_identical(as.numeric(default), as.numeric(at_bounds))
  expect_identical(attr(default, "censored_summary")$n_above,
                   attr(at_bounds, "censored_summary")$n_above)
})

test_that("nsec refuses an infinite limit and extends a finite one", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  f <- suppressMessages(suppressWarnings(pull_out(manec_example,
                                                  model = "ecx4param")))
  # Every NSEC is read off a curve, so no fit makes an infinite limit
  # computable here.
  expect_error(nsec(f, extrapolate = TRUE), "samples a NEC")
  narrow <- suppressWarnings(nsec(f, x_range = c(0.0324, 0.9)))
  wide <- suppressMessages(suppressWarnings(
    nsec(f, x_range = c(0.0324, 0.9), extrapolate = 3)
  ))
  n_narrow <- attr(narrow, "censored_summary")$n_above
  expect_gt(n_narrow, 0)
  # The curve is re-evaluated out to 3, so draws that had not reached the
  # reference by 0.9 are identified and the fraction falls.
  expect_lt(sum(attr(wide, "censored_summary")$n_above, 0), n_narrow)
  expect_error(suppressMessages(nsec(f, x_range = c(0.0324, 0.9),
                                     extrapolate = 0.5)),
               "narrow it with x_range instead")
})

test_that("a lower limit below the control is accepted and reported", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  f <- suppressMessages(suppressWarnings(pull_out(manec_example,
                                                  model = "ecx4param")))
  # The reference is a quantile of the control posterior, so the search begins
  # at the control whatever the grid does below it.
  expect_message(
    suppressWarnings(nsec(f, extrapolate = c(0.001, 5))),
    "search cannot begin below"
  )
})
