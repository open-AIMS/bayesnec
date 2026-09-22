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
    nsec(f, x_range = c(0.0324, 0.9), extrapolate = 5)
  ))
  n_narrow <- attr(narrow, "censored_summary")$n_above
  expect_gt(n_narrow, 0)
  # The curve is re-evaluated out to 5, so draws that had not reached the
  # reference by 0.9 are identified and the fraction falls.
  expect_lt(sum(attr(wide, "censored_summary")$n_above, 0), n_narrow)
  expect_error(suppressMessages(nsec(f, x_range = c(0.0324, 0.9),
                                     extrapolate = 0.5)),
               "narrow it with x_range instead")
})

test_that("with no x_range the stored grid counts as well", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  f <- suppressMessages(suppressWarnings(pull_out(manec_example,
                                                  model = "ecx4param")))
  # With no x_range of its own, nsec() searches the observed range, which ends
  # at 3.22, while this fit stores a grid reaching 8. A limit between the two
  # would censor the estimate inside the grid the fit was built on, which is
  # the silent tightening the argument exists to refuse, so with no x_range the
  # limit is measured against the wider of the two and nec() and nsec() refuse
  # the same numbers.
  wide <- suppressMessages(suppressWarnings(bayesnec:::expand_and_assign_nec(
    manec_example$mod_fits[["ecx4param"]],
    manec_example$mod_fits[["ecx4param"]]$bayesnecformula,
    model = "ecx4param",
    x_range = c(min(manec_example$mod_fits[["ecx4param"]]$fit$data$x), 8),
    resolution = 50
  )))
  expect_equal(bayesnec:::ne_grid_bounds(wide)$upper, 8)
  expect_lt(bayesnec:::grid_x_range(wide, NA)$upper, 4)
  expect_equal(bayesnec:::searched_or_stored_bounds(wide, NA)$upper, 8)
  expect_error(suppressMessages(nsec(wide, extrapolate = 5)),
               "which ends at 8")
  # The same object through both functions, which is the whole claim: one grid
  # was cleared, not two different ones on two different fits.
  threshold <- suppressMessages(suppressWarnings(
    bayesnec:::expand_and_assign_nec(
      manec_example$mod_fits[["nec4param"]],
      manec_example$mod_fits[["nec4param"]]$bayesnecformula,
      model = "nec4param",
      x_range = c(min(manec_example$mod_fits[["nec4param"]]$fit$data$x), 8),
      resolution = 50
    )
  ))
  expect_error(suppressMessages(nec(threshold, extrapolate = 5)),
               "which ends at 8")
  expect_error(suppressMessages(nsec(threshold, extrapolate = 5)),
               "which ends at 8")
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

# A set of two threshold equations, assembled from one packaged fit so that the
# pure-NEC branch can be exercised without fitting a second model. The two
# names are both in mod_groups$nec, which is what the branch reads; the draws
# behind them are the same fit's, which is all the branch does read.
pure_nec_stub <- function(posterior = as.numeric(manec_example$w_ne_posterior),
                          index = TRUE) {
  f <- manec_example$mod_fits[["nec4param"]]
  n <- length(posterior)
  out <- list(
    success_models = c("nec4param", "nec3param"),
    mod_fits = list(nec4param = f, nec3param = f),
    w_ne_posterior = posterior,
    w_pred_vals = manec_example$w_pred_vals,
    sample_size = n
  )
  if (index) {
    half <- floor(n / 2)
    out$w_draw_index <- list(nec4param = seq_len(half),
                             nec3param = seq.int(half + 1, n))
  }
  attr(out$w_ne_posterior, "censored") <- bayesnec:::censoring_record(
    3.22051966293556, 0.03234801324009,
    posterior >= 3.22051966293556, posterior <= 0.03234801324009
  )
  bayesnec:::allot_class(out, c("bayesmanecfit", "bnecfit"))
}

test_that("a set of threshold equations is released without a curve", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # Half the draws pushed above the top of the grid, so the stored record is
  # censored and the branch has something to release. Nothing here reads a
  # curve, which is what makes an infinite limit available at all.
  post <- as.numeric(manec_example$w_ne_posterior)
  post[1:50] <- post[1:50] + 3
  m <- pure_nec_stub(post)
  expect_identical(bayesnec:::manec_ne_types(m), c("NEC", "NEC"))
  bounds <- bayesnec:::ne_grid_bounds(m)
  lims <- bayesnec:::extrapolate_limits(TRUE, bounds,
                                        bayesnec:::manec_ne_types(m),
                                        m$success_models)
  released <- suppressMessages(
    bayesnec:::extrapolated_manec_ne(m, lims, bounds, 0.01, 200)
  )
  # Every draw keeps its value and none is censored, so the summary is the
  # ordinary one of the whole posterior.
  expect_false(bayesnec:::has_censoring(attr(released, "censored")))
  expect_identical(as.numeric(released), post)
  # The stored record censored the fifty that were pushed out.
  expect_identical(sum(attr(m$w_ne_posterior, "censored")$above), 50L)
})

test_that("a draw with no value is refused rather than dropped in silence", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # The case manec_ne_types() cannot see: a set of joint two-block fits whose
  # survival blocks hold a smooth equation is named NEC by the equation name,
  # and its combined estimate is read off a curve. Releasing such a draw by
  # comparison would quietly restore the deleted-draw summary.
  post <- as.numeric(manec_example$w_ne_posterior)
  post[1:40] <- post[1:40] + 3
  post[c(5, 11)] <- NA_real_
  m <- pure_nec_stub(post)
  bounds <- bayesnec:::ne_grid_bounds(m)
  lims <- bayesnec:::extrapolate_limits(20, bounds, c("NEC", "NEC"),
                                        m$success_models)
  expect_error(
    suppressMessages(
      bayesnec:::extrapolated_manec_ne(m, lims, bounds, 0.01, 200)
    ),
    "has no value for 2 of"
  )
})

test_that("a set with no stored draw index is rebuilt under one index", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # An object stored before the weighted draw index was kept. pull_draw_index()
  # regenerates a different draw, so slicing the stored mixture for the
  # threshold components would put those draws beside curve-read draws taken
  # under the new one. The mixture is rebuilt under the one index instead.
  m <- truncated_manec_fit()
  legacy <- m
  legacy$w_draw_index <- list()
  rebuilt <- suppressMessages(suppressWarnings(
    nec(legacy, extrapolate = 3, posterior = TRUE)
  ))
  expect_length(as.numeric(rebuilt), length(m$w_ne_posterior))
  again <- suppressMessages(suppressWarnings(
    nec(legacy, extrapolate = 3, posterior = TRUE)
  ))
  expect_identical(as.numeric(rebuilt), as.numeric(again))
})

test_that("extrapolating a fit with nothing censored changes no number", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # Every draw was identified inside the range the fit used, so a wider bound
  # censors none of them and a wider grid has none left to identify. Measured
  # before this was short-circuited: the rebuilt mixture read 1.449 (0.808,
  # 1.528) against the stored 1.450 (0.749, 1.527), a lower bound eight per
  # cent away on a fit with nothing censored.
  default <- suppressMessages(suppressWarnings(nec(manec_example)))
  wider <- suppressMessages(suppressWarnings(nec(manec_example,
                                                 extrapolate = 5)))
  expect_identical(as.numeric(default), as.numeric(wider))
  expect_null(attr(wider, "censored_summary"))
})

test_that("the censoring report names the limit rather than the range", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  fit <- truncated_nec_fit()
  plain <- tryCatch(nec(fit), bayesnec_censored = conditionMessage)
  expect_true(grepl("bound of the prediction range", plain, fixed = TRUE))
  # 1.45 is the limit the caller named, and the prediction range ends at 0.9,
  # so calling 1.45 the end of that range would state the wrong number twice.
  moved <- suppressMessages(
    tryCatch(nec(fit, extrapolate = 1.45), bayesnec_censored = conditionMessage)
  )
  expect_true(grepl("bound of the extrapolation range", moved, fixed = TRUE))
  expect_true(grepl("1.45", moved, fixed = TRUE))
  # A limit that extends nothing leaves the record and the wording alone.
  bounds <- bayesnec:::ne_grid_bounds(fit)
  unmoved <- suppressMessages(tryCatch(
    nec(fit, extrapolate = c(bounds$lower, bounds$upper)),
    bayesnec_censored = conditionMessage
  ))
  expect_true(grepl("bound of the prediction range", unmoved, fixed = TRUE))
})

test_that("a grid point outside the fitted domain is left out of the bounds", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # expand_nec() drops such a point from its own bounds, because an x_range
  # reaching zero under crf(log(x)) puts -Inf at the foot of the fitted grid.
  # Filtering the recorded grid alone kept the zero, and the lower limit of a
  # single-number extrapolate defaults to this bound, so the fit refused every
  # upper limit for naming a value the caller had not given.
  f <- suppressMessages(suppressWarnings(pull_out(manec_example,
                                                  model = "nec4param")))
  logged <- f
  logged$bayesnecformula <- bayesnecformula(y ~ crf(log(x), "nec4param"))
  logged$pred_vals$data$x <- c(0, logged$pred_vals$data$x[-1])
  bounds <- bayesnec:::ne_grid_bounds(logged)
  expect_gt(bounds$lower, 0)
  expect_silent(
    bayesnec:::fitted_extrapolate_limits(
      list(lower = bounds$lower, upper = 20), bounds, logged$bayesnecformula
    )
  )
})

test_that("a fit that stores no prediction range refuses a limit", {
  expect_null(bayesnec:::ne_grid_bounds(list(pred_vals = list(data = NULL))))
  expect_error(
    bayesnec:::extrapolate_limits(5, NULL, "NEC"),
    "stores none"
  )
  # The default forces none of it, so an object with no grid is unaffected.
  expect_null(bayesnec:::extrapolate_limits(FALSE, stop("not forced"), "NEC"))
})

test_that("a component classed for nsec gives what pull_out gives", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # The assumption behind component_fit(): nsec() reads the stored brms fit and
  # the formula and nothing expand_nec() adds, so re-evaluating a curve through
  # the classed component rather than through pull_out() is the same
  # computation without the discarded expansion. Pinned bit for bit, because a
  # future nsec() that reached for pred_vals or ne_posterior would otherwise
  # fail silently on a component that has neither.
  heavy <- suppressMessages(suppressWarnings(pull_out(manec_example,
                                                      model = "ecx4param")))
  light <- bayesnec:::component_fit(manec_example, "ecx4param")
  expect_s3_class(light, "bayesnecfit")
  expect_false("pred_vals" %in% names(light))
  from_heavy <- suppressWarnings(
    nsec(heavy, resolution = 300, x_range = c(0.0324, 5), posterior = TRUE)
  )
  from_light <- suppressWarnings(
    nsec(light, resolution = 300, x_range = c(0.0324, 5), posterior = TRUE)
  )
  expect_identical(attributes(from_heavy), attributes(from_light))
  expect_identical(as.numeric(from_heavy), as.numeric(from_light))
})

test_that("a supplied x_range is the range at both ends", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # The caller chose the range, so extrapolate extends from it rather than from
  # anything wider. Measured before this was so: extrapolate = 9 beside
  # x_range = c(0.5, 0.9) searched from 0.0324, discarding the narrowing
  # without saying so, and the pair that states the intent was refused for
  # naming a lower limit inside a range the caller had already narrowed.
  f <- suppressMessages(suppressWarnings(pull_out(manec_example,
                                                  model = "ecx4param")))
  expect_equal(
    unlist(bayesnec:::searched_or_stored_bounds(f, c(0.5, 0.9))),
    c(lower = 0.5, upper = 0.9)
  )
  single <- suppressMessages(suppressWarnings(
    nsec(f, x_range = c(0.5, 0.9), extrapolate = 9)
  ))
  pair <- suppressMessages(suppressWarnings(
    nsec(f, x_range = c(0.5, 0.9), extrapolate = c(0.5, 9))
  ))
  # The pair form is accepted and gives what the single-number form gives, so
  # stating the lower limit is never punished.
  expect_equal(as.numeric(single), as.numeric(pair), tolerance = 1e-12)
  direct <- suppressWarnings(nsec(f, x_range = c(0.5, 9)))
  expect_equal(as.numeric(single), as.numeric(direct), tolerance = 1e-12)
  # With no x_range the stored grid still counts, which is what makes nec() and
  # nsec() refuse the same numbers.
  union <- bayesnec:::searched_or_stored_bounds(f, NA)
  expect_equal(union$upper, bayesnec:::ne_grid_bounds(f)$upper)
})

test_that("a hurdle component error is relabelled only when it is one", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # The default names no extrapolate, so nothing on that path may be reported
  # as a refusal of it. Measured before this was gated: a growth component
  # holding an ecx-type equation reported "the growth component refused
  # extrapolate: nec is not a parameter in ecx model types".
  smooth <- suppressMessages(suppressWarnings(pull_out(manec_example,
                                                       model = "ecx4param")))
  obj <- structure(list(growth = smooth, survival = smooth),
                   class = c("bayesnechurdlefit", "bnecfit"))
  plain <- tryCatch(suppressMessages(nec(obj)), error = conditionMessage)
  expect_match(plain, "not a parameter in ecx model types")
  expect_false(grepl("refused extrapolate", plain, fixed = TRUE))
  # Where extrapolate is named, the component that raised the error is named
  # too, because the two components have prediction ranges of their own.
  named <- tryCatch(suppressMessages(nec(obj, extrapolate = 5)),
                    error = conditionMessage)
  expect_match(named, "growth component refused extrapolate")
})

test_that("a threshold posterior is read without expanding the component", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # expand_nec() stores it as as_draws_df(fit)[["b_nec_Intercept"]] and nothing
  # else, so the rebuild path reads it straight from the fit rather than paying
  # for an expansion whose every other product it discards.
  direct <- bayesnec:::component_ne_posterior(manec_example, "nec4param")
  expanded <- as.numeric(
    suppressMessages(suppressWarnings(
      pull_out(manec_example, model = "nec4param")
    ))$ne_posterior
  )
  expect_identical(direct, expanded)
})

test_that("a call that recomputes nothing gives no advice about the prior", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # manec_example has nothing censored, so extrapolate = 5 returns the stored
  # estimate bit for bit. Advising a refit with a wider nec prior bound on such
  # a call names a constraint that had no bearing on the number beside it.
  msgs <- character(0)
  out <- withCallingHandlers(
    suppressWarnings(nec(manec_example, extrapolate = 5)),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_identical(as.numeric(out),
                   as.numeric(suppressMessages(suppressWarnings(
                     nec(manec_example)
                   ))))
  expect_false(any(grepl("bounded above", msgs, fixed = TRUE)))
  # It is still said where the limit reaches past the bound on a fit that does
  # recompute.
  censored <- truncated_nec_fit()
  said <- character(0)
  withCallingHandlers(
    suppressWarnings(nec(censored, extrapolate = 5)),
    message = function(m) {
      said <<- c(said, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_true(any(grepl("bounded above", said, fixed = TRUE)))
})

test_that("the extrapolation marker does not reach the caller", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  fit <- truncated_nec_fit()
  post <- suppressMessages(suppressWarnings(
    nec(fit, extrapolate = 1.45, posterior = TRUE)
  ))
  expect_null(attr(post, "extrapolated"))
  expect_false(is.null(attr(post, "censored")))
})
