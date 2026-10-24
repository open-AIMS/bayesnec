test_that("ecnsec returns expected object types", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecnsec_summary <- ecnsec(manec_example, nsec = 1.5, resolution = 10) |>
    suppressWarnings()
  expect_length(ecnsec_summary, 3)
  expect_equal(names(ecnsec_summary), c("50%", "2.5%", "97.5%"))
  expect_true(is.numeric(ecnsec_summary))

  ecnsec_posterior <- ecnsec(manec_example, nsec = 1.5, posterior = TRUE,
                             resolution = 10) |>
    suppressWarnings()
  expect_true(is.numeric(ecnsec_posterior))
  expect_gt(length(ecnsec_posterior), length(ecnsec_summary))
})

test_that("ecnsec works for a bayesnecfit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecnsec1 <- ecnsec(nec4param, nsec = 1.5, resolution = 10) |>
    suppressWarnings()
  expect_length(ecnsec1, 3)
  expect_equal(names(ecnsec1), c("50%", "2.5%", "97.5%"))
})

test_that("ecnsec argument validation behaves as expected", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # prob_vals must supply central, lower and upper quantiles, in that order
  expect_error(ecnsec(manec_example, nsec = 1.5, prob_vals = 0.9))
  expect_error(ecnsec(manec_example, nsec = 1.5, prob_vals = c(0.6, 0.9, 0.1)))
  # hormesis_def must be one of "max" or "control"
  # hormesis_def is removed, and a call still passing it is refused by name
  # rather than absorbed by ... in silence. See D15 ruling 4.
  expect_error(ecnsec(manec_example, nsec = 1.5, hormesis_def = "invalid"),
               "hormesis_def has been removed")
  expect_error(ecnsec(manec_example, nsec = 1.5, hormesis_def = "control"),
               "hormesis_def has been removed")
  # xform must be a function
  expect_error(ecnsec(manec_example, nsec = 1.5, xform = "notafunction"))
})


test_that("ecnsec takes the ecx vocabulary and refuses direct", {
  # T8: ecnsec inverts the ecx reference construction under the same type, so
  # it takes the same four names minus "direct", which is a response value
  # rather than a percentage and has no percent effect to report.
  skip_on_cran()
  expect_length(ecnsec(nec4param, nsec = 1.5, resolution = 50), 3)
  expect_length(
    ecnsec(nec4param, nsec = 1.5, resolution = 50, type = "range"), 3
  )
  expect_error(ecnsec(nec4param, nsec = 1.5, resolution = 50, type = "direct"),
               "names a response value")
  expect_error(ecnsec(nec4param, nsec = 1.5, resolution = 50,
                      type = "nonsense"),
               "type must be one of")
})

test_that("the absolute ecnsec agrees with the ecx definition", {
  # The two must answer the same question of the same curve. Read the percent
  # effect at a concentration, then ask ecx for the concentration at that
  # percent effect, and the round trip must return where it started.
  skip_on_cran()
  target_x <- 1.5
  pct <- ecnsec(nec4param, nsec = target_x, resolution = 500)
  back <- ecx(nec4param, ecx_val = as.numeric(pct[1]), type = "absolute",
              resolution = 500)
  expect_equal(as.numeric(back[1]), target_x, tolerance = 0.05)
})


# ---- a model set under every type --------------------------------------------

# ecx_asymptote() read object$fit, which a bayesmanecfit does not have, so
# ecnsec(<set>, type = "relative") failed inside posterior with "Don't know how
# to transform an object of class 'NULL'" -- a message naming neither the
# argument nor the class. "absolute" and "range" worked on the same object, so
# the failure was in the asymptote alone.

test_that("ecnsec answers a model set under every percentage type", {
  skip_on_cran()
  out <- lapply(c("absolute", "relative", "range"), function(ty) {
    suppressWarnings(
      ecnsec(manec_example, nsec = 1.5, resolution = 50, type = ty)
    )
  })
  for (o in out) {
    expect_equal(length(o), 3)
    expect_false(anyNA(o))
  }
  names(out) <- c("absolute", "relative", "range")
  # The types order as their denominators do. manec_example's equations both
  # carry a bot well below the observed response, so the control-to-bot span is
  # the widest and the same decline is the smallest percentage of it.
  expect_lt(out$relative[1], out$range[1])
  expect_lt(out$range[1], out$absolute[1])
})

test_that("the model-averaged asymptote is one value per averaged draw", {
  skip_on_cran()
  a <- manec_asymptote(manec_example)
  # Stacked in the same order and thinned by the same index as the
  # model-averaged posterior, so the two pair row for row.
  expect_equal(length(a),
               nrow(posterior_epred(manec_example,
                                    newdata = bnec_newdata(manec_example,
                                                           resolution = 5),
                                    re_formula = NA)))
})


# ---- an nsec beyond the range (#437, D33) ------------------------------------

test_that("ecnsec refuses an nsec beyond the range on a fit and a model set", {
  skip_on_cran()
  # The curve was evaluated at nsec itself, so 1000 on data reaching 3.22 was
  # read off the curve extended past the data with nothing said. Every class
  # now refuses it, naming the range.
  for (obj in list(nec4param, manec_example)) {
    expect_error(ecnsec(obj, nsec = 1000, resolution = 50),
                 paste0("nsec = 1000 lies outside the observed range of the ",
                        "predictor, 0.03235 to 3.221. ecnsec() reads the ",
                        "curve over that range only. Supply an x_range that ",
                        "includes nsec to read the curve extended past the ",
                        "data."),
                 fixed = TRUE)
    expect_error(ecnsec(obj, nsec = 0.01, resolution = 50),
                 "lies outside the observed range of the predictor")
    expect_error(ecnsec(obj, nsec = 2.5, resolution = 50, x_range = c(0.5, 2)),
                 "nsec = 2.5 lies outside the x_range supplied, 0.5 to 2.",
                 fixed = TRUE)
    # Checked after xform, which takes nsec to the scale the curve is read on.
    expect_error(ecnsec(obj, nsec = log(1000), resolution = 50, xform = exp),
                 "nsec = 6.908, which xform takes to 1000, lies outside",
                 fixed = TRUE)
    # An x_range that includes nsec reads the curve there, as before.
    expect_length(
      suppressWarnings(ecnsec(obj, nsec = 4, resolution = 50,
                              x_range = c(0.0324, 5))),
      3
    )
  }
})

test_that("ecnsec inside the range returns what it returned before #437", {
  skip_on_cran()
  # Pinned from an installed build of origin/predev at ba30b91e, R 4.6.1. The
  # refusal is the only change to ecnsec.bnecfit(), so every value inside the
  # range, the two ends included, is unchanged.
  x <- nec4param$fit$data$x
  pinned <- list(
    list(ecnsec(nec4param, nsec = 1.5, resolution = 50),
         c(9.6679346982, 4.5264663626, 13.4112060620)),
    list(ecnsec(nec4param, nsec = 1.5, resolution = 50, type = "range"),
         c(3.3818138375, 1.4867218674, 5.0045613634)),
    list(ecnsec(nec4param, nsec = max(x), resolution = 50),
         c(290.4982012775, 282.6041443962, 301.3408679053)),
    list(ecnsec(nec4param, nsec = min(x), resolution = 50),
         c(-0.0000046669, -5.6917471932, 4.1438965336)),
    list(ecnsec(nec4param, nsec = 1.5, resolution = 50, x_range = c(0.5, 2)),
         c(9.6679346982, 4.5264663626, 13.4112060620)),
    list(ecnsec(nec4param, nsec = log(1.5), resolution = 50, xform = exp),
         c(9.6679346982, 4.5264663626, 13.4112060620)),
    list(suppressWarnings(ecnsec(manec_example, nsec = 1.5, resolution = 50)),
         c(12.5097875487, 7.4548986212, 19.7283206278)),
    list(suppressWarnings(ecnsec(manec_example, nsec = 1.5, resolution = 50,
                                 type = "relative")),
         c(2.6660538137, 1.2901198629, 5.7809537326)),
    list(suppressWarnings(ecnsec(manec_example, nsec = 1.5, resolution = 50,
                                 type = "range")),
         c(4.3277366050, 2.4286266731, 6.6623454970)),
    list(suppressWarnings(ecnsec(manec_example, nsec = max(x),
                                 resolution = 50)),
         c(291.2865280396, 275.5040983078, 302.3384174507))
  )
  for (p in pinned) {
    expect_equal(unname(p[[1]]), p[[2]], tolerance = 1e-8)
  }
  # An end that has been through a transformation and its inverse is inside.
  expect_equal(ecnsec(nec4param, nsec = exp(log(max(x))), resolution = 50),
               ecnsec(nec4param, nsec = max(x), resolution = 50),
               tolerance = 1e-8)
})
