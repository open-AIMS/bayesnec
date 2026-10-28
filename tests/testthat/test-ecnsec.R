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

test_that("ecnsec values inside the range are pinned", {
  skip_on_cran()
  # A change to any value here changes what ecnsec() reports, and needs a
  # NEWS.md entry rather than a new pin.
  x <- nec4param$fit$data$x
  pinned <- list(
    list(ecnsec(nec4param, nsec = 1.5, resolution = 50),
         c(9.5536944482, 0, 27.6839491278)),
    list(ecnsec(nec4param, nsec = 1.5, resolution = 50, type = "range"),
         c(3.2843282488, 0, 10.3206366095)),
    list(ecnsec(nec4param, nsec = max(x), resolution = 50),
         c(290.5836279393, 260.1106561140, 317.7187010446)),
    list(ecnsec(nec4param, nsec = min(x), resolution = 50),
         c(0, 0, 0)),
    list(ecnsec(nec4param, nsec = 1.5, resolution = 50, x_range = c(0.5, 2)),
         c(9.5536944482, 0, 27.6839491278)),
    list(ecnsec(nec4param, nsec = log(1.5), resolution = 50, xform = exp),
         c(9.5536944482, 0, 27.6839491278)),
    list(suppressWarnings(ecnsec(manec_example, nsec = 1.5, resolution = 50)),
         c(12.2832175296, 0, 37.4176007938)),
    list(suppressWarnings(ecnsec(manec_example, nsec = 1.5, resolution = 50,
                                 type = "relative")),
         c(2.2009263264, 0, 11.7468392046)),
    list(suppressWarnings(ecnsec(manec_example, nsec = 1.5, resolution = 50,
                                 type = "range")),
         c(4.1178541346, 0, 12.7351441563)),
    list(suppressWarnings(ecnsec(manec_example, nsec = max(x),
                                 resolution = 50)),
         c(292.0665156400, 257.4168091106, 335.2299396389))
  )
  for (p in pinned) {
    expect_equal(unname(p[[1]]), p[[2]], tolerance = 1e-8)
  }
  # An end that has been through a transformation and its inverse is inside.
  expect_equal(ecnsec(nec4param, nsec = exp(log(max(x))), resolution = 50),
               ecnsec(nec4param, nsec = max(x), resolution = 50),
               tolerance = 1e-8)
})


# ---- each draw against its own curve -----------------------------------------

test_that("ecnsec compares each draw with the same draw's curve at nsec", {
  skip_on_cran()
  # nec4param is flat at its control below its nec, so below every nec draw
  # every draw has no effect.
  expect_lt(0.5, min(nec4param$ne_posterior))
  p <- ecnsec(nec4param, nsec = 0.5, posterior = TRUE)
  expect_length(p, length(nec4param$ne_posterior))
  expect_true(all(p == 0))
  expect_equal(unname(ecnsec(nec4param, nsec = 0.5)), c(0, 0, 0))
  # A model set stacks the nec4param draws first. They are 0 for the same
  # reason, and the ecx4param draws, whose curve declines from the control,
  # are not.
  mods <- names(manec_example$mod_fits)
  n_nec <- length(bayesnec:::pull_draw_index(
    manec_example, mods, manec_example$sample_size
  )[["nec4param"]])
  expect_identical(mods[1], "nec4param")
  pm <- suppressWarnings(ecnsec(manec_example, nsec = 0.5, posterior = TRUE))
  expect_true(all(pm[seq_len(n_nec)] == 0))
  expect_true(all(pm[-seq_len(n_nec)] > 0))
})

test_that("ecnsec takes one non-missing nsec on a fit and a model set", {
  skip_on_cran()
  for (obj in list(nec4param, manec_example)) {
    for (bad in list(c(0.5, 1.5, 3), NA_real_, NA, numeric(0), "1.5")) {
      expect_error(suppressWarnings(ecnsec(obj, nsec = bad)),
                   "`nsec` must be a number", fixed = TRUE)
    }
  }
  # The three values nsec() returns are refused, not read at the last of them.
  three <- suppressWarnings(nsec(nec4param, resolution = 50))
  expect_length(three, 3)
  expect_error(ecnsec(nec4param, nsec = three), "`nsec` must be a number",
               fixed = TRUE)
  # An xform that takes nsec off its domain is refused by name, not as an
  # x_range.
  expect_error(suppressWarnings(ecnsec(nec4param, nsec = -1, xform = log)),
               "nsec = -1, which xform takes to NaN, lies outside",
               fixed = TRUE)
})


# ---- a joint refit whose levels span different ranges -------------------------

# One fit split into two levels at x = 1: "low" observed from 0.0323 to 0.971
# and "high" from 1.02 to 3.22. Both levels read the same curve, so only the
# range each level is read over differs.
joint_split <- function() {
  d <- nec4param$fit$data
  d$site <- factor(ifelse(d$x < 1, "low", "high"), levels = c("low", "high"))
  bayesnec:::allot_class(
    list(fit = nec4param$fit, data = d, group_var = "site",
         levels = c("low", "high"), model = "nec4param",
         bayesnecformula = nec4param$bayesnecformula,
         level_spec = list(composed = FALSE)),
    c("bayesnecjointfit", "bnecfit")
  )
}

test_that("ecnsec on a joint refit names the level whose range refused nsec", {
  skip_on_cran()
  j <- joint_split()
  expect_error(ecnsec(j, nsec = 2),
               paste0("nsec = 2 lies outside the observed range of the ",
                      "predictor at level \"low\" of site, 0.03235 to 0.9712. ",
                      "ecnsec() reads the curve of each level over that ",
                      "level's own range only. Supply an x_range that ",
                      "includes nsec to read the curve extended past that ",
                      "level's data."),
               fixed = TRUE)
  expect_error(ecnsec(j, nsec = 0.5),
               paste0("nsec = 0.5 lies outside the observed range of the ",
                      "predictor at level \"high\" of site, 1.019 to 3.221."),
               fixed = TRUE)
  # A supplied x_range is every level's range, so no level is named.
  expect_error(ecnsec(j, nsec = 2, x_range = c(0.5, 1.5)),
               paste0("nsec = 2 lies outside the x_range supplied, 0.5 to ",
                      "1.5. ecnsec() reads the curve over that range only."),
               fixed = TRUE)
  expect_error(ecnsec(j, nsec = c(0.5, 2)), "`nsec` must be a number",
               fixed = TRUE)
})

test_that("ecnsec refuses relative on a joint two-block fit (#491)", {
  fit <- structure(list(model = "nec4param",
                        fit = list(family = validate_family("hurdle_gamma")),
                        bayesnecformula = bnf(y ~ crf(x, "nec4param"))),
                   class = c("bayesnecfit", "bnecfit"))
  expect_error(
    suppressWarnings(ecnsec(fit, nsec = 1, type = "relative")),
    paste0("not defined for the combined curve of the hurdle_gamma family.*",
           "Use type = \"absolute\" or type = \"range\"\\.$")
  )
  set <- structure(list(mod_fits = list(a = fit)),
                   class = c("bayesmanecfit", "bnecfit"))
  expect_error(suppressWarnings(ecnsec(set, nsec = 1, type = "relative")),
               "combined curve of the hurdle_gamma family")
})
