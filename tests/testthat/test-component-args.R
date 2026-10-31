# Paired hurdle fits take which; joint two-block fits take dpar. Refuse the
# other argument rather than dropping it through dots, and check that model
# averaging forwards dpar to each component.

test_that("check_component_arg rejects dpar on a bayesnechurdlefit", {
  hurdle <- structure(list(), class = c("bayesnechurdlefit", "bnecfit"))
  expect_true(bayesnec:::check_component_arg(list(), hurdle))
  expect_true(bayesnec:::check_component_arg(list(resolution = 10), hurdle))
  expect_error(
    bayesnec:::check_component_arg(list(dpar = "mu"), hurdle),
    "holds two separate fits"
  )
})

test_that("check_component_arg rejects which on a single or averaged fit", {
  for (cls in c("bayesnecfit", "bayesmanecfit")) {
    obj <- structure(list(), class = c(cls, "bnecfit"))
    expect_true(bayesnec:::check_component_arg(list(), obj))
    expect_error(
      bayesnec:::check_component_arg(list(which = "growth"), obj),
      "bnec_hurdle"
    )
    # the error names the argument that should have been used
    expect_error(
      bayesnec:::check_component_arg(list(which = "growth"), obj), "dpar"
    )
  }
})

test_that("nec() rejects dpar rather than silently returning the combined", {
  expect_true(bayesnec:::check_nec_no_dpar(list()))
  expect_error(bayesnec:::check_nec_no_dpar(list(dpar = "mu")),
               "no block selection")
})

test_that("dpar is a formal argument of the estimate methods", {
  # Reading it out of `...` is what allowed a mistyped argument to be
  # discarded without complaint.
  for (f in list(bayesnec:::ecx.bayesnecfit, bayesnec:::ecx.bayesmanecfit,
                 bayesnec:::nsec.bayesnecfit, bayesnec:::nsec.bayesmanecfit)) {
    expect_true("dpar" %in% names(formals(f)))
    expect_null(eval(formals(f)$dpar))
  }
})

test_that("the guards fire through the exported generics", {
  # manec_example is an ordinary (non-hurdle) bayesmanecfit
  skip_if_not(exists("manec_example"))
  expect_error(ecx(manec_example, which = "growth"), "bnec_hurdle")
  expect_error(nsec(manec_example, which = "growth"), "bnec_hurdle")
  expect_error(nec(manec_example, which = "growth"), "bnec_hurdle")
  expect_error(nec(manec_example, dpar = "mu"), "no block selection")
  # dpar on a non-hurdle family is still rejected by the existing check
  expect_error(ecx(manec_example, dpar = "mu"), "only valid for hurdle")
  expect_error(nsec(manec_example, dpar = "mu"), "only valid for hurdle")
})

test_that("ecx and nsec forward dpar to the per-model calls", {
  # Use real joint fits to check component forwarding through model averaging.
  skip_on_cran()
  skip_on_ci()
  set.seed(7)
  nec3 <- function(x, top, beta, nec) {
    top * exp(-exp(beta) * (x - nec) * (x > nec))
  }
  conc <- rep(c(0, 0.5, 1, 2, 3, 4, 5), each = 12)
  mu <- nec3(conc, 25, log(0.55), 1)
  pa <- nec3(conc, 0.97, log(0.9), 2)
  y <- ifelse(rbinom(length(conc), 1, pa) == 1,
              rgamma(length(conc), 12, 12 / mu), 0)
  d <- data.frame(x = conc, y = y)

  jm <- suppressWarnings(suppressMessages(
    bnec(y ~ crf(x, model = c("nec3param", "ecxexp")), data = d,
         family = "hurdle_gamma", chains = 2, iter = 1000, warmup = 500,
         seed = 7, refresh = 0)
  ))
  expect_s3_class(jm, "bayesmanecfit")

  combined <- suppressMessages(ecx(jm, ecx_val = 50))
  survival <- suppressMessages(ecx(jm, ecx_val = 50, dpar = "hu"))
  # Distinct combined and survival estimates detect a dropped dpar argument.
  expect_false(isTRUE(all.equal(unname(combined), unname(survival))))
  # Survival is the less sensitive process in this simulation, so its ECx sits
  # above the combined one.
  expect_gt(survival[[1]], combined[[1]])

  # Check the same component distinction through nsec().
  n_combined <- suppressMessages(nsec(jm))
  n_survival <- suppressMessages(nsec(jm, dpar = "hu"))
  expect_false(isTRUE(all.equal(unname(n_combined), unname(n_survival))))
})
