# The selection itself is tested on a fabricated weights table rather than on a
# fitted set: pull_out() rebuilds the weighted quantities of the object it
# returns and takes seconds to do it, and none of the tie, the empty and the
# non-finite cases can be produced by a real fit anyway.
mod_stats_frame <- function(model, wi) {
  data.frame(model = model, waic = seq_along(model), wi = wi)
}

test_that("the highest weight is selected and reported", {
  ms <- mod_stats_frame(c("nec4param", "ecx4param", "ecxlin"),
                        c(0.2, 0.7, 0.1))
  expect_message(out <- bayesnec:::best_weighted_model(ms),
                 "Highest-weighted model is ecx4param, holding 0.7 of the")
  expect_equal(out, "ecx4param")
  expect_message(bayesnec:::best_weighted_model(ms), "3 candidate models")
})

test_that("an exact tie takes the first of the set and says so", {
  ms <- mod_stats_frame(c("nec4param", "ecx4param", "ecxlin"),
                        c(0.45, 0.45, 0.1))
  bayesnec:::best_weighted_model(ms) |>
    expect_equal("nec4param") |>
    expect_message("nec4param, ecx4param hold the same weight") |>
    expect_message("Highest-weighted model is nec4param")
  # Not a tie: the larger is taken without comment on the tie.
  ms$wi <- c(0.45, 0.450001, 0.099999)
  expect_equal(suppressMessages(bayesnec:::best_weighted_model(ms)),
               "ecx4param")
})

test_that("weights that are absent or not finite are refused", {
  expect_error(bayesnec:::best_weighted_model(NULL), "holds no model weights")
  expect_error(bayesnec:::best_weighted_model(mod_stats_frame(
    character(0), numeric(0))), "holds no model weights")
  expect_error(bayesnec:::best_weighted_model(mod_stats_frame(
    c("nec4param", "ecx4param"), c(NA_real_, NaN))), "finite number")
  # One usable weight among unusable ones is still a selection.
  expect_equal(suppressMessages(bayesnec:::best_weighted_model(
    mod_stats_frame(c("nec4param", "ecx4param"), c(NA_real_, 0.3)))),
    "ecx4param")
})

test_that("a bayesmanecfit yields its highest-weighted model as a bayesnecfit", {
  best <- pull_best(manec_example) |>
    suppressMessages() |>
    suppressWarnings()
  ms <- manec_example$mod_stats
  expect_equal(ms$model[which.max(ms$wi)], "nec4param")
  expect_s3_class(best, "bayesnecfit")
  expect_equal(best$model, "nec4param")
  # Identical to naming the model by hand, which is the block this replaces.
  expect_equal(best, nec4param)
})

test_that("a bayesnecfit is returned unchanged and without comment", {
  expect_identical(expect_no_message(pull_best(nec4param)), nec4param)
})

test_that("classes other than the three fitted ones are refused", {
  expect_error(pull_best(list(a = 1)), "applies to an object of class")
  expect_error(pull_best(manec_example$mod_stats), "applies to an object")
})

test_that("a hurdle fit selects within each component", {
  # The two components are selected from separately, so they can differ. Built
  # by hand from the stored fits: the only elements pull_best() reads are the
  # two components, and fitting a hurdle pair would take minutes.
  hurdle <- structure(list(growth = manec_example, survival = ecx4param,
                           y_var = "y"),
                      class = c("bayesnechurdlefit", "bnecfit"))
  out <- pull_best(hurdle) |>
    suppressMessages() |>
    suppressWarnings()
  expect_s3_class(out, "bayesnechurdlefit")
  expect_s3_class(out$growth, "bayesnecfit")
  expect_equal(out$growth$model, "nec4param")
  expect_equal(out$survival$model, "ecx4param")
  expect_equal(out$y_var, "y")
})
