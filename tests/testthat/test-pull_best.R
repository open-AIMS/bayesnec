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
  expect_error(bayesnec:::best_weighted_model(NULL), "holds no table of model")
  expect_error(bayesnec:::best_weighted_model(mod_stats_frame(
    character(0), numeric(0))), "holds no table of model")
  # A frame without the column the name is read from, which would otherwise
  # report NA as the model and fail inside pull_out().
  expect_error(bayesnec:::best_weighted_model(
    data.frame(waic = 1:2, wi = c(0.4, 0.6))), "holds no table of model")
  expect_error(bayesnec:::best_weighted_model(
    data.frame(model = c("nec4param", "ecx4param"))), "holds no table of model")
  expect_error(bayesnec:::best_weighted_model(mod_stats_frame(
    c("nec4param", "ecx4param"), c(NA_real_, NaN))), "finite number")
  # One usable weight among unusable ones is still a selection.
  expect_equal(suppressMessages(bayesnec:::best_weighted_model(
    mod_stats_frame(c("nec4param", "ecx4param"), c(NA_real_, 0.3)))),
    "ecx4param")
})

test_that("a bayesmanecfit yields its highest-weighted model as a bayesnecfit", {
  best <- suppressMessages(pull_best(manec_example))
  ms <- manec_example$mod_stats
  expect_equal(ms$model[which.max(ms$wi)], "nec4param")
  expect_s3_class(best, "bayesnecfit")
  expect_equal(best$model, "nec4param")
  # Identical to naming the model by hand, which is the block this replaces.
  expect_equal(best, nec4param)
})

test_that("a bayesnecfit is returned unchanged, and says which model it holds", {
  pull_best(nec4param) |>
    expect_identical(nec4param) |>
    expect_message("holds the single model nec4param")
})

test_that("arguments other than the object are refused before anything else", {
  # Refused rather than ignored: honoured on the bayesmanecfit branch and
  # ignored on the bayesnecfit branch, they would make the returned object
  # depend on the class the caller was told not to test for.
  expect_error(pull_best(manec_example, resolution = 50),
               "`resolution` cannot be passed here")
  expect_error(pull_best(nec4param, resolution = 50),
               "`resolution` cannot be passed here")
  # Reaching pull_out() this one would be reported by base R, several frames
  # away, as "matched by multiple actual arguments".
  expect_error(pull_best(manec_example, model = "ecx4param"),
               "`model` cannot be passed here")
  expect_error(pull_best(manec_example, 50), "takes the object and nothing")
  # Before dispatch, so an unusable object is not what gets reported first.
  expect_error(pull_best(list(a = 1), resolution = 50),
               "takes the object and nothing")
  # And before the selection is reported.
  expect_no_message(try(pull_best(manec_example, resolution = 50),
                        silent = TRUE))
})

test_that("classes other than the three fitted ones are refused", {
  expect_error(pull_best(list(a = 1)), "applies to an object of class")
  expect_error(pull_best(manec_example$mod_stats), "applies to an object")
})

test_that("a hurdle fit selects within each component", {
  # Both components are model sets, and the survival set is weighted the other
  # way round, so each has to be selected from on its own to land where it
  # does. A component left untouched would still be a bayesmanecfit. Built by
  # hand from the stored fits: the only elements pull_best() reads are the two
  # components, and fitting a hurdle pair would take minutes.
  flipped <- manec_example
  # `wi[]` rather than `wi`: assigning the vector whole would strip the
  # pseudobma_bb_weights class and the `method` attribute pull_out() reads, and
  # the fixture would then exercise its unknown-method path rather than the one
  # a fitted object takes.
  flipped$mod_stats$wi[] <- rev(as.numeric(flipped$mod_stats$wi))
  hurdle <- structure(list(growth = manec_example, survival = flipped,
                           y_var = "y"),
                      class = c("bayesnechurdlefit", "bnecfit"))
  # The labels are what tells the two components' reports apart, so they are
  # asserted on the call that is made anyway rather than by calling twice. The
  # assignment is inside the expectations because expect_message() returns the
  # condition it matched, not the value of the expression, while the expression
  # itself still runs to completion under its calling handler.
  out <- NULL
  expect_message(
    expect_message(out <- pull_best(hurdle), "Growth component:"),
    "Survival component:"
  ) |>
    suppressMessages()
  expect_s3_class(out, "bayesnechurdlefit")
  expect_s3_class(out$growth, "bayesnecfit")
  expect_s3_class(out$survival, "bayesnecfit")
  expect_equal(out$growth$model, "nec4param")
  expect_equal(out$survival$model, "ecx4param")
  # Everything outside the two components is left as it was.
  expect_equal(out$y_var, "y")
})
