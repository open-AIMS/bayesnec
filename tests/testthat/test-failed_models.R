test_that("failed_models is empty for a fit where nothing failed", {
  out <- failed_models(manec_example)
  expect_s3_class(out, "bnecfailures")
  expect_length(out, 0)
  expect_output(print(out), "No models failed to fit")
  # An object saved before failures were recorded has no such element, and must
  # keep working rather than erroring on the missing field.
  expect_null(manec_example[["failed_models"]])
  expect_length(failed_models(nec4param), 0)
})

test_that("failed_models rejects objects bnec did not fit", {
  expect_error(failed_models(list()), "objects fitted by bnec")
  expect_error(failed_models(1:3), "objects fitted by bnec")
})

test_that("failure_record keeps the priors and inits when it has them", {
  pr <- define_prior("nec3param", gaussian(), nec_data$x, nec_data$y)
  cond <- bayesnec:::fit_failure_condition(
    "nec3param", "Initialization failed.", prior = pr,
    init = list(list(top = 1), list(top = 2))
  )
  rec <- bayesnec:::failure_record("nec3param", cond)
  expect_equal(rec$model, "nec3param")
  expect_equal(rec$message, "Initialization failed.")
  expect_s3_class(rec$prior, "brmsprior")
  expect_length(rec$init, 2)
  # A failure raised before the priors and inits existed still gets recorded,
  # with those fields empty rather than the whole model going unmentioned.
  plain <- bayesnec:::failure_record(
    "ecxlin", simpleError("something went wrong")
  )
  expect_equal(plain$message, "something went wrong")
  expect_null(plain$prior)
  expect_null(plain$init)
  expect_match(bayesnec:::failure_record("ecxlin", NULL)$message,
               "unrecorded reason")
})

test_that("attach_failed_models leaves an object alone when nothing failed", {
  expect_identical(bayesnec:::attach_failed_models(manec_example, list()),
                   manec_example)
  fake <- bayesnec:::attach_failed_models(
    manec_example, list(ecxlin = bayesnec:::failure_record("ecxlin", NULL))
  )
  expect_s3_class(fake, "bayesmanecfit")
  expect_s3_class(fake, "bnecfit")
  expect_length(failed_models(fake), 1)
})

test_that("print.bnecfailures names each model and its error", {
  fake <- bayesnec:::attach_failed_models(
    manec_example,
    list(ecxlin = bayesnec:::failure_record(
           "ecxlin", simpleError("Initialization failed.")))
  )
  expect_output(print(failed_models(fake)), "1 model\\(s\\) failed to fit")
  expect_output(print(failed_models(fake)), "ecxlin")
  expect_output(print(failed_models(fake)), "Initialization failed")
  expect_output(print(failed_models(fake)), "\\$prior")
})

test_that("summary reports the models that failed", {
  fake <- bayesnec:::attach_failed_models(
    manec_example,
    list(ecxlin = bayesnec:::failure_record("ecxlin", NULL))
  )
  out <- capture.output(
    print(summary(fake)) |> suppressWarnings() |> suppressMessages()
  )
  expect_true(any(grepl("1 model\\(s\\) failed to fit: ecxlin", out)))
  expect_true(any(grepl("\\?failed_models", out)))
  # The single-model summary carries it too: where all but one model failed,
  # bnec returns a bayesnecfit and that is when the failures matter most.
  fake1 <- bayesnec:::attach_failed_models(
    nec4param, list(ecxlin = bayesnec:::failure_record("ecxlin", NULL))
  )
  out1 <- capture.output(
    print(summary(fake1)) |> suppressWarnings() |> suppressMessages()
  )
  expect_true(any(grepl("1 model\\(s\\) failed to fit: ecxlin", out1)))
})

test_that("failed_models splits the two components of a hurdle fit", {
  obj <- structure(
    list(growth = bayesnec:::attach_failed_models(
           manec_example,
           list(ecxlin = bayesnec:::failure_record("ecxlin", NULL))),
         survival = manec_example),
    class = c("bayesnechurdlefit", "bnecfit")
  )
  out <- failed_models(obj)
  expect_named(out, c("growth", "survival"))
  expect_length(out$growth, 1)
  expect_length(out$survival, 0)
})

test_that("the hurdle summary names the component a model failed on", {
  # Built from the pieces print.hurdlesummary reads, as in
  # test-bayesnechurdlefit-methods.R: the summary itself needs two real fits.
  ne_row <- c(Estimate = 1, Q2.5 = 0.5, Q97.5 = 1.5)
  hs <- structure(
    list(n_exposed = 10L, n_dead = 4L, growth_family = "beta",
         growth_models = c("nec4param", "ecx4param"),
         survival_models = "nec4param",
         ne = list(combined = ne_row, growth = ne_row, survival = ne_row),
         ne_types = c(combined = "NEC", growth = "NEC", survival = "NEC"),
         ecs = NULL, growth_averaged = TRUE, survival_averaged = FALSE,
         failed_models = list(
           growth = failed_models(bayesnec:::attach_failed_models(
             manec_example,
             list(ecxwb1 = bayesnec:::failure_record("ecxwb1", NULL)))),
           survival = failed_models(manec_example))),
    class = "hurdlesummary"
  )
  out <- capture.output(print(hs))
  # Labelled by component, because the two are independent fits and a model can
  # fail on one and not the other.
  expect_true(any(grepl("growth: 1 model\\(s\\) failed to fit: ecxwb1", out)))
  expect_false(any(grepl("survival: ", out, fixed = TRUE)))
  # One pointer line however many components failed.
  expect_equal(sum(grepl("\\?failed_models", out)), 1)
  # A hurdle fit where nothing failed says nothing at all.
  hs$failed_models <- list(growth = failed_models(manec_example),
                           survival = failed_models(manec_example))
  expect_false(any(grepl("failed to fit", capture.output(print(hs)))))
  # As does one summarised from an object fitted before the record existed.
  hs$failed_models <- NULL
  expect_false(any(grepl("failed to fit", capture.output(print(hs)))))
})

test_that("pull_out carries the record across", {
  fake <- bayesnec:::attach_failed_models(
    manec_example, list(ecxlin = bayesnec:::failure_record("ecxlin", NULL))
  )
  # Nothing is refitted here, only subset, so what bnec() attempted is still
  # an accurate description of the object being returned.
  single <- suppressMessages(suppressWarnings(
    pull_out(fake, model = "nec4param")
  ))
  expect_s3_class(single, "bayesnecfit")
  expect_named(failed_models(single), "ecxlin")
  # An object with no record is returned without one rather than erroring.
  clean <- suppressMessages(suppressWarnings(
    pull_out(manec_example, model = "nec4param")
  ))
  expect_length(failed_models(clean), 0)
  expect_null(clean[["failed_models"]])
})

test_that("a failed fit carries the priors and inits it was given", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # An invalid `algorithm` makes brm() error immediately, after add_brm_defaults
  # has built the priors and initial values -- exactly the situation the record
  # exists for, and without paying for a Stan compile to reach it.
  out <- try(
    fit_bayesnec(
      formula = bnf(y ~ crf(x, "nec3param")), data = nec_data,
      model = "nec3param",
      brm_args = list(family = gaussian(), iter = 200, warmup = 100,
                      chains = 2, refresh = 0, open_progress = FALSE,
                      algorithm = "notanalgorithm")
    ),
    silent = TRUE
  ) |>
    suppressMessages()
  expect_s3_class(out, "try-error")
  cond <- attr(out, "condition")
  expect_s3_class(cond, "bnec_fit_failure")
  expect_equal(cond$model, "nec3param")
  expect_s3_class(cond$prior, "brmsprior")
  expect_length(cond$init, 2)
})

test_that("a model that fails in a set is reported with its priors and inits", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  skip_if_not_installed("R.utils")
  # A tiny timeout is a deterministic, fast way to make one model fail: the two
  # models already in manec_example are not refitted, so the set still returns.
  out <- amend(manec_example, add = "ecxwb1", timeout = 1e-3) |>
    suppressMessages() |>
    suppressWarnings()
  expect_s3_class(out, "bayesmanecfit")
  expect_setequal(names(out$mod_fits), c("nec4param", "ecx4param"))
  failed <- failed_models(out)
  expect_named(failed, "ecxwb1")
  expect_equal(failed$ecxwb1$model, "ecxwb1")
  # Windows reports "CPU time limit" where Linux and macOS report "elapsed".
  expect_match(failed$ecxwb1$message, "time limit")
  # The whole point: the priors and initial values used are recoverable without
  # re-running the set, and the prior is usable as a `prior =` argument.
  expect_s3_class(failed$ecxwb1$prior, "brmsprior")
  expect_setequal(unique(failed$ecxwb1$prior$nlpar),
                  c("bot", "ec50", "top", "beta"))
  expect_length(failed$ecxwb1$init, extract_simdat(out$mod_fits[[1]])$chains)
  expect_output(print(failed), "ecxwb1")
})
