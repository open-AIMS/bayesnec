test_that("input checks work correctly and return appropriate messages", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  general_error <- paste(
    "Nothing to amend, please specify a proper model to either add or drop, or",
    "changes to loo_controls;\n Returning original model set."
  )
  m_1 <- paste0("Nothing to amend, please specify a model to either add or",
                " drop that differs from the original set")
  m_2 <- "Returning original model set."
  m_3 <- paste0("No new LOO fitting/weighting arguments have been specified;",
                " ignoring argument loo_controls.")
  expect_message(amend(manec_example), general_error)
  amend(manec_example, drop = "nec3param") |>
    expect_message(general_error) |>
    expect_message(m_1)
  m_4 <- "loo_controls list names are incorrect. See ?bnec"
  manec_example |>
    amend(loo_controls = list(method = "somethingwrong")) |>
    expect_message(m_4) |>
    expect_error()
  expect_message(amend(manec_example, drop = "nec4param")) |>
    suppressWarnings()
  amend(manec_example, add = "nec3param") |>
    expect_message() |>
    expect_message("Fitted models are: nec4param ecx4param") |>
    suppressWarnings()
  amend(manec_example, add = "nec4param",
        loo_controls = list(weights = list(method = "pseudobma"))) |>
    expect_message(general_error) |>
    expect_message(m_1) |>
    expect_message(m_3)
  amend(manec_example,
        loo_controls = list(weights = list(method = "pseudobma"))) |>
    expect_message(general_error) |>
    expect_message(m_3)
})

test_that("amend.bayesnecfit rejects drop and no-ops cleanly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # Dropping the only model would have to return an empty object, so it is an
  # error rather than a silent no-op.
  expect_error(amend(nec4param, drop = "nec4param"),
               "Cannot drop models from a bayesnecfit")
  expect_error(amend(nec4param, drop = "ecx4param"),
               "Cannot drop models from a bayesnecfit")
  general_error <- paste(
    "Nothing to amend, please specify a proper model to either add or drop, or",
    "changes to loo_controls;\n Returning original model set."
  )
  expect_message(amend(nec4param), general_error)
  expect_identical(class(suppressMessages(amend(nec4param))),
                   c("bayesnecfit", "bnecfit"))
  expect_error(amend(nec4param, add = "ecx4param", timeout = -1),
               "must be a positive number")
})

test_that("amend.bayesnecfit adds models and promotes to bayesmanecfit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  added <- amend(nec4param, add = "ecx4param") |>
    suppressMessages() |>
    suppressWarnings()
  expect_s3_class(added, "bayesmanecfit")
  expect_setequal(names(added$mod_fits), c("nec4param", "ecx4param"))
  expect_setequal(added$success_models, c("nec4param", "ecx4param"))
  # The pre-existing fit must be carried over rather than refitted; identical
  # draws is the check that matters, since the point of amend() is not paying
  # for the original fit twice.
  expect_identical(fixef(added$mod_fits$nec4param$fit),
                   fixef(nec4param$fit))
})
