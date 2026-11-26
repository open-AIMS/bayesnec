test_that("fails without model argument", {
  expect_error(pull_out(manec_example))
})

test_that("rejects wrong model names/structure, returns original", {
  msg <- "non-existent in current set of models"
  pull_out(manec_example, model = "wrong") |>
    expect_equal(manec_example) |>
    expect_message(msg)
  pull_out(manec_example, model = NA) |>
    expect_equal(manec_example) |>
    expect_message(msg)
  pull_out(manec_example, model = c("nec4param", "ecx4param")) |>
    expect_error()
})

test_that("pull_out() computes the estimates with the arguments the set records", {
  q <- function(expr) suppressWarnings(suppressMessages(expr))
  recorded <- expanded_ecx4param(expand_record())
  defaults <- expanded_ecx4param(default_expand_record())
  # The two records give different estimates, so the comparisons below can
  # tell which one pull_out() used.
  expect_false(isTRUE(all.equal(recorded$ne_posterior,
                                defaults$ne_posterior)))
  m <- with_expand_record(manec_example)
  out <- q(pull_out(m, "ecx4param"))
  expect_equal(out$ne_posterior, recorded$ne_posterior)
  expect_equal(out$pred_vals, recorded$pred_vals)
  expect_equal(out$fit$criteria$waic$estimates,
               recorded$fit$criteria$waic$estimates)
  expect_identical(attr(out, "bnec_expand_args"), expand_record())
  # Arguments passed through `...`, and a "fitting" list, replace the record.
  out <- q(pull_out(m, "ecx4param", x_range = NA, resolution = 1000,
                    sig_val = 0.01, loo_controls = list(fitting = list())))
  expect_equal(out$ne_posterior, defaults$ne_posterior)
  expect_identical(attr(out, "bnec_expand_args"), default_expand_record())
  # One value supplied leaves the others to the record.
  out <- q(pull_out(m, "ecx4param", sig_val = 0.01))
  expect_identical(attr(out, "bnec_expand_args"),
                   expand_record(sig_val = 0.01))
  # An object that records none takes the bnec() defaults, and records them.
  # A model set that records no leave_out was scored by observation.
  out <- q(pull_out(legacy_fit(manec_example), "ecx4param"))
  expect_equal(out$ne_posterior, defaults$ne_posterior)
  expect_identical(attr(out, "bnec_expand_args"),
                   default_expand_record(leave_out = "observation"))
})

test_that("pull_out() refuses an argument it does not use", {
  expect_error(pull_out(manec_example, "nec4param", foo = 1),
               "pull_out\\(\\) takes no further arguments, so `foo`")
  expect_error(pull_out(manec_example, "nec4param", , , , , 1),
               "pull_out\\(\\) takes no further arguments\\.")
})

test_that("pull_out() says which loo_controls elements it ignores", {
  q <- function(expr) suppressWarnings(expr)
  msgs <- function(expr) {
    out <- character(0)
    withCallingHandlers(q(expr), message = function(m) {
      out <<- c(out, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
    out
  }
  ignored <- msgs(pull_out(manec_example, "ecx4param",
                           loo_controls = list(leave_out = "observation",
                                               refit = TRUE)))
  expect_true(any(grepl(
    "loo_controls$leave_out, loo_controls$refit are ignored here", ignored,
    fixed = TRUE
  )))
  weights <- msgs(pull_out(manec_example, "ecx4param",
                           loo_controls = list(weights = list(BB = FALSE))))
  expect_true(any(grepl("loo_controls$weights is ignored here. Use ?amend",
                        weights, fixed = TRUE)))
  fitting <- msgs(pull_out(manec_example, "ecx4param",
                           loo_controls = list(fitting = list())))
  expect_false(any(grepl("ignored here", fitting)))
})

test_that("pull_out() refuses a sig_val outside (0, 1) before rebuilding", {
  expect_error(pull_out(manec_example, "ecx4param", sig_val = 2), "sig_val")
})

test_that("pull_out refuses a joint refit", {
  expect_error(pull_out(joint_split(), model = "nec4param"),
               "pull_out() is not defined for a bayesnecjointfit",
               fixed = TRUE)
  expect_error(pull_out(joint_split(), model = "nec4param"),
               "no equation or level can be taken out of it", fixed = TRUE)
})
