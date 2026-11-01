test_that("amend honours prediction settings without a model or LOO change", {
  set.seed(525)
  local_mocked_bindings(
    fit_bayesnec = function(...) stop("No equation should be refitted"),
    .package = "bayesnec"
  )
  q <- function(expr) suppressWarnings(suppressMessages(expr))
  record <- expand_record()
  reference <- expanded_ecx4param(record)
  for (fit in list(ecx4param, manec_example)) {
    for (loo in list(NULL, list(weights = list(method = "pseudobma")))) {
      args <- c(list(fit, x_range = record$x_range,
                     resolution = record$resolution, sig_val = record$sig_val),
                if (!is.null(loo)) list(loo_controls = loo))
      out <- q(do.call(amend, args))
      expected <- record
      expected$loo_controls$fitting <- list()
      # manec_example records no leave_out, so it was scored by observation.
      if (inherits(fit, "bayesmanecfit")) {
        expected$loo_controls$leave_out <- "observation"
      }
      expect_identical(attr(out, "bnec_expand_args"), expected)
      grid <- if (inherits(out, "bayesnecfit")) {
        out$pred_vals$data
      } else {
        out$w_pred_vals$data
      }
      expect_equal(nrow(grid), record$resolution)
      expect_equal(range(grid$x), record$x_range)
      if (inherits(out, "bayesnecfit")) {
        expect_equal(out$ne_posterior, reference$ne_posterior)
        expect_identical(brms::fixef(out$fit), brms::fixef(fit$fit))
      }
    }
  }
})

test_that("c and plus join records when all inputs fit the same equation", {
  set.seed(525)
  q <- function(expr) suppressWarnings(suppressMessages(expr))
  a <- bayesnec:::attach_bnec_record(
    with_expand_record(ecx4param),
    requested = c("ecx4param", "nechorme4"),
    attempted = "ecx4param",
    excluded = data.frame(model = "nechorme4", reason = "declined"),
    substitutions = NULL
  )
  b <- bayesnec:::attach_bnec_record(
    with_expand_record(ecx4param),
    requested = c("ecx4param", "ecxsigm"),
    attempted = c("ecx4param", "ecxsigm"),
    excluded = NULL, substitutions = NULL
  )
  b <- bayesnec:::attach_failed_models(
    b, list(ecxsigm = list(model = "ecxsigm", message = "failed"),
            ecx4param = list(model = "ecx4param", message = "old failure"))
  )
  reference <- expanded_ecx4param(expand_record())
  for (out in list(q(c(a, b)), q(a + b))) {
    expect_s3_class(out, "bayesnecfit")
    expect_setequal(bnec_record(out)$requested,
                    c("ecx4param", "nechorme4", "ecxsigm"))
    expect_setequal(bnec_record(out)$attempted, c("ecx4param", "ecxsigm"))
    expect_equal(bnec_record(out)$excluded$model, "nechorme4")
    expect_identical(names(failed_models(out)), "ecxsigm")
    expect_identical(attr(out, "bnec_expand_args"), expand_record())
    expect_equal(out$pred_vals, reference$pred_vals)
    expect_equal(out$ne_posterior, reference$ne_posterior)
    expect_identical(brms::fixef(out$fit), brms::fixef(a$fit))
  }
  expect_null(bnec_record(q(c(a, ecx4param))))
  expect_null(bnec_record(q(c(ecx4param, a))))
})

test_that("redundant model edits do not suppress changed settings", {
  set.seed(525)
  q <- function(expr) suppressWarnings(suppressMessages(expr))
  for (out in list(
    q(amend(ecx4param, add = "ecx4param", resolution = 250)),
    q(amend(manec_example, drop = "ecxsigm", resolution = 250))
  )) {
    expect_equal(attr(out, "bnec_expand_args")$resolution, 250)
    grid <- if (inherits(out, "bayesnecfit")) {
      out$pred_vals$data
    } else {
      out$w_pred_vals$data
    }
    expect_equal(nrow(grid), 250)
  }
  out <- q(amend(ecx4param, add = "ecx4param",
                  loo_controls = list(fitting = list(draw_ids = 1:50))))
  expect_equal(attr(out, "bnec_expand_args")$loo_controls$fitting,
               list(draw_ids = 1:50))
  expect_equal(out$fit$criteria$waic$estimates,
               expanded_ecx4param(expand_record())$fit$criteria$waic$estimates)
  expect_identical(q(amend(ecx4param, add = "ecx4param")), ecx4param)
})

test_that("pull_out honours settings when the requested group keeps every model", {
  set.seed(525)
  q <- function(expr) suppressWarnings(suppressMessages(expr))
  m <- with_expand_record(manec_example)
  expect_identical(q(pull_out(m, "all")), m)
  out <- q(pull_out(m, "all", resolution = 250,
                    loo_controls = list(fitting = list())))
  expect_identical(attr(out, "bnec_expand_args"),
                   expand_record(resolution = 250, fitting = list()))
  expect_equal(nrow(out$w_pred_vals$data), 250)
  expect_equal(out$mod_stats$waic, manec_example$mod_stats$waic)
})

test_that("pull_out keeps positional and abbreviated prediction arguments", {
  set.seed(525)
  q <- function(expr) suppressWarnings(suppressMessages(expr))
  reference <- expanded_ecx4param(expand_record(fitting = list()))
  positional <- q(pull_out(manec_example, "ecx4param", ,
                            c(0.03, 4), 200, 0.05))
  abbreviated <- q(pull_out(manec_example, "ecx4param", x_r = c(0.03, 4),
                             res = 200, sig = 0.05))
  for (out in list(positional, abbreviated)) {
    expect_equal(out$pred_vals, reference$pred_vals)
    expect_equal(out$ne_posterior, reference$ne_posterior)
    # manec_example records no leave_out, so it was scored by observation.
    expect_identical(attr(out, "bnec_expand_args"),
                     expand_record(fitting = list(), leave_out = "observation"))
  }
})
