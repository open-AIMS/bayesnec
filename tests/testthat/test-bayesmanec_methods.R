test_that("predict is silent", {
  expect_silent(predict(manec_example))
})

test_that("predict/fitted is a matrix of appropriately name elements", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  pred_p <- predict(manec_example)
  expect_equal(class(pred_p), c("matrix", "array"))
  expect_equal(dim(pred_p), c(100, 4))
  expect_equal(colnames(pred_p), c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
  fitt_p <- fitted(manec_example)
  expect_equal(class(fitt_p), c("matrix", "array"))
  expect_equal(dim(fitt_p), c(100, 4))
  expect_equal(colnames(fitt_p), c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
})

test_that("plot returns null, is invisible, and is silent", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  expect_null(plot(manec_example))
  expect_silent(plot(manec_example))
  expect_invisible(plot(manec_example))
})

test_that("rhat behaves as expected", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  rhat_p <- suppressMessages(rhat(manec_example))
  rhat2_p <-  rhat(manec_example, rhat_cutoff = 1)
  expect_message(rhat(manec_example, rhat_cutoff = 1))
  expect_equal(names(rhat2_p), manec_example$success_models)
  expect_equal(names(rhat_p[[1]]), c("rhat_vals", "failed"))
  # The prior_* draws are not reported, and the verdict is not taken over them
  expect_false(any(grepl("^prior_", names(rhat_p[[1]]$rhat_vals))))
  expect_false(anyNA(rhat_p[[1]]$rhat_vals))
  # `failed` is read as a logical by summary() and screen_models()
  expect_type(rhat_p[[1]]$failed, "logical")
  expect_false(anyNA(vapply(rhat_p, "[[", logical(1), "failed")))
})

test_that("summary behaves as expected", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  summary.p <- suppressWarnings(summary(manec_example))
  expect_equal(class(summary.p), "manecsummary")
  # `rhat_cutoff` added under #148 Part D: print.manecsummary reports the
  # threshold actually in use rather than the hard-coded 1.05 it printed
  # before, so the summary object has to carry it.
  expect_equal(names(summary.p), c("models", "family", "sample_size",
                                   "mod_weights", "mod_weights_method",
                                   "ecx_mods", "nec_vals", "ecs", "bayesr2",
                                   "rhat_issues", "rhat_cutoff",
                                   "failed_models"))
  expect_equal(summary.p$rhat_cutoff, 1.01)
  # and it is honoured rather than decorative
  expect_equal(suppressWarnings(summary(manec_example,
                                        rhat_cutoff = 1.5))$rhat_cutoff, 1.5)
  # The verdict itself is computed, not stored. This is the assertion that
  # catches a regression to has_r_hat_warnings(), which grepped brms's warning
  # text for the literal "some Rhats are > 1.05" and so returned the same
  # answer whatever cutoff was asked for.
  expect_true(all(unlist(summary.p$rhat_issues)))
  loose <- suppressWarnings(summary(manec_example, rhat_cutoff = 1.5))
  expect_false(any(unlist(loose$rhat_issues)))
})

test_that("print.manecsummary names models, never NA", {
  # An older stored manecsummary can carry an NA rhat_issues, from before
  # zero-variance parameters were excluded. Logical indexing turns that into an
  # element named NA, and the warning then reads "- NA" as though a model
  # called NA had failed.
  skip_on_cran()
  s <- suppressWarnings(summary(manec_example))
  s$rhat_issues <- list(nec4param = NA, ecx4param = TRUE)
  msg <- tryCatch(print(s), warning = function(w) conditionMessage(w))
  expect_match(msg, "ecx4param")
  expect_false(grepl("-  NA", msg, fixed = TRUE))
})

test_that("print.manecsummary falls back to 1.05 for an object with no cutoff", {
  # rhat_cutoff post-dates the move to 1.01, so an object without it was
  # assessed against the old 1.05 grep; reporting 1.01 would attribute a
  # threshold to it that was never applied.
  skip_on_cran()
  s <- suppressWarnings(summary(manec_example))
  s$rhat_cutoff <- NULL
  msg <- tryCatch(print(s), warning = function(w) conditionMessage(w))
  expect_match(msg, "Rhats > 1.05")
})

test_that("the summary warning points at screen_models", {
  skip_on_cran()
  s <- suppressWarnings(summary(manec_example))
  msg <- tryCatch(print(s), warning = function(w) conditionMessage(w))
  expect_match(msg, "screen_models", fixed = TRUE)
})

test_that("formula behaves as expected", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  expect_error(formula(manec_example))
  expect_s3_class(formula(manec_example, "nec4param"), "bayesnecformula")
  expect_s3_class(formula(manec_example, model = "nec4param"),
                          "bayesnecformula")
  expect_s3_class(formula(manec_example, model = "ecx4param"),
                          "bayesnecformula")
  expect_null(formula(manec_example, model = "ecxlin"))
})

test_that("model.frame behaves as expected", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  expect_error(model.frame(manec_example))
  expect_s3_class(model.frame(manec_example, "nec4param"),
                  "data.frame")
  expect_s3_class(model.frame(manec_example, model = "nec4param"),
                  "data.frame")
  expect_s3_class(model.frame(manec_example, model = "ecx4param"),
                  "data.frame")
  expect_error(model.frame(manec_example, model = "ecxlin"))
})

test_that("model-averaged output does not change between identical calls", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # #216: every quantity taken through the averaged posterior used to redraw the
  # component index with an unseeded sample(), so no two calls agreed. Asserted
  # on the expectation and on the summaries built from it, not on stored
  # constants -- the point is that repeated calls agree, whatever the values.
  nd <- bnec_newdata(manec_example, resolution = 20)
  expect_identical(posterior_epred(manec_example, newdata = nd),
                   posterior_epred(manec_example, newdata = nd))
  expect_identical(fitted(manec_example, newdata = nd),
                   fitted(manec_example, newdata = nd))
  # Edge case: a single grid column. Asserted on the *shape* as well as on
  # agreement between calls -- two equally malformed results compare identical,
  # so reproducibility alone would not have caught the dropped dimension this
  # originally hid (see w_pred_list_calc).
  nd_1 <- bnec_newdata(manec_example, resolution = 1)
  expect_identical(posterior_epred(manec_example, newdata = nd_1),
                   posterior_epred(manec_example, newdata = nd_1))
  expect_equal(dim(posterior_epred(manec_example, newdata = nd_1)),
               c(manec_example$sample_size, 1))
  # The way a user actually meets that case: one row of newdata, asking what the
  # averaged curve predicts at a single concentration. The answer must have the
  # same shape as the single-model one, and must not warn.
  nd_pt <- data.frame(x = 3)
  expect_silent(ma_pt <- posterior_epred(manec_example, newdata = nd_pt))
  expect_equal(dim(ma_pt), c(manec_example$sample_size, 1))
  expect_identical(dim(fitted(manec_example, newdata = nd_pt)),
                   dim(fitted(nec4param, newdata = nd_pt)))
  # ... and the averaged estimate must sit between the components it averages.
  ma_est <- fitted(manec_example, newdata = nd_pt)[1, "Estimate"]
  cmp_est <- sapply(names(manec_example$mod_fits), function(m) {
    fitted(suppressMessages(pull_out(manec_example, model = m)),
           newdata = nd_pt)[1, "Estimate"]
  })
  expect_gte(ma_est, min(cmp_est))
  expect_lte(ma_est, max(cmp_est))
  # posterior_predict() stays stochastic on purpose: it simulates new
  # observations, exactly as brms does for a single fit, so it varies for a
  # bayesnecfit too. What #216 fixes is the weighting, which is now the same
  # draw every time -- so seeding the likelihood is enough to pin it down.
  set.seed(216)
  p1 <- predict(manec_example, newdata = nd)
  set.seed(216)
  expect_identical(p1, predict(manec_example, newdata = nd))
})

test_that("model-averaged ecx and nsec are reproducible and internally paired", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # These are the numbers that go into a report, and the instability landed on
  # the lower bound -- the end a protective concentration is read off. See #216.
  expect_identical(suppressMessages(ecx(manec_example)),
                   suppressMessages(ecx(manec_example)))
  post <- suppressMessages(nsec(manec_example, posterior = TRUE))
  expect_identical(post, suppressMessages(nsec(manec_example,
                                               posterior = TRUE)))
  # The NSEC and its ecnsec used to be drawn by two independent sample() calls,
  # so a pair was two unrelated draws of possibly different models. One index
  # now covers both: element i of each is draw idx[i] of the same component.
  idx <- bayesnec:::pull_draw_index(manec_example,
                                    manec_example$success_models,
                                    manec_example$sample_size)
  parts <- lapply(manec_example$success_models, function(mod) {
    single <- pull_out(manec_example, model = mod) |>
      suppressMessages() |>
      suppressWarnings()
    one <- suppressMessages(nsec(single, posterior = TRUE))
    cbind(one[idx[[mod]]], attributes(one)$ecnsec_relativeP[idx[[mod]]])
  })
  parts <- do.call("rbind", parts)
  expect_equal(as.numeric(post), unname(parts[, 1]))
  expect_equal(as.numeric(attributes(post)$ecnsec_relativeP),
               unname(parts[, 2]))
})
