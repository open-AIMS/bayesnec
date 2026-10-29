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

test_that("predict() with neither model nor average is unchanged (#120)", {
  skip_on_cran()
  # Build the expected summary independently from posterior_predict(). Seed
  # the likelihood simulation so comparison with predict() is reproducible.
  nd <- bnec_newdata(manec_example, resolution = 20)
  set.seed(120)
  expected <- t(apply(posterior_predict(manec_example, newdata = nd), 2,
                      brms::posterior_summary, robust = FALSE,
                      probs = c(0.025, 0.975)))
  colnames(expected) <- c("Estimate", "Est.Error", "Q2.5", "Q97.5")
  set.seed(120)
  expect_identical(predict(manec_example, newdata = nd), expected)
  set.seed(120)
  expect_identical(predict(manec_example, newdata = nd, model = NULL,
                           average = TRUE), expected)
  set.seed(120)
  draws <- posterior_predict(manec_example, newdata = nd)
  set.seed(120)
  expect_identical(predict(manec_example, newdata = nd, summary = FALSE),
                   draws)
})

test_that("predict() returns a named list where model is given", {
  skip_on_cran()
  nd <- bnec_newdata(manec_example, resolution = 20)
  set.seed(121)
  avg <- predict(manec_example, newdata = nd)
  set.seed(121)
  both <- predict(manec_example, newdata = nd, model = "nec4param")
  expect_type(both, "list")
  expect_named(both, c("average", "nec4param"))
  # The model average comes first and is computed first, so for the same seed
  # it is what model = NULL returns.
  expect_identical(both$average, avg)
  # An equation's element is predict() on the equation pulled out of the set.
  set.seed(122)
  alone <- predict(manec_example, newdata = nd, model = "nec4param",
                   average = FALSE)
  expect_named(alone, "nec4param")
  set.seed(122)
  expect_identical(alone$nec4param, predict(nec4param, newdata = nd))
  # The equations follow in the order given, a repeated name once.
  expect_named(
    predict(manec_example, newdata = nd,
            model = c("ecx4param", "nec4param", "ecx4param")),
    c("average", "ecx4param", "nec4param")
  )
  # summary and probs reach every element.
  draws <- predict(manec_example, newdata = nd, model = "ecx4param",
                   summary = FALSE)
  expect_equal(dim(draws$average), c(manec_example$sample_size, nrow(nd)))
  expect_equal(dim(draws$ecx4param),
               c(brms::ndraws(pull_brmsfit(ecx4param)), nrow(nd)))
  q <- predict(manec_example, newdata = nd, model = "ecx4param",
               probs = c(0.1, 0.9))
  expect_identical(colnames(q$average), c("Estimate", "Est.Error", "Q10", "Q90"))
  expect_identical(colnames(q$ecx4param),
                   c("Estimate", "Est.Error", "Q10", "Q90"))
})

test_that("predict() ignores all_models, with a warning (#120)", {
  # predict() warns on all_models but ignores its value. Check that the
  # warning does not change the returned prediction.
  skip_on_cran()
  nd <- bnec_newdata(manec_example, resolution = 20)
  set.seed(123)
  plain <- predict(manec_example, newdata = nd)
  set.seed(123)
  old <- collect_warnings(predict(manec_example, newdata = nd,
                                  all_models = TRUE))
  expect_length(old$warnings, 1)
  expect_match(old$warnings, "is ignored", fixed = TRUE)
  expect_identical(old$value, plain)
  # Beside model it changes nothing either.
  set.seed(124)
  listed <- predict(manec_example, newdata = nd, model = "nec4param")
  set.seed(124)
  expect_identical(
    suppressWarnings(predict(manec_example, newdata = nd, model = "nec4param",
                             all_models = TRUE)),
    listed
  )
})

test_that("predict() refuses a model outside the set and an empty selection", {
  skip_on_cran()
  expect_error(predict(manec_example, model = "nope"),
               "not in this set: \"nope\"", fixed = TRUE)
  expect_error(predict(manec_example, average = FALSE), "Nothing is selected")
  expect_error(predict(manec_example, model = NA_character_),
               "must be NULL or a character vector")
  expect_error(predict(manec_example, average = "yes"), "flag")
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
  # Store both diagnostic verdicts and their actual thresholds so printing
  # can describe the sampler and control-fit checks consistently.
  expect_equal(names(summary.p), c("models", "family", "sample_size",
                                   "mod_weights", "mod_weights_method",
                                   "ecx_mods", "nec_vals", "ecs", "bayesr2",
                                   "rhat_issues", "rhat_cutoff",
                                   "divergences", "divergence_cutoff",
                                   "fit_issues", "fit_ratio_cutoff",
                                   "failed_models", "bnec_version"))
  expect_equal(summary.p$rhat_cutoff, 1.01)
  # and it is honoured rather than decorative
  expect_equal(suppressWarnings(summary(manec_example,
                                        rhat_cutoff = 1.5))$rhat_cutoff, 1.5)
  # Recompute the verdict from the requested cutoff; stored warning text
  # cannot detect a changed cutoff.
  expect_true(all(unlist(summary.p$rhat_issues)))
  loose <- suppressWarnings(summary(manec_example, rhat_cutoff = 1.5))
  expect_false(any(unlist(loose$rhat_issues)))
})

test_that("print.manecsummary names models, never NA", {
  # An NA diagnostic in an older summary must not become a fictitious model
  # named NA through logical indexing.
  skip_on_cran()
  s <- suppressWarnings(summary(manec_example))
  s$rhat_issues <- list(nec4param = NA, ecx4param = TRUE)
  msg <- tryCatch(print(s), warning = function(w) conditionMessage(w))
  expect_match(msg, "ecx4param")
  expect_false(grepl("-  NA", msg, fixed = TRUE))
})

test_that("print.manecsummary falls back to 1.05 for an object with no cutoff", {
  # A summary without rhat_cutoff predates the recorded threshold and was
  # assessed against 1.05. Printing 1.01 would misstate that assessment.
  skip_on_cran()
  s <- suppressWarnings(summary(manec_example))
  s$rhat_cutoff <- NULL
  msg <- tryCatch(print(s), warning = function(w) conditionMessage(w))
  expect_match(msg, "Rhat > 1.05")
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
  # Check reproducibility of expectations and derived summaries, rather than
  # fixed numerical values, across model-average calls.
  nd <- bnec_newdata(manec_example, resolution = 20)
  expect_identical(posterior_epred(manec_example, newdata = nd),
                   posterior_epred(manec_example, newdata = nd))
  expect_identical(fitted(manec_example, newdata = nd),
                   fitted(manec_example, newdata = nd))
  # Check one-column shape as well as reproducibility: two equally malformed
  # results would still compare identical.
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
  # posterior_predict() simulates new observations and remains stochastic.
  # Its model-index draw is reproducible, so seeding the likelihood is enough
  # to reproduce predictions.
  set.seed(216)
  p1 <- predict(manec_example, newdata = nd)
  set.seed(216)
  expect_identical(p1, predict(manec_example, newdata = nd))
})

test_that("model-averaged ecx and nsec are reproducible and internally paired", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # Check interval bounds as well as the point estimate: protective
  # concentrations can be read from the lower bound.
  expect_identical(suppressMessages(ecx(manec_example)),
                   suppressMessages(ecx(manec_example)))
  post <- suppressMessages(nsec(manec_example, posterior = TRUE))
  expect_identical(post, suppressMessages(nsec(manec_example,
                                               posterior = TRUE)))
  # Pair NSEC and ECNSEC by the same draw index and component, rather than
  # independent draws that can come from different models.
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
