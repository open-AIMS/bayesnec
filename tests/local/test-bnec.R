fit1 <- pull_brmsfit(manec_gausian_identity, "nec4param")
fit2 <- pull_brmsfit(manec_gausian_identity, "ecx4param")
  
test_that("models fit correctly", {
  expect_message(pull_out(manec_gausian_identity, "nec4param"))
})

test_that("ecx works", {
  ec10 <- ecx(manec_gausian_identity, type = "relative")
  ec90 <- ecx(manec_gausian_identity, ecx_val = 90, type = "relative")
  expect_gt(ec90[1], ec10[1])
  expect_range(ec10[1], 1.38, 1.64)
  expect_range(ec90[1], 2.93, 2.97)
})

test_that("gaussian model with identity works correctly", {
  expect_true(grepl("gaussian", summary(manec_gausian_identity)$family$family))
  expect_equal(fit1$family$link, "identity")
  
  expect_range(fixef(fit1)["bot_Intercept", 1], -12, -5)
  expect_range(fixef(fit1)["top_Intercept", 1], 2.1, 2.2)
  expect_range(fixef(fit1)["beta_Intercept", 1], -0.9, -0.1)
  expect_range(fixef(fit1)["nec_Intercept", 1], 1.3, 1.6)
  expect_warning(expect_range(waic(fit1)$estimates[3, 1], 167, 180))
  
  expect_range(fixef(fit2)["bot_Intercept", 1], -6.6, -5.0)
  expect_range(fixef(fit2)["ec50_Intercept", 1], 2.3, 2.5)
  expect_range(fixef(fit2)["top_Intercept", 1], 2.1, 2.4)
  expect_range(fixef(fit2)["beta_Intercept", 1], 0.8, 1.0)
  expect_warning(expect_range(waic(fit2)$estimates[3, 1], 175, 215))
})

test_that("all model families return expected ecx values", {
  lapply(manec_fits, function(x) {
    expect_range(ecx(x, type = "relative")[1], 1.3, 1.64)
  })
  lapply(manec_fits[-1], function(x) {
    expect_range(ecx(x, ecx_val = 90, type = "relative")[1], 1.9, 2.97)
  })
})

test_that("all model families return expected nec values", {
  lapply(manec_fits, function(x) {
    expect_range(x$w_nec["Estimate"], 1.4, 1.6)
  })
})

test_that("all model families return expected weights", {
  lapply(manec_fits, function(x) {
    expect_range(x$mod_stats[1,"wi"], 0.7, 1)
  })
})

test_that("all model families return expected class", {
  lapply(nec_fits, function(x) expect_s3_class(x, "bayesnecfit"))
  lapply(manec_fits, function(x) expect_s3_class(x, "bayesmanecfit"))
})

test_that("beta model returns expected family and link", {
  expect_true(grepl("beta", summary(manec_beta_identity)$family$family))
  expect_true(grepl("beta", summary(manec_beta_logit)$family$family))
  expect_equal(manec_beta_identity$mod_fits$nec4param$fit$family$link,
               "identity")
  expect_equal(manec_beta_logit$mod_fits$nec4param$fit$family$link, "logit")
})

test_that("binomial model returns expected family and link", {
  expect_true(grepl("binomial", summary(manec_binomial_identity)$family$family))
  expect_true(grepl("binomial", summary(manec_binomial_logit)$family$family))
  expect_equal(manec_binomial_identity$mod_fits$nec4param$fit$family$link,
               "identity")
  expect_equal(manec_binomial_logit$mod_fits$nec4param$fit$family$link, "logit")
})
  
test_that("poisson model returns expected family and link", {
  expect_true(grepl("poisson", summary(manec_poisson_identity)$family$family))
  expect_true(grepl("poisson", summary(manec_poisson_log)$family$family))
  expect_equal(manec_poisson_identity$mod_fits$nec4param$fit$family$link,
               "identity")
  expect_equal(manec_poisson_log$mod_fits$nec4param$fit$family$link, "log")
})

test_that("negbinomial model returns expected family and link", {
  expect_true(grepl("negbinomial",
                    summary(manec_negbinomial_identity)$family$family))
  expect_true(grepl("negbinomial",
                    summary(manec_negbinomial_log)$family$family))
  expect_equal(manec_negbinomial_identity$mod_fits$nec4param$fit$family$link,
               "identity")
  expect_equal(manec_negbinomial_log$mod_fits$nec4param$fit$family$link, "log")
})

test_that("gamma model returns expected family and link", {
  expect_true(grepl("gamma", summary(manec_gamma_identity)$family$family))
  expect_true(grepl("gamma", summary(manec_gamma_log)$family$family))
  expect_equal(manec_gamma_identity$mod_fits$nec4param$fit$family$link,
               "identity")
  expect_equal(manec_gamma_log$mod_fits$nec4param$fit$family$link, "log")
})

test_that("bnec takes model formula input and provides warning for unnecessary arguments", {
  m1 <- "Arguments x_var, y_var, trials_var, model, random and random_vars"
  bnec(y ~ crf(x, "ecxlin"), data = nec_data, x_var = "x") |>
    expect_warning(m1) |>
    expect_message() |>
    expect_error()
  bnec(y ~ crf(x, "ecxlin"), data = nec_data, model = "ecxlin") |>
    expect_warning(m1) |>
    expect_message() |>
    expect_error()
})
