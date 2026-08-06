bub_data <- function(n_conc = 8, reps = 5, top = 0.8, u = 1, phi = 30,
                     seed = 173) {
  set.seed(seed)
  x <- rep(seq(0, 6, length.out = n_conc), each = reps)
  mu <- top * exp(-exp(0) * pmax(x - 2, 0))
  data.frame(x = x, y = u * rbeta(length(mu), (mu / u) * phi,
                                  (1 - mu / u) * phi))
}

bub_bdat <- function(d) {
  model.frame(bayesnecformula(y ~ crf(x, "nec3param")), data = d,
              run_par_checks = TRUE)
}

# ---- Phase 1: the family object -------------------------------------------

test_that("beta_ub() returns a well-formed brms custom family", {
  f <- beta_ub()
  expect_s3_class(f, "customfamily")
  expect_identical(f$name, "beta_ub")
  expect_identical(f$dpars, c("mu", "phi", "delta"))
  # brms unpacks `links` into one field per dpar, mu's being `link`
  expect_identical(f$link, "identity")
  expect_identical(f$link_phi, "log")
  expect_identical(f$link_delta, "log")
  expect_identical(f$type, "real")
  expect_identical(f$vars, "ymax")
  expect_true(f$loop)
  # methods are attached to the family object rather than looked up by name,
  # so they resolve from inside the package namespace
  expect_true(is.function(f$log_lik))
  expect_true(is.function(f$posterior_predict))
  expect_true(is.function(f$posterior_epred))
})

test_that("beta_ub() refuses a non-identity link", {
  expect_error(beta_ub(link = "log"), "requires link = \"identity\"")
  expect_error(beta_ub(link = "logit"), "requires link = \"identity\"")
})

test_that("beta_ub_stanvars() carries the functions and ymax", {
  sv <- beta_ub_stanvars(1.5)
  expect_s3_class(sv, "stanvars")
  scode <- paste(vapply(sv, function(z) z$scode, character(1)), collapse = "\n")
  expect_match(scode, "beta_ub_lpdf")
  expect_match(scode, "beta_ub_rng")
  expect_match(scode, "real<lower=0> ymax;")
  expect_equal(sv$ymax$sdata, 1.5)
})

test_that("the Stan lpdf guards all three ways the shapes leave (0, Inf)", {
  scode <- beta_ub_stanvars(1)$beta_ub_lpdf
  scode <- paste(vapply(beta_ub_stanvars(1), function(z) z$scode,
                        character(1)), collapse = "\n")
  # mu >= U is the ceiling violation; the other two are underflow, which the
  # Phase 0 study found to be the more common rejection
  expect_match(scode, "mu >= U")
  expect_match(scode, "m \\* phi <= 0")
  expect_match(scode, "\\(1 - m\\) \\* phi <= 0")
  expect_match(scode, "- log\\(U\\)")   # Jacobian for z = y / U
})

# ---- Phase 2: recognition --------------------------------------------------

test_that("family_tag() resolves custom families to their name", {
  expect_identical(family_tag(beta_ub()), "beta_ub")
  expect_identical(family_tag(Gamma(link = "identity")), "Gamma")
  expect_identical(family_tag("hurdle_gamma"), "hurdle_gamma")
  # brms' own view, which is why family_tag() has to exist
  expect_identical(beta_ub()$family, "custom")
})

test_that("is_beta_ub_family() recognises the family and nothing else", {
  expect_true(is_beta_ub_family(beta_ub()))
  expect_true(is_beta_ub_family("beta_ub"))
  expect_false(is_beta_ub_family(Gamma(link = "identity")))
  expect_false(is_beta_ub_family(Beta(link = "identity")))
  expect_false(is_beta_ub_family("hurdle_gamma"))
  # and it must not be mistaken for a hurdle family
  expect_false(is_hurdle_family(beta_ub()))
})

test_that("validate_family() accepts beta_ub as a string and as a family", {
  expect_s3_class(validate_family("beta_ub"), "customfamily")
  expect_s3_class(validate_family(beta_ub()), "customfamily")
  expect_s3_class(validate_family(beta_ub), "customfamily")
  expect_identical(validate_family("beta_ub")$name, "beta_ub")
})

test_that("beta_ub is never guessed by set_distribution()", {
  d <- bub_data()
  # a positive continuous response stays Gamma; the ceiling prior is something
  # only the user can supply, so the family must be asked for explicitly
  expect_identical(set_distribution(d$y * 10, silence_y_msgs = TRUE), "Gamma")
  expect_identical(set_distribution(d$y, silence_y_msgs = TRUE), "Beta")
})

# ---- Phase 2: data checks --------------------------------------------------

test_that("check_data() records ymax for beta_ub and NULL otherwise", {
  d <- bub_data()
  out <- check_data(bub_bdat(d), family = beta_ub(), model = "nec3param")
  expect_equal(out$ymax, max(d$y))
  out_g <- check_data(bub_bdat(d), family = Gamma(link = "identity"),
                      model = "nec3param")
  expect_null(out_g$ymax)
})

test_that("check_data() rejects zeros and negatives for beta_ub", {
  d <- bub_data()
  d0 <- d
  d0$y[1] <- 0
  expect_error(check_data(bub_bdat(d0), family = beta_ub(),
                          model = "nec3param"),
               "strictly positive response, but yours contains zeros")
  expect_error(check_data(bub_bdat(d0), family = beta_ub(),
                          model = "nec3param"), "hurdle_gamma")

  dn <- d
  dn$y[1] <- -0.1
  expect_error(check_data(bub_bdat(dn), family = beta_ub(),
                          model = "nec3param"),
               "contains negative values")
  expect_error(check_data(bub_bdat(dn), family = beta_ub(),
                          model = "nec3param"), "issues/175")
})

test_that("check_data() applies no boundary nudge for beta_ub", {
  # a maximum of exactly 1 is unremarkable for this family: the ceiling is a
  # parameter, not the number 1. The Beta family would shift it by 0.001.
  d <- bub_data()
  d$y <- d$y / max(d$y)
  out <- check_data(bub_bdat(d), family = beta_ub(), model = "nec3param")
  expect_equal(max(out$mod_dat$y), 1)
  expect_equal(out$ymax, 1)
  out_b <- check_data(bub_bdat(d), family = Beta(link = "identity"),
                      model = "nec3param")
  expect_lt(max(out_b$mod_dat$y), 1)
})

# ---- Phase 3: model-set routing --------------------------------------------

test_that("check_models() drops the models beta_ub cannot support", {
  d <- bub_data()
  all_mods <- c("nec3param", "nec4param", "ecxexp", "ecx4param", "neclin",
                "neclinhorme", "ecxlin", "nechormepwr01", "nechorme")
  kept <- suppressMessages(check_models(all_mods, beta_ub(), bub_bdat(d)))
  # neclin/neclinhorme/ecxlin can take the response negative; nechormepwr01 is
  # the 0-1 bounded hormesis equation and the response here is not on (0, 1)
  expect_false(any(c("neclin", "neclinhorme", "ecxlin", "nechormepwr01") %in%
                     kept))
  expect_true(all(c("nec3param", "nec4param", "ecxexp", "ecx4param",
                    "nechorme") %in% kept))
  # same set a Gamma with an identity link would keep
  gam <- suppressMessages(check_models(all_mods, Gamma(link = "identity"),
                                       bub_bdat(d)))
  expect_setequal(kept, gam)
})

test_that("check_models() errors when nothing valid is left", {
  d <- bub_data()
  expect_error(suppressMessages(
    check_models(c("neclin", "ecxlin"), beta_ub(), bub_bdat(d))
  ), "None of the model")
})
