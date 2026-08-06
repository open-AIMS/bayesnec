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

# ---- Phase 4: priors and inits ---------------------------------------------

test_that("the delta prior is the ceiling prior, shifted", {
  d <- bub_data()
  ymax <- max(d$y)
  p <- as.data.frame(define_prior("nec3param", beta_ub(), d$x, d$y,
                                  ymax = ymax, u_loc = 1, u_scale = 0.1))
  dp <- p[p$class == "delta", ]
  expect_equal(nrow(dp), 1)
  expect_equal(dp$lb, "0")
  # normal(U_loc - ymax, U_scale): the pure location shift that makes a prior
  # on delta identical to a truncated prior on U
  expect_equal(dp$prior, paste0("normal(", 1 - ymax, ", 0.1)"))
  # the mu block keeps the natural-scale Gamma defaults
  expect_setequal(p$nlpar[nzchar(p$nlpar)], c("beta", "top", "nec"))
})

test_that("no U_loc gives a prior-driven ceiling, and says so", {
  d <- bub_data()
  expect_message(define_prior("nec3param", beta_ub(), d$x, d$y,
                              ymax = max(d$y)),
                 "prior-driven")
  p <- suppressMessages(
    as.data.frame(define_prior("nec3param", beta_ub(), d$x, d$y,
                               ymax = max(d$y)))
  )
  dp <- p[p$class == "delta", ]
  expect_equal(dp$prior, paste0("normal(0, ", max(d$y) / 4, ")"))
  expect_equal(dp$lb, "0")
})

test_that("U_loc at or below ymax warns rather than passing silently", {
  d <- bub_data()
  ymax <- max(d$y)
  expect_warning(define_prior("nec3param", beta_ub(), d$x, d$y, ymax = ymax,
                              u_loc = ymax / 2, u_scale = 0.1),
                 "lies entirely in the region the likelihood rejects")
  # exactly equal is still degenerate
  expect_warning(define_prior("nec3param", beta_ub(), d$x, d$y, ymax = ymax,
                              u_loc = ymax, u_scale = 0.1))
  expect_silent(define_prior("nec3param", beta_ub(), d$x, d$y, ymax = ymax,
                             u_loc = ymax * 1.5, u_scale = 0.1))
})

test_that("U_loc and U_scale must be supplied together and be sane", {
  d <- bub_data()
  ymax <- max(d$y)
  expect_error(define_prior("nec3param", beta_ub(), d$x, d$y, ymax = ymax,
                            u_loc = 1), "Supply both")
  expect_error(define_prior("nec3param", beta_ub(), d$x, d$y, ymax = ymax,
                            u_scale = 0.1), "Supply both")
  expect_error(define_prior("nec3param", beta_ub(), d$x, d$y, ymax = ymax,
                            u_loc = 1, u_scale = -1), "single positive")
  expect_error(define_prior("nec3param", beta_ub(), d$x, d$y, ymax = ymax,
                            u_loc = c(1, 2), u_scale = 0.1), "single finite")
})

test_that("the delta prior does not reach the init machinery", {
  # make_inits() checks its parameter names against the prediction function's
  # arguments, so a prior with no nlpar would abort the whole model set
  d <- bub_data()
  pr <- suppressMessages(define_prior("nec3param", beta_ub(), d$x, d$y,
                                      ymax = max(d$y), u_loc = 1,
                                      u_scale = 0.1))
  expect_true("delta" %in% as.data.frame(pr)$class)
  i0 <- make_good_inits("nec3param", d$x, d$y, priors = pr, chains = 2,
                        seed = 1)
  expect_length(i0, 2)
  expect_setequal(names(i0[[1]]), c("b_top", "b_beta", "b_nec"))
})

test_that("add_beta_ub_inits() primes phi and delta and respects mu < U", {
  d <- bub_data()
  ymax <- max(d$y)
  pr <- suppressMessages(define_prior("nec3param", beta_ub(), d$x, d$y,
                                      ymax = ymax, u_loc = 1, u_scale = 0.1))
  i0 <- make_good_inits("nec3param", d$x, d$y, priors = pr, chains = 2,
                        seed = 1)
  i1 <- add_beta_ub_inits(i0, d$x, d$y, ymax = ymax, u_loc = 1, u_scale = 0.1,
                          seed = 1)
  expect_length(i1, 2)
  for (z in i1) {
    expect_true(all(c("phi", "delta") %in% names(z)))
    expect_gt(z$delta, 0)
    expect_gt(z$phi, 0)
    # the curve must start below its own ceiling
    expect_gt(ymax + z$delta, z$b_top)
  }
  # method of moments over all replicated predictor levels, not just controls
  expect_gt(i1[[1]]$phi, 5)
  expect_lt(i1[[1]]$phi, 200)
})

test_that("add_beta_ub_inits() passes Stan's random fallback through", {
  d <- bub_data()
  out <- add_beta_ub_inits(list(random = "random"), d$x, d$y, ymax = max(d$y))
  expect_identical(out, list(random = "random"))
})
