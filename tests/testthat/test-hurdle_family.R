test_that("is_hurdle_family recognises tags and family objects", {
  expect_true(bayesnec:::is_hurdle_family("hurdle_gamma"))
  expect_true(bayesnec:::is_hurdle_family(
    brms::hurdle_gamma(link = "identity", link_hu = "identity")
  ))
  expect_false(bayesnec:::is_hurdle_family("Gamma"))
  expect_false(bayesnec:::is_hurdle_family(Gamma(link = "identity")))
  expect_false(bayesnec:::is_hurdle_family(brms::bernoulli()))
})

test_that("make_hu_block prefixes every parameter and inverts the curve", {
  hb <- bayesnec:::make_hu_block("nec3param")
  expect_equal(sort(hb$pars), sort(c("hutop", "hubeta", "hunec")))
  rhs <- deparse1(hb$nlf[[3]])
  expect_true(grepl("^1 - ", rhs))
  # every original name is gone, every prefixed one present
  for (p in c("hutop", "hubeta", "hunec")) expect_true(grepl(p, rhs, fixed = TRUE))
  expect_false(grepl("(?<![[:alnum:]_])top(?![[:alnum:]_])", rhs, perl = TRUE))
  expect_false(grepl("(?<![[:alnum:]_])nec(?![[:alnum:]_])", rhs, perl = TRUE))
})

test_that("make_hu_block does not double-prefix overlapping names", {
  # bot/top are substrings of one another once prefixed; a naive gsub would
  # turn "hutop" into "huhutop" on a second pass.
  hb <- bayesnec:::make_hu_block("nec4param")
  rhs <- deparse1(hb$nlf[[3]])
  expect_false(grepl("huhu", rhs, fixed = TRUE))
  expect_setequal(hb$pars, c("hubot", "hutop", "hubeta", "hunec"))
})

test_that("make_hu_block produces a formula brms accepts for every model", {
  skip_on_cran()
  for (m in models()$all) {
    hb <- bayesnec:::make_hu_block(m)
    comb <- try(
      get(paste0("bf_", m), envir = asNamespace("bayesnec")) +
        brms::nlf(hb$nlf) + brms::lf(hb$lf),
      silent = TRUE
    )
    expect_false(inherits(comb, "try-error"), label = m)
    expect_false(grepl("huhu", deparse1(hb$nlf[[3]]), fixed = TRUE), label = m)
  }
})

test_that("add_hu_block substitutes the real predictor name", {
  bb <- bayesnec:::add_hu_block(
    get("bf_nec3param", envir = asNamespace("bayesnec")), "nec3param", "log_x"
  )
  hu_rhs <- deparse1(bb$pforms$hu[[3]])
  expect_true(grepl("log_x", hu_rhs, fixed = TRUE))
  # the generic "x" must not survive as a bare variable
  expect_false(grepl("(?<![[:alnum:]_])x(?![[:alnum:]_])", hu_rhs, perl = TRUE))
})

test_that("survival_by_x returns proportions clamped inside (0, 1)", {
  x <- rep(1:3, each = 4)
  y <- c(1, 2, 3, 4, 1, 2, 0, 0, 0, 0, 0, 0)   # 4/4, 2/4, 0/4 alive
  s <- bayesnec:::survival_by_x(x, y)
  expect_equal(s$x, 1:3)
  expect_equal(unname(s$y[2]), 0.5)
  # exact 0 and 1 are unusable under an identity link, so they are clamped
  expect_true(all(s$y > 0 & s$y < 1))
  expect_gt(s$y[1], s$y[2])
  expect_gt(s$y[2], s$y[3])
})

test_that("split_hurdle_response separates survivors from the survival curve", {
  x <- rep(1:3, each = 4)
  y <- c(1, 2, 3, 4, 1, 2, 0, 0, 0, 0, 0, 0)
  p <- bayesnec:::split_hurdle_response(x, y)
  expect_equal(p$mu$y, c(1, 2, 3, 4, 1, 2))
  expect_equal(p$mu$x, c(1, 1, 1, 1, 2, 2))
  expect_false(any(p$mu$y == 0))
  expect_equal(length(p$hu$x), 3)
})

test_that("validate_family sets identity links for the character form", {
  fam <- bayesnec:::validate_family("hurdle_gamma")
  expect_equal(fam$family, "hurdle_gamma")
  expect_equal(fam$link, "identity")
  expect_equal(fam$link_hu, "identity")
})

test_that("validate_family rejects a non-identity link_hu", {
  # hu is written as `1 - survival` on the link scale, so a logit link_hu
  # would silently pass that through inv_logit.
  expect_error(
    bayesnec:::validate_family(brms::hurdle_gamma(link = "identity")),
    "link_hu"
  )
  expect_error(
    bayesnec:::validate_family(brms::hurdle_gamma()), "link_hu"
  )
})

test_that("check_data preserves zeros for a hurdle family", {
  dat <- data.frame(x = as.numeric(rep(1:4, each = 5)),
                    y = c(rep(10, 15), rep(3, 3), 0, 0))
  bdat <- model.frame(bnf(y ~ crf(x, "nec3param")), data = dat)
  out <- bayesnec:::check_data(
    bdat, bayesnec:::validate_family("hurdle_gamma"), "nec3param"
  )
  expect_equal(sum(out$mod_dat$y == 0), 2)
})

test_that("check_data still nudges zeros for Gamma, and now says so", {
  dat <- data.frame(x = as.numeric(rep(1:4, each = 5)),
                    y = c(rep(10, 15), rep(3, 3), 0, 0))
  bdat <- model.frame(bnf(y ~ crf(x, "nec3param")), data = dat)
  expect_message(
    out <- bayesnec:::check_data(bdat, Gamma(link = "identity"), "nec3param"),
    "hurdle_gamma"
  )
  expect_equal(sum(out$mod_dat$y == 0), 0)
})

test_that("extract_pars anchors on the parameter name", {
  # Regression test for the cascade: unanchored matching returns a 2-row
  # matrix, tt["Estimate"] is then NA, every parameter comes back NA, and
  # expand_nec() misclassifies a nec model as ecx.
  rn <- c("top_Intercept", "beta_Intercept", "nec_Intercept",
          "hutop_Intercept", "hubeta_Intercept", "hunec_Intercept")
  fef <- matrix(seq_along(rn), nrow = length(rn), ncol = 3,
                dimnames = list(rn, c("Estimate", "Q2.5", "Q97.5")))
  fake <- structure(list(), class = "brmsfit")
  local_mocked_bindings(fixef = function(...) fef)
  out <- bayesnec:::extract_pars("top", fake)
  expect_length(out, 3)
  expect_false(anyNA(out))
  expect_equal(unname(out["Estimate"]), 1)
  hu_out <- bayesnec:::extract_pars("top", fake, prefix = "hu")
  expect_equal(unname(hu_out["Estimate"]), 4)
})

test_that("extract_pars returns NA when nothing matches", {
  rn <- c("top_Intercept")
  fef <- matrix(1, nrow = 1, ncol = 3,
                dimnames = list(rn, c("Estimate", "Q2.5", "Q97.5")))
  fake <- structure(list(), class = "brmsfit")
  local_mocked_bindings(fixef = function(...) fef)
  expect_true(is.na(bayesnec:::extract_pars("ec50", fake)))
})

test_that("check_models applies both sets of restrictions to a hurdle family", {
  dat <- data.frame(x = as.numeric(1:10), y = c(rep(5, 8), 0, 0))
  bdat <- model.frame(bnf(y ~ crf(x, "nec3param")), data = dat)
  fam <- bayesnec:::validate_family("hurdle_gamma")
  out <- suppressMessages(
    bayesnec:::check_models(models()$all, fam, bdat)
  )
  # zero-bounded rules (from Gamma/identity) plus 0-1 rules (from the hu block)
  expect_false(any(c("neclin", "neclinhorme", "ecxlin", "nechormepwr01")
                   %in% out))
  expect_true("nec3param" %in% out)
})

test_that("define_prior builds both blocks for a hurdle family", {
  x <- c(-9, -5, -2, 0, 2, 3)
  y <- c(20, 18, 15, 9, 4, 0)
  pr <- as.data.frame(
    bayesnec:::define_prior("nec3param", bayesnec:::validate_family("hurdle_gamma"), x, y)
  )
  expect_setequal(pr$nlpar, c("top", "beta", "nec", "hutop", "hubeta", "hunec"))
  # hu block is a probability: 0-1 bounded
  hutop <- pr[pr$nlpar == "hutop", ]
  expect_equal(hutop$lb, "0"); expect_equal(hutop$ub, "1")
  # both thresholds may range over the whole predictor, not just the part
  # their own block was primed from
  for (p in c("nec", "hunec")) {
    row <- pr[pr$nlpar == p, ]
    expect_equal(as.numeric(row$lb), min(x))
    expect_equal(as.numeric(row$ub), max(x))
  }
})

test_that("hurdle mu priors are built from survivors, not from the zeros", {
  x <- c(-9, -5, -2, 0, 2, 3)
  y <- c(20, 18, 15, 9, 4, 0)
  hurdle <- as.data.frame(
    bayesnec:::define_prior("nec3param", bayesnec:::validate_family("hurdle_gamma"), x, y)
  )
  survivors <- as.data.frame(
    bayesnec:::define_prior("nec3param", Gamma(link = "identity"),
                            x[y > 0], y[y > 0])
  )
  expect_equal(hurdle$prior[hurdle$nlpar == "top"],
               survivors$prior[survivors$nlpar == "top"])
  # and it must differ from the naive version that includes the zeros
  with_zeros <- as.data.frame(
    bayesnec:::define_prior("nec3param", Gamma(link = "identity"), x, y + 1e-6)
  )
  expect_false(identical(hurdle$prior[hurdle$nlpar == "top"],
                         with_zeros$prior[with_zeros$nlpar == "top"]))
})

test_that("make_good_hurdle_inits returns both blocks per chain", {
  set.seed(1)
  x <- rep(c(-4, -2, 0, 2, 3), each = 8)
  y <- c(rep(20, 8), rep(18, 8), rep(12, 8), c(rep(5, 6), 0, 0), rep(0, 8))
  pr <- bayesnec:::define_prior(
    "nec3param", bayesnec:::validate_family("hurdle_gamma"), x, y
  )
  inits <- bayesnec:::make_good_hurdle_inits("nec3param", x, y, priors = pr,
                                             chains = 2, seed = 1)
  skip_if(length(inits) == 1 && "random" %in% names(inits),
          "init search fell back to random")
  expect_length(inits, 2)
  expect_setequal(names(inits[[1]]),
                  c("b_top", "b_beta", "b_nec", "b_hutop", "b_hubeta", "b_hunec"))
  # hu block is a probability
  expect_gte(inits[[1]]$b_hutop, 0)
  expect_lte(inits[[1]]$b_hutop, 1)
})

test_that("wrangle_model_formula only adds a hu block for hurdle families", {
  dat <- data.frame(lx = as.numeric(1:10), y = c(rep(5, 8), 0, 0))
  f <- bnf(y ~ crf(lx, "nec3param"))
  bdat <- model.frame(f, data = dat)
  plain <- bayesnec:::wrangle_model_formula("nec3param", f, bdat)
  expect_null(plain$pforms$hu)
  hurdle <- bayesnec:::wrangle_model_formula(
    "nec3param", f, bdat, bayesnec:::validate_family("hurdle_gamma")
  )
  expect_false(is.null(hurdle$pforms$hu))
  expect_true(all(c("hutop", "hubeta", "hunec") %in% names(hurdle$pforms)))
})
