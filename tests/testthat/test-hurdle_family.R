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
  hb <- bayesnec:::make_hu_block("nec3param", "hu")
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
  hb <- bayesnec:::make_hu_block("nec4param", "hu")
  rhs <- deparse1(hb$nlf[[3]])
  expect_false(grepl("huhu", rhs, fixed = TRUE))
  expect_setequal(hb$pars, c("hubot", "hutop", "hubeta", "hunec"))
})

test_that("make_hu_block produces a formula brms accepts for every model", {
  skip_on_cran()
  for (m in models()$all) {
    hb <- bayesnec:::make_hu_block(m, "hu")
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
    get("bf_nec3param", envir = asNamespace("bayesnec")), "nec3param", "log_x",
    "hu"
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

test_that("validate_family rejects a non-identity link_hu the caller chose", {
  # hu is written as `1 - survival` on the link scale, so a logit link_hu
  # would silently pass that through inv_logit. Where the caller chose a link
  # the object is honoured, so the guard still has to fire.
  expect_error(
    bayesnec:::validate_family(brms::hurdle_gamma(link = "identity"),
                               link_source = "chosen"),
    "link_hu"
  )
  # The reachable form: link_hu written and nothing else. family_link_source()
  # reads it as chosen, so the guard fires rather than the link being rewritten.
  expect_error(
    bayesnec:::validate_family(
      brms::hurdle_gamma(link_hu = "logit"),
      link_source = bayesnec:::family_link_source(
        quote(hurdle_gamma(link_hu = "logit"))
      )
    ),
    "link_hu"
  )
})

test_that("a hurdle family named without a link gets both links assigned", {
  # Previously `family = hurdle_gamma()` errored, because link_hu defaults to
  # logit and the guard above fired on a link the user had not chosen. Naming
  # the family now leaves both links to bayesnec, which is the whole point of
  # #256: the caller named a family and nothing more.
  fam <- bayesnec:::validate_family(brms::hurdle_gamma())
  expect_equal(fam$family, "hurdle_gamma")
  expect_equal(fam$link, "identity")
  expect_equal(fam$link_hu, "identity")

  zib <- bayesnec:::validate_family(brms::zero_inflated_beta())
  expect_equal(zib$link, "identity")
  expect_equal(zib$link_zi, "identity")
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

# ---------------------------------------------------------------------------
# zero_inflated_beta: the same two-block structure, brms just calls the second
# block "zi" instead of "hu"
# ---------------------------------------------------------------------------

test_that("hurdle_dpar reports the name brms uses for the second block", {
  expect_equal(bayesnec:::hurdle_dpar("hurdle_gamma"), "hu")
  expect_equal(bayesnec:::hurdle_dpar("zero_inflated_beta"), "zi")
  expect_equal(
    bayesnec:::hurdle_dpar(brms::zero_inflated_beta(link = "identity",
                                                    link_zi = "identity")),
    "zi"
  )
})

test_that("zero_inflated_beta is recognised as a two-block family", {
  expect_true(bayesnec:::is_hurdle_family("zero_inflated_beta"))
  expect_true(bayesnec:::is_hurdle_family(
    brms::zero_inflated_beta(link = "identity", link_zi = "identity")
  ))
  expect_false(bayesnec:::is_hurdle_family(brms::Beta()))
})

test_that("hurdle_mu_family picks the right family for the non-zero subset", {
  expect_equal(bayesnec:::hurdle_mu_family("hurdle_gamma")$family, "Gamma")
  expect_equal(bayesnec:::hurdle_mu_family("zero_inflated_beta")$family, "beta")
})

test_that("make_hu_block prefixes with zi for zero-inflated families", {
  hb <- bayesnec:::make_hu_block("nec3param", "zi")
  expect_setequal(hb$pars, c("zitop", "zibeta", "zinec"))
  expect_equal(deparse1(hb$nlf[[2]]), "zi")
  rhs <- deparse1(hb$nlf[[3]])
  expect_true(grepl("^1 - ", rhs))
  expect_false(grepl("zizi", rhs, fixed = TRUE))
  expect_false(grepl("hu", rhs, fixed = TRUE))
})

test_that("validate_family sets both identity links for zero_inflated_beta", {
  fam <- bayesnec:::validate_family("zero_inflated_beta")
  expect_equal(fam$family, "zero_inflated_beta")
  expect_equal(fam$link, "identity")
  expect_equal(fam$link_zi, "identity")
})

test_that("validate_family names the right link argument in its error", {
  # the guard must reference link_zi, not link_hu, for this family
  expect_error(
    bayesnec:::validate_family(brms::zero_inflated_beta(link = "identity"),
                               link_source = "chosen"),
    "link_zi"
  )
})

test_that("wrangle_model_formula builds a zi block for zero_inflated_beta", {
  dat <- data.frame(lx = as.numeric(1:10), y = c(rep(0.6, 8), 0, 0))
  f <- bnf(y ~ crf(lx, "nec3param"))
  bdat <- model.frame(f, data = dat)
  bb <- bayesnec:::wrangle_model_formula(
    "nec3param", f, bdat, bayesnec:::validate_family("zero_inflated_beta")
  )
  expect_false(is.null(bb$pforms$zi))
  expect_null(bb$pforms$hu)
  expect_true(all(c("zitop", "zibeta", "zinec") %in% names(bb$pforms)))
})

test_that("define_prior builds zi-prefixed blocks bounded on (0, 1)", {
  x <- c(-4, -2, 0, 2, 3)
  y <- c(0.8, 0.7, 0.5, 0.2, 0)
  pr <- as.data.frame(
    bayesnec:::define_prior("nec3param",
                            bayesnec:::validate_family("zero_inflated_beta"),
                            x, y)
  )
  expect_setequal(pr$nlpar, c("top", "beta", "nec", "zitop", "zibeta", "zinec"))
  # both blocks are probabilities here, so both top parameters are 0-1 bounded
  for (p in c("top", "zitop")) {
    row <- pr[pr$nlpar == p, ]
    expect_equal(row$lb, "0")
    expect_equal(row$ub, "1")
  }
})

test_that("check_models keeps nechormepwr01 for zero_inflated_beta", {
  # nechormepwr01 is the 0-1 bounded hormesis equation, so it is valid where the
  # mu block is Beta but not where it is Gamma.
  dat <- data.frame(x = as.numeric(1:10), y = c(rep(0.5, 8), 0, 0))
  bdat <- model.frame(bnf(y ~ crf(x, "nec3param")), data = dat)
  zib <- suppressMessages(bayesnec:::check_models(
    models()$all, bayesnec:::validate_family("zero_inflated_beta"), bdat
  ))
  hg <- suppressMessages(bayesnec:::check_models(
    models()$all, bayesnec:::validate_family("hurdle_gamma"), bdat
  ))
  expect_true("nechormepwr01" %in% zib)
  expect_false("nechormepwr01" %in% hg)
  # the linear-decay models go for both
  for (m in c("neclin", "neclinhorme", "ecxlin")) {
    expect_false(m %in% zib)
    expect_false(m %in% hg)
  }
})

test_that("check_data keeps zeros but nudges ones for zero_inflated_beta", {
  dat <- data.frame(x = as.numeric(rep(1:4, each = 5)),
                    y = c(rep(0.5, 13), 1, 1, rep(0.2, 3), 0, 0))
  bdat <- model.frame(bnf(y ~ crf(x, "nec3param")), data = dat)
  out <- bayesnec:::check_data(
    bdat, bayesnec:::validate_family("zero_inflated_beta"), "nec3param"
  )
  expect_equal(sum(out$mod_dat$y == 0), 2)   # zeros are the signal, kept
  expect_equal(sum(out$mod_dat$y == 1), 0)   # ones are outside Beta support
  expect_true(all(out$mod_dat$y < 1))
})

test_that("make_good_hurdle_inits uses the zi prefix", {
  set.seed(1)
  x <- rep(c(-4, -2, 0, 2, 3), each = 8)
  y <- c(rep(0.8, 8), rep(0.7, 8), rep(0.5, 8), c(rep(0.3, 6), 0, 0), rep(0, 8))
  pr <- bayesnec:::define_prior(
    "nec3param", bayesnec:::validate_family("zero_inflated_beta"), x, y
  )
  inits <- bayesnec:::make_good_hurdle_inits("nec3param", x, y, priors = pr,
                                             chains = 2, dpar = "zi", seed = 1)
  skip_if(length(inits) == 1 && "random" %in% names(inits),
          "init search fell back to random")
  expect_setequal(names(inits[[1]]),
                  c("b_top", "b_beta", "b_nec", "b_zitop", "b_zibeta", "b_zinec"))
})

# ---------------------------------------------------------------------------
# model_survival: a different equation on each of the two blocks
# ---------------------------------------------------------------------------

test_that("wrangle_model_formula uses model_survival for the hu block", {
  dat <- data.frame(lx = as.numeric(1:10), y = c(rep(5, 8), 0, 0))
  f <- bnf(y ~ crf(lx, "nec3param"))
  bdat <- model.frame(f, data = dat)
  mixed <- bayesnec:::wrangle_model_formula(
    "nec3param", f, bdat, bayesnec:::validate_family("hurdle_gamma"),
    model_survival = "ecx4param"
  )
  # response block keeps nec3param's parameters, hu block takes ecx4param's
  expect_true(all(c("top", "beta", "nec") %in% names(mixed$pforms)))
  expect_true(all(c("hutop", "hubot", "huec50", "hubeta") %in%
                    names(mixed$pforms)))
  expect_false("hunec" %in% names(mixed$pforms))
  # and the block is still written as 1 - <declining equation>
  expect_true(grepl("^1 - ", deparse1(mixed$pforms$hu[[3]])))
})

test_that("define_prior builds hu priors for the survival equation", {
  x <- as.numeric(rep(1:5, each = 6))
  y <- c(rep(c(3, 2.5, 2, 1), each = 6), rep(0, 6))
  prs <- as.data.frame(
    bayesnec:::define_prior("nec3param",
                            bayesnec:::validate_family("hurdle_gamma"), x, y,
                            model_survival = "ecx4param")
  )
  expect_true(all(c("top", "beta", "nec") %in% prs$nlpar))
  expect_true(all(c("hutop", "hubot", "huec50", "hubeta") %in% prs$nlpar))
  expect_false("hunec" %in% prs$nlpar)
})

test_that("make_good_hurdle_inits primes each block with its own equation", {
  x <- as.numeric(rep(1:5, each = 6))
  y <- c(rep(c(3, 2.5, 2, 1), each = 6), rep(0, 6))
  pr <- bayesnec:::define_prior("nec3param",
                                bayesnec:::validate_family("hurdle_gamma"),
                                x, y, model_survival = "ecx4param")
  inits <- bayesnec:::make_good_hurdle_inits("nec3param", x, y, priors = pr,
                                             chains = 2,
                                             model_survival = "ecx4param")
  skip_if(length(inits) == 1 && "random" %in% names(inits),
          "init search fell back to random")
  expect_length(inits, 2)
  expect_setequal(names(inits[[1]]),
                  c("b_top", "b_beta", "b_nec", "b_hutop", "b_hubot",
                    "b_huec50", "b_hubeta"))
})

test_that("check_model_survival validates against the survival block", {
  dat <- data.frame(lx = as.numeric(1:10), y = c(rep(5, 8), 0, 0))
  bdat <- model.frame(bnf(y ~ crf(lx, "nec3param")), data = dat)
  hurdle_fam <- bayesnec:::validate_family("hurdle_gamma")
  expect_null(bayesnec:::check_model_survival(NULL, hurdle_fam, bdat))
  expect_equal(
    bayesnec:::check_model_survival("ecx4param", hurdle_fam, bdat), "ecx4param"
  )
  # only meaningful for a two-block family
  expect_error(
    bayesnec:::check_model_survival("ecx4param", Gamma(link = "identity"),
                                    bdat),
    "only applies to the two-block families"
  )
  # a set is not accepted: averaging over both blocks means fitting every pair
  expect_error(
    bayesnec:::check_model_survival(c("nec3param", "ecx4param"), hurdle_fam,
                                    bdat),
    "must be a single model name"
  )
  # the survival block is 0-1 bounded, so the linear models are not valid
  expect_error(
    suppressMessages(
      bayesnec:::check_model_survival("ecxlin", hurdle_fam, bdat)
    ),
    "valid for a bernoulli"
  )
})
