# --- #256: one statement of what mu may be, and what each model can produce ---
# The point of these tests is that the two artefacts cannot silently disagree
# with the code they describe. #170 was opened because ?models and
# check_models() had drifted apart; a table that is written down and never
# checked is the same failure with an extra file.

expected_supports <- function() {
  # Keyed on the constructor names mod_fams holds as values, which is the form
  # validate_family() accepts. The tag brms reports differs for one family --
  # "beta" against the constructor "Beta" -- and validate_family("beta") errors,
  # which is recorded on #256 and not this test's subject. A family added to
  # mod_fams without a decision here fails the completeness test below.
  list(gaussian = c(-Inf, Inf),
       Gamma = c(0, Inf), poisson = c(0, Inf), negbinomial = c(0, Inf),
       zero_inflated_poisson = c(0, Inf),
       zero_inflated_negbinomial = c(0, Inf),
       hurdle_gamma = c(0, Inf),
       bernoulli = c(0, 1), Beta = c(0, 1), binomial = c(0, 1),
       beta_binomial = c(0, 1), zero_inflated_beta = c(0, 1))
}

test_that("mu_support is a property of the response distribution", {
  exp_s <- expected_supports()
  for (fam in names(exp_s)) {
    expect_equal(bayesnec:::mu_support(validate_family(fam)), exp_s[[fam]],
                 info = fam)
  }
  # the hu and zi blocks are probabilities whatever the mu block is, which is
  # the case hurdle_gamma makes: (0, Inf) on mu and (0, 1) on hu at once
  expect_equal(bayesnec:::mu_support(validate_family("hurdle_gamma"),
                                     dpar = "hu"), c(0, 1))
  expect_equal(bayesnec:::mu_support(validate_family("zero_inflated_beta"),
                                     dpar = "zi"), c(0, 1))
  expect_equal(bayesnec:::mu_support(NULL), c(-Inf, Inf))
})

test_that("every family in mod_fams has a decided support", {
  # This is the test the comment claims: an unlisted family errors here rather
  # than passing on a fallback that happens to satisfy a range check.
  expect_setequal(names(expected_supports()), unname(bayesnec:::mod_fams))
})

test_that("mu_is_constrained asks family and link together, not either alone", {
  f <- bayesnec:::mu_is_constrained
  # identity passes the linear predictor through untouched, so the mean is
  # whatever the curve produces
  expect_true(f(validate_family("Beta")))
  expect_true(f(validate_family("Gamma")))
  expect_true(f(validate_family("beta_binomial")))
  expect_true(f(validate_family("hurdle_gamma")))
  # gaussian has nothing to violate, whatever the link
  expect_false(f(validate_family("gaussian")))
  expect_false(f(gaussian(link = "log")))
  # a link whose inverse maps into the support guarantees a valid mean
  expect_false(f(Beta(link = "logit")))
  expect_false(f(binomial(link = "probit")))
  expect_false(f(binomial(link = "cloglog")))
  expect_false(f(poisson(link = "sqrt")))
  expect_false(f(Gamma(link = "log")))

  # The case that shows neither family nor link decides it alone. exp(eta) is
  # positive but unbounded above, so on a (0, 1) response a log link can hand
  # beta_lpdf a negative second shape parameter -- while guaranteeing a valid
  # mean for every count family. Confirmed against the generated Stan code,
  # which emits `mu = exp(mu)` then `beta_lpdf(Y | mu .* phi, (1 - mu) .* phi)`.
  expect_true(f(Beta(link = "log")))
  expect_true(f(binomial(link = "log")))
  expect_true(f(brms::bernoulli(link = "log")))
  expect_true(f(brms::zero_inflated_beta(link = "log")))

  # inverse maps onto the whole real line, so it guarantees nothing
  expect_true(f(Gamma(link = "inverse")))

  # an unrecognised link is treated as reaching anywhere, so it never lies
  # inside a bounded support and the answer errs towards raising adapt_delta
  odd <- validate_family("Beta"); odd$link <- "not_a_real_link"
  expect_true(f(odd))
  odd_g <- validate_family("gaussian"); odd_g$link <- "not_a_real_link"
  expect_false(f(odd_g))

  expect_false(f(NULL))
})

test_that("a two-block family is asked about the block's own link", {
  # validate_family() requires link_hu = "identity", so the hu block is always
  # on identity with (0, 1) support and is always reachable, whatever the mu
  # link is. Reading family$link for the hu block would answer the mu question
  # twice.
  f <- bayesnec:::mu_is_constrained
  hg <- validate_family("hurdle_gamma")
  expect_true(f(hg, dpar = "hu"))
  hg_log <- hg; hg_log$link <- "log"
  expect_false(f(hg_log))
  expect_true(f(hg_log, dpar = "hu"))

  # The case that discriminates. On hurdle_gamma both readings answer TRUE for
  # the hu block, so reverting to family$link would leave the tests passing. A
  # zero_inflated_beta on a logit mu link does not: the mu block is guaranteed
  # and the zi block, on identity, is not.
  zib <- validate_family("zero_inflated_beta")
  zib$link <- "logit"
  expect_false(f(zib))
  expect_true(f(zib, dpar = "zi"))
  expect_equal(zib$link_zi, "identity")
})

test_that("no link any accepted family takes falls to the default unintended", {
  # link_range()'s default is c(-Inf, Inf), which reports the mean as reachable.
  # That is correct for identity, inverse and 1/mu^2 and wrong for anything
  # else, so the enumeration is asserted rather than assumed. Obtained by
  # constructing every family in mod_fams against every candidate link.
  fams <- list(gaussian = stats::gaussian, Gamma = stats::Gamma,
               poisson = stats::poisson, binomial = stats::binomial,
               negbinomial = brms::negbinomial, bernoulli = brms::bernoulli,
               Beta = brms::Beta, beta_binomial = brms::beta_binomial,
               hurdle_gamma = brms::hurdle_gamma,
               zero_inflated_beta = brms::zero_inflated_beta,
               zero_inflated_poisson = brms::zero_inflated_poisson,
               zero_inflated_negbinomial = brms::zero_inflated_negbinomial)
  candidates <- c("identity", "log", "logit", "probit", "probit_approx",
                  "cloglog", "cauchit", "inverse", "sqrt", "softplus",
                  "squareplus", "softit", "1/mu^2")
  accepted <- character(0)
  for (fn in names(fams)) {
    for (l in candidates) {
      ok <- tryCatch({fams[[fn]](link = l); TRUE},
                     error = function(e) FALSE, warning = function(w) TRUE)
      if (ok) accepted <- union(accepted, l)
    }
  }
  falls_to_default <- accepted[vapply(accepted, function(l)
    all(is.infinite(bayesnec:::link_range(l))), logical(1))]
  expect_setequal(falls_to_default, c("identity", "inverse", "1/mu^2"))
})

test_that("the model range table covers every model exactly once", {
  tab <- bayesnec:::model_mu_ranges()
  expect_setequal(tab$model, models()$all)
  expect_equal(anyDuplicated(tab$model), 0)
  expect_setequal(names(tab), c("model", "below_zero", "unscaled_excess",
                               "can_exceed_one", "ceiling_at_one",
                               "zero_asymptote"))
})

# --- the table regenerated from the formulas -------------------------------
# Rather than trusting the flags, evaluate each model's mean over a grid of
# parameter values inside the constraints a 0-1 bounded family imposes, and
# read the flags back off the result.

eval_mu <- function(model, pars, x) {
  expr <- get(paste0("bf_", model))[[1]][[3]]
  env <- c(as.list(pars), list(x = x, step = function(z) as.numeric(z >= 0)))
  eval(expr, envir = env)
}

par_grid <- function(model, slope_val = NULL) {
  pars <- names(get(paste0("bf_", model))[[2]])
  vals <- list(top = c(0.2, 0.9), bot = c(0.05, 0.5), nec = c(1, 3),
               beta = c(-2, 1), d = c(-1, 1), f = c(-1, 1),
               ec50 = c(1, 3),
               slope = if (is.null(slope_val)) c(-2, 1) else slope_val)
  expand.grid(vals[pars], KEEP.OUT.ATTRS = FALSE)
}

sweep_mu <- function(model, slope_val = NULL, x = seq(0, 6, length.out = 200)) {
  g <- par_grid(model, slope_val)
  mus <- lapply(seq_len(nrow(g)), function(i) eval_mu(model, g[i, , drop = FALSE], x))
  mus <- unlist(mus)
  mus[is.finite(mus)]
}

test_that("below_zero is what the formulas do", {
  tab <- bayesnec:::model_mu_ranges()
  for (m in tab$model) {
    got <- min(sweep_mu(m)) < 0
    expect_equal(got, tab$below_zero[tab$model == m], info = m)
  }
})

test_that("unscaled_excess is an excess the fit cannot shrink away", {
  # With slope driven far negative, exp(slope) is effectively zero, so any model
  # whose excess term carries a coefficient can no longer exceed its level. What
  # still exceeds 1 is the excess with no coefficient.
  tab <- bayesnec:::model_mu_ranges()
  has_slope <- vapply(tab$model, function(m)
    "slope" %in% names(get(paste0("bf_", m))[[2]]), logical(1))
  for (m in tab$model[has_slope]) {
    got <- max(sweep_mu(m, slope_val = -25)) > 1
    expect_equal(got, tab$unscaled_excess[tab$model == m], info = m)
  }
  # and the coefficiented hormesis models can exceed 1 for some slope, which is
  # why the distinction is needed at all rather than a plain "can exceed"
  for (m in c("nechorme", "nechorme4", "ecxhormebc4", "ecxhormebc5")) {
    expect_true(max(sweep_mu(m, slope_val = 2)) > 1, info = m)
    expect_false(tab$unscaled_excess[tab$model == m], info = m)
  }
  # the flag is FALSE for the thirteen equations with no slope at all, and that
  # is asserted rather than assumed: each is either top times a factor in
  # (0, 1] or a convex combination of top and bot, so none can exceed 1 when
  # both are inside it
  for (m in tab$model[!has_slope]) {
    expect_lte(max(sweep_mu(m)), 1 + 1e-8, label = m)
    expect_false(tab$unscaled_excess[tab$model == m], info = m)
  }
})

test_that("zero_asymptote is what the formulas do, and matches mod_groups", {
  tab <- bayesnec:::model_mu_ranges()
  # The mean of a zero-asymptote model decays onto zero, so at a large
  # predictor value it is a negligible fraction of its own level. Checked as a
  # ratio rather than against a fixed tolerance, because necsigm and ecxsigm
  # approach zero slowly enough that any single cutoff is arbitrary.
  for (m in tab$model[tab$zero_asymptote]) {
    g <- par_grid(m)
    ratios <- vapply(seq_len(nrow(g)), function(i) {
      p_i <- g[i, , drop = FALSE]
      eval_mu(m, p_i, 1e6) / max(eval_mu(m, p_i, 0), .Machine$double.eps)
    }, numeric(1))
    ratios <- ratios[is.finite(ratios)]
    expect_lt(max(ratios), 1e-6)
  }
  # a model with a free lower asymptote does not, whatever the predictor
  for (m in c("nec4param", "ecx4param", "ecxll4")) {
    g <- par_grid(m)
    tail_vals <- vapply(seq_len(nrow(g)), function(i)
      eval_mu(m, g[i, , drop = FALSE], 1e6), numeric(1))
    expect_true(all(tail_vals > 0.01), info = m)
  }
  # and the derivation reproduces the group the package already ships
  expect_setequal(tab$model[tab$zero_asymptote], mod_groups$zero_bounded)
})

test_that("ceiling_at_one is a saturating hormetic term, not a support failure", {
  # Read off bf_nechormepwr01 rather than a hand-written copy of its hormetic
  # term, so that editing the equation breaks this test. nec is placed beyond
  # the evaluated predictor range so the decay factor is exactly 1 and what is
  # left is the hormetic term alone.
  x <- c(0, 0.5, 1, 2, 4, 10)
  fac <- function(top, slope = 0) {
    eval_mu("nechormepwr01",
            data.frame(top = top, slope = slope, beta = 0, nec = 1e6), x)
  }
  rising <- fac(0.3)
  expect_true(all(diff(rising) > 0))
  expect_equal(rising[1], 0.3)
  expect_lt(max(rising), 1 + 1e-8)

  falling <- fac(20)
  expect_true(all(diff(falling) < 0))
  expect_equal(falling[1], 20)
  # it approaches one from above rather than reaching it, so a mean above top
  # cannot be represented and the shape is a decline rather than an increase
  expect_gt(falling[length(falling)], 1)
  expect_lt(eval_mu("nechormepwr01",
                    data.frame(top = 20, slope = 0, beta = 0, nec = 1e6), 100),
            1 + 1e-8)
  # mu stays strictly positive throughout, so no support flag would exclude it
  expect_true(all(c(rising, falling) > 0))

  tab <- bayesnec:::model_mu_ranges()
  expect_equal(tab$model[tab$ceiling_at_one], "nechormepwr01")
  # and no slope value takes it above one, which is why it is the one model
  # carrying slope that slope cannot make unsafe
  for (sl in c(-5, 0, 5, 20)) {
    expect_lt(max(fac(0.9, slope = sl)), 1 + 1e-8)
  }
})

# --- the gates derive from the two artefacts --------------------------------
# This is the test that stops the table becoming a second source of truth. It
# does not change check_models(); it asserts that what check_models() does is
# what the table says it should. A later change makes the gates read the table
# directly, and any difference found then is a decision rather than a
# regression. See #256.

test_that("check_models' gates agree with the model range table", {
  tab <- bayesnec:::model_mu_ranges()
  all_m <- models()$all
  dropped <- function(family) {
    setdiff(all_m, suppressMessages(bayesnec:::check_models(all_m, family)))
  }

  # support: a 0-1 bounded mean cannot go below zero, and cannot exceed one
  # through a term the fit is unable to shrink
  for (fam in c("Beta", "binomial", "beta_binomial", "bernoulli")) {
    expect_setequal(dropped(validate_family(fam)),
                    tab$model[tab$below_zero | tab$unscaled_excess])
  }

  # support and appropriateness: a (0, Inf) mean cannot go below zero, and a
  # mean that saturates at one cannot describe a count
  for (fam in c("Gamma", "poisson", "negbinomial", "zero_inflated_poisson",
                "zero_inflated_negbinomial")) {
    expect_setequal(dropped(validate_family(fam)),
                    tab$model[tab$below_zero | tab$ceiling_at_one])
  }

  # reachability: a mean that decays onto zero cannot produce the negative
  # values a log or logit linear predictor needs
  expect_setequal(dropped(Beta(link = "logit")),
                  tab$model[tab$zero_asymptote])
  expect_setequal(dropped(gaussian(link = "log")),
                  tab$model[tab$zero_asymptote])

  # INVERTED with #206. The gaussian exclusion was keyed on the family
  # irrespective of link and dropped every zero-asymptote equation; it was
  # asserted here as current behaviour and explicitly not endorsed. It is
  # removed: the exclusion conflated the range of the mean function with the
  # support of the likelihood, and a gaussian likelihood evaluates y - mu and
  # never tests the sign of y. Nothing is dropped for gaussian now.
  expect_length(dropped(validate_family("gaussian")), 0)
  # The link exclusion above is a different condition and still applies, which
  # is what stops this reading as "gaussian drops nothing ever".
  expect_setequal(dropped(gaussian(link = "log")),
                  tab$model[tab$zero_asymptote])

  # the two-block families apply both blocks' restrictions at once, which is
  # why mu_support() takes a dpar. hurdle_gamma is the case that needs it:
  # (0, Inf) on mu and (0, 1) on hu, so it drops the union of what each block
  # would drop on its own. Nothing else in the model set exercises that.
  hg <- validate_family("hurdle_gamma")
  expect_equal(bayesnec:::mu_support(hg), c(0, Inf))
  expect_equal(bayesnec:::mu_support(hg, dpar = "hu"), c(0, 1))
  expect_setequal(
    dropped(hg),
    tab$model[tab$below_zero | tab$ceiling_at_one | tab$unscaled_excess]
  )
  # zero_inflated_beta is 0-1 on both blocks, so the union collapses to the
  # 0-1 rule and it looks like an ordinary bounded family
  zib <- validate_family("zero_inflated_beta")
  expect_equal(bayesnec:::mu_support(zib), c(0, 1))
  expect_equal(bayesnec:::mu_support(zib, dpar = "zi"), c(0, 1))
  expect_setequal(dropped(zib),
                  tab$model[tab$below_zero | tab$unscaled_excess])
})

test_that("the agreement test covers every family in mod_fams", {
  # Finding from review: the first version asserted "every family" and covered
  # ten of twelve, omitting the two-block branch of check_models() entirely.
  covered <- c("Beta", "binomial", "beta_binomial", "bernoulli",
               "Gamma", "poisson", "negbinomial", "zero_inflated_poisson",
               "zero_inflated_negbinomial", "gaussian",
               "hurdle_gamma", "zero_inflated_beta")
  expect_setequal(covered, unname(bayesnec:::mod_fams))
})

test_that("the slope and beta observations hold over the admissible equations", {
  # The roxygen records two observations rather than a per-parameter column.
  # They are asserted here over the equations that are actually admissible for
  # a (0, 1) response, which is the domain they are stated for -- the two the
  # numerical derivation could not baseline, nechormepwr and nechorme4pwr, are
  # excluded by that gate anyway.
  admissible <- suppressMessages(
    bayesnec:::check_models(models()$all, validate_family("Beta"))
  )
  base <- list(top = 0.6, bot = 0.2, nec = 2, ec50 = 2,
               beta = 0, slope = -5, d = 0, f = 0)
  xs <- seq(0, 6, length.out = 200)
  leaves_range <- function(model, par, grid) {
    pars <- names(get(paste0("bf_", model))[[2]])
    any(vapply(grid, function(v) {
      p_i <- base[pars]
      p_i[[par]] <- v
      mu <- eval_mu(model, as.data.frame(p_i), xs)
      mu <- mu[is.finite(mu)]
      length(mu) > 0 && (max(mu) > 1 + 1e-9 || min(mu) < -1e-9)
    }, logical(1)))
  }
  has_par <- function(model, par) {
    par %in% names(get(paste0("bf_", model))[[2]])
  }

  # slope is exponentiated everywhere, but it sets a level, and in four of the
  # five admissible equations carrying it a deviation alone takes the mean
  # above one
  slope_models <- admissible[vapply(admissible, has_par, logical(1), "slope")]
  expect_setequal(slope_models, c("nechorme", "nechorme4", "nechormepwr01",
                                  "ecxhormebc4", "ecxhormebc5"))
  for (m in setdiff(slope_models, "nechormepwr01")) {
    expect_true(leaves_range(m, "slope", seq(-5, 5, length.out = 25)), info = m)
  }
  # and cannot in nechormepwr01, whose factor is bounded by max(top, 1)
  expect_false(leaves_range("nechormepwr01", "slope",
                            seq(-25, 25, length.out = 60)))

  # beta enters through a factor bounded in (0, 1] in every admissible
  # equation, so a deviation on it alone never leaves the range
  beta_models <- admissible[vapply(admissible, has_par, logical(1), "beta")]
  for (m in beta_models) {
    expect_false(leaves_range(m, "beta", seq(-25, 25, length.out = 60)),
                 info = m)
  }
  # neclinhorme is the equation where it does not hold, and it is excluded on
  # below_zero rather than on anything to do with beta
  expect_false("neclinhorme" %in% admissible)
  expect_true(leaves_range("neclinhorme", "beta", seq(-5, 5, length.out = 25)))
  # 21 of the 23 equations carry beta
  expect_equal(sum(vapply(models()$all, has_par, logical(1), "beta")), 21)
  expect_setequal(models()$all[!vapply(models()$all, has_par, logical(1), "beta")],
                  c("neclin", "ecxlin"))
})


# ---- #257, the scale a group-level deviation is applied on -------------------

test_that("ogl_transform_kind picks the scale from the family's support", {
  # Gate 1: the likelihood constrains mu and the link cannot keep it inside.
  expect_equal(ogl_transform_kind("nec3param", validate_family("Beta")),
               "logit")
  expect_equal(ogl_transform_kind("nec3param", validate_family("binomial")),
               "logit")
  expect_equal(
    ogl_transform_kind("nec3param", validate_family("beta_binomial")), "logit"
  )
  expect_equal(ogl_transform_kind("nec3param", validate_family("Gamma")),
               "log")
  expect_equal(ogl_transform_kind("nec3param", validate_family("poisson")),
               "log")
  # gaussian is unconstrained, so there is nothing to protect against and the
  # additive offset is kept. #245 measured 0% divergent for exactly this case.
  expect_equal(ogl_transform_kind("nec3param", validate_family("gaussian")),
               "none")
  # A link that already maps into the support does the same job.
  expect_equal(
    ogl_transform_kind("nec3param",
                       validate_family(Beta(link = "logit"),
                                       link_source = "chosen")),
    "none"
  )
})

test_that("ogl_transform_kind refuses the equations the transform is undefined for", {
  # Gate 2, and it is a blocker rather than a caveat. These keep the additive
  # offset and the raised adapt_delta permanently.
  beta <- validate_family("Beta")
  # Unbounded below: log and logit of a negative mean are both NaN.
  for (m in c("neclin", "neclinhorme", "ecxlin")) {
    expect_equal(ogl_transform_kind(m, beta), "none")
    expect_equal(ogl_transform_kind(m, validate_family("Gamma")), "none")
  }
  # Can exceed 1: logit is NaN, and the collapsed form has a pole at
  # o = log((m - 1) / m) once m > 1 and changes sign across it. ALL SIX
  # hormesis equations with an excess term are excluded, not only the two whose
  # excess carries no coefficient -- the gate needs the mean to be provably
  # strictly inside (0, 1), and "the fit can shrink the term" is not that.
  # An earlier version of this test asserted "logit" for nechorme on the
  # grounds that scaled hormesis is bounded by top; model_mu_ranges()'s own
  # documentation says otherwise, and it is right.
  for (m in c("nechormepwr", "nechorme4pwr", "nechorme", "nechorme4",
              "ecxhormebc4", "ecxhormebc5")) {
    expect_equal(ogl_transform_kind(m, beta), "none")
  }
  # Saturates at exactly 1, where logit is Inf.
  expect_equal(ogl_transform_kind("nechormepwr01", beta), "none")
  # The exclusion is specific to the unit interval: an excess above 1 is no
  # obstacle to a log transform, which needs only positivity.
  expect_equal(ogl_transform_kind("nechorme", validate_family("Gamma")), "log")
  expect_equal(ogl_transform_kind("ecxhormebc5", validate_family("poisson")),
               "log")
  # A bot fixed at 0 is not a problem: the mean is still strictly inside.
  expect_equal(ogl_transform_kind("ecxwb1p3", beta), "logit")
  # Degenerate input.
  expect_equal(ogl_transform_kind("nec3param", NULL), "none")
  expect_equal(ogl_transform_kind("notamodel", beta), "none")
  expect_equal(ogl_transform_kind(character(0), beta), "none")
})

test_that("can_exceed_one is stronger than unscaled_excess, and is what gates", {
  # The two columns answer different questions and must not be conflated.
  # unscaled_excess is about admissibility -- can the fit shrink the excess --
  # and check_models() is right to admit an equation whose excess it can.
  # can_exceed_one is about whether a logit is defined, and holds for all six.
  tab <- model_mu_ranges()
  excess <- tab$model[tab$unscaled_excess]
  exceeds <- tab$model[tab$can_exceed_one]
  expect_setequal(excess, c("nechormepwr", "nechorme4pwr"))
  expect_setequal(exceeds, c("nechormepwr", "nechorme4pwr", "nechorme",
                             "nechorme4", "ecxhormebc4", "ecxhormebc5"))
  # Strictly stronger: everything unscaled_excess flags, can_exceed_one flags.
  expect_true(all(excess %in% exceeds))
  expect_false(all(exceeds %in% excess))
  # And the pole this exists to avoid is real.
  ev <- function(m, o) m * exp(o) / (1 - m + m * exp(o))
  expect_gt(ev(1.5, -1.0986), 1e4)
  expect_lt(ev(1.5, -1.1), 0)
  # while an equation the gate admits is well behaved over the same range
  expect_true(all(vapply(c(-1.0986, -1.1, -5, 5),
                         function(o) ev(0.7, o) > 0 && ev(0.7, o) < 1,
                         logical(1))))
})

test_that("the transform is collapsed, and is the identity at zero deviation", {
  # Never the literal inv_logit(logit(m) + o) sandwich: logit(m) underflows to
  # -Inf once the decay term exceeds about 709, and inv_logit(-Inf + o) is
  # exactly 0, which fails the likelihood's positivity check as surely as
  # mu > 1 does. The collapsed forms are stable as m -> 0.
  expect_false(grepl("logit", ogl_transform_expr("logit")))
  expect_false(grepl("log\\(", ogl_transform_expr("log")))
  expect_error(ogl_transform_expr("nonsense"))

  ev <- function(kind, m, o) {
    eval(str2lang(ogl_transform_expr(kind, "m", "o")), list(m = m, o = o))
  }
  # A zero-centred deviation leaves the curve alone, which is what makes top,
  # bot, nec and beta keep their meanings.
  expect_equal(ev("logit", 0.7, 0), 0.7)
  expect_equal(ev("log", 3.2, 0), 3.2)
  # The mean stays strictly inside its support across everything the prior
  # makes reachable. The ogl prior is normal(0, s) with s of order 0.5 for a
  # proportion, so +/- 20 is about forty standard deviations.
  for (o in c(-20, -5, -1, 1, 5, 20)) {
    expect_gt(ev("logit", 0.7, o), 0)
    expect_lt(ev("logit", 0.7, o), 1)
    expect_gt(ev("log", 3.2, o), 0)
    expect_true(is.finite(ev("log", 3.2, o)))
  }
  # In exact arithmetic the logit form cannot reach 1 for any finite deviation.
  # In double precision it rounds to 1 once (1 - m) / (m * exp(o)) falls below
  # 2^-53, which for m = 0.7 is about o = 36 -- seventy standard deviations of
  # the prior. Asserted rather than claimed, so the limit of the guarantee is
  # recorded rather than implied away.
  expect_lt(ev("logit", 0.7, 35), 1)
  expect_equal(ev("logit", 0.7, 40), 1)
  # The log form overflows to Inf only beyond exp()'s own range.
  expect_true(is.finite(ev("log", 3.2, 700)))
  expect_false(is.finite(ev("log", 3.2, 710)))
  # Stable in the tail of a declining curve, which is the region that broke the
  # sandwich form.
  expect_gt(ev("logit", 1e-300, 5), 0)
  expect_true(is.finite(ev("logit", 1e-300, 5)))
  # It is the odds that scale: odds(mu) = odds(m) * exp(o).
  m <- 0.4
  o <- 0.8
  expect_equal((ev("logit", m, o) / (1 - ev("logit", m, o))),
               (m / (1 - m)) * exp(o))
})


# ---- #294, a group-level deviation on a single parameter ---------------------

test_that("par_transform_kind picks the scale from the family alone", {
  expect_equal(par_transform_kind(validate_family("Beta")), "logit")
  expect_equal(par_transform_kind(validate_family("binomial")), "logit")
  expect_equal(par_transform_kind(validate_family("beta_binomial")), "logit")
  expect_equal(par_transform_kind(validate_family("bernoulli")), "logit")
  expect_equal(par_transform_kind(validate_family("Gamma")), "log")
  expect_equal(par_transform_kind(validate_family("poisson")), "log")
  expect_equal(par_transform_kind(validate_family("negbinomial")), "log")
  # gaussian is unconstrained, so top and bot are not against a boundary and
  # the additive deviation is kept. The issue's isolating arm: the same term on
  # the same data gives 51 divergent transitions of 2000 under Beta and none
  # under gaussian.
  expect_equal(par_transform_kind(validate_family("gaussian")), "none")
  # A link that already maps into the support does the job itself.
  expect_equal(
    par_transform_kind(validate_family(Beta(link = "logit"),
                                       link_source = "chosen")),
    "none"
  )
  expect_equal(par_transform_kind(NULL), "none")
})

test_that("the parameter gate is not the mean gate", {
  # The deliberate divergence from ogl_transform_kind(). That function needs the
  # MEAN provably strictly inside (0, 1) and so refuses every equation whose
  # mean can reach or pass 1. A parameter is not the mean: define_prior() bounds
  # top and bot to the family's support with lb and ub whatever equation they
  # appear in, so the transform is defined for them there. Those equations keep
  # the raised adapt_delta for the separate reason that their mean can leave the
  # support with every parameter inside it -- see add_brm_defaults().
  beta <- validate_family("Beta")
  hormesis <- c("nechorme", "nechorme4", "nechormepwr", "nechorme4pwr",
                "nechormepwr01", "ecxhormebc4", "ecxhormebc5")
  for (m in hormesis) {
    expect_equal(ogl_transform_kind(m, beta), "none")
  }
  expect_equal(par_transform_kind(beta), "logit")
  # And the equations that are unbounded below have no bot parameter to
  # transform, so the two gates cannot disagree about them in practice.
  for (m in c("neclin", "neclinhorme", "ecxlin")) {
    expect_false("bot" %in% names(get(paste0("bf_", m))[[2]]))
  }
})

test_that("only the response-scale parameters are transformed", {
  # nec and ec50 are on the predictor scale and are routinely negative on a log
  # predictor, so log and logit of them are undefined; beta, slope, d and f are
  # dimensionless and enter through an exponential. None of them is bounded by
  # the likelihood, so none needs a transform, and the issue measured 0
  # divergent transitions of 2000 for a (nec | group) term at Stan's default
  # adapt_delta of 0.8.
  expect_setequal(par_transform_pars(), c("top", "bot"))
  for (p in c("top", "bot")) {
    expect_true(par_is_transformed(p, "logit"))
    expect_true(par_is_transformed(p, "log"))
    expect_false(par_is_transformed(p, "none"))
  }
  for (p in c("nec", "ec50", "beta", "slope", "d", "f")) {
    expect_false(par_is_transformed(p, "logit"))
    expect_false(par_is_transformed(p, "none"))
  }
})

test_that("the generated term names are reserved and are the ones generated", {
  expect_setequal(generated_term_names(),
                  c("bnecmu", "ogl", "topgl", "bnectop", "botgl", "bnecbot"))
  expect_equal(unname(par_gl_names("bot")), c("botgl", "bnecbot"))
  expect_equal(unname(par_gl_names("top")), c("topgl", "bnectop"))
  # The parameter itself keeps its name, which is what leaves b_bot_Intercept
  # and every estimate function that reads it untouched.
  expect_false("bot" %in% generated_term_names())
  expect_false("top" %in% generated_term_names())
})

test_that("the parameter transform is the identity at zero deviation", {
  # Same collapsed expression as the mean transform, applied to the parameter,
  # so the same two properties are what matter: it does not change the model
  # when the deviation is zero, and no deviation can put the parameter outside
  # its support. bot lives near zero, which is exactly where the literal
  # inv_logit(logit(m) + o) sandwich fails.
  ev <- function(kind, m, o) {
    eval(str2lang(ogl_transform_expr(kind, "bot", "botgl")),
         list(bot = m, botgl = o))
  }
  expect_equal(ev("logit", 0.02, 0), 0.02)
  expect_equal(ev("log", 0.02, 0), 0.02)
  for (o in c(-20, -5, -1, 1, 5, 20)) {
    expect_gt(ev("logit", 0.02, o), 0)
    expect_lt(ev("logit", 0.02, o), 1)
    expect_gt(ev("log", 0.02, o), 0)
  }
  # A bot estimated at 1e-8 is still strictly inside after a deviation of forty
  # prior standard deviations, which is what the additive form cannot promise.
  expect_gt(ev("logit", 1e-8, -20), 0)
  expect_lt(ev("logit", 1e-8, 20), 1)
})


test_that("mu_confined_by_pars tests the excess term on every support", {
  # The raise gate. It must not delegate to ogl_transform_kind(), which tests
  # can_exceed_one on the (0, 1) branch only -- there the question is whether a
  # logit is defined -- and on (0, Inf) returns "log" after testing below_zero
  # alone. An excess term in exp(slope) * x makes the mean negative for a
  # sufficiently negative predictor: nechorme's mean is negative for
  # x < -top / exp(slope), and crf(log(x), ...) supplies a negative predictor as
  # a matter of course.
  hormesis <- c("nechorme", "nechorme4", "nechormepwr", "nechorme4pwr",
                "nechormepwr01", "ecxhormebc4", "ecxhormebc5")
  for (m in c(hormesis, "neclin", "neclinhorme", "ecxlin")) {
    expect_false(mu_confined_by_pars(m))
  }
  for (m in c("nec3param", "nec4param", "necsigm", "ecxexp", "ecxsigm",
              "ecx4param", "ecxwb1", "ecxwb2", "ecxwb1p3", "ecxwb2p3",
              "ecxll5", "ecxll4", "ecxll3")) {
    expect_true(mu_confined_by_pars(m))
  }
  # Every equation is decided, and the two functions agree only where the
  # support is the unit interval.
  expect_setequal(c(models()$all),
                  model_mu_ranges()$model)
  beta <- validate_family("Beta")
  for (m in hormesis) {
    expect_equal(ogl_transform_kind(m, beta), "none")
    # The divergence this test exists for.
    expect_equal(ogl_transform_kind(m, validate_family("Gamma")), "log")
  }
  expect_false(mu_confined_by_pars(NULL))
  expect_false(mu_confined_by_pars("notamodel"))
})

test_that("group_zero_intercepts names ogl and nothing else", {
  # A transformed parameter deviation is written botgl ~ 0 + (1 | group) and has
  # no population intercept to start at zero. ogl does.
  beta <- validate_family("Beta")
  expect_equal(group_zero_intercepts(list(nlpars = "ogl", ogl = TRUE), beta),
               "ogl")
  expect_equal(
    group_zero_intercepts(list(nlpars = c("top", "bot"), ogl = FALSE), beta),
    character(0)
  )
  expect_equal(
    group_zero_intercepts(list(nlpars = c("bot", "ogl"), ogl = TRUE), beta),
    "ogl"
  )
  expect_equal(group_zero_intercepts(NULL, beta), character(0))
})
