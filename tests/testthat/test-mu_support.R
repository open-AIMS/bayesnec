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
                               "ceiling_at_one", "zero_asymptote"))
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

  # appropriateness: the Gaussian exclusion. Keyed on the family irrespective
  # of link, and the subject of #206; asserted here as current behaviour, not
  # endorsed.
  expect_setequal(dropped(validate_family("gaussian")),
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
