# --- #256: one statement of what mu may be, and what each model can produce ---
# The point of these tests is that the two artefacts cannot silently disagree
# with the code they describe. #170 was opened because ?models and
# check_models() had drifted apart; a table that is written down and never
# checked is the same failure with an extra file.

expected_supports <- function() {
  # Keyed on the family tag, so a family added to mod_fams without a decision
  # here fails the completeness test below rather than taking a fallback.
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

test_that("mu_is_constrained asks the link as well as the family", {
  f <- bayesnec:::mu_is_constrained
  # identity passes the linear predictor straight into the likelihood
  expect_true(f(validate_family("Beta")))
  expect_true(f(validate_family("Gamma")))
  expect_true(f(validate_family("beta_binomial")))
  expect_true(f(validate_family("hurdle_gamma")))
  # gaussian has nothing to violate
  expect_false(f(validate_family("gaussian")))
  # brms applies the inverse link before the likelihood, so under these the mean
  # is valid by construction whatever is proposed
  expect_false(f(Beta(link = "logit")))
  expect_false(f(gaussian(link = "log")))
  expect_false(f(binomial(link = "probit")))
  expect_false(f(binomial(link = "cloglog")))
  expect_false(f(poisson(link = "sqrt")))
  # inverse is the exception: inv(eta) is negative wherever eta is, so a Gamma
  # fitted on it can still be handed an invalid mean. Confirmed against the
  # generated Stan code, which emits `mu = inv(mu)` then
  # `gamma_lpdf(Y | shape, shape ./ mu)`.
  expect_true(f(Gamma(link = "inverse")))
  expect_false(f(NULL))
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
