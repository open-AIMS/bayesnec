# --- #256: one statement of what mu may be, and what each model can produce ---
# The point of these tests is that the two artefacts cannot silently disagree
# with the code they describe. #170 was opened because ?models and
# check_models() had drifted apart; a table that is written down and never
# checked is the same failure with an extra file.

test_that("mu_support answers on family and link together", {
  s <- bayesnec:::mu_support
  expect_equal(s(validate_family("Beta")), c(0, 1))
  expect_equal(s(validate_family("binomial")), c(0, 1))
  expect_equal(s(validate_family("beta_binomial")), c(0, 1))
  expect_equal(s(validate_family("bernoulli")), c(0, 1))
  expect_equal(s(validate_family("zero_inflated_beta")), c(0, 1))
  expect_equal(s(validate_family("Gamma")), c(0, Inf))
  expect_equal(s(validate_family("poisson")), c(0, Inf))
  expect_equal(s(validate_family("negbinomial")), c(0, Inf))
  expect_equal(s(validate_family("hurdle_gamma")), c(0, Inf))
  expect_equal(s(validate_family("zero_inflated_poisson")), c(0, Inf))
  # gaussian is unconstrained: the data enter through the residual only
  expect_equal(s(validate_family("gaussian")), c(-Inf, Inf))
  # and the link is asked as well as the family, because mu is then the linear
  # predictor whatever the family
  expect_equal(s(gaussian(link = "log")), c(-Inf, Inf))
  expect_equal(s(Beta(link = "logit")), c(-Inf, Inf))
  expect_equal(s(NULL), c(-Inf, Inf))
})

test_that("mu_is_constrained is the coarsest reading of mu_support", {
  f <- bayesnec:::mu_is_constrained
  expect_true(f(validate_family("Beta")))
  expect_true(f(validate_family("Gamma")))
  expect_false(f(validate_family("gaussian")))
  expect_false(f(Beta(link = "logit")))
})

test_that("every family bayesnec accepts has a support", {
  # A new family added to mod_fams without a decision here would silently take
  # the (0, Inf) fallback.
  # the values of mod_fams are the constructor names; its names are the family
  # tags brms reports, and "beta" as a tag resolves to base::beta()
  for (fam in unname(bayesnec:::mod_fams)) {
    s <- bayesnec:::mu_support(validate_family(fam))
    expect_length(s, 2)
    expect_true(s[1] < s[2])
  }
})

test_that("the model range table covers every model exactly once", {
  tab <- bayesnec:::model_mu_ranges()
  expect_setequal(tab$model, models()$all)
  expect_equal(anyDuplicated(tab$model), 0)
  # every unsafe parameter named is a parameter that model actually has
  for (i in seq_len(nrow(tab))) {
    pars <- names(get(paste0("bf_", tab$model[i]))[[2]])
    expect_true(all(tab$unsafe[[i]] %in% pars),
                info = tab$model[i])
  }
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
  # and every model that carries slope can exceed 1 for some slope, which is
  # why the distinction is needed at all rather than a plain "can exceed"
  for (m in c("nechorme", "nechorme4", "ecxhormebc4", "ecxhormebc5")) {
    expect_true(max(sweep_mu(m, slope_val = 2)) > 1, info = m)
    expect_false(bayesnec:::model_mu_ranges()$unscaled_excess[
      bayesnec:::model_mu_ranges()$model == m], info = m)
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
  # nechormepwr01's hormetic factor rises towards 1 for top < 1, which is the
  # intended increase on a 0-1 response, and falls towards 1 for top > 1, where
  # it expresses a decline and cannot represent a mean above top. mu stays
  # positive either way, so no support flag would exclude it.
  fac <- function(top, x, slope = 0)
    1 / (1 + ((1 / top) - 1) * exp(-exp(slope) * x))
  x <- c(0, 0.5, 1, 2, 4, 10)
  rising <- fac(0.3, x)
  expect_true(all(diff(rising) > 0))
  expect_lt(max(rising), 1 + 1e-8)
  falling <- fac(20, x)
  expect_true(all(diff(falling) < 0))
  expect_equal(falling[1], 20)
  # it approaches one from above rather than reaching it
  expect_gt(falling[length(falling)], 1)
  expect_lt(fac(20, 100), 1 + 1e-8)
  # and it is the only model flagged that way
  tab <- bayesnec:::model_mu_ranges()
  expect_equal(tab$model[tab$ceiling_at_one], "nechormepwr01")
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
})
