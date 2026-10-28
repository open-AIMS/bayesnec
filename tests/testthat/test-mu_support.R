# Check the support and equation-range tables against the formulas and
# family restrictions so their descriptions cannot drift from the code.

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
       hurdle_poisson = c(0, Inf), hurdle_negbinomial = c(0, Inf),
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
  # Require an explicit entry for every supported family.
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
  expect_true(f(validate_family("hurdle_poisson")))
  expect_true(f(validate_family("hurdle_negbinomial")))
  # gaussian has nothing to violate, whatever the link
  expect_false(f(validate_family("gaussian")))
  expect_false(f(gaussian(link = "log")))
  # a link whose inverse maps into the support guarantees a valid mean
  expect_false(f(Beta(link = "logit")))
  expect_false(f(binomial(link = "probit")))
  expect_false(f(binomial(link = "cloglog")))
  expect_false(f(poisson(link = "sqrt")))
  expect_false(f(Gamma(link = "log")))

  # A log link ensures positivity but can exceed one. For a Beta mean that
  # makes (1 - mu) * phi invalid even though the same link suits count means.
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
  # Read the second block’s own link: its identity-link probability can leave
  # support even when the mu block has a support-preserving link.
  f <- bayesnec:::mu_is_constrained
  hg <- validate_family("hurdle_gamma")
  expect_true(f(hg, dpar = "hu"))
  hg_log <- hg; hg_log$link <- "log"
  expect_false(f(hg_log))
  expect_true(f(hg_log, dpar = "hu"))

  # Use logit mu with identity zi to distinguish the two link lookups.
  # The default hurdle Gamma would give TRUE under either lookup.
  zib <- validate_family("zero_inflated_beta")
  zib$link <- "logit"
  expect_false(f(zib))
  expect_true(f(zib, dpar = "zi"))
  expect_equal(zib$link_zi, "identity")
})

test_that("no link any accepted family takes falls to the default unintended", {
  # Enumerate constructor-accepted links to catch an accidental fallback to
  # the unconstrained range.
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
  # Include ecxflat even though it belongs to no model group.
  expect_setequal(tab$model, bayesnec:::equation_names())
  expect_equal(anyDuplicated(tab$model), 0)
  expect_setequal(names(tab), c("model", "below_zero", "unscaled_excess",
                               "can_exceed_one", "ceiling_at_one",
                               "zero_asymptote"))
})

# Evaluate formulas over admissible parameter grids to check the range flags.

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
  # Without slope, these curves are a bounded multiple of top or a convex
  # combination of top and bot, so bounded parameters keep the mean below one.
  for (m in tab$model[!has_slope]) {
    expect_lte(max(sweep_mu(m)), 1 + 1e-8, label = m)
    expect_false(tab$unscaled_excess[tab$model == m], info = m)
  }
})

test_that("zero_asymptote is what the formulas do, and matches mod_groups", {
  tab <- bayesnec:::model_mu_ranges()
  # Compare the distant response with each curve’s own level: necsigm and
  # ecxsigm approach zero slowly, making a fixed absolute cutoff unsuitable.
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
  expect_setequal(tab$model[tab$zero_asymptote], mod_groups$zero_bounded)
})

test_that("ceiling_at_one is a saturating hormetic term, not a support failure", {
  # Read the actual formula and put nec beyond the grid so the decay factor
  # is one. The remaining response isolates the hormetic term.
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

# Check that check_models() applies the restrictions described by the tables.

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

  # A Gaussian likelihood permits negative observations around a zero-bounded
  # mean, so identity-link Gaussian fits need no such equation exclusion.
  expect_length(dropped(validate_family("gaussian")), 0)
  # The log-link restriction remains a separate condition.
  expect_setequal(dropped(gaussian(link = "log")),
                  tab$model[tab$zero_asymptote])

  # A hurdle Gamma has positive mu and unit-interval hu. Apply the union
  # of the restrictions required by those two blocks.
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
  # Include the two-block families in the completeness check.
  covered <- c("Beta", "binomial", "beta_binomial", "bernoulli",
               "Gamma", "poisson", "negbinomial", "zero_inflated_poisson",
               "zero_inflated_negbinomial", "gaussian",
               "hurdle_gamma", "hurdle_poisson", "hurdle_negbinomial",
               "zero_inflated_beta")
  expect_setequal(covered, unname(bayesnec:::mod_fams))
})

test_that("the slope and beta observations hold over the admissible equations", {
  # Check slope and beta over the equations admissible for unit-interval
  # responses. The gate excludes nechormepwr and nechorme4pwr, which cannot
  # be baselined by the numerical comparison.
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
  expect_equal(sum(vapply(models()$all, has_par, logical(1), "beta")), 21)
  expect_setequal(models()$all[!vapply(models()$all, has_par, logical(1), "beta")],
                  c("neclin", "ecxlin"))
})



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
  # Keep additive deviations for unconstrained Gaussian means.
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
  # Keep additive offsets and the adapt_delta raise where the mean transform
  # is undefined.
  beta <- validate_family("Beta")
  # Unbounded below: log and logit of a negative mean are both NaN.
  for (m in c("neclin", "neclinhorme", "ecxlin")) {
    expect_equal(ogl_transform_kind(m, beta), "none")
    expect_equal(ogl_transform_kind(m, validate_family("Gamma")), "none")
  }
  # A mean above one makes logit undefined and the collapsed expression can
  # have a pole. A shrinkable excess term does not guarantee a mean in (0, 1).
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
  expect_equal(ogl_transform_kind("nec3param", NULL), "none")
  expect_equal(ogl_transform_kind("notamodel", beta), "none")
  expect_equal(ogl_transform_kind(character(0), beta), "none")
})

test_that("can_exceed_one is stronger than unscaled_excess, and is what gates", {
  # unscaled_excess tests whether a fit can shrink the excess. can_exceed_one
  # tests whether any such mean can make logit undefined; it covers more equations.
  tab <- model_mu_ranges()
  excess <- tab$model[tab$unscaled_excess]
  exceeds <- tab$model[tab$can_exceed_one]
  expect_setequal(excess, c("nechormepwr", "nechorme4pwr"))
  expect_setequal(exceeds, c("nechormepwr", "nechorme4pwr", "nechorme",
                             "nechorme4", "ecxhormebc4", "ecxhormebc5"))
  # Strictly stronger: everything unscaled_excess flags, can_exceed_one flags.
  expect_true(all(excess %in% exceeds))
  expect_false(all(exceeds %in% excess))
  # Evaluate both sides of the pole.
  ev <- function(m, o) m * exp(o) / (1 - m + m * exp(o))
  expect_gt(ev(1.5, -1.0986), 1e4)
  expect_lt(ev(1.5, -1.1), 0)
  # The same deviations must stay in range for an admissible mean.
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
  # Double precision can round the transformed mean to one for large finite
  # deviations. Check this limit explicitly rather than promising strict bounds
  # for every floating-point input.
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



test_that("par_transform_kind picks the scale from the family alone", {
  expect_equal(par_transform_kind(validate_family("Beta")), "logit")
  expect_equal(par_transform_kind(validate_family("binomial")), "logit")
  expect_equal(par_transform_kind(validate_family("beta_binomial")), "logit")
  expect_equal(par_transform_kind(validate_family("bernoulli")), "logit")
  expect_equal(par_transform_kind(validate_family("Gamma")), "log")
  expect_equal(par_transform_kind(validate_family("poisson")), "log")
  expect_equal(par_transform_kind(validate_family("negbinomial")), "log")
  # An unconstrained Gaussian parameter keeps its additive deviation.
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
  # The parameter transform uses bounds on top and bot, not bounds on the
  # whole mean. A hormetic mean can still leave support, so its adapt_delta
  # raise remains necessary.
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
  # nec and ec50 use the predictor scale; beta, slope, d and f are unbounded
  # parameters used through exponentials. Only top and bot need these
  # likelihood-support transforms.
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
  # Do not use the mean-transform gate to decide the adapt_delta raise.
  # A hormetic excess can violate positive support on negative predictors,
  # including those produced by crf(log(x)).
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
  # Cover every equation, including the different Gamma gate decisions.
  expect_setequal(equation_names(), model_mu_ranges()$model)
  beta <- validate_family("Beta")
  for (m in hormesis) {
    expect_equal(ogl_transform_kind(m, beta), "none")
    # The Gamma mean transform can be log even when parameters do not confine it.
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

test_that("ecxflat is confined as top is, and is not zero-bounded (#419)", {
  # ecxflat has mean top, so its bounds determine the transform and whether
  # an excursion-specific adapt_delta raise is needed.
  tab <- bayesnec:::model_mu_ranges()
  row <- tab[tab$model == "ecxflat", ]
  expect_false(any(unlist(row[setdiff(names(row), "model")])))
  expect_true(bayesnec:::mu_confined_by_pars("ecxflat"))
  expect_identical(bayesnec:::ogl_transform_kind("ecxflat",
                                                 validate_family("Beta")),
                   "logit")
  expect_identical(bayesnec:::ogl_transform_kind("ecxflat",
                                                 validate_family("Gamma")),
                   "log")
  expect_identical(bayesnec:::ogl_transform_kind("ecxflat", gaussian()),
                   "none")
  # The formula agrees: the mean is top whatever the predictor, an infinite
  # one included, so it can be neither below top nor above it.
  mus <- eval_mu("ecxflat", data.frame(top = 0.4), c(-Inf, 0, 3, Inf))
  expect_identical(mus, rep(0.4, 4))
})
