# Structural tests for best_crossed() and bnec_joint(). Neither requires
# sampling: they are built from a mock carrying only the pieces each function
# reads, in the style of test-bayesnechurdlefit-methods.R.

mock_manec <- function(models, weights) {
  mod_stats <- data.frame(model = models, wi = weights,
                          row.names = models)
  structure(list(mod_fits = stats::setNames(vector("list", length(models)),
                                            models),
                 success_models = models, mod_stats = mod_stats,
                 ne_type = "N(S)EC"),
            class = c("bayesmanecfit", "bnecfit"))
}

mock_crossed <- function() {
  structure(list(growth = mock_manec(c("nec3param", "ecx4param"),
                                     c(0.3, 0.7)),
                 survival = mock_manec(c("nec3param", "ecx4param"),
                                       c(0.8, 0.2)),
                 data = data.frame(x = 1:4, y = c(2, 1, 0, 0)),
                 formula = bnf(y ~ crf(x, c("nec3param", "ecx4param"))),
                 y_var = "y", n_exposed = 4L, n_dead = 2L),
            class = c("bayesnechurdlefit", "bnecfit"))
}

test_that("crossed_weights is the outer product of the component weights", {
  w <- crossed_weights(mock_crossed())
  expect_equal(dim(w), c(2L, 2L))
  expect_equal(rownames(w), c("nec3param", "ecx4param"))
  expect_equal(sum(w), 1)
  expect_equal(unname(w["ecx4param", "nec3param"]), 0.7 * 0.8)
})

test_that("best_crossed returns the highest weighted pair", {
  best <- best_crossed(mock_crossed())
  expect_equal(best$growth, "ecx4param")
  expect_equal(best$survival, "nec3param")
  expect_equal(best$weight, 0.7 * 0.8)
})

test_that("best_crossed handles a single-model component", {
  o <- mock_crossed()
  o$survival <- structure(list(model = "nec4param", ne_type = "NEC"),
                          class = c("bayesnecfit", "bnecfit"))
  best <- best_crossed(o)
  expect_equal(best$growth, "ecx4param")
  expect_equal(best$survival, "nec4param")
  expect_equal(best$weight, 0.7)
})

test_that("crossed_weights and bnec_joint reject the wrong class", {
  expect_error(crossed_weights(1:3), "class bayesnechurdlefit")
  expect_error(bnec_joint(1:3), "class bayesnechurdlefit")
  # The generic accepts two classes, so the refusal has to name both: a user
  # who reached it from bnec_group() is told nothing by the hurdle class alone.
  expect_error(bnec_joint(1:3), "bayesnecgroupfit")
  expect_error(bnec_joint(manec_example), "bayesnecgroupfit")
})

test_that("bnec_joint errors where there is no two-block family", {
  o <- mock_crossed()
  # a growth component fitted with a family that has no hurdle counterpart
  o$growth <- structure(
    list(model = "nec3param", ne_type = "NEC",
         fit = structure(list(family = gaussian()), class = "brmsfit")),
    class = c("bayesnecfit", "bnecfit")
  )
  expect_error(bnec_joint(o), "no two-block family")
})

# The joint refit of a grouped fit (#382). Everything that can be decided
# before a model is compiled is decided against a mock carrying only the model
# weights and a data frame with no response column, which stops fit_bayesnec()
# at its model frame. The fitted fixture below is shared by the tests that
# genuinely need a posterior.

mock_group_fit <- function(weights) {
  fits <- lapply(weights, function(w) mock_manec(names(w), unname(w)))
  structure(
    list(fits = fits, group_var = "site", levels = names(weights),
         formula = bnf(y ~ crf(x, c("nec3param", "nec4param", "ecx4param"))),
         data = data.frame(x = c(1, 2, 3, 4), site = c("a", "a", "b", "b"),
                           stringsAsFactors = FALSE),
         family = gaussian(), n = c(2L, 2L), weights_method = "pseudobma"),
    class = c("bayesnecgroupfit", "bnecfit")
  )
}

# nec4param sums to 0.8 of the 2 available and nec3param to 0.7, so nothing
# holds half, and the two levels favour different equations.
spread_group_fit <- function() {
  mock_group_fit(list(
    a = c(nec3param = 0.4, nec4param = 0.35, ecx4param = 0.25),
    b = c(nec4param = 0.45, nec3param = 0.3, ecx4param = 0.25)
  ))
}

test_that("the summed weight adds over levels, a dropped equation as zero", {
  g <- mock_group_fit(list(a = c(nec3param = 0.6, nec4param = 0.4),
                           b = c(nec4param = 0.7, ecx4param = 0.3)))
  w <- joint_equation_weights(g)
  expect_equal(names(w)[1], "nec4param")
  expect_equal(unname(w[["nec4param"]]), 1.1)
  expect_equal(unname(w[["nec3param"]]), 0.6)
  expect_equal(unname(w[["ecx4param"]]), 0.3)
})

test_that("a spread of weight across equations is reported, not refused", {
  msg <- paste(
    capture_messages(try(bnec_joint(spread_group_fit()), silent = TRUE)),
    collapse = ""
  )
  expect_match(msg, "do not agree on an equation")
  expect_match(msg, "\"a\" favours nec3param")
  expect_match(msg, "\"b\" favours nec4param")
  expect_match(msg, "nec4param holds the highest summed weight, 0.4")
  expect_match(msg, "Refitting jointly as one nec4param model")
})

test_that("`model` overrides the summed-weight choice", {
  g <- spread_group_fit()
  msg <- paste(
    capture_messages(try(bnec_joint(g, model = "ecx4param"), silent = TRUE)),
    collapse = ""
  )
  expect_match(msg, "Refitting jointly as one ecx4param model")
  expect_false(grepl("do not agree", msg))
  expect_error(bnec_joint(g, model = c("nec3param", "nec4param")),
               "single equation")
})

level_term_formulas <- function() {
  d <- rbind(transform(nec_data[, c("x", "y")], site = "a"),
             transform(nec_data[, c("x", "y")], site = "b"))
  d$site <- factor(d$site)
  f <- bnf(y ~ crf(x, "nec3param"))
  bdat <- model.frame(f, data = d)
  fam <- validate_family("beta")
  plain <- wrangle_model_formula("nec3param", f, bdat, fam)
  spec <- list(group_var = "site", levels = c("a", "b"),
               nlpars = names(plain[[2]]), disp = TRUE)
  shared_spec <- spec
  shared_spec$disp <- FALSE
  list(data = d, family = fam, plain = plain,
       by_level = wrangle_model_formula("nec3param", f, bdat, fam,
                                        level_spec = spec),
       shared_disp = wrangle_model_formula("nec3param", f, bdat, fam,
                                           level_spec = shared_spec))
}

test_that("a level term replaces the intercept on every curve parameter", {
  fs <- level_term_formulas()
  expect_equal(unname(vapply(fs$plain[[2]], deparse1, character(1))),
               c("top ~ 1", "beta ~ 1", "nec ~ 1"))
  expect_equal(
    unname(vapply(fs$by_level[[2]][c("top", "beta", "nec")], deparse1,
                  character(1))),
    c("top ~ 0 + site", "beta ~ 0 + site", "nec ~ 0 + site")
  )
})

test_that("disp_by_level decides whether the dispersion varies by level", {
  fs <- level_term_formulas()
  expect_true("phi" %in% names(fs$by_level[[2]]))
  expect_false("phi" %in% names(fs$shared_disp[[2]]))
  # Read off the Stan program rather than the formula alone, because what the
  # argument is for is the number of dispersion coefficients declared.
  sc_by <- brms::stancode(fs$by_level, data = fs$data, family = fs$family)
  sc_sh <- brms::stancode(fs$shared_disp, data = fs$data, family = fs$family)
  expect_match(sc_by, "vector[K_phi] b_phi", fixed = TRUE)
  expect_false(grepl("b_phi", sc_sh, fixed = TRUE))
  expect_match(sc_sh, "real<lower=0> phi;", fixed = TRUE)
})

test_that("a disp() term and disp_by_level cannot both model the dispersion", {
  g <- spread_group_fit()
  g$formula <- bnf(y ~ crf(x, "nec3param") + disp("power"))
  expect_error(bnec_joint(g, model = "nec3param"), "disp\\(\\) term")
})

test_that("the level coefficients take the priors and inits of one pass", {
  fs <- level_term_formulas()
  d <- fs$data
  priors <- define_prior("nec3param", fs$family, d$x, d$y)
  brm_args <- list(
    prior = priors,
    init = list(list(b_top = as.array(0.9), b_beta = as.array(0),
                     b_nec = as.array(1)))
  )
  spec <- list(group_var = "site", levels = c("a", "b"),
               nlpars = c("top", "beta", "nec"), disp = TRUE)
  out <- add_level_defaults(brm_args, spec, fs$family, d$y)
  # One value per level for each curve parameter, and every level starts from
  # the value the search found for the model with no level term.
  expect_equal(unname(vapply(out$init[[1]][c("b_top", "b_beta", "b_nec")],
                             length, integer(1))), c(2L, 2L, 2L))
  expect_equal(as.numeric(out$init[[1]]$b_top), c(0.9, 0.9))
  # The curve priors are untouched: brms applies one class "b" row with an
  # nlpar to every coefficient of that parameter.
  pr <- as.data.frame(out$prior)
  expect_equal(nrow(pr[nzchar(pr$nlpar), ]), nrow(as.data.frame(priors)))
  disp_row <- pr[pr$class == "b" & pr$dpar == "phi", ]
  expect_equal(nrow(disp_row), 1L)
  expect_equal(disp_row$prior, "normal(4, 3)")
  expect_equal(as.numeric(out$init[[1]]$b_phi), c(4, 4))
})

test_that("no dispersion prior is added where disp_by_level is FALSE", {
  fs <- level_term_formulas()
  d <- fs$data
  brm_args <- list(prior = define_prior("nec3param", fs$family, d$x, d$y),
                   init = list(list(b_top = as.array(0.9))))
  spec <- list(group_var = "site", levels = c("a", "b"),
               nlpars = c("top", "beta", "nec"), disp = FALSE)
  out <- add_level_defaults(brm_args, spec, fs$family, d$y)
  pr <- as.data.frame(out$prior)
  expect_equal(nrow(pr[pr$class == "b" & pr$dpar == "phi", ]), 0L)
  expect_null(out$init[[1]]$b_phi)
})

# One fixture for every test that needs a posterior. Compiling Stan programs is
# what makes this file slow, and this one costs three of them: two levels and
# the joint refit.
joint_gate_fixture <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) {
      return(cached)
    }
    d <- rbind(
      transform(nec_data[, c("x", "y")], site = "a"),
      transform(nec_data[, c("x", "y")],
                y = pmin(pmax(y * 0.6, 0.001), 0.999), site = "b")
    )
    d$site <- factor(d$site)
    grouped <- suppressWarnings(suppressMessages(
      bnec_group(y ~ crf(x, "nec3param"), data = d, group_var = "site",
                 iter = 2000, warmup = 1000, chains = 2, seed = 7,
                 refresh = 0)
    ))
    joint <- suppressWarnings(suppressMessages(
      bnec_joint(grouped, iter = 2000, warmup = 1000, chains = 2, seed = 7,
                 refresh = 0)
    ))
    cached <<- list(data = d, grouped = grouped, joint = joint)
    cached
  }
})

test_that("the joint refit carries a coefficient per level", {
  skip_on_cran()
  f <- joint_gate_fixture()
  expect_s3_class(f$joint, "bayesnecjointfit")
  expect_equal(f$joint$group_var, "site")
  expect_equal(f$joint$levels, c("a", "b"))
  expect_equal(f$joint$model, "nec3param")
  vars <- brms::variables(f$joint$fit)
  expect_true(all(c("b_top_sitea", "b_top_siteb", "b_beta_sitea",
                    "b_beta_siteb", "b_nec_sitea", "b_nec_siteb",
                    "b_phi_sitea", "b_phi_siteb") %in% vars))
  expect_false("phi" %in% vars)
})

test_that("per-level estimates agree with bnec_group on the same equation", {
  # The phase 1 gate of #382. The two fits are the same model written two ways
  # once the priors match, so their per-level posteriors must agree to Monte
  # Carlo error. A joint posterior tighter than the grouped one means a prior
  # has been applied twice; a wider one means a level coefficient is running on
  # brms's improper default.
  skip_on_cran()
  f <- joint_gate_fixture()
  jf <- suppressWarnings(summary(f$joint$fit))$fixed
  for (lev in c("a", "b")) {
    gf <- suppressWarnings(summary(f$grouped$fits[[lev]]$fit))$fixed
    for (p in c("top", "beta", "nec")) {
      j <- jf[paste0(p, "_site", lev), ]
      g <- gf[paste0(p, "_Intercept"), ]
      mcse <- sqrt(j[["Est.Error"]]^2 / j[["Bulk_ESS"]] +
                     g[["Est.Error"]]^2 / g[["Bulk_ESS"]])
      expect_lt(abs(j[["Estimate"]] - g[["Estimate"]]) / mcse, 5)
      expect_gt(j[["Est.Error"]] / g[["Est.Error"]], 0.8)
      expect_lt(j[["Est.Error"]] / g[["Est.Error"]], 1.25)
    }
  }
})

test_that("the estimators do not report per level for a joint refit yet", {
  # Remove with phase 2 of #382, which gives prediction_grid() a level column
  # and each estimator a row per level. Until then the grid carries no level
  # column, a level term is population-level so re_formula = NA does not drop
  # it, and the prediction stops rather than answering for an unnamed level.
  skip_on_cran()
  f <- joint_gate_fixture()
  expect_error(ecx(f$joint, ecx_val = 10))
  nd <- prediction_grid(f$joint$fit, f$joint$bayesnecformula,
                        resolution = 10)$newdata
  expect_false("site" %in% names(nd))
  expect_error(
    brms::posterior_epred(f$joint$fit, newdata = nd, re_formula = NA),
    "site"
  )
})
