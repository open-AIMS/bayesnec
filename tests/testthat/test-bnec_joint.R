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

test_that("each level is fitted the equation its own weights favour", {
  # The correction of #388. The summed-weight winner imposed one functional
  # form on levels that rejected it; best_crossed() already returns the growth
  # and survival equations separately, and this is that rule over levels.
  g <- spread_group_fit()
  expect_equal(joint_level_equations(g),
               c(a = "nec3param", b = "nec4param"))
  msg <- paste(capture_messages(try(bnec_joint(g), silent = TRUE)),
               collapse = "")
  expect_match(msg, "composing nec3param at \"a\", nec4param at \"b\"")
  expect_match(msg, "each level with its own curve parameters")
})

test_that("`model` forces one equation at every level", {
  g <- spread_group_fit()
  msg <- paste(
    capture_messages(try(bnec_joint(g, model = "ecx4param"), silent = TRUE)),
    collapse = ""
  )
  expect_match(msg, "Refitting jointly as one ecx4param model")
  expect_false(grepl("composing", msg))
  expect_error(bnec_joint(g, model = c("nec3param", "nec4param")),
               "single equation")
})

test_that("levels that agree take the dummy-coded form, not a composition", {
  # The reduction to the special case. Where every level favours the same
  # equation there is nothing to compose, and the message names the single
  # equation the phase 1 route already built.
  g <- mock_group_fit(list(a = c(nec3param = 0.6, nec4param = 0.4),
                           b = c(nec3param = 0.8, nec4param = 0.2)))
  msg <- paste(capture_messages(try(bnec_joint(g), silent = TRUE)),
               collapse = "")
  expect_match(msg, "Refitting jointly as one nec3param model")
  expect_false(grepl("composing", msg))
})

test_that("a group-level term is refused where the equations differ", {
  g <- spread_group_fit()
  g$formula <- bnf(y ~ crf(x, "nec3param") + ogl(tank))
  expect_error(bnec_joint(g), "group-level term cannot be written")
  # and is accepted once one equation is forced at every level
  expect_false(grepl("group-level term cannot be written",
                     paste(capture_messages(try(
                       bnec_joint(g, model = "nec3param"), silent = TRUE)),
                       collapse = "")))
})

test_that("prior and init are refused where the equations differ", {
  # The composed parameter names are internal, so a set written against the
  # equation's own names names nothing in the model. Refused rather than
  # dropped: add_brm_defaults() validates a supplied prior against the
  # representative equation and discards it silently when it does not match.
  g <- spread_group_fit()
  expect_error(bnec_joint(g, prior = brms::prior(normal(0, 1), nlpar = "top")),
               "cannot be supplied where the levels favour different")
  expect_error(bnec_joint(g, init = list(list(b_top = 0.5))),
               "cannot be supplied where the levels favour different")
  # and both are accepted again once one equation is forced everywhere
  expect_false(grepl("cannot be supplied", paste(capture_messages(try(
    bnec_joint(g, model = "nec3param", init = list(list(b_top = 0.5))),
    silent = TRUE)), collapse = "")))
})

test_that("a level tag is legal, unique, and independent of the level order", {
  # brms refuses a non-linear parameter name containing a dot or an underscore
  # (validate_par_formula), so the tag can use neither.
  tags <- level_par_tags(c("site 1", "site-1", "b"))
  expect_false(any(grepl("[._]", tags)))
  expect_equal(length(unique(tags)), 3L)
  # The two levels differing only in punctuation share a stem and are still
  # distinct, and an empty stem is given one.
  expect_true(all(grepl("^site1Lv", tags[1:2])))
  expect_equal(unname(level_par_tags("!!")), "levLv1")
  # The rank is taken over the sorted level set, so reordering the levels does
  # not rename a level's parameters.
  expect_equal(level_par_tags(c("b", "a"))[["a"]],
               level_par_tags(c("a", "b"))[["a"]])
  expect_error(brms::bf(y ~ top_a * x, top_a ~ 1, nl = TRUE),
               "dots or underscores")
})

composed_spec <- function(models = list(a = "nec3param", b = "ecxll5"),
                          disp = TRUE) {
  d <- rbind(transform(nec_data[, c("x", "y")], site = "a"),
             transform(nec_data[, c("x", "y")], site = "b"))
  d$site <- factor(d$site)
  f <- bnf(y ~ crf(x, "nec3param"))
  cd <- compose_level_data(f, d, c("a", "b"), "site")
  spec <- list(group_var = "site", levels = c("a", "b"), models = models,
               composed = TRUE, disp = disp, tags = cd$tags, inds = cd$inds,
               x_ref = cd$x_ref)
  list(data = cd$data, formula = f, spec = spec,
       family = validate_family("beta"))
}

test_that("the composed formula carries one equation per level", {
  cs <- composed_spec()
  bdat <- model.frame(cs$formula, data = cs$data)
  bff <- wrangle_model_formula("nec3param", cs$formula, bdat, cs$family,
                               level_spec = cs$spec)
  pars <- names(bff[[2]])
  expect_true(all(c("topaLv1", "betaaLv1", "necaLv1") %in% pars))
  expect_true(all(c("botbLv2", "topbLv2", "betabLv2", "ec50bLv2", "fbLv2") %in%
                    pars))
  # nec3param has no ec50 and ecxll5 has no nec, so a name from one level's
  # equation must not appear under the other's tag.
  expect_false("ec50aLv1" %in% pars)
  expect_false("necbLv2" %in% pars)
  mu <- deparse1(bff$formula[[3]])
  expect_match(mu, "bnecindaLv1 * (", fixed = TRUE)
  expect_match(mu, "bnecindbLv2 * (", fixed = TRUE)
  # The predictor is masked, not the result: on a row it does not own, a level
  # evaluates its equation at one of its own observed predictor values.
  expect_match(mu, "(1 - bnecindaLv1) * ", fixed = TRUE)
  expect_true(cs$spec$x_ref$a %in% cs$data$x)
  sc <- brms::stancode(bff, data = cs$data, family = cs$family)
  expect_match(sc, "b_topaLv1", fixed = TRUE)
  expect_match(sc, "b_fbLv2", fixed = TRUE)
})

test_that("three levels with colliding stems compose to a valid model", {
  # More than two levels, three different equations, and two level names that
  # differ only in punctuation, which is the case the rank in the tag exists
  # for. No sampling: what is asserted is the model brms is asked to build.
  levs <- c("site 1", "site-1", "b")
  d <- do.call(rbind, lapply(levs, function(l)
    transform(nec_data[, c("x", "y")], site = l)))
  d$site <- factor(d$site, levels = levs)
  f <- bnf(y ~ crf(x, "nec3param"))
  cd <- compose_level_data(f, d, levs, "site")
  spec <- list(group_var = "site", levels = levs,
               models = stats::setNames(
                 list("nec3param", "ecxll5", "nechormepwr01"), levs),
               composed = TRUE, disp = TRUE, tags = cd$tags, inds = cd$inds,
               x_ref = cd$x_ref)
  fam <- validate_family("beta")
  bff <- wrangle_model_formula("nec3param", f, model.frame(f, data = cd$data),
                               fam, level_spec = spec)
  expect_equal(unname(unlist(cd$tags)),
               c("site1Lv2", "site1Lv3", "bLv1"))
  expect_true(all(c("necsite1Lv2", "fsite1Lv3", "slopebLv1") %in%
                    names(bff[[2]])))
  expect_length(unique(names(bff[[2]])), length(names(bff[[2]])))
  expect_match(brms::stancode(bff, data = cd$data, family = fam),
               "b_slopebLv1", fixed = TRUE)
  out <- suppressMessages(
    add_level_defaults(list(chains = 1, seed = 1), spec, fam, cd$data$y,
                       predictor = cd$data$x)
  )
  # Twelve curve parameters over the three equations, each with a prior row and
  # an initial value under its own level's tag.
  pr <- as.data.frame(out$prior)
  expect_length(pr$nlpar[nzchar(pr$nlpar)], 12L)
  expect_setequal(setdiff(names(out$init[[1]]), "b_phi"),
                  paste0("b_", pr$nlpar[nzchar(pr$nlpar)]))
})

test_that("an indicator column that would overwrite the data is refused", {
  d <- rbind(transform(nec_data[, c("x", "y")], site = "a"),
             transform(nec_data[, c("x", "y")], site = "b"))
  d$site <- factor(d$site)
  d$bnecindaLv1 <- 1
  expect_error(compose_level_data(bnf(y ~ crf(x, "nec3param")), d,
                                  c("a", "b"), "site"),
               "which the data already")
})

test_that("a composed level takes its own equation's priors and inits", {
  cs <- composed_spec()
  brm_args <- list(chains = 2, seed = 1)
  out <- suppressMessages(
    add_level_defaults(brm_args, cs$spec, cs$family, cs$data$y,
                       predictor = cs$data$x)
  )
  pr <- as.data.frame(out$prior)
  # Every parameter of each level's own equation has a row under that level's
  # tag, and no parameter of the other equation does.
  expect_setequal(pr$nlpar[nzchar(pr$nlpar)],
                  c("topaLv1", "betaaLv1", "necaLv1",
                    "botbLv2", "topbLv2", "betabLv2", "ec50bLv2", "fbLv2"))
  # Derived once per equation over the whole response, so the two levels' top
  # rows state the same prior: the levels are exchangeable a priori.
  expect_equal(pr$prior[pr$nlpar == "topaLv1"],
               pr$prior[pr$nlpar == "topbLv2"])
  # And the bounds are the equation's own.
  expect_equal(pr$lb[pr$nlpar == "topaLv1"], "0")
  expect_equal(length(out$init), 2L)
  expect_setequal(names(out$init[[1]]),
                  c(paste0("b_", c("topaLv1", "betaaLv1", "necaLv1",
                                   "botbLv2", "topbLv2", "betabLv2",
                                   "ec50bLv2", "fbLv2")), "b_phi"))
  expect_equal(length(out$init[[1]]$b_phi), 2L)
})

test_that("the composed grid carries an indicator column per level", {
  cs <- composed_spec()
  fake <- list(data = cs$data, family = cs$family)
  g <- prediction_grid(fake, cs$formula, resolution = 5,
                       level_spec = cs$spec)$newdata
  expect_equal(nrow(g), 10L)
  expect_true(all(c("bnecindaLv1", "bnecindbLv2") %in% names(g)))
  expect_equal(g$bnecindaLv1, rep(c(1, 0), each = 5))
  expect_equal(g$bnecindbLv2, rep(c(0, 1), each = 5))
  # One level predicted still writes every level's column, because the composed
  # mean reads all of them on every row.
  cs$spec$predict_levels <- "b"
  one <- prediction_grid(fake, cs$formula, resolution = 5,
                         level_spec = cs$spec)$newdata
  expect_equal(nrow(one), 5L)
  expect_equal(one$bnecindaLv1, rep(0, 5))
  expect_equal(one$bnecindbLv2, rep(1, 5))
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
  # The reduction to the special case, on a fitted object: both levels favour
  # nec3param, so nothing is composed and the dummy-coded form is what was
  # built. The coefficient names asserted below are that form's.
  expect_false(isTRUE(f$joint$level_spec$composed))
  expect_equal(unname(unlist(f$joint$models)), c("nec3param", "nec3param"))
  expect_null(f$joint$level_spec$tags)
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

# Phase 2 of #382. The test that stood here asserted that ecx() on a joint
# refit errored and that the grid carried no level column; both are now false
# and the assertions below replace them.

test_that("the level column is added only where a level spec asks for it", {
  # No fit and no sampling: prediction_grid() reads the family and the data off
  # whatever it is handed, which is what makes the conditional testable without
  # a posterior. The unconditional case is the one that matters, because every
  # caller that existed before #382 passes no level spec.
  d <- data.frame(x = c(1, 2, 3, 4), y = c(0.9, 0.6, 0.3, 0.1),
                  site = factor(c("a", "a", "b", "b")))
  fake <- list(data = d, family = gaussian())
  f <- bnf(y ~ crf(x, "nec3param"))
  plain <- prediction_grid(fake, f, resolution = 7)$newdata
  expect_equal(nrow(plain), 7L)
  expect_false("site" %in% names(plain))
  spec <- list(group_var = "site", levels = c("a", "b"))
  by_level <- prediction_grid(fake, f, resolution = 7,
                              level_spec = spec)$newdata
  expect_equal(nrow(by_level), 14L)
  expect_equal(levels(by_level$site), c("a", "b"))
  # The level varies slowest, so a posterior over this grid splits into
  # contiguous per-level blocks.
  expect_equal(as.character(by_level$site), rep(c("a", "b"), each = 7))
  expect_equal(by_level$x[1:7], plain$x)
  expect_equal(by_level$x[8:14], plain$x)
  # One level predicted keeps every level on the factor, because brms builds
  # the design matrix against the levels the fit was given.
  spec$predict_levels <- "b"
  one <- prediction_grid(fake, f, resolution = 7, level_spec = spec)$newdata
  expect_equal(nrow(one), 7L)
  expect_equal(levels(one$site), c("a", "b"))
  expect_true(all(one$site == "b"))
})

test_that("bnec_newdata on a joint refit gives resolution rows per level", {
  skip_on_cran()
  f <- joint_gate_fixture()
  nd <- bnec_newdata(f$joint, resolution = 10)
  expect_equal(nrow(nd), 20L)
  expect_equal(as.character(nd$site), rep(c("a", "b"), each = 10))
  # The prediction that phase 1 recorded as stopping on the missing column now
  # runs, which is the whole of what phase 2 needed from the grid.
  pe <- brms::posterior_epred(f$joint$fit, newdata = nd, re_formula = NA)
  expect_equal(ncol(pe), 20L)
})

test_that("the estimators report one row per level of a joint refit", {
  skip_on_cran()
  f <- joint_gate_fixture()
  e <- suppressWarnings(suppressMessages(ecx(f$joint, ecx_val = 10)))
  expect_s3_class(e, "data.frame")
  expect_equal(e$level, c("a", "b"))
  expect_equal(ncol(e), 4L)
  n <- nec(f$joint)
  expect_equal(n$level, c("a", "b"))
  s <- suppressWarnings(suppressMessages(nsec(f$joint)))
  expect_equal(s$level, c("a", "b"))
  # The same columns the grouped route returns, so the two tables can be read
  # against each other.
  expect_equal(names(n), names(nec(f$grouped)))
  # ecnsec() has to be given a method of its own: a bayesnecjointfit inherits
  # from bnecfit, so without one the inherited ecnsec.bnecfit() ran on the
  # multi-level grid and returned a number for the fit rather than per level.
  en <- suppressWarnings(suppressMessages(ecnsec(f$joint, nsec = 1.5)))
  expect_equal(en$level, c("a", "b"))
  expect_equal(names(en), names(e))
  # posterior = TRUE has no one-row-per-level form, and unlike a grouped fit
  # there are no per-level fits to send the user to, so the draws come back as
  # a named list rather than being refused.
  ep <- suppressWarnings(suppressMessages(
    ecx(f$joint, ecx_val = 10, posterior = TRUE)
  ))
  expect_named(ep, c("a", "b"))
  expect_equal(length(ep$a), brms::ndraws(f$joint$fit))
})

test_that("per-level ecx from a joint refit agrees with bnec_group", {
  # The phase 2 gate of #382. Phase 1 established that the curve parameters
  # agree; this carries that agreement through the estimators, which is what a
  # user actually reads. A disagreement here and not in phase 1's test is a
  # grid or extraction fault in phase 2, because the priors are already pinned.
  skip_on_cran()
  f <- joint_gate_fixture()
  jp <- suppressWarnings(suppressMessages(
    ecx(f$joint, ecx_val = 10, posterior = TRUE)
  ))
  for (lev in c("a", "b")) {
    gp <- suppressWarnings(suppressMessages(
      ecx(f$grouped$fits[[lev]], ecx_val = 10, posterior = TRUE)
    ))
    j <- jp[[lev]]
    # Monte Carlo error of each median, from the draws themselves. The two fits
    # are independent samples of the same posterior, so the difference of the
    # medians is compared against the two errors added in quadrature.
    mcse <- sqrt(stats::sd(j, na.rm = TRUE)^2 / sum(!is.na(j)) +
                   stats::sd(gp, na.rm = TRUE)^2 / sum(!is.na(gp)))
    expect_lt(
      abs(stats::median(j, na.rm = TRUE) -
            stats::median(gp, na.rm = TRUE)) / mcse,
      6
    )
  }
})

test_that("autoplot and ggbnec_data carry the level of a joint refit", {
  skip_on_cran()
  f <- joint_gate_fixture()
  dat <- suppressWarnings(suppressMessages(ggbnec_data(f$joint)))
  expect_true("panel" %in% names(dat))
  expect_equal(levels(dat$panel), c("a", "b"))
  expect_equal(attr(dat, "panel_var"), "site")
  expect_equal(attr(dat, "group_var"), "site")
  expect_true(attr(dat, "group_fitted"))
  # One raw observation per fitted row, split between the panels as the data
  # were.
  raw <- dat[!is.na(dat$y_r), ]
  expect_equal(as.integer(table(raw$panel)), c(nrow(nec_data), nrow(nec_data)))
  # Each level gets its own curve and its own three NEC rows.
  expect_equal(as.integer(table(dat$panel[!is.na(dat$nec_vals)])), c(3L, 3L))
  p <- suppressWarnings(suppressMessages(autoplot(f$joint)))
  expect_s3_class(p, "ggplot")
  expect_equal(levels(p$layers[[1]]$data$model), c("site = a", "site = b"))
})

# The composed branch of #388: the levels favour different equations, so the
# refit carries one equation per level in a single posterior. The grouped fit
# is mocked down to the weights, which is all bnec_joint() reads off it, so the
# fixture costs one Stan program for the refit and one per level for the
# comparison rather than a full bnec_group() over two equations.
composed_gate_fixture <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) {
      return(cached)
    }
    set.seed(101)
    xb <- nec_data$x
    mu_b <- 0.05 + (0.9 - 0.05) / (1 + exp(2 * (xb - 1.4)))
    yb <- pmin(pmax(stats::rnorm(length(xb), mu_b, 0.05), 0.001), 0.999)
    d <- rbind(data.frame(x = nec_data$x, y = nec_data$y, site = "a"),
               data.frame(x = xb, y = yb, site = "b"))
    d$site <- factor(d$site)
    grouped <- mock_group_fit(list(a = c(nec3param = 0.8, ecx4param = 0.2),
                                   b = c(ecx4param = 0.9, nec3param = 0.1)))
    grouped$data <- d
    grouped$formula <- bnf(y ~ crf(x, c("nec3param", "ecx4param")))
    grouped$family <- validate_family("beta")
    brm_opts <- list(iter = 2000, warmup = 1000, chains = 2, seed = 7,
                     refresh = 0)
    joint <- suppressWarnings(suppressMessages(
      do.call(bnec_joint, c(list(grouped), brm_opts))
    ))
    singles <- list(
      a = suppressWarnings(suppressMessages(do.call(bnec, c(
        list(y ~ crf(x, "nec3param"), data = d[d$site == "a", ]), brm_opts)))),
      b = suppressWarnings(suppressMessages(do.call(bnec, c(
        list(y ~ crf(x, "ecx4param"), data = d[d$site == "b", ]), brm_opts))))
    )
    cached <<- list(data = d, joint = joint, singles = singles)
    cached
  }
})

test_that("a composed refit carries one equation per level in one posterior", {
  skip_on_cran()
  f <- composed_gate_fixture()
  expect_true(isTRUE(f$joint$level_spec$composed))
  expect_equal(unname(unlist(f$joint$models)), c("nec3param", "ecx4param"))
  expect_true(is.na(f$joint$model))
  vars <- brms::variables(f$joint$fit)
  # Level a's nec3param parameters and level b's ecx4param parameters, with
  # neither level carrying a parameter of the other's equation.
  expect_true(all(paste0("b_", c("topaLv1", "betaaLv1", "necaLv1"),
                         "_Intercept") %in% vars))
  expect_true(all(paste0("b_", c("botbLv2", "ec50bLv2", "topbLv2", "betabLv2"),
                         "_Intercept") %in% vars))
  expect_false("b_necbLv2_Intercept" %in% vars)
  expect_false("b_ec50aLv1_Intercept" %in% vars)
  # disp_by_level keeps its meaning: the dispersion is a linear term on the
  # factor either way, so it is named as the dummy-coded branch names it.
  expect_true(all(c("b_phi_sitea", "b_phi_siteb") %in% vars))
  expect_output(print(f$joint), "a = nec3param, b = ecx4param")
})

test_that("a composed refit agrees with a fit of each level's own equation", {
  # The gate of #388. Each level of the composed refit and a fit of that level
  # alone with that level's equation estimate the same curve, so they must
  # agree to Monte Carlo error. The comparison is made on the curve parameters
  # and on ecx(), which is what a user reads.
  skip_on_cran()
  f <- composed_gate_fixture()
  jf <- suppressWarnings(summary(f$joint$fit))$fixed
  for (lev in c("a", "b")) {
    tag <- f$joint$level_spec$tags[[lev]]
    m <- f$joint$models[[lev]]
    gf <- suppressWarnings(summary(pull_brmsfit(f$singles[[lev]])))$fixed
    for (p in names(get(paste0("bf_", m))[[2]])) {
      j <- jf[paste0(p, tag, "_Intercept"), ]
      g <- gf[paste0(p, "_Intercept"), ]
      mcse <- sqrt(j[["Est.Error"]]^2 / j[["Bulk_ESS"]] +
                     g[["Est.Error"]]^2 / g[["Bulk_ESS"]])
      expect_lt(abs(j[["Estimate"]] - g[["Estimate"]]) / mcse, 5)
      expect_gt(j[["Est.Error"]] / g[["Est.Error"]], 0.8)
      expect_lt(j[["Est.Error"]] / g[["Est.Error"]], 1.25)
    }
  }
  jp <- suppressWarnings(suppressMessages(
    ecx(f$joint, ecx_val = 10, posterior = TRUE)
  ))
  for (lev in c("a", "b")) {
    gp <- suppressWarnings(suppressMessages(
      ecx(f$singles[[lev]], ecx_val = 10, posterior = TRUE)
    ))
    j <- jp[[lev]]
    mcse <- sqrt(stats::sd(j, na.rm = TRUE)^2 / sum(!is.na(j)) +
                   stats::sd(gp, na.rm = TRUE)^2 / sum(!is.na(gp)))
    expect_lt(abs(stats::median(j, na.rm = TRUE) -
                    stats::median(gp, na.rm = TRUE)) / mcse, 6)
  }
})

test_that("the composed curve at a level matches a fit of that level alone", {
  # The curve rather than an estimate read off it. nsec() disagreed by five
  # Monte Carlo errors of its median on a measured run while the two curves
  # agreed to 1.3e-3 at every grid point, because nsec() reads the 1 per cent
  # tail of the control posterior against a curve that is nearly flat there: a
  # 2e-3 shift in the threshold moves the crossing by 2e-2 in x. The curve is
  # therefore what the agreement is asserted on, and the estimator test above
  # uses ecx(), which crosses the curve where it is steep.
  skip_on_cran()
  f <- composed_gate_fixture()
  for (lev in c("a", "b")) {
    lf <- joint_level_fit(f$joint, lev)
    ndj <- bnec_newdata(lf, resolution = 25)
    ndg <- bnec_newdata(f$singles[[lev]], resolution = 25)
    pj <- brms::posterior_epred(f$joint$fit, newdata = ndj, re_formula = NA)
    pg <- brms::posterior_epred(pull_brmsfit(f$singles[[lev]]),
                                newdata = ndg, re_formula = NA)
    expect_lt(max(abs(apply(pj, 2, stats::median) -
                        apply(pg, 2, stats::median))), 0.01)
  }
})

test_that("the estimators and the plot follow each level's own equation", {
  skip_on_cran()
  f <- composed_gate_fixture()
  e <- suppressWarnings(suppressMessages(ecx(f$joint, ecx_val = 10)))
  expect_equal(e$level, c("a", "b"))
  # nec() is defined at level a, whose equation is nec3param, and not at level
  # b, whose ecx4param has no nec parameter, so the whole-fit call fails rather
  # than reporting a number for one level.
  expect_error(suppressWarnings(nec(f$joint)))
  expect_equal(length(joint_level_fit(f$joint, "a")$ne_posterior),
               brms::ndraws(f$joint$fit))
  expect_null(joint_level_fit(f$joint, "b")$ne_posterior)
  dat <- suppressWarnings(suppressMessages(ggbnec_data(f$joint)))
  # One curve and one set of three no-effect rows per panel, and every raw
  # observation assigned to the panel its row belongs to. The row-to-level map
  # is read off the indicator columns here, because a composed refit with a
  # shared dispersion never puts the factor in the formula at all.
  expect_equal(as.integer(table(dat$panel[!is.na(dat$nec_vals)])), c(3L, 3L))
  expect_equal(as.integer(table(joint_row_levels(f$joint))),
               c(nrow(nec_data), nrow(nec_data)))
  p <- suppressWarnings(suppressMessages(autoplot(f$joint)))
  expect_s3_class(p, "ggplot")
  # The annotation names what was read off each panel's own curve: a threshold
  # equation reports its nec parameter, a smooth one its NSEC. Read off the
  # plot rather than ggbnec_data(), which does not carry the tag.
  tagged <- p$layers[[which(vapply(p$layers, function(l)
    is.data.frame(l$data) && "tag" %in% names(l$data), logical(1)))[1]]]$data
  expect_equal(unique(as.character(tagged$tag[tagged$panel == "a"])), "NEC")
  expect_equal(unique(as.character(tagged$tag[tagged$panel == "b"])), "NSEC")
})
