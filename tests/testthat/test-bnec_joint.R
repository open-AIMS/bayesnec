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

# A crossed mock whose growth component was fitted with `growth_family`, on
# nec_data with the responses above x = 1.8 recorded as zeros. bnec_joint()
# reads the family off the first stored growth fit and nothing else from it.
mock_disp_crossed <- function(formula, growth_family, scale = 1) {
  o <- mock_crossed()
  d <- nec_data[, c("x", "y")]
  d$y[d$x > 1.8] <- 0
  d$y <- d$y * scale
  o$data <- d
  o$formula <- bnf(formula)
  o$growth$mod_fits[[1]] <- list(
    fit = structure(list(family = validate_family(growth_family)),
                    class = "brmsfit")
  )
  o
}

# Replaces bnec() inside bnec_joint() and records what it was given, so the
# joint model can be built without being fitted.
capture_joint_call <- function(env = parent.frame()) {
  captured <- new.env()
  local_mocked_bindings(
    bnec = function(formula, data, family, model_survival, ...) {
      captured$formula <- formula
      captured$data <- data
      captured$family <- family
      invisible(NULL)
    },
    .package = "bayesnec", .env = env
  )
  captured
}

test_that("bnec_joint includes the growth component's disp() term (#410)", {
  cases <- list(
    list(family = "Gamma", scale = 10, joint = "hurdle_gamma", dpar = "shape",
         disp = 'disp("power")'),
    list(family = "Beta", scale = 1, joint = "zero_inflated_beta",
         dpar = "phi", disp = 'disp("twosided")')
  )
  for (cs in cases) {
    f <- paste0('y ~ crf(x, c("nec3param", "ecx4param")) + ', cs$disp)
    o <- mock_disp_crossed(f, cs$family, cs$scale)
    captured <- capture_joint_call()
    suppressMessages(bnec_joint(o))
    expect_equal(captured$family, cs$joint)
    expect_false(is.null(bayesnec:::parse_disp_term(captured$formula)))
    # The joint model's dispersion sub-model is the one the growth component
    # is built with: same curve, same centring literal. ecx4param is the
    # growth equation best_crossed() picks from mock_crossed()'s weights.
    joint <- make_brmsformula(captured$formula, captured$data,
                              family = validate_family(cs$joint))[[1]]
    growth <- make_brmsformula(
      bnf(paste0('y ~ crf(x, "ecx4param") + ', cs$disp)),
      o$data[o$data$y > 0, ], family = validate_family(cs$family)
    )[[1]]
    expect_identical(deparse1(joint$pforms[[cs$dpar]][[3]]),
                     deparse1(growth$pforms[[cs$dpar]][[3]]))
  }
})

test_that("a formula passed to bnec_joint replaces the held disp() term", {
  o <- mock_disp_crossed(
    'y ~ crf(x, c("nec3param", "ecx4param")) + disp("power")', "Gamma", 10
  )
  captured <- capture_joint_call()
  suppressMessages(bnec_joint(o, formula = y ~ crf(x, "nec3param")))
  expect_null(bayesnec:::parse_disp_term(captured$formula))
})

test_that("bnec_joint refuses a non-syntactic name before rebuilding (#398)", {
  # swap_crf_model() rebuilds crf() from deparsed text, which dropped the
  # backticks and failed as a parse error naming neither column nor term.
  o <- mock_disp_crossed('y ~ crf(x, c("nec3param", "ecx4param"))', "Gamma",
                         10)
  captured <- capture_joint_call()
  expect_error(bnec_joint(o, formula = y ~ crf(`odd x`, "nec3param")),
               "not syntactic R names.*odd x")
  expect_null(captured$formula)
})
