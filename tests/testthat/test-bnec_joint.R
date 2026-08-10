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
