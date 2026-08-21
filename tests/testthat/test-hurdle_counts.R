# #209: hurdle_poisson and hurdle_negbinomial as two-block families -- the count
# analogues of hurdle_gamma.
#
# The distinction from the zero-inflated count families added in #104 is whether
# the zeros are observed or latent, and it is the whole reason these belong in
# the two-block registry while those do not. Under zero-inflation a zero could
# have come from either component, so zi and mu are weakly separated exactly
# where mu is small -- the high-concentration end that sets the NEC. Under a
# hurdle the zeros are observed to be structural, the likelihood factorises, and
# both blocks carry an interpretable curve.

test_that("the count hurdle families are registered as two-block", {
  for (f in c("hurdle_poisson", "hurdle_negbinomial")) {
    expect_true(bayesnec:::is_hurdle_family(f))
    expect_identical(bayesnec:::hurdle_dpar(f), "hu")
  }
})

test_that("the zero-inflated count families are deliberately NOT two-block", {
  # Guards the #104 decision against being undone by someone pattern-matching
  # on the family name.
  for (f in c("zero_inflated_poisson", "zero_inflated_negbinomial")) {
    expect_false(bayesnec:::is_hurdle_family(f))
  }
})

test_that("the mu block reuses the corresponding count family", {
  # Priors and initial values for the mu block should come from what the
  # response looks like with the zeros set aside.
  expect_identical(bayesnec:::hurdle_mu_family("hurdle_poisson")$family,
                   "poisson")
  expect_identical(bayesnec:::hurdle_mu_family("hurdle_negbinomial")$family,
                   "negbinomial")
  # and on the identity link, like every other bayesnec family
  expect_identical(bayesnec:::hurdle_mu_family("hurdle_poisson")$link,
                   "identity")
})

test_that("validate_family accepts them and forces identity", {
  for (f in c("hurdle_poisson", "hurdle_negbinomial")) {
    v <- validate_family(f)
    expect_identical(v$family, f)
    expect_identical(v$link, "identity")
  }
})

test_that("check_models routes them like a zero-bounded mu block", {
  # The mu block is a count, so it is zero-bounded and unbounded above: the
  # linear-decay models go, and so does nechormepwr01, which is bounded on
  # (0, 1) by construction.
  for (f in c("hurdle_poisson", "hurdle_negbinomial")) {
    keep <- suppressMessages(
      check_models(c("nec3param", "nec4param", "neclin", "nechormepwr01"),
                   validate_family(f))
    )
    expect_setequal(keep, c("nec3param", "nec4param"))
  }
})

test_that("bnec_hurdle refuses an untruncated count growth family", {
  # The correctness fix. bnec_hurdle fits data[y > 0, ] with an ordinary count
  # family, which estimates mu / (1 - exp(-mu)) rather than mu -- a bias that
  # grows as the mean falls towards zero, which is the end the NEC is read off.
  # The separate-fits path cannot express the truncation; the joint families
  # do not need to, because brms writes the zero-truncated positive part
  # itself. So the user is sent there.
  for (f in c("poisson", "negbinomial")) {
    expect_error(
      bayesnec:::check_hurdle_growth_family(validate_family(f)),
      "untruncated"
    )
    expect_error(
      bayesnec:::check_hurdle_growth_family(validate_family(f)),
      paste0("hurdle_", f)
    )
  }
})

test_that("bnec_hurdle still accepts the continuous growth families", {
  # The refusal above must not catch the case bnec_hurdle exists for. For
  # hurdle_gamma the same construction is exact, because a Gamma has no mass at
  # zero -- which is why this was never a problem before counts were in scope.
  for (f in c("Gamma", "gaussian", "Beta")) {
    expect_no_error(
      bayesnec:::check_hurdle_growth_family(validate_family(f))
    )
  }
})

test_that("a two-block family is still refused as a growth family", {
  # bnec_hurdle IS the two-part model, so the new families must be refused here
  # for the pre-existing reason, not the new one.
  expect_error(
    bayesnec:::check_hurdle_growth_family(validate_family("hurdle_poisson")),
    "already a two-block family"
  )
})
