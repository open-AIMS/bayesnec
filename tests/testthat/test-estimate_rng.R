# Packaged posterior draws exercise the public API without fitting new models.
rng_calls <- function() {
  fits <- list(a = nec4param, b = ecx4param)
  priors <- data.frame(prior = "normal(1, 1)", class = "b", nlpar = "top",
                       lb = "0", ub = "3")
  list(
    average = function(seed = 10) average_estimates(
      fits, posterior = TRUE, seed = seed),
    estimates = function(seed = 10) compare_estimates(
      fits, comparison = "ecx", resolution = 20, seed = seed),
    fitted = function(seed = 10) compare_fitted(
      fits, resolution = 5, ndraws = 20, seed = seed),
    priors = function(seed = 10) sample_priors(
      priors, n_samples = 100, plot = NA, seed = seed)
  )
}

test_that("estimates and prior samples repeat and preserve the caller's RNG", {
  for (fun in rng_calls()) {
    set.seed(343)
    before <- .Random.seed
    first <- fun()
    expect_identical(.Random.seed, before)
    expect_identical(fun(), first)
    set.seed(99)
    before <- .Random.seed
    expect_identical(fun(), first)
    expect_identical(.Random.seed, before)
    expect_false(identical(fun(seed = 11), first))
    expect_identical(.Random.seed, before)
    expect_error(fun(seed = NULL), "seed")
    expect_identical(.Random.seed, before)
  }
})

test_that("calls preserve an absent seed and non-default RNG kinds", {
  original_kind <- RNGkind()
  set.seed(343)
  original_seed <- .Random.seed
  on.exit({
    suppressWarnings(RNGkind(original_kind[1], original_kind[2], original_kind[3]))
    assign(".Random.seed", original_seed, globalenv())
  })
  for (fun in rng_calls()) {
    suppressWarnings(RNGkind("L'Ecuyer-CMRG", "Inversion", "Rounding"))
    set.seed(343)
    before <- .Random.seed
    kind <- RNGkind()
    first <- fun()
    expect_identical(.Random.seed, before)
    expect_identical(RNGkind(), kind)
    rm(".Random.seed", envir = globalenv())
    expect_identical(fun(), first)
    expect_false(exists(".Random.seed", globalenv(), inherits = FALSE))
    expect_identical(RNGkind(), kind)
  }
})

test_that("comparison dispatch forwards the seed for both routes", {
  fits <- list(a = nec4param, b = ecx4param)
  expect_identical(
    compare_posterior(fits, comparison = "ecx", resolution = 20, seed = 23),
    compare_estimates(fits, comparison = "ecx", resolution = 20, seed = 23)
  )
  expect_identical(
    compare_posterior(fits, comparison = "fitted", resolution = 5,
                      ndraws = 20, seed = 23),
    compare_fitted(fits, resolution = 5, ndraws = 20, seed = 23)
  )
  group <- structure(list(fits = fits), class = "bayesnecgroupfit")
  expect_identical(
    compare_posterior(group, comparison = "ecx", resolution = 20, seed = 23),
    compare_estimates(fits, comparison = "ecx", resolution = 20, seed = 23)
  )
})

test_that("errors after sampling restore the caller's RNG", {
  priors <- data.frame(prior = c("normal(1, 1)", "unsupported(1, 1)"),
                       class = "b", nlpar = c("top", "bot"), lb = "", ub = "")
  set.seed(343)
  before <- .Random.seed
  expect_error(sample_priors(priors, plot = NA), "unsupported")
  expect_identical(.Random.seed, before)
  expect_error(compare_fitted(list(a = nec4param, b = nec4param),
                              ndraws = 0), "draw")
  expect_identical(.Random.seed, before)
})

test_that("model-averaged inputs repeat across estimate types", {
  fits <- list(a = manec_example, b = nec4param)
  for (estimate in c("nec", "ecx", "nsec")) {
    set.seed(343)
    before <- .Random.seed
    average <- average_estimates(fits, estimate = estimate, resolution = 20)
    comparison <- compare_estimates(fits, comparison = estimate,
                                    resolution = 20)
    expect_identical(average_estimates(fits, estimate = estimate,
                                       resolution = 20), average)
    expect_identical(compare_estimates(fits, comparison = estimate,
                                       resolution = 20), comparison)
    expect_identical(.Random.seed, before)
  }
  set.seed(343)
  before <- .Random.seed
  fitted <- compare_fitted(fits, resolution = 5, ndraws = 20)
  expect_identical(compare_fitted(fits, resolution = 5, ndraws = 20), fitted)
  expect_identical(.Random.seed, before)
})
