test_that("handle_set works", {
  m_0 <- paste0("Nothing to amend, please specify a model to either add",
                " or drop that differs from the original set.")
  handle_set(c("nec4param", "nec3param"), add = "nec4param") |>
    expect_equal("wrong_model_output") |>
    expect_message(m_0)
  handle_set(c("nec4param", "nec3param")) |>
    expect_equal("wrong_model_output") |>
    expect_message(m_0)
  handle_set(c("nec4param", "nec3param"),
             drop = c("nec4param", "nec3param")) |>
    expect_error("All models removed, nothing to return")
  handle_set(c("nec4param", "nec3param"), add = c("ecx4param", "ecxlin")) |>
    expect_equal(c("nec4param", "nec3param", "ecx4param", "ecxlin"))
  handle_set(c("nec4param", "nec3param", "ecx4param",  "ecxlin"),
             drop = c("ecxlin", "nec4param", "nec3param")) |>
    expect_equal(c("ecx4param"))
})

test_that("weighted_draw_index is a pure function of its arguments", {
  # The whole of #216 is that this draw must not depend on when it is called.
  stats <- data.frame(wi = c(0.75, 0.25),
                      row.names = c("nec4param", "ecx4param"))
  models <- rownames(stats)
  a <- bayesnec:::weighted_draw_index(models, 100, stats, seed = 7)
  b <- bayesnec:::weighted_draw_index(models, 100, stats, seed = 7)
  expect_identical(a, b)
  expect_named(a, models)
  expect_equal(lengths(a), c(nec4param = 75L, ecx4param = 25L))
  expect_true(all(unlist(a) %in% seq_len(100)))
  # A different seed must actually move it, or the seed is doing nothing.
  expect_false(identical(a, bayesnec:::weighted_draw_index(models, 100, stats,
                                                           seed = 8)))
  # Objects saved before #216 carry no seed. They must still be self-consistent.
  n1 <- bayesnec:::weighted_draw_index(models, 100, stats, seed = NULL)
  n2 <- bayesnec:::weighted_draw_index(models, 100, stats, seed = NULL)
  expect_identical(n1, n2)
})

test_that("weighted_draw_index leaves the caller's RNG stream alone", {
  # Model averaging silently resetting a user's simulation seed would be a
  # worse bug than the one being fixed.
  stats <- data.frame(wi = c(0.5, 0.5),
                      row.names = c("nec4param", "ecx4param"))
  set.seed(99)
  expected <- runif(3)
  set.seed(99)
  first <- runif(1)
  bayesnec:::weighted_draw_index(rownames(stats), 50, stats, seed = 7)
  expect_equal(c(first, runif(2)), expected)
})

test_that("weighted_draw_index handles a model carrying no weight", {
  # loo_model_weights() returns exact zeros often enough that this is a real
  # path, not a hypothetical one: the model must contribute no rows, not error.
  stats <- data.frame(wi = c(1, 0),
                      row.names = c("nec4param", "ecx4param"))
  idx <- bayesnec:::weighted_draw_index(rownames(stats), 40, stats, seed = 3)
  expect_length(idx$ecx4param, 0)
  expect_length(idx$nec4param, 40)
  expect_setequal(idx$nec4param, seq_len(40))
})

test_that("weighted_draw_index ignores the session's sample.kind", {
  # A seed alone does not fix a draw. R 3.6.0 changed sample()'s algorithm, so
  # the same seed gives a different index either side of it; an object reloaded
  # under a different R would otherwise rebuild a different index and stop
  # matching its own stored summaries. Pinning is what makes the seed archival.
  stats <- data.frame(wi = c(0.6, 0.4),
                      row.names = c("nec4param", "ecx4param"))
  models <- rownames(stats)
  ref <- bayesnec:::weighted_draw_index(models, 100, stats, seed = 11)
  old_kind <- RNGkind()
  on.exit(suppressWarnings(RNGkind(old_kind[1], old_kind[2], old_kind[3])),
          add = TRUE)
  suppressWarnings(RNGkind(sample.kind = "Rounding"))
  # Same seed under the pre-3.6.0 algorithm: this is what an unpinned draw
  # would have returned, and it is not the same index.
  set.seed(11)
  expect_false(identical(sample(seq_len(100), 60L), ref$nec4param))
  expect_identical(bayesnec:::weighted_draw_index(models, 100, stats, seed = 11),
                   ref)
  # The caller's choice of generator is left as it was found.
  expect_identical(RNGkind()[3], "Rounding")
})

test_that("pull_draw_index prefers the stored index over rebuilding it", {
  stats <- data.frame(wi = c(0.6, 0.4),
                      row.names = c("nec4param", "ecx4param"))
  models <- rownames(stats)
  obj <- list(sample_size = 100, mod_stats = stats, w_draw_seed = 11,
              w_draw_index = bayesnec:::weighted_draw_index(models, 100, stats,
                                                            11))
  expect_identical(bayesnec:::pull_draw_index(obj, models, 100),
                   obj$w_draw_index)
  # sample_size arrives as a double off the object and an integer off nrow().
  # identical() would call those different and take the fallback every time.
  expect_identical(bayesnec:::pull_draw_index(obj, models, 100L),
                   obj$w_draw_index)
  # Edge case: a caller thinning to fewer draws cannot use the stored index at
  # all, so the fallback has to produce one that is actually in range.
  small <- bayesnec:::pull_draw_index(obj, models, 40)
  expect_equal(lengths(small), c(nec4param = 24L, ecx4param = 16L))
  expect_true(all(unlist(small) <= 40))
  # An object saved before either field existed.
  legacy <- obj[c("sample_size", "mod_stats")]
  expect_identical(bayesnec:::pull_draw_index(legacy, models, 100),
                   bayesnec:::weighted_draw_index(models, 100, stats, NULL))
})
