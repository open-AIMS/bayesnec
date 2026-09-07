# bnec_record() reports what bnec() did to the request before fitting. The set
# as requested, the set attempted, the reason for the difference, and what was
# altered in the response are what a methods section has to state, and all of
# it used to exist only as console output. See #261 and #93.

test_that("amend_requested extends the request with what was added", {
  expect_equal(bayesnec:::amend_requested(c("a", "b"), NULL), c("a", "b"))
  expect_equal(bayesnec:::amend_requested(c("a", "b"), "c"), c("a", "b", "c"))
  # A model asked for twice is requested once.
  expect_equal(bayesnec:::amend_requested(c("a", "b"), "b"), c("a", "b"))
})

test_that("amend_exclusions keeps requested partitioned exactly", {
  # Three things can put an equation in `requested` but not in the set
  # attempted, and all three have to be recorded or the partition the record
  # documents does not hold.
  rec <- list(
    requested = c("a", "b", "c"),
    attempted = c("a", "b"),
    excluded = data.frame(model = "c", reason = "not valid for this family",
                          stringsAsFactors = FALSE)
  )
  # (i) declined by this call's check_models().
  now <- data.frame(model = "b", reason = "declined now",
                    stringsAsFactors = FALSE)
  out <- bayesnec:::amend_exclusions(rec, now, "a", NULL)
  expect_setequal(c("a", out$model), rec$requested)
  # (ii) declined in the call being amended, and not asked for again: the
  # original reason is kept rather than replaced by a generic one.
  expect_equal(out$reason[out$model == "c"], "not valid for this family")
  # (iii) dropped here.
  out2 <- bayesnec:::amend_exclusions(rec, NULL, "a", NULL)
  expect_equal(out2$reason[out2$model == "b"], "dropped by amend()")
  # An added model that is attempted adds nothing to the table.
  out3 <- bayesnec:::amend_exclusions(rec, NULL, c("a", "b", "d"), "d")
  expect_equal(sort(out3$model), "c")
})

test_that("bnec_record refuses an object it cannot describe", {
  expect_error(bnec_record(nec_data), "not a bayesnecfit")
})

test_that("bnec() records the request, and leaves $model a plain string", {
  # Two things at once, because both need a real fit. The exclusion record is
  # attached to `model` as an attribute; unstripped, the single-model branch
  # passed that vector to fit_bayesnec(), which stores it as out$model, so
  # every single-model fit printed the record and compared unequal to the
  # plain string it used to be. See #261.
  skip_on_cran()
  set.seed(410)
  x <- rep(seq(0, 5, length.out = 20), 3)
  y <- 3 * exp(-exp(-0.5) * pmax(x - 2, 0)) + rnorm(length(x), 0, 0.1)
  d <- data.frame(x = x, y = y)
  fit <- suppressWarnings(suppressMessages(
    bnec(y ~ crf(x, model = "nec4param"), data = d, family = gaussian(),
         chains = 2, iter = 400, warmup = 200, seed = 410,
         open_progress = FALSE, refresh = 0)
  ))
  skip_if_not(is_bayesnecfit(fit), "the single candidate had to fit")

  expect_identical(fit$model, "nec4param")
  expect_null(attributes(fit$model))

  rec <- bnec_record(fit)
  expect_named(rec, c("requested", "attempted", "excluded", "substitutions"))
  expect_equal(rec$requested, "nec4param")
  expect_equal(rec$attempted, "nec4param")
  expect_equal(nrow(rec$excluded), 0)
  expect_null(rec$substitutions)
  # requested is partitioned by attempted and the exclusion table.
  expect_setequal(c(rec$attempted, rec$excluded$model), rec$requested)

  # The record survives an update. expand_nec() builds a new object, so an
  # attribute not re-attached is lost, and bnec_record() then returned NULL for
  # a fit this version had recorded -- the one thing the documented NULL is
  # meant to rule out.
  upd <- suppressWarnings(suppressMessages(
    update(fit, chains = 2, iter = 400, warmup = 200, refresh = 0)
  ))
  expect_equal(bnec_record(upd)$requested, "nec4param")
  expect_equal(bnec_record(upd)$attempted, "nec4param")
  # The record keeps its shape. `$<-` with a NULL value deletes the name, so an
  # update that substituted nothing returned three elements where bnec() and
  # amend() return four.
  expect_named(bnec_record(upd),
               c("requested", "attempted", "excluded", "substitutions"))

  # And an amend rebuilds it for the set it produced, rather than dropping it.
  # Amended by the weighting method rather than by adding an equation: this
  # exercises the same rebuild in amend_model_set() without compiling a second
  # Stan model, and the set arithmetic it would have tested is covered by the
  # amend_requested() and amend_exclusions() blocks above.
  amended <- suppressWarnings(suppressMessages(
    amend(fit, loo_controls = list(weights = list(method = "pseudobma")))
  ))
  rec2 <- bnec_record(amended)
  expect_named(rec2, c("requested", "attempted", "excluded", "substitutions"))
  expect_equal(rec2$requested, "nec4param")
  expect_setequal(c(rec2$attempted, rec2$excluded$model), rec2$requested)
  # amend() fits from the stored data with skip_check = TRUE, so it makes no
  # substitution of its own and the one bnec() recorded still stands.
  expect_equal(rec2$substitutions, rec$substitutions)
})
