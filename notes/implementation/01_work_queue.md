# Work queue — the training-course run

Read `00_protocol.md` first, then `03_decisions.md`.

**Rebuilt 2026-09-06.** The queue that stood here was written on 2026-09-03 and
ordered the work by test coverage of the paths producing defects. That ordering
was correct and most of its tier A has merged. Four decisions taken by RF on
2026-09-06 change what follows it, so the file is rebuilt rather than amended.

## The goal this queue serves

A training course is to be written against the development version of
`bayesnec`. That fixes an end state the previous queue did not have: `dev` must
be installable, its estimators must return correct values, and its vignettes
must be finalised and re-rendered, because the course teaches from them. The
release to CRAN is still not a gate on anything here.

## The four decisions

| | decision |
|---|---|
| 1 | **`ecx()`, `nsec()` and `ecnsec()` are fixed in `bayesnec` now.** The migration to `toxval` is deferred, not cancelled. See D11 |
| 2 | **PR #228 is deferred within this run, not dropped.** PRs #243 and #238 are finalised first; #228 follows once the divergence question is settled. See D12 |
| 3 | **The full precompile runs once, after all three vignettes are settled.** See D13 |
| 4 | **The run is autonomous and stacked**, on the terms already in `00_protocol.md` |

## What changed since 2026-09-03

| | |
|---|---|
| #275 | fixed by PR #280. `R-CMD-check` is green on `dev` for the first time in the run. Nothing in this queue is blocked on the matrix any more |
| #277 | tier A item A1, merged as PR #276. `test-check_data.R`, `test-plot.R` and `test-autoplot.R` exist |
| #278 | merged as PR #279. The three argument behaviours and four dead guards A1 found |
| #245, #265, #269 | closed |

`dev` is at 2.1.3.26. `NEWS.md` headings are `# bayesnec 2.2.0` and
`# bayesnec 2.1.4`; new entries go under 2.2.0.

---

# 0. Housekeeping — do first, it takes minutes

- **Close #275, #277 and #278 by hand.** All three merged; every PR here targets
  `dev` rather than the default branch, so `Closes #n` does not fire. This is the
  sixth occurrence of that pattern.
- **Prune the stale worktrees.** Twenty-two are registered and most are on
  branches that have merged. `00_protocol.md` names the ones that must not be
  touched; the rest are removable with `git worktree remove`. This matters
  because a stale worktree holds a branch checked out, and a branch held by a
  worktree cannot be checked out again.

---

# The batches

**Rebuilt as four batches on 2026-09-06** (RF: as few pull requests as possible,
worked unattended). The tiers that stood here are unchanged in content; what
changes is that issues sharing a subsystem are fixed in one branch and one pull
request rather than one each. Each pull request body is sectioned by issue, so
review is still per-issue.

The batches **stack**, per `00_protocol.md` and D14: batch 2 is cut from batch 1
and so on. If a batch stalls, cut the next from the last good branch and record
the skip in `05_run_log.md`.

## Batch 1 — the estimate and the curve it is read from

**Closes #195, #196, #39, #206, #160, #161, #268.** Files: `R/ecx.R`,
`R/nsec.R`, `R/ecnsec.R`, `R/check_models.R`, `R/plot.R`, `R/autoplot.R`,
`R/helpers.R`.

One theme: what a reported estimate is measured against, and whether the
predictor transformation is inverted correctly on the way out. D15 and D16 state
the eight rulings; this batch implements them.

| # | what |
|---|---|
| #196 | inline `crf()` arithmetic discarded when back-transforming, in `R/ecx.R:198-201` and `R/nsec.R:173-175`. Substitute into the whole argument expression, not its first slot |
| #195 | the reference becomes the control per D15; `hormesis_def` is removed; the four `type` values are implemented; the documented default is made the effective default |
| #39 | root-finding for the crossing rather than the nearest grid point. Required by D15 ruling 3, which cannot report `NA` reliably off a nearest-point search |
| #206 | remove the `gaussian` exclusion of the zero-bounded equations, and the `R/ecx.R:161` absolute-ECx refusal that was its other half |
| #160 | *NEC* mis-plotted when a function is called for `x`. Re-check after #196; the plotting path applies the same substitution |
| #161 | post-processing failure on a case-study dataset. Re-check after #195 and #196 |
| #268 | `xform` skipped on the predictor axis when the response is transformed |

**Take #196 first within the batch.** It is the smallest change, it is the one
that alters a published vignette number, and #160 and #161 are both suspected to
share its cause — settle them against the fixed version before writing anything
further.

**`ecnsec` is realigned here too**, per D15 ruling 5. It has no `bayesnec` issue
of its own; it is toxval#49, and the alignment is recorded there.

## Batch 2 — what `bnec()` checks, and what it reports

**Closes #274, #271, #272, #266, #93, #262, #261, #218.** Files:
`R/check_data.R`, `R/bnec.R`, `R/set_distribution.R`, `R/inits_functions.R`,
`R/bnecfit-methods.R`, `R/summary.R`, `R/print.R`, `R/compare_posterior.R`.

| # | what |
|---|---|
| #274 | `update()` with `newdata` reports a boundary correction and then discards it |
| #271 | no `disp()` sub-model is checked for finiteness before `brm()` sees it |
| #272 | `set_distribution()` returns `NULL` for an integer vector with negative values |
| #266 | `make_good_inits()` spends up to 561 s on one model before falling back |
| #93 | the two remaining silent response corrections message once and are recorded on the fit, per D16 |
| #262 | report the posterior probability of over-dispersion, and state that `beta_binomial` does not address under-dispersion |
| #261 | record which equations `bnec()` excluded, and report the candidate set as fitted |
| #218 | `compare_posterior()` pairs draws by unseeded permutation. Documentation and a constraint, not a code fix |

**#93 and #261 are one change seen from two sides** — both are about `bnec()`
recording what it did to the input before fitting — and #206 in batch 1 changes
what #261 has to report, since the `gaussian` exclusion goes away.

## Batch 3 — the default `nec` and `ec50` prior

**Closes #273.** Its own batch because D16 requires a measurement before the
choice, and because the change affects every default fit. The measurement is a
refit of reference datasets under both candidates, reported on the issue before
the code changes.

## Batch 4 — group-level deviations on a constrained scale

**Closes #257.** Its own batch, and last, because it is the one candidate
explanation not yet refuted for PR #228's 2000/2000 divergences under `ogl()`
for the binomial families. Taking it may settle batch 5c without a vignette
decision being needed at all.

---

# Batch 5 — the vignettes

All three are finalised in this run. The ordering is decision 2: the two that are
close go first, and the one blocked on a scientific question goes last.

| order | PR | issue | state, and what remains |
|---|---|---|---|
| 5a | #243 | #193 | example7. Reported mergeable at `0a8b5d35` with `dev` merged in. Three open items: CI has never been seen green on the branch, a follow-up issue was scoped and never opened, and the 14,700-word length was flagged for a judgement never made |
| 5b | #238 | #219 | example9. A full review is on the PR and unactioned. It needs restructuring so `screen_models()` sits with the sampler diagnostics, and `summary(fit)` shown after the candidate set is fitted. **Two of its errors of fact have expired** — see below |
| 5c | #228 | #6, #33 | the grouping vignette. Blocked on a scientific question, not on code |

**Re-check the #238 review before acting on it.** It was written before #260
merged. Its statement that `bnec()` does not force the identity link, and that
`family = binomial` takes logit, described `dev` at the time and does not
describe `dev` now: the link is assigned unless the caller wrote a `link`
argument. Its other findings — the untransformed dose justification refuted by
`example6`, and `summary()` already reporting per-model dispersion — are
unaffected. Note that the review's closing observation that `CLAUDE.md` §11 is
wrong as written still stands and is now wrong for the opposite reason.

**What blocks #228.** The dataset has 72.7% of observations on a boundary, so
`Beta` requires nudging three-quarters of the data and is hard to defend, while
`binomial` and `beta_binomial` — the families the source paper used — give
2000/2000 divergences and R-hat 2.7 as soon as `ogl(chamber)` is added. Three
explanations were proposed and all three refuted by test; the record is on the
PR. It is material beyond the vignette, because PR #250 claims group-level terms
work for bounded families generally and this is evidence that the claim holds for
`Beta` and not for the binomial families.

**Open it as its own issue rather than continuing to hold it on the PR**, and
take batch 4 (#257) before deciding anything: a group-level deviation applied on a scale
where the mean can leave its support is the mechanism that would produce exactly
this signature on a binomial response near the ceiling, and it is the one
explanation not yet tested.

---

# Batch 6 — the precompile

Runs once, after batch 5. Decision 3.

| # | what |
|---|---|
| #190 | the full `precompile.R`. Attended. The #251/#252 fan-out makes it a matrix rather than one serial run |
| #248 | the rendered `example2` still documents the 1.05 Rhat default; rides on #190 |

Two constraints on the run. The rendered `example1` changes if B1 lands, because
its ECx values are computed from the defective back-transform. And `example7`
alone takes about 137 minutes locally and about 2 h 56 m in CI, inside the
350-minute ceiling — PR #243's handoff note records the two traps that cost a
previous session time, and should be read before starting.

---

# Not in this queue

| # | why |
|---|---|
| #255 | the toxval migration tracker. Deferred by decision 1, not cancelled. D11 records what changed |
| #44 | hypothesis method for *NEC*/*NSEC*/*ECx* exceedance. A new API rather than a fix, and larger than the run. Tracked as toxval#41 |
| — | nothing further; #39 moved into batch 1, since D15 ruling 3 requires it |
| #120 | replacing `all_models`. D5 requires that no existing user script breaks; `test-plot.R` and `test-autoplot.R` now make that detectable, so the precondition is met, but it is behaviour change rather than a fix |
| #209, #249, PR #225 | the factorised count hurdle, blocked on `brms` upstream. `issue-136-rate-aterm` is PR #225's base and **must not be deleted** |
| #184 | `future_apply`. Attended: RF wants a testing pass posted as a comment before any implementation |
| #218 | unseeded permutation in `compare_posterior()`. Documentation-and-constraint outcome; cheap, add to a later pass |
| #27 | zero-truncated gaussian. The body is empty, and a title is not a specification |
