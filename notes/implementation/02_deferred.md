# Deferred, and why

Everything open in `open-AIMS/bayesnec` that is **not** in `01_work_queue.md`.
Recorded so the exclusion is a decision rather than an oversight.

**Refreshed 2026-09-06.** The estimator section below is rewritten: #195, #196,
#160, #161 and #206 were deferred here on the toxval migration and are now in
tier B of `01_work_queue.md`. See D11.

The 2026-09-03 refresh is preserved below because its reasoning still applies to
the entries that remain.

**Refreshed 2026-09-03.** The version that stood here was written on 2026-08-14
and was wrong in five places by the time it was read: #136, #139, #148 and #33
were filed under *needs a design decision first* after all four had been scoped,
implemented and merged; #166 was listed as *close as a duplicate* after it had
been closed. Those entries are removed rather than corrected — an entry that
describes work already done is worse than no entry, because it sends a reader to
the wrong place.

---

## Migrating to `toxval` — deferred, and no longer a reason to defer a fix

`ecx()`, `nsec()`, `ecnsec()` and `zero_crossings()` are still to move to
[open-AIMS/toxval](https://github.com/open-AIMS/toxval), which will become a
dependency of `bayesnec`. **The migration itself is deferred** (RF, 2026-09-06):
toxval#39, the dependency reversal, has not started, and a training course is to
be written against `dev` within days.

**The consequence for this file is that a defect in an estimator is no longer
deferred on the migration.** The four entries that stood here are now in tier B
of `01_work_queue.md`, on the reasoning in D11: the corrected code relocates with
the file when the migration runs, so no work is discarded, while leaving the
back-transform defective until the migration means every ECx and NSEC computed
from an inline transformation with arithmetic is wrong in the interim,
`example1`'s included.

| | where it went |
|---|---|
| #195 | tier B4 |
| #196 | tier B1 — first, because it changes an ECx reported in `example1` |
| #160 | tier B2 |
| #161 | tier B3 |
| #206 | tier B5 |

Two entries stay out, for reasons that are not the migration:

| | |
|---|---|
| #44 | hypothesis method for *NEC*/*NSEC*/*ECx* exceedance. A new API rather than a fix, and larger than the run |
| #39 | `uniroot.all` for speed and precision. An enhancement, and it requires #195's `type` reference semantics to be pinned down first |

**#255 is the tracker for the `bayesnec` side**, opened 2026-08-26. It holds the
method inventory and the dependency reversal; do not restate either here. Record
each tier B fix on the matching toxval issue as it lands, so the migration
re-lands the corrected version rather than the one toxval forked.

**The `bayesnec` release does not gate the migration** (RF, 2026-09-03), and the
migration no longer gates the estimator fixes (RF, 2026-09-06). What remains of
D10 is the file-relocation ordering alone.

---

## Deferred, with the reason still current

| | |
|---|---|
| #120 | replacing `all_models` in `predict`/`plot`/`autoplot` for `bayesmanecfit`. D5 stands. Its precondition is now met — D5 requires that no existing user script breaks, and `test-plot.R` and `test-autoplot.R` exist since 2026-09-03 — so what defers it is that it is behaviour change rather than a fix |
| #209 | `hurdle_poisson` and `hurdle_negbinomial`. PR #225 open, blocked on the `brms` bug tracked as #249. Its base branch `issue-136-rate-aterm` has merged and **must not be deleted** |
| #249 | the factorised count hurdle. Blocked on `brms`, upstream |
| #184 | `future_apply`. RF wants a testing pass with findings posted as a comment before any implementation, and concurrency plus RNG seeding is the class of change that fails silently. Attended |
| #218 | `compare_posterior()`/`average_estimates()` pair draws by unseeded permutation. Documentation-and-constraint outcome rather than a code fix; cheap, and belongs in a later pass |
| #27 | zero-truncated gaussian. Body is empty — a title is not a specification |

---

## Moved out of deferred

Recorded so the change is visible rather than silent.

| | what changed |
|---|---|
**2026-09-03**

| | what changed |
|---|---|
| #206 | **re-check.** It was deferred on the `R/ecx.R:161` absolute-ECx guard and on #170. #170 closed on 2026-08-17 via #208, and #256 settled the link policy the issue's argument depends on. Whether the ECx coupling still binds has not been re-checked since. Now tier B5 |
| #93 | **narrowed, not deferred.** #270 removed the predictor correction, which the issue's own second comment identifies as the half that matters. What remains is that two of the three response corrections are silent, measured on the issue, and whether substitutions should be recorded on the fit. No response substitution is an offset that can be reversed in an estimate, so the original proposal has nothing left to apply to. Now tier C |
| #257 | **unblocked.** It states *Depends on #256*, which closed 2026-09-02 via #259 and #260. It was in no queue at all. Now tier C, and it may settle PR #228 |
| #245 | **merged**, PR #250, 2026-08-26. Closed 2026-09-05 |

**2026-09-06**

| | what changed |
|---|---|
| #195, #196, #160, #161 | out of the toxval deferral and into tier B. See D11 |
| #190, #248 | out of *attended, immediately before submission* and into tier E. The precompile is no longer tied to a CRAN submission; it is tied to the training course reading a current rendered vignette. See D13 |
| #219, #193 | the two vignette PRs are no longer deferred. Tier D finalises all three. See D12 |

---

## Not specified well enough to implement unattended

A title is not a specification, and guessing at the intended design produces a
pull request that has to be thrown away.

| | body |
|---|---|
| #27 | *empty* — zero-truncated gaussian |

#6 and #33 were in this list and are no longer: #33 stage 1 merged as PR #227
and #6's capability shipped in v2.0. Both are held open by PR #228, the grouping
vignette, and by nothing else — except that #33 also stays open for stage 2,
which is gated on the toxval migration and so is deferred with it.

---

## Housekeeping carried forward

- `DESCRIPTION` requires `brms (>= 2.23.0)`; earlier versions mis-generate
  `beta_binomial`.
- The `disp()` documentation carries a caution about inline `crf()`
  transformations that must be removed when #196 is fixed — recorded on #196.
- **Three issues are merged and still open** because every PR here targets `dev`
  rather than the default branch, so `Closes #n` never fires: #275 (PR #280),
  #277 (PR #276) and #278 (PR #279). This is the sixth occurrence; #245, #265
  and #269 had it on 2026-09-03 and #229 to #232 on 2026-08-24. Writing
  `Closes #n` in the body does not help — the keyword's behaviour depends on the
  base branch, not on the wording. Item 0 of the queue closes these three.
- **`issue-136-rate-aterm` and `issue-148-check-fit` have merged and must not be
  deleted.** They are the bases of PRs #225 and #238, and deleting a merged base
  closes the PR above it irrecoverably.
