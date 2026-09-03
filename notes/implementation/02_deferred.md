# Deferred, and why

Everything open in `open-AIMS/bayesnec` that is **not** in `01_work_queue.md`.
Recorded so the exclusion is a decision rather than an oversight.

**Refreshed 2026-09-03.** The version that stood here was written on 2026-08-14
and was wrong in five places by the time it was read: #136, #139, #148 and #33
were filed under *needs a design decision first* after all four had been scoped,
implemented and merged; #166 was listed as *close as a duplicate* after it had
been closed. Those entries are removed rather than corrected — an entry that
describes work already done is worse than no entry, because it sends a reader to
the wrong place.

---

## Out of scope — migrating to `toxval`

`ecx()`, `nsec()`, `ecnsec()` and `zero_crossings()` are moving to
[open-AIMS/toxval](https://github.com/open-AIMS/toxval), which will become a
dependency of `bayesnec`. Fixing them here would be work thrown away.

| | |
|---|---|
| #39 | `uniroot.all` for speed and precision. Forces the `type` reference semantics to be pinned down, so it goes with #195 |
| #44 | hypothesis method for *NEC*/*NSEC*/*ECx* exceedance — new API, belongs with the estimators |
| #195 | inert `hormesis_def`, divergent `type` implementations, wrong documented default |
| #196 | inline `crf()` transformations back-transformed incorrectly |

**#255 is the tracker for the `bayesnec` side**, opened 2026-08-26. It holds the
method inventory and the dependency reversal; do not restate either here.

**The `bayesnec` release no longer gates the migration** (RF, 2026-09-03), so
the ordering constraint in D10 is now about file relocation alone, not about
shipping to CRAN first.

---

## Deferred, with the reason still current

| | |
|---|---|
| #120 | changes `predict`/`plot`/`autoplot` for `bayesmanecfit`; toxval registers the same methods. D5 stands. **`test-plot.R` and `test-autoplot.R` are a precondition**: D5 requires that no existing user script breaks, and until 2026-09-03 there was no test that would detect one breaking |
| #160 | *NEC* mis-plotted when a function is called for `x`. Likely shared post-processing with #196, but that is a suspicion rather than a finding — `test-autoplot.R` is how it gets settled |
| #161 | probably #195 or #196, in which case it is toxval's. Same instrument as #160 |
| #209 | `hurdle_poisson` and `hurdle_negbinomial`. PR #225 open, blocked on the `brms` bug tracked as #249. Its base branch `issue-136-rate-aterm` has merged and **must not be deleted** |
| #249 | the factorised count hurdle. Blocked on `brms`, upstream |
| #184 | `future_apply`. RF wants a testing pass with findings posted as a comment before any implementation, and concurrency plus RNG seeding is the class of change that fails silently. Attended |
| #218 | `compare_posterior()`/`average_estimates()` pair draws by unseeded permutation. Documentation-and-constraint outcome rather than a code fix; cheap, and belongs in a later pass |
| #248 | the rendered `example2` still documents the 1.05 Rhat default. Rides on #190 |
| #190 | the full `precompile.R`. Attended, once, immediately before submission |
| #219 | the workflow vignette. PR #238 open and needs rewriting onto `screen_models()`, which it predates |
| #193 | example7. Another session's; PR #243 |
| #27 | zero-truncated gaussian. Body is empty — a title is not a specification |

---

## Moved out of deferred, 2026-09-03

Recorded so the change is visible rather than silent.

| | what changed |
|---|---|
| #206 | **re-check.** It was deferred on the `R/ecx.R:161` absolute-ECx guard and on #170. #170 closed on 2026-08-17 via #208, and #256 settled the link policy the issue's argument depends on. Whether the ECx coupling still binds has not been re-checked since. In tier B of `01_work_queue.md` |
| #93 | **narrowed, not deferred.** #270 removed the predictor correction, which the issue's own second comment identifies as the half that matters. What remains is that two of the three response corrections are silent, measured on the issue, and whether substitutions should be recorded on the fit. No response substitution is an offset that can be reversed in an estimate, so the original proposal has nothing left to apply to. In tier B |
| #257 | **unblocked.** It states *Depends on #256*, which closed 2026-09-02 via #259 and #260. It was in no queue at all. In tier B |
| #245 | **merged**, PR #250, 2026-08-26. The issue is still open only because the PR targeted `dev` and `Closes` never fired. Close it |

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
which is toxval-gated.

---

## Housekeeping carried forward

- `DESCRIPTION` requires `brms (>= 2.23.0)`; earlier versions mis-generate
  `beta_binomial`.
- The `disp()` documentation carries a caution about inline `crf()`
  transformations that must be removed when #196 is fixed — recorded on #196.
- **Three issues are merged and still open** because every PR here targets `dev`
  rather than the default branch, so `Closes #n` never fires: #245 (PR #250),
  #265 and #269 (both PR #270). This is the fifth occurrence; #229 to #232 had
  it on 2026-08-24. Writing `Closes #n` in the body does not help — the
  keyword's behaviour depends on the base branch, not on the wording.
- **`issue-136-rate-aterm` and `issue-148-check-fit` have merged and must not be
  deleted.** They are the bases of PRs #225 and #238, and deleting a merged base
  closes the PR above it irrecoverably.
