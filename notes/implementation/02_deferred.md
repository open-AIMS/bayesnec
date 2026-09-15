# Deferred, and why

Everything open in `open-AIMS/bayesnec` that is **not** in `01_work_queue.md`.
Recorded so the exclusion is a decision rather than an oversight.

## The refresh

Refreshed 2026-09-14. The estimator entries that stood here have all merged, so
the toxval section below records what landed and what the migration still gates,
rather than what is waiting on it. The dated blocks from 2026-09-03 and
2026-09-06 are removed with the entries they described, on this file's own
standing rule: an entry describing work already done is worse than no entry,
because it sends a reader to the wrong place.

---

## The toxval migration

`ecx()`, `nsec()`, `ecnsec()` and `zero_crossings()` are still to relocate to
[open-AIMS/toxval](https://github.com/open-AIMS/toxval), which will become a
dependency of `bayesnec`. The migration itself is deferred (RF, 2026-09-06):
toxval#39, the dependency reversal, has not started, and a training course is to
be written against `dev`.

The estimator fixes were taken in `bayesnec` on the D11 reasoning rather than
held for the migration, and all of them have merged: #195, #196, #39, #206, #160
and #161 closed with PR #281. The corrected code relocates with the file when the
migration runs, so nothing is discarded. Record each fix on the matching toxval
issue as it lands, so the migration re-lands the corrected version rather than
the one toxval forked.

Two entries are gated on the migration and stay out of the queue. #255's own
table is the record of why, and it is not restated here.

| | |
|---|---|
| #44 | hypothesis method for *NEC*, *NSEC* or *ECx* exceedance. A new API that belongs with the estimators, and tracked as toxval#41 |
| #120 | replacing `all_models` in `predict`, `plot` and `autoplot` for `bayesmanecfit`. D5's precondition is met — `test-plot.R` and `test-autoplot.R` exist — so what defers it is that toxval registers the same three methods on the same class |

#255 is the tracker for the `bayesnec` side, opened 2026-08-26. It holds the
method inventory and the dependency reversal; do not restate either here.

The `bayesnec` release does not gate the migration (RF, 2026-09-03), and the
migration no longer gates the estimator fixes (RF, 2026-09-06). What remains of
D10 is the file-relocation ordering alone.

---

## Deferred, with the reason still current

| | |
|---|---|
| #209 | `hurdle_poisson` and `hurdle_negbinomial`. PR #225 is open as a draft, blocked on the `brms` defect tracked as #249. Its base branch `issue-136-rate-aterm` has merged and **must not be deleted** |
| #249 | the factorised count hurdle. Blocked on `brms`, upstream. `CLAUDE.md` §11 of the working directory records the truncation defect it waits on |
| #283 | the `example7` dispersion sub-model and the re-run of the precision sweep. Downstream of PR #243, and downstream of the #296 decision, which may retire the design the sweep scores. The re-run is about three hours of CI, so the decision comes first |
| #296 | redesigning the `negative-sgr` study around the model-averaged workflow. RF's decision, and it sets whether #283 is run at all |
| #27 | zero-truncated gaussian. The body is empty, and a title is not a specification. Listed in the queue under features with no gate, which is not the same as being ready to start |

---

## Moved out of deferred

Recorded so the change is visible rather than silent.

| | what changed |
|---|---|
| #195, #196, #160, #161, #206, #39 | merged with PR #281 |
| #184 | `future_apply`. The attended testing pass was run and the work merged; #338 and #329 are what remain of it, and both are in PR #347 |
| #218 | `compare_posterior()` draw pairing. Merged with PR #282 as documentation and a constraint |
| #190, #248 | #248 closed with the `example2` re-render. #190 is in the queue, with #319, #340 and #310 as its recorded blockers |
| #219, #193, #6, #33 | the three vignettes. #219 merged with PR #238; #193 is PR #243 and #6 and #33 are PR #228, both open |

---

## Not specified well enough to implement unattended

A title is not a specification, and guessing at the intended design produces a
pull request that has to be thrown away.

| | body |
|---|---|
| #27 | *empty* — zero-truncated gaussian |

#33 also stays open for stage 2, which is gated on the toxval migration and so is
deferred with it.

---

## Housekeeping carried forward

- `DESCRIPTION` requires `brms (>= 2.23.0)`; earlier versions mis-generate
  `beta_binomial`.
- **Every pull request here targets `dev` rather than the default branch, so
  `Closes #n` never fires.** Writing `Closes #n` in the body changes nothing; the
  keyword's behaviour depends on the base branch. Close by hand after verifying
  the work is on `dev`. Nothing is merged-and-open today, and #338 and #329 will
  need it when #347 merges.
- **`issue-136-rate-aterm` has merged and must not be deleted.** It is the base of
  PR #225, and deleting a merged base closes the pull request above it
  irrecoverably.
