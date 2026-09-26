# Deferred work

Everything open in `open-AIMS/bayesnec` that is not in `01_work_queue.md`,
recorded so the exclusion is a decision rather than an oversight.

Refreshed 2026-09-26 for the backlog run. The version of 2026-09-14 listed #209,
#249, #283 and #296 as deferred; all four have closed. It is in the history of
this file.

## The toxval migration

`ecx()`, `nsec()`, `ecnsec()` and `zero_crossings()` are still to relocate to
[open-AIMS/toxval](https://github.com/open-AIMS/toxval), which will become a
dependency of `bayesnec`. The migration is excluded from the backlog run by RF
(2026-09-26). #255 is the tracker for the `bayesnec` side and holds the method
inventory and the dependency reversal; neither is restated here.

D11 stands: a fix to an estimator is made in `bayesnec`, and recorded on the
matching toxval issue as it lands, so that the migration relocates the corrected
file. The backlog run changes `ecx()`, `nsec()` and their callers in several
items, and each records itself on toxval the same way.

#120 and #44 were deferred to the migration by the version of this file dated
2026-09-14. D19 (2026-09-26) takes both into the backlog run on D11's reasoning,
so they are no longer deferred.

## Implemented elsewhere

| | |
|---|---|
| #382, #388 | PR #402, into `predev`, being repaired by another session. Items 13 and 14 of the queue wait for it (D18) |

## Deferred for a decision

| | |
|---|---|
| #27 | a truncated gaussian, motivated by the `nassarius` growth data. Kept open (D27); an investigation is drafting the issue body with fitted evidence, and the implementation needs RF's decisions |

## Housekeeping carried forward

- `DESCRIPTION` requires `brms (>= 2.23.0)`; earlier versions mis-generate
  `beta_binomial`.
- A pull request into `dev` or `predev` does not fire `Closes #n`, because
  neither is the default branch. Close issues by hand after verifying the work is
  on `dev`.
- `issue-136-rate-aterm` still exists on the remote. PR #225, which it was the
  base of, merged, so the constraint recorded against deleting it has lapsed.
