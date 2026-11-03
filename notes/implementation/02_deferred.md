# Deferred work

Everything open in `open-AIMS/bayesnec` that is not in `01_work_queue.md`,
recorded so the exclusion is a decision rather than an oversight.

Refreshed 2026-09-26 for the backlog run. The version of 2026-09-14 listed #209,
#249, #283 and #296 as deferred; all four have closed. It is in the history of
this file.

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
