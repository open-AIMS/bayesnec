# Deferred, and why

Everything open in `open-AIMS/bayesnec` that is **not** in `01_work_queue.md`.
Recorded so the exclusion is a decision rather than an oversight.

## Out of scope — migrating to `toxval`

`ecx()`, `nsec()`, `ecnsec()` and `zero_crossings()` are moving to
[open-AIMS/toxval](https://github.com/open-AIMS/toxval), which will become a
dependency of `bayesnec`. Fixing them here would be work thrown away.

| | |
|---|---|
| #39 | `uniroot.all` for speed and precision — see the spike, and note it forces the `type` reference semantics to be pinned down, so it should be done with #195 |
| #44 | hypothesis method for *NEC*/*NSEC*/*ECx* exceedance — new API, belongs with the estimators |
| #166 | `zero_crossings()` misses narrow crossings — **duplicate of toxval #29**, close as such |
| #195 | inert `hormesis_def`, divergent `type` implementations, wrong documented default |
| #196 | inline `crf()` transformations back-transformed incorrectly |

**Prerequisite for all of these:** toxval currently has `bayesnec` in its
`Imports`, so the dependency runs the wrong way, and both packages already
register `predict.bayesnecfit`, `predict.bayesmanecfit`, `nsec.brmsfit` and
`nsec.drc`. That collision is live today. Untangling it comes before any of the
five.

## Owned by another session

| | |
|---|---|
| #193 | rewrite `example7` as a growth-data case study |

## Not specified well enough to implement unattended

These have **empty or near-empty bodies** — a title is not a specification, and
guessing at the intended design produces a PR that has to be thrown away.

| | body |
|---|---|
| #6 | *empty* — add capacity for random structure |
| #27 | *empty* — zero-truncated gaussian |
| #33 | *empty* — factor covariate |

#6 and #33 are also large: group-level effects and factor covariates both change
the formula interface, the prior machinery and every post-processing path. They
are design work with an author, not queue items.

## Decided on 2026-08-14 and moved into the queue

#104, #120 and #141 were in this section until the author settled their shape.
See D4, D5 and D9 in `03_decisions.md`; they are now queue items 11, 10 and 12.

## Needs a design decision first

Each is a legitimate request whose *shape* is undetermined. An unattended
session would be inventing the API rather than implementing one.

| | the undetermined part |
|---|---|
| #56 | which validation plots, and whether to depend on `DHARMa` (a new dependency) |
| #84 | how a user names parameters to fix, and how fixing interacts with prior generation |
| #136 | `rate` aterm versus `offset`; the issue itself offers both |
| #139 | which `drc` *NEC* parameterisations to add, and how they are named alongside the existing set |
| #148 | rescoped under D6, but still needs a specific list of diagnostics before it is actionable |
| #149 | automatic family switching on overdispersion — a decision rule that changes what users get by default, and #180 first, since it changes what a fit stores |

## Deliberately excluded from unattended work

| | |
|---|---|
| #184 | `future_apply` parallelism. The issue itself says "investigate first, resolve the design questions, then implement", and lists correctness hazards. Concurrency plus RNG seeding is exactly the class of change that fails silently and is expensive to detect — and seeding in this package is already subtle enough that `set.seed()` alone does not reproduce a fit. Wants an attended session. |
| #190 | full `precompile.R` run. Mechanical but takes hours, needs network access to pastebin at authoring time, and must run **after** the queue merges, or it is immediately stale. Do it once, at the end. #180 in particular changes what every fit stores. |

## Housekeeping carried forward

Not issues, but they should not be lost:

- `DESCRIPTION` now requires `brms (>= 2.23.0)`; the constraint went in with the
  `?nsec` roxygen fix.
- The `disp()` documentation carries a caution about inline `crf()`
  transformations that must be removed when #196 is fixed — recorded on #196.
