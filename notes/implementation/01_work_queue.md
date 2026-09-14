# Work queue — the training-course run

Read `00_protocol.md` first, then `03_decisions.md`.

## The rebuild

Rebuilt 2026-09-14. The queue that stood here was written on 2026-09-06 and
gave four stacked pull requests as the state of the work. All four have merged,
and so has the second of the three vignettes, so every batch it describes is
complete and none of it says so. The board has since gained eighteen open issues
that no queue covers. The file is rebuilt rather than amended, on the same
reasoning `02_deferred.md` records: an entry describing work already done sends a
reader to the wrong place.

## The goal this queue serves

A training course is to be written against the development version of
`bayesnec`. That fixes the end state: `dev` must be installable, its estimators
must return correct values, and its vignettes must be finalised and re-rendered,
because the course teaches from them. The release to CRAN is still not a gate on
anything here.

`dev` is at 2.1.3.35. `NEWS.md` headings are `# bayesnec 2.2.0` and
`# bayesnec 2.1.4`; new entries go under 2.2.0.

---

# Current state

## Merged since the last rebuild

| PR | closed |
|---|---|
| #281 | #195, #196, #39, #206, #268, #160, #161 |
| #282 | #274, #271, #272, #266, #93, #262, #261, #218 |
| #284 | #257 |
| #286 | #273 |
| #238 | #219, the `example9` workflow vignette |
| #348 | #343, estimates and prior samples reproducible |

#184 and #285 are also closed. Batches 1 to 4 and batch 5b of the 2026-09-06
queue are therefore finished, and #228's blocker was settled by #284 as that
queue expected.

## Open pull requests

| PR | closes | state |
|---|---|---|
| #347 | #338, #329 | open |
| #243 | #193, `example7` | open, batch 5a |
| #228 | #6, #33 | open, batch 5c |
| #225 | #209 | draft, blocked on `brms` through #249 |

Eighteen open issues are covered by none of them.

---

# The constraint that sets the order

#190, the full re-run of `precompile.R`, takes more compute than anything else on
the board: `example7` alone takes about 137 minutes locally and about 2 h 56 m
in CI.
Five open issues change what a vignette prints, so each of them either lands
before the re-render or is spent on a second one.

| # | what it changes in the rendered output |
|---|---|
| #317 | the `nec` and `ec50` prior, so every fit changes |
| #319 | whether `example8` renders at all — the precompile errors at its first fit |
| #344 | `ecxhormebc5` becomes an exclusion with a reason in all six `lum31` fits |
| #310 | whether the committed output is the same from one run to the next |
| #299 | every ECx, NEC and NSEC reported, if it is decided as a change |

#340 is the sixth precondition and is of a different kind: until `precompile.R`
loads the checkout it is run from, a render cannot be attributed to a branch, so
the re-run cannot be checked. #190 now names #319, #340 and #310 in its
`Blocked by:` line.

---

# The order

## Throughput

Hours rather than days, and taken first because four pull requests are open
against `dev` and each item after this one is read through the same check
matrix.

| # | what |
|---|---|
| #311 | twelve `open_progress = FALSE` calls in `tests/testthat/`. Mechanical, and it is why a `cmdstanr`-backend run of the suite fails, which is needed before any measurement of #328 |
| #328 | memoise the repeated fixtures, then set `TESTTHAT_CPUS`. The test phase is 21 to 26 minutes of a 25 to 31 minute job, and the issue holds the measured plan and the changes not to make |
| #333 | a design decision between three options. The cheapest is one documentation commit, and it stops the next cancelled devel job being investigated as a branch defect |

## Defects that change a reported number

| # | what |
|---|---|
| #317 | the predictor is classified as already logged by `min(x) < 0`, so a series whose lowest dose is at or above 1 receives a prior built from the log of a log. Present on `master`, so it is in the released package, and which prior a user receives depends on the units the concentrations were recorded in |
| #319 | `crf()` resolves its `model` argument in its own frame, so a variable model set is found only in the global environment. The same call fails inside a function and under `knitr` |
| #344 | `ecxhormebc5` fails to initialise on a negative predictor under a positive family. `check_models()` already declines `ecxsigm` on a negative predictor with a reason |
| #299 | the estimators return the fitted scale and `autoplot()` draws the recorded one. Three options are on the issue and the choice is RF's; its position in this order depends on whether it is wanted in the next release |

## The re-render

| # | what |
|---|---|
| #340 | `precompile.R` renders against the installed package rather than the checkout |
| #310 | re-measure on current `dev` before working on it. #309 and #343 have both merged since the measurement was taken; the variable-length loop at `R/inits_functions.R:988` is still there, so the finding is expected to stand |
| #190 | the full precompile, attended, once the two sections above it are settled |

## The decision behind #296 and #283

Both wait on PR #243 merging, and they are one decision rather than two tasks.
#283 re-runs the precision sweep under a dispersion sub-model; #296 proposes
retiring the design that sweep scores. Take the #296 decision first, because
running the sweep before it spends about three hours of CI on a study that may
not survive.

## Features with no gate

#301, then #27, last. Neither blocks anything else.

---

# Not in this queue

| # | why |
|---|---|
| #255, #120, #44 | the toxval migration. #255's own table records #120 (toxval registers the same `predict`, `plot` and `autoplot` methods) and #44 ("new API, belongs with the estimators") as gated on toxval#39 and toxval#45 |
| #249, PR #225 | the factorised count hurdle, blocked on `brms` upstream. `issue-136-rate-aterm` is PR #225's base and **must not be deleted** |
| #27 | zero-truncated gaussian. The body is empty, and a title is not a specification. Listed under features with no gate, not as something a session can start from the issue alone |

---

# Housekeeping

- **Prune the worktrees.** Twenty-one are registered and every branch among them
  has merged into `dev`: `issue-310-init-seed`, `issue-319-crf-env`,
  `issue-333-devel-cancellation` and `issue-344-ecxhormebc5-init` are all zero
  commits ahead, so the four open issues of those names have no work in progress
  anywhere. `00_protocol.md` names the worktrees that must not be touched.
- **Every pull request here targets `dev` rather than the default branch, so
  `Closes #n` never fires.** Close issues by hand after verifying the work is on
  `dev`. Nothing is merged-and-open today; #338 and #329 will need it when #347
  merges.
- `DESCRIPTION` requires `brms (>= 2.23.0)`; earlier versions mis-generate
  `beta_binomial`.
