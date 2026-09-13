# Cancelled check jobs on pull requests

Measured 2026-09-13 by `notes/scripts/ci_devel_cancellation.R`, over the 200 most
recent `pull_request` runs of `R-CMD-check`, of which 196 had all four matrix
jobs finished and are the denominator throughout. The window is
2026-08-24 08:27 to 2026-09-13 14:29 UTC. Elapsed time is `completed_at` minus
`started_at` from the jobs API. Re-running the script samples a later window and
will not reproduce these figures; it prints the window it sampled.

A *supersede* here means a run stopped by GitHub because a newer commit was
pushed to the same pull request, which is what `concurrency.cancel-in-progress`
in `.github/workflows/R-CMD-check.yaml` is configured to do. The API reports it
as `cancelled`, indistinguishable from a manual stop.

This document exists because #333 reported that the `ubuntu-latest (devel)` job
is cancelled on two thirds of pull-request runs and read that as a gap in
R-devel coverage before merge. The reading is not supported. The figures are
here rather than in the workflow file because there are too many of them to keep
current in a comment.

## The structure of the cancellations

Which jobs of a run were cancelled together, over 196 runs:

| cancelled | runs |
|---|---|
| nothing | 134 |
| all four | 44 |
| `ubuntu-latest (devel)` alone | 10 |
| `windows-latest (release)` alone | 4 |
| two or three jobs | 4 |

The 44 runs whose four jobs went together were superseded a median 12.1 minutes
in, range 0.6 to 24.4, measured as the span from the first job starting to the
last one stopping. None of them had finished a job. They discard nothing that a
later run does not supply, and they are what `cancel-in-progress` is for.

The asymmetry #333 describes, three jobs reported and devel lost, is the ten runs
in the third row.

## The cancellation rate over time

Cancellation rates over the 196 runs: `ubuntu-latest (devel)` 29.1 per cent,
`windows-latest (release)` 26.5, `ubuntu-latest (release)` 24.0,
`macOS-latest (release)` 23.0.

That pooled figure describes no day in the window. By day:

| day | runs | devel cancelled |
|---|---|---|
| 2026-08-24 | 9 | 0% |
| 2026-08-25 | 14 | 0% |
| 2026-08-26 | 8 | 0% |
| 2026-08-27 | 2 | 0% |
| 2026-09-02 | 8 | 0% |
| 2026-09-03 | 8 | 0% |
| 2026-09-05 | 5 | 0% |
| 2026-09-06 | 13 | 0% |
| 2026-09-07 | 13 | 8% |
| 2026-09-08 | 9 | 0% |
| 2026-09-09 | 19 | 42% |
| 2026-09-10 | 21 | 33% |
| 2026-09-11 | 26 | 58% |
| 2026-09-12 | 20 | 70% |
| 2026-09-13 | 21 | 57% |

No devel job was cancelled on any of the eight days to 2026-09-06. From
2026-09-09 the rate runs between 33 and 70 per cent. This is a step change with
an onset, not noise around a mean, and the high part of it is the current
regime: #333's 68 per cent, from 36 runs, describes the present accurately and
the pooled 29.1 per cent is diluted by two weeks in which cancellation did not
happen at all.

What changed is how often the branches are pushed, and that is measurable. Taking
each run and the interval to the next run on the same branch: where the devel job
was cancelled the median interval is 14.1 minutes (n = 56), and where it
succeeded, 97.6 minutes (n = 88). A run is superseded when another push arrives
before it finishes, and a stacked set of pull requests being propagated through
supplies those pushes.

## Coverage of the commit that merges

Of the 30 most recently merged pull requests, 30 had a successful
`ubuntu-latest (devel)` check on the head commit, completed before the merge
timestamp. None merged without one.

The commit tested is the right one. A `pull_request` run checks out
`refs/pull/N/merge`, so the devel job compiled and checked the head merged into
its base, and GitHub attaches the resulting check run to the head. The merge
commit that lands on `dev` is a different object, and it is checked afterwards by
the `push` trigger.

The result is an observation, not a configuration. `dev` has no branch protection
(`gh api repos/open-AIMS/bayesnec/branches/dev/protection` returns 404) and the
repository has no rulesets, so `ubuntu-latest (devel)` is not a required status
check and nothing prevented a merge without it.

On `dev` itself, of the last 40 pushes with a settled devel job, 35 succeeded and
5 were cancelled. All five were superseded: a later push to `dev` was created
while the devel job was still running.

What is measured is that the check existed, succeeded and completed before the
merge. Whether a reviewer read it is not measurable from the API.

## Elapsed time by matrix cell

Over jobs that ran to a conclusion. A cancelled job is right-censored — its
elapsed time says where it had reached, not how long it would have taken — so
these medians are biased downwards, and the bias is heavier for the longer jobs.

| job | n | median | maximum |
|---|---|---|---|
| `macOS-latest (release)` | 151 | 25.7 | 45.1 |
| `ubuntu-latest (release)` | 149 | 32.0 | 55.6 |
| `ubuntu-latest (devel)` | 139 | 36.0 | 72.7 |
| `windows-latest (release)` | 144 | 40.6 | 64.2 |

Paired within a run, which removes the between-run variation:

| pair | n | median | Hodges-Lehmann (95% CI) | longer in |
|---|---|---|---|---|
| devel minus `ubuntu-latest (release)` | 126 | +2.4 | +2.61 (1.62 to 3.67) | 87 |
| `windows-latest` minus devel | 110 | +6.4 | +6.26 (5.24 to 7.23) | 96 |

Censoring is not symmetric: 11 pairs were lost because devel was cancelled and
the release job was not, against 1 the other way. Admitting failures as well as
successes recovers 12 of those pairs and enlarges the difference rather than
removing it — n = 138, Hodges-Lehmann +3.18, 95 per cent interval 2.20 to 4.46 —
so the conclusion does not rest on the censoring.

`ubuntu-latest (devel)` therefore runs between 1.6 and 3.7 minutes longer than
`ubuntu-latest (release)`, and `windows-latest (release)` is the slowest job in
the matrix, longer than devel by 5.2 to 7.2 minutes.

#333 reported 25 minutes. That figure was CPU time, not elapsed, so it is not the
quantity measured here. It is also not what #333's own CPU ranges give: 42 to 56
minutes for devel against 31 to 51 for the release job is a difference of 5 to 11
minutes, taken at either end. The elapsed measurement and #333's own evidence
agree that the gap is single-digit minutes.

## The tail against the ninety-minute bound

The longest devel job that ran to a conclusion took 72.7 minutes and reported
`failure`. Seven devel jobs ran past 60 minutes to a conclusion and five past 70,
one of them a success at 72.4 minutes (run 34066060038). No release job in the
window exceeded 64.2.

`timeout-minutes: 90` in the workflow is therefore 1.24 times the longest
completed devel job, not the wide margin its comment claimed when it was set
against a 42-minute maximum. The bound has not yet stopped a legitimate job — no
job of any cell reached 90 in 583 completed observations — so it is left as it
is, and the margin is recorded here so the decision can be revisited on evidence
rather than rediscovered. If it does fire, the `if: always()` log dump in the
same workflow reports where the suite had reached, so a false positive is
diagnosable rather than bare.

## Job scheduling and runner capacity

The four cells run in parallel, not in a queue. The median delay from a job being
created to starting is 2 seconds, and the median gap between the first and last
of a run's four jobs starting is 0.1 minutes. Up to 12 `R-CMD-check` jobs ran
simultaneously in the window, median 4 while any were running.

Queueing is real at the tail: the 90th centile of the create-to-start delay is
870 seconds and the maximum 9,900, and 36 runs had more than a minute between
their first and last job starting, to a maximum of 102 minutes. Nine of the 800
job rows report a negative delay, which is what the API returns for a re-run.

This supersedes the description of runner capacity at the head of
`.github/workflows/R-CMD-check.yaml`, which was written when duplicate `push` and
`pull_request` runs had filled the queue and states that the repository runs one
job at a time. The reason for the `push` filter is unaffected: halving the number
of runs is worth doing whether or not they queue.

## Reproduction

```bash
Rscript notes/scripts/ci_devel_cancellation.R 200 30
```

Requires the `gh` CLI, authenticated, and takes about five minutes. It needs no R
package beyond base, `stats` and `utils`, because it reads TSV from `gh --jq`.

Two properties of the API bound what it can establish. The jobs endpoint defaults
to `filter=latest`, so a re-run reports the re-run's conclusion and the original
attempt is invisible; three runs in this window had `run_attempt` above 1, too
few to change any figure. And the API reports a supersede and a manual stop
identically, as `cancelled`, so a supersede is inferred from the timing of a
later run rather than read directly.
