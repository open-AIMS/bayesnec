# Cancelled check jobs on pull requests

#333 reported that the `ubuntu-latest (devel)` job of `R-CMD-check` is cancelled
on about two thirds of pull-request runs, and read that as a gap in R-devel
coverage before merge. The rate is real for the days it was measured on. The gap
is not: over the 30 most recently merged pull requests, every one had a
successful devel check on its head commit, completed before the merge.

Measured 2026-09-13 by `notes/scripts/ci_devel_cancellation.R`, over the 200 most
recent `pull_request` runs of `R-CMD-check`, of which 197 had all four matrix
jobs finished and are the denominator throughout. The window is
2026-08-24 09:11 to 2026-09-13 14:41 UTC. Elapsed time is `completed_at` minus
`started_at` from the jobs API. Re-running the script samples a later window and
will not reproduce these figures; it prints the window it sampled.

A *supersede* is a run stopped by GitHub because a newer commit was pushed to the
same pull request, which is what `concurrency.cancel-in-progress` in
`.github/workflows/R-CMD-check.yaml` is configured to do. Every unfinished job of
that run is then reported as `cancelled`.

## The structure of the cancellations

Which jobs of a run were cancelled together, over 197 runs:

| cancelled | runs |
|---|---|
| nothing | 133 |
| all four | 44 |
| `ubuntu-latest (devel)` alone | 10 |
| `windows-latest (release)` alone | 5 |
| two or three jobs | 5 |

The 44 runs whose four jobs went together were superseded a median 12.1 minutes
in, range 0.6 to 24.4, measured as the span from the first job starting to the
last one stopping. None of them had finished a job. They discard nothing that a
later run does not supply, and they are what `cancel-in-progress` is for.

The asymmetry #333 describes, three jobs reported and devel lost, is the ten runs
in the third row.

## The cancellation rate over time

Cancellation rates over the 197 runs: `ubuntu-latest (devel)` 29.4 per cent,
`windows-latest (release)` 27.4, `ubuntu-latest (release)` 23.9,
`macOS-latest (release)` 23.4.

That pooled figure describes no day in the window. By day:

| day | runs | devel cancelled |
|---|---|---|
| 2026-08-24 | 7 | 0% |
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
| 2026-09-13 | 24 | 54% |

No devel job was cancelled on any of the eight days to 2026-09-06. From
2026-09-09 the rate runs between 33 and 70 per cent. That is a step change with
an onset, not variation about a mean, and the pooled 29.4 per cent is the two
regimes averaged together.

Two figures describe the second regime, and they are not the same quantity. Over
every run from the onset on 2026-09-09, 110 of them, the rate is 51.8 per cent.
Over the 36 most recent runs, which is the window #333 sampled, it is 63.9 per
cent. #333's 68 per cent is therefore an accurate reading of the days around its
own measurement, and about twelve points above the regime rate.

What changed is how often the branches are pushed. Taking each run and the
interval to the next run on the same branch: where the devel job was cancelled
the median interval is 14.1 minutes (n = 56), and where it succeeded, 100.0
minutes (n = 88). A run is superseded when another push arrives before it
finishes, and a stacked set of pull requests being propagated through supplies
those pushes. Forty-one runs are the most recent on their branch and have no
successor in the sample; cancellations concentrate among recent runs, so those
41 are drawn disproportionately from the cancelled group and the contrast above
is understated rather than manufactured.

## Coverage of the commit that merges

For each of the 30 most recently merged pull requests, the check runs attached to
`headRefOid`, the tip of the source branch, were read and tested for a successful
`ubuntu-latest (devel)` completing before the merge timestamp. All 30 had one.
None merged without one.

That is the right commit to ask about. A `pull_request` run checks out
`refs/pull/N/merge`, so the devel job compiled and checked the head merged into
its base, and GitHub attaches the resulting check run to the head. The merge
commit that lands on `dev` is a different object, and it is checked afterwards by
the `push` trigger.

The result is an observation, not a configuration. `dev` has no branch protection
(`gh api repos/open-AIMS/bayesnec/branches/dev/protection` returns 404) and the
repository has no rulesets, so `ubuntu-latest (devel)` is not a required status
check and nothing prevented a merge without it.

On `dev` itself, 40 of the last 40 pushes had a settled devel job: 35 succeeded
and 5 were cancelled. All five were superseded, in each case verified by finding
a later push to `dev` created while the devel job was still running.

What is established is that the check existed, succeeded and completed before the
merge. Whether a reviewer read it is not measurable from the API.

## Elapsed time by matrix cell

Over jobs that ran to a conclusion. A cancelled job is right-censored — its
elapsed time says where it had reached, not how long it would have taken — so
these medians are biased downwards, and the bias is heavier for the longer jobs.

| job | n | median | maximum |
|---|---|---|---|
| `macOS-latest (release)` | 151 | 25.7 | 45.1 |
| `ubuntu-latest (release)` | 150 | 32.0 | 55.6 |
| `ubuntu-latest (devel)` | 139 | 36.0 | 72.7 |
| `windows-latest (release)` | 143 | 40.8 | 64.2 |

`macOS-latest` is the fastest cell and `windows-latest` the slowest, by a median
of 15 minutes. Only `ubuntu-latest (devel)` has a maximum above 65 minutes, which
the next section takes up.

Paired within a run, which removes the between-run variation:

| pair | n | median | Hodges-Lehmann (95% CI) | longer in |
|---|---|---|---|---|
| devel minus `ubuntu-latest (release)` | 125 | +2.4 | +2.57 (1.53 to 3.61) | 85 |
| `windows-latest` minus devel | 109 | +6.5 | +6.35 (5.32 to 7.35) | 95 |

So `ubuntu-latest (devel)` runs between 1.5 and 3.6 minutes longer than
`ubuntu-latest (release)`, and `windows-latest (release)` is the slowest job in
the matrix, longer than devel by 5.3 to 7.4 minutes.

Censoring is not symmetric: 12 pairs were censored because devel was cancelled
and the release job concluded, against 1 the other way. Those 12 cannot be
recovered, because a cancelled job has no duration. They can be bounded. In each,
devel had already been running when it was stopped, and comparing that against
what the release job took gives a median of +1.57 minutes with 9 of the 12
positive. Each of those is a value the pair's true difference lies above, so
admitting the censored pairs could only enlarge the estimate, never reverse it.

Widening the criterion to admit failures as well as successes adds 13 different
pairs, none of them censored ones, and takes the estimate from +2.57 to +3.07
(n = 138, 95 per cent interval 2.08 to 4.35). Both checks point the same way: the
censoring understates the difference rather than creating it.

#333 reported 25 minutes. That figure was CPU time, not elapsed, so it is not the
quantity measured here. It is also not what #333's own CPU ranges give: 42 to 56
minutes for devel against 31 to 51 for the release job is a difference of 5 to 11
minutes, taken at either end. The elapsed measurement and #333's own evidence
agree that the gap is single-digit minutes.

## The tail against the ninety-minute bound

Centiles of the 139 devel jobs that ran to a conclusion: median 36.0, 90th 54.8,
95th 59.8, 99th 72.2, maximum 72.7. Seven ran past 60 minutes and five past 70.

The tail is one afternoon. The eight longest, with the day each started:

| minutes | started | ended |
|---|---|---|
| 72.7 | 2026-09-06 | failure |
| 72.4 | 2026-09-06 | success |
| 71.8 | 2026-09-06 | failure |
| 71.7 | 2026-09-06 | failure |
| 70.7 | 2026-09-06 | failure |
| 69.8 | 2026-09-06 | failure |
| 69.4 | 2026-09-06 | failure |
| 58.7 | 2026-08-25 | success |

Seven of the eight started on 2026-09-06, six of those seven ended `failure`, and
there is a 10.7-minute gap to anything else in the sample. No release job in the
window exceeded 64.2 minutes.

`timeout-minutes: 90` was set against a 42-minute maximum, which this measurement
supersedes. Against the observed maximum the margin is 1.24 times; against the
95th centile, which is where the routine work sits, it is 1.5. The bound is left
as it is. Nothing of any cell has reached it in 583 completed observations, the
excursion that produced the 1.24 figure is one day of mostly failing checks
rather than a standing property of the job, and the `if: always()` log dump in
the same workflow reports where the suite had reached, so a timeout is
diagnosable rather than bare. The figures are here so the decision can be
revisited on evidence rather than rediscovered.

## Job scheduling and runner capacity

Over the 788 job rows, the median delay from a job being created to starting is 2
seconds, and the median gap between the first and last of a run's four jobs
starting is 0.1 minutes. The four cells therefore run in parallel rather than in
a queue. Counting started minus completed across the window, up to 16
`R-CMD-check` jobs ran simultaneously, median 4 while any were running. That
maximum is the busiest instant of one window and will differ on another; the
0.1-minute stagger is the stable form of the same fact.

Queueing is real at the tail. The 90th centile of the create-to-start delay is
860 seconds and the maximum 9,900, and 36 runs had more than a minute between
their first and last job starting, to a maximum of 102 minutes. Nine of the 788
rows report a negative delay, which is what the API returns for a re-run;
dropping those nine gives a median of 3 seconds and a 90th centile of 878.

This supersedes the description of runner capacity at the head of
`.github/workflows/R-CMD-check.yaml`, written when duplicate `push` and
`pull_request` runs had filled the queue. The reason for the `push` filter is
unaffected: halving the number of runs is worth doing whether or not they queue.

## The rejected alternatives

Exempting devel from cancellation adds nothing to the coverage established above.
It would also add runner time: 58 devel jobs were cancelled in the window, and
estimating each one's remaining time from the concluded jobs that ran longer than
it had gives about 1,288 further runner-minutes over three weeks. Those minutes
are unbilled on a public repository, so what is spent is queue latency rather
than money. It takes more than the obvious per-matrix-cell concurrency group as
well: a job-level group does not override the workflow-level
`cancel-in-progress`, which stops the whole run first, so every cell would need
its own group and the workflow-level setting would have to be removed.

Shortening devel would not change which job is unfinished when a supersede
arrives, because devel is not the job that is still running. It is 1.5 to 3.6
minutes longer than `ubuntu-latest (release)` and 5.3 to 7.4 minutes shorter than
`windows-latest`.

## Reproduction

```bash
Rscript notes/scripts/ci_devel_cancellation.R 200 30
```

Requires the `gh` CLI, authenticated, and takes about five minutes. It needs no R
package beyond base, `stats` and `utils`, because it reads TSV from `gh --jq`.

Three properties of the API bound what it can establish. The jobs endpoint
defaults to `filter=latest`, so a re-run reports the re-run's conclusion and the
original attempt is invisible; three runs in this window had `run_attempt` above
1, too few to change any figure. The API reports a supersede and a manual stop
identically, as `cancelled`, so a supersede is inferred from the timing of a
later run rather than read directly. And the run listing is fetched as two pages
of 100, which have been observed to disagree: one fetch returned 113 distinct
runs spanning nine months. The script stops rather than reporting the short
sample.
