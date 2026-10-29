# Autonomous implementation protocol

How a Claude Code session works through a queue of issues without supervision.
Read this first, then the run's plan in `notes/tasks/`, which gives the queue,
its order and any deadline, then `03_decisions.md` for RF's rulings. Where a
ruling made for one run, such as D18, differs from this file, the ruling holds
for that run.

## Branches and merging

`predev` is the integration branch for an unattended run. Every pull request
targets it, and `predev` reaches `dev` later as one pull request that RF reviews.
`master` is never touched.

One item, one branch, one pull request, each cut from the tip of `predev`.
Branches do not stack unless the run's plan says so. After an item merges, the
next branch is cut from the new tip:

```bash
git fetch origin predev
git checkout -B issue-<n>-<slug> origin/predev
# ... work, commit ...
git push -u origin issue-<n>-<slug>
gh pr create --repo open-AIMS/bayesnec --base predev
```

The session merges, and only when every check has passed:

```bash
gh pr merge <pr> --repo open-AIMS/bayesnec --merge
```

Never `--auto`: auto-merge is disabled on the repository, so the flag merges at
once without waiting for the checks. Poll the checks instead, about every ten
minutes (`gh pr checks <pr>`). An empty check list on an open pull request means
it conflicts with its base, not that the checks are queued. A cancelled job is
usually superseded by a newer push; look for a newer run before investigating.

Push once per item where possible. The repository runs one job at a time, and
each push to an open pull request cancels and requeues its matrix. Finish the
review before the first push.

Close each issue an item covers once its pull request has merged into `predev`.
The closing comment names the pull request and the merge commit, and asks for the
issue to be reopened if RF's review of `predev` finds a problem. `Closes #n` does
not fire on a merge into `predev` or `dev`, because neither is the default
branch, so close the issue by hand. Where branches do stack, a pull request
merges into the branch below it rather than into `predev`. Confirm with
`git merge-base --is-ancestor <merge commit> origin/predev` before closing.

Notes-only changes go straight to `predev`, with no pull request. A commit that
touches `notes/**` and nothing else is not read by `R CMD check`, so a pull
request for it spends a four-platform matrix on nothing. A commit that touches
one line of `R/`, `tests/`, `man/`, `DESCRIPTION`, `NAMESPACE`, `vignettes/` or
`.github/` is not a notes change.

## Worktree and machine

One worktree for the whole run, created by the run beside the main checkout. Do
not work in `/mnt/c/Rworking/bayesnec`: it is RF's main checkout, and the Windows
and WSL paths are the same directory, so two sessions there overwrite each other.

```bash
git -C /mnt/c/Rworking/bayesnec fetch origin predev
git -C /mnt/c/Rworking/bayesnec worktree add --detach \
    /mnt/c/Rworking/bayesnec-<run> origin/predev
```

Never touch a worktree or a branch this run did not create.

The machine has 22 cores. Check `uptime` before any fitting and take no more
than half of the idle cores. Run each Stan fit as a separate `Rscript` process:
a fit inside a `future` `multisession` worker deadlocks on this machine. Kill
processes by PID, never by a `pkill` pattern, which has killed the invoking shell
in this project.

Wait and launch in separate tool calls. On #207 a job launched in the same call
as a long `until` loop that preceded it did not run, and the call still reported
success. Before reporting a job as running, confirm that its log file exists and
its process is alive.

## Definition of done, per item

1. The behaviour in the specification is implemented, and nothing beyond it.
2. `testthat` tests covering the main behaviour and at least one edge case, in
   `tests/testthat/test-<file>.R` following the existing names.
3. `devtools::document()` where roxygen changed, and the regenerated `man/*.Rd`
   committed. Then run `Rscript notes/scripts/check_rd_links.R`, which exits
   non-zero and names any `\link{}` target that does not resolve.
   `devtools::test()` never builds the Rd files, so a broken link otherwise
   first appears as a warning on every CI platform. Check the generated Rd for
   `\\%` (`git grep '\\%' -- man/`); write a bare `%` in roxygen under markdown.
4. A `NEWS.md` entry under the development heading at the top of the file.
5. `DESCRIPTION` `Version` is bumped by one in its fourth component in an item
   that changes what a fit produces. A bug fix that changes no fitted result
   does not bump it (D18).
6. The test files the item touches pass under `NOT_CRAN=true` against an
   installed build, not only under `devtools::load_all()`, which exposes
   unexported functions that `R CMD check` will not see. Each file is run on
   its own with `TESTTHAT_PARALLEL=false`. The full suite is left to CI: run
   locally under `devtools::test()`, its parallel workers deadlocked on this
   machine on 2026-09-26, three suites idle for over an hour at load 0.3.
7. An independent review: a fresh session given the issue, the specification
   section and the diff, and asked for correctness defects only. Address what
   is real; up to three rounds.
8. The pull request body follows section 14 of the global `CLAUDE.md`: what,
   why and evidence above the fold, implementation detail in a `<details>`
   block. It names the issues the item covers. Run
   `python3 ~/.claude/hooks/prose-check.py` on it.
9. The status column of the run's queue is updated in the item's own last
   commit, linking the pull request.

## Records

Reasoning, evidence and what was left undone go in the pull request body or the
issue comment, where they stay attached to the change. `05_run_log.md` records
only what GitHub cannot: an item stopped or skipped, and why. The prompt log for
the run goes in `prompts/` of the main checkout, never of the worktree.

## Hard constraints

- No new package dependency without a decision recorded in `03_decisions.md`.
  Stop the item and say so in the run log.
- Do not run `vignettes/precompile.R`, and do not regenerate any `*.Rmd` from its
  `*.Rmd.orig`. Edit the `.Rmd.orig` where an item requires it, and say in the
  pull request body which rendered output is now stale.
- Do not edit `CLAUDE.md`, `.github/`, or any settings file.
- Base R inside `R/`; tidyverse is acceptable in tests and scripts. Native pipe
  `|>` only. `snake_case`. There is no `air.toml`, so do not run a formatter.
- Code comments follow section 6 of the global `CLAUDE.md`: a short comment
  where a reader could make a plausible wrong change, and the reasoning in the
  pull request body.
- Pass `seed =` to `bnec()` for a reproducible fit; `set.seed()` does not reach
  the sampler.
- Any raw `brms` code written to compare against a `bnec()` fit names the link
  explicitly (`bayesnec/CLAUDE.md`, the identity link).

## Stop conditions

Stop the item, record why in `05_run_log.md`, and take the next item that does
not depend on it, where any of these holds:

- a decision the specification marks as open has not been recorded in
  `03_decisions.md`;
- the fix needs a user-visible behaviour change the specification does not
  sanction;
- the fix needs deciding what the correct statistical behaviour is, rather than
  implementing what the specification states;
- it needs a new dependency;
- a check fails and the change is not the cause, or the same check also fails on
  the tip of `predev`;
- the issue turns out to be already fixed on `predev`, or a duplicate;
- about ninety minutes pass with no progress.

Do not open a pull request that guesses at a modelling decision. A clear account
of the blocker is worth more than a speculative implementation.

## Submissions to the HPC

A run that precompiles vignettes on the AIMS HPC needs RF's VPN connection both
to submit and to collect the output. The run's plan states the deadline, and
`hpc/README.md` gives the commands. A submitted job keeps running after the
connection closes.

## Verification

```r
devtools::load_all(".")
Sys.setenv(NOT_CRAN = "true")
testthat::test_file("tests/testthat/test-<file>.R")
```

Without `NOT_CRAN=true` a local run skips nearly every fitting assertion and
reports no failures. Prefer the packaged `manec_example` and `nec_data`, and
structural fixtures, to new fits. Where a fixture is fitted, reuse it across
tests: compiling Stan programs is what makes the suite slow.
