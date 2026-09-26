# Autonomous implementation protocol

How a Claude Code session works through `01_work_queue.md` without supervision.
Read this first, then the queue, then the specification the queue names, then
`03_decisions.md`.

Revised 2026-09-26 for the backlog run (`notes/tasks/backlog-run-human.md`).
The run of 2026-08-21 stacked its branches and left every merge to RF; this run
merges each pull request into `predev` in order and RF reviews `predev` as a
whole. The earlier procedure is in the history of this file.

## Branches and merging

`predev` is the integration branch for this run. Every pull request targets it,
and `predev` reaches `dev` later as one pull request that RF reviews. `master` is
never touched.

One item, one branch, one pull request, each cut from the tip of `predev`.
Branches do not stack. After an item merges, the next branch is cut from the new
tip:

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

Issues stay open. A pull request into `predev` does not close anything, and RF
has not reviewed the change. After merging, comment on each issue the item
covers: which pull request implements it, and that the issue closes when
`predev` reaches `dev`.

Notes-only changes go straight to `predev`, with no pull request. A commit that
touches `notes/**` and nothing else is not read by `R CMD check`, so a pull
request for it spends a four-platform matrix on nothing. A commit that touches
one line of `R/`, `tests/`, `man/`, `DESCRIPTION`, `NAMESPACE`, `vignettes/` or
`.github/` is not a notes change.

## Worktree and machine

One worktree for the whole run, at `/mnt/c/Rworking/bayesnec-predev-run`. Do not
work in `/mnt/c/Rworking/bayesnec`: it is RF's main checkout, and the Windows and
WSL paths are the same directory, so two sessions there overwrite each other.

```bash
git -C /mnt/c/Rworking/bayesnec fetch origin predev
git -C /mnt/c/Rworking/bayesnec worktree add --detach \
    /mnt/c/Rworking/bayesnec-predev-run origin/predev
```

Never touch a worktree this run did not create, and never touch the branch of
PR #402 (`issue-382-example8-group-terms`), which another session is repairing.

The machine has 22 cores. Check `uptime` before any fitting and take no more
than half of the idle cores. Run each Stan fit as a separate `Rscript` process:
a fit inside a `future` `multisession` worker deadlocks on this machine. Kill
processes by PID, never by a `pkill` pattern, which has killed the invoking shell
in this project.

## Definition of done, per item

1. The behaviour in the specification is implemented, and nothing beyond it.
2. `testthat` tests covering the main behaviour and at least one edge case, in
   `tests/testthat/test-<file>.R` following the existing names.
3. `devtools::document()` where roxygen changed, and the regenerated `man/*.Rd`
   committed. Check the generated Rd for `\\%` (`git grep '\\%' -- man/`); write
   a bare `%` in roxygen under markdown.
4. A `NEWS.md` entry under `# bayesnec 2.2.0`.
5. `DESCRIPTION` `Version` is bumped by one in its fourth component only where
   the specification says so: once in item 1, because the #386 programme did not
   bump it and a build before PR #409 cannot otherwise be told from one after it
   (#413), and in any item that changes what a fit produces. A bug fix that
   changes no fitted result does not bump it (RF, 2026-09-26, D18).
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
   block. It names the issues the item covers and says the merge does not close
   them. Run `python3 ~/.claude/hooks/prose-check.py` on it.
9. The status column of `01_work_queue.md` is updated in the item's own last
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
- Comment decision points in code: why this approach over the plausible
  alternative.
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

## Items that need PR #402

Items 13 to 16 change code that PR #402 rewrites or edits, so they are done after
it has merged into `predev`. If it has not merged when item 12 is finished, do
them on `predev` anyway (D18) and post a `[claude]` comment on PR #402 naming the
changes it must take in when it is next brought up to date with `predev`. Never
rebase, push to or edit the branch of PR #402.

## The deadline

The run ends by submitting a precompile of the vignettes to the AIMS HPC, which
needs RF's VPN connection. That connection closes near 18:00 AEST on 2026-09-27.
The times at which the store refit and the precompile are submitted, and what is
done when they are missed, are in `notes/tasks/backlog-run-claude.md` §4, under
the precompile and the store refit. Check the clock at each item boundary.

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
