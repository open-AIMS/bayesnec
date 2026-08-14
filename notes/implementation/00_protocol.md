# Autonomous implementation protocol

How a Claude Code session should work through `01_work_queue.md` without
supervision. Read this first, then the queue.

## Working rules

**One issue, one branch, one PR.** Branch from `dev`, never from another issue
branch. Name it `issue-<n>-<slug>`. PR targets `dev`. **Do not merge.** The
author reviews and merges.

**Separate worktrees.** If more than one session runs at once, each must use its
own `git worktree`. The Windows path `C:/Rworking/...` and the WSL path
`/home/rfisher/Rworking_wsl/...` are the *same checkout* — two sessions in the
same directory will corrupt each other's work.

```bash
git -C /mnt/c/Rworking/bayesnec worktree add ../bayesnec-issue-<n> -b issue-<n>-<slug> dev
```

**Never touch these.** `master`. Any branch or worktree you did not create.
`/mnt/c/Rworking/bayesnec-dispersion`, `-negsgr`, `-issue173`, `-betaub`,
`-cens`, `-hurdlegamma` — all belong to other work.

## Definition of done, per issue

1. The behaviour described in the issue is fixed or implemented.
2. `testthat` tests covering the main behaviour **and at least one edge case**,
   in `tests/testthat/test-<file>.R` matching the existing naming.
3. `devtools::document()` if roxygen changed, and the resulting `man/*.Rd`
   committed.
4. A `NEWS.md` entry under the current development heading.
5. `devtools::test()` passes locally. Push and let GitHub Actions run R CMD
   check; **if checks fail, fix them before opening the PR for review**.
6. PR body: what changed, why, how it was verified, and anything left undone.

## Hard constraints

- **No new package dependencies.** If an issue appears to need one, stop that
  issue, leave a note in the PR body, and move to the next. Do not add to
  `DESCRIPTION` `Imports`/`Suggests`.
- **Do not run `vignettes/precompile.R`,** and do not regenerate any `*.Rmd`
  from `*.Rmd.orig`. Vignette rebuilds take hours and are handled separately
  under #190. If a change alters vignette output, say so in the PR body.
- **Do not edit** `CLAUDE.md`, `.github/`, or any settings/config file.
- **Base R inside `R/`** to minimise dependencies; tidyverse is fine in tests
  and scripts. Native pipe `|>` only. `snake_case`. No `air.toml` in this repo,
  so **do not** run a formatter.
- **Comment decision points** — why this approach over the plausible
  alternative — not what the code obviously does.
- `set.seed()` is **not** sufficient for reproducible fits. Pass `seed =` to
  `bnec()`/`brm()`; it reaches Stan's sampler, `set.seed()` does not.
- Requires `brms >= 2.23.0`. Earlier versions mis-generate `beta_binomial`.

## When to stop rather than guess

Stop the current issue, write what you found in the PR body (or a
`notes/blocked_<n>.md` if there is nothing to push), and move to the next issue
if any of these occur:

- the fix requires a **user-visible behaviour change** not sanctioned in the
  queue entry;
- it requires a **new dependency**;
- it requires **deciding what the correct statistical behaviour is** — as
  opposed to implementing behaviour the queue entry already specifies;
- the issue turns out to be a **duplicate**, or already fixed on `dev`;
- more than roughly **90 minutes** of wall clock goes by with no progress.

Do not open a PR that guesses at a modelling decision. A clear write-up of the
blocker is worth more than a speculative implementation.

## Scope boundaries

**Anything touching `ecx()`, `nsec()`, `ecnsec()` or `zero_crossings()` is out
of scope.** That code is migrating to
[open-AIMS/toxval](https://github.com/open-AIMS/toxval), which will become a
dependency of `bayesnec`. Issues #39, #44, #166, #195 and #196 belong there and
must not be worked here. If a queued issue turns out to require changing those
functions, stop and report it — that is a finding, and it changes the migration
plan.

**#193 (example7 rewrite) is being done by another session.** Do not touch
`vignettes/example7*`.

## Verification you can rely on

```r
devtools::load_all(".")
devtools::test()                       # full suite
testthat::test_file("tests/testthat/test-<file>.R")
```

Fitting anything real is slow. Prefer `manec_example` / `nec_data` and the
smallest `chains`/`iter` that exercises the code path. Long fits are rarely
needed to demonstrate a post-processing fix.
