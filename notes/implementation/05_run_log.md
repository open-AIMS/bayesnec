# Stack run log

Appended as the run proceeds. RF reads this first on Monday.

Worktree: `/mnt/c/Rworking/bayesnec-stack`. Branches stack — each cut from the
previous, each PR targeting the previous. Machine budget: 2 cores, `chains = 2`,
one R process at a time, while `negative-sgr/analysis/phase10_run.R` holds 16 of
22 cores.

---

## Item 0 — close #79, #212, #166

- **#212 — CLOSED** (not planned). RF's call; reasoning posted on the issue.
- **#166 — CLOSED** (not planned) as a duplicate of toxval#29.
- **#79 — verification running.** Short-chain run on `dev` already showed it no
  longer reproduces; re-running at the issue's own default `iter` before closing,
  at `chains = 2, cores = 1` per the machine budget. Result recorded below when
  it lands.

## Item 1 — #215, publish `dev` vignettes from CI

Branch `issue-215-dev-vignette-ci`. Tier **2.1.4**. Version 2.1.3.7 -> 2.1.3.8.

**Approach.** Used pkgdown's own `development: mode: auto` rather than
hand-rolling a second deploy target. It keys off the version in `DESCRIPTION`,
which is already how this repo separates the two lines — `master` carries a
release version and `dev` a four-component development version — so a build from
`master` lands at the site root and a build from `dev` at `/dev/`, with no branch
name hard-coded and no second config to keep in sync. pkgdown adds its own
development banner, which is the part of #215 that matters: the two sites must
not be mistakable for each other.

**Deviation from the issue.** It asked for `articles/dev/`. pkgdown generates a
whole site, not a bare articles directory, so the natural equivalent is
`/dev/articles/`. Same guarantee, idiomatic layout.

**Also added** `.github/workflows/precompile-vignettes.yaml`, manually
dispatched. `precompile.R` fits every model in every vignette, so it cannot run
on push; on demand it regenerates the `.Rmd` and figures and **opens a PR**
rather than pushing to the branch, because precompilation changes every number
in every vignette and that is a diff someone should look at. It takes a
`vignettes` input so a single vignette can be rebuilt without paying for all
seven. This is also the mechanism #190 needs.

**Not verified, and RF should know.** The `pkgdown` change cannot be fully
verified before merge — the deploy step is skipped on pull requests, so CI will
build the site but not publish it. The first real test is the push to `dev` after
merge. The precompile workflow has not been run at all; it would take hours and
the machine is busy. Both are flagged in the PR body.


**PR: https://github.com/open-AIMS/bayesnec/pull/220** (base `dev`).
