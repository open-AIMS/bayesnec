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
- **#79 — CLOSED** (completed) as not reproducible. Re-ran the issue's reprex on
  `dev` at the `bnec()` default `iter`, `chains = 2, cores = 1`. It **fitted**,
  returning a `bayesnecfit` with *NEC* 4.065 [3.245, 4.547] against a simulated
  truth of 4 — so the initial-value search is not merely getting past the guard,
  it is landing somewhere that identifies the model. Evidence posted on the
  issue.

  Kept on the record there: the fit took **1050 s** on ten data points, far
  beyond compilation, consistent with `make_good_inits()` retrying many times
  before succeeding. The symptom is gone; the cause may only be softened. If
  slow starts on low-probability binomial data are reported later, that is a
  performance issue rather than this failure.

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

## Item 2 — #139, document the drc NEC equivalence

Branch `issue-139-drc-nec-equivalence`, cut from `issue-215-dev-vignette-ci`.
Tier **2.1.4**. Version 2.1.3.8 -> 2.1.3.9.
**PR: https://github.com/open-AIMS/bayesnec/pull/221** (base
`issue-215-dev-vignette-ci`).

Full suite: **1470 pass, 0 fail, 11 warn**, against a `dev` baseline of 1457
pass / 11 warn — exactly the 13 new assertions, warnings unchanged.

Documentation only, per RF's choice of option A. Equivalence table added to
`?models` and to `vignettes/example2b.Rmd.orig`, with the `NEC.2()` omission
explained and pointed at #84.

**Two corrections to the scoping comment, both found by testing its claims.**

1. The comment reports the difference between `drc`'s NEC and ours as "exactly
   `0`". That holds at the one parameter set it tested, but over a grid varying
   every parameter I got **7.1e-15** — traced to the *test* round-tripping `b`
   through `exp(log(b))`, not to any difference between the models. Compared the
   way the reparameterisation actually runs, with `b = exp(beta)`, it **is**
   bit-exact. Both the documentation and the tests now say the precise thing,
   and there is a test pinning the round-trip error at ~1e-15 so a `drc` user
   who converts via `log(b)` and sees a discrepancy knows what it is.
2. `check_normalisation()` is **not exported**, so the `\link{}` the first draft
   used would have been a broken cross-reference in `R CMD check`. Replaced with
   plain text. Worth remembering for the rest of the stack: the internal check
   functions are not all exported.

Tests in a new `tests/testthat/test-pred_equations.R`, asserting the equivalence
against `drc`'s generator transcribed inline rather than taking a dependency to
check a documentation claim. 13 pass, 0 fail, 0 warn on that file.

## Note on pacing

Full `devtools::test()` runs take ~27 min and counting under `/mnt/c` — the
Windows-mounted filesystem is markedly slower than the Linux one. At roughly an
hour per item this dominates the wall clock for the whole stack. Not changing
anything about it, but it is why items land at the rate they do.
