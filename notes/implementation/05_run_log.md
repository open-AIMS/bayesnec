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

## Item 3 — #210, define_prior collapse on many zeros

Branch `issue-210-define-prior-zeros`, cut from `issue-139-drc-nec-equivalence`.
Tier **2.1.4**. Version 2.1.3.9 -> 2.1.3.10.
**PR: https://github.com/open-AIMS/bayesnec/pull/222**.

All three failures from the issue verified fixed on its own reprex. `bot` rate
at 33% zeros 66.7 -> 0.199 (prior mean 0.03 -> 10.0 against a true `bot` of 5);
`regularizing` no longer pinned; `gamma(2, Inf)` -> `gamma(2, 0.079)` past 75%
zeros. A no-zeros response is provably untouched.

Full suite **1504 pass / 0 fail / 11 warn**, +34 on the branch below.

**Two deviations from the queue's "R/define_prior.R only" scope, both flagged in
the PR rather than folded in silently.**

1. `R/helpers.R` — `response_link_scale()` computed `min_z_val` eagerly, so an
   all-zero response warned about a value it never uses. Made lazy.
2. `tests/testthat/test-failed_models.R` — de-flaked, committed on the #139
   branch. See below.

## BLOCKER RESOLVED — a flaky test at the base of the stack

PR 221's CI went red on `test-failed_models.R:193`, a #133 test unrelated to
either #139 or #210. It forces a failure with `timeout = 1e-3` and then asserts
the recorded message matches `"time limit"`; CI recorded *"Expecting a single
value when fixing parameter 'ec50'"* from the initial-value search instead.

Where a 1 ms interrupt lands is a race, so the message was never a stable thing
to assert. Confirmed not caused by #139 by arithmetic: PR 220 ran 1449 pass /
0 fail on the identical runner, PR 221 ran 1461 / 1, and 1449 + 13 new = 1462 =
1461 + 1. It also passed locally at 1470/0 on the same commit.

**Fixed on the #139 branch, not #210**, because branches stack: a fix higher up
would have left #221 red and every later PR inheriting the flake. `issue-210`
was rebased onto the corrected `issue-139`. The test now asserts what it is
named for — the failure was captured and reported with its priors and inits —
instead of the incidental wording.

**Worth watching for the rest of the stack:** this suite has at least one
timing-dependent test, and CI runners are slower and more variable than this
machine. A red check on a test unrelated to the item in hand should be checked
against the pass/fail arithmetic before it is assumed to be the item's fault.
