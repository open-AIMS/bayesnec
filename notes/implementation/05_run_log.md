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

## Item 4 — #207, dispersion priors and incomplete prior sets

Branch `issue-207-dispersion-priors`, cut from `issue-210-define-prior-zeros`.
Tier **2.1.4**. Version 2.1.3.10 -> 2.1.3.11.
**PR: https://github.com/open-AIMS/bayesnec/pull/223**.

Both parts done. Full suite **1526 pass / 0 fail / 11 warn**, +22 on the branch
below.

**A correction to the issue's premise, recorded on the issue itself.** RF read
part 2 as saying `disp()` loses automatic prior building. It does not — checked
on `dev`, `get_priors()` on a `disp("power")` formula returns proper `c0` and
`c1` priors. The loss happens only when the user supplies an *incomplete* set,
which `validate_priors()` accepted wholesale. RF's stated principle then decides
the open question directly: warn and fill from bayesnec defaults, not error.

**Flagged for review: this changes results.** A fit that previously ran on flat
priors for unmentioned parameters now runs on bayesnec priors. Own NEWS
paragraph.

**Process slip worth recording.** A `git add -A` swept the whole #207 change set
into a commit whose message was about the run notes. Caught on push (`nothing to
commit, working tree clean` with the wrong commit at HEAD), split with
`reset --soft` and force-pushed with `--force-with-lease`. Stage deliberately
when two unrelated changes are in the tree at once.

---

# 2.1.4 TIER COMPLETE

#216 (merged), #215 (PR 220), #139 (PR 221), #210 (PR 222), #207 (PR 223).
All green at the time of writing except 223, which is still in CI.

Next: the 2.2.0 tier. The first PR of it opens the `# bayesnec 2.2.0` heading in
NEWS.md.

## Item 5 — #136, rate() aterm for poisson and negbinomial

Branch `issue-136-rate-aterm`, cut from `issue-207-dispersion-priors`.
Tier **2.2.0** — the first of that tier, so it opens the `# bayesnec 2.2.0`
heading in NEWS.md. Version 2.1.3.11 -> 2.1.3.12.
**PR: https://github.com/open-AIMS/bayesnec/pull/224**.

Full suite **1546 pass / 0 fail / 11 warn**, +20 on the branch below.

Verified end to end on the issue's reprex: `top` prior mean 61 -> **19.8**
against a true 20; the fit that previously errored in post-processing now
returns NEC 3.893 [3.634, 4.125] against a true 4; `bnec_newdata()` resolves
with the denominator pinned at 1; `ecx()` returns 4.071 [3.823, 4.289].

**One worry in the scoping comment turned out to be moot.** It flagged
`dispersion()` as needing care because negbinomial scales the shape by the
denominator. `dispersion()`'s `allowed_fams` is `c("poisson", "binomial")` —
negbinomial was never supported, so there is nothing to get wrong today. The
Poisson case is the exact `mu * denom` analogue of the binomial branch and is
implemented, with a comment warning whoever widens `allowed_fams`. Checked
`brms:::posterior_epred_poisson` rather than assuming: it calls
`multiply_dpar_rate_denom`, so it returns expected counts.

**The breaking change bit two existing tests**, exactly as the queue entry
warned. `test-make_brmsformula.R` carried `se(sei)` in two multi-aterm chains
(incidental — the chains still exercise parsing with trials + weights + cens)
and `test-cens.R` asserted the old message. Both updated. Vignettes and the JSS
article swept for non-validated aterms: none.

**Two bugs of mine caught by the new tests, worth remembering:**

1. `[["rate_var"]]` on a named character vector is a subscript **error** when
   the name is absent, where `[[` on a list gives NULL. That broke every
   existing fit through `prediction_grid()`. The existing `trials_var` lookups
   are safe only because a family branch guards them. Use single-bracket lookup
   plus `is.na()` for any optional `bnec_pop` entry.
2. An assertion compared an integer response against `as.numeric()`.

## Item 6 — #209, hurdle_poisson and hurdle_negbinomial

Branch `issue-209-hurdle-counts`, cut from `issue-136-rate-aterm`.
Tier **2.2.0**. Version 2.1.3.12 -> 2.1.3.13.
**PR: https://github.com/open-AIMS/bayesnec/pull/225**.

Full suite **1570 pass / 0 fail / 11 warn**, +24 on the branch below. Verified
on a real `hurdle_poisson` fit: NEC 3.049 [2.536, 3.607] against a true 3, both
parameter blocks present, `ecx()` working on each via `dpar`.

**The issue's two halves needed opposite treatments.** The joint families need
no truncation work — brms writes the zero-truncated positive part itself. The
`bnec_hurdle()` untruncated-likelihood defect cannot be fixed the same way: it
would need a `trunc()` aterm, and #136 (directly below in the stack) had just
made unvalidated aterms an error. Validating `trunc()` publicly is well beyond
what #209 sanctions, so `bnec_hurdle()` refuses count growth families and points
at the path that is correct by construction. **Flagged for RF as a judgement
call.**

**Two behaviour changes, both deliberate**, and one of them reverses a #104
test that asserted a plain poisson growth family IS accepted. That was correct
when written (no zero-truncated count family existed) and its own neighbouring
comment names #209 as the fix. The test now asserts the refusal with the
reasoning written into it.

**A third registry the issue does not mention:** `mod_fams` in
`data-raw/sysdata.R` is the allow-list `validate_family()` checks, separate from
`hurdle_fams` and `hurdle_mu_fams`. Without it the families are rejected as "not
currently implemented".

---

# 2.2.0 TIER: 2 of 3 done

#136 (PR 224), #209 (PR 225). Remaining: #148, the largest item in the tier.

## Item 7 — #148, check_fit() and pp_check()

Branch `issue-148-check-fit`, cut from `issue-209-hurdle-counts`.
Tier **2.2.0**. Version 2.1.3.13 -> 2.1.3.14.
**PR: https://github.com/open-AIMS/bayesnec/pull/226**. Closes #148 and #56.

Full suite **1608 pass / 0 fail / 11 warn**, +38 on the branch below.
---

## Item 7 — #148 parts A/B/C, decisions (b) and (d): `check_fit()` and `pp_check()`

Branch `issue-148-check-fit`, **restacked off the stalled 2.2.0 tier and rebased
straight onto `dev`**. Version 2.1.3.19 -> 2.1.3.20.
**PR: https://github.com/open-AIMS/bayesnec/pull/226**. Closes #148 (with #240,
which carried Part D) and #56.

**Why it left the stack.** It was cut from `issue-209-hurdle-counts` (PR #225),
which is stalled on a `brms` bug, behind `issue-136-rate-aterm` (PR #224). Both
are `DIRTY`. Nothing in this branch depends on either: `bnec_hurdle()`,
`is_hurdle_family()`, `hurdle_component_preds()` and `bayesnechurdlefit` are all
already on `dev`, and the hurdle test fixture is a continuous gamma hurdle, not
one of the count families #225 adds. Rebasing rather than merging was required —
a merge would have dragged all of #224 and #225 into `dev`.

**Resolutions the restack needed**, both against #240, which touched the same
warning block from the other side:

1. `R/print.R` — kept this branch's single two-axis block (D5: the sampler
   question and the fit question feed the same drop-or-keep decision), grafted
   #240's two guards onto it. `which()` rather than logical indexing on *both*
   axes, so a `manecsummary` stored by an older version carrying an `NA` cannot
   print `- NA` as a failed model; and the `rhat_cutoff` fallback stays **1.05**,
   not 1.01, because an object stored before that field existed was assessed
   against the old 1.05 grep. `fit_ratio_cutoff` gets the same treatment for the
   same reason.
2. `R/summary.R` — `chk_number`, not `chk_numeric`, for both cutoffs: the
   documented type is a vector of length 1 and `chk_numeric` admits any length.

**NEWS placement changed.** The entries go under `# bayesnec 2.1.4`, not the
`# bayesnec 2.2.0` heading this branch used to open. Part D of the same issue
already shipped into 2.1.4 via #240, and `dev` is still at 2.1.3.x working
towards 2.1.4; opening a 2.2.0 section for the other half of an issue whose
first half is in 2.1.4 would split one issue across two releases. A one-line
move if the tier is restored.

**`fit_ratio_cutoff = 1.15` confirmed by RF** on 2026-08-24. It was flagged on
the PR as a judgement the spec does not settle — the spec fixes only that the
threshold is on the ratio and not the `ppp`. Now the documented default.

**Reproduces the finding the issue rests on**: on `manec_example` the control
group's `sd_ratio` is 0.796 -- the model simulates ~26% more variability than
the data show -- while the global `dispersion()` statistic reads 1.011
[0.71, 1.44]. The scoping comment measured ~27% by hand. Pinned as a test,
because it is the one assertion that would catch an implementation that looks
right but is not actually local.

All of RF's decisions implemented: part B in scope with control lack-of-fit;
ggplot2 only, no bayesplot; replication preferred with warned binning fallback;
#56 folded in via LOO-PIT, no DHARMa; both a numeric table and (via pp_check)
plots; mixture families need nothing special.

**Decision (d), the combined hurdle check, is deliberately NOT implemented** --
the scoping comment left it open and said not to let it hold up the rest.
Flagged in the PR.

**Two implementation bugs found by running against manec_example rather than
by reasoning:**

1. New S3 generics need `devtools::document()` before `load_all()` can dispatch
   them. This is the first stack item to add one, so the first where document()
   is load-bearing for the code to run at all rather than just for man/.
2. `ndraws` had to be clamped to what the fit holds. brms **errors** rather than
   truncating, and manec_example carries 100 draws, so the default of 1000 would
   have failed on the package's own example object.

---

# 2.2.0 TIER COMPLETE

#136 (PR 224), #209 (PR 225), #148 (PR 226).

Seven PRs open: #220 #221 #222 #223 (2.1.4) and #224 #225 #226 (2.2.0).
Remaining: items 8 and 9, the 2.3.0 tier -- #33 stage 1 and the #6/#33 vignette.
[0.71, 1.44]. Pinned as a test, because it is the one assertion that would catch
an implementation that looks right but is not actually local.

**Two implementation bugs found by running rather than by reasoning:**

1. New S3 generics need `devtools::document()` before `load_all()` can dispatch
   them.
2. `ndraws` had to be clamped to what the fit holds. `brms` **errors** rather
   than truncating, and `manec_example` carries 100 draws, so the default of
   1000 would have failed on the package's own example object.

**One test broke on the restack, and it was the resolution's fault, not the
feature's.** `test-bayesmanec_methods.R` "print.manecsummary falls back to 1.05
for an object with no cutoff" is #240's test and asserts the message text
`"Rhats > 1.05"`. Decision (b) folds the Rhat warning into the two-axis block
and restates the line as `"Rhat > ..."`, so the assertion missed. The 1.05
fallback it guards is intact and still asserted; only the regexp moved. The
other two #240 guards in that file -- the `NA` index test and the
`screen_models` pointer -- passed unchanged, which is what confirmed the graft
held. Worth recording because a branch cut before a test exists cannot fail it
until the two meet, and this is the second time that shape of thing has bitten
this stack.

---

# MERGE RECORD — 2026-08-25

**The PR bodies are the record.** This table exists only so a session knows
which PR to read; nothing here restates one.

| merged | PR | issues closed |
|---|---|---|
| 08-24 | #240, #241, #242, #246 | #244 |
| 08-24 | #226 | **#148** (with #240) — closed by hand 08-25 |
| 08-25 | #252 | #251 |
| 08-25 | #224 | #136, #247 |
| 08-25 | #227 | — (#33 **stage 1**; #33 left open for stage 2) |

Release tiers were revised in the #227 pass: `2.1.4` is everything through the
feature work, **`2.2.0` is the factor covariate release**, and the short-lived
`2.3.0` tier is gone.
