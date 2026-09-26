# Plan for the open issues after PR #409

Companion specification: `backlog-run-claude.md`. Every decision below is
stated in full there, with the evidence and the alternatives.

Written 2026-09-26, after PR #409 brought the incomplete-design programme (#386)
into `dev`, and revised the same day after RF's review.

---

## Summary

This plan covers every open issue except #255, the toxval migration, and #27,
which is being written up for a later decision. It includes #419, filed during
the review of this plan. It sets the order in which the
issues are fixed, and how a Claude Code session fixes them without supervision.
That unattended session working through the list is called the run below.

Each fix is a pull request into `predev`, merged by the run once its checks
pass. RF then reviews `predev` as a whole, and it reaches `dev` as one pull
request, as the #386 programme did. That merge is expected to become the 3.0
release.

The run ends by launching a precompile of the vignettes on the AIMS HPC. RF's
VPN connection closes about 22 hours after the plan was approved, and a job can
only be submitted while it is open, so the launch has a deadline and the order
of work puts first the fixes that change what a vignette prints.

Three issues found in the review of PR #409 are defects that PR #409 itself
introduced: #415, #416 and #417. That code is already on `dev`, so these three go
first.

Several items concern censored estimates. An estimate is censored where it lies
beyond the range of concentrations tested, so the data give only a bound on it:
"above 10" rather than a value. PR #409 made every reported estimate state when
it is censored. Some functions that take those estimates as input still discard
that statement.

Two comments were posted on each issue on 2026-09-26: the findings from
checking it against current code, which in several cases go further than the
issue body, and RF's decision where one was needed.

---

## The procedure for each item

A session works through the list in order. For each item it:

1. branches from the current tip of `predev`;
2. writes the fix and its tests, and runs them against an installed build;
3. has the change reviewed by a second, independent session, and addresses what
   it finds;
4. pushes once, opens a pull request into `predev`, and waits for the checks;
5. merges it when every check has passed, and comments on the issue.

The next item then branches from the new tip of `predev`. No branch is built on
another unmerged branch. A stalled item therefore holds up only the items that
depend on it. While one pull request waits on its checks, which take 40 to 60
minutes, the run writes the next item and brings it up to date with `predev`
before pushing.

Issues stay open until `predev` reaches `dev`. A pull request into `predev` does
not close an issue, and RF has not yet reviewed the change. The run comments on
each issue with the pull request that implements it.

The run stops an item, and records why, where any of the following holds:

- a check fails and the change is not the cause;
- a decision in this plan is still open;
- the fix turns out to need a behaviour change this plan does not sanction;
- it would need a new package dependency.

It then takes the next item that does not depend on the stopped one. It never
merges a pull request with a failing check.

`notes/implementation/00_protocol.md` gives the procedure in full.
`notes/implementation/01_work_queue.md` holds the list, with a status column that
links each item to its pull request.

---

## The order of work

### Items that do not need PR #402

PR #402 adds a joint refit across factor levels and is being repaired by another
session. Twelve of the sixteen items change nothing it touches, so they go ahead
whatever happens to it. They are ordered so that the fixes that change a
vignette's printed output come before the precompile, and within that the
defects PR #409 introduced come first.

| # | issue | what it does for a user | vignette output it changes |
|---|---|---|---|
| 1 | #416 | `summary(fit, ecx = TRUE, x_range = c(0, 40))` on a hurdle fit works instead of stopping with an argument error | none |
| 2 | #417 | a bound drawn on a plot under a decreasing transformation, such as `xform = function(x) -x`, reads `<=` instead of the incorrect `>=` | none |
| 3 | #415 | `nec()` on a hurdle fit no longer reports an exact value where the combined threshold is only known to exceed a limit | `example6` |
| 4 | #299 | a call such as `nec(fit)` on a fit of `crf(log(conc))` states that the result is on the log scale and what `xform` to pass | `example1`, `example3`, `example8`, `example9` |
| 5 | #120 | `autoplot(fit, model = "nec4param")` in place of `all_models`, which keeps working with a warning for one release | `example2` |
| 6 | #404, #403 | `compare_posterior()` and `average_estimates()` report censored draws instead of silently dropping them or averaging a missing value in as 1 | `example4`, `example8` |
| 7 | #412 | `summary(fit, ecx = TRUE)` and `ecx(fit)` agree on a hurdle fit | `example6` |
| 8 | #398 | a column named with a space is refused with a message naming it, instead of a parse error | none |
| 9 | #400 | a response with every observation at 0 or 1 is refused by name, instead of failing inside prior construction | none |
| 10 | #397 | the dispersion sub-model of a `rate()` or `trials()` fit is centred on the scale of the mean | none; no vignette fits that combination |
| 11 | #44 | a new function, `exceedance()`, gives the probability that an estimate exceeds a threshold concentration | none |
| 12 | #410 | `bnec_hurdle(... + disp("power"))` models the dispersion of the growth block instead of failing after the growth fit has sampled | none |

### Items that need PR #402

Four items change code that PR #402 rewrites.

| # | issue | what it does for a user | vignette output it changes |
|---|---|---|---|
| 13 | #404 | `nec()` on a grouped or joint fit marks a censored level as the level's own fit does | `example8` |
| 14 | #413 | rendering `example8` against a fit store made by an older `bayesnec` stops with a message | `example8` |
| 15 | #410 | `bnec(family = "hurdle_gamma")` and `bnec_joint()` model the dispersion of the positive part, instead of refusing `disp()` | none |
| 16 | #419 | a constant equation, `ecxflat`, fits a response with no concentration effect and reports every estimate as above the tested range | none |

Item 13 changes the table code that PR #402 relocates into a new shared
function, which also serves the joint fits PR #402 adds. Done after PR #402, one
change covers both. Item 14 changes `vignettes/fit_store.R`, item 15
`R/bnec_joint.R`, and item 16 the prior and initial-value code, all of which
PR #402 rewrites or edits.

If PR #402 has not merged when item 12 is done, the run does items 13 to 16 on
`predev` anyway. PR #402 then has to take those changes in when it is next
brought up to date, and the run leaves a comment on it saying exactly what.
Waiting would hold four items back behind a pull request whose check failure is
not yet understood.

### In the background

#418 fits a small set of simulated incomplete designs under the code before and
after PR #409, and records what the fitted estimates show. It changes no package
code and depends on no pull request, so it starts at the beginning of the run.
About 24 fits, roughly fifteen minutes on this machine.

### The precompile at the end

The precompile re-renders every vignette on the HPC, which takes hours. The run
submits it and does not wait for it.

- At the start, one short vignette is precompiled as a test, so that a problem
  with the image or the cluster is found early rather than at the deadline.
- The full precompile is submitted when the list is finished, or at 14:00 AEST
  on 2026-09-27, whichever comes first, against the tip of `predev` at that
  moment. Fixes merged after it are listed in the run log as not reflected in
  the rendered vignettes.
- `example8` needs its fit store refitted first, because the store was fitted
  before PR #409 changed the priors. That refit runs on the HPC through the
  `grouping-structures` compendium, and is launched as soon as PR #402 has
  merged, since `example8`'s source is part of PR #402. If PR #402 has not merged
  in time for the refit to finish before the deadline, `example8` is left out of
  this precompile.
- Collecting the output needs the VPN. RF, or a later session, fetches it and
  opens one pull request with the rendered vignettes.

---

## Changes a user will see

Four changes alter a number a user already gets:

- `negbinomial` fits with a `rate()` term and a `disp()` term, and
  `beta_binomial` fits with a `disp()` term, fit a different model (item 10);
- the combined threshold of a hurdle fit is reported as a bound where it was
  wrongly reported as exact (item 3);
- comparisons and averages over censored estimates change value (item 6);
- the ECx estimates of a hurdle fit change where `summary()` and `ecx()` read
  them from different grids today (item 7).

Everything else is a new refusal, a new message, a corrected label, a new
function or a renamed argument. `NEWS.md` states each change under the 2.2.0
heading, which RF renames for the release.

The development version is bumped twice in the run. The first pull request
bumps it once, because the #386 programme did not: a `bayesnec` built before
PR #409 and one built after it report the same version, which is why nothing can
tell that the `example8` fit store predates the change (#413). After that, only a
pull request that changes what a fit produces bumps it, which in this list is
item 10.

---

## Decisions

RF settled every question on 2026-09-26. The rulings are recorded in
`notes/implementation/03_decisions.md` and posted on each issue.

| | issue | ruling |
|---|---|---|
| Q1 | #120, #44 | both in the run, each recorded on its toxval issue |
| Q2 | #27 | kept open; a separate investigation writes it up with fitted evidence |
| Q3 | #415 | the combined value is marked as exceeding the smaller component limit |
| Q4 | #403 | a missing component makes the averaged draw censored, reported as a bound |
| Q5 | #404 | censoring on a group table is a column |
| Q6 | #404 | a comparison reports a lower and an upper probability with the censored fractions |
| Q7 | #412 | growth estimates on growth's own range; survival and combined on the survival range |
| Q8 | #299 | a message now; the change of default is left to the 3.0 release |
| Q9 | #413 | a version check and documentation; the store is refitted on the HPC in this run |
| Q10 | #410 | the dispersion is modelled in both hurdle routes, on the positive part |
| Q11 | #400 | refused by name, including a single grouped level and `beta` at 1, until #419 fits such a response with `ecxflat` |
| Q12 | #44 | a new function, `exceedance()` |
| Q13 | #418 | one data seed and the gaussian family first |

Q7 was revised after RF first accepted it. The first recommendation read the
combined hurdle estimate on the range both components cover. The code reads it on
the survival range by design: growth is fitted to survivors alone, and the
combined endpoint needs the concentrations above the last at which anything
survived. The revised ruling keeps that, and stops a growth estimate being read
off a curve extrapolated past growth's own data.

Q10 extends #410. The refusal of `disp()` on the joint hurdle families recorded a
choice not yet made, which was which block a variance function applies to. The
choice is the positive block, where the dispersion parameter is, so the joint
route now models it too (item 15).

Q11 led to #419, a constant equation RF proposed for a response with no
variation. RF settled its three questions on 2026-09-26:

- it is named `ecxflat`, since `?bnec` states that an equation without a step
  takes the `ecx` prefix, and two checks in `nec()` read that prefix;
- it is available by name in this run, and joins the `all`, `ecx` and `decline`
  sets at the 3.0 release. Where it takes appreciable model weight, a flat line
  predicts the data about as well as the declining curves, and the averaged ECx
  and NSEC are then reported as above the tested range, which is what such data
  support. `bayesnec` weights a set by pseudo-BMA unless a user asks for
  stacking;
- a response with no variation, fitted with any set, is fitted with `ecxflat`
  alone, with a message. This replaces the refusal of item 9 once item 16 lands.

---

## Defaults the run applies unless RF says otherwise

- No pull request is merged with a failing check.
- No vignette is re-rendered inside a pull request. The precompile at the end is
  the only render, and its output comes back as one pull request.
- The work found without an issue (specification §6) is filed as issues but not
  done in this run, apart from two documentation corrections included in item 6.

---

## Corrections to `CLAUDE.md` for RF

A session does not edit `CLAUDE.md` unless RF asks. At RF's request on
2026-09-26 it now records that model weights are pseudo-BMA unless a user asks
for stacking; it had said "stacking weight". Three further statements there are
contradicted by merged work and are left for RF:

- the section on `dispersion()` says it stops on a model-averaged fit. It has
  returned one result per equation since PR #377;
- the section on the identity link says `validate_family()` sets it for every
  family. Since #256 it does so only where no link is named;
- the section on branches does not mention `predev`.

---

## Not in this run

- #255, the toxval migration.
- #27, the truncated gaussian, being written up separately.
- #382 and #388, which PR #402 implements.
