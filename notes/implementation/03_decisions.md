# Settled decisions

Answers given by the author on 2026-08-14, before the unattended run. A session
must **implement these as written** rather than re-open them. Where an entry
says "report", write the finding in the PR body and change nothing.

---

## D1 — Branching and review

Branch per issue from `dev`, named `issue-<n>-<slug>`. PR targets `dev`.
**Sessions do not merge.** PRs stack up for review.

## D2 — #180, the cached posterior matrix

**Drop the cache and compute on demand.** Remove `pred_vals$posterior`;
accessors recompute from the `brmsfit` via `posterior_epred()`. Expected
31.8 MB → roughly 1.5 MB on the measured example.

Accessors must **fall back gracefully for objects saved before the change** — an
old object still carrying the cache must keep working, and a new object without
it must not error. Test both.

## D3 — #170, `check_models()` versus `?models`

**Align the documentation to the code, and report only.** Add a test asserting
the two agree so they cannot drift again. **Do not relax the restriction**, even
if investigation suggests it is unnecessary — write that up as a recommendation
in the PR body instead.

## D4 — #104, zero-inflated counts

**Joint path only.** Add `zero_inflated_poisson` and
`zero_inflated_negbinomial` to the normal `bnec()` family path.
`bnec_hurdle()` must **refuse** them, with an error explaining why and pointing
at the hurdle families instead.

The reasoning matters and should appear in the documentation. #183 treated
`zero_inflated_beta` as identical to `hurdle_gamma`, and that was correct
*because neither Gamma nor Beta can emit a zero* — so zero-inflation collapses
to a hurdle and `brms` generates the hurdle form with no `log_sum_exp`. Poisson
and negbinomial **can** emit zeros, so the equivalence fails: a zero-inflated
count model is a genuine mixture. The existing two-block machinery does not
carry over, and pretending it does would give users a different model from the
one they asked for.

## D5 — #120, replacing `all_models`

**Two orthogonal arguments**: `model =` names one or more models, `average =`
(logical) controls whether the model-averaged outcome is shown. Applies to
`predict`, `plot` and `autoplot` for `bayesmanecfit`.

**Deprecate `all_models` for one cycle**: it keeps working, warns once, and maps
onto the new arguments. Removal is a later release. No existing user script
should break on upgrade.

## D6 — #148, diagnostics

**Split it, on this principle:** sensitivity to the *estimator's* own choices
belongs to toxval; sensitivity to what the *fit* supplies belongs here.

- **toxval** — how *NSEC* responds to `sig_val`, to the reference definition and
  to resolution. Not Bayesian-specific: it applies equally to `nsec.drc` and
  `nsec.brmsfit`, which is the point of testing it there.
- **bayesnec** — diagnostics on whether the fit supports a stable control
  estimate: posterior spread at the control, prior sensitivity, and the
  dispersion assumption.

The dispersion half already has a tool. On the `example1` simulation, `nsec`
moved from 0.93 [0.43, 1.32] to 1.09 [0.50, 1.53] under `disp("power")` while
`ecx` tightened — the estimator was unchanged, the fit was not. `disp()` is
therefore a *NSEC*-stability diagnostic, and that is worth saying explicitly.

**#148 stays open** as the bayesnec half, rescoped. It is **not** in the
unattended queue — it needs a specific list of diagnostics first.

## D7 — Sessions

**One session, sequential**, working Tier 1 of `01_work_queue.md` top to bottom
in a single worktree.

## D10 — Sequencing against the toxval migration

**Step 3 is superseded by D11 (2026-09-06):** the estimator bug fixes are made in
`bayesnec` and the migration is deferred. The rest of this entry stands.

The queue is split so that **no Tier 1 item edits a file the migration moves.**
That was checked, not assumed: the migration deletes `R/ecx.R` and `R/nsec.R`
from `bayesnec` and relocates `predict.bayesnecfit` / `predict.bayesmanecfit`,
and #120, #93, #160 and #161 all touch that territory. They are Tier 2.

#180 was checked in the same pass and **is** clear: `pred_vals$posterior` is
written at `R/expand_classes.R:78` and read at exactly one place,
`R/helpers.R:105`, in the model-averaging path. `predict()` does not read it,
plotting uses the `pred_vals$data` summary, and toxval does not touch it.

The order overall:

1. **Tier 1**, unattended, `bayesnec` only.
2. **toxval Phase 1** — the dependency untangle. *Attended*: it spans two repos,
   needs a `Remotes:` entry in the interim, and carries the ordering constraint
   in D8.
3. **toxval bug fixes** — #195, #196, #166, #39, #44.
4. **Tier 2** in `bayesnec`, once the boundary is stable.
5. **#190**, the full precompile, last.

The bugs do not have to be fixed before `bayesnec` work proceeds. What has to
come first is the *structural* move, because it relocates files. That is the
whole reason for the split.

## D8 — Repository scope

**The unattended run is `bayesnec` only.** toxval is a separate session with a
separate queue, and it is *not* runnable concurrently:

`bayesnec` cannot declare `Imports: toxval` until a toxval carrying the new API
is installable, or bayesnec's R CMD check fails on a function that does not
exist yet. toxval's Phase 1 must land and be installable first; in the interim
bayesnec would need a `Remotes:` entry. That ordering is a hard constraint, not
a preference.

## D9 — #141, `get_priors()`

**Both entry points, one function.**

- given a **fit**, return the priors that fit actually used, including any the
  user overrode — so `bnec(..., prior = get_priors(fit))` reproduces the model;
- given a **formula and data**, return the priors `bayesnec` would generate,
  without fitting anything — so a user can inspect and edit them before their
  first run.

Return a `brmsprior` for a single model and a named list of `brmsprior` objects
for a model set, in both cases directly usable as the `prior =` argument.
Document that the two entry points answer different questions and can disagree
once a user has overridden a prior.

## D11 — The estimator fixes are made in `bayesnec`, and the migration is deferred

RF, 2026-09-06. **This supersedes D10's step 3.** `ecx()`, `nsec()` and
`ecnsec()` still migrate to `toxval`, and #255 remains the tracker, but the
migration is not run in this pass and it no longer defers a fix to an estimator.
#195, #196, #160, #161 and #206 are tier B of `01_work_queue.md`.

**What changed.** D10 was written when the migration was the next structural
piece of work. It has not started: toxval#39 is open with no commits against it,
and its own prerequisite toxval#45, CRAN readiness, is also open. Meanwhile a
training course is to be written against `dev` within days, so the estimators are
about to be taught from.

**Why fixing here is not work discarded.** The migration relocates files. A
corrected `R/ecx.R` relocates exactly as an uncorrected one does, and the
correction is then in toxval's history rather than being re-derived there. What
the deferral was avoiding is editing a file that is about to be deleted; what it
was producing instead is a known-wrong result shipped for as long as the
migration takes.

**The measurement that settled it.** `example1.Rmd.orig:253` fits
`resp ~ crf(log(raw_x + 1), model = "nec4param")` and `:109` calls `ecx()` on
that fit. #196's back-transform substitutes into the first argument slot of the
parsed call, so the inverse applied is `log(raw_x)` and the `+ 1` is discarded.
The package's introductory vignette therefore reports an ECx computed from a
back-transform that drops a term. `example1` is the only vignette using an inline
transformation with arithmetic; `example6` uses a pre-computed `log_dose` column.

**What D10 keeps.** Its ordering constraint on the structural move stands, and
D8's constraint that `bayesnec` cannot declare `Imports: toxval` before toxval is
installable is unchanged. Only the position of the bug fixes changes.

**The obligation this creates.** Each tier B fix is recorded on the matching
toxval issue as it lands — #196 to toxval#19, #195 to toxval#8 and toxval#12, #39
to toxval#40 — so the migration re-lands the corrected version rather than the
one toxval forked.

## D12 — All three vignettes are finalised, and #228 goes last

RF, 2026-09-06. **This supersedes the 2026-09-03 decision to keep the vignette
pull requests open until the software stabilised.** The training course teaches
from the vignettes, so they are a deliverable of this run rather than an
instrument of it.

The ordering is by what blocks each one, not by issue number:

1. **PR #243**, example7 — reported mergeable; what remains is verification.
2. **PR #238**, example9 — a full review is on the PR, unactioned. Two of its
   errors of fact expired when #260 merged and must be re-checked before the
   review is applied.
3. **PR #228**, the grouping vignette — blocked on a scientific question about
   family choice and on 2000/2000 divergences under `ogl()` for the binomial
   families.

**#228's blocker is raised as its own issue** rather than held on the PR, because
it is material to #250's claim that group-level terms work for bounded families
generally, and that claim outlives this vignette. **Take #257 first**: applying a
group-level deviation on a scale where the mean cannot leave its support is the
one candidate explanation not yet refuted, and it would produce this signature on
a binomial response near the ceiling.

## D13 — The precompile runs once, after the vignettes

RF, 2026-09-06. #190 and #248 are tier E, and they run after tier D rather than
before a CRAN submission. The trigger changed: the rendered vignettes have to be
current because the training course reads them, not because a submission is due.

One ordering constraint follows from D11. B1 changes the ECx values reported in
`example1`, so `example1` cannot be re-rendered before B1 lands or it is rendered
twice.

## D14 — The run is autonomous and stacked

RF, 2026-09-06. `00_protocol.md` stands unchanged: one worktree, one branch per
issue cut from the previous issue's branch, each PR targeting the branch below
it, and RF reviews and merges down the stack. Parallel worktrees were considered
and rejected for the reason already recorded — tier B's five items are all in
`R/ecx.R`, `R/nsec.R` and the plotting path, so a fan-out would produce mutually
conflicting pull requests.

## D15 — The ECx reference, and the `type` vocabulary in `bayesnec`

RF, 2026-09-06. Eight rulings, taken together because they are one definition.
They align `bayesnec` with toxval T8, T9 and T10, which are stated in full in
`/mnt/c/Rworking/toxval/REFACTOR-claude.md` §3.9 and §3.10 and summarised in
that repository's `notes/implementation/02_decisions.md`.

**The governing ruling.** *Any ECx value is measured relative to the control
predicted mean, taken as the predicted response at the lowest concentration in
the supplied predictor.* `bnec()` fits decreasing curves only, so the increasing
forms in toxval T9 do not arise here; hormesis remains in scope and is the case
the ruling decides.

**What the code does now, and why that is the defect.** `ecx_x_absolute()`
(`R/ecx.R:331`) uses `max(y_d)` as the reference and `ecx_x_relative()`
(`R/ecx.R:319`) uses `max(y_d)` as the top of the span. For a monotonic
decreasing curve the maximum of the predicted curve is the control, so the two
agree. For a hormetic curve the maximum is the peak at the *NEC*, so every
reported ECx is measured from the peak rather than from the control. That is the
behaviour `hormesis_def = "max"` describes, applied unconditionally and without
the `modify_posterior()` call that would have implemented the alternative.
`nsec()` is anchored on the control already (`R/nsec.R:155`,
`quantile(p_samples[, 1], sig_val)`) except for a live `hormesis_def == "max"`
branch at `:167`. The two estimators therefore disagree on hormetic curves, which
is the inconsistency the ruling removes.

| | ruling |
|---|---|
| 1 | **Four `type` values, matching toxval T9.** `absolute` (default), control → 0; `relative`, control → the equation's theoretical asymptote; `range`, control → the lowest response the curve predicts over the predictor range; `direct`, a supplied response value. `range` is what `relative` computes today |
| 2 | **The control is read at the lowest observed predictor value**, not at the lowest point of the prediction grid. Supplying `x_range` therefore does not change any reported estimate |
| 3 | **A target the curve never reaches within the predictor range returns `NA` with a warning.** Today the nearest grid point is returned, which for a curve that never declines to the target can report the control concentration itself as the ECx |
| 4 | **`hormesis_def` is removed** from `ecx()`, `nsec()` and `ecnsec()`, as in toxval T10. Its `ecx()` consumer is already commented out and it selects nothing once the control is the reference |
| 5 | **`ecnsec` follows toxval T8**: it inverts the `ecx` reference construction under the same `type` and defaults to `absolute`. One formula replaces the three that stand in `R/nsec.R:157`, `R/nsec.R:363` and `R/ecnsec.R:131` |
| 6 | **`relative` is refused where the bound is infinite** — an equation with no `bot` fitted with a family that has no lower bound. Error for a single fit; drop with a warning, name the equations and renormalise the weights for a model-averaged one |
| 7 | **`absolute` uses 0 on an unbounded family deliberately**, following OECD TG 201, so the `gaussian`-without-`bot` refusal at `R/ecx.R:161` is removed and `ecx_val` stays uncapped |
| 8 | **`type = "relative"` warns when supplied explicitly**, naming `range`, because its meaning changes and 2.1.3 is released. A plain `warning()` rather than `lifecycle::deprecate_warn()`, so no dependency is added |

**Consequences to state in `NEWS.md` rather than let arrive as side effects.**
Every ECx from a hormesis equation changes. Every `ecnsec` changes and generally
becomes smaller. `type = "relative"` returns a different quantity. `NA` appears
in results that previously always returned a number. `example1` reports ECx
values computed through the #196 back-transform and changes for that reason as
well.

**Recorded on the matching toxval issues as each lands**, per D11: #196 to
toxval#19, #195 to toxval#8 and toxval#12, #39 to toxval#40, and the `ecnsec`
alignment to toxval#49.

## D16 — Three further rulings taken with D15

RF, 2026-09-06.

**#206 — the `gaussian` exclusion is removed entirely.** The six zero-bounded
equations become available under `gaussian()` with an identity link, which is the
curve shape OECD TG 201 and Ritz, Gerhard & Streibig (2026) both recommend for
algal growth-rate data. The issue measures that they fit cleanly and that model
weights reject them when wrong. The separate `log`/`logit` link exclusion at
`R/check_models.R:20` is untouched — it is reachable and it is correct.

**#273 — `gamma(5, 4/m)`, chosen on consistency rather than on fits.** RF,
2026-09-07, superseding the instruction to choose on a measurement. The three
entries of `x_prs` are selected on the predictor's distribution, and two of them
place their maximum density at a central measure of it: `beta(2, 2)` at the
centre of the unit interval, `normal(median(x), ...)` at the median. The gamma
entry should therefore peak at *m*, which `rate = 4/m` gives and `rate = 2/m`
does not. It is also the only one of the three that did not do what `?bnec` and
`vignette("example3")` describe.

`gamma(2, 2/m)` was considered and rejected: its *mean* is *m* but its maximum
density is at *m*/2, so it matches neither the documentation nor the convention
the other two entries follow, both of which are specified by where the density
peaks rather than by where its mean falls.

**Refits were run and are not the basis of the decision.** They measured a
bias-variance trade-off with no clear winner, which is not what settles a
question about whether a default is internally consistent, and they have been
removed from the record so that a later reader does not treat them as the
argument. The one quantitative statement that remains is arithmetic rather than
empirical: `(5 - 1) / (2/m)` equals `max(x)` exactly on a series spaced evenly
from zero, so the old prior's mode sat on its own truncation bound.

One thing the episode is worth keeping: `x_prs`'s entries are labelled with
family names but are indexed on the \emph{predictor}'s distribution, so the
`Gamma` entry is the one any non-negative predictor takes whatever the response
family is. That is easy to read the other way round and now has its own test.

**#93 — the two remaining response corrections message once and are recorded on
the fit.** Once per `bnec()` call rather than once per model, stating what was
substituted and how many rows, and stored on the fitted object so a user
comparing `bayesnec` against another engine can recover what was altered. The
`bnec()`-not-`check_data()` placement rule in `R/bnec.R` applies.

## D17 — The NSEC of a draw at or below the reference at the control

RF via Claude, 2026-09-12, on #325. Corrects D15 ruling 3 where it reaches the
NSEC, and leaves it standing for the ECx.

**The ruling as taken.** "A target the curve never reaches within the predictor
range returns `NA` with a warning. Today the nearest grid point is returned,
which for a curve that never declines to the target can report the control
concentration itself as the ECx."

**Two of the three statements in that rationale are wrong.** Measured on
`x = seq(0, 10, 11)` and `y = 10 - x`: for a target below the whole curve, which
is the case a curve that never declines to the target presents,
`which.min(abs(y - target))` returns the *highest* concentration, not the
control. The nearest grid point is the control only where the target lies
*above* the curve at the first grid point. For an ECx that requires
`type = "direct"` with a supplied target above the control, since every other
type derives the target from the draw's own control. For an NSEC it is the lower
`sig_val` tail of the control posterior, and there the control is the correct
answer rather than a defect.

**Why it is the correct answer.** The NSEC reference is the `sig_val` quantile of
the control posterior, so `sig_val` of the draws have a control at or below it by
construction of the quantile, and each of those reaches the reference at the
control itself. Fisher and Fox (2023) obtain every draw's NSEC by backward
interpolation, solving the inverse of the fitted equation analytically, and
exclude none; at p. 2026 they state that the lower bound of the credible interval
"will be 0 for any significance level greater than the 0.025 quantile", and in
the discussion that such an outcome "potentially validly reflects the underlying
fact that the confidence bounds of a true no-effect value may in fact contain 0
because, for smooth curves, a decline in the response may occur at the lowest
concentration". Their Table 3 reports it: 10.5 (6.15–13.6) at 0.01, and 8.78
(0–12.1), 7.84 (0–11.5) and 6.51 (0–10.7) at 0.05, 0.10 and 0.20.

**What `bayesnec` reads as zero concentration.** The lowest observed value of the
predictor named in `crf()`. The package already treats it as the control —
`control_posterior()`, `R/bnec.R:23` and the `top` prior rule at
`R/define_prior.R:374` — and a design that adds a small value to a zero control
so the predictor can be modelled on a log scale needs no separate case, because
that value is the control.

**The rulings.**

| | ruling |
|---|---|
| 1 | A draw whose curve is at or below the reference where the search begins returns the control concentration. `crossing_x()` takes an `x_start` argument, defaulting to `NA_real_` so that the ECx callers are unchanged, and the NSEC callers pass `control_x()` |
| 2 | Those draws are not reported. They are `sig_val` of the draws of every fit by construction, so a message about them restates the definition of the quantile |
| 3 | `NA` and the warning are kept for a draw whose curve does not reach the reference at any tested concentration, and the wording is theirs alone. This is a departure from Fisher and Fox (2023), whose analytic inversion is unbounded above and returns an extrapolated concentration; declining to extrapolate is a package decision and is documented as one |
| 4 | The NSEC crossing is sought at or above the control, because the reference is defined there. A crossing below it would be read off an extrapolation into concentrations the design did not cover, and would make the estimate depend on how far `x_range` extends, which ruling 2 of D15 removed |
| 5 | `hurdle_control_x()` is removed. `control_x()` resolves a hurdle fit to its survival component and returns the same value |
| 6 | A hormetic curve that begins below the target takes `x_start` as well. Its first sign change is the rising limb --- the concentration at which the response reaches the target on the way up --- which estimates nothing. For an NSEC the draw is at or below the reference at the control and the control is its estimate. For an ECx the case is reachable only under `type = "direct"` with a target above the curve, so `NA` is returned, which is the one change this makes to any ECx |

**What was measured, and on what.** `cache/logworkflow/fit_log_regularizing.rds`,
the `example9` workflow fit, `suc | trials(tot) ~ crf(log(dose_adj), model =
"decline")`, 8000 draws. Of the draws `crossing_x()` discarded, every one reached
the reference and none failed to: 80 of 80 on `ecxll3`, `ecxwb1` and
`ecx4param`, 79 of 79 on `nec3param`. The count is `sig_val × n_draws` at each of
0.01, 0.05, 0.10 and 0.20, and the reported 2.5% bound was 0.983, 0.883, 0.829
and 0.761 where the control is 0.005. Master's estimator, applied to the same
posterior, places exactly `sig_val × n_draws` draws at the control, discards
none, and reproduces the structure of Table 3, so the defect was introduced in
this development cycle and never released; it entered in PR #281 at `ce2aa236`
and `027b9f9d`.

**Not decided here.** Whether a draw that never reaches the reference should
instead return the highest tested concentration, making the estimate a lower
bound in the conventional "greater than the highest concentration tested" sense,
and whether a summary should be reported at all when most draws are in that
class — `quantile(..., na.rm = TRUE)` currently reports a value computed from
whichever draws crossed. No fit in which the class is non-empty over the full
tested range has been measured; the only demonstration was obtained by
truncating a prediction grid, which is not a design that would have been fitted.
RF, 2026-09-12: a real example is needed before deciding.
