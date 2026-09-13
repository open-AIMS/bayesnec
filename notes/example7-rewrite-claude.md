# example7 rewrite — specification

Implementation specification for the rewrite of `vignettes/example7.Rmd.orig`
against the revised `bayesnec` defaults. The collaborator-facing plan is on
issue #193; this document is the one to implement from. Where the two differ,
this one is wrong and should be corrected, not worked around.

Written 2026-09-07 against `batch-4-group-scale` at `783ae73d`, which is the
tip of the stack going into `dev` through PR #284. The rewrite must be written
against the landed code, not against this branch, which is 81 commits behind
`dev`.

---

## 0. Status

**Executed 2026-09-07.** The stack landed while this was being written — #286
into `batch-4-group-scale`, then #284 into `dev` — so the rewrite was carried
out the same night rather than deferred. `origin/dev` at `a7ba018f` was merged into
`negsgr-cens-vignette`, the verification checklist in §7 was worked, the
vignette was rewritten to §4, arm G was added, and the vignette was
precompiled. What follows is the plan as agreed; §12 records where the delivered
work departed from it.

---

## 1. Thesis

One sentence, and every section is subordinate to it:

> Flooring a negative growth rate is standard practice, and it is not only an
> edit to the data — it asserts a response distribution bounded at zero, which
> no available family represents exactly. The revised `bayesnec` removes the
> model-side reasons to floor, so the measurements can be kept as recorded.

The supporting result is unchanged from the current vignette: flooring biases
ErCx low and interval coverage falls to zero as the experiment becomes more
precise.

## 2. Two framing errors to avoid

Both were made in the first draft of this plan and corrected by RF on
2026-09-07. They are recorded because they are easy to make again.

### 2.1 The package's restrictions were not the reason people floored

Do **not** write that analysts floored because `bayesnec` would not fit a
zero-asymptote curve under a Gaussian family. The causal order is the other
way round, and in any case the practice is not specific to this package:

- flooring is standard practice across the field and across software;
- a negative value cannot be expressed as a percentage of the control without
  the percentage exceeding 100;
- several commonly used concentration-response equations cannot produce a
  negative mean at all;
- `bayesnec`'s original design reflected that same reasoning. The Gaussian
  exclusion was a *consequence* of the convention, not its cause.

The correct statement is that the revised package removes the **model-side
reasons** to floor. It does not claim the package created the practice.

### 2.2 Flooring and the choice of family are one practice, not two

Do **not** present "substituting zeros" and "choosing a Beta or a Gamma" as
alternatives joined by *or*. Replacing negative values with zero asserts that
the response cannot be negative, which is a statement about the response
distribution. The family choice is that assertion made explicit, so the two are
the same practice at different stages.

The consequence, which the rewrite should state once and plainly:

- a floored response has a point mass at zero;
- the families available for a non-negative continuous response exclude zero
  itself — Beta is on (0, 1), Gamma on (0, Inf), both open;
- so the floored values sit outside the support of the family the flooring
  implies;
- `bayesnec` accommodates this by nudging them inside, in both the original and
  the revised package: an exact zero moves to one tenth of the smallest
  positive value, an exact one to `1 - 0.001` (`R/check_data.R`);
- the nudge is an accommodation, not a model. A continuous non-negative
  response with a point mass at zero would need a family such as Tweedie
  (compound Poisson-gamma), which is not native to `brms`.

Two qualifications to get right. A censored row is now exempt from the nudge,
because the value there is a declared bound rather than an artefact. And the
hurdle and zero-inflated families are **not** the answer to floored negatives:
they treat a zero as a separate process, which is right for a population that
died and wrong for a measurement that was negative and got substituted.

## 3. What the revised defaults do

Verified in the source at `783ae73d`. Every claim the rewrite makes about
package behaviour must be re-verified against the landed code — see §7.

**`mu_support()` (`R/mu_support.R`, new).** States where the mean must lie for
the likelihood to be defined, as a property of the response distribution and
not of the link. `gaussian` is unconstrained, `c(-Inf, Inf)`: "the data enter
only through the residual, and a negative fitted mean is an ordinary prediction
rather than an invalid one."

**`check_models()` (`R/check_models.R`).** The block that dropped every
zero-bounded equation under a Gaussian family is removed with #206. All
fourteen declining equations are now retained under Gaussian, `nec3param`
included. The separate exclusion keyed on a `log` or `logit` link is unaffected
and still applies. A new `record = TRUE` argument attaches an `excluded` data
frame of model and reason, which `bnec()` alone requests (#261).

**`ecx()` (`R/ecx.R`).** Every ECx is measured from the control, the control
being the predicted mean at the lowest concentration in the supplied predictor,
per posterior draw. `type` names what the percentage is measured towards:

| `type` | reference | note |
|---|---|---|
| `"absolute"` (default) | control to 0 | follows OECD TG 201 |
| `"relative"` | control to `bot` | refused where the equation has no `bot` and the family is unbounded below |
| `"range"` | control to the lowest predicted response | what `"relative"` computed up to 2.1.3, except measured from the control rather than the curve maximum |
| `"direct"` | — | `ecx_val` is a response value |

`ecx_val` is no longer capped at 100: under `"absolute"` the reference is zero,
so 120% inhibition of a growth rate is a rate of -0.2 times the control, "which
is what OECD TG 201 reports rather than truncating at 100". The refusal of an
absolute ECx for a Gaussian response fitted without `bot` is removed with #206.
`hormesis_def` is removed; the control is always the reference. An unreachable
target returns `NA` with a warning naming the number of draws affected.

**`nsec()` (`R/nsec.R`).** The reference is the `sig_val` quantile of the
control posterior, the control again being the predicted mean at the lowest
concentration. `sig_val` still defaults to 0.01. `hormesis_def` removed. The
`ecnsec` attribute is now the percent effect at the NSEC defined as `ecx()`
defines it under `"absolute"`; up to 2.1.3 it was measured against the fitted
range by three formulas that agreed only for a monotonic curve.

**`check_data()` (`R/check_data.R`).** The zero and one nudges are unchanged in
value but are now **silent in `check_data()` and reported once from the
user-facing entry points** — `bnec()`, `bnec_group()`, `get_priors()` and
`update()` (#93). Two of the three were previously silent altogether.

**`validate_family()` (`R/validate_family.R`).** `supported_links()` is
`identity`, `log`, `logit`. Identity is forced only when `family` is a
character string or an unevaluated function; a constructed family object passes
through untouched. The current vignette's claim that `bnec()` sets
`link = "identity"` for every family it accepts is therefore too strong.

## 4. Section plan

Target about 6,800 words, from 16,300. Budgets are guidance, not quotas; the
cuts in §5 are what makes them reachable.

| # | Section | Words | What it establishes |
|---|---|---|---|
| 1 | Negative growth rates | 350 | The response, why it goes negative, and that removing it is standard practice for reasons that pre-date any one package (§2.1) |
| 2 | The reported estimate | 500 | TG 201's ErCx is percent inhibition measured from the control, which is what `type = "absolute"` computes; growth rate is not normalised (Ritz et al. 2026); zero growth is 100% effect and values above 100 are meaningful and uncapped; NSEC in two sentences with citations |
| 3 | What flooring commits you to | 700 | §2.2 in full: the point mass at zero, the family the flooring implies, the absence of an exact one, and the nudge |
| 4 | What the revised defaults permit | 400 | `mu_support()`, the retained zero-asymptote equations, the removed ECx refusal. **The pivot of the vignette** |
| 5 | The approaches compared | 500 | The eight arms in three groups plus arm G (§6), with code |
| 6 | The `alga` case studies | 1,100 | Computed live, with arm G added |
| 7 | The simulation | 1,400 | Compendium pointer; scenario table; bias and coverage in the reaching regime; the precision figure; the result that the discrepancy does not shrink as precision improves |
| 8 | Two routes to an adequate floored result | 1,200 | Model averaging and the dispersion sub-model. Both repair a floored analysis; neither is now required |
| 9 | Recommendations | 500 | |
| 10 | Limitations | 400 | |

Section 8 is the one the rewrite exists to produce. It absorbs the current
*Model averaging* section and the dispersion material now scattered through
*Limitations*, and states them as one answer: these are the two established
ways a floored analysis was made tolerable, they work, and the revised defaults
mean neither is needed. Model averaging is the historic `bayesnec` workflow and
underlies a body of existing work; it is not to be presented as a workaround
invented for this vignette, and it is not to be cut.

## 5. What is removed, and why

Each of these explains a mechanism rather than establishing a result. None of
the removals changes a reported number.

| Current section | Words | Disposition |
|---|---|---|
| *Mechanism of the family-floored approaches* (the Gamma sign change) | 700 | Two sentences. The sign change is reported; the investigation moves to the compendium |
| *Direction of the B1 contrast* | 350 | Cut. Its conclusion is superseded by the dispersion result, which shows the compression channel is an artefact of the homoscedastic fit |
| *Sensitivity to the `bot` prior* | 800 | 200 words, keeping the conclusion that the estimates are robust while the plateau is not |
| The `nec3param`/`nec4param` equivalence argument | 250 | Cut. The arm now simply is `nec3param` |
| The issue #195 caveat on `type` definitions | 150 | Cut. Resolved by the ECx work in the stack |
| *ECx on the absolute scale* | 600 | 250 words. The package documents the definitions now; cross-reference `?ecx` |
| Simulation function definitions and design prose | 1,400 | 500 words plus the compendium pointer |
| The eight-curve walkthrough | 1,600 | 600 words. Keep the figure |
| NSEC limiting-value discussion | 900 | 300 words |
| *Limitations* | 1,400 | 400 words |

## 6. Arm G — the zero-asymptote equation fitted directly

**Add one arm to the case studies.** It demonstrates the revised default rather
than describing it, which is what #193 asked for.

```r
G = bnec(y ~ crf(dose, "nec3param"), data = d,
         family = gaussian(link = "identity"), seed = 333,
         open_progress = FALSE)
```

- **Data:** intact, as arm A sees them. Not floored.
- **Prior:** `bnec()` defaults. Arm G is a demonstration of the default path,
  so it must not be handed the shared prior. State in the prose that the
  comparison with B2 is therefore not prior-controlled.
- **Expected result:** agreement with B2 up to the prior, because `nec3param`
  is `nec4param` with `bot` fixed at zero. **State this as a check, not as a
  claim.** If the two disagree materially, that is a finding and must be
  reported, not smoothed over.
- **Simulation:** arm G is **not** added to the simulation. The simulation
  figures are transcribed from the compendium at `0181d66e` and adding an arm
  would require re-running it.

## 7. Claims requiring verification against the landed source

This checklist exists because the first pass on this vignette verified none of
these and asserted six of them wrongly. Work through it against the merged
`dev`, not against `batch-4-group-scale`, and not against this branch.

| Claim in the vignette | Source to check |
|---|---|
| Which equations the Gaussian candidate set retains | `check_models()` |
| The three, now four, `ecx()` `type` definitions | `ecx()` roxygen and `ecx_x_*()` |
| That the ECx reference is the predicted control | `ecx()` implementation, not only its docs |
| That `ecx_val` above 100 is permitted | `ecx()` argument validation |
| `nsec()`'s reference and `sig_val` default | `nsec()` |
| The `ecnsec` definition | `nsec()` |
| The zero and one nudge values, and where they are reported | `check_data()`, `bnec()` |
| That a censored row is exempt from the nudge | `check_data()` |
| The link policy | `validate_family()`, `supported_links()` |
| Which `disp()` forms are available for a response crossing zero | `disp_model.R` |
| `nec()` behaviour where quoted | `nec.R` |
| The count of declining equations | `show_params("decline")`, run |

Where a claim is about behaviour, prefer a chunk that demonstrates it over a
sentence that asserts it. A demonstrated claim cannot go stale silently.

## 8. Computed against transcribed

The distinction must survive the rewrite and be stated in the vignette.

- **Computed by the vignette:** the `alga` case studies, including arm G; the
  scenario table and scenario figure; anything derived from `show_params()` or
  from the package's own behaviour.
- **Transcribed from `open-AIMS/negative-sgr` at `0181d66e`:** the
  eight-approach simulation metrics, the precision sweep, the model-averaging
  sweep, the dispersion check. Keep the *Provenance of the reported results*
  section and its frozen-figure disclosure intact.

## 9. Scope limits to state

1. **The model-averaging sweep was run over the eight-equation Gaussian
   candidate set**, which the removal of the Gaussian exclusion has made
   fourteen. The added equations are the zero-asymptote shapes a floored
   dataset most resembles. State this in one sentence where the sweep is
   introduced. Do not speculate about which way a re-run would move it. Open an
   issue to re-run Phase 10 over the fourteen-equation set.
2. **The case studies remain single-model.** Re-running them under the
   model-averaged workflow would multiply the precompile substantially. Keep
   the existing limitation that says so.
3. **The dispersion check is one scenario at one noise level**, and
   `"loglinear"` approximates the linear `sigma` ramp the data were generated
   from. The existing wording in `notes/disp_sensitivity_example7.md` covers
   this.

## 10. Sequencing

1. Stack lands: #286 into `batch-4-group-scale`, then #284 into `dev`.
2. Merge `dev` into `negsgr-cens-vignette`. Not before — the other session owns
   the stack and merging from both ends produces conflicts in files this
   vignette does not touch.
3. Work through §7 against the merged source and record what changed.
4. Rewrite the prose to §4 and §5.
5. Add arm G to the case-study chunk.
6. Precompile once, for the whole package. About 137 minutes for this vignette.
7. Update PR #243, body and a comment saying what moved (§14 of `CLAUDE.md`).

## 11. Out of scope

- Re-running the simulation compendium.
- Adding a model-averaged case-study arm.
- Re-running Phase 10 over fourteen equations. That is the issue opened under
  §9.1, not part of this rewrite.
- `CLAUDE.md` §11's first entry, which states that `bnec()` forces
  `link = "identity"` on every family. That is now inaccurate for the reason in
  §3, but it is RF's file and is not edited here. Raise it separately.


---

## 12. Departures from the plan, and what was measured

Recorded 2026-09-07, after execution.

**Length.** The plan set about 6,800 words. Delivered is 8,199 words of prose,
from 11,674 — a 30% reduction rather than the 40% budgeted. The overshoot is in
the simulation and the model-averaging sections, which were kept fuller than
planned once the decision to retain model averaging was taken. Counted as prose
only: code chunks, HTML comments and fenced blocks excluded. The whole-file
count including code is 13,094 words, from 16,341.

**The Gaussian candidate set is demonstrated, not asserted.** `check_models()`
is not exported, but `models()` is, and `R/models.R` records that it derives its
answer from `check_models()` rather than restating it. `models(c(-Inf, Inf))`
therefore returns the set `bnec()` retains for an unbounded response, and the
vignette prints `setdiff(names(show_params("decline")), names(models(c(-Inf,
Inf))))`, which is `character(0)`. This is better than the plan asked for: the
claim cannot drift from the package's own rule without the chunk changing.

**Arm G's palette.** The eight-hue separation analysis recorded in the colour
chunk was done for eight series and has not been re-run for nine. Arm G is
therefore given its own `case_arm_levels` and `case_arm_cols`, used only by the
case-study figure, where the arm is encoded on the y axis as well as by colour.
The simulation figures are untouched and still use the original eight.

**Phase 10 re-run: measured, and not attempted.** The compendium records its own
budget in `analysis/phase10_gate2_pilot.R`: 31.5 worker-minutes per iteration
measured on the pilot, over the eight-equation set, with wall-clock per
iteration equal to worker-minutes because each iteration occupies one worker.
The script's own formula is `n * 3 * per_iter / w / 60` hours plus about two
hours of warm-up. At n = 200, three cells and 16 workers that is **21.7 hours
for the eight-equation set**. Fitting fourteen equations rather than eight
scales the per-iteration cost by roughly 14/8, giving **about 36 hours**. This
machine has 22 cores. The re-run is therefore between one and two days of
dedicated compute, not an overnight job, and it was not started: it would have
competed with the precompile for cores, and the compendium checkout carries a
recorded hazard about concurrent sessions (`RESUME.md`).

**Verification results.** All twelve checks in §7 were run against merged `dev`.
Confirmed: fourteen declining equations, all fourteen retained under
`gaussian(link = "identity")`, `nec3param` among them; `disp("power")` refused
where the fitted mean crosses zero, with the message naming `disp("loglinear")`;
the zero and one nudges unchanged in value, censored rows exempt, and reported
once from `bnec()` via `report_substitutions()` with a `remedy` field naming
`hurdle_gamma()` or `zero_inflated_beta()`; `supported_links()` is `identity`,
`log`, `logit`. Arm G was smoke-tested before the precompile was launched:
`nec3param` under a Gaussian family fits, and `ecx(type = "absolute")` and
`nsec()` both return three finite values on it — the path that was refused
before #206.
