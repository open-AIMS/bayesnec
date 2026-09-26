# Specification for the backlog run

Companion to `backlog-run-human.md`, which states each decision in a sentence and
refers here. This document is written for the Claude Code session that executes
the run. Read `notes/implementation/00_protocol.md` first for how to work, then
this document for what to build, then `notes/implementation/03_decisions.md` for
RF's rulings.

Written 2026-09-26 against `predev` at `21472c39`, whose tree is identical to
`dev` at `c3824643`, the merge of PR #409.

## 1. Sources and what they cover

Every "current location" and every reproduction below comes from three read-only
investigations run on 2026-09-26 against `predev` at `21472c39`, under
`devtools::load_all()` on R 4.6.1 and brms 2.23.0. No model was sampled; the
only Stan-facing call was `brms::make_stancode()`. Line numbers are those of
`21472c39`, and any item that lands earlier changes them, so confirm each location
before editing.

The findings were posted on 2026-09-26 as a comment on each issue, headed
"Findings on `predev`", and RF's rulings as a separate comment headed
"Decision". An implementing session reads both before starting.

The reproductions quoted in #403, #415, #416 and #417 were re-run and gave the
outputs the issues quote. The defects filed against `eff06ea9` (#397, #398,
#400) were confirmed still present on `21472c39` and found to be broader than
filed; §4 gives the extent.

## 2. Scope

In scope: #397, #398, #400, #403, #404, #410, #412, #413, #415, #416, #417,
#418, #299, #120 and #44, and a precompile of the vignettes on the HPC at the
end. #419, filed on 2026-09-26 at RF's suggestion, is in scope under D31.

Out of scope:

- #255, the toxval migration, by RF's instruction of 2026-09-26.
- #27, which stays open (D27). A separate investigation is drafting its body.
- #382 and #388. PR #402 implements both and is being repaired by another
  session. The run neither edits its branch nor merges it.

## 3. Order and dependencies

The items are numbered in the order they are done. Items 1 to 12 change nothing
PR #402 touches and go ahead whatever happens to it. Items 13 to 16 change
code PR #402 rewrites, so they wait for it; if it has not merged when item 12 is
done, they are done on `predev` anyway (D18), and the run posts a `[claude]`
comment on PR #402 naming the changes it must take in when it is next brought up
to date with `predev`.

Within items 1 to 12, the order puts first the defects PR #409 introduced, then
every other item that changes a vignette's printed output, so that those land
before the precompile is submitted.

| # | issue | decision | depends on | vignette output | reason for the position |
|---|---|---|---|---|---|
| 1 | #416 | none | nothing | none | a defect PR #409 introduced; the smallest item; includes the first version bump |
| 2 | #417 | none | nothing | none | a defect PR #409 introduced; extracts the grid mapping a later #299 option 1 would reuse |
| 3 | #415 | D20 | nothing | `example6` | a defect PR #409 introduced |
| 4 | #299 | D23 | 2 | `example1`, `example3`, `example8`, `example9` | option 3; follows item 2, which it reads the mapping from |
| 5 | #120 | D19 | nothing | `example2` | interface settled by D5 |
| 6 | #404, #403 | D21, D22 | nothing | `example4`, `example8` | the comparison and averaging half of #404, with #403 |
| 7 | #412 | D28 | 1 | `example6` | follows item 1, which edits the same summary method |
| 8 | #398 | none | nothing | none | a refusal |
| 9 | #400 | D30 | nothing | none | a refusal |
| 10 | #397 | none | nothing | none | changes fitted results; includes the second version bump |
| 11 | #44 | D19, D25 | 6 | none | reuses item 6's record-preserving subset |
| 12 | #410 | D29 | nothing | none | the `bnec_hurdle()` growth block; independent of PR #402 |
| 13 | #404 | D22 | PR #402 | `example8` | the table half of #404; `estimate_table()` exists only on PR #402 |
| 14 | #413 | D24 | PR #402 | `example8` | `vignettes/fit_store.R` is rewritten by PR #402 |
| 15 | #410 | D29 | PR #402, 10, 12 | none | `disp()` on the joint two-block families; PR #402 rewrites `R/bnec_joint.R`, and item 10 edits the same centring code |
| 16 | #419 | D31 | PR #402, 9, 13 | none | the constant equation; PR #402 edits `R/define_prior.R` and `R/inits_functions.R`, and the equation's estimates rely on the censoring of items 3, 6 and 13 |
| bg | #418 | D26 | nothing | none | starts at the beginning, in the background |
| end | precompile | D18 | the deadline | all | §4, the precompile and the store refit |

An item whose decision is still open when it is reached is skipped and taken up
when the decision is recorded.
## 4. The items

Each item gives the current locations, the change, the tests, the definition of
done, and what stops it. The general definition of done in `00_protocol.md`
applies to every item in addition.

### Item 1. An explicit `x_range` in the hurdle summary (#416)

Current locations. `summary.bayesnechurdlefit()` at
`R/bayesnechurdlefit-methods.R:354-437`; the ECx call that collides at `:379`,
inside `:374-383`. The same `...` reaches `nec(object, which = w, ...)` at `:391`,
where an `x_range` supplied by the caller is silently absorbed:
`nec(bayesnecfit, x_range = c(0, 1))` returns the ordinary estimate with no
message.

The change. Take `x_range` from `...` when the caller supplies it and pass it
once; otherwise use `hurdle_summary_range()`. Do not forward `x_range` to
`nec()`: a NEC is a sampled parameter with a censoring bound fixed at fit time.
State in `?summary.bayesnechurdlefit` that `x_range` sets the grid of the ECx
rows only.

Tests. An explicit `x_range` reaches each ECx call exactly once with both limits
intact, which is asserted by mocking `ecx()` and inspecting its arguments; with
`x_range` absent the summary default is used; `xform` and `resolution` are still
forwarded; one component with no stored grid; single-equation and model-averaged
components.

Does not decide #412.

This item also bumps `DESCRIPTION` `Version` from 2.1.3.39 to 2.1.3.40, the
first bump since before PR #409 (D18). Item 14 declares 2.1.3.40 as the oldest
`bayesnec` a fit store may have been made with.

### Item 2. The bound label under a decreasing transformation (#417)

Current locations. `to_axis_scale()` at `R/helpers.R:2393-2420`, both branches,
including the automatic inversion of a decreasing `crf()` term. In
`R/autoplot.R`: `bind_nec()` `:317-341`, `censored_label()` `:351-357`,
`bind_ecx()` `:366-381`, callers `:472`, `:476`, `:534`, `:538`. In `R/plot.R`:
`:143-144`, `:291-292`, and the legends `:173-184` and `:315-326`, which apply
`lxform` to the labels. `bound_prefix()` at `R/helpers.R:698`.

The measured defect. The reproduction in #417 gives `">=-0.90"` on all three
labels where `"<=-0.90"` is correct. The per-draw path is already right:
`nec(fit, xform = function(x) -x)` returns `<=` marks, because it applies
`xform` to each draw and `xform_censoring()` swaps the ends. Only the
summary-level remapping in `to_axis_scale()` is wrong, and it also leaves `Q2.5`
and `Q97.5` in their original positions, so the second entry becomes the larger.

The change. Where the map is decreasing, detected by comparing the mapped values
of the two ends of the grid, swap `Q2.5` and `Q97.5` and remap the
`censored_summary` attribute with the same rule `xform_censoring()` applies per
draw. Extract the mapping itself, the numeric inversion on the prediction grid,
into a named internal function so that #299 option 1 can call it rather than
reimplement it.

Tests. As #417 lists: `>=` becomes `<=` under `function(x) -x`, for NEC and ECx
annotations, single-equation and model-averaged fits, explicit `xform` and
automatic inversion of a decreasing `crf()` term; the lower and upper entries
exchange places; identity and increasing maps are unchanged. Check the base
`plot()` annotations and legends by inspection, and add a test where they share
the code path.

Item 13 re-checks the joint-fit caller at
`R/bayesnecjointfit-methods.R:486-494` that PR #402 adds.

### Item 3. The combined hurdle threshold across unequal bounds (#415)

Current locations. `combine_censored_min()` at
`R/bayesnechurdlefit-class.R:421-462`, with the equal-grids comment at `:442-444`,
called from `nec.bayesnechurdlefit()` at `:210`.

The change, under D20. The combined draw is
the minimum of the two components. Where one component is censored above its
upper limit `U` and the other is identified with a value above `U`, the minimum
is known only to exceed `U`: mark the draw censored above. Where the identified
value is at or below `U`, the minimum is that value and is identified. Where both
are censored above, the minimum exceeds the smaller of the two limits. The record
holds one upper bound for the posterior, so every censored draw takes the
smaller of the two components' upper limits. That is weaker than the per-draw
truth and never false. It must not depend on the order of the components. The
below-range branch is already conservative and true; confirm it with a test
rather than change it.

Tests. The fixture in #415 returns a censored result, not `c(20, 20, 20)`; growth
censored above 10 against survival at 5 still returns 5; component order
reversed; censoring at both ends; unequal ranges in both directions; a mixture
containing a curve-derived NSEC with no stored value; the public `nec()` method
and the hurdle summary.

Record in the PR body, without changing it, that `concat_censoring()`
(`R/helpers.R:506-537`) makes the same equal-bounds assumption for model-averaged
sets, and that its documentation limits the exposure to sets assembled by hand.

### Item 4. A message naming the scale of a returned estimate (#299)

Under D23. A message, once per call, where the predictor is
transformed inline in `crf()` and no `xform` is supplied, naming the
transformation found and the `xform` to pass. It applies to `nec()`, `ecx()`,
`nsec()`, `ecnsec()`, `curve_params()` and `average_estimates()`, 23 method
signatures in all at `21472c39`. `compare_estimates()` has no `xform` argument
and calls the estimators with `xform = identity` at
`R/compare_estimates.R:83` and `:86`; it must not produce the message once per
fit. Follow the once-per-call pattern of `bayesnec.relative_warned` in
`R/compare_estimates.R`.

`nsec.brmsfit()` and `nsec.drc()` have no `crf()` term and are exempt.

### Item 5. `model` and `average` in place of `all_models` (#120)

Under D19. The interface is D5 in
`03_decisions.md`: `model =` names one or more equations, `average =` is a
logical choosing whether the model-averaged outcome is shown, and `all_models`
keeps working for one release cycle with a warning, mapped onto the new
arguments.

Current locations. `autoplot.bayesmanecfit()` at `R/autoplot.R:92`, `:118`,
`:129`, `:134`, examples `:45`, `:49`; `plot.bayesmanecfit()` at `R/plot.R:212`,
`:226`, `:240`, `:242`, example `:28`; `man/autoplot.Rd`, `man/plot.Rd`;
`vignettes/example2.Rmd.orig:113`; `tests/testthat/test-plot.R:86-104`.
`predict.bayesmanecfit()` never had `all_models`. `model` is not an argument of
these methods today: `autoplot()` and `predict()` absorb it in `...`, and
`plot()` warns that it is not a graphical parameter. Nothing clashes.

The change. As D5. The deprecation warning follows the plain-warning precedent of
`validate_ecx_type()`, since `lifecycle` is not in `Imports`. `predict()` gains
`model` and `average` as D5 states. Edit `example2.Rmd.orig`; do not regenerate
`example2.Rmd`.

Two external callers were found and are not edited by the run: `toxtools`
(`plot_workbook_fit(all_models = )`, in its Shiny app and Quarto report) and
`cr_modelling_training` modules 4 and 5. Name both in the PR body. The
deprecation keeps both working.

The D11 obligation applies: toxval registers the same three methods on the same
class, so record the change on the matching toxval issue when it lands.

### Item 6. Censoring in comparisons and averages (#404 and #403)

Current locations. `compare_estimates()` in `R/compare_estimates.R`: the
estimator calls at `:83-99`, the draw subset that strips the censoring record at
`:110` (`m[sample(...)]`), and `mean(m, na.rm = TRUE)` at `:137`.
`compare_posterior()` (`R/compare_posterior.R:85-111`) only forwards.
`average_estimates()` has the same pattern at `R/average_estimates.R:119-125`.
`gm_mean()` at `R/helpers.R:1006`, whose only caller is
`R/average_estimates.R:124`.

The measured defects, on structural fixtures from the packaged `manec_example`:

- `compare_estimates(..., "ecx")`: 74 and 59 of 100 draws censored; 90 of 100
  difference pairs `NA`; `prob_diff` of 0.6 reported over the remaining 10 pairs
  with nothing in the result to say so.
- `compare_estimates(..., "nec")` does the opposite: parameter draws beyond the
  grid keep their values, so the comparison uses values that `nec()` itself
  reports only as a bound.
- `average_estimates(list(a = nec4param, b = ecx4param), "ecx", ecx_val = 50,
  x_range = c(0.03, 1.66))`: 43 of 100 averaged draws are exactly 1.000, and the
  reported `Q2.5` is exactly 1.000, because `gm_mean()` divides by `length(x)`
  while summing over the values present.
- `gm_mean(c(4, NA))` and `gm_mean(c(4, 1))` both return 2; `gm_mean(c(NA, NA))`
  returns 1; `gm_mean(c(4, 0))` returns 2.

The change, under D21 and D22. Subset draws with their record rather than
through `[`, so the record survives. Compute the comparison from the record per
Q6, and the average from the record per Q4. Both comparison types must treat a
draw beyond the grid the same way `nec()`, `ecx()` and `nsec()` report it.

Also in this pull request: `?nsec` contradicts itself. `R/nsec.R:82-84` says a
censored draw is "excluded from the summary" and `:122-125` says it "keeps its
rank"; the second is what the code does since #395.

Tests. As #403 and #404 list, with the pinned values following the decisions: a
comparison with one component censored and one not; an average with one
component `NA`; an input with nothing missing, bit-identical to today's result as
the regression guard; an all-missing input.

### Item 7. The hurdle ECx grid (#412)

The issue names the wrong helper. A bare `ecx()` on a hurdle fit goes through
`hurdle_component_preds()` (`R/bayesnechurdlefit-class.R:63-97`, survival default
at `:70-77`), not through `hurdle_newdata_pair()`
(`R/bayesnechurdlefit-methods.R:193-206`), which serves only `posterior_predict()`
and `predict()`. `nsec()` (`:266`), `ecnsec()` (`:1022`), `posterior_epred()`
(`:57`) and the plot panels (`:788`) share `hurdle_component_preds()`.

`hurdle_component_preds()` reads both components, and the combined curve, on the
survival range by design. Its comment at `:64-69` gives the reason: growth is
fitted to survivors only, so it has no data above the highest concentration at
which anything survived, and the combined endpoint needs that stretch. Growth is
extrapolated over it, which is harmless in the product because survival there is
near zero.

`summary.bayesnechurdlefit()` (`R/bayesnechurdlefit-methods.R:354-437`) instead
passes the intersection of the two stored grids (`hurdle_summary_range()`,
`:1061-1075`) as `x_range`, which cuts survival and the combined curve at growth's
maximum. Its survival NEC row still uses survival's own range, so the summary is
inconsistent within itself as well as with a bare `ecx()`.

The change, under D28: growth's estimates read from growth's own observed range, so that a growth
ECx beyond it is reported as censored rather than read off an extrapolated curve;
survival's and the combined estimates read from the survival range, as
`hurdle_component_preds()` does now; `summary()` and a bare `ecx()`, `nsec()` and
`ecnsec()` follow the same rule. The plots and `posterior_epred()` keep the
survival range for every curve, since a plotted growth curve beyond its data is a
prediction, not an estimate. `NEWS.md` records the changes to a bare growth ECx
and to the summary's survival and combined rows.

Tests. A hurdle fixture whose components' ranges differ: `summary()` and `ecx()`
agree for each component and for the combined estimate; a growth ECx beyond
growth's range is censored; the combined ECx reads beyond growth's range.

### Item 8. Non-syntactic column names (#398)

The measured extent is wider than filed. `rate()` and `cens()` fail at
`R/bayesnecformula.R:1257` and `:1220` as the issue describes. `crf()` fails as
well, at `:446` in `check_formula()`, so the issue's statement that the same name
works in `crf()` is wrong. `brms::make_stancode()` gives the same parse error for
a backticked name in `rate()`, `cens()`, a non-linear predictor and the
response, so making such a name work is not available.

The change. One refusal at the top of `check_formula.bayesnecformula()`, near
`R/bayesnecformula.R:436`: compare `all.vars()` of the formula against
`make.names()` of the same, and name every offending column and the term it
appears in. The message suggests renaming the column, for example with
`make.names()`. Raise it before any fitting, and confirm that `bnec()`,
`bnec_group()`, `bnec_hurdle()` and `make_brmsformula()` all reach it.

Tests. A backticked name in `crf()`, `rate()`, `cens()` and the response, each
refused by name; syntactic names unchanged.

### Item 9. A bounded response at the bound in every observation (#400)

The measured extent is wider than filed. `get_priors()` fails with the
`quantile()` error for `bernoulli` with every observation 1, `bernoulli` with
every observation 0 (through `min_z_val`, which returns `Inf`), and `binomial`
with every count equal to its trials. `beta` with every observation 1 does not
fail: it is shifted to 0.999 with a message.

Current locations. `response_link_scale()` at `R/helpers.R:860`, `:862-863` and
`:893-894`; the call-level check beside `check_inline_boundary()` at
`R/bnec.R:874`; `bnec_group()` before its level loop, `R/bnec_group.R:183-266`;
a backstop in the prior path for `get_priors()` and `amend()`.

The change. A refusal that names the response column, states that every
observation is at the bound, and names which bound. Raised in `bnec()` before the
model loop and in `bnec_group()` before the level loop, following
`bayesnec/CLAUDE.md` on the placement of a new data refusal, with the backstop
kept for the routes that do not come through `bnec()`. Under D30,
`bnec_group()` refuses the whole call before any level is fitted where any one
level has every observation at a bound, naming the level, and `beta` with every
observation at a bound is refused rather than shifted to 0.999.

Tests. Each of the four cases, and `beta` at 1, through `bnec()` and through
`get_priors()`; one observation away from the bound still fits; a `bnec_group()`
call with one level at a bound is refused before any level is fitted.

Amend `notes/prior_audit.md` where it records the nine `bernoulli` cells lost to
this, to say they are now refused by name. Do not re-run the audit.

### Item 10. The `disp()` centring constant (#397)

The measured extent is wider than filed. On the `rate_data()` series with
`negbinomial` and `disp("power")` the literal is `3.02013`, which is
`median(log(y))`; the value on the scale of the mean is `2.19722`. Under
`disp("loglinear")` the literal is `20.5` where the rate scale gives `9`. The
same defect affects `beta_binomial` with `trials()`: `disp("power")` centres on
`2.48142`, which is `median(log(count))`, where the mean is a proportion with
`median(log(y / n))` of `-0.514`, and under `disp("twosided")` the `LOG1MREF`
term collapses to `- 0` because `1 - count` is never positive.

Current location. `disp_y` at `R/bayesnecformula.R:791-793`; the rate variable
is already read at `:767`. `fit_bayesnec()` divides by both the rate and the
trials before building priors and initial values (`R/fit_bayesnec.R:69-77`); the
formula builder does not.

The change. Divide `disp_y` by the rate denominator, or by the trials, inside the
formula builder, so that `bnec()` and `make_brmsformula()` generate the same
literal. Share the read of the rate variable rather than repeating it.

Tests. The literal for a `negbinomial` `rate()` fit under `disp("power")` and
`disp("identity")`, and for `beta_binomial` with `trials()` under `disp("power")`
and `disp("twosided")`, each against the value computed on the scale of the
mean; a fit with neither term, whose literal is unchanged.

`NEWS.md` records a change to fitted results for `negbinomial` fits with both a
`rate()` and a `disp()` term, and for `beta_binomial` fits with a `disp()` term.

This item bumps `DESCRIPTION` `Version` by one, because it changes what a fit
produces (D18).

### Item 11. Exceedance of a threshold (#44)

Under D19. Nothing in the package answers "what is the
probability that this estimate exceeds a concentration of concern" with the
censoring accounted for. `compare_posterior()` compares fits with each other;
`brms::hypothesis()` on `pull_brmsfit()` reaches the `nec` parameter of a single
equation only.

The measurement that sets the design, on `manec_example`'s `nec4param` with the
grid capped at 1.67: P(EC50 > 1.65) is 0.83 counting draws censored above as
exceeding, which equals the answer on the full range, and 0.66 when those draws
are dropped. Dropping them is wrong by 0.17 on this example.

The change, under D25. A threshold inside the prediction range is decided
exactly by the record, because a draw censored above exceeds it. A threshold
beyond an end is decided only as an interval,
[P(identified > T), P(identified > T) + P(censored above)], and is reported so.
The function reuses item 6's record-preserving subset.

Tests. A threshold inside the range with censored draws; a threshold beyond the
upper end; a model set; a hurdle fit through its combined threshold.

### Item 12. `disp()` on a hurdle fit (#410)

`swap_response()` (`R/bnec_hurdle.R:566-575`, called at `:213`)
keeps the whole right-hand side, so the survival formula receives the `disp()`
term; the growth fit (`:250`) runs before the survival fit (`:257`), so the
refusal arrives after sampling.

The change. Remove any `disp()` term before `swap_response()` builds the survival
formula. Validate the term against `family_growth` before either component is
sampled. `?bnec_hurdle` states that a `disp()` term applies to the growth block
only. The joint route is item 15 (D29). Check that
`check_fit.bayesnechurdlefit()` (`R/check_fit.R:372`, `:435`), `dispersion()`
(`R/dispersion.R:101`), `posterior_predict()`, `predict()` and `fitted()`
(`R/bayesnechurdlefit-methods.R:52-190`), the `hurdle_positive_mean()` shape path
for a `negbinomial` growth block, `update.bayesnechurdlefit()` (`:667`) and
`amend()` read the growth block's dispersion sub-model.

Tests, with short fits reused as one fixture: `disp("power")` and `disp(~x)` on
the growth block, present in `$growth` and absent from `$survival`; a
specification the growth family cannot take, refused before sampling.

Whether the survival block should ever take an observation-level term in place of
`disp()` is out of scope, as #410 says.

### Item 13. The censoring marks on group and joint tables (#404)

PR #402 places the body of `group_estimate_table()`
(`R/bayesnecgroupfit-class.R:85-116` at `21472c39`) in a new
`estimate_table()`, shared by `group_estimate_table()`, `joint_estimate_table()`
and `joint_ne_table()`. The marks are lost at
`as.data.frame(as.list(unclass(e)[nms]))`, so `nec()`, `ecx()`, `nsec()` and
`ecnsec()` on both a `bayesnecgroupfit` and a `bayesnecjointfit` return unmarked
numbers. `joint_level_ne()` (`R/bayesnecjointfit-methods.R:431-437` on the PR #402
branch) calls `as.numeric(nsec(...))`, which strips `censored_summary` from the
joint-fit plot annotation for levels fitted with a smooth equation.

The change, under D22. Keep the marks in `estimate_table()` so that all
three callers inherit them, and stop `joint_level_ne()` discarding them.
Re-check the joint-fit caller of `to_axis_scale()` that item 2 fixed.

Tests. A group fit with one level censored: `nec()` on the group and `nec()` on
that level's own fit agree about the mark as well as the number. The same for a
joint refit. `test-bnec_group.R:119` and `:146` pin the column names and change
under a new column.

### Item 14. Measuring `example8` (#413)

A local store exists at `grouping-structures/store`, 1.2 GB, fifteen assembled
targets and 226 manifest rows, whose `MANIFEST` records `bayesnec` 2.1.3.39,
assembled 2026-09-18. The compendium's `hpc/bayesnec.lock` pins commit
`dd9e2944`, which is before PR #409 changed the priors. Two hazards make such a
store unsafe to use. `vignettes/fit_store.R` prints the `MANIFEST` and never
compares versions. The compendium's own guard compares `packageVersion()`
strings, and `Version` was 2.1.3.39 before and after the whole #386 programme.

The change, under D24. `vignettes/fit_store.R` declares the oldest `bayesnec`
version a store may have been fitted with, 2.1.3.40 (item 1's bump), and stops
with a message naming both versions and how to refit where the store's
`MANIFEST` records an older one. A minimum rather than an equality, because a
bump for an unrelated change must not invalidate a store whose fits it does not
touch; the declared minimum is raised only by a change to what `example8` fits.
Document in `notes/` how to refit and fetch a store with the compendium. The
refit itself is in §4, the precompile and the store refit.

The same item corrects `example8.Rmd.orig` where it speaks of a "stacking
weight", in prose and as the axis title `"stacking\nweight"` (lines 964 and 1428
on PR #402's branch). `bayesnec` weights a set by pseudo-BMA unless a user asks
for stacking, so the term misdescribes the figure.

### Item 15. `disp()` on the joint hurdle and zero-inflated families (#410)

Under D29. `check_disp_spec()` (`R/disp_model.R:116-121`) refuses `disp()` for
every two-block family, recording that coupling a variance function to one block
of a joint fit needed a decision about the other. The decision is that `disp()`
models the dispersion parameter of the positive block only: `shape` for
`hurdle_gamma` and `hurdle_negbinomial`, `phi` for `zero_inflated_beta`. The `hu`
or `zi` block is unaffected. `hurdle_poisson` has no dispersion parameter and
keeps its refusal. brms 2.23.0 reports these dispersion parameters for the three
families and accepts a `shape` formula beside an `hu` formula on `hurdle_gamma`
(`make_stancode()`, 2026-09-26).

The change. Remove the refusal for the three families. Build the dispersion
sub-model on the positive block's parameter, written in terms of that block's
`mu`, which in brms is the component mean rather than the overall mean
`(1 - hu) * mu`, so that the factorised and joint routes fit the same variance
function. Compute the centring constant from the positive responses only, on the
scale of the mean as item 10 establishes. `bnec_joint()` includes the growth
block's `disp()` term in the joint model it builds from a `bnec_hurdle()` fit.
Check the downstream readers that item 12 lists for the joint class as well.

Tests. `bnec(family = "hurdle_gamma")` with `disp("power")` and with `disp(~x)`,
asserting the generated `shape` formula and that no `hu` term gains one;
`zero_inflated_beta` likewise with `phi`; `hurdle_poisson` still refused;
`bnec_joint()` on a `bnec_hurdle()` fit with `disp()`, reusing item 12's fixture,
whose joint model includes the same variance function; the centring literal
computed without the zeros.

### Item 16. A constant equation (#419)

Under D31. #419 gives the pieces and the tests; its decision comment settles the
three questions it raised. In this run:

- the equation is named `ecxflat`, is available by name, and joins no group in
  `data-raw/sysdata.R`; joining `all`, `ecx` and `decline` is left to the 3.0
  release and recorded in `NEWS.md` as planned;
- the two checks in `R/nec.R` (`:196` and `:292`) that classify an equation by
  the substring `"ecx"` in its name are changed to test for a `nec` parameter,
  as `R/expand_classes.R:63-67` already does;
- a response with no variation, fitted with any model set through `bnec()`, is
  fitted with `ecxflat` alone, with a message saying why the other equations
  were not fitted. This replaces item 9's refusal on the fitting routes. In
  `bnec_group()` it applies to the level concerned; if the crossed table of
  `crossed_group_weights()` cannot hold a level fitted with a different set,
  stop and report rather than change that table. `get_priors()` keeps item 9's
  refusal for the curve equations;
- the comment at `R/check_models.R:308` that speaks of "stacking weight" is
  corrected to "model weight", since the default is pseudo-BMA.

Tests. As #419 lists, and a `bernoulli` response with every observation at 1
through `bnec()` with `model = "all"`, fitted as `ecxflat` with the message.

### The background item. Fitted validation of the incomplete-design changes (#418)

PR #409 has merged, so this validates the merged state rather than informing
the merge. The candidate is `c3824643` and the base is `eff06ea9`, the parent of
the programme. Both report `Version` 2.1.3.39, so the record states the commit.

The design, per #418 and D26: `nec4param` and `ecx4param`, each on three
simulated designs (a complete response as control, an incomplete response whose
lower asymptote is not observed, and a threshold above the highest concentration
tested), gaussian, positive response, known parameters. Fits: base defaults, six;
candidate defaults, six; candidate with `asymptote_observed = FALSE`, four;
`prior_type = "uninformative"` on the incomplete cases, four on the candidate and
four on the base. About 24 fits per data seed.

Mechanics. Two detached worktrees in the scratchpad, each installed with
`R CMD INSTALL --library=<scratch>/lib-base` or `lib-cand`. Each script prepends
its library with `.libPaths()` and asserts `find.package("bayesnec")` before
fitting. `R_LIBS_USER` is not used, because it replaces the user library rather
than adding to it. Fits run as separate `Rscript` processes, five at a time, at
`bnec()` defaults with a fixed `seed`; never through a `multisession` future,
which deadlocks on this machine. Budget: about fifteen minutes of wall clock per
data seed at five concurrent fits.

Output. `notes/scripts/incomplete_design_fits.R` and
`notes/incomplete_design_fits.md`, recording for each fit what #418 lists, with
the revision, seed, settings and limitations. Commit straight to `predev`: it
touches `notes/` only. Do not describe a successful example as showing that the
unobserved part of a curve can be recovered from the data.

### The precompile and the store refit

D18. RF's VPN connection, needed to submit to the AIMS HPC, was renewed at 19:50
AEST on 2026-09-26 and closes near 18:00 AEST on 2026-09-27. A job already
submitted keeps running after it closes; fetching the output needs the VPN
again. The run therefore submits and does not wait.

`hpc/local.conf` is git-ignored, so the run's worktree has none. Copy it from
`/mnt/c/Rworking/wt-190/hpc/local.conf`. It holds the account and login node and
is never committed. The login node answered over SSH on 2026-09-26.

A test at the start. From the run worktree, clean and at the tip of `predev`,
submit one short vignette with `./hpc/precompile-hpc.sh --no-wait example2b`,
fetch it with `--fetch example2b` when it finishes, confirm that a rendered
`.Rmd` came back, then discard it with `git checkout -- vignettes/`. Record the
job and the outcome in `05_run_log.md`. If it fails, record the error; the full
submission is then not attempted, since it would fail the same way.

The `example8` store. Once PR #402 has merged into `predev`, from
`/mnt/c/Rworking/grouping-structures` run `./hpc/deploy.sh --ref <predev
commit>`, following that repository's `README.md` and `CLAUDE.md`. It records
the commit in `hpc/bayesnec.lock` and chains install, 189 units, assembly and the
joint refit on the cluster; commit the lock in that repository. If PR #402 has
not merged by 10:00 AEST on 2026-09-27, the refit is not started and `example8`
is left out of this precompile. None of items 1 to 14 changes what `example8`
fits, so the store needs no refit after this one.

The full submission, at the end of the list or at 16:00 AEST on 2026-09-27,
whichever is first, from the run worktree, clean and at the tip of `predev`:
`./hpc/precompile-hpc.sh --no-wait` naming every vignette except `example8`.
Where the store's assembly and joint jobs have finished, submit `example8`
separately with
`BAYESNEC_FIT_STORE=/export/scratch/$USER/grouping-structures/store`. Record in
`05_run_log.md` the job identifiers, the `predev` commit submitted, and every
item merged after it, whose effect the rendered vignettes will not show.

Collecting the output needs the VPN, so it is left to RF or a later session:
`--fetch` each vignette, check the rendered `.Rmd` files for home directories,
library paths and compiler output as `bayesnec/CLAUDE.md` requires, and open one
pull request into `predev` with the rendered vignettes and figures.

## 5. Decisions

RF answered these on 2026-09-26. A settled question is marked with the D-entry
in `03_decisions.md` that records the ruling, and the ruling is also posted on
the issue. Every question is settled. An item that meets a question this
document does not answer stops, with the reason recorded in `05_run_log.md`.

### Q1. Whether #120 and #44 are in this run

Settled: D19, as recommended.

`02_deferred.md` defers both to the toxval migration: #120 because toxval
registers the same `predict`, `plot` and `autoplot` methods on `bayesmanecfit`,
and #44 because it is a new estimator interface, tracked as toxval#41. D11
(2026-09-06) already reversed the same deferral for the estimator fixes, on the
reasoning that the migration relocates a corrected file exactly as it relocates
an uncorrected one, provided each fix is recorded on the matching toxval issue.

Options: include both on D11's reasoning; include #120 only, whose interface D5
already settles; keep both deferred with #255.

Recommendation: include both. #44 depends on the censoring record added by #395,
which exists only in `bayesnec`, so building it here builds it on the code it
needs.

### Q2. The zero-truncated gaussian (#27)

Settled: D27. #27 stays open and is not part of the run. An investigation fits a
truncated gaussian to the `nassarius` growth data, growth block only, against
Gamma with and without `disp("power")`, and drafts the issue body with the
evidence; a hurdle version is written up if the growth-only result shows value.
The implementation will need decisions of RF's, including which mean an ECx
refers to under truncation.

### Q3. The representation of a combined hurdle bound (#415)

Settled: D20, as recommended.

Options: the existing one-sided record, marking the combined draw censored above
the smaller limit; or an interval record holding both limits. An interval
changes `censoring_record()`, `summarise_censored()`, `xform_censoring()`,
`concat_censoring()`, the `censored_summary` vocabulary, `censored_label()`,
`bound_prefix()` and the print note, which are the surfaces item 2 and item 13 edit.

Recommendation: the one-sided record. It removes the false point estimate, which
is the defect, and omits only the finite upper limit (20 in the issue's example).

### Q4. What an average does with a missing component (#403)

Settled: D21, as recommended, including both edge cases.

Options: drop the component and average the rest; propagate, so the averaged
draw is missing; or propagate through the censoring record, so the averaged
draw is marked censored in the direction of its censored component and is
summarised as a bound.

Evidence. Where the missing component is censored above, it is the larger value,
so dropping it biases the average low and still reports an identified number.
Plain propagation makes the draw `NA`, which `quantile(na.rm = TRUE)` at
`R/average_estimates.R:125` then deletes, the same silent deletion #404 objects
to.

Recommendation: propagate through the censoring record. Also rule on two edge
cases: a component value of zero, which the current code excludes from the sum
but counts in the denominator (recommendation: treat as censored below at the
grid's lower limit, which is the only way an estimate reaches zero); and an
all-missing input, which returns 1 today (recommendation: `NA`, marked
censored).

### Q5. A column or an attribute on group and joint tables (#404)

Settled: D22, as recommended.

Options: a column naming the bound direction per estimate; an attribute. Evidence:
tabulation matches by name rather than position; `test-bnec_group.R:119` and
`:146` pin the column names; PR #402's `nec.bayesnecjointfit()` already adds
`model` and `ne_type` columns, which is precedent; an attribute does not print,
and `rbind()` and subsetting drop it.

Recommendation: a column.

### Q6. What a comparison returns when components are censored (#404)

Settled: D22, as recommended.

Options: refuse; return the difference with each component's censored fraction
attached; return bounds on the probability.

Evidence. `prob_diff` depends only on the sign of each paired difference, and the
record fixes the sign of many censored pairs: a draw censored above `U` against
an identified draw below `U` is a positive difference whatever the censored
value is. Only pairs in which the sign cannot be determined are indeterminate.

Recommendation: report `prob_diff` as a lower and upper bound computed from the
determinable pairs, equal where no pair is indeterminate, together with each
component's censored fraction; do not refuse. The difference draws themselves are
returned with indeterminate pairs marked rather than deleted.

### Q7. The hurdle ECx grid (#412)

Settled: D28, option 4. RF first accepted option 3; it was then found to conflict
with the design of `hurdle_component_preds()`, and RF accepted option 4 in its
place.

Options, from the issue and the investigation:

1. the intersection of the two grids as the default for `ecx()` and `nsec()` on
   a hurdle fit, so the bare call agrees with `summary()`. Applied inside
   `hurdle_component_preds()` this also changes the plots and
   `posterior_epred()`;
2. keep both defaults and state in `?ecx` and in the summary which grid was used;
3. each component's estimates on its own grid, and the combined estimates on the
   intersection;
4. growth's estimates on growth's own range; survival's and the combined
   estimates on the survival range; `summary()` and the bare estimators alike.

Evidence against option 3. `hurdle_component_preds()` reads every curve on the
survival range deliberately (its comment at `R/bayesnechurdlefit-class.R:64-69`):
growth has data only where something survived, and the combined endpoint needs
the concentrations above that, where survival falls to zero. The intersection
ends at growth's maximum, so option 3 would censor combined estimates that the
current code identifies. `combine_censored_min()` combines the two component
thresholds and is not affected either way.

Recommendation: option 4. It keeps the combined endpoint on the range it is
designed for, stops a growth estimate being read off a curve extrapolated past
growth's data, and makes the summary agree with a bare call.

### Q8. The scale of a returned estimate (#299)

Settled: D23, as recommended; option 1 is left to the 3.0 release.

Options, from the issue: (1) estimators default to the recorded scale by numeric
inversion, which changes every reported value from an inline-transformed fit;
(2) an attribute naming the scale; (3) a message once per call where the
predictor is transformed inline and no `xform` is supplied.

Recommendation: option 3 in this run, as item 4. Option 1 changes published numbers
in `example1` and `example9` and builds interpolation error into every estimate,
so it is a release decision rather than a defect fix, and needs the vignettes
re-rendered after it.

### Q9. Measuring `example8` (#413)

Settled: D24, as recommended, except that the store is refitted on the HPC within the run (§4, the precompile and the store refit).

Options, from the issue: (1) document how to obtain a current store; (2) keep a
small measurement-only set of `example8` fits outside the vignette build, about
28 local fits taking 30 to 60 minutes; (3) accept the gap and have every
cross-vignette measurement say so.

Recommendation: option 1 with the version check in item 14 and the resumed version
bump, so that a stale store is refused rather than loaded. The store itself is
refitted on HPC by RF after the run.

### Q10. The joint route for a hurdle fit with `disp()` (#410)

Settled: D29. RF asked for the dispersion to be modelled in the joint route
rather than refused. Nothing prevents it: the refusal in `check_disp_spec()`
recorded a choice not yet made, which was which block a variance function
applies to, and the choice is the positive block. Items 12 and 15.

### Q11. Responses at a bound in grouped fits and under `beta` (#400)

Settled: D30, as recommended on both points. RF raised the alternative of a
constant equation that could fit such a response; it is #419 and Q14.

`bnec_group()` fits a separate model for each level of a factor, for example
one per site. Where every observation at one site is at a bound (every organism
survived at every concentration) and the other sites vary, that site identifies
no curve.

First, options: refuse the whole call before any level is fitted, naming the
level; fit the other levels and report that level as skipped. Recommendation:
refuse, so that the user removes the level explicitly and the omission is
visible in their script. The body of #400 already places the check before the
level loop, which implies this.

Second, whether `beta` with every observation at a bound joins the refusal rather
than being shifted to 0.999 and fitted. A response with no variation identifies
no curve whatever the family. Recommendation: refuse.

### Q12. The interface of an exceedance test (#44)

Settled: D25, as recommended.

Options: a new exported function; an argument to `nec()`, `ecx()`, `nsec()` or
`summary()`. For a threshold beyond an end of the prediction range: refuse;
report the interval of §4 item 11; require `extrapolate = TRUE`.

Recommendation: a new function, `exceedance(object, threshold, estimate =
c("nec", "nsec", "ecx"), ecx_val, xform, ...)`, returning the probability with
its lower and upper bound and the number of draws censored at each end, and
reporting the interval beyond the range. A probability only; an evidence ratio in
the style of `brms::hypothesis()` is a transformation of it and can be added
later.

### Q13. Whether and how far #418 is run

Settled: D26, as recommended.

Options: one data seed, gaussian only, stating the other families unmeasured;
three data seeds; not run.

Recommendation: one data seed and gaussian first, which is about fifteen minutes;
extend only if the one seed shows something that needs a second look.

### Q14. The constant equation (#419)

Settled: D31. The name is `ecxflat`, following the prefix convention of `?bnec`,
since two checks in `R/nec.R` classify by the substring `"ecx"`. It joins no group
in this run, and `all`, `ecx` and `decline` at the 3.0 release. A response with no
variation, fitted with any model set, is fitted with `ecxflat` alone with a
message.

## 6. Work without an issue

The staleness audit of 2026-09-26 found open work recorded only in `notes/` or
on closed issues. Two items are folded into the run: the `?nsec` contradiction
into item 6, and the difference between the two comparison types into item 6. The rest
are to be filed as issues and are not part of the run:

- the open questions on #388 (families other than Beta never sampled on the
  joint route; one grid range for every level; `adapt_delta` not raised;
  `disp_by_level = FALSE` never fitted), before PR #402 closes #388;
- divergent transitions in `summary()`, part D5 of
  `notes/tasks/148-model-fit-diagnostics.md`, which `example2.Rmd.orig:97`
  already claims;
- regenerating `example3.Rmd`, which still shows the truncated prior, and the
  release precompile that goes with it;
- the flatness report's rate on complete designs with a wide last dose step
  (0.944 on `log_wide`), and whether to document it in `?bnec` or add a
  condition on extent;
- whether `MASS` belongs in `Suggests` (`notes/prior_audit.md:868-872`);
- a ruling on the gamma `bot` coverage measured in `notes/prior_audit.md:1147`.

## 7. Documents brought up to date with this plan

Dated status notes were added to the planning documents whose work has finished,
and amendments to `386-incomplete-designs-*.md`. The `382-joint-group-refit-*`
pair exists only on PR #402's branch and is not edited here; it needs correcting
before PR #402 merges, since it still describes one equation for every level. `CLAUDE.md` contains three statements that later work
contradicts and is not edited by a session; they are listed for RF in the human
document.
