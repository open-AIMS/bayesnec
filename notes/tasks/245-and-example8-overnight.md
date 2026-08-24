# Overnight run, 2026-08-24: fix #245, then build example8

Authorised by RF before going afk on 2026-08-24. RF returns 2026-08-25 and will
review. Work autonomously; stop and leave a written question only if a decision
is genuinely blocking and cannot be made under a stated assumption.

## Decisions already taken by RF — do not re-litigate

| question | decision |
|---|---|
| base for the #245 fix | **`dev`**, not PR 227. `ogl()`/`pgl()` are shipped `dev` behaviour, and another session is actively committing to `issue-33-factor-covariate`. |
| coral data | **ship it** as `data/coral.rda` with `data-raw/coral.R`, roxygen docs, provenance and the Brinkman citation. Flag in the PR that AIMS permission still needs confirming — do not assume it. |
| default group-level `sd` prior | **response-scaled half-t**: `student_t(3, 0, diff(range(y))/10)`. No family special-casing. |
| vignette scope | **all three parts**, plus `example_fi.csv` for the #33 factor interaction. |
| precompile | **not run.** Protocol holds. Chunks are executed locally to verify and to get real numbers, but `precompile.R` is not invoked. |

## Phases

- [x] **1. Issue #245 updated.** DONE 2026-08-24. Defect 2 narrowed (the `sd`
      row is already dropped by the `class == "b"` filter at
      `R/inits_functions.R:134`; what fails is the `ogl` *intercept*, class
      `"b"`, at the set-equality check on line 143). Defect 3 added: Stan
      initialises a lower-bounded `sd` as `uniform(-2, 2)` on the unconstrained
      scale, i.e. `sd in (0.135, 7.39)` *independent of the prior*, so a scaled
      prior may not be sufficient and an init may also be needed.
- [x] **2. Branch `issue-245-group-priors` off `origin/dev`**, in worktree
      `/mnt/c/Rworking/wt-245`. Implement:
      (a) response-scaled half-t `sd` prior in `get_priors()`;
      (b) exclude group-level `nlpar`s from the init name check;
      (c) test whether (a) alone initialises — if not, generate inits too;
      (d) round-trip `sd` rows through `usable_prior()`/`get_priors()`;
      (e) testthat tests, NEWS, roxygen, `devtools::document()`.
      Verify on the `nec_data` reprex in #245 **and** on the coral data.
      Push, open PR to `dev`, link #245.
- [x] **3. `data/coral.rda`** on the vignette branch, from
      `ignore/Toxicity Test Data and WQ_brinkman2022.xlsx`.
- [x] **4. example8 rewrite** on `issue-6-33-grouping-vignette` (worktree
      `/mnt/c/Rworking/bayesnec-stack`), with current `issue-33-factor-covariate`
      and `issue-245-group-priors` merged in. Run every chunk.
- [x] **5. PR 228 body rewritten**; `prompts/example8-data-decision.md` updated.

## The vignette design, as agreed with RF

Data: Brinkman et al. 2023, *Environ. Pollut.* 332:121963 — coral fragments on
coloured glass tiles, WAF exposure. Use **T4 (96 h)**, which has ten missing
values and all of them benign (Nominal 0 and 8, every one alive at T5) — unlike
T2, whose 18 gaps are dying corals and would need reconstruction.

- **Part 1, within-concentration.** `ogl(Chamber)` — 32 chambers, one
  concentration each. Beta on proportion of live tissue. Ungrouped baseline is
  known clean: NEC 838 [680, 980], R-hat 1.007, 0 divergences, 1.6 min.
- **Part 2, across-concentration.** `pgl(TileColour)` — 5 colonies spanning the
  full range — and `bnec_group(group_var = "Light")` for #33. Add
  `example_fi.csv` (Chlorella/Zn over hardness x pH, from a published study) as
  the stronger motivating dataset for the factor; mention briefly that the
  approach is no longer recommended and cite Ritz 2026.
- **Part 3, the joint structure.** Grouping on `bnec_hurdle()`. State the
  constraint from its own docs: group-level terms apply within each component
  **independently**, so a single shared chamber effect across both blocks is not
  available — the crossed-weights arithmetic depends on that.

Contrast to show rather than smooth over: `bernoulli` on binary survival gives
NEC 1855 [1616, 1910] against Beta's 838, with 10 divergent transitions.

Known simplification to state explicitly in part 3: of the zeros in growth, 60
are dead corals but **20 are alive with growth exactly 0**, at Nominal 5, 8, 13
and 22 — the descending limb. `hurdle_gamma` attributes all 80 to the hurdle
process. State it as a simplification and point back to example6's three-kinds-
of-zero section; do not re-teach zero provenance here.

Do **not** cite example7's model-averaging result as cover for conditioning on
survival. Example7 tests zero-*flooring*; conditioning is a different failure and
it never tests it. Growth conditioned on survival loses the top three
concentrations entirely (every coral dead at 36, 60 and 100% saturation), which
is the nassarius failure that got that dataset rejected — so if growth appears
at all it is as a deliberate counter-example, per RF's agreement that live
tissue and survival are modelled separately, consistent with example6.

## Writing style

Scientific report throughout. RF has flagged AI-signature headings twice —
"Where these numbers come from", "What saturation costs", "The eight approaches".
No headings of that shape.

## Standing constraints

- Never `--delete-branch` a stacked PR base.
- `air.toml` — check for it before formatting; format only if present.
- Log the session in `prompts/example8-data-decision.md` as work proceeds.

## Design for the #245 fix, settled 2026-08-24 by reading the code

### The scaling rule

`define_prior()` already sorts the curve parameters into three groups, and its
own comments name them: **y-dependent** (`top`, `bot`), **x-dependent** (`nec`,
`ec50`), and **x- and y-independent** (`beta`, `slope`, `d`, `f`, all
`normal(0, 5)`).

The group-level SD prior follows the same split, under one rule: **the SD prior
scale is one tenth of the scale the parameter's own prior spans.**

| nlpar | group-level SD prior |
|---|---|
| `top`, `bot`, `ogl` | `student_t(3, 0, diff(range(y))/10)` |
| `nec`, `ec50` | `student_t(3, 0, diff(range(x))/10)` |
| `beta`, `slope`, `d`, `f` | `student_t(3, 0, 0.5)` — one tenth of their `normal(0, 5)` |

This keeps RF's chosen `diff(range(y))/10` for the response-scaled case and
generalises it, with no family special-casing. `student_t(3, 0, .)` keeps the
`brms` shape and heavy tail; only the scale changes.

### The `ogl` intercept is confounded, and needs a prior of its own

`ogl` enters as `y ~ ogl + bot + (top - bot) * f(x)`, so its **population
intercept is not identified** — a constant added to `ogl` can be removed from
`top` and `bot` with no change to the likelihood. `brms` gives a class-`b`
non-linear parameter a flat improper prior by default, so nothing currently
pins it.

Generate `normal(0, diff(range(y))/10)` for it, centred at zero: `top` and `bot`
carry the level, so the grouping term's job is deviation about that level, and
zero-centring is what makes the decomposition identified. **This is my
judgement call, not RF's — flag it in the PR.** The cleaner alternative is to
drop the intercept outright (`ogl ~ 0 + (1 | group)`), which removes the
confounding rather than shrinking it, but that changes shipped formula
construction and is a bigger behavioural change than a prior fix should carry.

### Where the code goes

1. `R/bayesnecformula.R` — new `parse_group_terms(formula, model)`, mirroring
   `parse_disp_term()`. Returns the `ogl` vars, the `pgl` vars expanded over the
   model's own parameters, and the `(par | group)` pairs. The parsing to copy is
   in `add_formula_glef()`, which already splits exactly these three forms.
2. `R/define_prior.R` — new `define_group_prior()`, appended the way
   `define_disp_prior()` is at the end of `define_prior()`; new `group_spec`
   argument.
3. `R/get_priors.R` — parse and pass down; add `"sd"` to the classes
   `usable_prior()` keeps, so an `sd` row round trips as #207 did for dispersion.
4. `R/helpers.R:749` and `R/amend.R:256` — the two other `define_prior()` call
   sites, both on the fit path.
5. `R/inits_functions.R` — exclude group-level `nlpar`s from the name check,
   following the `disp_pars()` precedent at `helpers.R:835`.

### Defect 3 must be tested, not assumed

After the priors generate, run the #245 reprex. If it still fails to initialise,
the prior is not sufficient and inits are needed for the group-level parameters
too — Stan's default init for a lower-bounded `sd` is `uniform(-2, 2)` on the
unconstrained scale and ignores the prior entirely. Do not close this out on a
passing `get_priors()` call; it has to actually sample.

## Progress log

**2026-08-24, phase 2 part-built.** Written and passing:
`parse_group_terms()` (`R/bayesnecformula.R`), `define_group_prior()` and the
`group_spec` argument (`R/define_prior.R`), the plumbing at
`R/fit_bayesnec.R`, `R/get_priors.R` and `R/amend.R`, the init filter and
`"sd"` in `auxiliary_classes()`, `group_inits()` / `sd_prior_scales()`
(`R/inits_functions.R`). Tests added to `test-define_prior.R`,
`test-get_priors.R` and `test-inits_functions.R`; all three files pass.
`devtools::document()` produced no `man/` or `NAMESPACE` change -- every new
helper is `@noRd`. `DESCRIPTION` at 2.1.3.25, above the whole live stack
(dev .19, stack .20-.24). NEWS entry written.

**A confounded experiment, and the lesson.** The first attempt to settle
defect 3 fitted a hand-rolled `brms` replica of the bayesnec model. Every arm
failed at initialisation, which looked like strong evidence. It was not: the
replica hard-coded `beta(2, 2)` for `nec` with bounds 0.03-3.22, and a Beta
density is zero outside (0, 1), so the log density was `-Inf` at every starting
point whatever else was varied. The real generated prior is
`gamma(5, 2.283)`. **Test through `bnec()` and `get_priors()`, not through a
hand-written brms replica** -- the same trap `CLAUDE.md` section 11 records for
the link argument, arriving by a different route.

Defect 3 is therefore **still unsettled by experiment** as of this note. The
mechanism argument stands on its own -- Stan initialises a lower-bounded `sd`
as `exp(uniform(-2, 2))` regardless of the prior, and `b_ogl` is unbounded and
starts anywhere in (-2, 2) -- and `group_inits()` is written on that basis. The
run that settles it is `bnec()` with `ogl(tank)` on `nec_data`, with the
group-level inits first disabled and then enabled. Do not report defect 3 as
confirmed until that pair has actually run.

**Defect 3 confirmed, 2026-08-24.** The pair of runs, both through `bnec()` on
`nec_data` with `ogl(tank)`, Beta/identity, same priors, same seed:

| | result |
|---|---|
| scaled priors, no group-level init | `Error : Initialization failed.` |
| scaled priors **+ `group_inits()`** | fitted, max R-hat 1.019 |

So the prior is necessary but not sufficient, exactly as the mechanism
predicted, and #245 needs all three parts. The grouped fit on `nec_data`
carries 178 divergent transitions, which is **not** evidence against the fix:
tank labels were assigned arbitrarily to that dataset, so the true group SD is
zero and the non-centred parameterisation funnels. The confirmation that
matters is the coral fit, which has a real chamber effect.

**Phase 3 done.** `data-raw/coral.R`, `data/coral.rda` and the roxygen block in
`R/data.R` are on `issue-6-33-grouping-vignette`. 160 fragments, 30 columns.
Provenance is now conclusive rather than inferred: `ignore/1-s2.0-S026974912300965X-main.pdf`
is Brinkman et al. 2023, *Environ. Pollut.* 332:121963, "Sensitivity of the
Indo-Pacific coral *Acropora millepora* to aromatic hydrocarbons", covering
toluene, naphthalene and 1-MN — the three sheets of the workbook. The sheet
also embeds its own 32-line data dictionary, so every column description is
verbatim rather than inferred. **AIMS permission to redistribute is still
unconfirmed and must be flagged in the PR.**

Corroboration of the earlier session's `example_ogl.csv` match: that file's
`Treatment` levels 255, 425, 708, 1180 are exactly this dataset's day-7 measured
concentrations at nominals 5, 8, 13 and 22. `example_ogl.csv` is a
four-concentration subset of this same experiment.

**Two corrections to the earlier session's numbers.**

1. **Day 3 is a serious rival to day 4, and was dismissed on the wrong
   measure.** The earlier session scored the days on informative concentrations
   for *binary* survival, where day 3 has one. On the *proportion of live
   tissue* — the endpoint actually chosen — day 3 carries 41 interior
   observations, the same as day 4, with **no missing values at all** and 20
   fragments at every concentration. Day 4 loses 10 fragments and is unbalanced
   (n = 15 at nominals 0 and 8), but has more spread on the descending limb
   (0.703, 0.457 against day 3's 0.865, 0.651) and one more concentration
   carrying interior values. Decide on the fits, not on either summary.
2. **22 corals are alive with growth exactly 0, not 20.** 60 NA + 22 zeros +
   78 positive = 160. The vignette text must say 22, and the hurdle section's
   "every zero is the hurdle process" count is 82, not 80.

**Day 4 confirmed on evidence, 2026-08-24.** Beta/identity `nec4param`, 2000
iterations, 2 chains, on the shipped `coral`:

| | NEC | max R-hat | divergences |
|---|---|---|---|
| day 3, ungrouped | 1877 [1723, 1943] | **1.112** | 50 |
| day 4, ungrouped | 842 [685, 972] | 1.007 | **0** |

Day 4 reproduces the earlier session's 838 [680, 980] almost exactly. Day 3's
completeness does not compensate for a fit that will not converge, and the
reason is visible in the means: day 3 falls 0.651 → 0 between nominals 36 and
60, a cliff, where day 4 descends 0.982 → 0.703 → 0.457 → 0. **Use day 4**, and
say in the vignette that the ten missing values were checked and are benign
(nominals 0 and 8, every one alive the next day) rather than passing over them.

**A gap found in my own fix, and closed.** `define_prior()` returns early for a
hurdle family, so `group_spec` was ignored for exactly the fits part 3 needs.
Now appended in that branch too, scaled from the **survivors only**, which is
the response the mu block is fitted to.

**And a finding for part 3's text.** A group-level term on a hurdle fit reaches
the **mu block only** — `add_formula_glef()` runs before the `hu` sub-formulas
are attached, so `ogl` and `pgl` never see them. Verified against the formula
`wrangle_model_formula()` actually builds, and pinned in a test. This is
*stronger* than the "applies within each component independently" wording the
earlier session quoted from `bnec_hurdle()`: for a hurdle **family** fitted
through `bnec()`, the term applies to one component. Whether `bnec_hurdle()`
itself behaves differently is a separate question and has NOT been checked —
do not write either claim into the vignette without testing the specific route
the vignette uses.

**The `ogl` identifiability ridge, 2026-08-24.** The fix makes grouped fits
*start*, which is what #245 is about, but the fits are divergence-heavy. Day 3
with `ogl(chamber)`: fitted, max R-hat 1.011, but **630 divergent transitions**
of 2000, where the *ungrouped* fit on the same data and the same boundary mass
had 50 and day 4 ungrouped had none. So the divergences come from the group
term, not from the zeros and ones.

The mechanism is the confounding already documented in `define_group_prior()`:
`mu = ogl + bot + (top - bot) * f(x)`, so a constant added to `ogl` comes
straight back out of `top` and `bot`. The zero-centred prior penalises that
ridge but does not remove it, and a shallow ridge is what a divergent
transition reports. Two remedies under test on day 4 (`ogl_divergences.log`):
`adapt_delta = 0.99`, and fixing the `ogl` intercept at zero with
`constant(0)` — which removes the ridge outright rather than shrinking it, and
is only possible because #246 made `constant()` priors usable.

If `constant(0)` is the answer, there is a **design question for RF**: should
`get_priors()` generate `constant(0)` for the `ogl` intercept by default,
rather than the zero-centred normal? That is the honest reading — a group-level
term should carry deviations about a level the curve already has — but it
changes the model rather than the prior, so it is RF's call and not mine.

**A loose end in my own code.** `group_inits()` adds `b_ogl = 0`
unconditionally when an `ogl` term is present. If the `ogl` intercept carries a
`constant()` prior, Stan does not declare `b_ogl` at all, and the #244 logic
that strips inits for constant parameters runs inside `add_brm_defaults()` --
*before* the group inits are appended in `fit_bayesnec()`, so it does not cover
them. Harmless today (both backends ignore an init for an undeclared
parameter), but it is the same hygiene #244 chose to keep, so apply the same
strip to the group inits.

**All four coral fits, 2026-08-24.** Beta/identity `nec4param`, 2000 iterations,
2 chains, seed 245.

| day | grouping | NEC [95% CI] | max R-hat | divergences |
|---|---|---|---|---|
| 3 | none | 1877 [1723, 1943] | 1.112 | 50 |
| 3 | `ogl(chamber)` | 1867 [1649, 1947] | 1.011 | **630** |
| **4** | none | 842 [685, 972] | 1.007 | **0** |
| **4** | `ogl(chamber)` | 845 [682, 973] | 1.004 | 25 |

Day 4 wins on every count. Two things the vignette should say rather than hide:

1. **The chamber term barely moves the NEC** — 845 [682, 973] grouped against
   842 [685, 972] ungrouped. That is precisely the comparison PR 228's
   failure-mode section already tells the reader to make, and the honest answer
   here is that the term is carried because the *design* demands it, not
   because it changes the estimate.
2. **25 divergent transitions remain** where the ungrouped fit had none. Report
   them and their remedy rather than sampling until they disappear.

**The `light` factor, and why it replaces `example_fi.csv`.** The source paper
included UVR as a co-factor in the 1-MN experiment *because* UVR is known to
increase the toxicity of some aromatic hydrocarbons to corals — and found no
effect:

> There was no significant effect of UVR exposure on 1-MN concentrations on
> coral survival over the 7-d exposures (p = 0.845, Table S10), therefore
> replicates from the 1-MN experiment were pooled per concentration.

The design it describes matches the shipped data exactly: "four replicate
chambers per treatment including two replicate chambers exposed to visible
light only (400-700 nm) and the other two replicate chambers exposed to visible
light + UVR (300-400 nm)" — 8 concentrations x 4 chambers = the 32 chambers in
`coral`, 5 colonies in each, 10 fragments per light x concentration cell.

So `light` is a **motivated factor covariate with a published null result**,
which makes it a better `bnec_group()` demonstration than a factor with a large
effect: the vignette reaches a conclusion that can be checked against the paper,
and it teaches the negative answer, which is the one a reader is more likely to
have to defend. The apparent PAR/UV gap at nominal 36 (0.293 against 0.620) is
noise consistent with p = 0.845, and the vignette must NOT present it as a UV
effect — the direction is backwards for phototoxicity anyway.

**Consequence for `example_fi.csv`.** RF asked for it as "a stronger motivating
dataset for the factor", said before the coral workbook was found and while the
plan was built on `example_ogl.csv`. `coral` now carries the factor itself, so
the vignette is complete on one dataset — which is the property RF liked in the
first place. `example_fi` is therefore **not shipped**: doing so is a second
redistribution decision, on a second published dataset, that RF has not been
asked about specifically. Built on `coral` alone, flagged in the PR as an open
option with the work it would need. This is a deliberate narrowing and must be
stated plainly to RF, not left for them to notice.

**The two divergence remedies, and the design question answered.** Day 4,
`ogl(chamber)`, 2000 iterations:

| | NEC | R-hat | divergences | sd(ogl) |
|---|---|---|---|---|
| default `adapt_delta` = 0.8 | 845 [682, 973] | 1.004 | 25 | — |
| **`adapt_delta` = 0.99** | 845 [686, 973] | 1.003 | **0** | 0.0053 |
| `constant(0)` on the `ogl` intercept | 837 [683, 966] | 1.007 | 5 | 0.0052 |

So **the design question I raised for RF is answered and does not need
escalating**: `get_priors()` should keep generating the zero-centred normal for
the `ogl` intercept, not `constant(0)`. Fixing the intercept cuts the
divergences but does not remove them, and it changes the model rather than the
prior. My earlier note speculated it "removes the ridge outright" — it does
remove the ridge, but the ridge is not the whole cause. The residual funnel is
the standard deviation itself sitting at 0.005 against a response spanning
[0, 1], which is the ordinary non-centred funnel and is a property of the data,
not of the parameterisation.

`sd(ogl)` = 0.005 either way is itself the headline: **the chambers do not
distinguish themselves**, which is the first of the three failure patterns the
vignette teaches, occurring in the vignette's own worked example. The vignette
now shows it rather than describing it in the abstract.

**RETRACTED -- see the correction that follows. Original note:** **An open problem the #245 fix does NOT solve: `pgl()`.** On the coral day-4
data, `pgl(colony)` fails to initialise with `chains = 2, iter = 2000`, while
the same model with `chains = 1, iter = 400` fits. `ogl(chamber)` and
`(nec | colony)` both fit cleanly, so this is specific to `pgl`.

What has been ruled out, by capturing the argument list `brm()` actually
receives:

- the group inits *are* generated (8 elements: `sd_1..sd_4`, `z_1..z_4`);
- every `z` is exactly 0 and both chains' curve inits are valid --- chain 1
  `top` 0.947 / `bot` 0.617, chain 2 `top` 0.933 / `bot` 0.075 --- so the mean
  at the supplied starting point lies inside (0, 1) for both;
- yet Stan reports "Initialization between (-2, 2) failed after 100 attempts",
  which is its **random**-init message. So the supplied values are being
  discarded somewhere between `brm()` and Stan, rather than being rejected on
  their merits.

Not yet explained. It must **not** be written up as though the fix covers
`pgl`. Report it in the #245 PR as a known remaining gap and open a follow-up
issue; the honest claim is that #245 fixes `ogl()` and `(par | group)` and
leaves `pgl()` improved but not working on this dataset.

A separate oddity worth checking while there: the generated `sd` prior for
`nec` under `pgl` is `student_t(3, 0, 369.9)` --- one tenth of the coral
predictor range, which is 3724 ug/L. That is correct by the stated rule and
still very wide in absolute terms. `group_inits()` starts every `sd_k` at the
*median* of the generated scales, which mixes the response-scaled 0.094 with
the predictor-scaled 370 and gives 0.297 for all four. That is defensible while
`z = 0`, but the per-parameter scale is available and would be better.

**Consequence for the vignette.** `pgl()` stays as an unevaluated illustration,
which is what PR 228 already did, and `(nec | colony)` --- which fits cleanly,
NEC 840 [680, 973], R-hat 1.004, 0 divergences --- carries the
across-concentration demonstration. That is the narrower and better-supported
claim anyway.

**Correction, 2026-08-24: `pgl()` is fixed, and the note above is wrong.** The
exact configuration recorded as failing — `pgl(colony)`, `chains = 2`,
`iter = 2000`, `seed = 245` — now fits: NEC 895 [730, 1046], R-hat 1.009, 4
divergent transitions. The only code change between the failing run and this one
is `group_inits()` querying the group-level dimensions with `gaussian()` instead
of the fit's own family.

That change was made as robustness, not as a fix, and it turned out to be the
fix. The mechanism: `make_standata()` validates the response against the
family's support, the `coral` response still carried exact zeros and ones at the
point `group_inits()` saw it, the Beta family rejected it, the `try()` swallowed
the error, and the function returned no initial values at all — silently. With
`gaussian()` the query cannot fail for a reason unrelated to what is being
asked.

**One part is still not fully explained and should not be written up as though
it is:** a `chains = 1` run reproduced *before* that change did produce inits
and did fit, so the pre-change failure was not unconditional. The honest claim
is that the `gaussian()` query is what separates the failing and passing runs,
not that every detail of the earlier behaviour is understood.

The lesson is the one worth keeping: **a `try()` that turns a failure into a
quietly empty result is how a fix hides its own gap.** That is why the fallback
now warns.

**Two vignette facts corrected against the data, 2026-08-24.**

1. Growth is missing at the top **three** concentrations (36, 60, 100 — zero
   non-missing at each), not the top two. My draft said two; the earlier
   session's "top three" was right. The vignette now shows the count rather
   than asserting it.
2. Growth is measured over the **full seven days**, while the curve analyses use
   **day four**. The vignette now says so explicitly, because the two windows
   are not interchangeable and a reader would otherwise assume one dataset slice
   throughout. Day 7 was not used for the curves because the live-tissue
   proportion is close to a step by then — `surv_d7` means are 1.00, 0.996,
   0.996, 0.980, 0.512, 0, 0, 0.

**Phase 2 complete, 2026-08-24.** Commit `5bea5494` on
`issue-245-group-priors`, pushed, **PR #250** open against `dev`. Full suite
`NOT_CRAN=true`: 0 failures, 0 skips. `inits_functions` re-run separately to
confirm the two `skip_on_cran` end-to-end fits (`ogl` and `pgl`) actually run.
`devtools::document()` added `importFrom(brms, make_standata)` to `NAMESPACE` —
it was missing and R CMD check would have failed without it.

**Part 3 does not yet have a fit that can be shipped, 2026-08-24.**
`hurdle_gamma` with `ogl(chamber)` on the seven-day growth:

| | NEC | max R-hat | divergences |
|---|---|---|---|
| default `adapt_delta` | 575 [155, 1107] | 1.023 | 511 |
| `adapt_delta` = 0.99 | 634 [147, 1135] | >1.05 on some parameters | 133 |

At 0.99 `brms` reports "Parts of the model have not converged", `hubeta` is
0.95 +/- 3.84 [-4.59, 10.23] — effectively unidentified — and bulk ESS falls to
46. **This must not ship as a worked example**; a reader would copy it.

The cause is visible in the data and is not a sampler problem to tune away:

| concentration | zero growth | growing |
|---|---|---|
| 0 | 0 | 20 |
| 255 | 2 | 18 |
| 425 | 5 | 15 |
| 708 | 9 | 11 |
| 1180 | 6 | 14 |
| 1966, 2938, 3818 | 20 each | **0** |

The hu block has information everywhere; the mu block has none above the fifth
concentration. Asking for a chamber effect on top of a two-block model, where
one block sees five of eight concentrations, is asking more than these data
carry.

Testing now: whether the **ungrouped** joint fit converges. If it does, part 3
becomes the joint fit as the working example, with the grouped attempt shown as
the failure and its diagnostics as the lesson — which is consistent with what
the rest of the vignette teaches, and better than presenting a fit that has not
converged. If the ungrouped fit is also poor, part 3 needs rethinking and should
be raised with RF rather than patched.

**PR 228 depends on PR 250, and that is the honest answer, 2026-08-24.** The
vignette's grouped fits cannot run on `issue-6-33-grouping-vignette` as it
stands, because the #245 fix is not on that branch. Two routes were tried and
both rejected:

- **Merging `issue-245-group-priors` in** conflicts in `R/print.R` and
  `R/summary.R` — it drags `dev`'s newer state (240, 246) against 226's
  `check_fit` work.
- **Cherry-picking the fix commit** conflicts in `DESCRIPTION`, `NEWS.md`,
  `R/amend.R` and `test-inits_functions.R`.

Neither is worth leaving on a shared branch overnight. **PR 228 is now stacked
on PR 250**: merge 250 to `dev` first, then merge `dev` down. Verification in
the meantime runs the vignette's chunks against the wt-245 package with
`coral.rda` copied in — that tree has both halves and neither branch is
polluted.

Useful accident worth recording: `(nec | colony)` and `bnec_group()` *do* fit
without the #245 fix, and `ogl`/`pgl` do not. That is not luck about the code —
`nec` is on the predictor scale, which spans thousands of ug/L here, so the
`brms` default `student_t(3, 0, 2.5)` offset is negligible against it. The
defect bites only where the group term sits on a **bounded, response-scaled**
parameter. Worth stating in #245: the severity depends on the parameter's scale,
not just on the family.

**The hurdle is not a sampler-tuning problem.** The *ungrouped* joint fit is
also divergence-heavy — 713 of 4000 at `adapt_delta = 0.95`, R-hat 1.003, NEC
320 [100, 682]. So part 3's difficulty is the data, not the group term. Re-run
in progress at 0.99 for both arms against the fixed package. If neither is
clean, part 3 becomes a section that explains the joint structure and shows,
with its diagnostics, that these data do not support a grouped hurdle — with a
plain statement to RF that a *working* grouped hurdle example needs different
data. Do not ship a non-converged fit as a worked example.

**A verification tree, and a merge conflict PR 250 will hit.** Neither existing
worktree can run the whole vignette: `wt-245` (dev + the fix) has no
`bnec_group()`, which is #33's and lives on PR 227; `bayesnec-stack` (the
vignette branch) has `bnec_group()` but not the fix. So all chunks *were*
verified, but across two trees.

`/mnt/c/Rworking/wt-verify` is a detached throwaway worktree off
`issue-6-33-grouping-vignette` with the fix's `R/` diff applied, for one
end-to-end run. **Delete it when done** — nothing is meant to be committed from
it.

Applying that diff surfaced the conflict PR 250 will hit when it merges down,
and it is worth knowing in advance because it is trivial and easy to resolve
wrongly: `R/amend.R`. The vignette branch carries #136's rate-denominator block
before the `define_prior()` call; the fix adds a `group_spec` argument to that
same call. **Both sides are wanted** — keep the denominator block and add the
argument. Every other `R/` file applied cleanly.

**Part 3 settled on evidence, 2026-08-24.** `hurdle_gamma` on the seven-day
growth, `adapt_delta = 0.99`, 4000 iterations, against the fixed package:

| | NEC | max R-hat | min ESS | divergences |
|---|---|---|---|---|
| ungrouped | 314 [110, 689] | 1.01 | 240 | 229 |
| **+ `ogl(chamber)`** | 515 [123, 1091] | **1.238** | **19** | 124 |

The grouped fit has **not converged** and cannot be shipped as a worked example.
The ungrouped one has converged but keeps divergent transitions that
`adapt_delta` reduces without removing.

So part 3 is now: fit the joint model ungrouped and report it with its caveats;
show the grouped attempt as an **unevaluated** chunk with its measured
diagnostics quoted and the settings stated; explain from the data why it fails
(the mu block sees five of eight concentrations); and draw the same
compare-against-the-ungrouped-fit lesson part 1 draws. The grouped chunk is left
unevaluated deliberately — running a 4000-iteration fit at precompile time to
demonstrate a negative result is not worth the hours, and the numbers quoted are
real, from the run recorded in this table.

**This is a partial answer to what RF asked for.** RF's agreed structure had
part 3 "applying it to the more complex joint structure", and suggested a fitted
grouping model with `hurdle_gamma`. That specific thing is not deliverable on
these data — say so plainly in the PR rather than letting the section read as
though it were the plan all along. A working grouped-hurdle example needs a
dataset whose non-zero response spans more of the concentration series.

## Run complete, 2026-08-24

**Phase 4 and 5 done.** Commit `296c2456` on `issue-6-33-grouping-vignette`,
pushed; PR 228 body rewritten. Every chunk executed in a tree carrying both the
branch and #250 — 14 fitting/reporting chunks and 18 non-fitting ones, all pass.
The throwaway worktree `wt-verify` and the copied `coral.rda` in `wt-245` are
removed.

**The composed closing section fits cleanly**, which is what let the vignette
end on the composition rather than an unbacked claim:

| level | NEC | max R-hat | divergences | sd(chamber) |
|---|---|---|---|---|
| PAR | 978 [746, 1136] | 1.01 | 12 | 0.013 |
| UV | 822 [660, 988] | 1.003 | 0 | 0.009 |

**Left for RF, all stated in the PR bodies rather than buried here:**

1. **AIMS permission** to redistribute `coral` is unconfirmed. Blocking for
   merge, and not something to assume from the authors being AIMS staff.
2. **`example_fi.csv` was not shipped**, against what RF asked for. `coral`'s
   `light` covers the factor, and shipping a second published dataset is a
   second redistribution decision. Easy to add if RF wants it.
3. **Part 3 is a negative result**, not the fitted grouped hurdle the plan
   wanted. The data cannot support one.
4. **Merge order is now constrained**: #250 → `dev` → this branch. Watch the
   `R/amend.R` conflict — both sides are wanted.
