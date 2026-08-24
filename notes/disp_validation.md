# Validating `disp()` on real data

Written 2026-08-11, while building `example7` against #191/#192/#193. Records
what the exploratory fitting found, including two defects it exposed in the
implementation and one methodological trap that affects any use of the feature.

Scripts and fits are in the session scratchpad, not the repo — the fit objects
run to 1.8 GB. Everything below is reproducible from `data(alga)` plus the
recipe in each section.

## Two defects the exercise found and fixed

### 1. The variance-function covariate was uncentred

`"power"` was `log(dpar) = c0 + c1 * log(mu)`, so `c0` was the dispersion
parameter at `mu = 1`; `"loglinear"` was `c0 + c1 * mu`, so `c0` was its value at
`mu = 0`. Neither point is near the data unless the response happens to be of
order one.

Fitting algal cell **density** (`mu ~ 1.8e4`, so `log(mu) ~ 9.8`) gave:

| | uncentred |
|---|---|
| `cor(c0, c1)` posterior | **0.9906** |
| implied CV at the data | 2e5 – 2e6 (observed 0.03 – 0.62) |
| sign of `c1` | wrong |

The induced prior on `log(dpar)` at the data was `normal(2, 19.7)` — flat over
seventeen orders of magnitude. The `sgr` fits escaped only because `sgr ~ 0.12`
happens to sit near the uncentred origin.

Fixed by centring on a reference computed from the response (median, or
geometric median for the log forms) and splicing it in as a literal constant.
`cor(c0, c1)` on the regression test fell from 0.99 to **0.121**.

### 2. Dispersion parameters were left to Stan's random init

They had been filtered out of `make_good_inits()` deliberately — they play no
part in getting the mean curve inside the response range — and the comment
claimed Stan's default draw was "benign on a log link". It is not. The sign of a
slope is tied to the direction of the mean curve, so a chain started in the
mirror-image basin converges to an inverted solution.

On the `"loglinear"` regression test, under the default `rstan` backend:

| | R-hat | `c1` (true −5) |
|---|---|---|
| random init | **1.85** | +4.9, 97.5% = 32.4 |
| seeded at slope = 0 | **1.001** | −4.81, 95% [−5.45, −4.25] |

Fixed by `disp_inits()`, which seeds every slope at zero — the
constant-dispersion null — and `c0` at its prior centre.

## The methodological trap: lack of fit is absorbed by dispersion

**This is the most important finding and it belongs in the vignette, not just
here.** A variance function is free to explain a systematically wrong mean by
declaring that region noisy.

Two ways it bit during this exercise:

1. **Predictor scale.** `alga` doses are near log-spaced. On a raw linear axis
   every equation misfits badly; on `log(dose)` the same equation is far better:

   | | max &#124;obs − fit&#124; on `sgr` | elpd |
   |---|---|---|
   | `crf(dose, "nec4param")` | 0.2115 | 96.0 |
   | `crf(logdose, "nec4param")` | **0.0676** | **148.8** |

   The choice of *equation* barely matters by comparison — on `log(dose)`,
   `nec4param`, `ecx4param`, `ecxwb1`, `ecxll4` and `nechorme4` all land within
   1.2 elpd of one another.

2. **Residual misfit even at the best scale.** `c_proliferum × A` declines
   gradually from dose 2.5 to 10 and then falls off a cliff. No model in the
   package follows that shape, and **model averaging does not help**: fitting
   `model = "all"` leaves the same `max |obs − fit| = 0.0679`, which at the
   shoulder is 5.5 and 6.2 *replicate standard deviations*.

So some of the fitted `c1` on these data is misspecification, not measurement
noise. The heteroscedasticity is nonetheless real, and the way to establish that
is model-free — see below.

## Model-free evidence: within-dose replicate SD

Computed inside each dose, so no curve enters and no misfit can inflate it.

| group | min SD | max SD | ratio |
|---|---|---|---|
| `c_proliferum` × A | 0.0046 | 0.0923 | 20.0 |
| `c_proliferum` × B | 0.0034 | 0.0122 | 3.6 |
| `r_salina` × A | 0.0215 | 1.0319 | 48.1 |
| `r_salina` × B | 0.0160 | 0.5130 | 32.1 |

(#193's table reports 9.8 and 11.6 for the `r_salina` rows; those exclude the
substituted-zero rows, which is why they are lower.)

## Does `disp()` work on the four `alga` tests?

Yes, on the `sgr` scale, in all four — fitted on `log(dose)`, `nec4param`,
`gaussian()`. `loo` differences relative to the best:

| group | `disp("loglinear")` | `disp(~logdose)` | constant |
|---|---|---|---|
| `c_proliferum` × A | **0.0** | −19.1 (8.6) | −60.6 (16.2) |
| `r_salina` × A | **0.0** | −49.7 (7.5) | −130.8 (13.6) |
| `c_proliferum` × B | **0.0** | −16.3 (7.6) | −31.7 (8.8) |
| `r_salina` × B | **0.0** | −8.0 (4.9) | −42.2 (8.0) |

Route B beats route A in every group, and both beat constant dispersion. That is
the first empirical support for the route A / route B distinction being worth
having rather than merely conceptual.

`c1` is negative and excludes zero everywhere, as the delta-method argument
predicts:

| group | `c1` | 95% |
|---|---|---|
| `c_proliferum` × A | −16.32 | [−23.20, −11.57] |
| `c_proliferum` × B | −10.59 | [−14.71, −7.61] |
| `r_salina` × A | −1.62 | [−2.09, −1.31] |
| `r_salina` × B | −1.30 | [−1.64, −1.01] |

The two species differ tenfold, which is a **consistency check rather than a
discrepancy**: `c1` for `"loglinear"` carries units of `1/response`, and
`r_salina`'s growth rates are about twelve times larger than `c_proliferum`'s.
The ratio of the slopes (≈10) tracks the ratio of the scales (≈12).

Note also that the **constant-dispersion** fits are the ones that struggle to
sample — `c_proliferum × A` gives R-hat 1.166 with a bulk ESS of 16. Forcing one
`sigma` onto a twenty-fold spread is hard on the sampler, which is itself a
signal.

## The substituted zeros manufacture heteroscedasticity

Refitting `r_salina` with `sgr_source == "substituted"` rows removed collapses
the dispersion signal:

| group | constant vs best, as supplied | measured only |
|---|---|---|
| `r_salina` × A | −130.8 | **−19.0** |
| `r_salina` × B | −42.2 | **−8.6** |

So roughly 85% of the apparent dispersion signal in `r_salina × A` is an
artefact of substituting zero for below-limit growth. This ties sections 2 and 5
of the vignette together: fix the censoring first, and the dispersion story
shrinks but survives. On `r_salina × B` measured-only, route A actually wins,
which is a further reason not to reach for route B reflexively.

## The density scale is not a good demonstration

> **Correction, 2026-08-12.** Everything in this section is superseded by
> `disp_link_scale_defect.md`. These fits used `Gamma`, whose link under
> `bnec()` is **inverse**, not identity, so `log(<curve>)` in the variance
> function computed `-log(mu)` and the slope was estimated with the wrong sign.
> The convergence problems recorded below are real, and the lack of fit in the
> mean is real, but the inverted and implausible `c1` estimates were the defect,
> not the data. Nothing here should be cited until route B is fixed and these
> fits are re-run. The `sgr` results elsewhere in this note use `gaussian` with
> an identity link and are unaffected.

`Gamma` + `disp("power")` on `density` was tried for all four groups. Two fits
are unusable — `c_proliferum × B` gives **R-hat 1.736, ESS 6**, and
`r_salina × A` produces 671 and 1334 divergent transitions out of 4800. No
conclusion should be drawn from those, and in particular the `loo` result
favouring constant dispersion for `c_proliferum × B` is a non-convergent fit,
not evidence.

The mean-model problem above is the likely reason: on the density scale the
shoulder misfit is far larger in absolute terms. Keep the density scale in the
vignette as the *conceptual* point from TG 201 (`sgr` and `density` are one
measurement, so choosing between them is choosing a variance model) and do not
build a fitted demonstration on it.

## Independent check: `ignore/All data collated_Dissolved toxicants only_revised.csv`

Unpublished multi-toxicant dataset, 5 toxicants × 8 species, 4 endpoints. Not
distributed, and **nothing identifying it may appear in the vignette**. Used
here only to ask whether `disp()` is worth reaching for routinely.

Within-dose SD ratios are much smaller than `alga`'s:

| endpoint | n tests | median ratio | max |
|---|---|---|---|
| cell yield (4 algal spp.) | 20 | 3.45 | 7.8 |
| number of eggs | 5 | 4.7 | 13.6 |
| number of young | 10 | 1.95 | 9.1 |
| population growth rate (*Hydra*) | 5 | 2.2 | 3.3 |

Fitted `nec4param` on `log(conc)`, constant vs `disp("power")` for cell yield
and `disp("loglinear")` for growth rate:

* **Cell yield — no test benefits.** Every `elpd` difference is within about two
  standard errors of zero, in both directions, across all toxicants and species.
  `c1` excludes zero in 1 of 9 reported.
* **Population growth rate — one of four does.** *Hydra* × beryllium gives
  +78.4 (28.0), and the rest are within noise. That one fit needed a fallback to
  Stan's default initialisation and an earlier attempt returned R-hat 3.98, so
  treat it as suggestive rather than settled.

**Verdict: the feature is not valuable for this dataset.** That is a useful
negative — it says `disp()` is a response to a diagnosed problem, not a default.
The honest discriminator is not a single threshold on the SD ratio (`alga`'s
`c_proliferum × B` has a ratio of 3.6, comparable to these tests, and still
gains 31.7 elpd) but the combination of a large spread *and* a response that
collapses toward and through zero.

## What to do next

* Re-check the `"power"` form on a positive response that has a well-fitting
  mean model — none of the density-scale fits here qualified, so the flagship
  form is still under-validated on real data. Simulation covers it; real data
  do not yet.
* Consider whether `bnec()` should warn when a `disp()` fit's mean curve misses
  by more than a few replicate standard deviations at any dose, since that is
  the condition under which the fitted variance function is not interpretable.
