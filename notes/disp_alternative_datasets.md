# Is there a `disp()` motivating case without a censoring confound?

Written 2026-08-12. `alga` turned out to be a poor motivating case: censoring at
zero removes almost all of its apparent heteroscedasticity
(`censoring_at_zero_impact.md`), and no model in the package fits its mean well
(`disp_validation.md`). This records a search for a replacement, and the answer
to whether the feature should be kept at all.

## Method

**Screen (model-free).** Every dataset in the package and in `ignore/` was
screened for a within-dose mean-variance relationship — replicate SD computed
inside each dose, so no fitted curve is involved and no lack of fit can inflate
it. 24 dataset x group combinations were evaluated; most were excluded for
inadequate replication, a family with no free dispersion parameter
(survival/binomial), or the censoring confound that spoiled `alga`.

**Fit.** For each shortlisted dataset, four candidate mean models were fitted on
`log(dose)` and the best chosen by `elpd_loo`, recording **lack of fit in units
of the within-dose replicate SD**. Then constant dispersion was compared against
the available variance functions and against a predictor-based sub-model. 19
datasets, roughly 130 fits.

## Result: one clear case, and it is already in the package

**`nassarius` growth, contaminant C, gaussian, positive values only (n = 318).**

| sub-model | elpd gain over constant | `c1` |
|---|---|---|
| `disp("power")` | **56.2 (se 9.3)** | 1.460 [1.139, 1.832] |
| `disp("loglinear")` | 54.6 (se 9.5) | 0.101 [0.077, 0.134] |
| `disp(~log(dose))` | 51.5 (se 9.7) | — |

Six times its own standard error, and the mean model fits to within **0.5
replicate standard deviations** at every dose — against `alga`'s 4.5 to 6. So
this is not lack-of-fit absorption, which is the failure mode that made `alga`
uninterpretable. Replication is 21-36 per dose across 13 doses. Two
non-positive values out of 320, both dropped, so there is no censoring or
flooring issue.

`c1 = 1.46` means `sd` proportional to `mu^1.46`. That matters: constant CV,
which is what a `Gamma` assumes for free, is `mu^1.0`. The relationship is
steeper than any standard family's built-in assumption, which is exactly the
circumstance that justifies a variance function rather than a change of family.

Supporting evidence in the same direction from the other contaminants, though
weaker:

| test | n | `disp("power")` gain | `c1` | mean lack of fit |
|---|---|---|---|---|
| C (gaussian) | 318 | **56.2 (9.3)** | 1.460 [1.139, 1.832] | 0.48 SD |
| D (gaussian) | 327 | 6.6 (3.7) | 0.624 [0.321, 0.923] | 0.97 SD |
| B (gaussian) | 63 | 4.1 (3.3) | 0.782 [0.327, 1.222] | — |
| A (gaussian) | 164 | −11.2 (5.2) | −0.519 [−2.429, 0.362] | — |

A is unusable — R-hat 1.08, ESS 31-40 — and should not be read as evidence
either way. B and D point the same way as C at around 2 SE.

## Everything else was negative or unusable

| dataset | family | outcome |
|---|---|---|
| `herbicide`, 7 herbicides | Beta | route B not validly tested (wrong link supplied). Route A: ametryn **21.9 (5.2)**; the other six within noise |
| coral colour proportion, 3 climates | Beta | −1.4, −1.4, −1.1 — no signal |
| T7 PAM yield, 3 climates | Beta | 15.6 (10.1), 9.9 (5.4), 8.0 (5.6) — 1.4 to 1.8 SE, and 8/19/36 zeros dropped, so confounded |
| `nassarius` growth B, D | Gamma | no gain; and see the defect note — Gamma results are not trustworthy |
| `nec_data` | — | simulated from `abs(rnorm(100, 1))`, no replication within dose, cannot be screened model-free |

The one non-`nassarius` positive is **herbicide ametryn under route A**,
21.9 (5.2). Worth keeping in mind as a route-A example, but it is a single
herbicide out of seven and 12 of its 96 values are rounded zeros.

## Correction: the Beta results below are invalid

**2026-08-12.** The Beta datasets were run with `family = Beta()`, which takes
the logit link, because I specified the family explicitly instead of letting
`bnec()` auto-select it (auto-selection forces identity). Route B assumes the
curve expression is the mean, which holds only under identity, so those arms
failed for the wrong reason. See `disp_link_scale_defect.md`. The route-A arms
are unaffected. The Beta datasets need re-running under an identity link before
anything can be concluded about them, and `disp("twosided")` is now known to
work under identity.

The `nassarius` **gaussian** results are unaffected -- identity is gaussian's
only link -- so the headline finding below stands. The two `nassarius` **Gamma**
rows are affected and should be ignored.

## The defect this search uncovered

Route B substitutes the model's curve expression into the variance function on
the assumption that it is the mean. That holds under an identity link only.
`bnec()` forces identity when it auto-selects the family, but an explicitly
supplied family keeps the link the user gave it, and nothing checks. Under a
logit link route B fails loudly; under an inverse link it **runs and returns the
slope with the wrong sign**. Full write-up in `disp_link_scale_defect.md`.

The practical consequence for the table above is that the Beta and T7 PAM rows
test route A only, and must be re-run under identity before their route-B arms
mean anything.

## Recommendation

**Do not remove the feature.** The evidence for keeping it:

* One dataset shows a large, well-identified relationship with an adequate mean
  model and no censoring confound, and it is already shipped and already used in
  `example6`.
* That relationship is steeper than the nearest family's built-in assumption, so
  it cannot be absorbed by choosing `Gamma` instead.
* Route A works across families and gives a second, independent positive case.
* The exercise has produced three real defects — uncentred covariate, random
  initialisation, and now the link scale — all found by using the feature on
  real data.

But three things follow:

1. **Add the missing link check before anything else**, and re-run the Beta
   datasets under identity. Until then a user who supplies `family = Gamma()`
   gets a confidently wrong slope with no warning.
2. **Replace `alga` with `nassarius` as the motivating case** for the dispersion
   material. `alga` remains the right dataset for the censoring and
   non-normalisation arguments, which is most of `example7`; only the dispersion
   section needs to move.
3. **Temper the claim.** Of 19 datasets screened with adequate replication, one
   shows a compelling relationship and two more are suggestive. The honest
   framing is that non-constant dispersion is a condition to be diagnosed and
   occasionally corrected, not a routine feature of concentration-response data.


---

# Final verdict (2026-08-12, after the identity-link rerun)

Everything above that predates this section was run with explicitly supplied
families and therefore, for Beta and Gamma, the wrong link. The corrected run
covers 19 datasets under identity. **No dataset shows a dispersion relationship
that survives its own confound.**

Three candidates looked compelling and all three collapsed when tested:

| candidate | apparent gain | after removing the confound | confound |
|---|---|---|---|
| `alga` sgr, `c_proliferum` x A | 60.6 (16.2) | **4.0 (4.1)** | censoring at zero |
| `nassarius` growth C, gaussian | 56.2 (9.3) | **4.0 (3.6)** | family: Gamma baseline is 51 elpd better |
| `herbicide` ametryn, Beta | 18.0 (6.0) | **3.8 (3.5)** | 12 rounded zeros of 96 |

The best surviving result anywhere is `herbicide` tebuthiuron at 7.7 (5.1), about
1.5 standard errors, on a dataset with no zeros. T7 PAM yield is stronger and
replicates consistently across three climates (c1 between 0.88 and 1.16), but
drops 8, 19 and 36 zeros respectively, and is not distributable in any case.

## Three ways an apparent dispersion signal turns out to be something else

This is the durable product of the exercise, and it is worth more than the
feature:

1. **Censoring or flooring at a boundary.** Substituted or floored values pile up
   at one end of the series, and their spread is read as heteroscedasticity.
   Test: refit with the affected rows censored or dropped.
2. **A misspecified family.** A variance function under the wrong family recovers
   what the right family gives for free. `nassarius` C: gaussian + `disp("power")`
   gains 56.2, but plain `Gamma` beats plain gaussian by 51 and leaves only 4.0
   for the variance function. Test: compare baseline `elpd_loo` across families
   before adding any dispersion term.
3. **Lack of fit in the mean.** Residuals from a systematically wrong curve are
   large wherever it is wrong, and a variance function will call that region
   noisy. Test: express the misfit at each dose in units of the within-dose
   replicate SD.

## Recommendation

**Keep the feature, drop the claim.** Do not present `disp()` as a recommended
addition to a concentration-response analysis, because the evidence does not
support that. Present it as a **diagnostic**: the tool you use to test whether
non-constant dispersion is real, together with the three checks above, whose
usual answer on real data is that it is not.

That reframes `example7` productively. `alga` stays as the case study for
censoring and non-normalisation, where it is genuinely strong. The dispersion
section becomes a worked demonstration that an apparent 60-point `loo` gain was
an artefact of an unmade censoring decision -- which is a more useful thing to
teach than a recipe for adding variance functions.

The feature itself is sound: the simulation recovery tests pass, and using it on
real data is what exposed four defects (uncentred covariate, random
initialisation, the missing link check, and `loo_compare` row naming in my own
harness). It earns its place as an instrument. It has not earned a
recommendation.
