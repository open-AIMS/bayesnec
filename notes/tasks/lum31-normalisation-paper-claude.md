# Specification — normalisation, grouping and model averaging

Companion to `lum31-normalisation-paper-human.md`. Written 2026-09-11. Every
measurement below was made while preparing `vignette("example8")` and is
recorded, with the settings that produced it, in
`prompts/grouping-vignette-dataset.md`. Nothing here is settled science yet:
section 5 lists what would have to be true for each claim to stand.

---

## 1. Provenance of the measurements

All fits: `bayesnec` at `dev` `eebccdb3` with PR #304 merged locally, R 4.6.1,
rstan, `Gamma(link = "identity")`, `crf(log(conc), "all")` (15 equations
retained of 23), 4 chains, `iter = 8000`, `warmup = 4000`, `seed = 228`,
`adapt_delta = 0.99`. Model averaging is pseudo-BMA, which is `bayesnec`'s
default and the only method `bnec_group()` permits
(`R/helpers.R:626`, `R/bnec_group.R:145`).

Data: `lum31`, the acute copper and zinc Lum-31 assays of Luter et al. (2025),
built in this work from the AIMS repository workbooks. Primary arm is zinc at
15 minutes, 748 rows over 17 plates.

Fits are held at `cache/section-fits/` (git-ignored).

---

## 2. Result one — normalisation is unavailable under per-read gain

### 2.1 The distinction that matters

A divisor that is one constant for the whole dataset is a change of units. It
induces no correlation between observations and no estimate enters it. A divisor
computed per curve — a control mean, a curve maximum — is an estimate, shared by
every observation of that curve, and Ritz et al. (2026) Table 4 measures what it
does: ED10 bias 2.6 to 6.8 per cent and coverage 0.88 to 0.91 against a nominal
0.95, worst where few control measurements are used. The Lum-31 published
analysis divides by four control wells per plate, which is that configuration.

### 2.2 Why the global divisor does not substitute

The benefit of normalisation is not the divisor. It is that the control lands
near the top of a bounded scale, where a Beta variance `mu(1 - mu)/(1 + phi)`
is small, so the upper asymptote is well determined and any effect concentration
measured from it is stable. That requires every curve's control to land there,
which a global divisor achieves only if the curves share a scale.

Measured: the largest reading in `lum31` is 1,521,059 RLU. Dividing the zinc
15-minute arm by it puts plate control medians at 0.171, 0.212, 0.261, 0.285,
0.302, 0.313, 0.439, 0.440, 0.448, 0.513, 0.523, 0.534, 0.576, 0.604, 0.608,
0.730, 0.898 — fifteen of seventeen between 0.17 and 0.61, where the Beta
variance is at its largest.

### 2.3 Why the curves do not share a scale

The plate reader applies auto-scale gain adjustment, set per read. Control-well
median luminescence differs up to 2.8-fold between plates read on the same day
and 4.6-fold between the March and October batches. Specification §5 of the
vignette plan records that no universal ceiling in RLU exists for the same
reason.

### 2.4 What the group-level term does instead

Gain is a per-plate multiplicative constant. `ogl(plate)` multiplies the fitted
value; `(top | plate)` multiplies `top` alone, which coincides with gain only
where the equation has no lower asymptote. Generated formulas are in vignette
specification §2.5.

---

## 3. Result two — a low ECx is not robust to model-set composition

### 3.1 The measurement

Per-equation values, ungrouped arm, back-transformed with `exp`, mg/L:

| equation | weight | `top` | EC10 | EC50 | EC50/EC10 |
|---|---|---|---|---|---|
| ecxll5 | 0.403 | 727,700 | 0.397 | 1.576 | 3.97 |
| ecxll3 | 0.232 | 694,600 | 0.708 | 1.794 | 2.54 |
| ecxhormebc4 | 0.230 | 695,500 | 0.709 | 1.794 | 2.53 |
| ecx4param | 0.060 | 691,300 | 0.746 | 1.840 | 2.47 |
| ecxll4 | 0.056 | 691,100 | 0.746 | 1.840 | 2.47 |
| ecxwb1 | 0.018 | 780,200 | 0.176 | 1.294 | 7.37 |
| nec3param, nec4param, nechorme, nechorme4 | 0.000 | ~662,000 | 1.61--1.64 | 2.12--2.13 | 1.30 |
| ecxexp | 0.000 | 238,200 | 0.018 | 0.075 | 4.12 |

Among the weighted equations EC50 spans 1.576 to 1.840, 17 per cent. EC10 spans
0.176 to 0.746, a factor of four. Across the retained set EC10 spans ninety-fold.

### 3.2 The consequence, measured

Model-averaged estimates across the three plate structures, mg/L:

| quantity | ungrouped | `(top \| plate)` | `pgl(plate)` |
|---|---|---|---|
| N(S)EC | 0.471 (0.148--0.737) | 0.842 (0.323--1.309) | 0.680 (0.187--1.202) |
| EC10 | 0.662 (0.228--0.811) | 0.456 (0.321--0.768) | 0.287 (0.216--0.449) |
| EC50 | 1.734 (1.343--1.947) | 1.620 (1.436--1.853) | 1.561 (1.365--1.915) |

The weight on `ecxll5`, whose EC10 is the lowest of the well-weighted equations
at 0.397, rises from 0.403 to 0.830 between the first two. `ecxwb1`, EC10 0.176,
takes 0.692 in the third. EC10 tracks the weights; EC50 does not.

### 3.3 What is not yet established

Whether each equation's own EC10 is stable across the three arms, which
separates weight reallocation from a change in fitted shape. The per-equation
decomposition has been run for all three arms and only the ungrouped table has
been read.

---

## 4. Result three — the variance at the control is misstated

Observed within-cell coefficient of variation, zinc 15 minutes, 17 plates of
four wells, by concentration rank: 0.051, 0.055, 0.056, 0.042, 0.028, 0.058,
0.057, 0.082, 0.117, 0.201, 0.362 (medians). Control cells alone: 0.018 to
0.102, median 0.051, implying a Gamma shape near 391.

Fitted shape, and the coefficient of variation it implies, essentially identical
across equations within an arm:

| arm | shape | implied CV |
|---|---|---|
| ungrouped | 3.0 | 0.58 |
| `(top \| plate)` | 5.4 | 0.43 |
| `pgl(plate)` | 14.4 | 0.26 |

Two causes. Without a plate term the residual absorbs the 2.8-fold gain spread,
which is why the shape rises from 3.0 to 14.4 as structure is added. Beyond
that, one coefficient of variation cannot serve a series whose own runs 0.051 to
0.362, and the compromise is dominated by the low end.

`nsec()` is defined against the control's 0.01 quantile, so an overstated
control variance inflates and destabilises it directly. ECx is measured from the
modelled control mean since #281 and so does not depend on the variance
directly, but the variance model determines the relative fit of the equations
and therefore the weights, and by §3 the weights determine EC10.

The remedy available in `bayesnec` is a dispersion sub-model:
`disp("power")` sets `log(shape) = c0 + c1 * (log(mu) - log(m))`, a variance
function of the fitted mean; `disp(~log(conc))` is the descriptive alternative
(`R/bayesnecformula.R:98`). A two-chain screen on `ecxll5` with `(top | plate)`
is running. Baseline: shape 5.461 (4.926--6.025), EC10 0.436 (0.315--0.588).

---

## 5. What each claim requires before it can be published

**Result one** needs no further fitting. It is arithmetic on the design plus the
Ritz result, and the gain measurement is from the paper's own methods.

**Result two** needs the per-equation decomposition read for all three arms, and
the same decomposition on a second design with a different family — `alga`
(Beta or Gamma, censored) and `nassarius` (`hurdle_gamma`) are candidates
already in the package. Without a second design it is an observation on one
dataset.

**Result three** needs the dispersion screen to show that a sub-model brings the
implied control CV to the observed 0.05, and then a full fit to show what it
does to EC10 and the N(S)EC. If it does not, the conclusion changes from "the
variance model is wrong and here is the fix" to "the variance model is wrong and
Gamma may be the wrong family for this response", which is a different paper.

---

## 6. Rejected framings

**A paper about `bayesnec`.** The results are about concentration-response
practice and hold for any engine. Presenting them as package documentation
narrows the audience and invites the objection that they are artefacts of one
implementation.

**Reusing the vignette text.** The vignette teaches the syntax and shows the
case study. The paper argues three claims and must state the limits of each.
Sharing figures is sensible; sharing prose is not.

**Folding it into #228.** The vignette ships with the package and is bound to
its release cycle. The paper is not, and the evidence it needs — a second design
— is outside the vignette's scope.
