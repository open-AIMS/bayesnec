# What a dispersion sub-model does to `example7`'s conclusions

Written 2026-08-25, during review of #243. Companion to `disp_validation.md`,
which records the `disp()` work done while `example7` was being built, and to
`censoring_at_zero_impact.md`.

`example7` reports a simulation in which the residual error is *generated*
heteroscedastic and *fitted* homoscedastic — the residual scale rises 8.1-fold
from the control to the lower asymptote, and every arm fits a single pooled
`sigma`. The vignette's Limitations section called that a shared
misspecification and said there was no reason to expect it to change the
ordering of the approaches.

**That is true for the flooring arms and false for B2.** This note records the
check.

## Why it was worth checking

The vignette's account of the NSEC runs through the residual scale:

> At high noise the residual scale dominates the NSEC, because the reference
> quantile is set by the width of the control posterior. Flooring removes the
> most extreme low values, compressing the residual scale, which raises the
> reference and pulls the crossing to the left.

That channel exists only because `sigma` is **global**. Under a dispersion
sub-model the control-region scale is estimated where the control data are, and
flooring at high concentration has removed nothing there — so the channel should
close. The same argument applies to B2, whose inflated `sigma` is a global
response to observations the pinned curve cannot reach.

## Design

Scenario 8 only (`delta` = 4, `top_factor` = 2.0, `R` = 2.3, control CV 9.6%) —
the reference cell of the vignette's precision sweep. 50 simulated datasets,
each fitted by the six Gaussian arms twice: once homoscedastic, once with
`disp("loglinear")`. Paired, so every contrast is within-dataset. 600 fits,
0 failures.

`disp("loglinear")` rather than `disp("power")` because the response crosses
zero and a power law in `mu` is undefined there; `bnec()` refuses that
combination and points at `"loglinear"`. Note that `"loglinear"` is itself an
approximation to what `sim_sigma()` generates: the simulation makes `sigma`
*linear* in the mean, `"loglinear"` fits it *log-linear*, which over this range
is a max relative error of ~60% and an RMS of ~15% — against a pooled-`sigma`
fit that is wrong by 5x at the control.

Script: `notes/scripts/disp_sensitivity_sweep.R`.
Full output: `notes/disp_sensitivity_results.csv` (600 rows).

## Validation before reading anything else

The homoscedastic half reproduces the compendium's scenario-8 contrasts: NSEC
ratio to A of 1.49 for B2 (published 1.462) and 1.05 for B3 (1.014), and the
ErC50 bias ordering A < C < D < B1 < B3 < B2 exactly as published. Absolute
biases sit ~2.3 points lower than the published values across *every* arm, which
is the shared idiosyncrasy of a 50-dataset sample showing up in a paired design;
the between-arm contrasts, which is what this note is about, reproduce.

## Result

ErC50 relative bias and 95% coverage against a true 5.0533; NSEC as a ratio to
arm A; divergent transitions per fit. Left value homoscedastic, right value
under `disp("loglinear")`.

| arm | bias | bias `disp` | cov | cov `disp` | NSEC/A | NSEC/A `disp` | div | div `disp` |
|---|---|---|---|---|---|---|---|---|
| A  | -2.6 | -4.7 | 0.90 | 0.78 | 1.00 | 1.00 | 0.00 | 0.00 |
| C  | -4.1 | -5.5 | 0.78 | 0.78 | 0.91 | 1.02 | 0.00 | 0.40 |
| D  | -5.5 | -5.0 | 0.80 | 0.88 | 0.93 | 1.03 | 0.12 | 1.80 |
| B1 | -9.0 | -8.5 | 0.48 | 0.50 | 1.01 | 1.21 | 0.00 | 4.10 |
| B2 | **-12.7** | **-1.2** | 0.92 | 1.00 | **1.49** | **1.06** | 3.22 | 0.12 |
| B3 | -10.7 | -9.8 | 0.34 | 0.44 | 1.05 | 1.28 | 0.00 | 13.80 |

### 1. Flooring survives, and the vignette's headline claim is unaffected

B1 moves -9.0 to -8.5 and B3 -10.7 to -9.8; coverage stays at 0.50 and 0.44.
Flooring damages the mean structure and a dispersion model does not touch it.
**"Do not floor" stands on its own.**

### 2. B2's failure is largely an artefact of the pooled `sigma`

Bias -12.7% to **-1.2%**, NSEC inflation 1.49 to **1.06**, and the sampling
pathology goes with it: 3.22 divergent transitions per fit to **0.12**. All
three of B2's symptoms in the vignette have the same cause, and it is not the
pinned asymptote — it is one `sigma` being asked to cover both the control
scatter and observations the curve cannot reach. Give the model a dispersion
sub-model and pinning `bot` on intact data is close to unbiased.

This is the finding that reaches the vignette's text. B2's -11.4% ErC50 and its
134% NSEC overestimate are quoted as headline results.

### 3. The B1/B3 NSEC contrast flips direction, as predicted

1.01 to 1.21 and 1.05 to 1.28. Under a global `sigma` the flooring arms sit
level with A on the NSEC; under a dispersion sub-model they sit clearly above
it. The compression channel closes exactly as the mechanism predicts, and the
vignette's *Direction of the B1 contrast* section is therefore a statement about
homoscedastic fitting rather than about flooring as such.

### 4. Unanticipated: floored data plus a dispersion sub-model samples badly

B3 draws **13.8** divergent transitions per fit under `disp` and B1 4.1, against
0.00 for both when homoscedastic. The direction of the sampling problem reverses
along with everything else: homoscedastic, B2 is the arm that struggles; with a
dispersion sub-model, the floored arms are. Not chased further here.

## What this does not establish

One scenario, one noise level, n = 50, and a `"loglinear"` fit to a linear
`sigma` ramp. It says the ordering is dispersion-dependent for B2; it does not
re-score the study. Re-running the full precision sweep under `disp("loglinear")`
is the way to settle it, and belongs in the compendium rather than here.
