# Detecting a response normalised to an estimated quantity

Evidence behind `check_normalisation()` and `on_rational_grid()` in
`R/check_data.R`. Issue
[#173](https://github.com/open-AIMS/bayesnec/issues/173) item 3 asked for this
to be "prototyped against real datasets before being adopted", on the grounds
that "genuine proportion data often has many observations at `1`, so the check
must be narrow enough not to cry wolf". This is that prototyping.

## What is being detected

Two pre-processing practices leave an exact arithmetic trace.

**A. Divided by the observed maximum.** `v / max(v)` evaluates to exactly `1`
at the maximum and nowhere else, so `max(y) == 1` with exactly one observation
attaining it. Exact floating-point equality is the right test here, not a
tolerance: `x / x` is exactly `1` by IEEE 754 for any finite non-zero `x`.

**B. Divided by the control mean.** `mean(y[x == min(x)])` is `1` to within
accumulated rounding. Tested at `1e-8`.

## The problem with signature A on its own

Genuine proportion data derived from counts fires signature A whenever exactly
one replicate records every individual as alive, or every egg as fertilised.
That is not rare. Simulating a declining response over five concentrations with
six replicates, 2000 draws per scenario:

| genuine data | naive A hit rate |
|---|---|
| count proportion `k/n`, `n = 5` | 0.002 |
| count proportion `k/n`, `n = 10` | **0.148** |
| count proportion `k/n`, `n = 20` | **0.380** |
| count proportion `k/n`, `n = 30` | **0.218** |
| count proportion `k/n`, `n = 50` | 0.026 |
| continuous proportion (Beta) | 0.000 |
| positive continuous (Gamma) | 0.000 |
| proportion rounded to 2 dp | 0.027 |

A check that cries wolf on 38% of ordinary fertilisation assays is not
shippable. The rate peaks near `n = 20` for the obvious reason: much smaller
and no replicate reaches the ceiling, much larger and several do, which
breaks the uniqueness condition.

## The discriminator

`on_rational_grid()` asks whether every value is `k / n` for a single integer
`n <= 100`. Count-derived proportions are; a continuous response divided by a
continuous maximum is not. Adding it drives every false positive above to zero
while leaving the target cases untouched:

| scenario | truth | naive | with grid guard |
|---|---|---|---|
| count proportion, `n = 10` | genuine | 0.148 | **0.000** |
| count proportion, `n = 20` | genuine | 0.380 | **0.000** |
| count proportion, `n = 30` | genuine | 0.218 | **0.000** |
| proportion rounded to 2 dp | genuine | 0.027 | **0.000** |
| divided by observed max (Gamma) | normalised | 1.000 | **1.000** |
| divided by observed max (Beta) | normalised | 1.000 | **1.000** |
| divided by `max(y) * 1.01` | normalised | 0.000 | 0.000 |
| count proportion divided by its own max | normalised | 0.495 | 0.000 |

Two deliberate misses, both accepted:

- **Divided by `max(y) * 1.01`.** No observation lands on 1, so there is no
  trace to find. Undetectable by construction, and the `1.01` padding is what
  the `beta_ub` discussion in #173 proposes to replace anyway.
- **A count proportion divided by its own maximum.** `k / k_max` stays on a
  rational grid, so the guard suppresses it. This is a real cost, but a missed
  message is the safe direction to fail and the alternative is the 38% false
  positive rate above.

## Signature B

No guard needed. 2000 draws per scenario:

| scenario | truth | hit rate |
|---|---|---|
| genuine, 3 control replicates | genuine | 0.000 |
| genuine, 4 control replicates | genuine | 0.000 |
| genuine, 6 control replicates | genuine | 0.000 |
| genuine, 10 control replicates | genuine | 0.000 |
| divided by control mean, 3 reps | normalised | 1.000 |
| divided by control mean, 4 reps | normalised | 1.000 |
| divided by control mean, 6 reps | normalised | 1.000 |
| divided by control mean, 10 reps | normalised | 1.000 |
| divided by control mean, then rounded to 3 dp | normalised | 0.552 |
| divided by observed max | normalised (wrong signature) | 0.000 |

The tolerance stays at `1e-8` rather than being loosened to catch the rounded
case. Loosening it to `1e-3` would fire on any genuine survival assay with
control survival near 100%, which is most of them. Catching about half of the
3 dp cases is a better trade than that.

Two side conditions matter and are both in the code. At least three control
replicates are required, because with two the mean is 1 whenever the pair
happens to straddle it. A control group whose values are all identical is
excluded, because a constant control carries no information either way.

## Real-data sweep

Every numeric column of every real dataset available in the repository:
30 CSVs under `ignore/` plus the three exported data frames. Signature A was
applied to each numeric column (173 columns); signature B to each ordered pair
of numeric columns, treating the first as the predictor (1096 pairs).

**Signature A: 5 hits, all true positives.**

| file | column | verdict |
|---|---|---|
| `lum_plates.csv` | `Lum_27C_rep1_qtoxB` .. `rep4_qtoxB` | luminescence, values < 1 with a single exact 1 in each of four replicate columns — each replicate divided by its own maximum |
| `CoralColour.csv` | `Intensity` | affine transform of `T14.MeanPixel` (`r^2 = 1`) scaled so the observed maximum maps to exactly 1 |

`CoralColour$Intensity` is worth singling out. The scaling is not
`v / max(v)` — the minimum does not map to 0 — but the ceiling is still set by
the observed maximum, so the check catches it. Both endpoints of that transform
are order statistics of the data being analysed.

**Signature B: 10 hits across 2 files, all true positives.**

| file | column | verdict |
|---|---|---|
| `Monique_microtox.csv` | `X5min`, `X15min` | control values scatter around 1 (0.979, 1.013, 1.020 ...), maximum 1.02 — proportion of control |
| `tmp_test_dat.csv` | `resp`, `y` | same pattern; counted once per candidate predictor column, of which the file has four |

**Zero false positives in 1269 screens.** Zero hits on `herbicide$fvfm`
(a genuine physical proportion), `nassarius$growth`, or `nec_data$y`.

## Where the check runs

`bnec()`, immediately after the family is resolved — not `check_data()`.
`check_data()` runs once per model, so a model set of ten would repeat a
paragraph-long message ten times. `bnec_hurdle()` and `bnec_joint()` both route
through `bnec()`, so they inherit it.

`message()` rather than `warning()`: neither finding is fatal, both are
recoverable by refitting the raw response, and the user may have a reason.

## Reproducing

Both scripts are self-contained and need only base R.

- Real-data sweep: `notes/scripts/normalisation_sweep.R`
- Simulation: `notes/scripts/normalisation_sim.R` (`set.seed(173)`, 2000 draws
  per scenario)
