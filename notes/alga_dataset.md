# The `alga` dataset: what it is, and what it is for

Supersedes `dataset_usage_plan.md` (2026-08-06), which was written around the
`beta_ub` family and is wrong in its central claim. Written 2026-08-10.

`alga` ships (`data/alga.rda`), is documented (`man/alga.Rd`) and is built by
`data-raw/alga.R` from files in the git-ignored `ignore/alga_examples/`. Nothing
in the package currently uses it. It exists for the `example7` rewrite — see the
plan on [#189](https://github.com/open-AIMS/bayesnec/pull/189#issuecomment-5235587110).

## The design

310 rows: two marine microalgae x two anonymised contaminants, from four source
tests. The contaminants are on **different undisclosed scales and are never
comparable with each other**; dose values preserve relative spacing only.

| species | contaminant | days | `density_initial` | n | controls | doses |
|---|---|---|---|---|---|---|
| `c_proliferum` | A | 7 | 7968 | 85 | 20 | 0–20 |
| `r_salina` | A | 3 | 3871 | 85 | 20 | 0–20 |
| `c_proliferum` | B | 7 | 8681 | 70 | 10 | 0–1000 |
| `r_salina` | B | 3 | 3123 | 70 | 10 | 0–1000 |

`dose_measured` is populated for the 140 contaminant-B rows only.

## `sgr` and `density` are the same measurement

**This is the single most important fact about the dataset, and the superseded
plan got it wrong.** It is not two endpoints:

```
sgr = (log(density) - log(density_initial)) / days
```

This reproduces the data exactly for contaminant A (residuals ~1e-9); contaminant
B's growth rates are rounded to three decimals, so it holds to ~5e-3. `sgr` is
the OECD TG 201 average specific growth rate, the guideline's Equation [1], from
which `ErCx` is derived.

Choosing between `sgr` and `density` is therefore **choosing a variance model**,
not choosing an endpoint. The old plan's "two responses, two families — pick
deliberately" framing is false and led to the `beta_ub` dead end.

## `sgr` is on the real line

| group | control mean | min | max | n negative | n substituted 0 |
|---|---|---|---|---|---|
| `c_proliferum` x A | 0.1194 | **−0.5774** | 0.1367 | 12 | 0 |
| `r_salina` x A | 1.4307 | **−1.9862** | 1.4916 | 4 | 16 |
| `c_proliferum` x B | 0.1694 | **−0.3100** | 0.1830 | 10 | 0 |
| `r_salina` x B | 0.9506 | **−1.1470** | 1.0340 | 17 | 4 |

Negative growth is real: the population is declining. No bounded family applies.

## The substituted zeros — the point of the dataset

Density is counted to a resolution of 10, so a recorded density of `0` means
"below the counting limit", not "no cells". Where that happened the source set
`sgr` to `0`, and **a growth rate of 0 means "no change"** — which sits in the
*middle* of the observed range, above every genuinely negative value. Total loss
of the population is thereby recorded as a *less* severe effect than a merely
declining one, turning a monotone concentration–response non-monotone.

20 rows are affected, all `r_salina` (16 under A, 4 under B), flagged by
`sgr_source == "substituted"`. They are preserved as supplied on purpose.

OECD TG 201 Annex 5 is explicit about this practice:

> It is not recommended to assign a zero or small positive value to such negative
> values because this distorts the error distribution.

The censoring bound implied by the counting limit is
`(log(10) - log(density_initial)) / days`.

## Model choice: the constraint is `zero_bounded`, not `bot`

A response that goes negative needs a model whose *functional form* can take
negative values. That is **not** the same as needing a `bot` parameter. 12 of the
23 models qualify — the 9 with a free `bot`, plus `neclin`, `neclinhorme` and
`ecxlin`, which are `bot_free` yet unbounded below because they are subtractive.

`bnec()` already handles this: `check_models()` drops `mod_groups$zero_bounded`
automatically, so `model = "all"` is safe and no hand-picking is needed.

## Variance structure — the evidence behind #191

Within-group SD on the `sgr` scale is emphatically not constant:

| group | min SD | max SD | ratio | `log(sd) ~ log(mean)` slope (density scale) |
|---|---|---|---|---|
| `c_proliferum` x A | 0.0046 | 0.0923 | 20.0 | 0.29 |
| `r_salina` x A | 0.0215 | 0.2114 | 9.8 | 0.81 |
| `c_proliferum` x B | 0.0034 | 0.0122 | 3.6 | 0.86 |
| `r_salina` x B | 0.0160 | 0.1860 | 11.6 | 0.94 |

Variance is *largest* where the population is collapsing. The slope is the
exponent `p` in `sd ∝ mu^p`: near 1 (constant CV) in three of four groups, not
0.5 (which is what a bounded beta-type variance function would imply).

Model-based confirmation — 9 Gaussian `bnec()` fits (`ecx4param`; these four
tests plus five population growth-rate tests from an unpublished multi-toxicant
dataset held in `ignore/`, not distributed), PIT at the top of the curve, n = 244:

| | observed | expected |
|---|---|---|
| mean PIT | 0.462 | 0.500 |
| frac PIT > 0.9 | 0.025 | 0.100 |
| frac PIT < 0.1 | 0.025 | 0.100 |
| sd(PIT) | 0.193 | 0.289 |

Both tails are starved **equally**, so the predictive distribution is simply too
wide at the plateau — heteroscedasticity, not a truncated upper tail. This is the
motivating evidence for [#191](https://github.com/open-AIMS/bayesnec/issues/191).

## Why there is no ceiling (do not re-open `beta_ub`)

Under a bounded family the ceiling is not a free parameter: with `m = top/U`,
`CV² = (1−m)/(m(1+φ))`, so the measured control CV pins `φ` once `m` is chosen and
the control skew follows with no freedom left. Pooled `sgr` controls (n = 60) give
a skew of **−0.315** against a 5% parametric-bootstrap cutoff of **−0.497** —
compatible only with `U ≥ 1.13 × top`, best-matching `U = 1.20 × top`, 95%
interval `[1.08 × top, ∞)`.

But OECD TG 201 Annex 5 states the control is grown at maximum response for the
conditions imposed — the control **is** µ_max — so any biologically honest `U` is
~1.00–1.05 × top. **The biologically meaningful ceiling is precisely the one the
data exclude.** `beta_ub` was closed on this basis ([#187](https://github.com/open-AIMS/bayesnec/pull/187)).

## What `alga` is for

The `example7` rewrite: a growth-data case study covering the endpoint definition,
not flooring negatives, not normalising to the control mean or observed maximum,
model choice, and a demonstration of the dispersion sub-model. Structure is in the
[#189 plan comment](https://github.com/open-AIMS/bayesnec/pull/189#issuecomment-5235587110);
it is blocked on #191.

Note `ecx(type = "absolute")` is algebraically `ErCx`, since TG 201 defines
`%I_r = (µ_C − µ_T)/µ_C`. `type = "relative"` is **not** the ecotoxicological
definition when `bot` is negative.
