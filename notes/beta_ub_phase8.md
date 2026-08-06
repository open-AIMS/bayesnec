# `beta_ub` Phase 8 — SBC and the coverage study

Validation evidence for the upper-bounded beta family (issue
[#173](https://github.com/open-AIMS/bayesnec/issues/173)). Phase 0
(`notes/beta_ub_phase0.md`) de-risked the likelihood before any code was
written; this checks the family as shipped.

Scripts: `dev/validation/sbc.R`, `dev/validation/coverage.R`,
`dev/validation/beta_fixed_u.stan`.
Results: `dev/validation/sbc_results.rds`, `dev/validation/coverage_results.rds`.

Both studies fit the Stan program `brms` generates from the shipped
`beta_ub()`, written out once to `dev/validation/beta_ub_sbc.stan`, so they
exercise the code that actually runs.

## Simulation-based calibration

256 replicates, 99 thinned posterior draws each, ranks binned into 10.

| parameter | chi-square | p | KS p | mean rank | expected |
|---|---|---|---|---|---|
| `top` | 10.84 | 0.287 | 0.768 | 49.7 | 49.5 |
| `beta` | 6.84 | 0.653 | 0.214 | 47.5 | 49.5 |
| `nec` | 2.69 | 0.975 | 0.932 | 49.7 | 49.5 |
| `phi` | 4.10 | 0.905 | 0.554 | 50.9 | 49.5 |
| `U` | 3.16 | 0.958 | 0.585 | 50.7 | 49.5 |

**Ranks uniform on all five parameters. 0 divergences across 255 fits.**

255 of 256 replicates contributed. The one loss was a response that underflowed
(see below), and it is counted and reported rather than dropped quietly,
because a silent drop would select on the parameters and make the ranks
meaningless.

### Three things this study had to get right first

Each of these produced a *false* calibration failure before being fixed, which
is worth recording — the first two are traps anyone repeating this will hit.

**The prior is baked into the Stan program as a literal.** `make_stancode()`
writes prior parameters as numbers, so a model compiled once and reused across
replicates carries the first dataset's `ymax` in its `delta` prior for every
later fit. This showed as `U` and `phi` failing SBC badly (p ~ 1e-12) while
`top`, `beta` and `nec` passed. A prior-only fit isolated it: every prior was
recovered exactly except `delta`'s, which came back centred 0.45 away from
where it should have been. The fix is to give `delta` a flat prior from `brms`
and supply the real one as a stanvar reading `u_loc`, `u_scale` and `ymax` from
the data block — the same density, including the truncation normalisation, but
now one compile serves every replicate.

**Random initialisation fails, and it fails selectively.** 6 of the first 16
replicates aborted with "no chains finished successfully": Stan's uniform(-2, 2)
starts put `top` above `U`, where the density is zero everywhere. The
replicates that fail are the ones with particular parameter values, so the
survivors are calibrated for nothing. This is the same failure
`add_beta_ub_inits()` exists to prevent inside `bnec()`, and it confirms those
inits are load-bearing rather than a nicety.

**The response can underflow, and that is a real limit on the family.** With
`beta ~ normal(0, 1)` over a predictor range of 6, the upper tail drives
`mu(6)` below 1e-300. The response follows it into the denormals, the beta
shape `m * phi` reaches ~1e-316, and `beta_lpdf` is numerically degenerate
there. Those were the remaining initialisation failures. The SBC prior was
narrowed to `beta ~ normal(-1, 0.5)`, which keeps `exp(beta)` inside roughly
(0.09, 0.9) and the smallest fitted mean at about 0.5% of `top` — a near-total
decline, still representable.

**This belongs in the user documentation, not just here.** The family cannot fit
a response that decays past the floating-point floor over the observed
predictor range. It is the same underflow Phase 0 found in the rejection
counts, where the fixture that never approached the ceiling rejected *more*
often than the one sitting against it.

## Coverage study

Mirrors the design of Ritz, Gerhard & Streibig (2026): simulate from a known
truth, refit under competing treatments of the ceiling, report bias, spread,
interval width and coverage. 150 replicates per cell, 0 divergences throughout.

Truth: `top` 0.8, `U` 1.0, `nec` 2, `beta` 0, `phi` 30. EC10 = 2.1054,
EC50 = 2.6931, NEC = 2. Control replicates 3, 6 and 10; five replicates at each
of the other seven concentrations.

Three arms, differing **only** in what the ceiling is:

- **normalised** — `U = max(y) * 1.01`, the response divided by its own
  observed maximum. The practice #173 documents.
- **fixed** — `U` at the true ceiling, known in advance. The best a constant
  divisor can do, and what a design ceiling or a historical control value gives.
- **estimated** — `U` estimated with a prior on the ceiling: the `beta_ub`
  family.

The first two are one Stan model with `U` as data, so the comparison really is
a single difference. Toxicity estimates are computed analytically per draw
(`ECp = nec - log(1 - p/100)/exp(beta)`) rather than off a grid, so nothing is
confounded with grid resolution.

### The result: the damage lands on `top`, not on ECx

| quantity | control n | arm | bias % | width | coverage |
|---|---|---|---|---|---|
| `top` | 3 | normalised | +0.91 | 0.0717 | **0.880** |
| `top` | 3 | fixed | 0.00 | 0.0810 | 0.947 |
| `top` | 3 | estimated | −0.02 | 0.0830 | 0.960 |
| `top` | 6 | normalised | +0.57 | 0.0674 | 0.940 |
| `top` | 6 | fixed | −0.25 | 0.0732 | 0.973 |
| `top` | 6 | estimated | −0.18 | 0.0750 | 0.967 |
| `top` | 10 | normalised | +0.71 | 0.0612 | 0.940 |
| `top` | 10 | fixed | −0.12 | 0.0651 | 0.953 |
| `top` | 10 | estimated | −0.09 | 0.0664 | 0.940 |

Nominal coverage is 0.95; the Monte Carlo standard error at 150 replicates is
0.018.

**At three control replicates the normalised arm covers `top` at 0.880 — 3.9
standard errors below nominal — with the narrowest interval of the three
(0.0717 against 0.0830).** The point estimate is barely moved (+0.91%). That is
precisely the pattern the phase plan predicted: point estimates roughly stable,
intervals honestly wider once the ceiling is modelled rather than assumed. The
deficit fades to 0.940 by six controls, which is Ritz et al.'s finding that the
problem is worst when there are fewest controls.

The estimated arm's interval is the widest of the three at every control count,
and it is the only arm that does not need to know the true ceiling to get
there.

### The toxicity estimates, by contrast, barely care

| quantity | control n | normalised | fixed | estimated |
|---|---|---|---|---|
| NEC bias % | 3 | −2.46 | +0.26 | −1.29 |
| NEC bias % | 6 | −3.40 | −1.24 | −2.26 |
| NEC bias % | 10 | −3.09 | −0.48 | −1.80 |
| EC10 bias % | 3 | −2.10 | +0.22 | −1.04 |
| EC10 bias % | 6 | −2.88 | −1.24 | −1.94 |
| EC10 bias % | 10 | −2.54 | −0.11 | −1.69 |
| EC50 bias % | all | −0.17 to −0.62 | +0.06 to −0.23 | −0.15 to −0.75 |

Coverage for EC10, EC50 and NEC sits between 0.913 and 0.980 in every arm, with
no arm clearly better. The normalised arm carries a consistent 2–3% negative
bias on NEC and EC10 that the fixed arm does not, and the estimated arm sits
between the two — but all three are small, and EC50 is essentially unaffected.

**This is a more specific result than the phase plan anticipated, and worth
stating plainly.** The plan expected the estimated arm to beat the `max(y)`
arm on coverage "particularly for NSEC and EC10 at small control n". It does
not: coverage of the toxicity estimates is fine in all three arms. What the
`max(y)` arm actually gets wrong is the control-level response and its
uncertainty. The reason is structural and the same one Phase 0 found: ECx is a
relative decline from the fitted `top`, so an error in the ceiling largely
cancels out of it, while `top` itself is exactly what a mis-stated ceiling
distorts.

Note also that the bias here runs *downwards*, where Ritz et al. report
effective doses biased upwards. There is no contradiction: they analyse
division by the **control mean**, which biases the inhibition trend through
Jensen's inequality; this arm divides by the **observed maximum**, a different
quantity with a different mechanism. Both are the same underlying error —
dividing by something random — but they do not have to push the same way.

## What to carry into the documentation

1. The family is calibrated: SBC uniform on all five parameters, 0 divergences.
2. State the underflow limit. A response that decays past the floating-point
   floor over the observed predictor range cannot be fitted, and the failure
   presents as an initialisation error rather than a clear message.
3. The honest selling point is the interval on `top`, not on ECx. Where the
   ceiling matters, it matters at the top of the curve.
4. Supplied initial values are required, not optional.
