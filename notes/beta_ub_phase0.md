# `beta_ub` Phase 0 — standalone Stan prototype

De-risking run for the upper-bounded beta family proposed in
[#173](https://github.com/open-AIMS/bayesnec/issues/173). The phase plan requires
three decisions to be settled *with a number* before anything lands in `R/`, and
sets an acceptance bar. This records both.

Code: `dev/prototype/beta_ub.stan`, `dev/prototype/sim_recover.R`.
Raw results: `dev/prototype/phase0_results.rds`.
Reproduce: `Rscript dev/prototype/sim_recover.R 500 40` (about 12 minutes on 8
workers; cmdstan 2.39.0).

## The model as prototyped

```
mu(x) = top * exp(-exp(beta) * (x - nec) * step(x - nec))     # nec3param
m(x)  = mu(x) / U
y     ~ U * Beta(m*phi, (1 - m)*phi)                          # -log(U) Jacobian
U     = ymax + delta,   delta > 0
```

with the prior placed on `U` rather than on `delta`, exploiting the fact that the
map is a pure location shift: `delta ~ normal(U_loc - ymax, U_scale)` with
`lb = 0` is exactly `U ~ normal(U_loc, U_scale)` truncated to `U > ymax`.

Priors used in fitting, none of which know the generating values except `U_loc`,
which is the subject of decision 3:

| parameter | prior |
|---|---|
| `top` | `normal(mean(control), 3 * sd(control))`, `lb = 0` |
| `beta` | `normal(0, 2)` (log scale) |
| `nec` | `normal(mean(x), sd(x))` |
| `phi` | `gamma(0.01, 0.01)` — the brms default for Beta |
| `delta` | `normal(U_loc - ymax, U_scale)`, `lb = 0` |

Generating values: `top` varied, `beta = 0`, `nec = 2`, `phi = 30`, `U = 1`,
`x` on `[0, 6]`, five replicates per concentration. `U_scale = 0.1` throughout.

## Acceptance

Base fixture — 8 concentrations × 5 replicates, `top/U = 0.8`, **500 replicates**
(Monte Carlo SE on a coverage of 0.90 is 0.013):

| parameter | coverage of the 90% CI | 95% CI on that coverage |
|---|---|---|
| `top` | 0.916 | [0.892, 0.940] |
| `beta` | 0.880 | [0.852, 0.908] |
| `nec` | 0.900 | [0.874, 0.926] |
| `phi` | 0.940 | [0.919, 0.961] |
| `U` | 0.972 | [0.958, 0.986] |

**0 divergences** at `adapt_delta = 0.95` across all 500 replicates; max Rhat
1.022.

Four of the five meet the `>= 0.90` bar outright. `beta` sits at 0.880, whose
95% interval [0.852, 0.908] still contains 0.90, so this is not a demonstrated
failure — but it is the one parameter at the low end and worth re-checking under
SBC in Phase 8 rather than assuming it away. `U` and `phi` over-cover, which is
what a prior-informed ceiling should do.

The earlier 50-replicate run reported `top` at 0.86 and `phi` at 0.88. Both were
Monte Carlo noise: at 50 replicates the standard error is 0.042, which cannot
separate 0.86 from 0.90. Recorded here because it is the reason the run was
repeated at 500.

## Decision 1 — how to enforce `mu(x) < U`

**Answer: `negative_infinity()` is sufficient. Do not reparameterise `top` as a
fraction of `U`.**

Rejection counts, against a deliberately conservative denominator of one lpdf
evaluation per iteration (a NUTS iteration takes several leapfrog steps, each
evaluating the lpdf, so the true per-evaluation rate is lower still):

| fixture | `top/U` | rejections | rate | init failures | divergences |
|---|---|---|---|---|---|
| base | 0.80 | 10 / 4000 | 0.25% | 2 | 0 |
| near-ceiling | 0.98 | 24 / 4000 | 0.60% | 4 | 0 |
| far-from-ceiling | 0.30 | 14 / 4000 | 0.35% | 2 | 0 |

All are below the ~1% threshold the plan sets, including the near-ceiling case
constructed to make the constraint bite. The fallback reparameterisation is not
needed.

Two caveats that must carry into Phase 1:

**The ceiling is not the only way the shape parameters leave `(0, inf)`.** For a
steep decline over a wide predictor range, `mu` underflows to zero long before
the largest concentration and `beta_lpdf` then errors on a zero shape. This is
why the `far-from-ceiling` fixture — which never approaches `U` at all — still
rejects at 0.35%, *more* than the base fixture. The guard in the prototype
covers `mu >= U`, `mu * phi <= 0` and `(1 - mu/U) * phi <= 0` together, and the
custom family must do the same.

**Only `nec3param` was tested.** For declining models `mu <= top`, so constraining
`top` would have sufficed and these numbers say little about the hard case. The
hormesis models peak above `top`, and the plan is right that the fitted peak must
be checked against `U` explicitly. That check is still owed.

**Initialisation needs work.** Two to four init failures per two-chain fit.
cmdstanr retries and recovers, so nothing failed here, but `bnec()` supplies its
own inits and Phase 4 must draw `delta > 0` and ensure `mu(0) = top < U` at
initialisation rather than relying on random inits landing in the feasible
region.

## Decision 2 — are `U` and `phi` separately identified?

**Answer: yes, in every design tested. The `|r| > 0.9` flag never fired.**

Both act on variance through `mu(U - mu)/(1 + phi)`, so at any single value of
`mu` they are not separately identified — they are separated only by how variance
changes *across* `mu`. The grid varies the two things that could break that:
number of concentrations, and how much of the `0..U` range the curve spans.
40 replicates per cell, 640 fits.

| `top/U` | median `cor(U, phi)` by `n_conc` 4 / 6 / 8 / 12 | max abs | coverage of `U` | width of `U` 90% CI |
|---|---|---|---|---|
| 0.3 | 0.30 / 0.37 / 0.43 / 0.52 | 0.57 | 1.00 everywhere | 0.324 – 0.327 |
| 0.5 | 0.39 / 0.46 / 0.52 / 0.58 | 0.63 | 1.00 everywhere | 0.312 – 0.327 |
| 0.7 | 0.50 / 0.53 / 0.57 / 0.63 | 0.71 | 1.00 everywhere | 0.246 – 0.272 |
| 0.9 | 0.55 / 0.50 / 0.56 / 0.58 | 0.78 | 0.975 / 0.925 / 0.925 / 0.900 | 0.112 – 0.165 |

Largest posterior correlation anywhere in the grid: **0.779**, at 8
concentrations with `top/U = 0.9`. 3 divergences in 640 fits; max Rhat 1.072.

The correlation rises steadily with the number of concentrations at `top/U` of
0.3, 0.5 and 0.7 (0.30 → 0.52, 0.39 → 0.58, 0.50 → 0.63), and rises with `top/U`
at fixed `n_conc`. At `top/U = 0.9` the trend in `n_conc` flattens
(0.55 / 0.50 / 0.56 / 0.58), so the pattern is not uniform. Rising correlation
with *more* information is the opposite of the usual expectation, and the
plausible reading is that more information about the variance function lets the
sampler trade `U` against `phi` along a better-resolved ridge, rather than both
being pinned separately by their priors. It never becomes a problem at these
design sizes, but it is the direction to watch if the family is later used on
much larger datasets.

### The design condition the documentation will need

The width of the `U` posterior is the number that matters, read against the prior
90% width of **0.329**:

| `top/U` | posterior width | interpretation |
|---|---|---|
| 0.3 | 0.325 | indistinguishable from the prior — `U` is not estimated |
| 0.5 | 0.312 – 0.327 | still essentially the prior |
| 0.7 | 0.246 – 0.272 | ~20% narrower; the data begin to inform `U` |
| 0.9 | 0.112 – 0.165 | 50–66% narrower; genuinely data-informed |

**The control response has to reach roughly 70% of the ceiling before the data
say anything about where the ceiling is, and only near 90% do they say much.**
Number of concentrations barely matters below `top/U = 0.7`; at 0.9 it helps
(0.165 → 0.112 going from 4 to 12 concentrations). What identifies `U` is how
close the curve gets to it, not how finely the predictor is sampled.

### Degenerate case, as the plan asks

`top/U = 0.2`, 40 replicates: coverage of `U` 1.00, median `U` 0.996, posterior
90% width **0.3293** against a prior 90% width of **0.3290**. The posterior for
`U` is its prior to three decimal places. `top/U` recovered at 0.202.

This is the expected behaviour, not a failure — but it is the strongest argument
in the whole phase for the plan's refusal to default `U_loc`/`U_scale` silently.
Where the curve does not approach the ceiling, every statement the model makes
about `U` is a statement the user supplied.

## Decision 3 — prior sensitivity

**Answer: a 2-fold misspecification of `U_loc` moves the toxicity estimates by
under 2%.**

40 replicates each, base design, `U` truly 1.0:

| `U_loc` | posterior median `U` | coverage of `U` | NEC | EC10 | EC50 |
|---|---|---|---|---|---|
| 0.5 × truth | 0.948 | 0.35 | 1.955 (−1.81%) | 2.075 (−1.29%) | 2.679 (−0.34%) |
| 1.0 × truth | 1.021 | 1.00 | 1.992 (—) | 2.102 (—) | 2.688 (—) |
| 2.0 × truth | 1.984 | 0.00 | 2.029 (+1.89%) | 2.135 (+1.57%) | 2.691 (+0.11%) |

This is the number a reviewer will ask for, and it is a good one. `U` itself is
badly wrong under misspecification — at `U_loc = 2×` the posterior sits at 1.98
with zero coverage — yet NEC, EC10 and EC50 move by less than 2%. The reason is
structural: `ecx(type = "absolute")` is a relative decline from the fitted `top`,
and `top` is identified by the control data whatever `U` does. EC50 is almost
completely insensitive (0.1–0.3%), EC10 and NEC slightly less so, which is the
expected ordering since the low-effect end of the curve sits closer to the
ceiling.

Note the asymmetry in `U` itself. At `U_loc = 0.5` the posterior median came back
at 0.948 rather than 0.5, because `U > ymax ≈ 0.87` truncates the prior away.
This is exactly the diagnostic case the plan flags: `U_loc <= ymax` puts the
prior entirely in the rejected region, and the phase confirms both that it
happens and that it should raise a warning rather than being silently absorbed.

## Verdict

Phase 0 passes on its own terms. The likelihood recovers its generating values,
the ceiling constraint costs well under 1% of proposals, `U` and `phi` are
separable in every design tested, and the toxicity estimates the package actually
reports are robust to a 2-fold error in the ceiling prior.

Carried into Phase 1:

1. Guard `mu * phi` and `(1 - mu/U) * phi` for positivity, not just `mu < U`.
   The underflow case is more common than the ceiling case.
2. Check the fitted peak against `U` for hormesis models — untested here.
3. Custom inits are required; random inits fail 2–4 times per two-chain fit.
4. Document the `top/U >= 0.7` design condition, and refuse to default
   `U_loc`/`U_scale`. Below that threshold the `U` posterior *is* the prior, to
   three decimal places.
5. Re-check `beta` coverage under SBC in Phase 8; it is the one parameter whose
   coverage sits at the low end of nominal.

Not yet done, and outside Phase 0: SBC, the coverage study against `max(y)`-Beta
from Phase 8, and anything at all in `R/`.
