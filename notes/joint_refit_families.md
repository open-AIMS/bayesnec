# Joint refit of a grouped fit across families (#448)

Simulated crossed data for four families were fitted with `bnec_group()`, and
each grouped fit was refitted with `bnec_joint()` at its defaults, which give
every level its own dispersion. The Gamma grouped fit was also refitted with
`disp_by_level = FALSE`, one dispersion shared by every level. Each level's
estimates from a refit were compared with that level's own fit. These are the
two untested cases D40 lists. Two sets of diagnostic fits followed: one to
attribute a difference found on the negative binomial data, and one to test
whether the Gamma refit's convergence depends on `adapt_delta`. Every number is produced by
`notes/scripts/joint_refit_families.R`. No package code was changed.

## Simulated data

Three levels, `a`, `b` and `c`, were each observed at the same eight
concentrations (0, 0.3, 0.6, 1, 1.5, 2.2, 3.2 and 4.5) with four replicates:
32 observations per level and 96 per family. Because every level has the same
series, a level's prediction grid and control value are the same whether read
from its own rows or from the whole data. The per-level range of PR #463
therefore cannot cause a difference here. The means were simulated on the
natural response scale, since `bayesnec` fits every family on the identity
link. The levels differ in their threshold or midpoint.

**Table 1.** True values at levels `a`, `b` and `c`.

| family | equation | `top` | `bot` | `nec` or `ec50` | slope `exp(beta)` | dispersion |
|---|---|---|---|---|---|---|
| gaussian | `nec4param` | 10, 10, 10 | 2, 1.5, 2.5 | `nec` 0.8, 1.3, 2.0 | 1.2, 1.0, 1.5 | `sigma` 0.5, 0.8, 0.6 |
| Gamma | `ecx4param` | 20, 18, 22 | 2, 3, 2.5 | `ec50` 1.2, 1.8, 2.6 | 2.5, 2.0, 3.0 | `shape` 15 at every level |
| binomial, 20 trials | `nec3param` | 0.9, 0.85, 0.92 | | `nec` 0.9, 1.5, 2.2 | 1.0, 0.8, 1.4 | none |
| negative binomial | `nec3param` | 60, 45, 75 | | `nec` 0.9, 1.5, 2.2 | 0.8, 1.0, 0.7 | `shape` 8, 15, 25 |

The Gamma levels share one `shape` because the same data serve the shared
dispersion refit. A shared dispersion is the correct model only where the
levels have one. With three different values, a difference between the two
refits would come from that misfit and not from how the shared refit is built. Negative binomial was chosen as the count family because
it has a dispersion parameter, so its refit also samples a per-level dispersion
prior; a Poisson refit would not.

## Fits and comparisons

Each family was fitted with
`bnec_group(formula, data, group_var = "site", family = <family>)`, fitting the
one equation in Table 1. The binomial formula was
`y | trials(n) ~ crf(x, "nec3param")`. Each grouped fit was then refitted with
`bnec_joint()`. With one equation fitted, every level favours it, so each refit
takes the dummy-coded form, in which each curve parameter and the dispersion
take a coefficient per level. The composed form, in which levels are fitted
different equations, was not fitted.

Every fit used four chains of 2,000 iterations with 1,000 of warm-up, giving
4,000 retained draws, on four cores and the `rstan` backend, with a seed for
each call. No `control` list was passed, so these fits sampled at Stan's
default `adapt_delta` of 0.8. That value was read from the
sampler's own record of its arguments.

For each level, the refit was compared with the level's own fit on three sets
of quantities: the curve parameters, the dispersion parameter, and the
estimates `nec()` (for `nec3param` and `nec4param`) and `ecx()` at 10 and 50
per cent. The comparison was made at the median and at the 2.5th and 97.5th
percentiles. A per-level dispersion is modelled on the log scale in the refit, so
it was exponentiated first. Each difference was divided by its Monte Carlo
standard error, with the two fits' errors from `posterior::mcse_quantile()`
added in quadrature, and the result is written *z*. A quantity is taken to agree
where |*z*| < 3 at all three percentiles. A family gives 45 to 63 values of *z*.
If they were independent, at least one would exceed 3 by chance in 11 to 16 per
cent of families. `nec()` returns the draws of the `nec` parameter itself, so it
is counted once, with the parameter. The ratio of the refit's posterior standard
deviation to the level fit's is reported as the SD ratio.

## Convergence

**Table 2.** Convergence at `adapt_delta` 0.8: the largest R-hat and the
smallest bulk and tail ESS over the curve and dispersion parameters, and the
divergent transitions in 4,000 draws. Level fits are given in the order `a`,
`b`, `c`.

| family | fit | max R-hat | min bulk ESS | min tail ESS | divergent |
|---|---|---|---|---|---|
| gaussian | level fits | 1.002, 1.010, 1.004 | 2012, 596, 903 | 1602, 995, 770 | 0, 0, 0 |
| gaussian | joint | 1.003 | 768 | 816 | 2 |
| Gamma | level fits | 1.005, 1.026, 1.007 | 1117, 197, 1000 | 923, 149, 457 | 0, 9, 4 |
| Gamma | joint | 1.137 | 19 | 57 | 120 |
| Gamma | joint, shared dispersion | 1.004 | 728 | 351 | 8 |
| binomial | level fits | 1.003, 1.002, 1.003 | 994, 2158, 1818 | 1595, 2340, 2219 | 0, 0, 0 |
| binomial | joint | 1.002 | 1703 | 2103 | 0 |
| negative binomial | level fits | 1.001, 1.002, 1.003 | 1966, 1827, 1484 | 1627, 1900, 955 | 0, 0, 0 |
| negative binomial | joint | 1.003 | 2456 | 1647 | 0 |

The gaussian, binomial and negative binomial refits converged, with an R-hat of
at most 1.003 and a bulk ESS of at least 768. The only sampler warning was for
2 divergent transitions in the gaussian refit. The
binomial refit kept the `trials()` term.

The Gamma refit with a dispersion per level did not converge. Its 120 divergent
draws have a median `beta` at level `b` of 3.62, against 1.06 in the other
draws, and chain 4 holds 72 of them. Level `b`'s `beta` also has the smallest
ESS in the refit, 19, and the smallest in level `b`'s own fit, 197. Level `b`'s
`ec50` lies between the concentrations 1.5 and 2.2. Once the curve falls from
`top` to `bot` within that interval, a steeper slope changes the likelihood
little. That is consistent with the long upper tail of level `b`'s `beta` in
both fits: its 97.5th percentile is 3.7 in the refit and 2.6 in the level fit,
against a median of 1.0. It was not tested further.

## The Gamma refits with a raised `adapt_delta`

The Gamma case was fitted again with the same data and seeds, passing
`control = list(adapt_delta = 0.99)` to both `bnec_group()` and `bnec_joint()`.
That is the value `bnec()` uses for a group-level term on a constrained mean and
the value D40 names. The package default was not changed.

**Table 3.** Convergence of the Gamma fits at `adapt_delta` 0.99.

| fit | max R-hat | min bulk ESS | min tail ESS | divergent |
|---|---|---|---|---|
| level fits `a`, `b`, `c` | 1.002, 1.007, 1.005 | 1163, 497, 845 | 1039, 555, 515 | 0, 5, 12 |
| joint | 1.027 | 198 | 64 | 29 |
| joint, shared dispersion | 1.007 | 521 | 377 | 23 |

At 0.99 the refit with a dispersion per level reached an R-hat of 1.027 and a
tail ESS of 64, both in level `b`'s `beta`. Both are short of the R-hat below
1.01 and ESS above 400 that Vehtari et al. (2021) recommend. Its divergent draws again sit at large values of level `b`'s
`beta`: a median of 3.64, against 1.00 elsewhere. The level fits for `b` and
`c` diverge in the same region at 0.99, with medians of 2.70 and 3.98 in their
divergent draws. The divergences therefore arise from the `ecx4param` posterior
on these data, which both routes sample. The refit samples the three levels with
one step size. At both settings it returned more divergent transitions than the
three level fits together: 120 against 13 at 0.8, and 29 against 17 at 0.99.

## Agreement with the level fits

**Table 4.** Each refit against the level fits on the same data and settings.

| family | `adapt_delta` | quantities | values of *z* | max \|*z*\| | values above 3 | SD ratio |
|---|---|---|---|---|---|---|
| gaussian | 0.8 | 21 | 63 | 2.51 | 0 | 0.90 to 1.05 |
| Gamma | 0.8 | 21 | 63 | 3.08 | 1 | 0.88 to 1.69 |
| Gamma | 0.99 | 21 | 63 | 3.17 | 1 | 0.88 to 1.28 |
| binomial | 0.8 | 15 | 45 | 2.80 | 0 | 0.98 to 1.05 |
| negative binomial | 0.8 | 18 | 54 | 3.57 | 1 | 0.97 to 1.06 |

No difference was found for gaussian or binomial. The largest values were 2.51,
at the 97.5th percentile of level `c`'s `bot`, and 2.80, at the median of level
`b`'s EC50.

The Gamma comparison at 0.8 is not a test. A refit with an ESS of 19 has a
Monte Carlo error large enough that a small *z* says little. At 0.99 every
median agrees, the largest |*z*| at the median being 2.36 (level `c`'s EC50),
and no median differs by more than 0.13 posterior standard deviations. The one
value above 3 is the 97.5th percentile of level `a`'s `top`, 34.5 against 32.5.
The SD ratio of 1.28 is level `b`'s `beta`, whose 97.5th percentile is 4.0 in the
refit against 3.0 in the level fit. That is the tail where the refit's ESS is
64.

On the negative binomial data, the one value above 3 is the median of level
`a`'s `shape`: 6.59 in the refit against 6.36 in the level fit, *z* = 3.57.
Level `a`'s `top` is next, 63.09 against 62.46, *z* = 2.98. The two fits are
given different priors in two places, shown for level `a` in Table 5.

**Table 5.** Priors at level `a` of the negative binomial data.

| parameter | prior in the refit | prior in the level fit |
|---|---|---|
| `top` | `gamma(2, 0.0296)`, derived from the pooled response | `gamma(2, 0.0357)`, derived from level `a`'s response |
| `shape` | `normal(2, 2)` on `log(shape)`, from `add_level_disp_defaults()` | `inv_gamma(0.4, 0.3)` on `shape`, the `brms` default |

A dummy-coded refit with a dispersion per level factorises by level. Each
likelihood term and each coefficient prior involves one level's coefficients
alone, so level `a` fitted by itself under the refit's priors has the refit's
level-`a` posterior. Two such fits of level `a` were made with `bnec()`. In the
first, both priors were replaced; in the second, only the dispersion prior.
`lognormal(2, 2)` on `shape` is the same distribution as `normal(2, 2)` on its
logarithm.

**Table 6.** Fits of level `a` of the negative binomial data. The largest |*z*| is taken
over `top`, `beta`, `nec`, `shape`, EC10 and EC50 at the three percentiles.

| fit of level `a` | `shape` median | `top` median | max \|*z*\| against the refit | max \|*z*\| against the level fit |
|---|---|---|---|---|
| level fit, its own priors | 6.36 | 62.46 | 3.57 | |
| alone, both priors replaced | 6.53 | 63.25 | 2.44 | 3.34 |
| alone, dispersion prior replaced | 6.63 | 63.06 | 1.63 | 4.17 |
| joint refit | 6.59 | 63.09 | | |

Under the refit's priors, level `a` fitted alone reproduces the refit to within
Monte Carlo error. The refit's level `a` is therefore the posterior its priors
give, and no fault in how the refit is built was found. The difference from the
level fit is produced by the priors. The dispersion prior alone changes the
`shape` median by 4.2 Monte Carlo errors, and the pooled `top` prior accounts
for the rest of the difference in `top`. On a posterior scale the differences
are small: 0.11 posterior standard deviations for `shape` and 0.08 for `top`,
with SD ratios between 0.97 and 1.06. The 382 specification records that a
dummy-coded refit with a dispersion per level differs from a `bnec_group()` fit
only in its priors (`notes/tasks/382-joint-group-refit-claude.md`). This is the
size of that difference on these data, and it
was detectable with 4,000 draws.

The same two differences apply to the gaussian and Gamma refits. Their `top`
and `bot` priors are derived from the pooled response. Each level's log
dispersion takes the entry of `disp_intercept_priors()`: `normal(2, 2)` for
Gamma and `normal(log(sd(y)), 2)` for gaussian, `normal(1.197, 2)` here. A level
fit takes the `brms` default on the natural scale: `gamma(0.01, 0.01)` for the
Gamma `shape` and `student_t(3, 0, 5.1)` for the gaussian `sigma`. In those
comparisons the gaussian refit gave no value above 3, and the Gamma refit at
0.99 gave one, at the 97.5th percentile of level `a`'s `top`. The binomial refit
and level fits share every prior, since `top` takes the fixed `beta(5, 2)` and
the family has no dispersion parameter.

## A shared dispersion

The Gamma grouped fit was refitted with `bnec_joint(disp_by_level = FALSE)`, at
0.8 and again at 0.99 beside the 0.99 refit with a dispersion per level. The
refit has one `shape` and no `b_shape` coefficient. That `shape` takes the
`brms` default prior `gamma(0.01, 0.01)`, the same prior the level fits use, and
each curve parameter still takes one coefficient per level. Its convergence is
in Tables 2 and 3. It converged at both settings, with 8 and 23 divergent
transitions.

At 0.8 it cannot be compared with the refit with a dispersion per level, which
did not converge. At 0.99 the two refits were compared on the curve parameters
and the ECx estimates: 18 quantities and 54 values of *z*. The largest |*z*| was
2.73, none exceeded 3, and no median differed by more than 0.09 posterior
standard deviations. No difference in the estimates was found between the two
refits.

**Table 7.** The Gamma `shape` at `adapt_delta` 0.99, median and 95 per cent
interval. The simulated value is 15 at every level.

| fit | level `a` | level `b` | level `c` |
|---|---|---|---|
| level fits | 14.49 (8.30, 23.55) | 11.51 (6.16, 18.95) | 13.39 (7.26, 21.62) |
| joint, dispersion per level | 14.44 (8.43, 23.03) | 11.52 (6.40, 18.53) | 13.21 (7.74, 21.59) |
| joint, shared dispersion | 13.24 (9.60, 17.51) | same | same |

Every interval contains the simulated 15. The shared `shape` has a posterior
standard deviation 0.51 to 0.67 times that of each level's own, since it is
estimated from all 96 observations.

The intervals for level `b` are narrower under the shared dispersion. The SD
ratio is 0.77 for `beta`, 0.87 for `top` and `ec50`, and 0.90 and 0.91 for EC50
and EC10. For levels `a` and `c` it is 0.95 to 1.11. Level `b` has the smallest
`shape` when each level has its own, and the Gamma variance is the squared mean
divided by `shape`. A shared `shape` of 13.2 in place of 11.5 therefore gives
level `b` a smaller residual variance, and its curve is estimated more
precisely.

Against the level fits at 0.99, the shared refit gave one value above 3: the
97.5th percentile of level `b`'s `bot`, 3.13 against 3.25, *z* = 3.76. No median
differed by more than 0.13 posterior standard deviations.

## Scope of the measurement

- One simulated dataset per family and one seed per configuration. At 0.8 the
  two Gamma refits returned 120 and 8 divergent transitions; without further
  seeds that difference is not attributed to the dispersion model.
- A crossed design only. Levels observed over different ranges, the case PR
  #463 changed, were not sampled.
- The dummy-coded form only. The composed form, in which levels fit different
  equations, has still been sampled only for Beta.
- One equation per family, three levels and 32 observations per level.
- Poisson, beta-binomial, Bernoulli and the two-block families were not
  fitted. Neither was a group-level term, nor a `disp()` term with
  `disp_by_level = FALSE`.
- The shared dispersion was fitted only where the levels share one. Its
  behaviour where they do not was not measured.
- The `rstan` backend only.
- `adapt_delta` 0.99 was passed through `control` for the diagnostic alone. The
  default D40 holds was not changed.

## Provenance

`notes/scripts/joint_refit_families.R` was run on 2026-10-20 from the
repository root under `devtools::load_all()`, at `ba30b91e` on `predev` (version
2.1.3.42), with R 4.6.1, `brms` 2.23.0 and `rstan` 2.32.7 on the `rstan`
backend. The data were drawn after `set.seed(448)`. The grouped fits took seeds
4481 to 4484, the refits those seeds plus 10, the shared refit 4502, and the
level-`a` diagnostic fits 4514. The per-fit summaries, but not the fits, are in
`ignore/joint_refit_families.rds`, which is git-ignored. The Monte Carlo errors
come from the `posterior` package, which `brms` imports but `DESCRIPTION` does
not list.

The 28 fits took 62 minutes of wall clock. Summing the longest chain of each
fit, sampling took 63 s of that; the remainder is mainly Stan compilation. The five default cases ran first, in 2,149 s. The two
diagnostics ran afterwards, in 1,567 s. The negative binomial diagnostic first
refits that case with the same seeds, and the repeat reproduced every quantile
of the first run exactly.
