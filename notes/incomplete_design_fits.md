# Fitted validation of the incomplete-design prior changes (#418)

`nec4param` and `ecx4param` were fitted to six simulated gaussian designs with
known parameters, one data seed, on the build before PR #409 and on its merge:
24 fits in all. The prior audit (`notes/prior_audit.md` parts 3 to 5) measured
the priors and the initial-value search for the same kind of design and fitted
nothing. The fits measure what #418 asks of the posterior and of the reported
estimates. They are produced by `notes/scripts/incomplete_design_fits.R`.

## Builds and settings

| build | commit | what it contains |
|---|---|---|
| base | `eff06ea9` | the parent of the #386 programme |
| candidate | `c3824643` | the merge of PR #409 |

Both report `Version` 2.1.3.39, so each fit asserts the commit of the library it
loaded. Only #393 changes a fit on the default path between the two, so base
against candidate at defaults isolates the removal of the tested-range bound on
the `nec` and `ec50` priors. The candidate with and without
`asymptote_observed = FALSE` isolates that argument, the declaration that the
highest concentration did not reach the lower asymptote.

Every fit is a `bnec()` call at its defaults: four chains of 10,000 iterations
with 8,000 of warm-up, so 8,000 retained draws, on the `rstan` backend. Each
call is given `seed = 418` and `family = gaussian()`, whose link is the
identity. The family is named because `bnec()` otherwise chooses it from the
response, and it fits a positive continuous response as Gamma.

Five sets of fits were run:

| set | build | designs | `prior_type` | declaration | fits |
|---|---|---|---|---|---|
| base defaults | base | all six | `"uninformative"` | none | 6 |
| candidate defaults | candidate | all six | `"uninformative"` | none | 6 |
| declared | candidate | the four incomplete | `"uninformative"` | `asymptote_observed = FALSE` | 4 |
| base, `regularizing` | base | the four incomplete | `"regularizing"` | none | 4 |
| candidate, `regularizing` | candidate | the four incomplete | `"regularizing"` | none | 4 |

`"uninformative"` is the default `prior_type` on both builds, so the second
setting #418 asks for is `"regularizing"`.

## Designs

The series and response are the audit sweep's `linear` gaussian cell: eleven
concentrations from 0 to 10 by six replicates, `top` 10, `bot` 2 and a residual
standard deviation of 0.6. One vector of 66 residuals was drawn under
`set.seed(418)` and added to every design's mean, so the six datasets differ in
the mean curve alone. Every observation is positive, the smallest being 1.56,
which the declaration needs: on the gaussian branch its floor is zero only for a
non-negative response. #386's own series was not used, because it has no
concentration between 10 and 40. An `ecx4param` curve complete at both ends then
has at most one concentration on its slope.

The threshold is placed as the audit places it. The decay rate is set on the
complete design, at 5 over the distance from the threshold to the highest
concentration, and then held. The threshold is shifted so that the mean at the
highest concentration has fallen a stated fraction of the span from `top`
towards `bot`. For `nec4param` that fraction is 0.36, the audit's `f36`, and the
threshold stays inside the series. For `ecx4param` the midpoint leaves the
series once the fraction falls below one half, so the incomplete design with the
midpoint inside uses the audit's `f92`, and `f36` is the design with the
midpoint above. The `nec4param` threshold above the series is 1.5 times the
highest concentration, the ratio of #386's `nec` of 60 against 40. Its mean is
`top` at every concentration, and it is called the flat design below.

| design | true threshold | rate (`beta`, its log) | fraction of span at 10 | observed mean at 0 | observed mean at 10 | true absolute EC50 |
|---|---|---|---|---|---|---|
| `nec4param` complete | `nec` 4 | 0.833 (−0.182) | 0.993 | 9.48 | 2.51 | |
| `nec4param` incomplete | `nec` 9.46 | 0.833 (−0.182) | 0.36 | 9.48 | 7.57 | |
| `nec4param` above | `nec` 15 | 0.833 (−0.182) | 0 | 9.48 | 10.45 | |
| `ecx4param` complete | `ec50` 5 | 1 (0) | 0.993 | 9.43 | 2.51 | 5.53 |
| `ecx4param` incomplete | `ec50` 7.56 | 1 (0) | 0.92 | 9.47 | 3.09 | 8.07 |
| `ecx4param` above | `ec50` 10.58 | 1 (0) | 0.36 | 9.48 | 7.57 | 11.09 |

The `nec4param` incomplete and `ecx4param` above designs reach the same fraction
of the span at 10 and share their residuals, so their observed mean there is the
same, 7.57. The declared `bot` prior is built from that mean alone and is the
same on the two designs; the default entries also read the rest of the response
and differ. The observed control mean is 9.43 to 9.48 against a `top` of 10, a
shortfall of about two standard errors of a six-replicate mean that the shared
residuals give every design. The true absolute EC50 is the concentration at
which the generating mean falls to half its value at the control, which is what
`ecx(type = "absolute")` estimates.

## Results by design

In each table the `bot` prior interval is the central 95% of the prior after
truncation to its declared bounds. Posteriors are the median with the 2.5% and
97.5% quantiles. The CDF at the true `bot` is the share of posterior draws at or
below 2, and 0 means none of 8,000; a prior CDF at a true value is the same
quantity for the truncated prior. The prior mass above 10 is the
share of the threshold prior above the highest concentration after truncation,
and the share above 10 is the same for the posterior draws. The NEC or NSEC is
the no-effect estimate `summary()` prints, taken from the fitted object. The
EC50 is `ecx(fit, ecx_val = 50)`, of the default absolute type. A `>=` marks an
entry the candidate reports as a bound. `NA` is what the base returns for all
three entries when every draw is beyond the range. The extrapolated entry, on
the candidate only, is `nec(fit, extrapolate = TRUE)` for `nec4param` and
`ecx(fit, ecx_val = 50, x_range = c(0, 100))` for `ecx4param`, since
`extrapolate = TRUE` is refused for a curve-read estimate.

### The complete `nec4param` design

| fit | `bot` prior, 95% | `bot` posterior | CDF at true `bot` | `nec` prior mass above 10 | `nec` posterior | share above 10 | NEC | EC50 | extrapolated |
|---|---|---|---|---|---|---|---|---|---|
| base | -15.1 to 19.4 | 2.33 (2.02 to 2.62) | 0.0214 | 0 | 4.1 (3.97 to 4.37) | 0 | 4.1 (3.97 to 4.37) | 5.18 (5.05 to 5.34) |  |
| cand | -15.1 to 19.4 | 2.33 (2.02 to 2.62) | 0.0185 | 0.244 | 4.11 (3.97 to 4.38) | 0 | 4.11 (3.97 to 4.38) | 5.18 (5.04 to 5.33) | 4.11 (3.97 to 4.38) |

### The incomplete `nec4param` design

| fit | `bot` prior, 95% | `bot` posterior | CDF at true `bot` | `nec` prior mass above 10 | `nec` posterior | share above 10 | NEC | EC50 | extrapolated |
|---|---|---|---|---|---|---|---|---|---|
| base | 4.29 to 13 | 7.5 (5.05 to 8) | 0.00025 | 0 | 9.42 (9 to 9.95) | 0 | 9.42 (9 to 9.95) | NA |  |
| cand | 4.29 to 13 | 7.48 (4.83 to 8) | 0.00188 | 0.244 | 9.4 (8.99 to 9.94) | 0 | 9.4 (8.99 to 9.94) | `>=` 10 (`>=` 10 to `>=` 10) | 9.4 (8.99 to 9.94) |
| cand, declared | 0 to 7.57 | 4.52 (0.293 to 7.77) | 0.124 | 0.244 | 9.41 (8.93 to 9.92) | 0 | 9.41 (8.93 to 9.92) | `>=` 10 (`>=` 10 to `>=` 10) | 9.41 (8.93 to 9.92) |
| base, `regularizing` | 5.83 to 9.32 | 7.52 (6.36 to 7.98) | 0 | 0 | 9.41 (9 to 9.95) | 0 | 9.41 (9 to 9.95) | NA |  |
| cand, `regularizing` | 5.83 to 9.32 | 7.51 (6.42 to 7.99) | 0 | 0.205 | 9.41 (9.01 to 9.94) | 0 | 9.41 (9.01 to 9.94) | `>=` 10 (`>=` 10 to `>=` 10) | 9.41 (9.01 to 9.94) |

### The `nec4param` design with its threshold above the series

| fit | `bot` prior, 95% | `bot` posterior | CDF at true `bot` | `nec` prior mass above 10 | `nec` posterior | share above 10 | NEC | EC50 | extrapolated |
|---|---|---|---|---|---|---|---|---|---|
| base | 6.58 to 12.1 | 10.2 (8.08 to 12) | 0 | 0 | 3.29 (0.669 to 9.43) | 0 | 3.29 (0.669 to 9.43) | NA |  |
| cand | 6.58 to 12.1 | 10.2 (7.24 to 12.1) | 0 | 0.244 | 4.45 (0.658 to 29.6) | 0.24 | 4.45 (0.657 to `>=` 10) | `>=` 10 (`>=` 10 to `>=` 10) | 4.45 (0.658 to 29.6) |
| cand, declared | 0 to 10.5 | 5.94 (0.457 to 11.7) | 0.084 | 0.244 | 11.5 (1.33 to 42) | 0.626 | `>=` 10 (1.33 to `>=` 10) | `>=` 10 (`>=` 10 to `>=` 10) | 11.5 (1.33 to 42) |
| base, `regularizing` | 9.34 to 11.6 | 10.3 (9.94 to 11.4) | 0 | 0 | 3.25 (0.784 to 9.19) | 0 | 3.25 (0.784 to 9.19) | NA |  |
| cand, `regularizing` | 9.34 to 11.6 | 10.3 (9.92 to 11.3) | 0 | 0.205 | 3.59 (0.879 to 16.2) | 0.0786 | 3.59 (0.879 to `>=` 10) | `>=` 10 (`>=` 10 to `>=` 10) | 3.59 (0.879 to 16.2) |

### The complete `ecx4param` design

| fit | `bot` prior, 95% | `bot` posterior | CDF at true `bot` | `ec50` prior mass above 10 | `ec50` posterior | share above 10 | NSEC | EC50 | extrapolated |
|---|---|---|---|---|---|---|---|---|---|
| base | -13.1 to 17.4 | 2.31 (2 to 2.6) | 0.0251 | 0 | 5.03 (4.83 to 5.22) | 0 | 2.32 (1.16 to 2.8) | 5.61 (5.43 to 5.8) |  |
| cand | -13.1 to 17.4 | 2.32 (1.99 to 2.6) | 0.0301 | 0.244 | 5.03 (4.83 to 5.23) | 0 | 2.34 (1.27 to 2.82) | 5.61 (5.42 to 5.8) | EC50 5.61 (5.43 to 5.81) |

### The incomplete `ecx4param` design

| fit | `bot` prior, 95% | `bot` posterior | CDF at true `bot` | `ec50` prior mass above 10 | `ec50` posterior | share above 10 | NSEC | EC50 | extrapolated |
|---|---|---|---|---|---|---|---|---|---|
| base | -9.19 to 16.3 | 2.67 (1.85 to 3.25) | 0.0496 | 0 | 7.48 (7.24 to 7.79) | 0 | 4.57 (3.29 to 5.17) | 8.15 (7.96 to 8.35) |  |
| cand | -9.19 to 16.3 | 2.66 (1.88 to 3.22) | 0.044 | 0.244 | 7.48 (7.25 to 7.77) | 0 | 4.55 (3.3 to 5.14) | 8.16 (7.96 to 8.35) | EC50 8.17 (7.97 to 8.36) |
| cand, declared | 0 to 3.09 | 2.49 (1.69 to 3.07) | 0.107 | 0.244 | 7.54 (7.29 to 7.85) | 0 | 4.49 (3.16 to 5.09) | 8.17 (7.98 to 8.37) | EC50 8.18 (7.99 to 8.38) |
| base, `regularizing` | -2.01 to 8.2 | 2.68 (1.85 to 3.25) | 0.046 | 0 | 7.48 (7.24 to 7.78) | 0 | 4.57 (3.23 to 5.14) | 8.15 (7.96 to 8.35) |  |
| cand, `regularizing` | -2.01 to 8.2 | 2.68 (1.88 to 3.25) | 0.0411 | 0.205 | 7.48 (7.23 to 7.79) | 0 | 4.55 (3.24 to 5.15) | 8.15 (7.96 to 8.35) | EC50 8.16 (7.96 to 8.36) |

### The `ecx4param` design with its midpoint above the series

| fit | `bot` prior, 95% | `bot` posterior | CDF at true `bot` | `ec50` prior mass above 10 | `ec50` posterior | share above 10 | NSEC | EC50 | extrapolated |
|---|---|---|---|---|---|---|---|---|---|
| base | 3.77 to 12.6 | 7.12 (5.33 to 7.97) | 0 | 0 | 9.14 (8.59 to 9.91) | 0 | 7.7 (6.51 to 8.64) | NA |  |
| cand | 3.77 to 12.6 | 7.05 (4.01 to 7.96) | 0.00162 | 0.244 | 9.18 (8.59 to 10.4) | 0.0885 | 7.66 (6.39 to 8.7) | `>=` 10 (`>=` 10 to `>=` 10) | EC50 `>=` 100 (11.9 to `>=` 100) |
| cand, declared | 0 to 7.57 | 4.59 (0.366 to 7.58) | 0.115 | 0.244 | 10.2 (8.87 to 11.4) | 0.599 | 7.29 (5.74 to 8.19) | `>=` 10 (`>=` 10 to `>=` 10) | EC50 12.8 (11 to `>=` 100) |
| base, `regularizing` | 5.8 to 9.35 | 7.28 (6 to 7.97) | 0 | 0 | 9.06 (8.57 to 9.74) | 0 | 7.77 (6.56 to 8.67) | NA |  |
| cand, `regularizing` | 5.8 to 9.35 | 7.34 (5.92 to 7.97) | 0 | 0.205 | 9.04 (8.58 to 9.75) | 0.00475 | 7.84 (6.56 to 8.92) | `>=` 10 (`>=` 10 to `>=` 10) | EC50 `>=` 100 (`>=` 100 to `>=` 100) |

### Sampling diagnostics

| design | fit | divergent of 8000 | treedepth 10 reached | max R-hat | min bulk ESS | min tail ESS |
|---|---|---|---|---|---|---|
| `nec4param` complete | base | 0 | 0 | 1.002 | 2194 | 2739 |
| `nec4param` complete | cand | 0 | 0 | 1.002 | 2666 | 3059 |
| `nec4param` incomplete | base | 609 | 0 | 1.004 | 785 | 1145 |
| `nec4param` incomplete | cand | 343 | 0 | 1.010 | 472 | 295 |
| `nec4param` incomplete | cand, declared | 122 | 1 | 1.007 | 1456 | 1457 |
| `nec4param` incomplete | base, `regularizing` | 580 | 3 | 1.003 | 862 | 1254 |
| `nec4param` incomplete | cand, `regularizing` | 396 | 0 | 1.002 | 1268 | 1584 |
| `nec4param` above | base | 65 | 0 | 1.002 | 854 | 856 |
| `nec4param` above | cand | 162 | 0 | 1.005 | 573 | 709 |
| `nec4param` above | cand, declared | 3029 | 0 | 1.016 | 618 | 239 |
| `nec4param` above | base, `regularizing` | 16 | 148 | 1.003 | 931 | 434 |
| `nec4param` above | cand, `regularizing` | 37 | 632 | 1.005 | 1534 | 1955 |
| `ecx4param` complete | base | 0 | 0 | 1.001 | 5054 | 5165 |
| `ecx4param` complete | cand | 0 | 0 | 1.001 | 4654 | 4276 |
| `ecx4param` incomplete | base | 0 | 0 | 1.002 | 3142 | 2674 |
| `ecx4param` incomplete | cand | 0 | 0 | 1.003 | 3425 | 3351 |
| `ecx4param` incomplete | cand, declared | 0 | 0 | 1.001 | 3389 | 3349 |
| `ecx4param` incomplete | base, `regularizing` | 0 | 0 | 1.003 | 2933 | 3028 |
| `ecx4param` incomplete | cand, `regularizing` | 0 | 0 | 1.002 | 3314 | 3122 |
| `ecx4param` above | base | 19 | 0 | 1.003 | 1418 | 655 |
| `ecx4param` above | cand | 13 | 0 | 1.003 | 1513 | 659 |
| `ecx4param` above | cand, declared | 1 | 0 | 1.001 | 2311 | 2066 |
| `ecx4param` above | base, `regularizing` | 23 | 0 | 1.003 | 1420 | 542 |
| `ecx4param` above | cand, `regularizing` | 167 | 0 | 1.045 | 67 | 15 |

R-hat and ESS are taken over `top`, `bot`, `beta`, the threshold and `sigma`.
No fit failed, and the initial-value search found values for all four chains in
all 24, so none fell back to Stan's own initialisation.

## The tested-range bound

The two designs whose threshold lies above the series were fitted at defaults on
both builds. On the base, the bound determines the reported threshold on both. On the `nec4param` design above the series every `nec`
draw is below 10, and `summary()` reports an NEC of 3.29 (0.669 to 9.43) with no
mark, against a true `nec` of 15. On the `ecx4param` design above the series the
`ec50` posterior is 9.14 (8.59 to 9.91), with every draw below the true 10.58,
and `ecx()` returns `NA` for all three entries of the EC50.

On the candidate the bound no longer confines any draw to the series. At
defaults, 0.24 of the `nec` draws and 0.089 of the `ec50` draws lie above 10, and
the reported upper limits are marked `>= 10`. The central estimates on those two
designs are nonetheless still inside the series: an NEC of 4.45 and an `ec50` of
9.18, the latter with the true value at a posterior CDF of 0.986.

## The endpoint-centred `bot` prior

Both default `bot` entries are located from the observed response. Under
`"uninformative"` the gaussian entry is `normal(quantile(y, 0.1), 2.5 sd(y))`,
whose location on these designs falls among the observations at the highest
concentrations; under `"regularizing"` it is centred on the mean response at the
highest concentration, called the endpoint mean below. Both entries are the
same on the two builds, because #393 did not change them. On the three designs whose response stops short of the lower
asymptote the `"uninformative"` entry places the true `bot` of 2 in its far lower
tail: a prior CDF at the truth of 0.00139
under `normal(8.65, 2.22)` on the `nec4param` incomplete design, 0.00304 under
`normal(8.21, 2.26)` on the `ecx4param` design above, and 1e-07 on the flat
`nec4param` design. The posteriors follow the prior. `bot` is 7.48 (4.83 to 8.00),
7.05 (4.01 to 7.96) and 10.2 (7.24 to 12.1) on the candidate, and at most 0.0019
of the draws lie at or below the truth. On the two designs with a partial
decline the data add only an upper limit, near the observed mean at 10; on the
flat design they add nothing about `bot` below `top`. The posterior standard
deviation is 0.38 to 0.79 of the prior's.

The same prior determines the threshold where the threshold is not observed.

- On the `ecx4param` design above the series, a `bot` near 7 puts the response
  midway between `top` and `bot` near 8.5. The observed response passes that
  level between 9 and 10, so the `ec50` posterior stays inside the series. Under
  the declaration the same data give an `ec50` of 10.2 (8.87 to 11.4), with 0.599
  of the draws above 10.
- On the flat `nec4param` design, the default posterior explains the absence of
  any decline with a `bot` equal to `top` (10.2 against 9.95), which makes `nec`
  irrelevant to the fit. The `nec` posterior is then close to its prior: its
  standard deviation is 1.06 times the prior's, and the share above 10, 0.24, is
  close to the prior's 0.244. Under the declaration a `bot` near `top` is in the
  upper tail of the prior, so the fit explains the flat response with a `nec`
  above the series instead. 0.626 of the draws lie above 10 and the reported NEC
  is `>= 10` (1.33 to `>= 10`). That fit is the one whose transitions were more
  than a third divergent, so the share is approximate.
- On the `nec4param` incomplete design the `nec` median is 9.40 to 9.42 in all
  five fits, against a true 9.46, with every 95% interval inside 8.93 to 9.95,
  because the response is flat to 9 and falls at 10. The `bot` prior changes
  `bot` and `beta` there, not `nec`.

Under the declaration the `bot` posterior is 4.52 (0.293 to 7.77),
5.94 (0.457 to 11.7) and 4.59 (0.366 to 7.58) on the three designs, and each
interval contains the truth. The posterior standard deviation is 1.01 to 1.16
times the prior's, so the data do not narrow the declared prior at all. The
interval contains 2 because the declared prior spans from the floor of zero to
the endpoint mean, not because the data locate `bot`. The unobserved part of
each curve is no better determined than before; the declared fits report that
it is undetermined, where the default fits reported a narrow interval that
excludes the truth.

The absolute EC50 on the `ecx4param` design above the series shows the
consequence for a reported estimate. The curve reaches half the control mean,
about 4.96, only where `bot` lies below it. With the default `bot` posterior near
7, 7,452 of 8,000 candidate draws never reach it by a concentration of 100, and
the EC50 on that extended grid is `>= 100` (11.9 to `>= 100`). Under the
declaration 3,440 of 8,000 do not reach it, and the EC50 is 12.8 (11.0 to
`>= 100`), against a true 11.09. Both figures are set by the `bot` prior; neither
is an estimate of the curve beyond 10 that the data support alone.

On the `ecx4param` incomplete design the response reaches 92% of the span, and
the default prior was already wide enough: `bot` is 2.66 (1.88 to 3.22) at
defaults and 2.49 (1.69 to 3.07) declared, with a posterior standard deviation
0.45 of the declared prior's. The medians of the `ec50`, the NSEC and the EC50
agree across all five fits to within 0.08.

## The weakly identified parameters

- `bot`, on every design that stops short of the lower asymptote, as above. On
  the `ecx4param` incomplete design the observed curvature narrows it.
- `beta`, the log decay rate of `nec4param`, on the incomplete design: 3.43
  (−0.378 to 10.9) at defaults against a true −0.182, with a posterior standard
  deviation 0.60 of the prior's. One declining concentration fixes the mean
  there, which any combination of `bot` and decay rate can meet. Under the
  declaration it is 0.271 (−1.12 to 7.98). On the flat design the ratio is 0.82
  to 1.05.
- `nec` on the flat design, whose posterior is close to its prior and is
  located by the `bot` prior as described above.
- `ec50` on the `ecx4param` design above the series. Its posterior is narrow,
  0.057 of the prior's standard deviation at candidate defaults, but located by
  the `bot` prior and not by the data.

`top`, `sigma`, `nec` on the `nec4param` incomplete design and every parameter
of both complete designs are identified: posterior standard deviations are at
most 0.3 of the prior's for `top` and 0.13 for those thresholds.

## Sensitivity to `prior_type`

The four incomplete designs were fitted under `"regularizing"` on both builds.
That set centres the default `bot` prior on the endpoint mean and narrows it:
a central 95% of 5.83 to 9.32 on the `nec4param` incomplete design against 4.29
to 13 under `"uninformative"`, and 9.34 to 11.6 on the flat design against 6.58 to
12.1. Its `nec` and `ec50` entry is a narrower lognormal, with 0.205 of its mass
above 10 against 0.244. On the three designs that stop short of the asymptote no
posterior draw of `bot` lies at or below the truth under `"regularizing"`, on
either build. The share of threshold draws above 10 on the candidate falls from
0.24 to 0.079 on the flat `nec4param` design and from 0.089 to 0.0048 on the
`ecx4param` design above the series. The reported NEC on the flat design is 3.59
(0.879 to `>= 10`), with 629 draws above 10 rather than 1,916.

On the `ecx4param` incomplete design `prior_type` changes no posterior or
reported median by more than 0.02, and on the `nec4param` incomplete design it
leaves the `nec` median at 9.41.

## The complete-response comparison

The two builds fit different Stan programs on the complete designs. The
threshold is declared with an upper bound of 10 on the base and without one on
the candidate. That changes its unconstrained parameterisation, and therefore
the draws. The target density differs as well, by whatever posterior mass the
bound excluded. No candidate threshold draw lies above 10 on either complete
design, so that mass is below 1 in 8,000, and the two posteriors differ by
sampling.

The measured differences are consistent with that. Across the five parameters of
the two designs the largest difference in a posterior median is 0.0088, for
`top` on `ecx4param`. The `nec` medians are 4.1033 and 4.1056, each with a Monte
Carlo standard error of 0.0024, and the `ec50` medians 5.0287 and 5.0256, with
0.0013 and 0.0015. The reported NEC and EC50 medians differ by at most 0.005.
The NSEC of `ecx4param` differs more: its lower limit is 1.16 and 1.27, with
Monte Carlo standard errors of 0.039 and 0.060, and its median 2.32 and 2.34,
with 0.0054 and 0.0033. The median difference is about three combined standard
errors, computed on the pooled draws. The candidate's `nsec()` applied to the base fit returns the base
build's figures to every digit, so the difference lies between the two posterior
samples and not in the estimator; one pair of fits cannot resolve it further.
Neither complete fit has a divergent transition.

What does change on the complete designs is the prior. The prior CDF at the true
threshold falls from 0.474 to 0.359 for `nec` and from 0.606 to 0.458 for `ec50`,
because the base figure is taken after truncation. That is the change part 4 of
`notes/prior_audit.md` records over the sweep, and the fits show it does not
reach the posterior on these designs.

## Sampling on the incomplete designs

Divergent transitions, R-hat and ESS were read from every fit. Divergent
transitions occur on both builds wherever a design leaves `nec4param`
or `ecx4param` partly unidentified, so they are not introduced by PR #409. The
`nec4param` incomplete design has 122 to 609 of 8,000 in all five fits, with a
maximum R-hat of 1.010 and a smallest tail ESS of 295. Removing the bound
changed the count from 609 to 343 on that design and from 65 to 162 on the flat
design, one fit each.

Two fits did not sample adequately. The declared fit of the flat `nec4param`
design has 3,029 of 8,000 transitions divergent, R-hat 1.016 and a tail ESS of
239, so its share of 0.626 above 10 is not a reliable figure. The candidate
`"regularizing"` fit of the `ecx4param` design above the series has 167
divergent transitions, 125 of them in one chain. Its `beta` has an R-hat of
1.045, a bulk ESS of 67 and a tail ESS of 15, while its `ec50` has a bulk ESS of
2,938. The base fit of the same design under `"regularizing"` has 23
divergent transitions and a smallest tail ESS of 542. The two `"regularizing"`
fits of the flat design reached the maximum tree depth in 148 and 632
iterations.

The flatness report of #390 was raised on the `nec4param` incomplete and the
`ecx4param` above designs, on both builds. On the candidate it closes by
recommending `asymptote_observed = FALSE`, and it does so in the fits that had
already declared it. It was not raised on the `ecx4param` incomplete design,
whose observed mean falls from 3.59 to 3.09 between 9 and 10. The flat design
raised the existing warning that the response does not decline instead.

## Limitations

- One data seed, and one residual vector shared by the six designs. Whether an
  interval contains a true value is one realisation, not a coverage rate.
- The gaussian family only, on the identity link, with a response that is
  positive throughout. Every other family is unmeasured: `Beta`, `binomial`,
  `beta_binomial`, `bernoulli`, `Gamma`, `poisson`, `negbinomial` and the hurdle
  and zero-inflated families. So are a gaussian response spanning negative
  values, for which the declaration is refused, and every non-identity link.
- Two of the 23 equations, one series of eleven evenly spaced concentrations by
  six replicates, and the predictor on its recorded scale. A predictor given as
  `crf(log(x))` takes the normal branch of the threshold prior, which has no
  bound at all, and is unmeasured. So are model sets, the model weights of a
  model-averaged fit, and `bnec_group()`.
- The complete-design comparison is between unpaired samples, and its smallest
  differences are of the order of their Monte Carlo error.
- The fits show where each prior determines a result and where the data do. A
  declared fit whose interval contains the true `bot` or `ec50` does not show
  that the part of the curve beyond the series can be recovered from the data.

## Reproduction

The script's header gives the build and library mechanics. In short, each commit
was installed from a detached worktree with
`R CMD INSTALL --library=<scratch>/lib-base` or `lib-cand`, its commit was
written to `bayesnec.commit` in that library, and every fit prepends its library
with `.libPaths()` and asserts `find.package("bayesnec")` and the commit before
fitting. From the repository root, with `V418_LIB_BASE` and `V418_LIB_CAND` set:

```
Rscript notes/scripts/incomplete_design_fits.R list |
  xargs -P 8 -n 1 Rscript notes/scripts/incomplete_design_fits.R fit
Rscript notes/scripts/incomplete_design_fits.R report
Rscript notes/scripts/incomplete_design_fits.R markdown
```

Each fit runs in its own `Rscript` process with its chains in sequence. The
`fit` mode saves the fitted object beside its record, and `extract <id>`
rebuilds the record from it without refitting; the records behind the tables
were rebuilt that way once the script was final. The `markdown` mode prints the
per-design and diagnostics tables from the records.

Run on 2026-09-26 with R 4.6.1, brms 2.23.0 and rstan 2.32.7, eight fits at a
time. The 24 fits took 1,667 s of wall clock on a 22-core workstation shared with
other jobs, whose load average was near 20 throughout. The elapsed time recorded
for each fit reflects that load as much as the fit. Data seed and fit seed are
both 418. The per-fit records and fitted objects were written to
`ignore/incomplete_design_fits` and are not committed.
