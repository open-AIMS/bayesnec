<!-- Archived from a working directory on 2026-09-10. -->

> **Provenance.** Produced by `notes/scripts/prior_audit.R` against `dev` at
> `eebccdb3`, R 4.6.1, brms 2.23.0, 2026-09-09. Section 1 below describes the
> `nec` and `ec50` prior as it was then; that prior was replaced in #302 and
> PR #304, so section 1 is a record of the defect rather than of current
> behaviour. Section 2 was fixed in the same PR. The `top` and `bot` findings in
> section 3 and in part 2 are open as #305 and still hold.
>
> **Two design types this sweep does not contain** --- complete effect at the
> highest concentration, so that the top-dose group is entirely zero, and a
> hurdle or zero-inflated fit whose survival declines with concentration --- are
> measured by `notes/scripts/prior_hard_cases.R`, written for the review of
> PR #307. Both exposed defects this sweep could not. Note also that this script
> draws a new response inside its `prior_type` loop, so a ratio taken across the
> two prior types compares priors built from different draws; the coverage
> results are unaffected, because each prior is scored against its own data.

# Default priors in bayesnec: an assessment against known parameter values

`dev` at `eebccdb3`, R 4.6.1, brms 2.23.0, 2026-09-09. Nothing was fitted.
Priors were obtained from `get_priors(formula, data, family, prior_type)`,
which runs the same `check_data()` and `define_prior()` sequence `bnec()` runs.

## What was run

Response vectors were simulated from `nec4param` and `ecx4param` curves with
known `top`, `bot`, `nec`/`ec50` and a decay rate set from the transformed
predictor range, so that every design descends from `top` to within 5% of
`bot` at the highest dose. `bot` is therefore identified by the data in every
cell, and a prior that cannot locate it is a property of the prior rather than
of the design.

The factorial is 5 designs x 3 predictor transforms x 12 families x 2 links
x 2 prior types x 2 equations = 4,320 parameter-prior pairs. `top` and `bot`
are reported from the `nec4param` cells and `ec50` from the `ecx4param` cells,
so that each parameter is assessed on the equation whose curve identifies it.

| factor | levels |
|---|---|
| design (recorded dose) | `linear` 0(1)10; `linear_unit` 0(0.1)1; `log_2fold` 0, 0.078 ... 10; `log_unit` 0, 0.0039 ... 1; `log_wide` 0, 0.01 ... 20 (nassarius contaminant A, #302) |
| predictor transform | `x`, `sqrt(x)`, `log(x)` (control replaced by half the lowest non-zero dose) |
| family | all 12 in `mod_fams` |
| link | `identity`, plus `log` or `logit` as the family allows |
| prior type | `uninformative`, `regularizing` |

Each prior is truncated to its own `lb`/`ub` before being summarised, since
that is the density the sampler receives. The reported quantity is
**`p_truth`**, the truncated prior CDF evaluated at the true parameter value.
`p_truth` near 0.5 means the prior is centred on the truth; near 0 or 1 means
the truth sits in a tail; exactly 0 or 1 means the prior's support excludes it.
The true value is taken on the scale the parameter is fitted on: the transform
of the dose for `nec`/`ec50`, and the link of the response for `top`/`bot`.

## 1. The `nec` and `ec50` prior

The prior is identical across all 12 families, both links and both prior types
(one distinct string per design x transform cell, over 48 cells each). It is a
function of the transformed predictor alone. Three branches are selected by
`set_distribution()`:

| transformed predictor | branch |
|---|---|
| non-negative, maximum above 1 | `gamma(5, 4/m)`, m the median distinct value |
| within [0, 1] | `beta(2, 2)` |
| contains negatives (any `log(x)`) | `normal(median(x), 10 sd(x))` |

Truncated to `[min(x), max(x)]` in all three.

### The Gamma branch reaches a fixed multiple of the median dose

`gamma(5, 4/m)` has its mode at *m* and its 95% range at 0.41*m* to 2.56*m*,
whatever the data. The prior is therefore adequate only where the highest dose
tested is within about 2.6 times the median dose. That ratio is a property of
the design:

| design | max(x)/m, recorded | max(x)/m, after sqrt |
|---|---|---|
| `linear` | 2.0 | 1.4 |
| `log_2fold` | 16.0 | 4.0 |
| `log_unit` | 21.3 | 4.7 |
| `log_wide` | 125.0 | 11.2 |

A series spaced evenly from zero has a ratio near 2 by construction. Every
log-spaced series exceeds the prior's reach, by an order of magnitude or more
on the recorded scale.

Measured against known values (`nec4param` and `ecx4param`, one representative
family; the prior is family-invariant):

| design | transform | par | prior | bounds | truth | mode | 95% range | p_truth |
|---|---|---|---|---|---|---|---|---|
| linear | x | nec | gamma(5, 0.8) | [0, 10] | 4 | 5 | 1.97-9.55 | 0.244 |
| linear | sqrt(x) | nec | gamma(5, 1.79) | [0, 3.16] | 2 | 2.24 | 0.82-3.10 | 0.434 |
| linear | log(x) | nec | normal(1.61, 9.24) | [-0.69, 2.3] | 1.39 | 1.61 | -0.62-2.23 | 0.692 |
| log_2fold | x | nec | gamma(5, 6.4) | [0, 10] | 1.25 | 0.625 | 0.25-1.60 | 0.900 |
| log_2fold | sqrt(x) | nec | gamma(5, 5.06) | [0, 3.16] | 1.12 | 0.79 | 0.32-2.02 | 0.667 |
| log_2fold | x | ec50 | gamma(5, 6.4) | [0, 10] | 2.5 | 0.625 | 0.25-1.60 | 0.9996 |
| log_wide | x | nec | gamma(5, 25) | [0, 20] | 1.25 | 0.16 | 0.065-0.41 | 1.0000 |
| log_wide | sqrt(x) | nec | gamma(5, 10) | [0, 4.47] | 1.12 | 0.40 | 0.16-1.02 | 0.987 |
| log_wide | log(x) | nec | normal(-1.83, 24.2) | [-5.3, 3] | 0.223 | -1.83 | -5.09-2.79 | 0.668 |
| log_wide | x | ec50 | gamma(5, 25) | [0, 20] | 2 | 0.16 | 0.065-0.41 | 1.0000 |

### Reverting the rate to 2/m does not resolve the log-spaced case

`2/m` gives a mode at 2*m* and a 95% range of 0.81*m* to 5.12*m*, so it reaches
5.1 times the median rather than 2.6. Both rates were evaluated on the same
simulated series:

| design | transform | par | truth | p_truth, 4/m (dev) | p_truth, 2/m (CRAN) |
|---|---|---|---|---|---|
| linear | x | nec | 4 | 0.244 | 0.064 |
| linear | x | ec50 | 5 | 0.412 | 0.142 |
| log_2fold | x | nec | 1.25 | 0.900 | 0.371 |
| log_2fold | x | ec50 | 2.5 | 0.9996 | 0.900 |
| log_2fold | sqrt(x) | nec | 1.12 | 0.667 | 0.174 |
| log_wide | x | nec | 1.25 | 1.0000 | 0.9995 |
| log_wide | x | ec50 | 2 | 1.0000 | 1.0000 |
| log_wide | sqrt(x) | nec | 1.12 | 0.987 | 0.656 |
| log_wide | sqrt(x) | ec50 | 1.41 | 0.998 | 0.833 |

On `log_wide` the true NEC lies beyond the 99.9th percentile of both priors.
`2/m` is less wrong there, not right. On the linear design `2/m` places its
mode at 10, which is the upper truncation bound exactly, reproducing what #273
reported.

### The other two branches

`beta(2, 2)` reads nothing from the data. Its mode is at 0.5 of the unit
interval whether the doses are spread evenly across it (`linear_unit`,
p_truth 0.35 for a true NEC of 0.4) or concentrated at the bottom (`log_unit`,
p_truth 0.16 for a true NEC of 0.25). It cannot collapse onto the wrong scale
because it never reads one, and it cannot adapt for the same reason.

`normal(median(x), 10 sd(x))` truncated to the predictor range is close to flat
over that range. On all four log-transformed designs p_truth fell between 0.63
and 0.89. A log-transformed predictor is not affected by the Gamma branch's
behaviour, because it does not use it.

## 2. `top` and `bot` under a non-identity link on a hurdle or zero-inflated family

`hurdle_mu_family()` (`R/hurdle_family.R:50`) returns `Gamma(link = "identity")`
or `Beta(link = "identity")` regardless of the link the caller supplied, so
`define_hurdle_prior()` builds the mu-block priors as though the fit were on
the identity link. The non-hurdle path handles this at `R/define_prior.R:140`
by rewriting `fam_tag` to `"gaussian"` and calling `response_link_scale()`.

Confirmed against the generated Stan code, which for
`zero_inflated_beta(link = "logit")` emits

```
mu[n] = inv_logit(nlp_bot[n] + (nlp_top[n] - nlp_bot[n]) * exp(...));
```

so `top` and `bot` are on the logit scale, while the priors returned are

```
beta(5, 2)  top  lb 0  ub 1
beta(2, 5)  bot  lb 0  ub 1
```

The same call on the non-hurdle `Beta(link = "logit")` correctly returns
`normal(2.93, 6.28)` and `normal(-3.05, 6.28)` with no bounds.

Consequence, over all 15 design x transform cells:

| family | link | par | truth (link scale) | prior | p_truth |
|---|---|---|---|---|---|
| zero_inflated_beta | logit | top | 2.20 | beta(5, 2) or beta(5, 1) on [0, 1] | 1.0000 in 15/15 |
| zero_inflated_beta | logit | bot | -2.94 | beta(2, 5) or beta(1, 5) on [0, 1] | 0.0000 in 15/15 |
| hurdle_gamma | log | top | 2.30 | gamma(5, ~0.30-0.39) on [0, Inf) | 0.0007-0.0024 in 15/15 |
| hurdle_gamma | log | bot | 0.693 | gamma(2, ~0.24-0.93) on [0, Inf) | 0.013-0.14, outside the band in 6/15 |

The `zero_inflated_beta` case is not a misplaced prior but an impossible one:
the support is `[0, 1]` on a parameter that must be negative for any `bot`
below 0.5 and above 1 for any `top` above 0.73. The `hurdle_gamma` case
constrains `top` to be non-negative on the log scale, which excludes any
control mean below 1.

The second block is unaffected: `link_hu` and `link_zi` are required to be
`identity`, and `zitop`/`zibot` are correctly bounded to [0, 1].

## 3. The regularizing `bot` prior is anchored on the sample minimum

Under `prior_type = "regularizing"`, `bot` is `gamma(5, 5/(min(y) + min(y>0)/10))`
on the identity link, whose mode is about 0.8 times the smallest observation.
For a continuous response that is defensible. For a count response the smallest
observation is well below the asymptote it is meant to locate: at a true `bot`
of 5 the observed minimum was 1 to 3 across cells.

| family | link | prior_type | truth | prior (range over cells) | mode range | p_truth range | cells outside 0.02-0.98 |
|---|---|---|---|---|---|---|---|
| poisson | identity | regularizing | 5 | gamma(5, 1.14) .. gamma(5, 4.55) | 0.88-3.52 | 0.670-1.000 | 9/15 |
| negbinomial | identity | regularizing | 5 | gamma(5, 1.14) .. gamma(5, 4.67) | 0.86-3.52 | 0.670-1.000 | 12/15 |
| zero_inflated_poisson | identity | regularizing | 5 | gamma(5, 1.30) .. gamma(5, 5.82) | 0.69-3.08 | 0.775-1.000 | 11/15 |
| zero_inflated_negbinomial | identity | regularizing | 5 | gamma(5, 1.14) .. gamma(5, 5.95) | 0.67-3.51 | 0.672-1.000 | 10/15 |
| poisson | log | regularizing | 1.61 | normal(-3.51, 1.23) .. normal(1.61, 0.59) | -3.51-1.61 | 0.500-1.000 | 1/15 |
| negbinomial | log | regularizing | 1.61 | normal(-4.61, 1.36) .. normal(1.79, 0.62) | -4.61-1.79 | 0.384-1.000 | 6/15 |
| zero_inflated_poisson | log | regularizing | 1.61 | normal(-3.91, 2.25) .. normal(-3.91, 3.46) | -4.61 to -3.22 | 0.945-0.993 | 5/15 |
| zero_inflated_negbinomial | log | regularizing | 1.61 | normal(-4.61, 3.64) .. normal(-3.91, 2.60) | -4.61 to -3.00 | 0.956-0.983 | 4/15 |
| gaussian | log | regularizing | 0.693 | normal(-0.89, 0.69) .. normal(0.57, 0.56) | -0.89-0.57 | 0.585-0.989 | 2/15 |

A log-linked gaussian shows the same mechanism at lower severity, since the
minimum of a continuous response is closer to its asymptote. The count of
failing cells varies with the draw, because `min(y)` is a random variable; the
direction does not. The equivalent `uninformative` prior,
`gamma(2, 2/q25)`, failed in none of the 15 cells for any of these families.

On the log link the same anchoring interacts with `response_link_scale()`,
which maps a zero to `min(y > 0)/100`, two orders of magnitude below anything
observed; `min(log y)` is then about -4.6 against a true `bot` of log(5) = 1.61.

## 4. Behaviour that was checked and found sound

- `top` on `gaussian`, `Gamma`, `poisson` and `negbinomial`, both links, both
  prior types: p_truth between 0.02 and 0.98 in 59 of the 60 cells, and between
  0.4 and 0.6 for every `uninformative` set. The exception is `negbinomial`,
  identity link, `regularizing`, on the `linear` design: `gamma(5, 0.0365)`
  against a true `top` of 40 gives p_truth 0.017. That prior is built from
  `max(y)`, which for a negative binomial with mean 40 and size 5 overshoots.
- Every failing non-hurdle cell in the whole run is a `regularizing` `bot`, plus
  the single `negbinomial` `top` cell above. No `uninformative` cell on a
  non-hurdle family falls outside the band.
- All `uninformative` `top`/`bot` priors on non-hurdle families: no cell outside
  the band.
- `binomial`, `beta_binomial`, `bernoulli`, `Beta` on the identity link: fixed
  `beta(5, 2)`/`beta(2, 5)` and `beta(5, 1)`/`beta(1, 5)`. These read nothing
  from the data. Against a true top of 0.95 and bot of 0.05 they give p_truth
  0.967 and 0.033, inside the band but at its edge; a control response at 1.0
  and a floor at 0 would sit on the boundary.
- `binomial` and `beta_binomial` on the logit link, `regularizing`: `top` is
  `normal(7.60, ~3-4.8)`. The 7.60 is the logit of the value
  `response_link_scale()` clamps a response of 1 to, not a feature of the data.
  p_truth 0.06-0.17 against a true top of logit(0.95) = 2.94; wide enough not to
  exclude the truth, but located by the clamp constant.
- `nec`/`ec50` priors do not vary with family, link or prior type. Confirmed
  over all 48 family x link x prior-type combinations within each of the 15
  design x transform cells.
- The hurdle path primes the mu block from the non-zero response only, so its
  predictor vector excludes doses at which every response is zero. This changed
  the `nec` prior's location parameter on the log-transformed designs by one
  dose step in a minority of cells. That is the documented intent of #269, not
  a defect.

---

# Part 2: how the two prior types are defined, and what consistency would require

## The JSS specification, and where the code departs from it

The JSS article (Fisher et al. 2024, sections "Priors for response-scaled
parameters" and "Priors for predictor-scaled parameters") describes one prior
set. `master` at `6f6bd25d` (version 2.1.3.1, the CRAN release) has no
`prior_type` argument at all. Dev's `uninformative` set is that set;
`regularizing` is entirely new and unreleased.

| parameter | response or predictor type | JSS text | master code | dev code |
|---|---|---|---|---|
| top / bot | unbounded on the link scale: gaussian, or any family under `log` or `logit` | normal, mean at q90 / q10, sd 2.5 sd(y) | same | same |
| top / bot | positive: Gamma, poisson, negbinomial | gamma, shape 2, **mean** at q75 / q25 | same | same, with `positive_scale()` replacing the raw quantile |
| top / bot | [0, 1]: bernoulli, binomial, beta_binomial, Beta, identity link | fixed `beta(5, 2)` / `beta(2, 5)` | same | same |
| nec / ec50 | predictor strictly positive | gamma, shape 5, **maximum density** at median(x) | `gamma(5, 2/m)`: mode 2m, mean 2.5m | `gamma(5, 4/m)`: mode m, mean 1.25m |
| nec / ec50 | predictor in [0, 1] | fixed `beta(2, 2)` | same | same |
| nec / ec50 | predictor unbounded | normal, mean median(x), sd 10 sd(x) | same | same |

`positive_scale()` returns the raw quantile whenever the response contains no
zeros, so the weak set on dev reproduces CRAN except on a zero-inflated
response.

The one row where the code and the article disagree is `nec`/`ec50`. The
article states maximum density at the median predictor. `2/m` puts the maximum
density at `2m` and the mean at `2.5m`, which is neither. `4/m` puts the
maximum density at `m`, which is what the article states. So #273 moved the
code towards the published description, and the statement in #302 that "CRAN
and the JSS paper describe `2/m`" holds for the code but not for the paper.

Reading `mu` in "maximum density (mu, see above)" as the mean instead, since
`mu` was defined as the mean two paragraphs earlier, gives `gamma(5, 5/m)`,
which the code has never implemented. Under either reading the released rate is
not the documented one.

## What the two sets actually are, on a common footing

Each prior is a location and a spread. The location was recovered by matching
the prior's mean and mode against the six response quantiles `define_prior()`
reads, over all 15 design x transform cells, rather than asserted from the
source (`prior_definitions.csv`, columns `anchor_of_mean`, `anchor_of_mode`).

| branch | families | weak location | weak spread | regularizing location | regularizing spread |
|---|---|---|---|---|---|
| normal | gaussian on identity; every family under `log` or `logit` | mean at q90 / q10 | 2.5 sd(y) | mean at max / min | 1.0 sd(y) |
| gamma | Gamma, poisson, negbinomial, zero-inflated counts, on identity | mean at q75 / q25 | shape 2, so sd = 0.71 x mean | mean at max / min | shape 5, so sd = 0.45 x mean |
| beta | bernoulli, binomial, beta_binomial, Beta on identity; zero_inflated_beta on any link | none, fixed | none, fixed | none, fixed | none, fixed |

## Four ways the definitions differ across families

**The location statistic changes with the branch within one prior type.** The
weak set uses q90/q10 in the normal branch and q75/q25 in the gamma branch. The
article gives no reason for the difference and both work in the audit.

**The beta branch reads nothing from the response.** It is the only branch
whose prior is a constant. "Regularizing" therefore means "centre on the
observed extreme" for a Gamma or gaussian response and "use `beta(5, 1)`" for a
Beta response, which are different in kind rather than in degree.

**The gamma entries are specified by their mean and the normal entries by a
parameter that is also their mode, so the same stated rule peaks in different
places.** `gamma(2, 2/q75)` has its maximum density at `q75/2`, while
`normal(q90, 2.5 sd)` peaks at `q90`. Measured on the same simulated response
with a true `top` of 10: the weak gamma prior peaks at 4.4 to 5.4, the weak
normal prior at 10.2 to 10.7. Both cover the truth, because the gamma prior is
wide, but they do not implement one rule. This is the same mean-against-mode
ambiguity that produced #273 and #302 on the predictor side.

**The separation between the two prior types is not the same size in different
families.** Prior standard deviation as a fraction of the observed response
range, averaged over the 15 cells, for `top`:

| branch | weak | regularizing | weak / regularizing |
|---|---|---|---|
| normal (gaussian identity; anything on log or logit) | 0.52-1.12 | 0.21-0.46 | 2.39-2.64 |
| gamma (Gamma, poisson, negbinomial, zero-inflated counts, identity) | 0.33-0.56 | 0.45-0.50 | 0.73-1.22 |
| beta (0-1 families, identity) | 0.16 | 0.14 | 1.13 |

For the families that route to the normal entries, `regularizing` is about 2.5
times narrower than `weak`, as intended. For the gamma branch it is the same
width to within 20%, and for `negbinomial` and `zero_inflated_negbinomial` it is
**wider** (ratios 0.73 and 0.80). For the beta branch the two differ by 13%. So
for two of the three branches `regularizing` relocates the prior rather than
narrowing it.

## What consistency would require

Three changes, in increasing order of what they alter relative to JSS.

**1. Correct the two defects that make the weak set wrong for particular
family and link combinations.** Neither is a definitional question.
`hurdle_mu_family()` (`R/hurdle_family.R:50`) must carry the caller's mean link
instead of hard-coding `identity`, so that `hurdle_gamma(link = "log")` and
`zero_inflated_beta(link = "logit")` route to the normal entries the way every
other family does. The JSS text already specifies this: it puts "any response
variable for which the link ensures valid values from minus infinity to
infinity, including log and logit" in the normal branch. This restores 60 of
the 4,320 rows to the published specification and removes the only cases where
a prior's support excludes the truth. Nothing about the weak set's definition
changes.

**2. State the contract for both prior types once, and derive each branch from
it.** A prior type would be defined by a location quantile and a spread
multiple, both family-independent, and each branch would use whichever
distribution matches the parameter's support, with its parameters set so that
its **mode** is the location and its standard deviation is the spread. For a
gamma with mode L and sd S the shape solves `sqrt(s)/(s - 1) = S/L`, which has
the closed form `sqrt(s) = (L/S + sqrt(L^2/S^2 + 4))/2`, and the rate is then
`(s - 1)/L`. For a beta on the unit interval the two shape parameters are
determined the same way, numerically. This is the change that makes "weak" and
"regularizing" mean the same thing in every family, and it is what would stop a
family added later from drifting.

Adopting it with weak = (q90/q10, 2.5 sd) and regularizing = (max/min, 1.0 sd)
would change released numbers for the gamma branch: its weak location moves
from a mean at q75 to a mode at q90, and its spread from 0.71 times its own
mean to 2.5 times sd(y). Keeping the released numbers instead means keeping two
location quantiles and two spread rules, and stating in the documentation that
the branches differ.

The middle option preserves JSS locations and fixes only the specification
convention: keep q75/q25 for the gamma branch and q90/q10 for the normal
branch, but set the gamma's **mode** to its location rather than its mean. That
changes `gamma(2, 2/q75)` to a gamma peaking at q75. It leaves the location
statistic released with CRAN untouched and removes the mean-against-mode
ambiguity in one place.

**3. Decide whether the beta branch should read the response.** The article's
justification -- that a 0-1 bounded parameter needs no scaling -- is a statement
about support, not about location. It is why `regularizing` currently does
nothing for those families: `beta(5, 1)` against `beta(5, 2)` is a 13% change in
width and no change in what the prior is anchored to. If `regularizing` is to
mean the same thing for a binomial response as for a Gamma one, the 0-1 branch
has to be given a location taken from the response, at which point the mode-and-
sd construction above supplies the shape parameters. If it is to stay fixed,
that should be recorded as a deliberate exception rather than left as an
asymmetry.

Separately from consistency, the audit found one substantive failure inside the
regularizing set: anchoring on `max(y)` and `min(y)` is a biased anchor for a
discrete response, since the smallest count observed sits well below the
asymptote it is meant to locate. Anchoring the regularizing set on an extreme
quantile such as q95/q05 rather than the extremum would address that without
changing anything about the weak set, which is unaffected because it already
uses interior quantiles.
