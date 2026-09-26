<!-- Archived from a working directory on 2026-09-10. -->

> **Provenance.** Produced by `notes/scripts/prior_audit.R` against `dev` at
> `eebccdb3`, R 4.6.1, brms 2.23.0, 2026-09-09. Section 1 below describes the
> `nec` and `ec50` prior as it was then; that prior was replaced in #302 and
> PR #304, so section 1 is a record of the defect rather than of current
> behaviour. Section 2 was fixed in the same PR. The `top` and `bot` findings in
> section 3 and in part 2 were opened as #305; part 3 below re-ran the same
> cells on 2026-09-19 and found 2 of 720 outside the band against the counts
> section 3 reports, so they describe `eebccdb3` and not current behaviour.
> Read part 3 for what the priors do now.
>
> **Two design types this sweep does not contain** --- complete effect at the
> highest concentration, so that the top-dose group is entirely zero, and a
> hurdle or zero-inflated fit whose survival declines with concentration --- are
> measured by `notes/scripts/prior_hard_cases.R`, written for the review of
> PR #307. Both exposed defects this sweep could not. Note also that this script
> draws a new response inside its `prior_type` loop, so a ratio taken across the
> two prior types compares priors built from different draws; the coverage
> results are unaffected, because each prior is scored against its own data.
>
> **The two summary tables cited below** are archived beside this file in
> `notes/prior_audit/`. The full per-cell sweep they were reduced from was not
> kept; re-running the script against `eebccdb3` reproduces it.
>
> **Part 3 is a later run** of the same script, extended for #391 with designs
> that do not reach the lower asymptote. It was produced against `dev` at
> `441e7464` on 2026-09-19. Its complete-design cells do not reproduce the
> figures in parts 1 and 2 above, because the priors those parts describe were
> changed by #302, PR #304 and the work that followed.
>
> **Part 4 is the same script run twice** on 2026-09-22, before and after the
> change #393 makes to the `nec` and `ec50` bounds, and reports the paired
> difference. Its before run reproduces part 3 exactly. Read part 4 for the
> bounds on those two entries now; part 3's threshold tables describe the
> truncated entries that #393 replaced.

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
source (`notes/prior_audit/prior_definitions.csv`, columns `anchor_of_mean`,
`anchor_of_mode`).

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

---

# Designs that do not reach the lower asymptote

Part 3 of this record. `dev` at `441e7464`, R 4.6.1, brms 2.23.0, MASS 7.3.65,
2026-09-19. No concentration-response model was fitted and no Stan program was
compiled; parts 3 and 4 below fit generalised linear models, which is what the
rule they measure does. Produced by `notes/scripts/prior_audit.R`, which now
runs in four parts named `sweep`, `reproduction`, `calibration` and `miss`, and
takes about forty minutes in full. The figures #386 quotes were produced by a
script held in a session scratch and never committed; the reproduction part is a
committed reproduction of them.

## The completeness axis

Every cell of the factorial in part 1 above is now run at four settings of how
far the mean response has travelled from `top` towards `bot` at the highest
concentration tested. The complete setting is the one the sweep has always used,
a decay rate of 5 over the distance from the threshold to the highest dose,
which leaves `exp(-5)` of the span untravelled. The three incomplete settings
are #386's, at 0.92, 0.36 and 0.02 of the span.

The setting is reached by holding the series and the decay rate and placing the
true threshold along the series, not by dropping the design's own doses above a
cut point. Dropping doses gives no control over the fraction reached on an
evenly spaced series --- on `linear` the cut for 0.02 falls between the fourth
and fifth dose, whose realised fraction is zero --- and it changes the number of
concentrations and the ratio of the maximum to the median dose, which is the
sole input to the gamma branch of the `nec` prior described in part 1 section 1.
Placing the threshold holds every predictor-derived prior input constant across
the four settings and is the situation #386 describes: a fixed dilution series
against a sample more or less toxic than the one the series was designed for.
The threshold that reaches a fraction *f* at the highest dose is
`max(x) + log(1 - f) / rate` for `nec4param` and
`max(x) + log((1 - f) / f) / rate` for `ecx4param`. The substitution is recorded
in §6 of `notes/tasks/386-incomplete-designs-claude.md`, which is the
specification a later phase reads.

For `nec4param` the threshold stays inside the series at every setting, so those
cells measure what an unidentified lower asymptote does to the `bot` prior. For
`ecx4param` it rises above the highest dose once *f* falls below a half, so
those cells measure what it does to the threshold prior's truncation. The two
findings #386 reports are therefore separated by equation rather than mixed. The
audit never shortens a series, so `ub = max(prior_predictor)` is the same number
in every cell.

Two quantities are recorded per cell, because #386's label is not the
specification's. `f_reached` is the fraction of the `top`-to-`bot` span the mean
curve travelled within the series, which is what §6 of the specification asks
for. `max_effect` is `1 - mean(mu at max) / mean(mu at min)`, which is how #386
labels the same designs. For #386's `top` of 0.9 and `bot` of 0.1 the second is
0.889 times the first. Averaged over the sweep they are 0.972 and 0.877 at the
complete setting, 0.918 and 0.815 at `f92`, 0.360 and 0.319 at `f36`, and 0.020
and 0.018 at `f02`.

The complete setting's `f_reached` of 0.972 is an average of two different
numbers and not the `1 - exp(-5)` the rate is set to give. An `ecx4param` curve
has already fallen part of the way at the lowest dose, so the span it covers
within the series is less than the whole: by equation the averages at the
complete setting are 0.9933 for `nec4param`, which is `1 - exp(-5)` exactly, and
0.9508 for `ecx4param`.

The factorial is now 4 completeness settings x 5 designs x 3 transforms x 12
families x 2 links x 2 prior types x 2 equations = 5,760 cells and 17,262
parameter-prior rows.

## The `bot` prior as the design flattens

Mean truncated prior CDF at the true `bot`, over the `nec4param` cells. A value
near 0.5 means the prior is centred on the truth.

| prior type | complete | f92 | f36 | f02 |
|---|---|---|---|---|
| `uninformative` | 0.3499 | 0.2428 | 0.1235 | 0.1098 |
| `regularizing` | 0.4516 | 0.2762 | 0.0437 | 0.0304 |

Cells outside the central 95% of the truncated prior, out of 720 `top` and `bot`
cells per prior type per setting:

| prior type | complete | f92 | f36 | f02 |
|---|---|---|---|---|
| `uninformative` | 0 | 0 | 69 | 100 |
| `regularizing` | 2 | 26 | 289 | 327 |

The mean over families understates the collapse in the `uninformative` row,
because under that prior type five of the twelve take a fixed `beta(2, 5)` for
`bot` on the identity link, which reads nothing from the response and so does
not change across the axis. Those five are `bernoulli`, `Beta`, `beta_binomial`,
`binomial` and `zero_inflated_beta`; four of them return one distinct prior
string over all 60 design, transform and completeness cells, and `bernoulli`
over the 57 of its 60 that could be built. The `regularizing` entries for the
same five do read the response --- at the identity link they return 22, 60, 48,
43 and 60 distinct strings --- which is part of why the collapse is larger in
that row of the table above.

Median `p_truth` for `bot` on the gaussian cells at the identity link, which is
the branch #386 reports, is 0.490, 0.345, 6.33e-03 and 4.45e-07 under
`uninformative` and 0.503, 0.408, 6.25e-07 and 4.89e-40 under `regularizing`.
The direction and the order of magnitude agree with #386, whose figures are
0.507, 0.0386 and 6.43e-14 for `uninformative` and 0.525, 1.45e-05 and 2.30e-90
for `regularizing` on its own three designs; the reproduction section below
compares them on #386's design, where they agree to two significant figures.

The two `regularizing` failures at the complete setting are `bernoulli` and
`zero_inflated_beta` on the identity link. `bernoulli` is the family with the
coarsest response, and is the one #386's note on `regularizing_location()`
already names.

## The threshold priors

Cells whose prior support excludes the truth, out of 360 per parameter, prior
type and setting:

| parameter and prior type | complete | f92 | f36 | f02 |
|---|---|---|---|---|
| `ec50`, `uninformative` | 0 | 0 | 360 | 360 |
| `ec50`, `regularizing` | 0 | 0 | 360 | 359 |
| `nec`, either | 0 | 0 | 0 | 0 |

The `ec50` rows are `p_truth` of exactly 1: the true midpoint lies above
`max(prior_predictor)`, which is the upper truncation bound, so the prior places
no mass on it at all. This is #386's threshold finding, reproduced over every
design, transform, family and link rather than on one cell. The one `f02` cell
short of 360 is a `bernoulli` cell whose priors could not be built at all, for
the reason given below.

The `nec` rows are zero by construction and not by acquittal. The completeness
axis keeps a `nec4param` threshold inside the series at every setting, so these
cells cannot exhibit the exclusion. A `nec` above the tested range is measured
in the reproduction section below, on #386's own design, where it gives
`p_truth` of 1 as well. At `f92` no `ec50` cell is excluded but 144 of 360
`regularizing` cells and 72 `uninformative` cells sit outside the central 95%,
so the prior is already in the wrong place before its support fails.

## The figures #386 quotes

#386's design is restated from the issue text: a `nec4param` response on a
log-spaced series of eight concentrations from 0 to 40 by five replicates, with
`top` 0.9 and `bot` 0.1 on the gaussian branch and `top` 40 and `bot` 4 on the
positive branch. The seed, the residual standard deviation and the threshold and
rate settings are not recorded there. Two of the three were recovered from the
`sd(response)` values the issue quotes, which are 0.384, 0.115 and 0.040: a
residual standard deviation of 0.04 leaves `sd(response)` at 0.040 on the
flattest design, and a decay rate of 2.5 reaches 0.384 on the complete design,
where the response is at `top` for the five lowest doses and at `bot` for the
three highest. The run reproduces all three, at 0.387, 0.114 and 0.0402.

The gaussian branch, `bot` prior against a true `bot` of 0.1:

| effect | prior type | this run | #386 | `p_truth` here | `p_truth` in #386 |
|---|---|---|---|---|---|
| 0.92 | `uninformative` | `normal(0.0816, 0.967)` | `normal(0.084, 0.960)` | 0.508 | 0.507 |
| 0.92 | `regularizing` | `normal(0.0758, 0.387)` | `normal(0.076, 0.384)` | 0.525 | 0.525 |
| 0.36 | `uninformative` | `normal(0.609, 0.284)` | `normal(0.607, 0.287)` | 0.0366 | 0.0386 |
| 0.36 | `regularizing` | `normal(0.582, 0.114)` | `normal(0.579, 0.115)` | 1.11e-05 | 1.45e-05 |
| 0.02 | `uninformative` | `normal(0.848, 0.100)` | `normal(0.848, 0.101)` | 4.67e-14 | 6.43e-14 |
| 0.02 | `regularizing` | `normal(0.895, 0.0402)` | `normal(0.913, 0.040)` | 1.81e-87 | 2.30e-90 |

Every entry agrees to two significant figures except the location of the last,
0.895 against 0.913, which is the mean of five replicates at a response of 0.882
and differs by half a residual standard deviation. The two `p_truth` values in
that row differ by three orders of magnitude for the same reason: a normal
density 22 standard deviations from its location is that sensitive to its
location. Nothing turns on the digit, because both say the prior excludes the
truth.

The positive branch, `poisson` with a true `bot` of 4. The prior mass below the
truth is 0.500 and 0.157 on the complete design, 0.0251 and 2.39e-06 at a
realised effect of 0.314, and 0.0214 and 7.82e-10 at 0.120, for `uninformative`
and `regularizing`. #386 quotes 0.473 and 0.253, then 0.0277 and 6.91e-06 at 40%
effect, then 0.0203 and 4.17e-11 at 11%. The `uninformative` column reproduces;
the `regularizing` column is the same order of magnitude in two rows of three
and differs by a factor of 1.6 on the complete design and by two orders of
magnitude on the flattest one.

The two entries differ in how much of the response they read, which accounts
for that. `regularizing_location()` (`R/define_prior.R:521`) returns the mean of
the observations at the extreme concentration, so on this design the `bot`
anchor is a mean of five poisson draws: the three gamma entries above have their
maximum density at 5.00, 26.2 and 35.2, which are those three means exactly. At
a mean of 35.2 that statistic has a standard error of 2.65, and shifting a gamma
of shape 14 by a few units changes its far tail by orders of magnitude, so a
`p_truth` in the tenth decimal place is not comparable across two draws. The
`uninformative` entry is a gamma of shape 2 whose mean is set near the 25th
percentile of the whole response of 40 observations, and is correspondingly
stable. Neither run's seed is recoverable.
Part 1 section 3 above describes the `regularizing` `bot` anchor as the sample
minimum; that was its behaviour at `eebccdb3` and is not its behaviour here.

The threshold priors reproduce, at a truncated prior CDF of 1 under both prior
sets, which is what #386 reports for a true `ec50` of 45 and for a true `nec` of
60 against a highest concentration of 40. The two are one measurement rather
than two confirmations: the `nec` and `ec50` entries read the predictor alone
and nothing else, so the run prints the same prior string and the same bounds
for both equations, and `p_truth` of 1 follows from `ub = 40` and a truth above
it. The `nec4param` design is degenerate as well, since a threshold of 60 puts
the whole series below the break point and the mean response is `top` at every
dose.

The constant entries reproduce exactly. `beta(5, 2)` at a true `top` of 0.9 and
`beta(2, 5)` at a true `bot` of 0.1 give 0.886 and 0.114, which are #386's
figures.

## The initial-value search

The proposals `make_good_inits()` draws before the band accepts a full set of
four chains, counted by tracing `make_inits()` so that the released search is
run rather than reimplemented. `refine_inits()` re-draws are excluded, which is
the convention `notes/scripts/init_search_audit.R` uses. The audit caps the
search at 200 rounds rather than the shipped 10,000, because it runs one search
per cell over 5,760 cells.

Median proposals:

| equation | complete | f92 | f36 | f02 |
|---|---|---|---|---|
| `nec4param` | 6 | 6 | 7 | 8 |
| `ecx4param` | 5 | 5 | 5 | 7 |
| both | 5 | 5 | 6 | 7 |

The 90th percentile is 14 at the complete setting, 15 at `f92`, 14 at `f36` and
22.8 at `f02`. Twenty-seven of 5,751 cells reached the cap and fell back to
Stan's own initialisation: 21 `zero_inflated_beta`, 4 `binomial` and 2
`beta_binomial`, 26 of them at `f02` and one at `f36`. A two-block family is
over-represented, and its count covers both blocks because they are primed in
turn, but the fallback is not confined to one: the four `binomial` cells are
single-block. An incomplete design therefore raises the median proposal count
from 5 to 7 and the 90th percentile from 14 to 23, and the search fails
altogether in about one cell in 200 at the flattest setting.

## The flatness rule of #390

### The report rate over the sweep

| | complete | f92 | f36 | f02 |
|---|---|---|---|---|
| all families | 0.481 | 0.750 | 0.697 | 0.096 |

The complete column is not the rule's false-positive rate, and the reason is a
property of the rule rather than of this sweep. The rule tests the last interval
of the series, not whether the response has reached `bot`, and on a log-spaced
series that interval is the widest one there is. On `log_wide` under
`crf(log(x))` the mean is 71 per cent of the way from `top` to `bot` at a dose
of 2.5 and within 1 per cent of `bot` at 20, so the contrast between them is
large and the decline is real; on the identity transform of the same series the
mean at 2.5 is 28 per cent of the way. The report rate at the complete setting
rises with the spacing of the last step, from 0.104 on `linear` and 0.101 on
`linear_unit` to 0.476 on `log_2fold`, 0.781 on `log_unit` and 0.944 on
`log_wide`. Those reports are power and not false positives: the response is
still declining where the rule says it is. They are also advisories raised on
designs whose `bot` prior is sound, which follows from a rule that reads two
levels rather than the curve. The level is stated rather than tuned, so this is
recorded and not acted on.

At `f02` the rate falls to 0.096, which is the miss: a design showing 2 per cent
of its span is reported on one call in ten. The rule detects an incomplete design
by its slope and not by its extent, and a curve that has barely started to fall
has almost no slope to detect.

One note on what this column is. A hurdle or zero-inflated cell is reported
where either block declines, which is what `bnec()` does, but the hurdle
simulators in this sweep hold the hurdle probability constant at 0.8, so their
survival block is flat by construction and contributes its own false positive:
that is why `hurdle_gamma` reads 0.183 and `zero_inflated_beta` 0.192 at `f02`
against `poisson`'s 0.058.

The column is computed from the four variables `bnec()` hands
`check_response_flattened()` on the model frame, not from `check_data()`'s
output. The two agree cell for cell on this sweep --- the saved result is
unchanged in every one of its 17,262 rows --- but they are not interchangeable
in general, and they agree here only because nothing in the sweep reaches the
substitutions `check_data()` makes. It nudges an exact 0 or 1 away from the
boundary for `beta` and an exact 0 for `Gamma`, exempting a hurdle fit
(`R/check_data.R:1007`), and none of these simulators produces one:
`rgamma(shape = 25)` and `rbeta` at these parameters return no boundary value,
and `zero_inflated_beta` keeps its zeros.

### The false-positive rate on a flat top

Measured on the generator PR #399 describes: five predictor levels whose mean is
equal at the two highest, four replicates per level unless stated, so the true
contrast is zero. Intervals are Wilson score intervals.

| block | replicates | rate | interval | #390 |
|---|---|---|---|---|
| gaussian, dispersion pooled over the series | 20000 | 0.0502 | [0.0472, 0.0533] | 0.0499 |
| gaussian, dispersion from the two levels | 20000 | 0.0521 | [0.0491, 0.0553] | 0.0508 |
| Gamma | 4000 | 0.0475 | [0.0413, 0.0545] | 0.061 |
| poisson | 4000 | 0.0482 | [0.0420, 0.0553] | 0.049 |
| negbinomial, size 5 | 4000 | 0.0568 | [0.0500, 0.0644] | 0.050 |
| binomial, 20 trials | 4000 | 0.0505 | [0.0441, 0.0577] | 0.053 |
| beta_binomial, 20 trials, rho 0.1 | 4000 | 0.0528 | [0.0462, 0.0601] | 0.053 |
| Beta | 4000 | 0.0525 | [0.0460, 0.0599] | 0.059 |
| hurdle survival, 20 individuals | 4000 | 0.0597 | [0.0528, 0.0675] | 0.055 |
| bernoulli, 4 observations per level | 4000 | 0.0862 | [0.0779, 0.0954] | 0.088 |

Nine of the ten rows cover 0.05 or sit within 0.01 of it, and the tenth is
`bernoulli`, which is the third item below. The generator's response levels are
this run's rather than #390's, which are not recorded, so agreement of this kind
is what the comparison can establish and a digit-for-digit match is not.

The `beta_binomial` row of this table, the constant-trials row of the
varying-trials table below, and the replicated row at the end of the
unreplicated table are all the same data-generating process under three seeds:
five levels, four rows of twenty trials each, an intra-class correlation of 0.1,
flat top. They read 0.0528, 0.0485 and 0.0517, a spread of 0.004 over three
independent runs of 4000 replicates, which is the Monte Carlo error on a
4000-replicate row at this rate. The 20000-replicate rows are about half as
uncertain and the rows near 0.18 rather less certain; each row's own interval
is given beside it.

`check_response_flattened()`, which is the function a user meets, agreed with
the two helpers this section calls on 400 of 400 blocks, over a gaussian
response, a `binomial` response with a `trials()` term, a `hurdle_gamma` fit
whose two blocks are assessed separately, and an unreplicated `beta_binomial`
block of one row per level. Those four cover the estimated dispersion, the fixed
one, the fixed-dispersion fallback that produces the 0.1825 below, the
two-column count response and the hurdle split.

### The quasipoisson approximation to the negative binomial variance

Item 1 of #390. The claim is that quasipoisson's linear variance, `phi mu`,
approximates the negative binomial's quadratic `mu + mu^2 / theta` well enough
over two adjacent levels. On a flat top the approximation holds: `negbinomial`
with a size of 5 reports on 0.0568 [0.0500, 0.0644], against a nominal 0.05.

The departure appears in power rather than in calibration. On an over-dispersed
count design with `theta` 2 against a control mean of 40, so that the quadratic
term is 800 against a linear term of 40 at the control, the rule reports on
0.209, 0.395 and 0.569 of blocks as the fall between the two highest levels
rises through 0.1, 0.2 and 0.3 of the `top`-to-`bot` span. The miss rate is
therefore 0.79, 0.60 and 0.43. On the same generator with `poisson` variation
rather than `theta` 2 the rule reports on 0.519, 0.920 and 0.996.

### Quasibinomial where the trials vary within a level

Item 2 of #390. Beta-binomial blocks at an intra-class correlation of 0.1, flat
top, 4000 replicates:

| trials within a level | rate | interval | #390 |
|---|---|---|---|
| constant at 20 | 0.0485 | [0.0423, 0.0556] | 0.054 |
| 10, 20, 40, 80 | 0.0970 | [0.0882, 0.1066] | 0.088 |
| 5, 10, 20, 100 | 0.1482 | [0.1376, 0.1596] | 0.137 |

The direction and the size reproduce. As a proportion the beta-binomial variance
is `p(1-p)/n [1 + (n-1) rho]`, whose bracket depends on `n`, so quasibinomial
absorbs it as a constant multiplier only where the trials are constant, and the
departure grows with the spread of the trials: the rate doubles at an 8-fold
spread and triples at a 20-fold one.

The miss rate goes the other way, because the same inflation that raises the
false-positive rate also raises the power. At an intra-class correlation of 0.1
and a fall of 0.1, 0.2 and 0.3 of the span, the constant-trials design is
reported on 0.140, 0.277 and 0.455 and the 5, 10, 20, 100 design on 0.292, 0.451
and 0.600. Neither number means anything alone, which is why the pair is
reported: the varying-trials design is not detected better, it is tested at a
level nearer 0.15 than 0.05.

### Bernoulli small-sample discreteness

Item 3 of #390. Twenty thousand replicates per row, on a flat top of a
five-level series.

| observations per level | rate | interval |
|---|---|---|
| 1 | 0.0000 | [0.0000, 0.0002] |
| 2 | 0.0428 | [0.0400, 0.0456] |
| 4 | 0.0886 | [0.0847, 0.0926] |
| 10 | 0.0558 | [0.0528, 0.0591] |
| 25 | 0.0504 | [0.0475, 0.0536] |
| 50 | 0.0527 | [0.0497, 0.0559] |

#390 reports 0.082, 0.063 and 0.049 at four, ten and twenty-five, and states
that the rate is not monotone in the count. The three figures reproduce, at
0.0886, 0.0558 and 0.0504. Non-monotonicity is visible in the rows #390 does not
cover rather than in those three: the rate at two observations is 0.0428, below
both the nominal level and the rate at four, and the two intervals do not
overlap. The rows at 25 and 50 read 0.0504 and 0.0527 with overlapping
intervals, so they establish nothing either way. The row at one observation is
the guard #390 added for a block of one individual per level, which passes the
block over rather than testing it, and is a count of zero rather than a rate.

### The fixed-dispersion fallback on an unreplicated design

Item 4 of #390. One row per level, twenty trials, 4000 replicates, flat top.

| block | rate | interval | #390 |
|---|---|---|---|
| binomial | 0.0525 | [0.0460, 0.0599] | 0.058 |
| beta_binomial, rho 0.05 | 0.1375 | [0.1272, 0.1485] | |
| beta_binomial, rho 0.1 | 0.1825 | [0.1708, 0.1948] | 0.184 |
| beta_binomial, rho 0.2 | 0.2502 | [0.2371, 0.2639] | |

The item reproduces to two significant figures at the intra-class correlation
#390 measured, 0.1825 [0.1708, 0.1948] against its 0.184, and the two rows
either side of it show that the rate is roughly linear in the correlation over
this range. Supplying four replicate rows per level restores the over-dispersion
estimate and the rate returns to 0.0517 [0.0453, 0.0591], which is #390's own
remedy measured.

This is the largest departure from the stated level in the rule. It was accepted
deliberately, because the alternative was to pass the design over and report on
none of the genuinely incomplete ones, and that decision is recorded in
`R/check_data.R` and in `NEWS.md`. The measurement above is the audit item those
records point to, and it is not a case for changing the rule.

### The miss rate on a top that is still declining

The five-level series with the fourth level held a stated fraction of the span
above `bot` and the fifth at `bot`, so the contrast tested is a fall of that
fraction. 4000 replicates.

| block | fall 0.1 | fall 0.2 | fall 0.3 |
|---|---|---|---|
| gaussian | 0.428 | 0.025 | 0.000 |
| poisson | 0.481 | 0.080 | 0.004 |
| binomial, 20 trials | 0.766 | 0.449 | 0.178 |
| hurdle survival, 20 individuals | 0.899 | 0.820 | 0.737 |

The hurdle survival row is the design the rule was written for and the one it
misses most often. Its information is twenty individuals at each of two
concentrations and nothing else, so a fall of a third of the span between them is
detected on one call in four. A dilution series whose survival is still falling
at the undiluted end is therefore reported only where the fall is large.

### The comparison against `MASS::glm.nb`

Run once for the comparison, on the `theta` 2 blocks, testing the same one-sided
contrast by a likelihood ratio on a `glm.nb` fit of the two levels. 400
replicates. Every fit converged.

| setting | quasipoisson | interval | `glm.nb` | interval | missed by the rule, reported by `glm.nb` |
|---|---|---|---|---|---|
| flat top | 0.0550 | [0.0366, 0.0819] | 0.0775 | [0.0551, 0.1079] | 0.0250 |
| fall 0.1 | 0.2175 | [0.1799, 0.2605] | 0.3275 | [0.2833, 0.3749] | 0.1100 |
| fall 0.2 | 0.3675 | [0.3217, 0.4158] | 0.4875 | [0.4389, 0.5364] | 0.1225 |
| fall 0.3 | 0.5200 | [0.4711, 0.5685] | 0.6250 | [0.5766, 0.6710] | 0.1100 |

`glm.nb` reports about eleven blocks in a hundred that the rule misses, so the
quasi rule does miss designs a parametric fit finds. The trade is visible in the
first row: `glm.nb`'s own rate on a flat top is 0.0775 with an interval whose
lower bound is above 0.05, so part of its extra power is extra false reporting at
this size, which is the eight observations section 2.2 of the specification
describes as short of asymptotic.

The likelihood ratio is used rather than the difference of the two deviances,
because `glm.nb` estimates a `theta` for each fit and the two deviances are
computed under different variance functions. The likelihood ratio is also what
`anova.negbin()` reports as its `LR stat.` for the same pair of fits. Both are
computed here, and the deviance route reports on 0.0025, 0.0200, 0.0175 and
0.0125 of the same four settings: it is not a conservative version of the same
test but a broken one, which never reaches the nominal level and has almost no
power.

The measurement does not settle whether `MASS` belongs in `Suggests`. It
establishes what the fallback would add, about eleven percentage points of power
on a strongly over-dispersed count design, and what it would do to the null
rate, which rises from 0.055 to 0.078. That is RF's decision, and nothing was
added to `DESCRIPTION`.

## Defects the run exposed

`response_link_scale()` returns `NaN` where every observation of a bounded
response equals 1. `R/helpers.R:488` and `:519` compute
`max(response[which(response < 1)])`, which is `-Inf` when no observation is
below 1, and `linear_rescale()` then produces `NaN` for every element.
`define_prior()` stops in `quantile.default()` with "missing values and NaN's not
allowed if 'na.rm' is FALSE", which names neither the column nor the cause. Nine
of the 5,760 cells reached it, all `bernoulli` and all at the two flattest
settings, where the true mean is 0.95 at every dose and a draw of 60 or 66
observations can contain no zero. A user met it as a failed fit rather than as
a refusal. It was filed as #400, and since the change for #400 `get_priors()`
refuses such a response by name before any prior is built: the message names the
response column and says that every observation is at the upper bound of 1. The
nine cells are therefore now refused by name rather than lost to the
`quantile()` error. The audit was not re-run after the change, so the counts in
this document are those of the run described here.

The flatness rule reports on a design whose lower asymptote is identified,
wherever the last dose step is wide. Measured above at 0.944 on `log_wide` at
the complete setting, where the response is within 1 per cent of `bot` at the
highest dose. The report is true as a statement about the last interval and
misleading as a statement about the design, and what it then advises --- inspect
the priors, supply your own --- is not what that user needs. Recorded here rather
than acted on, because the rule's level is stated rather than tuned and the
remedy is a change to what the rule tests.

## Questions outside this run

The `nec` prior's support exclusion, which the completeness axis cannot produce
because it keeps a `nec4param` threshold inside the series; it is measured on
#386's own design instead. The hurdle response block's own completeness, since
the axis places one threshold and a two-block fit has two. And anything that
requires a concentration-response fit: every figure here is a prior, an initial
value or a pre-fit contrast.

---

# Part 4. The tested-range truncation removed (#393)

> **Provenance.** Parts 1 (sweep) and 2 (reproduction) of
> `notes/scripts/prior_audit.R` were run twice on 2026-09-22, R 4.6.1,
> brms 2.23.0, on branch `issue-393-remove-truncation` at the commit before and
> the commit after the change to `R/define_prior.R`. The script itself is
> unchanged between the two runs, and every cell is seeded from its own index,
> so the two runs are paired cell by cell. The before run reproduces the
> 2026-09-19 figures in part 3 above exactly, which is what establishes that it
> measured the released code.

The change removes `lb = min(prior_predictor)` and `ub = max(prior_predictor)`
from the `nec` and `ec50` entries and replaces them with the support of the
prior distribution: `lb = 0` on the lognormal branch and no bound on the normal
one.

## The complete-design cells

The `top` and `bot` entries are untouched. Every prior string and every
`p_truth` is identical across the two runs, over all 11,502 such rows, so the
tables in part 3 stand as printed.

The `nec` and `ec50` prior *strings* are identical on the complete cells as
well, over all 1,440 of them. What changes is the statistic: `p_truth` is the
CDF of the prior **after truncation**, which is a function of the bounds as well
as of the distribution. The fall is 0.0008 to 0.344, with a median of 0.055, and
it is a fall in every one of the 1,440 cells. It is largest where the truncation
removed most mass: 0.187 on average for `linear` and `linear_unit` under
`crf(log(x))`, against 0.013 to 0.016 for the three log-spaced series under the
identity and square-root transforms.

Writing *a* for the prior CDF at the old lower bound, *b* for the CDF at the old
upper bound and *u* for the CDF at the truth, the truncated statistic is
(*u* − *a*) / (*b* − *a*) and the untruncated one is *u*. Their difference is
(*u*(1 − *b*) − *a*(1 − *u*)) / (*b* − *a*), so removing the upper bound alone
always lowers the statistic, removing the lower bound alone always raises it,
and where both go the sign depends on where the truth sits: the difference is
negative at *u* = *a*, positive at *u* = *b*, and zero at
*u* = *a* / (1 − *b* + *a*). The lognormal cells with a zero control have
*a* = 0 and fall by construction. The `crf(log(x))` cells, where the
largest falls are, take the normal branch, where the old lower bound was binding
too, so the fall observed there is the net of two effects and not the
upper-tail term alone. That every one of the 1,440 cells falls is the
measurement and not a consequence of the algebra.

Specification §3.4 asks for the complete-design CDF to be unchanged within the
Monte Carlo error of the audit, on the ground that the truncation does not bind
there. That is right about the support and wrong about the statistic. The
truncation bound the normalisation on every design, complete or not, because the
lognormal always places mass above the highest concentration tested and the
normal places mass below the lowest; what a complete design guarantees is that
the truth is inside the support, not that the renormalisation is negligible. The
test the requirement was reaching for is whether a complete design's truth is
still well inside the prior, and it is: the count of complete `nec` and `ec50`
cells outside the central 95% of their own prior is 0 before and 0 after, and
the range of `p_truth` over those cells is 0.420 to 0.974 before and 0.334 to
0.965 after.

## The incomplete-design cells

`ec50` cells whose prior support excluded the truth, out of 2,159 incomplete
cells that could be built:

| | before | after |
|---|---|---|
| `p_truth` exactly 1 | 1,439 | 0 |
| largest `p_truth` | 1 | 0.99938 |

The 1,439 are part 3's 360, 360, 360 and 359. The largest value after the change
is 0.99938, so the truth is in the far upper tail of the prior on those designs
and is no longer outside it. The median over the incomplete `ec50` cells is
0.934.

The count of `nec` and `ec50` cells outside the central 95% falls as well, which
is the same prior mass counted a second way:

| parameter and prior type | complete | f92 | f36 | f02 |
|---|---|---|---|---|
| `ec50`, `uninformative`, before | 0 | 72 | 360 | 360 |
| `ec50`, `uninformative`, after | 0 | 0 | 144 | 144 |
| `ec50`, `regularizing`, before | 0 | 144 | 360 | 359 |
| `ec50`, `regularizing`, after | 0 | 96 | 216 | 216 |
| `nec`, `uninformative`, before | 0 | 24 | 168 | 357 |
| `nec`, `uninformative`, after | 0 | 0 | 0 | 0 |
| `nec`, `regularizing`, before | 0 | 96 | 215 | 357 |
| `nec`, `regularizing`, after | 0 | 24 | 215 | 215 |

## The `nec` support exclusion, on #386's own design

The sweep cannot produce it, for the reason recorded in the §6 amendment: the
completeness axis keeps a `nec4param` threshold inside the series. Part 2 of the
script measures it on #386's design, a log-spaced series of eight concentrations
from 0 to 40, with a true `ec50` of 45 and a true `nec` of 60. The prior string
is the same before and after and is the same for both equations, because the two
entries read the predictor alone.

| parameter | prior type | `ub` before | `p_truth` before | `ub` after | `p_truth` after |
|---|---|---|---|---|---|
| `ec50` | `uninformative` | 40 | 1 | none | 0.9795 |
| `ec50` | `regularizing` | 40 | 1 | none | 0.9923 |
| `nec` | `uninformative` | 40 | 1 | none | 0.9877 |
| `nec` | `regularizing` | 40 | 1 | none | 0.9962 |

## The initial-value search

`make_inits()` draws from the priors including their bounds, so removing the
upper bound widens the `nec` and `ec50` draws and
`check_init_predictions()` rejects any whose curve falls outside the band. The
counts are paired by cell, one search per cell, capped at 200 rounds as part 3
was, and the difference is reported as a paired mean with a normal interval.

On the complete designs the search takes more proposals, and the rise is
confined to `nec4param`:

| equation, complete designs | median before | median after | mean before | mean after | paired mean difference |
|---|---|---|---|---|---|
| `nec4param`, n = 720 | 6 | 7 | 10.37 | 11.68 | +1.31 [0.59, 2.04] |
| `ecx4param`, n = 720 | 5 | 4 | 7.36 | 7.14 | −0.22 [−0.60, 0.16] |
| both, n = 1,440 | 5 | 6 | 8.86 | 9.41 | +0.55 [0.14, 0.96] |

The 90th percentile over the complete cells rises from 14 to 15, and from 15 to
18.1 on the `nec4param` cells alone. The asymmetry follows from the shape of the
two equations. An `ec50` drawn above the highest concentration still gives a
curve that declines across the series, so the band often accepts it; a `nec`
drawn above the highest concentration gives a curve that is flat at `top` at
every dose, which the band rejects wherever the response has fallen.

The rise does not reach the cap. No complete-design cell fell back to Stan's own
initialisation in either run, and the largest single count over the complete
cells fell from 283 to 166. Over the whole sweep the fallback count fell from 27
cells to 21, all of them at `f36` or `f02`.

## The limits of a prior-only harness

No model is fitted, so the posterior this change exists to widen is not measured
here. The prior CDF at the truth and the proposal count are what the
specification asks for and are what a prior-only harness can supply.

## Outstanding at merge

`vignettes/example3.Rmd.orig` was corrected on the #393 branch and the committed
`vignettes/example3.Rmd` was not regenerated, because precompiling refits every
model. Until `vignettes/precompile.R` is run for `example3`, the published
vignette shows the truncation as live output: its stored `pull_prior()` tables
give the `nec` row `lb = 0.03234801324009`, which is `min(nec_data$x)`, and
`lb = 0, ub = 10` on a second fit, where the current code gives `0` and `NA`.

# Part 5. The declaration of an unobserved asymptote (#394)

> **Provenance.** `notes/scripts/prior_audit.R` part 5, run on branch
> `issue-394-asymptote-observed` on 2026-09-22, R 4.6.1, brms 2.23.0. Parts 1
> and 2 of the same script were run on that branch and on `predev` at
> `5fee2e9a` on the same day, with the script identical between the two runs
> and every cell seeded from its own index. All 17,262 parameter-prior rows
> agree in every column, and the printed summary tables are identical line for
> line, which is what establishes that the default path is untouched. No
> concentration-response model was fitted and no Stan program was compiled.

`asymptote_observed = FALSE` replaces the `bot` entry of whichever set
`prior_type` selected. Its central 95% runs from the floor of the response to
the mean response at the highest predictor level, so that mean is read as an
upper bound on `bot` rather than as an estimate of it. The initial-value band is
extended to the same floor.

Part 5 runs every cell twice from one simulated response, under the default and
under the declaration, so the two arms are paired by cell and the difference is
the declaration alone. Part 1 cannot supply that: it runs the default only, and
a comparison across two runs of it would also show whatever else differed
between them.

The factorial is part 1's, restricted in two ways. Only `nec4param` is run,
because an `ecx4param` threshold leaves the series once the design is flat and
its `bot` is then unidentified for a second reason. Only the identity link is
run, because the declaration is refused on any other. That is 4 completeness
settings x 5 designs x 3 transforms x 12 families x 2 prior types = 1,440 cells
and 2,880 prior builds.

## The `bot` prior

Mean prior CDF at the true `bot`. A value near 0.5 means the prior is centred on
the truth.

| prior type and arm | complete | f92 | f36 | f02 |
|---|---|---|---|---|
| `uninformative`, default | 0.1804 | 0.0865 | 0.0444 | 0.0420 |
| `uninformative`, declared | 0.9273 | 0.7830 | 0.2178 | 0.1184 |
| `regularizing`, default | 0.3148 | 0.1642 | 0.0011 | 0.0009 |
| `regularizing`, declared | 0.9174 | 0.7951 | 0.2218 | 0.1204 |

Cells placing the true `bot` outside the central 95% of its own prior, out of
180 per prior type per setting:

| prior type and arm | complete | f92 | f36 | f02 |
|---|---|---|---|---|
| `uninformative`, default | 0 | 0 | 15 | 15 |
| `uninformative`, declared | 56 | 11 | 0 | 0 |
| `regularizing`, default | 1 | 26 | 179 | 176 |
| `regularizing`, declared | 41 | 10 | 0 | 0 |

The declaration removes the exclusion entirely at `f36` and at `f02`, where it
is a true statement about the design, and it introduces one at the complete
setting, where it is a false one. That is the trade a declaration makes and it
is the user's to make: on a complete design the endpoint mean *is* `bot`, so a
prior whose central 95% ends there puts the truth at the top of its own
interval. The mean CDF of 0.92 at that setting says the same thing.

The 15 `uninformative` default cells at `f36` and at `f02` are the gaussian
ones, all 15 of them; no other family contributes any. That is the limitation
#391 records, read from the other side: five of the twelve take a fixed
`beta(2, 5)` under `"uninformative"` and six take a gamma whose mean is the
lower quartile of the response, and none of those eleven entries is narrow
enough to exclude a true `bot` of 0.05 or of 5. The `"regularizing"` entries for the same
families do read the response, and they exclude the truth in 179 of 180 cells at
`f36`. So the declaration is what corrects the gaussian `"uninformative"` entry
and every `"regularizing"` entry, and for the other eleven families under
`"uninformative"` it replaces a prior that was wide by construction with one
that is located. The script prints the count by family and prior type for each
arm, which is where that reading comes from.

## The realised span, by family

The rule asks for 0 and 1. The 2.5th and 97.5th percentiles of the declared
entry, as multiples of the endpoint mean it is built from, median over the 120
cells of each family:

| family | 2.5th / endpoint | 97.5th / endpoint |
|---|---|---|
| `gaussian` | 0.0000 | 1.0000 |
| `bernoulli` | 0.0084 | 1.0000 |
| `Beta` | 0.0086 | 1.0000 |
| `binomial` | 0.0086 | 1.0000 |
| `beta_binomial` | 0.0087 | 1.0000 |
| `zero_inflated_beta` | 0.0087 | 1.0000 |
| `Gamma` | 0.0606 | 1.3929 |
| `poisson` | 0.0606 | 1.3929 |
| `negbinomial` | 0.0606 | 1.3929 |
| `hurdle_gamma` | 0.0606 | 1.3929 |
| `zero_inflated_poisson` | 0.0606 | 1.3929 |
| `zero_inflated_negbinomial` | 0.0606 | 1.3929 |

The gaussian branch meets the rule exactly. The beta branch meets it at the
upper end exactly and reaches 0.008 to 0.009 of the endpoint at the lower. The
gamma branch cannot meet it at either end: at a fixed shape of 2 the central 95%
of a gamma is a fixed multiple of its mean, so setting the mean to half the
endpoint puts the interval at 0.0606 to 1.393 times it. §5.3 of the
specification anticipates that and asks for the realised figures rather than an
assertion.

The beta branch departs from §5.3's suggested mechanism, and the measurement is
why. `beta_from_mode_sd()` requires both shapes above 1, so the density is zero
at the floor; applied with the mode at half the endpoint and the gaussian
branch's spread it returns a 2.5th percentile of 0.18 of the endpoint. That sat
above the true `bot` in every one of the 75 bounded cells at `f36`, and again at
`f02`, under each prior type — where the fixed `beta(2, 5)` of the
`"uninformative"` set excludes the truth in none of them. Fixing `shape1` at 1
leaves `shape2` determined by the 97.5th percentile alone, gives a density that
is positive and finite at the floor, and excludes the truth in none of those
cells either. The governing sentence of §5.3 is the contract and the helper was
the mechanism it suggested.

## The initial-value search

Both arms are given the declared prior, so what differs between them is the band
and not the prior. One search per cell, capped at 200 rounds as parts 3 and 4
were.

Median proposals to a full set of four chains:

| arm | complete | f92 | f36 | f02 |
|---|---|---|---|---|
| default | 7 | 6 | 7 | 10 |
| declared | 6 | 6 | 6 | 7 |

Paired difference, declared minus default:

| setting | n | paired mean | 95% interval |
|---|---|---|---|
| complete | 360 | −1.18 | [−1.57, −0.80] |
| f92 | 360 | −2.83 | [−5.40, −0.27] |
| f36 | 360 | −2.19 | [−2.84, −1.54] |
| f02 | 358 | −4.58 | [−5.53, −3.63] |

One cell fell back to Stan's own initialisation under the default and none under
the declaration. At a cap of 200 rounds the fallback is rare either way, so the
proposal count rather than the fallback count is what shows the band admitting
the draws the prior makes. The comparison is not a statement about how often a
shipped fit falls back: that cap is 10,000 rounds.

## The refusal on a non-identity link

`prior_family_tag()` rewrites a `log` or `logit` link onto the gaussian entries,
and the floor of the mean maps to `-Inf` on that scale. All twelve families are
refused under their alternative link, which is the behaviour and not a defect: a
response that is non-negative once logged states that every observation exceeds
one in response units, which is a property of the units rather than a floor.

## The cells the run could not build

Four rows of the 2,880 report "missing values and NaN's not allowed if 'na.rm'
is FALSE", on two `bernoulli` cells at `f02` under `crf(log(x))`. They appear in
both arms of each cell, so they are not the declaration's. Every observation of
those responses is 1, and `response_link_scale()` then takes
`max(response[response < 1])` of an empty vector. This is #400. Since the
change for #400 these cells are refused by name, with a message naming the
response column and the upper bound; this part was not re-run after it.
