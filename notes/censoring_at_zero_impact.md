# What censoring at zero does to the `example7` draft

Written 2026-08-11, to decide whether to reframe `vignette("example7")` around
censoring negative growth rates at zero before it is committed. Companion to
`disp_validation.md`, which records the dispersion work the current draft is
built on.

Nothing here is decided. This is the evidence and the consequences.

## The proposal

Left-censor every non-positive growth rate at zero:

```r
d$censoring <- ifelse(d$sgr <= 0, "left", "none")
d$sgr_cens  <- ifelse(d$sgr <= 0, 0, d$sgr)
```

The rationale is that `ECx` should be estimated from the portion of the curve
between control growth and zero growth. Negative growth rates occur and belong
in the likelihood, but should not be allowed to determine the shape of the curve
outside that range.

### Three distinct treatments, which the current draft conflates down to two

| | what it asserts | when it is right |
|---|---|---|
| **Flooring** — replace a measured negative with 0 | the value *was* 0 | never; see `example6` |
| **Detection-limit censoring** — bound at `(log(10) − log(N0))/days` | truth ≤ −1.99 | a count below the counting resolution, where `µ` was never resolvable |
| **Censoring at zero** — bound at 0 | truth ≤ 0 | a resolved negative value whose magnitude is outside the range of interest |

The current draft covers the first two. The third is absent.

Note the naming caveat already recorded in `oecd_tg201_verification.md` §5b:
censoring at zero is not censoring in the observation-process sense, because the
value *was* resolved. It is a deliberate modelling choice, and should be
presented as one. Its cost is the loss of any algistatic/algicidal
discrimination — a test where growth merely stops and one where the population
is destroyed become indistinguishable.

The two censoring treatments are not additive. A row below the counting limit is
certainly `≤ 0`, so censoring at zero subsumes it with a weaker (less
informative) bound. Choosing between them is choosing whether the magnitude of
decline is of interest.

## What the fits show

Four tests, `nec4param` on `log(dose)`, `gaussian`, 6000 iterations, four chains.
Each fitted under both censoring schemes, with and without
`disp("loglinear")`. `loo` is compared only *within* a scheme — the censored and
uncensored fits use different response data and their `elpd` values are not on a
common scale.

### The dispersion signal is almost entirely below zero

| test | disp gain, uncensored | disp gain, censored at 0 |
|---|---|---|
| `c_proliferum` × A | 60.6 (16.2) | **4.0 (4.1)** |
| `c_proliferum` × B | 31.7 (8.8) | **−1.4 (0.6)** |
| `r_salina` × A | 130.8 (13.6) | **−0.9 (0.4)** |
| `r_salina` × B | 42.2 (8.0) | **3.6 (2.3)** |

| test | `c1` uncensored | `c1` censored at 0 |
|---|---|---|
| `c_proliferum` × A | −16.32 [−23.20, −11.57] | −7.12 [−12.93, −2.60] |
| `c_proliferum` × B | −10.59 [−14.71, −7.61] | −1.54 [−6.85, +4.41] |
| `r_salina` × A | −1.62 [−2.09, −1.31] | −0.06 [−0.91, +0.82] |
| `r_salina` × B | −1.30 [−1.64, −1.01] | −0.88 [−1.46, −0.26] |

Within the range `ECx` is actually read from — control growth down to zero —
these data are close to homoscedastic. Two of four slopes still exclude zero,
but no test shows a `loo` gain that survives its own standard error.

### Censoring at zero repairs the fits that were degenerate

This was not anticipated. The two tests the current draft reports as unusable
are unusable *because* of the uncensored sub-zero region.

| | ECx₁₀ uncensored, constant | ECx₁₀ censored, constant |
|---|---|---|
| `c_proliferum` × A | 0.00 [0.00, 9.89], R-hat 1.166, ESS 16 | **4.22 [3.67, 4.66]**, R-hat 1.001, ESS 1584 |
| `r_salina` × A | 4.80 [0.00, 5.30], R-hat 1.073, ESS 43 | **4.61 [4.18, 4.89]**, R-hat 1.011, ESS 353 |

### The two remedies converge

`c_proliferum` × B, the test used for the `ECx` comparison in the draft:

| fit | ECx₁₀ |
|---|---|
| constant, uncensored | **336.92** [321.74, 352.82] |
| `disp("loglinear")`, uncensored | 244.00 [210.53, 260.26] |
| constant, censored at 0 | 241.76 [222.51, 255.51] |
| `disp("loglinear")`, censored at 0 | 241.76 [212.48, 257.88] |

The outlier is the constant uncensored fit. Far-negative growth rates drag the
curve and bias `ECx` upward by roughly 40%. The variance function corrects this
by downweighting them; censoring corrects it by declining to model them. Once
censored, the variance function moves the point estimate by nothing — and the
same holds in the other three tests.

**The interpretation this forces:** non-constant dispersion was doing the work of
a censoring decision that had not been made. It was a distributional proxy for a
structural problem.

## Consequences for each section of the draft

**Background.** Unaffected in substance. The TG 201 design material, the validity
criteria table and the four properties of growth-rate data all stand. One
sentence would need to name censoring at zero among the issues covered.

**The growth rate and density scales.** Unaffected.

**Counts below the limit of detection.** Needs restructuring rather than
rewriting. Currently it presents detection-limit censoring as *the* treatment.
It would become the three-way framework above, with censoring at zero as the
primary recommendation and the detection-limit bound as the tighter alternative
where the magnitude of decline matters. The existing three-way fit comparison
(as supplied / dropped / censored) survives and gains a fourth arm.

**Normalisation of the response.** Unaffected. The Ritz and TG 201 Annex 4
material is independent of the censoring decision.

**Model and family selection.** Mostly unaffected, but the emphasis shifts. The
argument that models must be able to go negative remains correct and is still
needed — censoring at zero does not constrain the mean function, which may still
descend below zero; it constrains what the data are allowed to say about how far.
The Ritz zero-asymptote gap is unaffected.

**Non-constant dispersion.** This is where the damage is. The section currently
presents `disp("loglinear")` as the recommended treatment for these data, with a
`loo` gain of 60.6 and a slope of −16.3 as the headline. Under censoring at zero
neither survives. The section would have to be rewritten so that the variance
function is presented as the diagnostic that revealed the problem, and shown to
be largely redundant once the problem is fixed structurally.

That is a more useful result for #191 than the current one, but it is a
different claim and needs different fits.

**Consequences for toxicity estimates.** The 28% `ECx` shift the draft reports is
an artefact of the uncensored comparison. Under censoring the two models agree to
the reported precision. The subsection's argument — that a dispersion sub-model
is not only a correction to error bars — does not survive, at least not on these
data.

**Adequacy of the mean model.** Strengthened rather than weakened. The shoulder
misfit is unchanged, and the observation that a variance function absorbs
lack of fit in the mean now has a second instance: it also absorbs the
consequences of an unmade censoring decision.

**Censoring at the reporting resolution.** Unaffected.

## What `ecx()` already does, and what it does not

Established from `R/ecx.R`. In all branches `y` is a posterior draw of the
**predicted curve** over the x grid (`posterior_epred`, `R/ecx.R:174`, applied at
`R/ecx.R:195`), not observed data.

* `ecx_x_absolute` (`R/ecx.R:330`): `range_y <- c(0, max(y, na.rm = TRUE))`, so
  `ecx_y = max(y) × (1 − x/100)`. The reference range is `[0, fitted control]`.
* `ecx_x_relative` (`R/ecx.R:318`): `range_y <- range(y, na.rm = TRUE)`, so the
  reference range is `[fitted bot, fitted top]`.
* `ecx_x_direct` (`R/ecx.R:342`): `ecx_y <- ecx_val`, an absolute response value.

**The default `type = "absolute"` already references `[0, fitted control]`**,
which is exactly the range the censoring-at-zero proposal is concerned with, and
is algebraically `ErCx`. So the proposal's two halves are not both outstanding:

1. *the reference range for `ECx`* — already handled by the default;
2. *preventing sub-zero observations from determining the curve* — not handled,
   and this is what censoring at zero adds.

The draft's existing claim that `ecx(type = "absolute")` is algebraically `ErCx`
is therefore correct as written, and is not affected by this decision.

Also relevant: `R/ecx.R:159` refuses `type = "absolute"` for a `gaussian`
response unless the model has a `bot` parameter. Every model used in the draft
has one, but this constrains which of the twelve admissible models can be used
if absolute `ECx` is required.

## Costs and open questions

* **Refits.** Sections on censoring and dispersion would both need re-running,
  roughly 16 fits at 30–90 s. The vignette's own knit cost rises by two to four
  fits depending on how many arms are shown.
* **Untested combination.** `cens()` with `disp()` had never been exercised
  together. It works — all 16 fits ran — but only one, `r_salina` × A censored
  with `disp`, sampled badly (R-hat 1.079, ESS 34), consistent with there being no
  signal left to fit. Worth a test in `test-disp_model.R` regardless of the
  vignette decision.
* **The ceiling hypothesis is not fully resolved by this.** The motivating idea
  behind #191 was reduced variability near the maximum achievable growth rate,
  which lives in the *retained* range, not below zero. `c_proliferum` × A still
  gives `c1 = −7.12` excluding zero after censoring, so the effect is not
  eliminated in one of four tests. Whether that is the ceiling or residual mean
  misspecification is not established here.
* **What happens to #191's motivation.** The dispersion feature remains correct
  and well tested, and `disp_validation.md` records two real defects it exposed.
  But `alga` would no longer be an example where it is *needed*, only one where it
  is diagnostic. If the vignette is reframed, #191 should probably gain a comment
  recording that.
