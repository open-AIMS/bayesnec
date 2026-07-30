# Hurdle-Gamma support in bayesnec

Scoping note for modelling growth-rate data where exposure both kills
individuals and suppresses growth in the survivors. Written against
`ignore/nd_exampledat.csv` (snail growth, 15 concentrations of percent
pore water). Working prototype: `ignore/hurdle_gamma_prototype.R`.

---

## 1. Recommendation

### 1.1 The endpoint

Treat the exposed cohort — not the surviving cohort — as the sample. Every
snail that started the test contributes one row. Snails that died contribute
`y = 0`; snails that lived contribute their measured growth increment.

The response is then a mixture of a point mass at zero and a positive
continuous distribution, i.e. a **hurdle-Gamma**:

```
y = 0                     with probability  hu(x)
y ~ Gamma(mu(x), shape)   with probability  1 - hu(x)
```

Both components get their own concentration-response curve:

| component | interpretation | curve |
|---|---|---|
| `mu(x)` | mean growth **given the snail survived** | declining bayesnec equation |
| `1 - hu(x)` | probability of survival | declining bayesnec equation |
| `mu(x) * (1 - hu(x))` | expected growth **per snail exposed** | derived |

This gives three toxicity estimates from one fit — a growth NEC/ECx, a survival
NEC/ECx, and a combined-endpoint NEC/ECx — and the combined endpoint is exactly
the quantity being asked for: the impact on growth *given snails are still
alive*, reported alongside and jointly with the mortality that produced the
survivor set.

Assigning `y = 0` to a dead snail is a definitional choice, not a measurement.
It is defensible for a population-level growth endpoint (a dead snail
contributes no biomass increment), and it is the choice that makes the combined
curve mean something. It is *not* right if the intended endpoint is
individual-level physiological growth rate — in that case death is missingness,
not a zero. Worth stating explicitly in the methods.

### 1.2 What this does and does not fix

One result from the prototype matters for how this gets written up, and it cuts
against the stated motivation.

**The current survivors-only Gamma analysis is not wrong for the endpoint it
estimates.** The hurdle likelihood factorises exactly:

```
L = prod_i  hu_i^{1[y=0]} * ((1 - hu_i) * Gamma(y_i | mu_i, shape))^{1[y>0]}
```

The `hu` terms and the `mu`/`shape` terms share no parameters, so the posterior
for the growth block is the same whether you fit the hurdle model or a Gamma to
the survivors alone. Fitted both ways on the example data with identical
sub-model, priors and inits:

| parameter | Gamma, survivors only | hurdle-Gamma, joint |
|---|---|---|
| `top` | 20.02 [18.97, 21.29] | 19.96 [18.99, 21.21] |
| `beta` | −1.841 [−2.211, −1.474] | −1.837 [−2.217, −1.486] |
| `nec` | −1.964 [−3.579, −0.845] | −1.942 [−3.550, −0.854] |
| `shape` | 10.71 [9.16, 12.50] | 10.73 [9.19, 12.43] |

Agreement to Monte Carlo error, as the algebra requires.

So the gain from the hurdle model is *not* a corrected growth estimate. It is:

1. **The combined endpoint**, with uncertainty propagated coherently through
   both components in a single posterior. This cannot be obtained by running
   two separate models and multiplying point estimates.
2. **One model object** — one `loo`, one set of model-averaging weights, one
   `plot`, one `bayesmanecfit`. Model selection is over whole models rather
   than over two independently-weighted model sets that may disagree about
   which equation is best.
3. **A place to couple the two processes.** Once you add a shared tank-level
   random effect, or constrain the two curves to share a parameter, the
   factorisation breaks and joint fitting becomes genuinely necessary rather
   than merely convenient. This is where the real statistical argument for the
   joint model lives, and it is worth doing given the tank structure in the
   data.

What the hurdle model does **not** fix is survivor selection bias in the causal
sense. At high concentrations the survivors are a non-random subset of the
original cohort — probably the more robust individuals — so `mu(x)` estimates
growth among *those who happened to survive at x*, not growth of a typical
snail had it survived. Correcting that needs a shared latent frailty linking an
individual's survival propensity to its growth potential, which is a much
stronger modelling assumption and is not identified from these data. Say so
rather than implying the hurdle model has dealt with it.

### 1.3 Results on the example data

Prototype fit, `nec3param` on both components, `x = log(PercentPW + 1e-4)`,
estimates back-transformed to %PW:

| estimate | median | 95% CI |
|---|---|---|
| NEC, growth of survivors | 0.138 | 0.027 – 0.42 |
| NEC, survival | 14.2 | 11.2 – 14.9 |
| EC10, growth of survivors | 0.271 | 0.068 – 0.68 |
| EC10, survival | 14.0 | ~0 – 14.9 |
| EC10, combined | 0.271 | 0.068 – 0.68 |

Growth is roughly two orders of magnitude more sensitive than survival, so the
combined EC10 is driven entirely by the growth component and coincides with it.
For *this* dataset the hurdle model and the historical survivors-only analysis
would report near-identical protective concentrations. The endpoints only
diverge in the upper part of the curve, where mortality takes over.

Caveats from the fit. Deaths occur only at 15% and 20% pore water, and the 20%
treatment has no growth observations at all, so the survival curve is weakly
identified — its shape rests on two concentrations. This shows up as sensitivity
to sampler settings: at `adapt_delta = 0.95` the fit had 81/6000 divergent
transitions and tail-ESS around 110 on `hubeta`/`hunec`; at `adapt_delta = 0.99`
(the setting in the prototype script) that improves to 15 divergences and
tail-ESS around 1200. The survival EC10 median moved from 11.5 to 14.0 between
the two runs, which is the identifiability problem rather than a sampling
artefact — the survival credible interval spans nearly the whole tested range
and should be reported as such, or censored. Raising `adapt_delta` fixes the
geometry, not the information content.

### 1.4 Data reconstruction — needs confirmation

Two things in `nd_exampledat.csv` must be resolved before any of this is a real
analysis:

1. **The `ID` column mislabels the 15% treatment.** Rows at `PercentPW == 15`
   carry IDs `M1`/`M2`, the same prefix used for 10%. Letters A–O map 1:1 onto
   the 15 ordered concentrations, so the prototype re-derives treatment from
   concentration and ignores `ID`. That resolves it cleanly, but it should be
   fixed at source.
2. **Cohort sizes are assumed, not given.** The prototype assumes 36 snails
   started in the control and 24 in every other treatment, giving 45 deaths
   across the experiment. Alive counts per treatment are
   A 36, B 24, C 23, D 24, E 24, F 22, G 24, H 21, I 24, J 24, K 24, L 24,
   M 22, N 11, O 0.
3. **Tank allocation of the deaths at 15% and 20% is unknown.** The two
   NA-response rows are placeholders meaning "the rest of this treatment died",
   not identified tanks. Irrelevant for a treatment-level model; it matters as
   soon as a tank-level random effect is added — which is exactly the extension
   that would justify joint fitting.

---

## 2. Feasibility in bayesnec

Verified: brms 2.22.0 accepts the construction, generates correct Stan code,
and samples. The formula is

```r
bf(y ~ top * exp(-exp(beta) * (x - nec) * step(x - nec)),
   top + beta + nec ~ 1, nl = TRUE) +
  nlf(hu ~ 1 - hutop * exp(-exp(hubeta) * (x - hunec) * step(x - hunec))) +
  lf(hutop + hubeta + hunec ~ 1)
```

with `hurdle_gamma(link = "identity", link_hu = "identity")`. Identity links
keep every parameter on the natural response scale, which is what bayesnec's
priors, initial-value search and `top`/`nec` interpretation already assume.
Writing `hu` as `1 - <survival curve>` means the survival sub-model is
monotonically declining, matching the sign convention of every bayesnec
equation — so the existing 23 equations can be reused unchanged for the second
component.

Three pieces of existing machinery fall out for free:

- `posterior_epred(fit)` returns `mu * (1 - hu)` for `hurdle_gamma`. Since
  `expand_nec()`, `ecx()`, `nsec()` and `plot()` all route through
  `posterior_epred`, **the combined endpoint is what they compute by default**,
  with no change to those functions.
- `posterior_epred(fit, dpar = "mu")` and `dpar = "hu"` give the component
  curves, so per-component ECx needs only a pass-through argument.
- Raw data plot correctly against the combined curve, because the zeros are on
  the same scale.

### 2.1 Proposed user API

Default: the `hu` component reuses whichever equation `crf()` names, so model
averaging stays coherent (each candidate is a complete two-component model, and
`loo` compares like with like).

```r
bnec(y ~ crf(log_x, "nec3param"), data = dat, family = hurdle_gamma())
bnec(y ~ crf(log_x, "nec"),       data = dat, family = hurdle_gamma())
```

Override, for when the survival response has a different shape from the growth
response (likely — mortality is often more threshold-like):

```r
bnec(y ~ crf(log_x, "all") + hu(log_x, "nec3param"), data = dat,
     family = hurdle_gamma())
```

A full cross of 23 mu-equations by 23 hu-equations is not worth offering; the
same-shape default plus a fixed-hu override covers the useful cases.

### 2.2 Work items

Ordered roughly by dependency. The two marked **critical** are correctness
issues that will silently produce wrong answers if missed.

**`data-raw/sysdata.R`** — add `hurdle_gamma = "hurdle_gamma"` to `mod_fams`.
Rather than hand-writing 23 more `bf_` objects, generate the `nlf()`/`lf()` hu
block programmatically from each existing `bf_<model>`: prefix the non-linear
parameter names with `hu` and wrap the RHS as `1 - (...)`. Regenerate
`R/sysdata.rda`.

**`R/validate_family.R`** — `get(family)(link = "identity")` needs to pass
`link_hu = "identity"` for hurdle families. Currently hard-codes a single
`link` argument.

**`R/set_distribution.R`** — auto-detection. A numeric `y` with `max > 1` and
exact zeros present currently returns `"Gamma"`; it should return
`"hurdle_gamma"`. Distinguish "has exact zeros" from `min >= 0`.

**`R/check_data.R` — critical.** Lines 51–54 nudge zero responses up to
`min(y[y > 0]) / 10` for the Gamma family. For a hurdle family the zeros *are*
the signal and must be left alone. This guard has to be skipped, or the hurdle
component will see no zeros and the model becomes unidentifiable.

**`R/helpers.R`, `extract_pars()` — critical.** Uses
`grep(x, rownames(fixef(fit)))` unanchored, so `grep("top", ...)` will match
both `top_Intercept` and `hutop_Intercept` and return a two-row matrix where a
named vector is expected. Needs `^` anchoring plus a separate extraction pass
for the `hu`-prefixed block. This is the single most likely source of a silent
wrong answer.

**`R/check_models.R`** — hurdle_gamma must satisfy both sets of existing link
restrictions simultaneously: the Gamma/identity rules for `mu` (drop `neclin`,
`neclinhorme`, `ecxlin`, `nechormepwr01`) and the 0-1-bounded/identity rules for
`hu` (drop `neclin`, `neclinhorme`, `ecxlin`). Take the intersection.

**`R/define_prior.R`** — emit both parameter blocks. mu-block priors are the
existing Gamma/identity defaults but **computed on `response[response > 0]`**;
including the zeros drags the `top` and `bot` quantiles down and produces
priors centred well below the real control mean. hu-block priors are the
existing bernoulli/identity defaults (`beta(5, 2)` on `hutop`, bounds 0–1),
with `hunec`/`huec50` reusing the predictor-scaled priors. A prior on `shape`
should be added too.

**`R/helpers.R`, `response_link_scale()`** — needs a hurdle branch that scales
the mu part against the positive subset only.

**`R/inits_functions.R` and `add_brm_defaults()`** — `make_good_inits()` needs
to run twice and merge chain-wise: once against `y[y > 0]` for the mu block,
once against the per-concentration proportion alive for the hu block. It can
reuse the same `pred_<model>()` function both times, renaming `b_top` → `b_hutop`
etc. afterwards and taking `1 - value` for the hu target range. `make_inits()`
derives parameter names from the prior object, so it will pick the prefixed
names up automatically once the priors above exist.

**`R/expand_classes.R`, `expand_nec()`** —
`ne_posterior <- as_draws_df(fit)[["b_nec_Intercept"]]` captures only the growth
NEC. Store all three: growth NEC, survival NEC, and the combined-endpoint
N(S)EC derived from the combined `posterior_epred` curve. Recommend the combined
one as the default returned by `nec()`, with the components reachable
explicitly. Also store the `dpar = "mu"` and `dpar = "hu"` prediction curves
alongside the existing `pred_vals`.

**`R/ecx.R`, `R/nsec.R`** — add a `dpar` argument
(`"combined"` default / `"mu"` / `"hu"`) passed through to `posterior_epred`.
For the `hu` scale, invert to `1 - hu` before applying `ecx_x_absolute` so that
"decline from control" means the same thing as everywhere else.

**`R/plot.R`, `R/autoplot.R`** — work unchanged for the combined curve. Adding a
three-panel option (growth of survivors / survival / combined) is the
worthwhile enhancement.

**No change needed**: `dispersion()` (returns `numeric()` for anything outside
poisson/binomial), `bnec_newdata.R`, `model.frame.R`.

**Docs and tests** — `bnec()` family documentation, `models()` restrictions
text, a `test-hurdle.R` covering the zero-preservation guard in `check_data()`,
the anchored `extract_pars()` behaviour, and prior generation on the positive
subset. Plus a vignette section; the survivors-only-vs-joint comparison in
§1.2 is a good worked example for it.

### 2.3 Effort and risk

The formula construction and the downstream prediction machinery are the easy
parts — brms does the work and `posterior_epred` already returns the right
thing. The cost is concentrated in the surrounding bayesnec scaffolding that
currently assumes one parameter block per model: priors, initial values,
parameter extraction, and the `pred_vals`/`ne_posterior` structures in
`expand_nec()`.

Main risks:

- `extract_pars()` unanchored matching, and the `check_data()` zero-nudge, both
  fail silently rather than erroring. Test these first.
- Initial-value search is already the fragile part of bayesnec (see the
  `refine_inits` work on the Issue-157 line of development); doubling the
  parameter count over two differently-scaled blocks will make it more so.
- Weak identifiability of the survival curve whenever mortality is confined to
  the top one or two concentrations — which will be the common case in practice,
  and was the case here. Worth a diagnostic warning when the number of distinct
  concentrations with any mortality is below, say, three.

### 2.4 Related families worth folding in at the same time

The same scaffolding generalises at low marginal cost:

- `hurdle_lognormal` — same structure, for right-skewed growth data where the
  log-normal fits better than the Gamma.
- `zero_inflated_beta` / `zero_one_inflated_beta` — the equivalent problem for
  bounded proportional endpoints (e.g. proportional cover with true zeros),
  where bayesnec currently nudges zeros away in exactly the same way.

Implementing `hurdle_gamma` with the parameter-block generalisation done
properly makes these mostly a matter of adding family entries and prior
defaults.
