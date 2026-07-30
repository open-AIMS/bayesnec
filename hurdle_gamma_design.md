# Hurdle-Gamma support in bayesnec

Scoping note for modelling growth-rate data where exposure both kills
individuals and suppresses growth in the survivors. Written against
`ignore/collated_WET_data.csv` (snail growth, four independent WET rounds;
round 4 is the original single-round example, also held as
`ignore/nd_exampledat.csv`). Working prototype:
`ignore/hurdle_gamma_prototype.R`; four-round analysis:
`ignore/hurdle_gamma_rounds.R`.

The four rounds are treated as **separate case studies, not replicates** — they
use different concentration ladders, different cohort sizes, and show
qualitatively different balances of growth suppression against mortality. They
are never pooled.

---

## Recommendations at a glance

**On the science**

1. **Fit a hurdle-Gamma to the full exposed cohort**, dead snails coded `y = 0`,
   with a declining bayesnec equation on *both* the growth of survivors (`mu`)
   and survival (`1 - hu`). Report three endpoints: growth, survival, and the
   combined `mu·(1-hu)`. §1.1
2. **Do not describe the historical survivors-only Gamma as wrong.** The hurdle
   likelihood factorises, so it gives the identical growth posterior — verified
   to Monte Carlo error. The gain is the combined endpoint, one model object,
   and somewhere to couple the two processes. Claiming a corrected growth
   estimate would not survive review. §1.2
3. **Do not switch to Tweedie, and do not call this zero-inflated.**
   Zero-inflated Gamma *is* the hurdle. Tweedie welds mortality to the mean and
   would predict ~31% dead controls on round 4. §1.5
4. **Resolve the inferred dead rung before publishing any survival estimate** —
   it is the one assumption that moves the mortality curve, and it is
   unconfirmed for rounds 1–3. §1.4

**On the package**

5. **Ship phases 1–3; phase 4 is optional.** Phase 1 (two latent bug fixes) is
   worth doing regardless of whether hurdle support ever lands. §2.5
6. **The 23-equation concern is resolved** — the hu sub-model generates
   mechanically from each existing `bf_` object, verified against all 23. Store
   the generator, not 23 more objects. §2.2
7. **Two existing bugs will silently corrupt hurdle results**: unanchored
   matching in `extract_pars()` (which cascades into a `nec` model being
   misclassified as `ecx`) and the `check_data()` zero-nudge. Fix and test these
   first. §2.3
8. **Make `hurdle_gamma` opt-in.** Auto-detecting it from zeros in the response
   is a silent breaking change for existing users. §2.4

---

## 1. Statistical recommendation

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
the survivors alone. Fitted both ways on round 4 with identical sub-model,
priors and inits:

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

### 1.3 Results

> **INCOMPLETE — four-round refit interrupted.** Round 4 below is final. Round 1
> is provisional (NEC only, recovered from the run log; ECx not computed). Rounds
> 2 and 3 have not been fitted. See `ignore/HANDOVER_hurdle_gamma.md` for how to
> resume, and note the unresolved question about where each round's inferred
> dead rung sits, which will change rounds 1 and 2 if answered the other way.

**Round 1** (provisional; `x = log(PercentPW + 0.001)`, 204 snails, 38 deaths):

| estimate | median | 95% CI |
|---|---|---|
| NEC, growth of survivors | 0.587 | 0.467 – 0.755 |
| NEC, survival | 1.785 | 1.258 – 2.434 |

`top` 19.8, `hutop` 0.96, `shape` 5.8; 11 divergences in 9000 draws, max Rhat
1.002, min bulk-ESS 2578 — clean.

Note how different this is from round 4: here growth and survival thresholds sit
a factor of 3 apart, against a factor of ~100 in round 4. That is exactly the
cross-round contrast in growth-suppression-versus-mortality the rounds were
assembled to show, and it is the reason they are analysed separately.

**Round 4** (final). Prototype fit, `nec3param` on both components,
`x = log(PercentPW + 1e-4)`, estimates back-transformed to %PW:

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

### 1.4 Data reconstruction

Deaths are never recorded as such. They have to be recovered from four
different traces in the raw file, and getting this wrong silently changes the
mortality curve — which is the entire novel component of the model.

**Trace 1 — sentinel death codes.** Rounds 1–3 record dead snails in-line as a
single large negative constant, one per round:

| round | code | n |
|---|---|---|
| 1 | −46.35 | 8 |
| 2 | −34.61 | 28 |
| 3 | −38.40 | 4 |
| 4 | none — deaths omitted instead | — |

These are exact repeats, not biological variation, and they are cleanly
separated from real data: the next value up the sorted scale is −7.29, a gap of
27 units. The rule used is `y <= -30`.

This matters more than it looks. Taken at face value the codes make round 2 look
like a failed test — mean control growth 7.8 with a CV of 3.6, and mean growth
of −14.8 at 0.1 %PW where rounds 3 and 4 see roughly +18. Once the codes are
removed as deaths, all four rounds agree on control growth: **21.3, 24.8, 22.4,
21.4**. Round 2 is not noisy; it is a round with 29% control mortality.

**Trace 2 — blank reps within a present tank.** Round 4 only (9 snails).

**Trace 3 — absent tanks within a present treatment.** Round 1 at 2.5 %PW has 1
tank of 3; round 4 at 15 %PW has 2 tanks of 4. 30 and 12 snails respectively.

**Trace 4 — the omitted top rung.** Each round's ladder stops where the next
concentration killed everything, and that treatment is dropped from the file
entirely. Per the client, one further rung is assumed fully dead in each round:

| round | ladder ends at | inferred dead rung | cohort |
|---|---|---|---|
| 1 | 2.5 (clean 2× series) | 5.0 | 3 tanks × 6 |
| 2 | 0.1 | 0.3 (next rung of the round-3 series) | 3 tanks × 7 |
| 3 | 15 | 20 | 4 tanks × 6 |
| 4 | 15 | 20 *(known, not inferred)* | 4 tanks × 6 |

Only round 4's dead rung is confirmed. Rounds 1–3 are assumptions, and rounds 1
and 2 are the shakier of them — round 1's series is a clean doubling so 5.0 is
well motivated, but round 2's next rung is read across from round 3's ladder.

**Reconstructed cohorts:**

| round | cohort | alive | deaths | of which coded / blank rep / absent tank |
|---|---|---|---|---|
| 1 | 204 | 166 | 38 | 8 / 0 / 30 |
| 2 | 112 | 63 | 49 | 28 / 0 / 21 |
| 3 | 348 | 320 | 28 | 4 / 0 / 24 |
| 4 | 372 | 327 | 45 | 0 / 9 / 36 |

**Two remaining wrinkles.**

*Treatment labelling.* Round 4's `ID` column reuses the `M` prefix for both 10%
and 15% pore water. Treatment is therefore re-derived from the ordered
concentration throughout rather than parsed from `ID`. Should be fixed at
source.

*Four live snails with small negative growth* (−7.29, −2.75, −1.42, −0.74), all
at high concentrations. These sit far from the death codes and are read as
measurement noise on small snails, i.e. genuinely alive with ~zero growth. Gamma
needs `y > 0`, so they are nudged to half the smallest positive value in their
round — the same device `check_data()` already applies to Gamma zeros. Four
values in 916; immaterial to the result, but it should be stated in the methods
rather than left implicit.

### 1.5 Why not zero-inflated, and why not Tweedie

**Zero-inflated Gamma is the same model.** Zero-inflation adds structural zeros
on top of a base distribution that can *itself* produce zeros:
`P(y=0) = π + (1-π)·f(0)`. Gamma has support (0, ∞), so `f(0) = 0` and the
mixture collapses to the hurdle. brms reflects this — it ships `hurdle_gamma`
and no `zero_inflated_gamma`, and offers zero-inflated forms only for poisson,
negbinomial, binomial and beta_binomial. (Its `zero_inflated_beta` is a
misnomer: Beta's support is open on (0,1), and the generated Stan is the hurdle
form with no `log_sum_exp`.) The ecology and fisheries literature does use
"zero-inflated gamma", and also "delta-gamma" or "two-part"; all mean the
hurdle. Use the hurdle name in the methods and note the synonyms.

**Tweedie is genuinely different, and wrong here.** Compound Poisson-Gamma also
puts a point mass at zero, but ties it to the mean:
`P(y=0) = exp(-mu^(2-p) / (phi·(2-p)))`. There is no free mortality curve. With
`phi` calibrated to reproduce round 4's observed 59% mortality at 15 %PW, a
Tweedie at `p = 1.5` is then forced to predict ~31% dead in the *controls* and
30–43% dead at every concentration from 0.001 to 10 %PW, against an observed
~3%. It structurally cannot represent "growth collapses two orders of magnitude
before anything dies", which is precisely the pattern in rounds 3 and 4.

Nor is it more conservative — it targets the same estimand, `E[y|x]`, so any
difference is misspecification bias of unpredictable sign. And it is not in
brms (the density needs the Dunn–Smyth series approximation), so it would mean
a hand-written `custom_family`, harder than the hurdle rather than cleaner.

The only thing Tweedie offers is a `nec` parameter named directly in the model
instead of a derived one. That is cosmetic, and §2.3 shows the derived version
has a closed form anyway.

---

## 2. Implementation in bayesnec

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

### 2.2 The hu sub-model generator — verified

The concern with a second parameter block was having to hand-write and maintain
23 more `bf_` objects. That is not necessary: the hu block can be derived
mechanically from each existing equation. This generator was run against all 23
and **brms accepted every resulting formula**:

```r
make_hu_block <- function(bf_obj) {
  pars <- names(bf_obj$pforms)
  rhs  <- deparse1(bf_obj$formula[[3]])
  # Longest name first, so "top" inside an already-substituted "hutop" is
  # never rewritten twice. Word boundaries alone are not enough here because
  # bayesnec parameter names are substrings of one another (e.g. bot / top).
  for (p in pars[order(nchar(pars), decreasing = TRUE)]) {
    rhs <- gsub(paste0("(?<![[:alnum:]_])", p, "(?![[:alnum:]_])"),
                paste0("hu", p), rhs, perl = TRUE)
  }
  list(nlf  = as.formula(paste0("hu ~ 1 - (", rhs, ")")),
       lf   = as.formula(paste0(paste0("hu", pars, collapse = " + "), " ~ 1")),
       pars = paste0("hu", pars))
}
```

Spot checks across the equation families:

| model | generated hu sub-model |
|---|---|
| `nec3param` | `hu ~ 1 - (hutop * exp(-exp(hubeta) * (x - hunec) * step(x - hunec)))` |
| `ecx4param` | `hu ~ 1 - (hutop + (hubot - hutop)/(1 + exp((huec50 - x) * exp(hubeta))))` |
| `ecxll5` | `hu ~ 1 - (hubot + (hutop - hubot)/(1 + exp(exp(hubeta) * (x - huec50)))^exp(huf))` |
| `nechormepwr01` | `hu ~ 1 - ((1/(1 + ((1/hutop) - 1) * exp(-exp(huslope) * x))) * exp(...))` |

Two consequences worth noting. First, `data-raw/sysdata.R` stays small — store
the generator, not 23 more objects, and the hu blocks stay automatically in step
if an equation is ever edited. Second, `show_params()` gains the `hu` parameters
for free, since it reads the same `bf_` objects.

Caveat: generating a formula is not the same as it being *sensible*. Several
equations are poor choices for a probability-scale sub-model — anything with a
`slope` term is unbounded above and will push `hu` outside [0, 1] under an
identity link. §2.3 handles that through `check_models()`, not through the
generator.

### 2.3 Work items

Ordered roughly by dependency. The two marked **critical** are correctness
issues that will silently produce wrong answers if missed.

**`data-raw/sysdata.R`** — add `hurdle_gamma = "hurdle_gamma"` to `mod_fams`,
and add `make_hu_block()` from §2.2. Regenerate `R/sysdata.rda`.

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
`grep(x, rownames(fixef(fit)))` unanchored. Traced through, the failure
cascades into a wrong NEC rather than an error:

1. `grep("top", ...)` matches both `top_Intercept` and `hutop_Intercept`, so
   `fef[grep(x, rownames(fef)), cols]` returns a 2×3 matrix, not a named vector.
2. `tt["Estimate"]` on a matrix returns `NA`, so `extract_pars()` hits its
   `if (is.na(tt["Estimate"])) NA` branch and returns `NA` — for *every*
   parameter (`top`, `beta`, `nec`, `bot`, `ec50`, `slope`, `d`, `f`).
3. `expand_nec()` then does
   `if (is.na(extracted_params$ne["Estimate"])) mod_class <- "ecx"`, so a
   `nec3param` hurdle fit is **misclassified as an ecx model** and its NEC is
   silently computed as an interpolated NSEC from the curve instead of read from
   `b_nec_Intercept`.

The fix is anchoring plus a second pass for the prefixed block:

```r
extract_pars <- function(x, model_fit, prefix = "") {
  fef <- fixef(model_fit, robust = TRUE)
  tt <- fef[grep(paste0("^", prefix, x, "_"), rownames(fef)),
            c("Estimate", "Q2.5", "Q97.5")]
  if (length(tt) == 0 || is.na(tt["Estimate"])) NA else tt
}
```

Note `length(tt) == 0` guards the no-match case, which the current version does
not handle either. Verified: anchoring resolves `top`/`hutop`, `beta`/`hubeta`
and `nec`/`hunec` cleanly.

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
NEC. Store all three: growth NEC, survival NEC, and the combined-endpoint NEC.
Recommend the combined one as the default returned by `nec()`, with the
components reachable explicitly. Also store the `dpar = "mu"` and `dpar = "hu"`
prediction curves alongside the existing `pred_vals`.

For threshold (`nec`-type) equations on both components the combined NEC needs
no numerical search — it has a closed form. Below both thresholds `mu = top` and
`1 - hu = hutop`, so the product is flat and departs its plateau at whichever
threshold binds first:

```r
ne_posterior <- pmin(draws$b_nec_Intercept, draws$b_hunec_Intercept)
```

Verified against the numerically-detected breakpoint of the combined
`posterior_epred` curve — agreement to within the prediction grid resolution.
This is exact only when both components are threshold models; for `ecx`-type
equations on either component, fall back to the numerical N(S)EC from the
combined curve, as `expand_nec()` already does for `ecx` models.

A useful ordering property worth exposing in the docs: because both components
decline, the combined ECx is always reached at or below either component's own
ECx (confirmed in 6000/6000 posterior draws at both EC10 and EC50 on round 4).
The combined endpoint is therefore conservative by construction relative to
analysing growth or survival alone.

**`R/ecx.R`, `R/nsec.R`** — add a `dpar` argument
(`"combined"` default / `"mu"` / `"hu"`) passed through to `posterior_epred`.
For the `hu` scale, invert to `1 - hu` before applying `ecx_x_absolute` so that
"decline from control" means the same thing as everywhere else.

**`R/plot.R`, `R/autoplot.R`** — work unchanged for the combined curve. Adding a
three-panel option (growth of survivors / survival / combined) is the
worthwhile enhancement.

**No change needed**: `dispersion()` (returns `numeric()` for anything outside
poisson/binomial), `bnec_newdata.R`, `model.frame.R`.

**Docs** — `bnec()` family documentation, `models()` restrictions text, and a
vignette section; the survivors-only-vs-joint comparison in §1.2 is a good
worked example for it. Tests are specified separately in §2.6.

### 2.4 Backwards compatibility

Almost all of this is additive, but **one proposed change is a silent breaking
change** and should not ship as written.

The `set_distribution()` auto-detection item would make a numeric response with
`max > 1` and exact zeros return `"hurdle_gamma"` where it currently returns
`"Gamma"`. Any existing user who has been fitting zero-containing data —
relying, knowingly or not, on `check_data()` nudging those zeros up to
`min(y[y > 0])/10` — would silently get a structurally different model, a second
parameter block, and different ECx values, with no error and no deprecation
warning. Re-running an old script would not reproduce the published result.

Recommended instead:

- Leave `set_distribution()` returning `"Gamma"` for zero-containing data.
- Emit a **message** when zeros are present and the family is Gamma, pointing at
  `hurdle_gamma` and noting that the zeros are currently being nudged. That
  makes the existing behaviour visible, which it currently is not.
- Make `hurdle_gamma` strictly opt-in via `family = hurdle_gamma()` for at least
  one release cycle, and revisit auto-detection once there is field experience.

Everything else is additive: new family in `mod_fams`, new branches keyed on
`fam_tag`, a new optional `dpar` argument defaulting to existing behaviour. The
`extract_pars()` anchoring fix changes behaviour only where the current code
would already be wrong, and adds a `length(tt) == 0` guard the current version
lacks. No existing fitted objects are invalidated.

One genuine limitation to document rather than fix: `loo` comparison and
`bayesmanecfit` weighting across a hurdle model suite is comparing complete
two-component models. That is coherent — same response, same observations — but
weights are *not* comparable against a non-hurdle suite fitted to the
survivors-only subset, because the two are fitted to different data. `amend()`
already refuses to mix families via `has_family_changed()`; that guard should be
confirmed to fire for hurdle-vs-Gamma.

### 2.5 Phased plan

Four increments, each independently testable and shippable.

**Phase 1 — fix the latent bugs (no hurdle support yet).** The anchored
`extract_pars()` with its `length(tt) == 0` guard, and the `check_data()` zero
handling made explicit and messaged. Both are defensible on their own merits and
carry no hurdle dependency, so they can go in first and de-risk everything after.
*Small: a day, mostly tests.*

**Phase 2 — single-model hurdle fit.** `mod_fams` entry, `validate_family()`
link handling, `make_hu_block()` in `data-raw/sysdata.R`, `check_data()` hurdle
branch, `check_models()` intersection, `define_prior()` two-block priors,
`response_link_scale()` positive-subset branch, `make_good_inits()` run twice
and merged. Target: `bnec(y ~ crf(x, "nec3param"), family = hurdle_gamma())`
returns a valid `bayesnecfit`. *The bulk of the work — priors and inits are
where the time goes, not the formula.*

**Phase 3 — the estimates.** `expand_nec()` storing all three N(S)EC posteriors
and both component curves; the `pmin(nec, hunec)` closed form; `dpar` argument
on `ecx()`/`nsec()`. Target: all three endpoints reportable with correct
intervals. *Moderate.*

**Phase 4 — polish.** Model averaging across a hurdle suite, three-panel
`plot()`/`autoplot()`, the `hu()` formula term for a differently-shaped survival
sub-model, vignette. *Phase 4 is genuinely optional — phases 1–3 deliver the
scientific capability.*

Sequencing note: phase 2's initial-value work is the schedule risk, not the
formula construction. Budget accordingly.

### 2.6 Test plan

The two critical items fail silently, so they need tests that would catch a
regression rather than tests that merely exercise the path.

`tests/testthat/test-hurdle.R`:

- **Zeros survive `check_data()`** for `hurdle_gamma` and are still nudged for
  `Gamma`. Assert on the returned `mod_dat$y`, not on the fit.
- **`extract_pars()` anchoring.** Construct a `fixef`-shaped matrix with both
  `top_Intercept` and `hutop_Intercept`; assert a length-3 named vector comes
  back, not `NA`. This is the regression test for the cascade in §2.3 and does
  not need a fitted model.
- **`expand_nec()` classifies a hurdle `nec3param` as `"nec"`, not `"ecx"`** —
  the downstream symptom of the same bug, and the one a user would actually
  notice.
- **Priors are built from the positive subset.** Assert the `top` prior's rate
  parameter matches one computed from `y[y > 0]`, so re-including zeros fails.
- **`make_hu_block()` round-trips all 23 equations** and brms accepts each
  combined formula. Cheap — no sampling required, just `make_stancode()`.
- **`pmin(nec, hunec)` matches the numerical breakpoint** of the combined
  `posterior_epred` curve, on a small stored fit.
- **`check_models()` drops slope-bearing equations** for the hu component.

Sampling-dependent tests belong in `tests/local/`, following the existing split:
one short hurdle fit asserting that all three endpoints are finite, ordered
(`combined <= min(growth, survival)`), and that `posterior_epred(fit)` equals
`mu * (1 - hu)`.

### 2.7 Risks

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

### 2.8 Related families worth folding in at the same time

The same scaffolding generalises at low marginal cost:

- `hurdle_lognormal` — same structure, for right-skewed growth data where the
  log-normal fits better than the Gamma.
- `zero_inflated_beta` / `zero_one_inflated_beta` — the equivalent problem for
  bounded proportional endpoints (e.g. proportional cover with true zeros),
  where bayesnec currently nudges zeros away in exactly the same way.

Implementing `hurdle_gamma` with the parameter-block generalisation done
properly makes these mostly a matter of adding family entries and prior
defaults.
