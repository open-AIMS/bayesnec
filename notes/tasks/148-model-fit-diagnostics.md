# Task — bayesnec #148: model fit diagnostics

**Point a Claude Code session at this file from the `bayesnec` repo root.**
Read `notes/implementation/00_protocol.md` first for the working rules, then
this. Self-contained otherwise.

Issue: https://github.com/open-AIMS/bayesnec/issues/148
Absorbs: https://github.com/open-AIMS/bayesnec/issues/56 (closed 2026-08-21)
Absorbs: the diagnostic helpers originally listed on #219 (Part D below)
Blocks: https://github.com/open-AIMS/bayesnec/issues/219 (workflow vignette)

---

## The gap

Every existing diagnostic is about the sampler (`check_chains()`, `rhat()`),
the priors (`check_priors()`), or *relative* model ranking (`loo`/`waic` in
`mod_stats`). The one exception, `dispersion()`, is restricted to Poisson and
Binomial (`R/dispersion.R:53`), so eight of the ten supported families have no
variance diagnostic at all.

Nothing asks whether the fitted model's implied variability, or its fitted
level at the control, matches the data. Both land on `nsec`.

## Why it lands on `nsec`

`nsec` sets its reference at `R/nsec.R:155`:

```r
reference <- quantile(p_samples[, 1], sig_val)
```

`p_samples` is `posterior_epred`, and with the default `x_range = NA`
`bnec_newdata()` builds the grid as `seq(min(x_vec), max(x_vec), ...)`
(`R/bnec_newdata.R:48`). So column 1 is the fitted curve **at `min(x)`** — the
same rows `check_data.R:76` already calls the control. Therefore:

- mis-stated control **variance** biases the reference in *spread*;
- a curve **pulled away from the control data** biases it in *location*.

The location half is the worse of the two: it is a bias in the point estimate,
not in the interval. `ecx` is comparatively insulated — it is read relative to
the curve's own asymptotes.

D6 in `03_decisions.md` records the empirical case: on the `example1`
simulation `nsec` moved 0.93 [0.43, 1.32] to 1.09 [0.50, 1.53] under
`disp("power")` while `ecx` tightened. Same estimator, different variance model.

## Why a global statistic cannot do it

For any family with a free dispersion parameter (`gaussian`, `Gamma`,
`negbinomial`, `Beta`, `beta_binomial`) that parameter absorbs exactly what a
global Pearson-ratio statistic measures.

Measured on the packaged `manec_example` (gaussian, `nec4param`):

| | |
|---|---|
| global Pearson ratio | **1.011 [0.71, 1.44]** — a clean bill of health |

The same fit, residual SD in six quantile bins of `x`:

| bin | 0.03–0.34 | 0.34–0.61 | 0.61–0.88 | 0.88–1.36 | 1.36–1.77 | 1.77–3.22 |
|---|---|---|---|---|---|---|
| obs resid SD | 0.412 | 0.292 | 0.430 | 0.548 | 0.320 | 0.876 |
| sim resid SD | 0.525 | 0.510 | 0.503 | 0.508 | 0.508 | 0.525 |
| ratio | **0.79** | 0.57 | 0.86 | 1.08 | 0.63 | **1.67** |

The constant-`sigma` model simulates a flat ~0.51. In the control region it
simulates 27% more variability than the data show, and the global statistic
reported 1.01. **The diagnostic has to be local.**

## Two facts that shape the implementation

**Statistics must be residual-based, not raw.** Within a bin the raw SD of `y`
mixes residual variability with the slope of the curve across the bin. On
`manec_example` the top bin's raw SD is 1.72 against a residual SD of 0.88 —
the difference is entirely curve. Raw grouped SD flags every steep region as
over-dispersed.

**Model averaging does not protect against control misfit.** Stacking weights
come from a global `elpd`. The control is a handful of rows out of many, so a
candidate can hold high weight while fitting the control badly — it wins on the
bulk of the curve and pays almost nothing for the control. The table must
therefore report per-candidate-model rows as well as the averaged row for a
`bayesmanecfit`.

---

## Scope

### Part A — `pp_check()` methods

`importFrom(brms, pp_check)`. `brms` is in `Depends`, so **no new dependency**.

| class | behaviour |
|---|---|
| `bayesnecfit` | delegate to `pull_brmsfit()` |
| `bayesmanecfit` | model-averaged `yrep`, rendered in `ggplot2` (already in `Depends`) — **not** `bayesplot`, and **not** faceted by candidate model, which never shows the object being used for inference |
| `bayesnechurdlefit` | one result per component, following `dispersion()`'s precedent, **plus** a combined simulation (below) |

Document the LOO-PIT recipe here — this is what #56 becomes.

### Part B — `check_fit()`

One exported generic across the three classes. A table, one row per group:

| column | |
|---|---|
| `obs_mean`, `sim_mean`, `ratio`, `ppp` | location — does the curve go through the data here |
| `obs_sd`, `sim_sd`, `ratio`, `ppp` | scale — is the simulated variance representative |
| `p_zero_obs`, `p_zero_sim`, `ppp` | two-block families only |
| `control` | flag on the `x == min(x)` row |

Plus `plot`/`autoplot` methods.

Generalises `dispersion()` past Poisson/Binomial as a side effect. `dispersion()`
**stays** — it is documented in `example1` and appears in `mod_stats`. Keep the
global Pearson statistic where it is informative (the two families with no free
dispersion parameter) and have the docs say when each applies.

### Part C

Folded into A.

### Part D — sampler diagnostics and screening

Folded in from #219. `check_fit()` asks *does this model reproduce the data*;
Part D asks *did this model sample properly*. Both answer the same downstream
question — which candidates belong in the averaged set — and both feed the same
`amend(drop = )` step and the same `summary()` block, so they are designed once.

**D1. `check_sampling()`** — one data frame, one row per candidate model:

| model | max_rhat | min_ess_ratio | n_divergent | failed |
|---|---|---|---|---|

Sits with `check_chains()` / `check_priors()` / `check_fit()`. `rhat()` stays
unchanged — it is documented in `example2` and in the JSS paper.

**D2. Implementation routes — both verified on `manec_example`.**

- Divergences: `brms::nuts_params(x, pars = "divergent__")`, then `sum(np$Value)`.
  Exported, backend-agnostic, **no new dependency**. Do *not* use
  `rstan::get_num_divergent(x$fit)` as the project scripts do — `rstan` is only
  in `Suggests`, and that route assumes the rstan backend.
- ESS: `brms::neff_ratio()` is exported and free. Measured on `nec4param` from
  `manec_example` (`ndraws` = 100), it is **`min(ess_bulk, ess_tail) / ndraws`**
  in every row:

  | parameter | `ess_bulk` | `ess_tail` | `neff_ratio` |
  |---|---|---|---|
  | `b_bot_Intercept` | 10.16 | 34.54 | 0.102 |
  | `b_top_Intercept` | 94.78 | 75.57 | 0.756 |
  | `b_beta_Intercept` | 13.30 | 44.04 | 0.133 |
  | `b_nec_Intercept` | 57.97 | 32.96 | 0.330 |

  The project screen instead used `posterior::summarise_draws()` to threshold on
  **bulk** ESS specifically. That reports bulk and tail separately but adds
  `posterior` to `Imports`. **Settled: `neff_ratio()`, multiplied back by
  `ndraws()` to give an absolute `min_ess`** — no dependency, and since
  `neff_ratio` is `min(bulk, tail)/ndraws`, `min_ess > 400` is exactly Vehtari's
  "both bulk and tail exceed 100 per chain". See the settled block below.

**D3. The screening helper.** Table as primitive, thin wrapper on top: apply the
thresholds, `amend(drop = )`, and **message what was dropped and why** — that
message is what a methods section cites, not decoration.

**D4. The guard lives in the screening function, not in `amend()`.** The union
of failure lists can name a model that was never fitted or was already dropped,
so the wrapper does `intersect(remove_fits, names(x$mod_fits))` before calling
`amend()`. `amend()` itself is **not** modified — its behaviour was measured and
the case that matters is narrower than the project comment claims; see the
verified table below.

**D5. Merges with the `summary()` decision.** The settled decision to add a
`summary()` line for the control fit check becomes **one** summary block covering
both axes rather than two independent additions. Also the moment to deal with the
hard-coded 1.05 in `print.manecsummary` (`R/print.R:136`) and to add divergences,
which the summary never mentions. That in turn makes
`vignettes/example2.Rmd.orig:102` true — it currently claims the summary warns
about divergent transitions, and it does not.

**Sequencing: Part D is the independent half.** It touches no posterior
predictive machinery, needs no `loo`, and **does not depend on PR #217** — the
diagnostics are per candidate model, so nothing needs a reproducible averaged
draw. A and B both do. Build and merge D first while A and B wait on #217; that
also gets screening into the package before #219 is written.

## Settled decisions — Part D

| | |
|---|---|
| D1 | `check_sampling()`, one data frame, one row per candidate model |
| D2 | `brms::neff_ratio()` x `ndraws()` -> **`min_ess`** (screened) plus `min_ess_ratio` (reported). No new dependency, backend-agnostic |
| D3a | Rhat **1.01** (aligned across `rhat()`, `summary()` and the new functions), ESS **400**, divergences **10**; every threshold an argument, those as defaults only |
| D3b | The screen acts on sampler diagnostics only, **not** on `check_fit()` |
| D4 | Guard lives in the screening function; **`amend()` is not touched** |
| D5 | One `summary()` block covering sampler and fit |

Signature, `*_cutoff` matching `rhat()`'s existing naming:

```r
check_sampling(x, rhat_cutoff = 1.01, ess_cutoff = 400, divergence_cutoff = 10)
```

The screening function takes the same four and passes them through.
No open decisions remain on Part D.

**Reference for 1.01.** Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., &
Bürkner, P.-C. (2021). Rank-normalization, folding, and localization: An improved
R̂ for assessing convergence of MCMC (with discussion). *Bayesian Analysis*,
16(2), 667–718. doi:10.1214/20-BA1221. — "at convergence, Rhat <= 1.01". Already
the reference the project methods text cites. Add to the docs.

**Divergences at 10 has no literature behind it.** Stan's guidance is that *any*
divergence indicates the sampler failed to explore the posterior and estimates
may be biased. 10 is pragmatic, from practice with these non-linear models, which
routinely produce a handful near the boundary. **The docs must say that plainly**
— it will be quoted in methods sections, and the honest version is more
defensible than an implied citation.

**The dependency rule was downgraded.** `00_protocol.md` "no new package
dependencies" was a stop-and-ask rule for autonomous sessions, not a package or
CRAN constraint, and should not have appeared in a definition of done. Amended
2026-08-21. Part D needs no dependency regardless.

**Backend agnosticism — the reason to avoid the obvious route.** `nuts_params()`
and `neff_ratio()` are `brmsfit` generics and work under either backend.
`rstan::get_num_divergent(x$fit)` reaches past `brms` into the `stanfit` slot and
`rstan` is `Suggests`-only. Put that reasoning in a comment at the point of
choice — `get_num_divergent()` is the obvious thing to reach for and works often
enough to look correct.

### ESS — settled: report the absolute, threshold at 400

`neff_ratio()` is `min(ess_bulk, ess_tail) / ndraws`, so multiplying back gives
the absolute with no dependency and no arithmetic left for the user:

```r
min_ess <- min(brms::neff_ratio(x)) * brms::ndraws(x)
```

That is the minimum over parameters of `min(bulk-ESS, tail-ESS)`. Vehtari
recommends **both** bulk and tail ESS exceed 100 per chain, so `min_ess > 400` at
four chains is exactly the recommendation.

Columns: **`min_ess`** (screened on) and **`min_ess_ratio`** (reported). Both fall
out of the same call, and the ratio separates "passed 400 because we drew 8000"
from "passed 400 efficiently".

**Document, do not let it be discovered:** at 3 chains / `thin = 3` / ~999
retained draws, ESS 400 needs a ratio of 0.40 — demanding for these correlated
non-linear parameterisations. Under `bayesnec` defaults (8000 draws) it is a
ratio of 0.05 and trivially met. So the recommendation will fail some heavily
thinned fits that the old 0.1 ratio passed. **The correct response is to retain
more draws, not lower the cutoff** — thinning lowers ESS by construction. Say so
in the help page, because the tempting fix is the wrong one.

### Rhat 1.01 — aligned everywhere, and `summary()` does not compute Rhat

`summary()` builds `rhat_issues` at `R/summary.R:146` as
`map(x$mod_fits, "fit") |> map(has_r_hat_warnings)`, and `has_r_hat_warnings()`
at `R/helpers.R:262` is:

```r
any(grepl("some Rhats are > 1.05", x, fixed = TRUE))
```

It greps **brms's captured warning text for a literal string**. The summary's
threshold was never `bayesnec`'s to set, and aligning it to 1.01 means replacing
the mechanism with a real `rhat(x, rhat_cutoff = )` call — not changing a number.

Good change regardless of the threshold: `brms (>= 2.23.0)` is a floor, not a
ceiling, so if brms rewords or retunes that warning `has_r_hat_warnings()`
returns `FALSE` for every model, silently, and the summary stops warning.
Verified it currently works (`manec_example`: both `TRUE`, warning fires), so
this is latent fragility, not a live bug — but it is the failure mode where
**silence reads as a pass**, the same argument that settled the `bnec()` message
threshold.

Touchpoints:

| | |
|---|---|
| `R/rhat.R:41`, `R/rhat.R:62` | `rhat.bayesnecfit` / `rhat.bayesmanecfit` defaults 1.05 → 1.01 |
| `R/bayesnechurdlefit-methods.R:470` | `rhat.bayesnechurdlefit` default, same |
| `R/summary.R:146` | replace `has_r_hat_warnings` with a computed `rhat()` verdict |
| `R/print.R:136` | message hard-codes "Rhats > 1.05"; report the cutoff in use |
| `R/helpers.R:262` | `has_r_hat_warnings()` becomes unused — `summary.R` is its only caller, delete it |
| `man/rhat.Rd`, `man/rhat.bayesnechurdlefit.Rd` | regenerate |
| `NEWS.md` | user-facing behaviour change |

Tests are low risk: `test-bayesmanec_methods.R:32-36` uses the default and
`rhat_cutoff = 1`, and `manec_example` fails at both 1.05 and 1.01, so no
expectation changes.

### `example2` rewrite, same PR

`example2.Rmd.orig:146-158` exists entirely to contrast a permissive default with
a conservative override, and the rendered output shows it working — `rhat(exp_5)`
all 8 `FALSE`, `rhat(exp_5, rhat_cutoff = 1.01)` gives `ecx4param TRUE`. With the
default at 1.01 the two chunks become **identical** and the prose is wrong.
Rewrite: default is 1.01, cite Vehtari, contrast against the looser 1.05 if a
contrast is still wanted.

Fix the same sentence while it is open: "Here we get a message because none of our
models failed the default Rhat criterion" has it backwards — no message appears
in the rendered output, and `rhat.bayesmanecfit` only messages when **all** models
fail (`R/rhat.R:69-71`).

**Gating:** the `.Rmd.orig` edit lands with the PR but the rendered `.Rmd` stays
stale until the #190 precompile run, so the published vignette shows old prose
against the new default in the interim. Call it out in the PR body.

### `amend()` behaviour — verified, and it is not what the project comment claims

The script comment says `amend()` "errors/warns if asked to drop models that
don't exist". Measured:

| call | result |
|---|---|
| `amend(manec_example, drop = "notamodel")` | two messages, **returns the fit unchanged**. No error |
| `amend(manec_example, drop = c("nec4param", "notamodel"))` | works — drops `nec4param`, returns a `bayesnecfit` |
| `amend(manec_example, drop = c("nec4param", "ecx4param"))` | **error** — "All models removed, nothing to return" |

The mixed case, which is the common one, already works. `amend()` never errors on
an absent name; it silently returns unchanged when *every* named model is absent
(`handle_set()` at `R/helpers.R:154` hits `identical(sort(x), sort(tmp))` and
returns the `"wrong_model_output"` sentinel, which `amend_model_set()` at
`R/amend.R:222` turns into a message and the original object).

So the screening wrapper owns three cases, all of which it needs anyway:

1. **nothing failed** — do not call `amend()`; return unchanged, message that all
   models passed;
2. **some failed** — `intersect()` against `names(x$mod_fits)`, then
   `amend(drop = )`;
3. **all failed** — `amend()` errors; catch it and say something more useful than
   "All models removed, nothing to return".

Part D touches no existing exported function.

### CRAN and fixtures

Examples go in `\dontrun{}` — already the pattern in 22 of 85 `man/*.Rd`,
`rhat()` among them. The #219 vignette is precompiled from `.Rmd.orig` and does
not run at build or check time. **Nothing in Part D adds to CRAN check time.**

Consequence: the tests carry the verification, and **a fixture that does not yet
exist is needed**. `manec_example` exercises case 3 perfectly (both models fail at
any cutoff) but nothing in the package exercises case 2 — a set where some models
pass and some fail — which is the whole point of the feature. Build a small
stored fixture.

## Settled decisions

| | |
|---|---|
| Scope | Part B is in scope; `pp_check` alone does not close the issue |
| Averaged check | built in `ggplot2` directly; no `bayesplot` dependency |
| Grouping | replicate levels of `x` where they exist; **automatic binning with a warning** where they do not. `check_data.R:76` already uses `length(ctl) >= 3` as its usability threshold — reuse it as the trigger |
| #56 | closed into this issue. LOO-PIT, not `DHARMa` |
| Output | both a numeric test and a plot |
| Name | **`check_fit()`** — one function covering both statistics, sitting with `check_chains()` (sampler) and `check_priors()` (priors) |
| Surfacing | a message at the end of `bnec()` **and** a line in `summary()` |
| `dispersion()` | keep both |
| Hurdle | include the combined simulation |

## The combined hurdle simulation

For `bayesnechurdlefit`, simulate the joint response — draw alive/dead from the
survival fit, then a growth value for each survivor — reconstructing the full
observed response including its zeros. This is the only check of the hurdle fit
*as a model of the data the user handed in*, rather than of its two halves
separately.

Draws may be paired row-wise: the two posteriors are independent (the hurdle
likelihood factorises, the components share no parameters), which is the same
argument `hurdle_component_preds()` already rests on in
`R/bayesnechurdlefit-class.R`. Truncate both to the smaller draw count, as that
function does.

**Prototyped and verified.** It is about six lines:

```r
alive <- posterior_predict(sb)                 # bernoulli, every exposed row
grow  <- posterior_predict(gb, newdata = d)    # full data, not the survivors subset
n     <- min(nrow(alive), nrow(grow))
comb  <- alive[seq_len(n), ] * grow[seq_len(n), ]
```

`newdata = d` rather than the growth fit's own data is the point to get right —
the growth component is fitted on survivors only (48 of 90 rows in the test
case), so it must be predicted onto the full exposed set for the product to
align with the observed response.

On the test fit the result is a 400 x 90 matrix matching the observed response,
giving: zero fraction obs 0.467 vs sim 0.483 (ppp 0.605); mean obs 1.903 vs sim
1.747 (ppp 0.252); sd obs 2.301 vs sim 2.270 (ppp 0.44).

The extrapolation concern noted in `hurdle_component_preds()` — growth is
extrapolated past the highest concentration where anything survived — is benign
here, as that function's comment predicts. At the top concentration the
simulated survival probability was 0.167 against 1 survivor of 6 observed,
i.e. exactly 0.167, so the product is carried by the survival term as intended.

## Verified before writing this

Checked against `brms` 2.23.0 source and a live `hurdle_gamma` fit, not assumed:

- **The two-block families need no special handling.** `posterior_predict` for
  `hurdle_gamma`, `zero_inflated_beta`, `zero_inflated_poisson` and
  `zero_inflated_negbinomial` all draw the **full mixture including the point
  mass at zero** (`brms:::posterior_predict_hurdle_gamma` and friends). `yrep`
  is a draw of the response as observed. The reason `disp()` refuses these
  families is a modelling problem — a variance function on a *component's*
  dispersion parameter does not describe a *mixture's* dispersion — and has no
  bearing on checking.
- On a live `hurdle_gamma` `bnec()` fit: `pp_check`, `pp_check(type =
  "stat_grouped", stat = "sd")` and `pp_check(type = "loo_pit_overlay")` all
  return a ggplot. `bf$criteria` holds `loo,waic`, confirming `bnec()` already
  attaches the criterion at `R/add_criteria.R`.
- Zero-fraction statistic on that fit: obs 0.467, simulated 0.495, ppp 0.675.
- Control row on that fit (n = 6): obs mean 4.50 vs sim 5.53 (ppp 0.82); obs sd
  1.53 vs sim 2.58 (ppp 0.82). Both statistics compute cleanly on a two-block
  family with no special-casing.

## Hazards

- **LOO-PIT recomputes `loo` on every call.** `brms:::pp_check.brmsfit` calls
  `do_call(loo, c(pred_args, save_psis = TRUE))` unconditionally in the
  `loo_pit*` path — it ignores the stored criterion, and passing
  `save_psis = TRUE` to `add_criterion()` does not help (it does not retain a
  `psis_object`). On a `bayesmanecfit` that is a fresh `loo` per candidate model
  per call. **Keep LOO-PIT on demand only.** The `bnec()` end-of-fit message
  must use the posterior-predictive statistic, which is cheap.
- **Posterior predictive p-values are conservative** — the data are used twice,
  so they under-detect. Document `ppp` as a flag, not a test; LOO-PIT is the
  calibrated companion, which is a further reason to ship them together.
- **Control power is low when control replication is low.** With n = 6 at the
  control the `ppp` has very little power. This is the same constraint that
  drives the binning decision, and the message should not imply more confidence
  than the design supports.

  This was measured twice, on two independently fitted parameterisations of the
  same simulated data — a single `hurdle_gamma` `bnec()` fit and a
  `bnec_hurdle()` pair. Both put the simulated control mean at 5.5-5.6 against
  an observed 4.50 and a true value of 4.77, so roughly an 18% overshoot that
  reproduces across fits and is a property of the `ecxexp` shape rather than
  noise. Neither `ppp` came near flagging (both ~0.82). **This is the case for
  thresholding the automatic message on the ratio rather than on the `ppp`.**
- **Do not cache the check on the fitted object.** #180 / PR #205 went the other
  way — `pred_vals$posterior` was removed precisely to stop caching prediction
  matrices. Recompute in `summary()`.
- **Depends on PR #217.** Without it the model-averaged `yrep` is redrawn on
  every call and neither the plot nor the table is reproducible.
- **`manec_example` cannot be used to demonstrate screening.** Both its models
  return `failed = TRUE` at `rhat_cutoff = 1.05`, and `b_bot_Intercept` has an
  `ess_bulk` of 10 out of 100 draws. It is a deliberately tiny stored object, so
  this is not a bug, but a screen run on it drops everything and leaves no
  averaged set. Part D tests and examples need a different fixture, and so does
  #219.
- **Vignettes are gated on #190.** Anything added to `example1.Rmd.orig` will
  not render until the next full `precompile.R` run. Do not run it.

## Definition of done

Per `00_protocol.md`, plus: no new package dependency in `DESCRIPTION`
(Part D needs none; the rule itself was downgraded to stop-and-ask on
2026-08-21), and the
`bnec()` message must be one line and must not fire on a fit that is fine.
