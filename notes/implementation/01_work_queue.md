# Work queue

Read `00_protocol.md` first, then `03_decisions.md`.

**Tier 1 is the unattended run.** Work it top to bottom. Every entry has been
checked against the files the toxval migration will move, and none of them
touch those files.

**Tier 2 is deferred** until after the toxval untangle has landed. Do not start
it. It is recorded here so the reasoning is not lost.

Ordering within Tier 1: small, self-contained fixes first so PRs stack up early;
the two investigations in the middle, where a report is an acceptable outcome;
the largest change last.

---

# Tier 1 — run now

## 1. #176 — `amend()` cannot add models to a single-model `bayesnecfit`

**Scope.** Add an `amend.bayesnecfit` method. The natural implementation
promotes the single fit to a `bayesmanecfit` and delegates to the existing
method, so the two share one code path rather than growing a parallel one.

**Done when** `amend(pull_out(manec_example, "nec4param"), add = "ecxexp")`
returns a `bayesmanecfit` containing both models, and `?amend` no longer says
the object must be a `bayesmanecfit`.

**Hazard.** `amend()` has several arguments (`drop`, `add`, `loo_controls`,
`priors`) whose meaning for a one-model object is not all obvious — dropping the
only model should error clearly rather than return an empty object.

---

## 2. #188 — `bnec_hurdle()` rejects all aterms

**Scope.** `bnec_hurdle()` refuses any aterm on the response, which blocks the
censored-hurdle combination #181 was raised for. Allow `cens()` specifically.
Aterms that are meaningless for a hurdle, or that conflict with the two-block
construction, should keep erroring with a message naming which aterm and why.

**Done when** the reprex in the issue fits, and a test asserts both the accepted
and the rejected cases.

**Hazard.** The censoring declaration has to reach the correct block. A
left-censored *survivor* is an observation of the growth component; it is not a
structural zero and must not be routed to the hurdle block. Getting this
backwards produces a model that samples cleanly and is wrong — verify on
simulated data where the answer is known.

---

## 3. #170 — `check_models()` and `?models` disagree

**Scope.** The documentation is stricter than the code about which
slope-bearing equations are excluded for 0,1-bounded identity families.
**D3 applies:** make them agree, and report only.

**Done when** `?models` and `check_models()` state the same thing, with a test
asserting agreement so they cannot drift again.

**Hazard.** Do not relax a modelling restriction on your own judgement. If
investigation suggests the restriction is unnecessary, write that up in the PR
body as a recommendation and leave the behaviour alone.

---

## 4. #133 — report models that failed to fit

**Scope.** Return the models that failed, with the priors and initial values
used, so a failure can be diagnosed without a re-run. Attach to the
`bayesmanecfit` and surface in `summary()`.

**Done when** a deliberately failing model in a set appears in the returned
object with its priors and inits, and a test asserts it.

**Hazard.** Keep it small — this is an accessor and a print method, not a
diagnostics framework. #148 is the larger diagnostics issue and is deliberately
not queued.

---

## 5. #141 — `get_priors()` round trip

**Scope.** Two entry points on one function, per **D9**: given a fit, return the
priors it used; given a formula and data, return the priors `bayesnec` would
generate. Return a `brmsprior`, or a named list of them for a model set,
directly usable as `prior =`.

**Done when** `bnec(..., prior = get_priors(fit))` reproduces the same model,
the formula-and-data form works without fitting, and a test covers both — and
covers the case where a user-supplied prior makes the two disagree.

**Hazard.** The round trip is the whole point. A returned object that looks
right but is not accepted by `bnec(prior = )` fails the issue, so test the round
trip rather than the shape of the return value.

---

## 6. #177 — `nechormepwr` fails to initialise on bounded responses

**Scope.** Investigate first. The failure is specific to the combination of
`nechormepwr` with a bounded response, including the `hu` block of
`hurdle_gamma`. A `model = "zero_bounded"` call therefore silently averages over
9 equations rather than 11.

**Done when** either the initial-value generation is fixed for this combination,
or — if the equation genuinely cannot be fitted on a bounded response — the
model is excluded from the relevant model sets *by name and with a message*, so
the set size is honest rather than silently short.

**Hazard.** "Silently drops to 9 of 11" is the real defect. A fix that makes the
model fit but converge badly is worse than an explicit exclusion. Check R-hat
and ESS, not just that it started.

---

## 7. #104 — zero-inflated Poisson and negative binomial

**Scope.** Add `zero_inflated_poisson` and `zero_inflated_negbinomial` to the
`bnec()` family path. **`bnec_hurdle()` must refuse them** with an error
explaining why. **D4 applies** — the reasoning is settled and belongs in the
documentation.

**Done when** both families fit through `bnec()`, `bnec_hurdle()` errors
informatively, and `?bnec` states why zero-inflated counts are not routed
through the two-block machinery when `zero_inflated_beta` is.

**Hazard.** The temptation is to reuse the `zero_inflated_beta` path, since the
name matches. It is not the same model. Beta and Gamma cannot emit zeros, so
zero-inflation collapses to a hurdle there; Poisson and negbinomial can, so it
does not. Reusing that path would silently give users a different model.

---

## 8. #180 — `bnec()` caches a posterior matrix ~25x the fit

**Scope.** `expand_nec()` stores `pred_vals$posterior`, sized
`n_draws x resolution`, dominating object size (31.8 MB against a 1.2 MB
`brmsfit`). **D2 applies: drop the cache and compute on demand.**

**The cache has exactly one reader in the package.** It is written at
`R/expand_classes.R:78`

```r
pred_vals <- list(data = pred_data, posterior = pred_posterior)
```

and read at `R/helpers.R:105`

```r
mod_fits[[index]]$pred_vals$posterior[sample(x, size), ]
```

which is the **model-averaging path** — drawing from each model's posterior in
proportion to its weight. Nothing else reads it. `predict()` does not;
`plot()`/`autoplot()` use `pred_vals$data`, the small summary, which **stays**.

**Done when** a `nec3param` fit is materially smaller, model averaging still
produces the same weighted draws, and a test asserts both the size and that an
object saved *with* the cache still works.

**Hazard, and the real design question.** Removing the cache means the
model-averaging path must recompute `posterior_epred()` per model on demand —
for a 23-model set that is 23 recomputations where there were none. Measure it.
If model averaging becomes unacceptably slow, the right answer may be to compute
once and hold the draws in memory for the duration of the call rather than
storing them in the object. Either is consistent with D2; storing the matrix on
the object again is not. **Say in the PR which you chose and what it cost.**

---

# Tier 2 — deferred until after the toxval untangle

Not to be started. Each of these edits code that the migration moves, so doing
them now means conflicts or wasted work. Revisit once `ecx`/`nsec` have left
bayesnec and the `predict` methods have settled.

| | why it waits |
|---|---|
| #120 | changes `predict`/`plot`/`autoplot` for `bayesmanecfit`; toxval currently registers `predict.bayesnecfit` and `predict.bayesmanecfit` on the same classes. Changing their signatures mid-move invites a collision. Decisions D5 are settled and still stand. |
| #93 | the `check_data()` shift correction logically applies to `ecx`/`nsec` as well as to predictions and *NEC*. Doing half now strands the other half in code that is moving. |
| #160 | *NEC* mis-plotted when a function is called for `x`. Plotting itself is not moving, but the cause is likely shared post-processing of the transformed predictor — the same territory as #196. |
| #161 | post-processing bug on a case study dataset. Good chance it *is* #195 or #196, in which case it belongs to toxval outright. Worth reproducing when the boundary is stable, not before. |

---

# Out of scope entirely

`ecx()`, `nsec()`, `ecnsec()`, `zero_crossings()` — #39, #44, #166, #195, #196.
See `02_deferred.md`. #193 belongs to another session.

**#190 (full `precompile.R`) runs last**, after Tier 1 merges — #180 changes what
every fit stores, so any rebuild before it lands is immediately stale.
