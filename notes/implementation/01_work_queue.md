# Work queue

Ordered. Work top to bottom. Each entry states the scope, what "done" means, and
the hazard most likely to derail it. Read `00_protocol.md` first.

Ordering rationale: independent, well-specified fixes first, so that PRs stack
up early; the two entries that touch shared post-processing code (#93, #180) are
adjacent so that any conflict between them is obvious; investigations last,
because they may end in a report rather than a patch.

---

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

## 3. #160 — *NEC* mis-plotted when a function is called for `x`

**Scope.** With `crf(log(x), ...)`, `autoplot()` and the base plot draw the
*NEC* line in the wrong place. Reprex in the issue.

**Done when** the *NEC* line lands on the curve's break point for both a raw and
a transformed predictor, with and without `xform`.

**Hazard.** This is adjacent to #196 (`ecx`/`nsec` back-transformation), which
is **out of scope** and heading to toxval. Fix the plotting only. If the cause
turns out to be in a shared post-processing helper that `ecx`/`nsec` also use,
stop and report — that affects the migration.

---

## 4. #93 — correct estimates for shifts applied by `check_data()`

**Scope.** `check_data()` shifts `x` or `y` away from 0/1 in some cases.
Predictions and toxicity estimates should be returned on the user's original
scale, not the shifted one.

**Done when** a fit on data that triggers a shift returns predictions and *NEC*
on the original scale, demonstrated by a test comparing against the same data
pre-shifted by hand.

**Hazard.** *NEC* is a parameter and `ecx`/`nsec` are derived — the correction
does not apply identically to all three, and **`ecx`/`nsec` are out of scope**.
Confine the fix to predictions and *NEC*. Record in the PR what the shift
implies for the derived estimates so it can be carried into the toxval work.

---

## 5. #180 — `bnec()` caches a posterior matrix ~25x the fit

**Scope.** `expand_nec()` stores `pred_vals$posterior`, sized
`n_draws x resolution`, dominating object size (31.8 MB against a 1.2 MB
`brmsfit`). **See decision D2 in `03_decisions.md` for which approach to take.**

**Done when** a `nec3param` fit is materially smaller, every accessor that used
the cache still works, and a test asserts the object stays under a sane size.

**Hazard.** The cache is load-bearing for other methods and for objects saved by
users. Whatever changes, `predict()`, `autoplot()` and the `bayesmanecfit`
model-averaging path must all still work — including on an object saved before
the change if that is the chosen approach.

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

## 7. #170 — `check_models()` and `?models` disagree

**Scope.** The documentation is stricter than the code about which
slope-bearing equations are excluded for 0,1-bounded identity families.
**See decision D3.** The default is the conservative one: make them agree, and
report separately on whether the restriction is needed at all.

**Done when** `?models` and `check_models()` state the same thing, with a test
asserting agreement so they cannot drift again.

**Hazard.** Do not relax a modelling restriction on your own judgement. If
investigation suggests the restriction is unnecessary, write that up in the PR
body as a recommendation and leave the behaviour alone.

---

## 8. #161 — bug in post-processing of a case study dataset

**Scope.** Investigation. The issue carries the dataset inline but not a crisp
statement of the defect. Reproduce first, characterise what is wrong, and only
then decide whether it is fixable here.

**Done when** either a fix with a regression test, or — if the cause sits in
`ecx`/`nsec` — a clear write-up posted as an issue comment and the entry closed
out as out of scope. **Report either way.**

**Hazard.** Good chance this is the same root cause as #195 or #196 and
therefore belongs to toxval. Recognising that quickly is a success, not a
failure.

---

## 9. #133 — report models that failed to fit

**Scope.** Return the models that failed, with the priors and initial values
used, so a failure can be diagnosed without a re-run. Attach to the
`bayesmanecfit` and surface in `summary()`.

**Done when** a deliberately failing model in a set appears in the returned
object with its priors and inits, and a test asserts it.

**Hazard.** Keep it small — this is an accessor and a print method, not a
diagnostics framework. #148 is the larger diagnostics issue and is **not** in
this queue.

---

## 10. #120 — replace the `all_models` argument

**Scope.** On `predict`, `plot` and `autoplot` for `bayesmanecfit`, replace
`all_models` with two orthogonal arguments: `model =` naming one or more models,
and `average =` (logical) controlling the model-averaged outcome. **See D5** —
the interface and the deprecation are both settled, implement as written.

**Done when** the new arguments work across all three methods, `all_models`
still works but warns once and maps onto them, and tests cover the old and new
spellings plus the deprecation warning.

**Hazard.** This is a user-visible interface change. The deprecation shim is the
part most likely to be skipped under time pressure, and it is the part that
stops existing scripts breaking. Do not remove `all_models`.

---

## 11. #104 — zero-inflated Poisson and negative binomial

**Scope.** Add `zero_inflated_poisson` and `zero_inflated_negbinomial` to the
`bnec()` family path. **`bnec_hurdle()` must refuse them** with an error
explaining why. **See D4** — the reasoning is settled and belongs in the
documentation.

**Done when** both families fit through `bnec()`, `bnec_hurdle()` errors
informatively, and `?bnec` states why zero-inflated counts are not routed
through the two-block machinery when `zero_inflated_beta` is.

**Hazard.** The temptation is to reuse the `zero_inflated_beta` path, since the
name matches. It is not the same model. Beta and Gamma cannot emit zeros, so
zero-inflation collapses to a hurdle there; Poisson and negbinomial can, so it
does not. Reusing that path would silently give users a different model.

---

## 12. #141 — `get_priors()` round trip

**Scope.** Two entry points on one function: given a fit, return the priors it
used; given a formula and data, return the priors `bayesnec` would generate.
**See D9.** Return a `brmsprior`, or a named list of them for a model set,
directly usable as `prior =`.

**Done when** `bnec(..., prior = get_priors(fit))` reproduces the same model,
the formula-and-data form works without fitting, and a test covers both — and
covers the case where a user-supplied prior makes the two disagree.

**Hazard.** The round trip is the whole point. A returned object that looks
right but is not accepted by `bnec(prior = )` fails the issue, so test the
round trip rather than the shape of the return value.

---

## Not in this queue

Everything else open is either out of scope or not specified well enough to
implement unattended. See `02_deferred.md` for the reasoning, which is worth
reading before adding anything here.

**#148 has been rescoped, not queued.** Its bayesnec half is diagnostics on
whether a fit supports a stable control estimate; the *NSEC*-sensitivity half
goes to toxval. See D6. It still needs a specific list of diagnostics before it
can be worked.
