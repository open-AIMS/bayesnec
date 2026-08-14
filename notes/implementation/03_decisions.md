# Settled decisions

Answers given by the author on 2026-08-14, before the unattended run. A session
must **implement these as written** rather than re-open them. Where an entry
says "report", write the finding in the PR body and change nothing.

---

## D1 — Branching and review

Branch per issue from `dev`, named `issue-<n>-<slug>`. PR targets `dev`.
**Sessions do not merge.** PRs stack up for review.

## D2 — #180, the cached posterior matrix

**Drop the cache and compute on demand.** Remove `pred_vals$posterior`;
accessors recompute from the `brmsfit` via `posterior_epred()`. Expected
31.8 MB → roughly 1.5 MB on the measured example.

Accessors must **fall back gracefully for objects saved before the change** — an
old object still carrying the cache must keep working, and a new object without
it must not error. Test both.

## D3 — #170, `check_models()` versus `?models`

**Align the documentation to the code, and report only.** Add a test asserting
the two agree so they cannot drift again. **Do not relax the restriction**, even
if investigation suggests it is unnecessary — write that up as a recommendation
in the PR body instead.

## D4 — #104, zero-inflated counts

**Joint path only.** Add `zero_inflated_poisson` and
`zero_inflated_negbinomial` to the normal `bnec()` family path.
`bnec_hurdle()` must **refuse** them, with an error explaining why and pointing
at the hurdle families instead.

The reasoning matters and should appear in the documentation. #183 treated
`zero_inflated_beta` as identical to `hurdle_gamma`, and that was correct
*because neither Gamma nor Beta can emit a zero* — so zero-inflation collapses
to a hurdle and `brms` generates the hurdle form with no `log_sum_exp`. Poisson
and negbinomial **can** emit zeros, so the equivalence fails: a zero-inflated
count model is a genuine mixture. The existing two-block machinery does not
carry over, and pretending it does would give users a different model from the
one they asked for.

## D5 — #120, replacing `all_models`

**Two orthogonal arguments**: `model =` names one or more models, `average =`
(logical) controls whether the model-averaged outcome is shown. Applies to
`predict`, `plot` and `autoplot` for `bayesmanecfit`.

**Deprecate `all_models` for one cycle**: it keeps working, warns once, and maps
onto the new arguments. Removal is a later release. No existing user script
should break on upgrade.

## D6 — #148, diagnostics

**Split it, on this principle:** sensitivity to the *estimator's* own choices
belongs to toxval; sensitivity to what the *fit* supplies belongs here.

- **toxval** — how *NSEC* responds to `sig_val`, to the reference definition and
  to resolution. Not Bayesian-specific: it applies equally to `nsec.drc` and
  `nsec.brmsfit`, which is the point of testing it there.
- **bayesnec** — diagnostics on whether the fit supports a stable control
  estimate: posterior spread at the control, prior sensitivity, and the
  dispersion assumption.

The dispersion half already has a tool. On the `example1` simulation, `nsec`
moved from 0.93 [0.43, 1.32] to 1.09 [0.50, 1.53] under `disp("power")` while
`ecx` tightened — the estimator was unchanged, the fit was not. `disp()` is
therefore a *NSEC*-stability diagnostic, and that is worth saying explicitly.

**#148 stays open** as the bayesnec half, rescoped. It is **not** in the
unattended queue — it needs a specific list of diagnostics first.

## D7 — Sessions

**One session, sequential**, working `01_work_queue.md` top to bottom in a
single worktree.

## D8 — Repository scope

**The unattended run is `bayesnec` only.** toxval is a separate session with a
separate queue, and it is *not* runnable concurrently:

`bayesnec` cannot declare `Imports: toxval` until a toxval carrying the new API
is installable, or bayesnec's R CMD check fails on a function that does not
exist yet. toxval's Phase 1 must land and be installable first; in the interim
bayesnec would need a `Remotes:` entry. That ordering is a hard constraint, not
a preference.

## D9 — #141, `get_priors()`

**Both entry points, one function.**

- given a **fit**, return the priors that fit actually used, including any the
  user overrode — so `bnec(..., prior = get_priors(fit))` reproduces the model;
- given a **formula and data**, return the priors `bayesnec` would generate,
  without fitting anything — so a user can inspect and edit them before their
  first run.

Return a `brmsprior` for a single model and a named list of `brmsprior` objects
for a model set, in both cases directly usable as the `prior =` argument.
Document that the two entry points answer different questions and can disagree
once a user has overridden a prior.
