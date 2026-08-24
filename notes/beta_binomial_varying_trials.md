# `beta_binomial` failed to initialise under brms 2.22.0 — resolved

Found 2026-08-12 while re-knitting `example1` to add a dispersion section.
Unrelated to `disp()`; it reproduced on plain `brms` with no `bayesnec`
involved. **Fixed upstream in brms 2.23.0.**

## Root cause

brms 2.22.0 generated the likelihood as

```stan
target += beta_binomial_lpmf(Y[n] | trials, mu[n] * phi, (1 - mu[n]) * phi);
```

passing the whole `array[N] int trials` where it should pass `trials[n]`. Stan
took the vectorised signature and evaluated the scalar `Y[n]` against *every*
element of `trials`, summing the result.

* **Varying `trials`** — `beta_binomial_lpmf` is invalid when `y > N`, and
  broadcasting compares each `Y[n]` against the *smallest* trial count, so any
  observation above `min(trials)` made a term `-Inf` at every parameter value.
  Hence `Log probability evaluates to log(0)` at every initial value.
* **Constant `trials`** — no term was ever invalid, so the model sampled
  happily on a likelihood inflated by exactly `N`. Measured at a fixed parameter
  vector: correct log-likelihood -239.428, generated -13407.956, ratio 56.0000
  with N = 56. Posteriors were overconfident by roughly `sqrt(N)`.

The second was the more damaging half, because nothing signalled it.

Backend-independent: `rstan` (StanHeaders 2.32.10) and `cmdstanr`
(CmdStan 2.39.0) rejected every initial value identically, which is what ruled
out a sampler regression and pointed at the generated code.

Scope was `beta_binomial` alone. `binomial` generates a fully vectorised
`binomial_logit_lpmf(Y | trials, mu)` and `zero_inflated_binomial` indexes both
arguments; `beta_binomial` was the only family indexing the response but not
`trials`. Existing `binomial` results were therefore unaffected.

## Resolution

brms 2.23.0 generates

```stan
target += beta_binomial_lpmf(Y | trials, mu .* phi, (1 - mu) .* phi);
```

Verified after updating: both cases fit, and the constant-`trials` posterior SD
for `top` is 0.024 against 0.002 under 2.22.0, matching the 0.026 obtained by
hand-patching 2.22.0.

## Consequence for this package

`DESCRIPTION` lists `brms` in `Depends` with no version constraint, so a user on
2.22.0 still hits this. **`brms (>= 2.23.0)` is proposed but not yet applied** —
it was left out of the dispersion branch as unrelated to that work.

Any `beta_binomial` fit produced under 2.22.0 with constant `trials` has
understated uncertainty and should be refitted. No published `bayesnec` output
is affected: `example1`'s `binom_data` has varying `trials` (22–191), so under
2.22.0 it failed outright rather than fitting wrongly, and the committed
vignette predates the defect.
