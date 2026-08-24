# Defect: route-B variance functions require an identity link, and do not check

Found 2026-08-12. **An earlier version of this note claimed `bnec()` never
forces an identity link and that route B had therefore never worked outside
`gaussian`. That was wrong on both counts** — it came from my own test harness
passing `family = Beta()` and `family = Gamma()` explicitly, which takes each
family's *default* link rather than the one `bayesnec` would have chosen. The
corrected finding is narrower, but the silent-failure mode it exposes is worse.

## What actually happens

`bnec()` forces `link = "identity"` when it **auto-selects** the family. A
manually supplied family keeps whatever link the user gave it:

```r
bnec(fvfm ~ crf(logc, "ecx4param"), data = d)                      # beta / identity
bnec(fvfm ~ crf(logc, "ecx4param"), data = d, family = Beta())     # beta / logit
bnec(fvfm ~ crf(logc, "ecx4param"), data = d,
     family = Beta(link = "identity"))                             # beta / identity
```

That is by design — manual specification is the user's choice, with identity the
recommendation.

`make_disp_block()` substitutes the model's curve expression into the variance
function on the assumption that it is the mean, `mu`:

```
phi ~ c0 + c1 * (log(<curve>) - <ref>)
```

The curve expression is the linear predictor on the **link scale**. The
assumption therefore holds under an identity link and fails under any other.
Nothing checks this, and nothing in the documentation states the requirement.

## Consequences by link

| link | families | behaviour of route B |
|---|---|---|
| identity | any, and every auto-selected family | **correct** |
| logit | `Beta()`, `beta_binomial()` as supplied by the user | `log()` of a negative predictor — NaN, no chains start, loud failure |
| inverse | `Gamma()` as supplied by the user | `log(1/mu) = -log(mu)` — **runs, converges, returns the slope with the wrong sign** |
| log | `negbinomial()` as supplied by the user | `log(log(mu))` — meaningless, NaN below `mu = 1` |

The inverse case is the dangerous one, because it is silent. Same data, same
model, same variance function, only the link differs:

```
Gamma link=identity   RAN   c1 = +0.801 [ 0.346,  1.281]   rhat 1.026
Gamma link=inverse    RAN   c1 = -0.544 [-0.917, -0.143]   rhat 1.017
```

Both converge and both credible intervals exclude zero, in opposite directions.
A user who supplies `family = Gamma()` gets a confident answer of the wrong
sign with no indication that anything is amiss.

## What does work

`"power"` and `"twosided"` both run correctly on `Beta` under an identity link:

```
power     link=identity  OK   c1 = 0.378 [0.082, 0.652]   rhat 1.017
twosided  link=identity  OK   c1 = 0.881 [0.158, 1.577]   rhat 1.016
```

So the earlier claim that `"twosided"` had never worked was also wrong. It
works; it had simply never been exercised, because my harness supplied the
wrong link.

## What this invalidates

* **The Beta results in `disp_alternative_datasets.md`** — all 7 herbicides, 3
  coral colour and 3 T7 PAM datasets were run with `family = Beta()`, i.e.
  logit, so their route-B arms failed for this reason rather than for want of a
  signal. Their route-A arms are unaffected, since route A is an ordinary
  distributional formula and does not reference the curve.
* **The `alga` density-scale `Gamma` fits** in `disp_validation.md`, run with
  `family = Gamma()` and therefore inverse. The annotation added there stands,
  but the reason should be stated as a link mismatch caused by explicit family
  specification, not as an unconditional package defect.
* **Nothing `gaussian`.** Identity is the default and the only link, so the
  `alga` `sgr` results, the censoring-at-zero comparison, the `nassarius`
  gaussian results and both simulation regression tests are unaffected.

## Fix

Validate the link in `check_disp_spec()` and refuse a non-identity link for
route B, naming the family and pointing at `family = Beta(link = "identity")`.
The message should say that a variance function on the fitted mean is only
meaningful when the mean is modelled on its natural scale.

Wrapping the curve in the inverse-link function instead would also work and
would let route B run under any link, but it changes what the fitted `c0` and
`c1` mean relative to the documented forms, and identity is the recommendation
anyway. Refusing is the smaller and clearer change.

Route A needs no check: it is an ordinary distributional formula on the
predictor and never references the curve.

**The accompanying test must exercise a non-gaussian family through route B
under both links** — one asserting the refusal, one asserting a correct fit
under identity. Gaussian-only tests are why this went unnoticed.
