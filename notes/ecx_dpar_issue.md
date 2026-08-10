# Component selection on hurdle fits: bug record and correct usage

**Status:** fixed on branch `hurdle-vignette` (see NEWS.md). Kept as a record
because a wrong workaround was circulating and anyone who adopted it needs to
know.

---

## What was wrong

`ecx()` on a **model-averaged joint hurdle fit** (`bayesmanecfit` from
`bnec(family = "hurdle_gamma")`) ignored the `dpar` argument and returned the
combined endpoint instead of the requested parameter block. No error, no
warning — a plausible-looking toxicity estimate answering a different question.

`ecx.bayesmanecfit()` dispatched its per-model calls through
`sapply(to_iter, sample_ecx, object, ecx_val, resolution, ...)`, matching
arguments positionally against an inner function that enumerated every
parameter it forwarded. Anything not in that list — `dpar` among them — was
dropped before reaching `ecx.bayesnecfit()`. `nsec.bayesmanecfit()` had the
identical structure.

Introduced by the joint hurdle route (PR #172): `dpar` was added to
`ecx.bayesnecfit()` without a corresponding change to the model-averaged
method. `sample_ecx` itself predates the hurdle work (`727d6700`).

Measured, EC50 on simulated two-block data with growth threshold 1.0 and
survival threshold 2.0:

```
single joint fit (bayesnecfit)      model-averaged joint fit (bayesmanecfit)
default      2.091                  default      2.091
dpar = "mu"  2.091                  dpar = "mu"  2.091
dpar = "hu"  3.023  <- correct      dpar = "hu"  2.091  <- combined, wrong
```

## The workaround that was circulating does not work

A hand-rolled weighted sampler (`ma_component_ecx()`) was proposed, passing
`which = which` to `ecx()` on each pulled-out `bayesnecfit`. Component
selection on a joint fit is `dpar`, never `which`; `ecx.bayesnecfit()` read
only `dpar`, so `which` was discarded there too. Every per-model posterior was
still the combined curve and the result was the combined estimate with extra
steps. **Anyone who adopted it should replace it with a plain `ecx(fit, dpar =
...)` call on the fixed version.**

## Correct usage

The two implementations name the component differently, and the two arguments
are not interchangeable:

| object | produced by | component argument |
|---|---|---|
| `bayesnechurdlefit` | `bnec_hurdle()` | `which = "growth" / "survival" / "combined"` |
| `bayesnecfit` / `bayesmanecfit` | `bnec(family = "hurdle_gamma")` | `dpar = "mu" / "hu"` (`"zi"` for zero-inflated) |

The factorised route was never affected — `which` worked correctly there,
including with model-averaged components, because `ecx.bayesnechurdlefit()`
goes through `hurdle_component_preds()` and `posterior_epred.bayesmanecfit()`
does the weighting.

## What was changed

- `ecx.bayesmanecfit()` and `nsec.bayesmanecfit()` rewritten to dispatch
  through a closure rather than positional `sapply()`, so `dpar` reaches the
  per-model calls. The positional form was the root cause and would have
  silently dropped the next argument added too.
- `nsec.bayesnecfit()` gained `dpar`, mirroring `ecx.bayesnecfit()` — including
  the `is_hurdle_family()` guard, `match.arg()` against the family's block
  name, and the `1 - p` inversion so the NSEC is read off a declining survival
  curve. It previously had no block selection at all.
- `dpar` is now a formal argument of all four methods and documented in `?ecx`
  and `?nsec`, where it had never appeared.
- Supplying the wrong component argument is an error
  (`check_component_arg()`), not a silent no-op. `nec()` additionally rejects
  `dpar` (`check_nec_no_dpar()`), since what it returns for a joint fit is the
  combined threshold and there is no block selection to offer.
- Tests in `tests/testthat/test-component-args.R`: guards at unit level, `dpar`
  present as a formal, and a `skip_on_cran()` regression fitting a real
  two-model joint hurdle model and asserting `dpar = "hu"` differs from the
  combined estimate.

## Known gap, not addressed

`nec()` has no block selection for a joint fit. It returns the combined
threshold, the per-draw minimum of the two blocks. The block-specific
posteriors exist as `fit$hurdle$mu_ne_posterior` and `hu_ne_posterior` but are
not exposed, and on a `bayesmanecfit` exposing them would need the weighted
mixture treatment plus a decision about what the "block NEC" means when that
block carries an `ecx`-type equation (it would be an NSEC off the block curve,
which is not currently stored). Passing `dpar` to `nec()` now errors rather
than returning the combined value silently, so the gap is visible rather than
misleading. Worth an issue of its own if the component threshold is wanted.

## Not affected

`vignette("example6")`. Every `which =` call in it is on a
`bayesnechurdlefit`; the one `dpar =` call is on a single-model `bayesnecfit`.
The broken combination does not occur, so the fix required no re-render.
