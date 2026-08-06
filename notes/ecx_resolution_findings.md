# How `resolution` affects nsec, ec10 and ec50

Measurements for [#39](https://github.com/open-AIMS/bayesnec/issues/39)
(*Improve speed and precision of ecx and nsec functions using uniroot.all*),
with a bearing on [#166](https://github.com/open-AIMS/bayesnec/issues/166).
Nothing here has been implemented; this is evidence for that issue.

## The short version

The zero-crossing treatment reached `nsec` but not `ecx`.

* `nsec_fct()` (`R/nsec.R:258`) locates the crossing with
  `modelbased::zero_crossings()` and linearly interpolates between the
  bracketing grid points. NSEC is therefore insensitive to `resolution`.
* `ecx_x_absolute()`, `ecx_x_relative()` and `ecx_x_direct()`
  (`R/ecx.R:321` onward) still take `x_vec[min_abs(y - ecx_y)]` — the nearest
  point on the grid, no interpolation. ECx is therefore resolution-dependent
  and does not converge cleanly.

## Measured

`manec_example`, models pulled out individually so no model-averaging sampling
noise is involved. Percentage deviation from that metric's own
`resolution = 2000`:

```
ecx4param            nsec    ec10    ec50
res=25              -0.35   -6.21   -2.49
res=50              -0.05   -1.72   -0.54
res=100              0.00    0.46    0.41
res=200             -0.01   -0.03   -0.08
res=500              0.00    0.30    0.00
res=1000             0.00    0.05    0.00

nec4param            nsec    ec10    ec50
res=25              -1.61   -0.56   -2.81
res=50              -0.34    1.78   -0.87
res=100             -0.07    0.78    0.08
res=200              0.00    0.28   -0.42
res=500              0.00   -0.01    0.05
res=1000             0.00    0.00   -0.05
```

NSEC is converged by ~100. ECx never settles: it is 6% out at 25, 1.7% at 50,
and still jitters ±0.3% at 500–1000, because each posterior draw is snapped to
a grid point and the median of snapped values wanders non-monotonically.

## A prototype confirms the cause

Applying the same `zero_crossings` + interpolation to ECx:

```
ecx4param, EC10   nearest   interpolated
res=25             -6.21         -0.53
res=50             -1.72         -0.10
res=100             0.46         -0.01
res=200            -0.03          0.00
res=500             0.30          0.00
```

Interpolated ECx converges by 200 and is within 0.5% at 25.

**At the default `resolution = 1000` the two methods agree to 0.02%** (EC10
1.0260 vs 1.0262; EC50 1.6679 vs 1.6680). So this is not a behaviour change for
anyone on defaults — it removes the low-resolution error and would let
`resolution` drop by an order of magnitude, which matters because `resolution`
is the dominant term in the memory these functions use.

## Caveats for whoever implements #39

* `min(zero_crossings(...))` takes the *first* crossing. For a hormetic,
  non-monotone curve that can be on the rising limb. #39 already notes
  `uniroot.all` and that the second root is usually the wanted one. `nsec_fct`
  carries the same exposure today.
* Relatedly, the hormesis `modify_posterior()` block in `ecx.bayesnecfit()` is
  commented out (`R/ecx.R:165-169`), so hormesis handling in `ecx` is currently
  inert whatever is done about interpolation.
* All three `ecx_x_*` variants need the same treatment, and the no-crossing
  fallback must keep returning `max(x_vec)`.
* **#166 bounds what interpolation can achieve.** `zero_crossings()` finds
  roots by sampling a linear interpolant on a fixed internal grid of 100
  points, so a crossing that produces no sign change on that grid is missed
  regardless of how fine the caller's `resolution` is. It also means NSEC
  precision saturates near 100 and gains nothing above it — visible in the
  table above. `uniroot.all` on the fitted function, as #39 proposes, would
  avoid both the grid and this cap.

## Consequence for `vignette("example6")`

Chosen on this evidence, given the current implementation:

* reporting calls (`summary`, the estimates table) use `resolution = 500` —
  ECx within ~0.3%, comfortably inside the two significant figures reported;
* the `nassarius` fits store predictions at `resolution = 200`, since the only
  reported quantity that grid feeds is the interpolated NSEC, whose precision
  saturates below it. With 48 fits held simultaneously that storage is the
  dominant memory cost.
