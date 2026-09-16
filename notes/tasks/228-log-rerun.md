# Re-running example8 on a log axis

What the source now asks for, what has to be refitted, and which claims in the
vignette are not yet verified against a render.

## The change

`vignettes/example8.Rmd.orig` draws every figure on a log concentration axis
and fits the two coral endpoints on `crf(log(diuron_adj), ...)`. Six of the
nine fit calls were already on `crf(log(...))` and are unchanged. The
`lum31` and `herbicide` datasets contain no zero concentration; `coral_colour`
and `coral_pam` do, and the vignette substitutes 0.1 µg/L for it.

## The refit

The fit store is keyed on the normalised call and a digest of the data, so a
changed call changes the key and the render stops on it rather than loading the
old fit. Three calls changed:

| chunk | call | units |
|---|---|---|
| `fit-pam` | `yield ~ crf(log(diuron_adj), "all") + ogl(chamber)` | 15 |
| `fit-par-count` | `proportion ~ crf(log(diuron_adj), c("ecxll3", "ecxwb1p3")) + ogl(chamber)` | 2 |
| `fit-par-count` | `proportion ~ crf(log(diuron_adj), c("nec4param", "ecx4param")) + ogl(chamber)` | 2 |

`fit_pam` drops from 18 equations to 15. Measured on this branch with
`check_models()`: a negative predictor removes `necsigm` and `ecxsigm`, which
raise the predictor to a fractional power, and `ecxhormebc5`, which cannot be
reliably initialised where an identity-linked `Beta` mean must stay positive.
`log(diuron_adj)` runs from -2.30 to 3.37.

The other six calls keep their keys, so the existing store answers them. The
array re-run is 19 units rather than 189.

## Claims awaiting a render

Each of these was written without a fitted object to read, and the render is
where they are settled. Nothing below asserts a number that has not been
measured; the work is to add the numbers once they exist.

1. **Section 6, the parameter-count contrast.** The passage now states the
   structure --- that R-hat and divergent transitions answer different
   questions, and that the four-parameter band widens over the unsampled decade
   --- without the counts. The previous render gave 0 divergences for both
   three-parameter equations, 71 for `nec4param` and 243 for `ecx4param`, with
   R-hat between 1.004 and 1.018, all on the recorded predictor scale. Read the
   new table and write its numbers in. Should the contrast not reproduce on the
   log scale, the section's argument changes rather than its numbers, and #301
   and the `coral_colour` entry in `NEWS.md` are affected with it.

2. **Section 6, `ecx-pam`.** A new chunk reports NSEC, EC10 and EC50 for
   `fit_pam` with `xform = exp`. No prose reads it yet.

3. **The three new dataset figures.** `fig-herbicide-raw`, `fig-lum31-raw` and
   `fig-coral-raw` were rendered directly against the shipped data while the
   source was written, and the readings beside them are measured. They need no
   refit, but confirm they survive the vignette's chunk options.

4. **The two grouped figures.** `fig-herbicides` and `fig-metals` are built
   from `ggbnec_data()` rather than `plot()`, and were tested against a
   stand-in fit rather than the real ones. Check the facet count, the axis
   range and that the `nec_vals` rule lands inside each panel.

## The replacement of `plot()`

`plot.bayesnecgroupfit()` is base graphics and draws each level through
`plot.bayesnecfit()`, which applies its `xform` argument only where the
predictor was not transformed inside `crf()` (`R/plot.R:132`). Both grouped
fits are on `log(conc)`, so no argument to `plot()` reaches the axis.
`ggbnec_data()` returns the same model-averaged curve as a data frame on the
recorded scale, which a `ggplot` scale can then transform.
