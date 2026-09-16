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
old fit. Three calls changed, and their keys with them:

| chunk | call | units |
|---|---|---|
| `fit-pam` | `yield ~ crf(log(diuron_adj), "all") + ogl(chamber)` | 15 |
| `fit-par-count` | `proportion ~ crf(log(diuron_adj), c("ecxll3", "ecxwb1p3")) + ogl(chamber)` | 2 |
| `fit-par-count` | `proportion ~ crf(log(diuron_adj), c("nec4param", "ecx4param")) + ogl(chamber)` | 2 |

`fit_pam` holds 15 units rather than the 18 the recorded predictor gave.
Measured with `check_models()`: a negative predictor removes `necsigm` and
`ecxsigm`, which raise the predictor to a fractional power, and `ecxhormebc5`,
which cannot be reliably initialised where an identity-linked `Beta` mean must
stay positive. `log(diuron_adj)` runs from -2.30 to 3.37.

### The scope of the refit

The six unchanged calls keep their keys, so on the key contract alone 19 units
needed fitting. They were fitted, and `assemble_store.R` then refused the
store:

```
Error: units of fit_plain were fitted by bayesnec 2.1.3.37, but the manifest
was built against 2.1.3.38.
```

That guard is correct. A set whose units come from two versions of the package
is not a model average of anything, and 2.1.3.38 includes the `ecxhormebc5`
exclusion of bayesnec #344 among other changes. The store the vignette ships
against has to be one version throughout.

The manifest fell from 189 units to 180 for the same reason. `ecxhormebc5` was
planned and attempted under 2.1.3.37, where it failed to initialise on all six
`lum31` sets; under 2.1.3.38 it is excluded before fitting, so each `lum31` set
plans 14 units rather than 15 and `fits_tox` plans 28 rather than 30. The set
that is averaged over is the same either way --- the equation never fitted ---
but it is now recorded as excluded rather than as failed.

The consequence for this document is that every fit-derived number in the
vignette changes, not only section 6's. The list below covers all of them.

The run was directed at `units-2.1.3.38/` and `store-2.1.3.38/` rather than
deleting the 2.1.3.37 units, which is what the guard's message proposes. The
previous store is what the figures in the open pull request were rendered from,
and nothing is gained by destroying it. `hpc/job-common.sh` in the compendium
now forwards `GRP_UNITS` and `GRP_STORE` into the container, which is what makes
that possible; both default as before when unset.

## Claims awaiting a render

Every number below came from a render at bayesnec 2.1.3.37 on the recorded
predictor scale. Each has to be read off the new render and written in, or
removed where it no longer holds. Line numbers are of `example8.Rmd.orig` at
`8cf27c78`.

| where | claim to re-read |
|---|---|
| §4, ~632 | the concentration the `ogl(conc_group)` curve holds to before falling; EC10 0.280 (0.207--0.410) against 1.158 (0.624--2.377), and the multiples stated with them |
| §4, ~549 | the equation `which.max(mod_weights)` selects, and the `conc_group` standard deviation read from it |
| §5, ~695 | EC10 0.327 pooled against 0.349; intervals 0.219--0.708 against 0.314--0.383; N(S)EC 0.198 (0.049--0.533) against 0.771 (0.228--1.246) |
| §5, ~703 | weights `ecxll5` 0.55, `ecxwb1` 0.32, `ecxll3` and `ecxhormebc4` 0.05 each, against `ecxll5` 0.85 and `ecxwb1` 0.15; and that no threshold-estimating equation takes weight in either fit |
| §6, ~880 | the divergence counts and R-hat range, which the passage now states without. Under 2.1.3.37 on the recorded scale: 0 divergences for both three-parameter equations, 71 for `nec4param`, 243 for `ecx4param`, R-hat 1.004 to 1.018 |
| §6, `ecx-pam` | a new chunk, no prose reads it yet |
| §7, ~1000 | the seven N(S)EC values, irgarol 0.135 (0.108--0.176) through tebuthiuron 10.591 (4.349--14.154) |
| §7, ~1006 | `prob_diff` 0.0075 for the three pairs against tebuthiuron, 0.724 for diuron against irgarol, 0.0138 for the metals |

Two further claims are measured from the data rather than from a fit and are
unaffected by the refit, but were not measured in this session: the control
wells spanning about 250,000 to 1,450,000 relative light units across the
seventeen plates (§5, ~689), and the 880 divergent transitions `ecxll5`
returned at `adapt_delta = 0.8` (Provenance), which came from a separate run
rather than from the store.

The three new dataset figures and their readings were measured directly against
the shipped data while the source was written and need no refit. Confirm they
survive the vignette's chunk options. `fig-herbicides` and `fig-metals` were
tested against a stand-in fit rather than the real ones: check the facet count,
the axis range, and that the `nec_vals` rule lands inside each panel.

## The replacement of `plot()`

`plot.bayesnecgroupfit()` is base graphics and draws each level through
`plot.bayesnecfit()`, which applies its `xform` argument only where the
predictor was not transformed inside `crf()` (`R/plot.R:132`). Both grouped
fits are on `log(conc)`, so no argument to `plot()` reaches the axis.
`ggbnec_data()` returns the same model-averaged curve as a data frame on the
recorded scale, which a `ggplot` scale can then transform.
