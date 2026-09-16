# Re-fitting example8

What the vignette's fits require, and what each re-run established. Two re-runs
have happened: the change to a log axis, and the change to `bnec()`'s sampling
defaults that followed review.

## The current source

`vignettes/example8.Rmd.orig` is an introduction to the three group-level
syntaxes followed by three case studies, one per dataset. Every figure is on a
log concentration axis. The two coral endpoints are fitted on
`crf(log(diuron_adj), ...)`, where `diuron_adj` substitutes 0.1 µg/L for the
recorded zero control. The nine fit calls name no `iter`, `warmup` or `control`
and take `bnec()`'s defaults.

The syntax section uses a 24-row synthetic frame rather than a real dataset.
`make_brmsformula()` resolves names against a data frame and needs nothing else
from it; the formulae it prints were checked against the real frame and are
identical.

## The cost of a change

The fit store is keyed on the normalised call and a digest of the data. A change
to any fit call changes its key, the render finds nothing under it and stops.
`assemble_store.R` additionally refuses a set whose units were fitted by a
`bayesnec` other than the one the manifest was built against.

Two consequences worth knowing before editing a fit call. A change to `iter`,
`warmup` or `control` re-keys every call that names it, so it is a full re-fit
rather than a partial one. And a package upgrade between runs is a full re-fit
whatever the calls say, because of the version guard.

Each run has been directed at its own `units-*` and `store-*` directory rather
than deleting the previous one, which is what the guard's message proposes. The
previous store is what the figures in the open pull request were rendered from.
`hpc/job-common.sh` in the compendium forwards `GRP_UNITS` and `GRP_STORE` into
the container to make that possible; both default as before when unset.

| run | units | store | what changed |
|---|---|---|---|
| 2026-09-16, log axis | `units-2.1.3.38` | `store-2.1.3.38` | three coral calls to `log(diuron_adj)`; bayesnec 2.1.3.37 to 2.1.3.38 |
| 2026-09-16, defaults | `units-defaults` | `store-defaults` | all nine calls drop `iter`, `warmup` and `control` |

## The sampling defaults

`bnec()` defaults to `iter = 1e4` and `warmup = floor(iter / 5) * 4`, which is
8000, set in `R/helpers.R:831`. It also raises `adapt_delta` to 0.99 itself
(`R/helpers.R:895`) wherever the mean is constrained and a group-level term is
unbounded, which is every fit in the vignette except `fits_herb`. Dropping the
explicit `control` therefore keeps 0.99 on eight of the nine calls and returns
the herbicide call to Stan's 0.8.

## The log-axis run

`vignettes/example8.Rmd.orig` draws every figure on a log concentration axis
and fits the two coral endpoints on `crf(log(diuron_adj), ...)`. Six of the
nine fit calls were already on `crf(log(...))` and are unchanged. The
`lum31` and `herbicide` datasets contain no zero concentration; `coral_colour`
and `coral_pam` do, and the vignette substitutes 0.1 µg/L for it.

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

### The scope of that refit

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

### The findings

The store-backed render at bayesnec 2.1.3.38 (job 911405, 33.6 minutes, twelve
figures, `Precompiled without error`) was compared against the render committed
at `54c6e7da` by diffing the `#>` output lines of the two `example8.Rmd` files.
820 output lines before, 799 after, and every difference falls in the coral
section.

Sections 4, 5 and 7 are identical to every digit. The EC10 comparison in
section 4 returns 0.2800498 (0.2073675--0.4097915) ungrouped and section 5's
pooled fit 0.3268868 (0.2194524--0.7082089), which is what the previous render
printed. So do the model weights, the seven herbicide thresholds and every
`prob_diff`. An earlier note here said every fit-derived number would change.
That was an assumption and it was wrong: the four commits between 5f98d8d6 and
8cf27c78 that touch priors, initial values or the model set are all
behaviour-preserving for these calls. `859c9f98` defaults `predictor_scale` to
`"auto"` and keeps the existing sign heuristic; `dd0fca16` only validates the
shape of a seed; `5f99396e` removes an equation that failed to initialise
anyway; and the rebuilt manifest gave each `lum31` set the same 14 equations
that fitted before, so `d0af28a5` filtered nothing extra.

The refit of all 180 units was still required, because `assemble_store.R`
compares the version stamp and refuses to assemble at all when it disagrees.
That guard cannot know the changes were inert.

#### The differences

- `fit_pam` fits 15 equations rather than 18, and its N(S)EC is reported on the
  fitted scale as -1.51 (-2.12, 0.73). The new `ecx-pam` chunk gives it back in
  µg/L: NSEC 0.217 (0.119--2.112), EC10 0.478 (0.401--2.155), EC50 1.999
  (1.804--2.735). Weights concentrate on `ecxhormebc4` at 0.64 and `ecxwb1` at
  0.23.
- The parameter-count diagnostics changed, and the argument with them. Under
  2.1.3.37 on the recorded scale: 0 divergences for both three-parameter
  equations, 71 for `nec4param`, 243 for `ecx4param`, every R-hat between 1.004
  and 1.018. On the log scale: 0 and 0, then 212 for `nec4param` at an R-hat of
  1.3537 and 326 for `ecx4param` at 1.0088. The old passage said a check on
  R-hat alone would not separate the two sets, which no longer holds for
  `nec4param`; it holds for `ecx4param`, which records the most divergences of
  the four at an R-hat inside every conventional cutoff. The section now makes
  the point on `ecx4param`.
- Measured from `ggbnec_data()` on the two assembled sets: at 10 µg/L the
  four-parameter credible band is 0.639 wide against the three-parameter 0.278,
  and both close to about 0.05 at 29 µg/L. NSEC 1.57 (0.66--3.87) for three
  parameters against 2.79 (1.52--21.61) for four.
- The five `1 model(s) failed to fit: ecxhormebc5` notices are gone, because
  the equation is now excluded before fitting rather than attempted and failed.

#### Left alone

`check_formula()` prints the formula it returns, and a `bayesnecformula`'s print
method shows its environment, so the rendered vignette includes a line reading
`<environment: 0x555567925aa8>`. The address is a different number on every
render and means nothing to a reader. It predates this branch and is a property
of the print method rather than of the vignette, so it is recorded here rather
than worked around in the chunk.

## The replacement of `plot()`

`plot.bayesnecgroupfit()` is base graphics and draws each level through
`plot.bayesnecfit()`, which applies its `xform` argument only where the
predictor was not transformed inside `crf()` (`R/plot.R:132`). Both grouped
fits are on `log(conc)`, so no argument to `plot()` reaches the axis.
`ggbnec_data()` returns the same model-averaged curve as a data frame on the
recorded scale, which a `ggplot` scale can then transform.
