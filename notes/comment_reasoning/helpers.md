# Reasoning removed from the comments in R/helpers.R

The comments in `R/helpers.R` were shortened under #484. The reasoning below
was held only in those comments and is recorded in no pull request, issue or
other `notes/` file, so it is kept here. Everything else removed from the file
is recorded in the pull request that introduced it.

## `R/helpers.R`

### `normalise_cens()`, the reimplemented encoding

The mapping from a `cens()` indicator to the integer codes is fixed by the
formula syntax that `brms` documents, so the reimplementation is not expected
to drift from `brms:::prepare_cens`. An unrecognised value is returned as `NA`
and left for `brms` to reject, so that the error message a user sees comes
from one place.

### `add_brm_defaults()`, the `adapt_delta` raise

Measured on `nec3param` with an `ogl()` term and a `beta_binomial` response:
50.2 per cent of transitions were divergent at the `brms` default
`adapt_delta` of 0.8, against 4.9 per cent at 0.99. The other figures the
comment held (87.3 per cent on poisson at 0.99, the range of 0.03 to 42.9 per
cent across the sweep, and the raise requiring about fourteen times the
gradient evaluations) are recorded in #250, and the herbicide measurements in
#295.

### `nsec_from_posterior()`, the effect of grid spacing

Prepending the control to the searched grid makes the set of draws identified
independent of what `x_range` adds below the data, but the values of those
draws still change with the grid spacing, as any interpolated estimate does.
On the `ecx4param` fit used for the measurement, the estimate was 0.0001 at
`x_range = c(0, 3.22)`, the observed range, and 0.096 at `c(0, 100)`. The
`resolution` argument controls this. The gap the prepending closes, 11 of 100
draws lost at `c(0, 100)`, is recorded in decision D17 of
`notes/implementation/03_decisions.md`.

### `warn_censored_draws()`, the warning class

The warning has the class `bayesnec_censored` because
`nec.bayesnechurdlefit()` summarises what `nec()` returned for each component.
An unclassed warning was printed once for each component and once for the
combination, three times for one call.
