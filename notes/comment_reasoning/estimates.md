# Reasoning removed from the comments on the estimators

The comments in the estimator files of `R/` were shortened under #484. This
file holds the reasoning that they recorded and that no pull request, issue or
other `notes/` file records. Everything else removed from those comments is in
the pull request or issue that introduced it.

## `R/mu_support.R`

### The logit gate in `ogl_transform_kind()`

The logit transform of a group-level deviation on the whole curve is refused
for every equation flagged `can_exceed_one`, not only for those flagged
`unscaled_excess`. An earlier gate on `unscaled_excess` admitted `nechorme`,
`nechorme4`, `ecxhormebc4` and `ecxhormebc5`, whose mean can exceed 1 through
`exp(slope) * x`.

Once the mean `m` exceeds 1, the collapsed logit form
`m e^o / (1 - m + m e^o)` has a pole at `o = log((m - 1) / m)` and changes sign
across it. At `m = 1.5` it returns 8.1e4 at `o = -1.0986` and -720 at
`o = -1.1`. Fits admitted under the narrower gate would also have lost the
raised `adapt_delta`, on the incorrect premise that their mean cannot leave its
support.
