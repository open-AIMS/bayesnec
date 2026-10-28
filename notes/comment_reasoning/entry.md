# Reasoning removed from the comments of the entry points

The comments in `R/bnec.R`, `R/bnec_joint.R`, `R/ecx.R`, `R/summary.R`,
`R/print.R` and `R/check_sampling.R` were shortened under #484. This file keeps
the two findings they held that no pull request, issue or other `notes/` file
records. Everything else removed from those comments is in the pull request or
issue that introduced it.

## `R/summary.R`

### The partial match of `ecx_vals` in `summary_ecx_rows()`

The helper's own arguments follow `...` and begin with a dot. In an earlier
form `ecx_vals` stood before `...`, where R matches a formal by partial name.
An `ecx_val` meant for `ecx()` was then taken as `ecx_vals`, and every
positional argument shifted by one place. The comment recorded that
`summary(fit, ecx = TRUE, ecx_val = 50)` returned an EC50 of 10, read over a
grid of `c(10, 50, 90)` at a resolution equal to the stored range; it did not
name the fit. After `...` a name is matched only in full, so the same call now
stops with R's "matched by multiple actual arguments", as the hurdle summary
does.

### The rebuilt call in `summary_ecx_xform()`

`summary_ecx_dots()` names `x_range` in the call it rebuilds unless the caller
supplied `x_range = NULL`, because `ecx_row()` names it in the call it makes.
Without `x_range` in the rebuilt call, a fourth positional argument was read as
`xform` by `ecx()` and as `x_range` by `summary_ecx_xform()`, and the scale
message was raised beside rows already on the recorded scale.
