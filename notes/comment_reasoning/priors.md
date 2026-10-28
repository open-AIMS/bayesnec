# Reasoning removed from the comments on prior construction

The comments in `R/define_prior.R` and `R/get_priors.R` were shortened under
#484. This file keeps the measurements they held that no pull request, issue or
other `notes/` file records. The rest of what was removed is in the pull request
or issue that introduced it: #232 and #236 for `positive_scale()`, #305 and #307
for the `"regularizing"` set, #302, #304 and #315 for `predictor_prior()`, #350 for
`predictor_scale`, #407 and #451 for the declaration of an unobserved
asymptote, and #250, #284 and #295 for `define_group_prior()`.

## `R/define_prior.R`

### The equations whose curve is at `top` at a predictor of zero

`regularizing_location()` assumes that the lowest concentration is at the level
`top` describes. At a predictor of zero that is an identity for thirteen of the
23 equations: the ten with a `nec` parameter, and `ecxlin`, `ecxexp` and
`ecxsigm`. The hormesis equations among them reach `top` because their excess
term is zero there. The term is `exp(slope) * x` for `nechorme`, `nechorme4`,
`neclinhorme`, `ecxhormebc4` and `ecxhormebc5`, and `x^(1 / (1 + exp(slope)))`
for `nechormepwr` and `nechorme4pwr`. `nechormepwr01` reaches `top` through a
logistic increase, `1 / (1 + (1/top - 1) exp(-exp(slope) x))`, which equals
`top` at zero.

For the other ten equations it is a limit, because the sigmoid term is not at
its asymptote at zero, and how close it comes is governed by
`exp(beta) * ec50`. At `top` 40, `bot` 5, `ec50` 2 and a rate of 1.5,
`ecxwb2` and `ecxwb2p3` were within 1e-7 of `top`, and `ecx4param`, `ecxll3`,
`ecxll4`, `ecxll5`, `ecxwb1`, `ecxwb1p3`, `ecxhormebc4` and `ecxhormebc5`
returned 38.06 to 38.34, an error of 4 per cent. `ecxll5` also depends on its
shape parameter `f`. The excess terms of `ecxhormebc4` and `ecxhormebc5` are
zero at zero, but their denominators are not at their asymptotes, so their
full curves belong to the latter group. The shallow-curve range of 17.6 to
29.7 is in #307.

### The comparators in `regularizing_location()`

On the audit's simulated `negbinomial` responses with a true `top` of 40, the
mean of the control observations ran 38 to 43, against the 95th percentile of
the pooled response of up to 72 that #307 reports. Every one of the 40 cells in
which the 95th-percentile anchor placed the true value outside the central 95
per cent of the prior was an over-dispersed count.

On the six-concentration `poisson` design descending to complete effect, the
anchor that filtered zeros from the `bot` end placed the maximum density of the
`bot` prior at 10.8 against a true `bot` of zero (#307), where the released
entry placed it at 3.07.

### The truncated median in `predictor_prior()`

Up to and including 2.1.4 the `nec` and `ec50` prior was truncated at the
highest dose tested, which removed part of the upper tail and lowered the
median. On `nec_data` the truncated median was 0.58 against a median dose of
0.88, and on the `nassarius` contaminant B series 1.23 against 2.00. #393
removed the truncation. The dose-scale median is now the exponential of the
median of the distinct logged positive doses: the geometric mean of the two
central doses when their number is even.

### The spreads `beta_from_mode_sd()` receives

`define_prior()` requests spreads from 0.064, which is `regularizing_factor`
times the standard deviation of `beta(5, 2)`, up to that standard deviation
itself, 0.160. The uniform's standard deviation is 0.289, so neither the
uniform return nor the ceiling on the doubled bracket is reached from
`define_prior()`.

### The condition under which the `ogl` cap binds

#307 records that a constant cap on the log branch of `define_group_prior()`
gave both prior types `student_t(3, 0, 1)`. The response on which that was
measured had a range more than 25 times its mean.
