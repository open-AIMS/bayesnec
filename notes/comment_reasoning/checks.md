# Reasoning removed from the comments of the input checks

The comments in the input-check files held the reasoning below, which no pull
request, issue or other file under `notes/` records. It was removed from the
code under #484 and is kept here.

## `R/check_data.R`

### The replication test in `flatness_contrast()`

`replicated` is computed with `tabulate(match(x_keep, ux_keep))` rather than
`table()`. `table()` builds its factor through `as.character()`, which rounds the
predictor, and it was measured at 18 times slower than `tabulate(match(...))` on
a predictor of 200,000 values. The rounding is the reason the comment keeps; the
timing is recorded here.

## `R/check_models.R`

### The prior figures in `bounded_power_drops()`

The `@noRd` block quoted six prior probabilities for `nec` to explain why the
initial-value search now finds an admissible draw for `nechormepwr` and
`nechorme4pwr`: `P(nec < 0.25)` from 0.0063 to 0.269, `P(nec < 0.1)` from
0.00011 to 0.0975, and `P(nec < 1)` from 0.481 to 0.670, comparing the gamma
prior with the lognormal prior that replaced it. Those figures and the grid
measurements are in #304 and `NEWS.md`.

Each figure is the cumulative distribution function of the prior truncated to
the tested range, which both priors had when they were measured. #393 removed
that truncation. Reading the same quantity on the untruncated distribution is an
affine map, not a rescaling: with `a` and `b` the prior cumulative distribution
function at the two former bounds, the untruncated value is `T * (b - a) + a`,
which reduces to multiplication by `b` only where the series has a zero control.
On `nec_data`, where `a` is 0.025 for the lognormal prior and 5e-7 for the gamma
prior, the first two gamma figures are unchanged to the precision printed, the
third changes from 0.481 to 0.480, and the three lognormal figures become 0.2282,
0.0986 and 0.5314. The ratio of 40 to 900 becomes 36 and 896, and the last pair
becomes 0.480 against 0.531, which supports the same conclusion less strongly.
The affine map is also stated in the message of commit `15daba70`.
