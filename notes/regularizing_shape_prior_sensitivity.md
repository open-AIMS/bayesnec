# Curve-shape prior assessment for issue #535

The default set selected by `prior_type = "regularizing"` now assigns
`normal(0, 1)` to `d` and `f`. `beta` and
`slope` retain `normal(0, 5)`. All four uninformative entries remain
`normal(0, 5)`. Each parameter has one rule across equations, including the
two blocks of hurdle and zero-inflated fits.

## Width selection

`d` and `f` are log exponents. A `normal(0, 1)` prior puts the middle 95% of
their exponentiated values between 0.14 and 7.10; `normal(0, 5)` spans roughly
0.000055 to 18,000. In `ecxll5`, the response fraction at the parameter
called `ec50` is `2^(-exp(f))`. The new prior puts its middle 95% between
roughly 0.007 and 0.91. These are still broad shape ranges.

The numerical value of a rate changes when concentration units change. For
example, converting a rate of one to units 1,000 times smaller changes its log
rate from zero to -6.91. A `normal(0, 2)` prior would assign only 0.03% below
that value, compared with 8.4% under `normal(0, 5)`. `slope` also controls a
power exponent in `nechormepwr`; reducing its standard deviation from five to
two would narrow the middle 95% of that exponent from about 0.000055–0.999945
to 0.019–0.981. A single narrower zero-centred width for either parameter
would therefore constrain distinct physical curve shapes differently across
equations or units. We retained the existing width for both.

The conditional prior-curve simulation in
[`regularizing_shape_prior_curves.R`](scripts/regularizing_shape_prior_curves.R)
draws 100,000 coefficients per arm on a grid from zero to ten at unit dose
steps, holding the other curve coefficients fixed. For `d`, the fraction of
curves dropping more than 80% of the response between adjacent doses falls
from 38% to 7% after narrowing. For `f`, the fraction spanning 90% to 10%
of the response within the tested range rises from 27% to 78%. Those
fractions depend on the fixed rates and dose grid; they describe prior
behaviour for this design, not a universal target.

## Fitted sensitivity

Five known-parameter synthetic datasets compared the old and new priors.
Every pair used the same observations, likelihood and fitting controls. The
numbers below are posterior medians and 95% intervals for absolute EC10.
[`regularizing_shape_prior_fits.R`](scripts/regularizing_shape_prior_fits.R)
regenerates the observations and fits; the script saves each fit and its
session information in a chosen output directory.

| Case | Old prior | New prior | Sampler note |
| --- | --- | --- | --- |
| `ecxll5`, complete Gaussian series | 2.313 [2.047, 2.608] | 2.364 [2.098, 2.651] | Both converged, no divergences |
| `ecxll5`, sparse series without an observed asymptote | 2.692 [2.262, 3.031] | 2.685 [2.278, 3.009] | Old: 13 divergences; new: none |
| `ecxsigm`, complete beta response | 0.923 [0.788, 1.062] | 0.913 [0.794, 1.068] | Both converged, no divergences |
| `necsigm`, complete beta response | 2.669* | 2.673 [2.518, 2.806] | Old long fit failed convergence; new converged |
| `ecxsigm`, steep beta response | 2.530 [2.418, 2.632] | 2.523 [2.411, 2.633] | Both converged, no divergences |

*The old `necsigm` median comes from the short pilot. Its longer fit had
R-hat 1.59, two divergences and 996 transitions at the maximum tree depth,
so its interval is omitted. The new long fit had maximum R-hat 1.005 and no
divergences. In the short pilots, the NEC medians were 1.920 and 1.926, and
the NSEC medians were 2.098 and 2.115, under the old and new priors. The
steep sigmoid generated with `d = log(4)` recovered a median
near 1.45 under either prior; this check guards against simply pulling a
strongly identified exponent towards zero.

Weights use `loo::loo_model_weights(method = "pseudobma", BB_n = 10000)`
with seed 535; model-averaged EC10 intervals are quantiles of weighted
posterior draws. In the complete `ecxll5` case, the model's weight against
`ecxll4` changed from 0.790 to 0.809; model-averaged EC10 changed from
2.354 [2.061, 2.698] to 2.396 [2.111, 2.706]. For `ecxsigm` against
`ecxexp`, its weight changed from 0.9967 to 0.9966 and model-averaged EC10
from 0.923 [0.785, 1.062] to 0.913 [0.790, 1.068]. These LOO comparisons
had no Pareto-k values above 0.7. Across the five pairs, the in-sample
posterior predictive 95% coverage and RMSE were unchanged to the reported
precision. The fitted comparisons use one simulated dataset per case; they
cannot establish repeated-sample interval coverage or guarantee the same
sensitivity on every field dataset. Affected vignette fits should be
regenerated before release.
