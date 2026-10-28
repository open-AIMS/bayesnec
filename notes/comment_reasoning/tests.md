# Test commentary and retained reasoning

The test-comment review preserves the assumptions needed to interpret assertions
and keeps historical measurements here. The measurements below were recorded in
the comments being shortened; they are not new benchmark runs or guarantees for
regenerated fits. Executable expressions, including test names, remain unchanged.

## Fixtures and limits of coverage

Several plotting and scale-message tests relabel a stored fit's formula without
refitting it. They test metadata, transformations and dispatch, not posterior
estimates under the substituted formula. `setup.R` uses an exponential response
transformation because the fixture includes negative responses, with a recorded
minimum near -6.9; logarithms would produce non-finite values. The model-average
fixture relabels its first component only. Tests of individual equations must
use their own formula metadata.

Base plots are inspected through a null PDF device and `par("usr")`. Axis-span
ratios cancel the device's usual 4% expansion. Annotation tests intercept actual
`abline()` arguments: recomputing an expected annotation without observing the
call would miss a regression. Counting calls alone also cannot distinguish a
NEC line from an EC10 line. Both axis and annotation assertions are needed on
single-fit and model-average paths.

Mocked `brm()` write-back tests inspect supplied data, not Stan execution. They
supply initial values to avoid a stochastic search that can take minutes and
mock chain validation because the fake result has no chains. The separate
`empty = TRUE` route constructs a real brmsfit without compiling or sampling.
The dispersion and model-selection fixtures that assemble components by hand
exercise dispatch and bookkeeping, not independent fitted posteriors.

`capture_brms_update()` substitutes the entry in the generic's S3 table in the
stats namespace and restores it afterwards. An earlier `trace()` in the brms
namespace changed both entries, while `untrace()` restored only the namespace
binding. The recorded result was a later real `update()` stopping with
`halted by test`. An option stores the stub's result because its dispatch
environment differs from the test environment.

Cached fits and diagnostic tables are lazy and shared only by read-only tests.
A mutating consumer needs its own fixture. Apply guards before requesting a
fixture that compiles or samples. A skipped consumer does not prevent another
consumer from building the cache. Tests asserting first-call warnings must make a fresh call.

The amend test permits up to three attempts after an intermittent Ubuntu-devel
initialisation failure at 200 iterations, then skips; its class and shape checks
apply when an attempt succeeds. The timeout test requests 1 ms with one chain,
200 iterations, 100 warmup iterations and seed 10. Its assertion accepts an
error or a `bnecfit`; it does not count surviving models or impose an independent
wall-clock deadline. Where an interrupt lands depends on the runtime. These
limitations remain explicit in adjacent comments.

The fit-store helper is vignette tooling sourced in its own environment. Source
and R CMD check layouts are tried; installed coverage layouts may omit the file
and legitimately skip these tests. The store assembled on 2026-09-18 recorded
version 2.1.3.39. The prior change in
[PR409](https://github.com/open-AIMS/bayesnec/pull/409) initially kept that version,
so it cannot distinguish stores fitted before and after the change.

## Historical plotting and estimator measurements

Before the response-transform plotting fix, `xform = function(x) x * 100` left
`max(x_e)` at 3.22 while the largest NEC annotation changed from about 1.53 to
152.8 for `nec4param`, or 152.7 for `manec_example`. The largest ECx annotation
changed from 1.56 to 156.4. The issue described the axis defect; these annotation
measurements were recorded separately in the tests.
[Issue268](https://github.com/open-AIMS/bayesnec/issues/268) records its closure
through [PR281](https://github.com/open-AIMS/bayesnec/pull/281). The old test
comments telling a future maintainer to invert the assertions were stale: the
assertions already check the repaired behaviour. Their older test titles remain
unchanged to preserve executable identity.

On the packaged ECx fixture, the comments recorded an asymptote near -8.42 and
a predicted minimum near -4.13. With the absolute floor at zero, its ECx order
was absolute, range, relative. This ordering belongs to that fixture; it is not
a universal property when floors or response scales change. The nonlinear
transformation tests retain their numerical tolerance because finite-sample
quantile interpolation need not commute exactly with a nonlinear transform,
even when it is monotone.

The extrapolation regression recorded a rebuilt mixture summary of
1.449 (0.808, 1.528), compared with the stored 1.450 (0.749, 1.527), despite no
censoring. The lower interval endpoint differed by about 8%. Returning the
already identified stored posterior avoids that unnecessary recomputation.
Another recorded failure combined `extrapolate = 9` with
`x_range = c(0.5, 0.9)` but searched from 0.0324. Tests now preserve an explicit
lower search limit, while absent `x_range` uses the union of stored and observed
bounds. A synthetic pure-NEC set reuses one fitted posterior under two equation
names; it tests that branch's handling of draws and cannot validate independent
model fitting.

The bounded `nechormepwr` comments recorded successful initial NEC values around
0.08–0.24, and a 40–900-fold change in their truncated prior probability when
the earlier Gamma prior was replaced by a lognormal prior. This is historical
initialisation evidence, not evidence that all later draws keep a bounded mean
inside (0, 1). The prior is no longer truncated to the predictor range; the
previous comment claiming otherwise was corrected. Existing support reasoning
is retained in [prior notes](priors.md).

In summary tests, a supplied grid ending at 1 puts all EC50 draws above the
upper bound on both fixtures. With `xform = exp`, the bound is about 2.72. Before
[PR439](https://github.com/open-AIMS/bayesnec/pull/439), the summary ignored those
arguments and printed the stored-grid estimate near 1.67 without marking it.

## Censoring and comparison design

The ten-pair synthetic comparison distinguishes strict ordering, bounded pairs,
indeterminate bounds and unexplained missing values. Its reasoning is:

| Pair | First value | Second value | First strictly greater? |
| --- | --- | --- | --- |
| 1 | 5 | 3 | Yes |
| 2 | 3 | 5 | No |
| 3 | 4 | 4 | No |
| 4 | At least 10 | 3 | Yes |
| 5 | 3 | At least 10 | No |
| 6 | At least 10 | At least 10 | Indeterminate |
| 7 | At most 1 | 3 | No |
| 8 | At most 1 | At most 1 | Indeterminate |
| 9 | At least 10 | At most 1 | Yes |
| 10 | Unexplained missing value | Any | Missing |

The table distinguishes three positive pairs from four known negative pairs;
two pairs remain indeterminate and one is missing. Its purpose is to prevent
silently dividing by only the identifiable subset.
[PR404](https://github.com/open-AIMS/bayesnec/pull/404) records the motivating
example: 74 and 59 of 100 EC50 draws were censored, 90 paired differences were
missing, yet a probability of 0.6 was presented over the remaining ten.
Fixture-dependent NSEC comparisons explicitly check their preconditions and
skip if regenerated curves no longer produce the required censoring contrast.

NSEC draws whose control is already below the reference must remain identified
even if a grid starts above the control. Other draws crossing before that grid
can be censored. The test uses `sig_val = 0.05` on a 100-draw fixture so that the
control-below-reference class is countable. Preserving those draws does not
promise identical interpolated NSEC values on grids with different spacing.

## Diagnostics, numerical choices and performance

[The diagnostics specification](../tasks/148-model-fit-diagnostics.md) preserves
the rationale for surfacing control-fit ratios: a recorded roughly 19% control
overshoot had a posterior predictive p-value around 0.82. Reporting absolute ESS
alongside its ratio separates total effective information from efficiency;
400 absolute ESS corresponds to the 100-per-chain guideline at four chains.
Independent prior draws and fixed-parameter diagnostics are excluded from the
posterior convergence screen. The stored-fixture comparison with brms does not
run a CmdStanR fit. The corrected dependency description distinguishes the
package's suggested rstan dependency from brms's dependency requirements.

[Issue298](https://github.com/open-AIMS/bayesnec/issues/298) records why Pearson
residual exclusions are draw-specific: of 400 draws, 229 excluded nothing, 60
excluded three observations and 111 excluded six. A zero-variance disagreement
retains an infinite residual. The shared underflow fixture has no survivors at
its two highest doses; the fitted `nec3param` mean reaches numerical zero there.
A separate Poisson dispersion fit deliberately retains compilation to avoid a
family/link update route implicated in
[Issue247](https://github.com/open-AIMS/bayesnec/issues/247). Its former comment
estimated an 80-second compilation saving; this is an old local estimate.
The broader dated compilation inventory and environment are in
[Issue328](https://github.com/open-AIMS/bayesnec/issues/328).

The rate fixture uses seed 1, 40 observations and exposures 1, 2, 4 and 8. Its
true top is 20; the old count-based prior mean was about 61. Recorded count-scale
versus rate-scale dispersion centres were 3.02013 versus 2.19722 for the power
form and 20.5 versus 9 for the loglinear form. The assertions use broad prior
bounds and directly computed centres instead of freezing those old numbers.
Response asymptotes have rate units under the identity link; the NEC parameter
retains predictor units.

The drc equation comparison varies every parameter and includes thresholds at
and beyond the dose grid. Taking `b = exp(beta)` gives the intended mapping;
round-tripping from `b` through `exp(log(b))` previously produced differences
around 7e-15. The inlined drc formula fixes the numerical reference without
adding a dependency. Negative drc slopes and its fixed-top NEC.2 case lie
outside the documented mapping.

## Provenance and review scope

The source for historical values without a separate issue measurement is the
comment at commit `d9e39a988e854981409bac33786efdc47ee95fb6` in the named test
file. That snapshot preserves the accompanying expressions, seeds and fixture
construction. Values without a recorded runtime or package version are not
assigned one here. The review keeps factual corrections in separate commits so
that comment-formatting exclusions do not hide those corrections from blame.
