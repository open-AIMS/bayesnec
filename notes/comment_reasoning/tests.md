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

## Parallel execution and test coverage

`test-parallel_models.R` separates actual fitted-posterior comparisons from
mocked dispatch checks. The grouped fit uses the real function without a
parallel plan; the forked grouped test replaces `bnec()`. Tests of seeded bodies
check arrangement invariance without another fit. Model-averaged posterior
indices depend on ambient RNG streams, so agreement of each fitted model does
not assert agreement of the averaged draws. The timeout comparisons check
failure handling without Stan sampling; they do not require a specific timeout
outcome.

The original comments recorded several environment-dependent observations.
These are historical measurements, not new results or performance guarantees:

- On R 4.6.1 and future 1.70.0, the real grouped call took 173 seconds without
  a plan. A two-worker multisession call inside testthat failed to return in
  300 seconds on three runs, including runs with fitting removed. It returned
  in three seconds outside testthat. A dispatcher body loading brms, raising
  an error or calling `bnec()` returned in under three seconds in either
  setting. The interaction was unidentified. This explains the real-fit and
  mocked-orchestration split; it does not establish that real multisession
  grouped fitting is covered by these tests.
- On the same R/future versions, with parallelly 1.48.0, two outer workers and
  four bare inner workers hit the 300 per cent limit because workers set
  `mc.cores = 1`. Using `I(4)` returned four; one outer worker also accepted
  bare four. Nested cluster construction took 1.7 seconds in one bare
  `future_lapply()` run but did not finish within 280 seconds in two runs
  inside the test file. The test checks that printed advice includes `I()`;
  it does not construct that nested cluster.
- On 2026-09-11 with R 4.6.1 and future 1.70.0, multisession workers could not
  find the development-only helper after `load_all()`, while forked workers
  inherited it. Installed-package availability is therefore checked before
  these worker tests. The RNG baseline must be taken after the skip helper,
  which itself advances the stream; its old placement failed on macOS that
  day.
- Under `future.seed = TRUE`, `set.seed(43); runif(1)` produced 0.48503768 in
  the parent and 0.47899959 in a sequential future on those versions. The
  test characterises future's generator conversion: a changed dependency
  could make the package's restore unnecessary without indicating a package
  regression.
- A caller-local million-element normal vector occupied 7.6 MiB and reached
  stored grouped fits before formula environments were narrowed. A terms
  environment can nevertheless legitimately be large: with
  `R_KEEP_PKG_SOURCE=yes`, serialising the bound `trials()` function after
  the CI suite forced its namespace included 1.20 MiB of source references.
  Assert absence of caller bindings, not a fixed frame byte size. A local
  transformation closure can retain its enclosing data and cannot safely
  be pruned by the formula helper.

The shorter comments retain seed restoration order, fork versus serialisation
coverage, imported namespace bindings, host/PID worker cache identities, path
normalisation, NULL bindings and RNG-kind compatibility. Further export-size
and model-averaging rationale is in [fitting.md](fitting.md).

## Validation order and formula environments

Whole-data validation precedes model and level loops so a later invalid level
does not trigger earlier compilation, repeat errors under per-model `try()`,
or report row numbers from a subset. An earlier two-level twelve-row example
completed level a before rejecting level b. Original response values determine
entry-point correction messages; corrected values must reach priors and
fitting. In the positive-response prior example, the corrected scale yielded
`gamma(2, 0.3077)` for top rather than raw-data `gamma(2, 0.25)`. Current
assertions compare with the manually corrected data.

Constant-response substitution, family-boundary exceptions and hurdle growth
refusals remain beside their tests. Beta zeros throughout have no positive
value defining a boundary shift, and zero-inflated beta zeros have no beta
block observations. The constant-equation fits use short chains to assert
locations of draws rather than precision. `amend()` inherits sampler settings
from its model set and has no seed argument.

Formula tests deliberately use local functions or `baseenv()` parents to
exclude accidental global or namespace lookup. Installed-package `trials()`
resolution and knitr's function-local chunk environments are separate traps.
A helper implemented in R also needs a Stan definition if the formula reaches
Stan; the shared `square()` example tests both sides. Echo suppression and
explicit rejection of rendered chunk errors ensure equation names come from
successful results. Unary minus is rejected at the top of `crf()` because
model-frame formula semantics differ from estimator arithmetic; `I()` retains
negation. A binary minus is outside that check's scope.

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

## Independent fits and numerical comparisons

`test-bnec_group.R` uses fitted stand-ins to exercise dispatch and arithmetic.
Its crossed-weight product requires disjoint observations, independent fits
and pseudo-BMA weights; stacking is not covered by that identity. Pointwise
WAIC comparisons must restore original observation order. Stored model
averages may hold WAIC point estimates without the pointwise values needed
for an SE. A missing SE is consequently intentional.

The RF-requested `compare_posterior()` dispatch remains documented inline.
Random-permutation pairing applies to independently fitted levels, not to
correlated levels of one joint fit. Its external record is
[the PR 228 review follow-up](https://github.com/open-AIMS/bayesnec/pull/228#issuecomment-5695869434).
No unlocated RF ruling was removed.

The joint comparison uses `sqrt(sd(j)^2 / length(j) + sd(gp)^2 / length(gp))`
as a comparison scale. This is not a median-specific Monte Carlo standard
error and does not adjust for autocorrelation. The old comment's diagnosis
that narrower or wider intervals establish duplicated or improper priors
was also too strong. The revised comments describe a finite-sample comparison
under matched priors. Composed fits use internal coefficient names;
`add_brm_defaults()` does not establish the prior validation and silent
removal that the old comment claimed. These three corrections are kept in a
separate commit from the mechanical comment cleanup.

A historical joint comparison reported NSEC disagreement of five units of that
comparison scale while curves agreed within 1.3e-3 at every grid point. The
control's one per cent tail lies on a nearly flat curve: a threshold shift of
2e-3 changed the crossing by 2e-2 in predictor units. This motivated testing
curves directly and using a steeper ECx crossing for estimator agreement.
The baseline supplied no seed or version for this measurement; it is retained
as an observation, not a calibrated median-error calculation.

The unequal-range joint fixture represents a recorded example8 failure:
irgarol's EC50 was reported as 2.68 on a combined 0.1–1000 ug/L grid with
steps near five, although its curve's EC50 was 0.53. Per-level grids preserve
resolution for the smaller range. Under brms 2.23.0, factor labels such as
`Site A` became coefficient names such as `b_bot_siteSiteA`, so matching must
follow the actual design-matrix names. Metadata mocks test that distinction.

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

## Censoring and transformed fixtures

The censoring tests exercise ordered draw ranks, reversed maps and interval
marks. Reversing a type-1 median of 100 draws selects the original 51st order
statistic, whereas the unreversed median selects the 50th. Reversing an
already summarised interval therefore differs from transforming and then
summarising draws. Tests must retain the distinction between those operations.

The original reversal fixture used 101 grid points from zero to 100, 90
identified draws between ten and 90, and eleven above-range draws. The old
per-draw result was -3.808, -4.480, -4.605 with an above-range mark on Q97.5;
remapping the summary gave -4.006, -4.605, -2.468 with a below-range mark on
Q2.5. The exponential regression values came from installed predev commit
`ba30b91e` under R 4.6.1. These fixtures relabel the stored `nec4param` fit;
they do not refit its posterior under a logged predictor.

Grid endpoints intentionally cut through stored posteriors. At 1.5, fourteen
of `nec4param`'s 100 NEC draws and twelve model-average draws were above the
grid. At 1.7, 32 relative EC10 draws were above it. Explicit transformations
use the fit grid to determine direction; the automatic inverse also clamps
outside that grid, requiring the single-fit ECx fixture to use the ECx grid.
Model-set expansion and its summary each took about ten seconds in the
historical observation, motivating shared fixtures.

Unequal hurdle bounds are tested per draw using structural fixtures. A summary
can rank an identified value below the larger component lower bound incorrectly.
This is a legacy-object or structural-fixture limitation: new fits predict both
components over the whole concentration series, giving them matching bounds.
[Issue 421's closure](https://github.com/open-AIMS/bayesnec/issues/421#issuecomment-5885594594)
records that change through PR 461. The general question of summaries with
unequal bounds remains in [issue 403](https://github.com/open-AIMS/bayesnec/issues/403).
The upper-bound fixtures use the component endpoint needed for each combination;
none establishes independently sampled component posteriors. A ten-draw fixture
with values one to nine and one above 100 guards against interpolated quantiles
implying an unsupported bound.

`test-bayesnechurdlefit-methods.R` changes family labels on stored Gaussian
draws only where tests read metadata or mock refits. Shared component draws
exercise ranges and dispatch, not posterior independence. The growth control
is the lowest observed concentration. Survival predictions must use the
appropriate curve grid. Calling a transformed estimator and returning its
values on the original scale are separate contracts.

## Censoring, random streams and fit mocks

`test-helpers.R` retains why the fixture uses 101 draws for type-1 quantile reflection symmetry and why the 40-draw case checks per-draw censoring classifications instead. Even when censoring marks agree, unmarked summaries can differ by neighbouring order statistics. It also retains restoring RNG kinds before the seed in one handler, preserving a session with no seed, and explicitly fixing the sampling algorithm. [Issue 216](https://github.com/open-AIMS/bayesnec/issues/216) records the change of sample algorithm at R 3.6.0; [issue 337](https://github.com/open-AIMS/bayesnec/issues/337) covers restoring the caller's state. These tests do not establish restoration of a cached Box–Muller normal deviate.

Rate-route tests preserve the need for a real initial fit before amend(), then a mocked added fit whose later weighting can fail after the prior is captured. The prior-only expectation compares exposure-adjusted responses using varying exposure, with historical mean exposure 3.75 and count-scale prior mean 64 against generating rate top 20 ([issue 389](https://github.com/open-AIMS/bayesnec/issues/389)). The threshold never changes units with exposure. Other short comments retain mock boundaries, test-file subprocess isolation, early refusal before model loops, and non-vacuous comparisons.

## Prior scale, zero observations and endpoint anchors

The active comments retain the distinction between a finite prior and one on the data scale, the smooth zero-fraction check, the independent expected endpoint means, and the different treatment of zeros at top, bot and the hurdle mu block. The structural-zero interpretation is qualified: an observed zero in a count control need not be structural. The positive subset supplies a scale; it is not generally an unbiased estimate of a zero-inflated count process's mean.

[Issue 210](https://github.com/open-AIMS/bayesnec/issues/210) records the collapsed count priors: at 30% zero inflation, the bot prior mean was about 0.03 against a generating bot of 5. [Issue 232](https://github.com/open-AIMS/bayesnec/issues/232) records RF's choice on 2026-08-22 to rescale the probability smoothly, and the correction from division to multiplication: `z + (1-z)p`. The deleted comparisons were top 6.8 against truth 40 immediately below the 75% threshold, and a scale jump 6.75 to 29. The assertions still test factor-of-three scale and factor-of-two adjacent stability. The conditional quantile comparison remains in executable code.

[PR 307](https://github.com/open-AIMS/bayesnec/pull/307) records designs missed by the original audit: filtering bot zeros put its mode at 10.8 against zero; keeping one survivor inflated the endpoint mean sixfold with zero estimated standard error; an all-zero control put top at 0.1 against 40; pooling three survival proportions put hubot at 0.34 against 0.014. Its review also records the unavoidable equivalence between short unreplicated designs and hurdle survival blocks, and the width cap's ability to override the uncertainty floor. The source comments retain these mechanisms and the tests retain their cases. `notes/comment_reasoning/priors.md` retains the pooled upper quantile of 72 against count top 40 and the cap condition, response range more than 25 times its mean.

[Issue 305](https://github.com/open-AIMS/bayesnec/issues/305) records the old spread ratios, normal 0.40, gamma top/bot 0.87/0.34, negbinomial top 1.15 and beta 0.88, reaching 2.71 in the audit. The old count-bot CDF range 0.988 to 0.99999, beta spread change of 12%, and mode-versus-mean ambiguity describe prior versions, not current guarantees. The assertions still compare current and released constructions. For `beta_from_mode_sd()`, an interior-mode beta has spread below the uniform limit `1/sqrt(12)`; the uniform is the fallback when the requested spread is unattainable. The ordinary calls' spread range is retained in `notes/comment_reasoning/priors.md`.

## Threshold coverage, rounding and historical fixtures

The concentration series is independent of replication. The active comments retain geometric versus arithmetic interpolation of an even number of logged doses, wide asymmetric series, low thresholds, unit changes, a nominal positive control's effect, and the automatic discriminator's inability to recognise non-negative logged values. [Issue 302](https://github.com/open-AIMS/bayesnec/issues/302) records the old support-selected priors and fixed-width gamma failures. The removed test comparison was a gamma central 95% interval of 0.41m to 2.56m, highest-dose/median ratio 125 on nassarius A, and NEC 1.25 beyond the truncated prior's 99.99th percentile. The old branches differed roughly fiftyfold in width under a unit change. These are historical comparisons, not current distributional rules.

The following details were not located in the fetched issue 314 discussion or existing notes and are retained from `test-define_prior.R` at the base revision:

- The factor-form and direct coverage spreads differed by at most `2.22e-16` relatively over 200,000 random dilution series. Prior strings differed on 1.6% of them. The original comment supplied neither seed nor environment, so these are historical figures, not a newly reproducible benchmark. On nassarius A the strings were `1.93333703285197` versus `1.93333703285198` for recorded doses, and `0.966668516425987` versus `0.966668516425988` for square roots: four of 30 design/transform/parameter cells because nec and ec50 share a string. The current test compares numeric parameters deliberately.
- On the stated 0.1-to-100 series, the previous `regularizing` normal spread was 20.96 against a log range of 6.91; the recorded-dose spread was 1.51. The old truncated CDF agreed with uniform positions to three decimals. The newer CDF was 0.052 versus position 0.158 at the second dose, and 0.945 versus 0.826 at the sixth.
- With coverage quantiles 0.995 and 0.999, the log_unit ec50 cell's truncated CDF was 0.9823 and 0.9929, outside central 95% coverage. The current assertions retain this counterexample. [Issue 314](https://github.com/open-AIMS/bayesnec/issues/314) records the ten-cell requirement, the CDF range change 0.626–0.890 to 0.536–0.974 and the settled 0.99 quantile. Neither unchanged priors nor coverage alone satisfies that requirement.
- The literal default-prior fixture in the test named “the default leaves every generated entry bit-identical” was captured on predev at `5fee2e9a`, before the unobserved-asymptote change. The old comment attributes its generator to the PR for issue 394; that generator was not located in the fetched issue body. Preserve this provenance and the literal comparisons, including rounding.

The stored-fit round trip in `test-get_priors.R` predates removal of tested-range truncation. It compares the fit's own bounds after normalising blank values to NA, rather than demanding current default bounds. At least one bound is required so equality cannot pass vacuously. [Issue 393](https://github.com/open-AIMS/bayesnec/issues/393) and the existing prior notes retain the distinction between proper distributional support and tested-range truncation. `trunc_prior_cdf()` now explicitly describes a historical comparison. A missing-upper-bound assertion does not guarantee that testthat stops the following code; coincident bounds can still make a later rejection loop endless.

## Initial-value proposal comparisons and boundary limits

For `test-inits_functions.R`, “a chain is accepted on its own, so a low per-chain rate still succeeds” uses the packaged alga c_proliferum contaminant A series, nec3param, four chains and seeds 11, 22 and 33. The old comment reported 8,000-draw per-chain rates of 0.188 with the threshold prior truncated and 0.171 without truncation; 0.101 of threshold-prior mass lay above the largest dose, producing flat-at-top curves. It reported approximate four-at-once expectations of 803 and 1,163 rounds. A 2,000-round cap left one of those seeds capped; the raised cap is 20,000. All three seeds are explicitly asserted to complete. The comment's adjacent claim that they usually failed within this cap was stale and is corrected.

The historical timing was 8.1 seconds for all three seeds, measured with `system.time()` in one Rscript process on one core on 2026-09-22. It was not a CI timing; that suite used two workers. No seed was recorded for the separate 8,000-draw acceptance measurement. A direct calculation in this review using the rounded rate 0.171 gives cap probability `(1 - 0.171^4)^20000 = 3.72e-8`; the old `below 1e-7` claim is consistent with that approximation. Rounded acceptance rates imply about 801 and 1,170 expected rounds, so they cannot reproduce the historical integer expectations exactly.

The active paired-proposal test retains why two independent searches cannot compare band width: accepting different chains changes subsequent random-number consumption. `notes/prior_audit.md` records the search audit's mean reduction of 4.58 proposals over 358 flattest cells. [Issues 266](https://github.com/open-AIMS/bayesnec/issues/266) and [290](https://github.com/open-AIMS/bayesnec/issues/290) retain the historical 561.2-second search and 577.2-second prior-check fixture, respectively. Those timings justify mocks and supplied init values, not current speed promises.

`notes/comment_reasoning/fitting.md` retains the hurdle survival `1e-66` failure and its qualifications. The assertion is above `1e-5`, not “two orders” above `1e-66`. The boundary inset comes from level means and spread, capped by the nearest interior observation; it is not simply that observation or one tenth of it. For independent equal-variance noise, the noise component of a successive difference has twice the variance, but changes in the curve also contribute. The old unreplicated smooth-curve test reported a band wider than four response ranges with sd(y); it now asserts less than two under the difference-based construction. Count observations need not include the integer 1, so no universal minimum-positive-count claim remains.

## Group support and numerical safeguards

The active comments retain population-parameter bounds required by a collapsed logit transform, the distinction between parameter support and mean support, and the reason hormesis keeps its adapt_delta adjustment. [Issues 257](https://github.com/open-AIMS/bayesnec/issues/257) and [294](https://github.com/open-AIMS/bayesnec/issues/294) retain the gradient and divergence measurements. The removed test comments reported about fourteen times more gradient evaluations with the adjustment; the herbicide Beta nec4param `(bot | herbicide)` comparison was 51 of 2,000 divergences at adapt_delta 0.95 before transformation versus zero at the default afterwards. The Gaussian isolating arm had none. A `(nec | group)` term had zero of 2,000 at 0.8.

For an invalid population value m outside [0,1], `m*exp(o)/(1-m+m*exp(o))` has a pole at `o = log((m-1)/m)` and changes sign. Bounded priors are therefore part of the transform's precondition. The exact-arithmetic interior guarantee is not a universal floating-point guarantee: at m = 0.7 the original estimate was rounding to one near o = 36, and executable checks still distinguish 35 from 40. The comparison in prior-standard-deviation units was removed because the group deviation prior is not globally fixed at 0.5. Finite test grids do not cover every value allowed by an unbounded prior.

The current group-dimension query tries the fit family first and Gaussian second; the test comment no longer claims Gaussian is always used. Family names without chosen links default to identity, while explicitly chosen links remain meaningful. nec is predictor-scaled even in count and rate models; only top and bot use the corresponding response scale.

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

## Dispersion and design diagnostics

Parameter-level transformations must enter dispersion before an overall
`ogl()` deviation is applied. Including the overall deviation produced a
second posterior mode in the zinc fit, as recorded in
[issue 468](https://github.com/open-AIMS/bayesnec/issues/468).
Gaussian retains its additive overall deviation; the original comparison in
issue 245 recorded zero per cent divergence for the identical curve and
grouping. A free population intercept on `botgl` would be unidentified against
`bot`, invalidating the latter's population-asymptote interpretation; tests
inspect generated Stan declarations to exclude it.

Dispersion tolerance calibration used 500 simulations at thirty replicates
at each of six concentrations. Estimated slope standard deviations were 0.12
for Gamma at c1 = 0.8, 0.06 for Gaussian at c1 = 0.6 and 0.14 for negative
binomial at c1 = 0.5. Tolerances allow roughly twice those spreads. The
replicate-centred prior rule and family-dependent slope bounds are recorded
in [issue 472](https://github.com/open-AIMS/bayesnec/issues/472); these measured
calibration spreads are retained here. Rate denominators and zero truncation
invalidate the ordinary negative-binomial replicate statistic. A count that
happens to become an integer after dividing by exposure is still a rate.

The flattening check compares the two highest observed concentrations;
a decline from the control measures effect size instead. Survivor growth
and survival may have different highest observed levels. Unreplicated
continuous predictors give saturated factor fits and no contrast, so the
check remains silent for that ordinary layout. Composite samples with counted
individuals instead support the binomial fallback. Replicated vessels retain
quasibinomial dispersion, verified through the p-value rather than a common
status string. For a single individual at each of two levels, the deviance
p-value near 0.048 for outcomes (1, 0) differs from the exact conditional value
0.5, motivating the information guard. Calibration at alpha = 0.05 with 500
replicates allows about 0.019 for two standard errors.

## Provenance and review scope

All 71 tracked R test files were inspected. The source for historical values
without a separate issue measurement is the comment at commit `d9e39a988e854981409bac33786efdc47ee95fb6` in the named test
file. The [original test tree](https://github.com/open-AIMS/bayesnec/tree/d9e39a988e854981409bac33786efdc47ee95fb6/tests/testthat)
preserves the accompanying expressions, seeds and fixture construction. Values without a recorded runtime or package version are not
assigned one here. The review keeps factual corrections in separate commits so
that comment-formatting exclusions do not hide those corrections from blame.
