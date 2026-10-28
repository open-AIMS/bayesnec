# Comment reasoning index

The reviews of PRs #492–#497 and #507–#509 under
[issue #484](https://github.com/open-AIMS/bayesnec/issues/484) compared the
removed comments with the retained code, tests, decision records and PR
descriptions. Most constraints remain beside the code. The references
below locate the longer reasoning and the qualifications needed to interpret
it. The later three PRs cover the eleven source files that waited for #482
and #490. Comments in `tests/testthat/` have not been reviewed or shortened
under this work; their cleanup remains outside this review.

## Estimator definitions, censoring and scales

- [Decisions D15, D17, D20, D22, D23, D33 and D44](../implementation/03_decisions.md)
  record the control reference, crossings at the control, hurdle bounds,
  censored comparisons, returned scales and the hurdle growth range.
  [Censoring tests](../../tests/testthat/test-censoring.R) and
  [estimate-comparison tests](../../tests/testthat/test-compare_estimates.R)
  exercise the retained contracts.
- [The NSEC grid measurement](helpers.md#nsec_from_posterior-the-effect-of-grid-spacing)
  distinguishes retaining the same draws from obtaining the same interpolated
  values. A wider search range can change the estimate at fixed resolution.
  [The scale guidance](../../CLAUDE.md#estimates-and-plot-data-on-different-scales)
  distinguishes fitted-scale estimates from recorded-scale plot data.
- [The summary-call evidence](entry.md) preserves the partial-argument match
  and positional `x_range` failures. The short comments retain the guards
  against those failures. `ecx_asymptote()` retains the qualification that its
  unbounded-family refusal applies to equations without `bot`; the
  no-crossing classification describes draws without a crossing. A hormetic
  curve can reach a target above its control.

## Family links and admissible means

- [The current link rules](../../CLAUDE.md#default-and-explicitly-chosen-links)
  distinguish the default identity link from an explicit supported link and
  from a supplied family object. [Family tests](../../tests/testthat/test-validate_family.R)
  cover the argument forms.
- [The logit-gate evidence](estimates.md) explains why an `ogl()` logit
  transform is refused when an equation can exceed one. The parameter-specific
  slope and beta checks remain in [support tests](../../tests/testthat/test-mu_support.R).
  [The prior-probability correction](checks.md) distinguishes the formerly
  truncated NEC priors from the distributions used after truncation was removed.

## Priors and initial values

- [Prior measurements](priors.md),
  [PR #307](https://github.com/open-AIMS/bayesnec/pull/307) and
  [the hard-case audit](../scripts/prior_hard_cases.R) preserve the empirical
  comparisons behind the endpoint anchors and widths. A location's sampling
  error, a prior's central interval and its support are different quantities.
  The spread cap can override the standard-error floor; narrowing an
  untruncated prior does not remove doses from its support.
- [Issue #350](https://github.com/open-AIMS/bayesnec/issues/350) records why
  nonnegative logged predictors need an explicit scale declaration. A zero on
  that scale is `log(1)`, whereas a concentration-scale zero is the control.
  `predictor_prior()` now states the geometric-median qualification for an
  even number of distinct doses. The prior audit's older truncated-prior
  measurements must not be read as measurements of the current priors.
- [The incomplete-design specification](../tasks/386-incomplete-designs-claude.md)
  records the limits of inferring an asymptote from an endpoint. The
  declaration applies to both hurdle blocks; neither block's observed
  endpoint establishes whether the other reached its asymptote. The
  corrected `unobserved_bot_entry()` description distinguishes its specified
  upper quantile from its branch-dependent lower tail.
- [Initial-value evidence](fitting.md#rinits_functionsr),
  [PR #312](https://github.com/open-AIMS/bayesnec/pull/312) and its
  [audit script](../scripts/init_search_audit.R) record the band, spread,
  boundary inset and simulation scope. The results are measurements on those
  designs, not coverage guarantees for every response or design.
- [`make_good_inits()`](../../R/inits_functions.R) states the current search
  and whole-fit fallback. The historical acceptance argument in
  [fitting.md](fitting.md#the-search-in-make_good_inits) does not establish
  equality of the accepted distribution after refinement; the algorithm is
  checked in [initial-value tests](../../tests/testthat/test-inits_functions.R).
- [The boundary-inset record](fitting.md#the-gap-in-boundary_inset) distinguishes
  the survival-proportion clamp from the derived initial-value floor. A
  finite log likelihood alone does not establish acceptance by that band.

## Group effects and dispersion

- [Issue #294](https://github.com/open-AIMS/bayesnec/issues/294) preserves the
  evidence for transforming constrained parameter deviations. Reducing an
  additive deviation's prior scale cannot guarantee support. The log and
  logit transforms enforce different ranges, retained in the formula comments
  and exercised by [formula tests](../../tests/testthat/test-make_brmsformula.R).
- [Issue #468](https://github.com/open-AIMS/bayesnec/issues/468), including its
  correction to the original trade-off, records why the dispersion parameter
  reads the curve before `ogl()`. Parameter-level deviations still enter that
  curve. Excluding `ogl()` from a Gamma shape does not fix the observation
  variance: the fitted mean still changes it.
- [Issue #472](https://github.com/open-AIMS/bayesnec/issues/472) records the
  family-specific dispersion-exponent bounds and the measurements behind
  them. The comments retain the distinction between those bounds and an
  empirical prior, and between forms that require a positive mean and
  `loglinear`, which does not.

## Random streams and parallel execution

- [`with_preserved_rng_state()`](../../R/helpers.R) restores generator kinds
  and `.Random.seed`, but cannot restore the cached Box–Muller normal variate.
  `normal.kind = "Inversion"` preserves subsequent normal draws;
  [RNG tests](../../tests/testthat/test-estimate_rng.R) cover both cases.
- [Parallel-fitting evidence](fitting.md#rparallel_modelsr) records the
  level-seed choice, future scheduling and exported environments.
  [Grouped-fit evidence](grouped-hurdle-joint.md) records why sequential
  levels are dispatched separately to report progress promptly.

## Validation and prediction covariates

- [D36](../implementation/03_decisions.md) records the superseded refusal of
  non-predictor `disp(~z)` columns and the current grid, prediction and
  negative-binomial restrictions. [Formula documentation](../../man/bayesnecformula.Rd)
  and [dispersion-model tests](../../tests/testthat/test-disp_model.R) describe
  and exercise those routes.
- [The validation guidance](../../CLAUDE.md#the-placement-of-a-new-data-refusal)
  explains why data-and-formula refusals belong before the model or level loop,
  with a backstop for other callers. [Check evidence](checks.md) records why
  numeric predictor replication is counted without `table()` rounding.
- [Issue #319](https://github.com/open-AIMS/bayesnec/issues/319) records why
  formula construction must preserve the caller's environment.
  [Issue #398](https://github.com/open-AIMS/bayesnec/issues/398) retains the
  tested `brms` version and failures behind the backticked-name restriction.

## Diagnostics and external methods

- [`count_divergent()`](../../R/check_sampling.R) reads diagnostics through
  `brms`, which is a required dependency; `bayesnec` lists `rstan` only in
  `Suggests`. The former claim that the direct `rstan` call fails under
  CmdStanR was incorrect: `brms` 2.23.0 imports `rstan` and converts CmdStanR
  output to a `stanfit`. [PR #490](https://github.com/open-AIMS/bayesnec/pull/490)
  records the correction in its coordination section.
- [The dispersion guidance](../../CLAUDE.md#dispersion-on-a-model-average)
  distinguishes per-equation results from a statistic for the model average.
  [Dispersion tests](../../tests/testthat/test-dispersion.R) cover the results.
- [The control-diagnostic evidence](../tasks/148-model-fit-diagnostics.md#hazards)
  records two parameterisations of one simulated dataset, with six control
  observations. Their p-values near 0.82 motivated a ratio-based message;
  they do not establish that p-values always miss control misfit. The source
  now states the mean/SD ratios and their reciprocal cutoffs explicitly.
- [`nsec.drc()`](../../R/nsec.R) retains its confidence-limit crossing
  qualification and now states that the test suite does not exercise the
  method against `drc` fits. The comment cleanup does not establish numerical
  validation of that external method.

## Provenance and remaining work

The evidence files preserve historical observations. Read their original
design, version and limitations before applying a numerical claim to a new
case. Current tests also establish only the cases they exercise.

[`.git-blame-ignore-revs`](../../.git-blame-ignore-revs) lists the 60
comment-only commits recorded by PRs #492–#497 and #507–#509. Each commit was
checked for identical parsed R code before and after. Notes-only commits,
merges, functional changes and the subsequent retention and accuracy
corrections remain visible in blame. `.Rbuildignore` excludes the file from
the package build. After integration, enable it in a local checkout with:

```sh
git config blame.ignoreRevsFile .git-blame-ignore-revs
```

This follow-up is based on [PR #509](https://github.com/open-AIMS/bayesnec/pull/509).
Its base includes 54 of the listed commits; the final review commit from each
of PRs #492–#497 must still be integrated. After those PRs and #507–#509 merge,
retarget the follow-up to `predev` and verify that every listed hash is an
ancestor of `predev`. A rebase or squash requires the list to be checked
against the resulting commits and updated before this requirement of
[issue #484](https://github.com/open-AIMS/bayesnec/issues/484) is complete.
