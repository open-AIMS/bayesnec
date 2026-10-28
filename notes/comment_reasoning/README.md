# Comment reasoning index

The review of PRs #492–#497 under
[issue #484](https://github.com/open-AIMS/bayesnec/issues/484) compared the
removed comments with the retained code, tests, decision records and PR
descriptions. Most constraints remain beside the code. The references
below locate the longer reasoning and the qualifications needed to interpret
it. Coverage is limited to those six PRs; later comment-cleanup PRs require
their own review.

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

## Diagnostics and external methods

- [The dispersion guidance](../../CLAUDE.md#dispersion-on-a-model-average)
  distinguishes per-equation results from a statistic for the model average.
  [Dispersion tests](../../tests/testthat/test-dispersion.R) cover the results.
- [`nsec.drc()`](../../R/nsec.R) retains its confidence-limit crossing
  qualification and now states that the test suite does not exercise the
  method against `drc` fits. The comment cleanup does not establish numerical
  validation of that external method.

## Provenance and remaining work

The evidence files preserve historical observations. Read their original
design, version and limitations before applying a numerical claim to a new
case. Current tests also establish only the cases they exercise.

[Issue #484](https://github.com/open-AIMS/bayesnec/issues/484) requires
`.git-blame-ignore-revs` and its `.Rbuildignore` entry. The descriptions of
PRs #492–#497 and [PR #509](https://github.com/open-AIMS/bayesnec/pull/509)
defer that work to a follow-up after the series merges. Use the integrated
hashes of comment-only commits; do not ignore a commit that also changes
behaviour. PR #509 also proposes closing #484 when the last comment PR merges.
The blame-ignore requirement must remain tracked until that follow-up is
complete, either in #484 or in a separate linked issue before #484 closes.
