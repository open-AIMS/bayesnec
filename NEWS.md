# bayesnec 2.1.3.2

- New vignette, *Hurdle and zero-inflated concentration-response models*
  (`vignette("example6")`), covering the two routes to these models, what the
  combined endpoint gives that a survivors-only analysis cannot, when a hurdle
  Beta is and is not the right reach, and — using the shipped `herbicide` data
  as a counter-example — how to tell a structural zero from a rounded or
  floored one before choosing a model. See
  [#175](https://github.com/open-AIMS/bayesnec/issues/175).
- `bnec()` gains a `model_survival` argument, so a joint `"hurdle_gamma"` or
  `"zero_inflated_beta"` fit can use a different equation on each of its two
  blocks rather than the same one on both. The `crf` model in the formula names
  the response block's equation and `model_survival` the survival block's.
- New functions `best_crossed()` and `bnec_joint()`. `best_crossed()` reports
  the model combination carrying the highest weight in `crossed_weights()`, and
  `bnec_joint()` refits that combination as a single two-block model. Together
  they connect the two implementations in the order they are meant to be used:
  fit and average the model sets with `bnec_hurdle()`, where all
  `n_response * n_survival` combinations follow from two fits, then refit the
  selected combination jointly where structure spanning both blocks is needed.
- Fixed `ecx()` silently ignoring `dpar` on a model-averaged joint hurdle fit.
  `ecx.bayesmanecfit()` passed a fixed, positionally-matched argument list to
  its per-model calls, so `dpar` was dropped and the combined endpoint returned
  in place of the requested parameter block — a wrong answer with no error or
  warning. Only `bayesmanecfit` objects from the joint route were affected;
  `bnec_hurdle()` fits and single-model joint fits were always correct.
- `nsec()` gains a `dpar` argument, matching `ecx()`. It previously had no way
  to select a parameter block, so on a joint two-block fit it could only ever
  describe the combined curve.
- `dpar` is now a documented, formal argument of `ecx()` and `nsec()` rather
  than one read out of `...`, and supplying the wrong component argument is an
  error instead of being silently discarded: `which` belongs to a
  `bayesnechurdlefit` (two separate fits) and `dpar` to a joint two-block fit
  (two parameter blocks in one model). `nec()`, which reports the combined
  threshold and has no block selection, likewise rejects `dpar` rather than
  ignoring it.
- `summary()` on a `bayesnechurdlefit` now labels each no-effect estimate NEC,
  NSEC or N(S)EC according to the models in that component's set, and passes
  `...` (notably `xform`) through to both `nec()` and `ecx()`. It no longer
  errors when a component is model-averaged. The `?nec` documentation now
  states explicitly that a model-averaged estimate over a mixed model set is an
  N(S)EC rather than a NEC.
- New dataset `nassarius`: four chronic toxicity tests on the snail
  *Nassarius dorsatus*, one row per individual exposed, with mortality
  reconstructed from the four ways it was recorded other than as zeros. Used
  as the worked example in `vignette("example6")`.
- `bayesnechurdlefit` objects returned by `bnec_hurdle()` now support the full
  set of package methods. `summary()`, `plot()`, `autoplot()`, `ggbnec_data()`,
  `predict()`, `fitted()`, `posterior_epred()`, `posterior_predict()`, `nsec()`
  and `ecnsec()` all take a `which` argument — `"combined"` (the default),
  `"growth"` or `"survival"` — and `plot()`/`autoplot()` additionally accept
  `"all"` for a three-panel view. `rhat()`, `check_chains()`, `check_priors()`,
  `pull_brmsfit()`, `pull_out()`, `dispersion()` and `model.frame()` describe a
  single fit and so return one result per component. `amend()`, `update()`,
  `c()` and `+` apply to both components, keeping the pair in step;
  `update(newdata = )` re-splits the data so zeros continue to denote the
  hurdle. `compare_posterior()`, `compare_fitted()` and `average_estimates()`
  accept hurdle fits and compare their combined endpoints.
- Added the `"zero_inflated_beta"` family to `bnec()` and `bnec_hurdle()`, for
  proportional responses on (0, 1) where exposure also produces exact zeros.
  Structurally identical to `"hurdle_gamma"` — zero-inflation differs from a
  hurdle only when the base distribution can itself emit zeros, which neither
  the Gamma nor the Beta can — so the same two-block machinery serves both; the
  only difference is that `brms` names the second block `zi` rather than `hu`,
  giving parameters `zitop`, `zinec` and so on. `ecx()` accepts
  `dpar = "zi"`.
  Note that where such a response has been obtained by dividing through by a
  maximum, that divisor must be a constant fixed in advance rather than one
  computed from the data; see
  [#173](https://github.com/open-AIMS/bayesnec/issues/173).
- Added the `"hurdle_gamma"` family to `bnec()`, for concentration-response
  data where exposure both kills individuals and suppresses the response of
  those that survive. Zeros in the response denote individuals that did not
  survive, and the fit gains a second parameter block (prefixed `hu`) giving
  the probability of that its own concentration-response curve. All 23
  equations are available for both blocks. `posterior_epred()` returns the
  combined endpoint `mu * (1 - hu)`, so `ecx()`, `nsec()` and `plot()` describe
  it by default; `ecx()` gains a `dpar` argument for the components, and
  `nec()` returns the combined threshold. Must be requested explicitly as
  `family = "hurdle_gamma"` — a response containing zeros is still treated as
  Gamma so that existing analyses do not change.
- `check_data()` now emits a message when it shifts zeros away from zero for a
  Gamma fit, pointing at `hurdle_gamma` as the alternative. This behaviour was
  previously silent.
- Fixed `extract_pars()`, which matched parameter names without anchoring. For
  a hurdle fit `"top"` also matched `"hutop_Intercept"`, which returned `NA`
  for every parameter and caused `expand_nec()` to misclassify a `nec` model as
  an `ecx` one, silently reporting an NSEC as the NEC. It also now returns `NA`
  rather than erroring when a parameter is absent.
- Added `bnec_hurdle()` for concentration-response data where exposure both
  kills individuals and affects the response of those that survive. It fits the
  two components as two ordinary `bnec()` calls — a zero-bounded model for the
  survivors and a `bernoulli` model for survival — and returns them together as
  a `bayesnechurdlefit`. Zeros in the response denote individuals that did not
  survive. `nec()` and `ecx()` gain a `which` argument taking `"combined"` (the
  default), `"growth"` or `"survival"`.
- Added `crossed_weights()`, which returns pseudo-BMA weights over every
  combination of the two model sets in a `bayesnechurdlefit`. Because the hurdle
  likelihood factorises, these are the outer product of the two components'
  weights, so the full crossed comparison is available without fitting every
  pair.
- The `nec()` and `ecx()` generics now take `...`, so methods can add arguments.
  No change for existing methods.
- Corrected the `?models` description of which equations are excluded for 0, 1
  bounded families under an identity link. It stated that all models with a
  `slope` parameter are excluded; `check_models()` excludes only the three
  linear-decay models (`neclin`, `neclinhorme`, `ecxlin`). Whether the remaining
  restriction is still needed is tracked in
  [#170](https://github.com/open-AIMS/bayesnec/issues/170).
- Added a `prior_type` argument to `bnec()` and `amend()` for selecting the set
  of default priors. The default, `"uninformative"`, reproduces the
  weakly-informative priors described in Fisher et al. (2024, JSS). The new
  `"regularizing"` option builds narrower priors, with the no-effect (`top`)
  parameter centred on the control mean (the upper end of the response range
  for these monotonically decreasing models).
- `bnec()` now takes `prior` as an explicit argument (previously passed through
  `...`). This ensures a user-supplied `prior =` is matched exactly rather than
  being partial-matched to the new `prior_type` argument.
- Added a `timeout` argument to `bnec()` and `amend()` to cap the time allowed for any single model fit, so that a model with highly divergent (slow) chains can be abandoned while the remaining models still fit ([#157](https://github.com/open-AIMS/bayesnec/issues/157)).

# bayesnec 2.1.3.0

- Added citation for JSS manuscript describing `bayesnec`.

# bayesnec 2.1.2.0

- Added `step` function to toolkit to allow for `brms` non-linear formula evaluation ([89edef72](https://github.com/open-AIMS/bayesnec/commit/89edef72)).

# bayesnec 2.1.1.0

- Fixed issues with `summary` and `print` methods ([bf058f36](https://github.com/open-AIMS/bayesnec/commit/bf058f36)).

- Plotting methods now display the proper no-effect toxicity legend label ([2be55d51](https://github.com/open-AIMS/bayesnec/commit/2be55d51)).

- Implemented native `brms::beta_binomial()` instead of custom family `beta_binomial2` ([c6c78fb7](https://github.com/open-AIMS/bayesnec/commit/c6c78fb7)).

- `compare_*` functions now have N(S)EC as default behaviour ([c4c84ba1](https://github.com/open-AIMS/bayesnec/commit/c4c84ba1)).

- Changed argument name `precision` to `resolution` in many functions to maintain syntax compatibility with `brms` ([727d6700](https://github.com/open-AIMS/bayesnec/commit/727d6700)).

- Standardised plot style between `sample_priors` and `check_priors` ([6c59af07](https://github.com/open-AIMS/bayesnec/commit/6c59af07)).

# bayesnec 2.1.0.3

- Improved speed in test runs for CRAN ([d5d097fb](https://github.com/open-AIMS/bayesnec/commit/d5d097fb)).

- Aligned generic method consistency for  `rhat`  ([afb10577](https://github.com/open-AIMS/bayesnec/commit/afb10577)).

- Fixed a bug relating to 01 bounded x data in  `define_prior`  ([3ab93e0](https://github.com/open-AIMS/bayesnec/commit/3ab93e0)).

- Fixed messages returned for `set_distirbution` ([f483160](https://github.com/open-AIMS/bayesnec/commit/f483160)).

- Reduced n_trials and deleted unnecessary set.seed in while call for `make_good_inits` ([3c5f084](https://github.com/open-AIMS/bayesnec/commit/3c5f084)).


# bayesnec 2.1.0.2

- Fixed bug introduced when implementing recycling the seed passed to `bnec`  ([01394a17](https://github.com/open-AIMS/bayesnec/commit/01394a17)).

- Included additional criteria in the initial values algorithm to ensure initial values can be fit in stan ([fe89484a](https://github.com/open-AIMS/bayesnec/commit/fe89484a)).

# bayesnec 2.1.0.1

- Cleaned parameter title names in `sample_priors` ([6973bae1](https://github.com/open-AIMS/bayesnec/commit/6973bae1)).

- Small tweak to initialisation search, such that the seed used for a `bnec` call (via `...` arguments to `brms::brm`) gets recycled and therefore it generates the same initialisation values across platforms ([c74a3c46](https://github.com/open-AIMS/bayesnec/commit/c74a3c46)).

# bayesnec 2.1.0.0

- Many improvements to package following suggestions from JSS editors. These include many things like dependency on R 4.1 to support native pipeOp, revamp of predict method for classes, major overhaul on package documentation, and much more. All issues were dealt with collectively via a [milestone](https://github.com/open-AIMS/bayesnec/milestone/4?closed=1).

# bayesnec 2.0.2.5

- Fixed issue with new "inits"-->"init" argument name in `brm` ([`30eb8e6`](https://github.com/open-AIMS/bayesnec/commit/30eb8e6)).

# bayesnec 2.0.2.4

- Fixed issue with new prior structures in `brms` ([`c5c16be`](https://github.com/open-AIMS/bayesnec/commit/c5c16be)).

# bayesnec 2.0.2.3

- Fixed issue with `expand_manec` ([`1ced4d4`](https://github.com/open-AIMS/bayesnec/commit/1ced4d4)).

- Fixed issue with `check_formula` ([`87fab44`](https://github.com/open-AIMS/bayesnec/commit/87fab44)).

# bayesnec 2.0.2.2

- Fixed issue with `gsub` substitution of x in original brms non-linear formulas, and added various tests ([`fa588fb`](https://github.com/open-AIMS/bayesnec/commit/fa588fb)).

# bayesnec 2.0.2.1

- Fixed vignette formula bugs ([`162fec9`](https://github.com/open-AIMS/bayesnec/commit/162fec9)).

# bayesnec 2.0.2

- Fixed CRAN bugs related to `testthat` ([`cfe9f20`](https://github.com/open-AIMS/bayesnec/commit/cfe9f20)).

- Changed decline detection behaviour in function `check_data` ([`c2744e3`](https://github.com/open-AIMS/bayesnec/commit/c2744e3)).

# bayesnec 2.0.1

- Addition of `herbicide data` ([`2b13ef5`](https://github.com/open-AIMS/bayesnec/commit/2b13ef5)).

- streamlined `check_*` functions for negative predictors vectors -- no error anymore (via `check_data`), now drop models with informative message (via `check_models`) ([`f134311`](https://github.com/open-AIMS/bayesnec/commit/f134311)).

# bayesnec 2.0

- `bnec` now works exclusively with formulas ([`4bccc07`](https://github.com/open-AIMS/bayesnec/commit/4bccc07), [`d3531b4`](https://github.com/open-AIMS/bayesnec/commit/d3531b4), [`bccb0da`](https://github.com/open-AIMS/bayesnec/commit/bccb0da), [`340269e`](https://github.com/open-AIMS/bayesnec/commit/340269e)).

- Added `+`, `c` and `update` methods via a common class `bnecfit` ([`d5e81ab`](https://github.com/open-AIMS/bayesnec/commit/d5e81ab), [`3adf3fa`](https://github.com/open-AIMS/bayesnec/commit/3adf3fa)).

- Fixes to `ecx`, `nec` and `nsec` following new formula-based implementation ([`6ec1372`](https://github.com/open-AIMS/bayesnec/commit/6ec1372), [`0a9d307`](https://github.com/open-AIMS/bayesnec/commit/0a9d307) and [`19a37b9`](https://github.com/open-AIMS/bayesnec/commit/19a37b9)).

- Fixes to predict methods following formula-based implementation ([`8843a1c`](https://github.com/open-AIMS/bayesnec/commit/8843a1c), [`08484a7`](https://github.com/open-AIMS/bayesnec/commit/08484a7)).

- Dispersion now takes a `bayesnecfit` as input ([`200bc11`](https://github.com/open-AIMS/bayesnec/commit/200bc11)).

# bayesnec 1.1.0

- Added `brms::bernoulli` to the list of allowed model distributions ([`7606461`](https://github.com/open-AIMS/bayesnec/commit/76064617a299a07da29c690a7d07715c889bfa65)).

- Made `identity` the default link ([`6640867`](https://github.com/open-AIMS/bayesnec/commit/664086715092dfd9a61b766ea13230743beee0b7)).

- Added more controls and tweaks to checking model relative to input data ([`85a3819`](https://github.com/open-AIMS/bayesnec/commit/85a38196a5e963df956787174cd5235c7e65ae02)), [`065a445`](https://github.com/open-AIMS/bayesnec/commit/065a44513d509d9812c872bcc130e6c2db402049)).

- Added `formula` and `model.frame` methods to main classes ([`9df4a64`](https://github.com/open-AIMS/bayesnec/commit/9df4a64d1fb1ced15824a0d388d61e712dba7d92)).

- Upgraded `ggbnec` to `autoplot` as `bayesnec` standard `ggplot2` plotting method ([`65fe15f`](https://github.com/open-AIMS/bayesnec/commit/65fe15fec2c2a13f34d3238c05a841dffd7a1780)).

- Enhanced handling of argument `loo_controls` in `bnec` to allow argument for both `brms::add_criterion` and `loo::loo_model_weights` ([`9712fcc`](https://github.com/open-AIMS/bayesnec/commit/9712fcce54adc83dbd26edc9ce659dda354fed6a)).

- Added Bayesian R^2^ to `summary` ([`9a71a3c`](https://github.com/open-AIMS/bayesnec/commit/9a71a3cc2b9cfacf5920094cbd16f7da81709e4a)).

- Expanded `bnec`'s capacity to accept input x and y vectors, data frames ([`f256b39`](https://github.com/open-AIMS/bayesnec/commit/f256b399ab9115fffa7349a7a9daef21090f53f5)) and formulas ([`32e74ac`](https://github.com/open-AIMS/bayesnec/commit/32e74ac419c39c660aceb3d0914622de753a7a83)).

- Corrected error for logit link cases for the beta_binomial2 where data contain 0 and 1 to ensure appropriate prior values on top and bot ([`4158237`](https://github.com/open-AIMS/bayesnec/commit/41582378a1a55c9420f69e578cfc98dc23182515)).

- Series of internal fixes to standardise function class outputs ([`81369bb`](https://github.com/open-AIMS/bayesnec/commit/81369bbaef5e860410a5e2cc5227b6033687d36c), [`1c70efe`](https://github.com/open-AIMS/bayesnec/commit/1c70efeea54abe39c078ebfd014434e060c6f337), [`5bce2b5`](https://github.com/open-AIMS/bayesnec/commit/5bce2b5c40d8c1c480423529aaa59e0c82eda188), [`455ca70`](https://github.com/open-AIMS/bayesnec/commit/455ca70603a890b26a45b566975f21603f9f87df), [`5e6b41e`](https://github.com/open-AIMS/bayesnec/commit/5e6b41e6845321b5ff1f96c6733d59b6629fb707)).

# bayesnec 1.0.1

- If link functions are not specified in bnec, then the default link function is used; previous versions of bayesnec used the identity link.

- An additional family has been added betabinomial2 for over dispersered binomial data.

- The package supports using link functions for generalized modelling, which appears to be more stable and is also in line with more typical generalised modelling approaches.

- There are multiple options for model weights calculation from the loo package. The default is "pseudobma BB".

- There is now a compare_posterior function that also includes a bootstrapping procedure. This can be used to compare model fits across different datasets, or even different model sets for the same dataset (ie nec v ecx models). Please see the vignette for examples of usage.

- There is a vignette detailing the models available in bayesnec. Note that not all models are suitable for all families, and also depending if link functions are used.

- A new check_chains function has been added to allow chain plotting in base R and that works more smoothly with plotting chains for multiple fits for bayesmanec objects.