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
