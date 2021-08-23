# bayesnec development version (1.1.0)

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
