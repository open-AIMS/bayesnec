# bayesnec 2.2.0

## Breaking changes

- `average_estimates()` is removed. To combine estimates from several fits,
  call `nec()`, `nsec()` or `ecx()` with `posterior = TRUE` on each fit and
  combine the draws (#403).

- `nec()`, `nsec()`, `ecx()`, `ggbnec_data()`, `plot()` and `autoplot()` now
  default to `xform = NULL`, and `summary()` reports on the same scale. Where
  `crf()` transforms the predictor inline, as in `crf(log(concentration))`, an
  estimate is returned on the scale the predictor was recorded on, a
  concentration, where 2.1.3.1 returned it on the transformed scale. Calls that
  pass `xform = exp` return the same values as before. Write
  `xform = identity` to keep the transformed scale. A fit whose predictor is
  an untransformed column is unaffected. See `?nec` (#299).

- `nsec()` and `ecx()` no longer set a posterior draw whose curve does not
  reach the target within the prediction range to the end of that range, and
  `nec()` no longer reports a sampled `nec` beyond that range as a value. Such
  a draw is censored instead. A summary quantile that falls among censored
  draws is reported as the bound, `summary()`, `print()` and the plot
  annotations mark it `>=` or `<=`, and a warning gives the number of draws
  affected. With `posterior = TRUE` a censored draw is `NA`, and
  `attr(, "censored")` records which end of the range it lies beyond, so code
  that post-processes the draws must handle `NA`. A posterior with no
  censored draw is summarised as before. See `?nec` (#39, #395).

- `compare_estimates()` and `compare_posterior()` compare a censored draw
  through its `censored` attribute. `prob_diff` gains `prob_lower` and
  `prob_upper`, and `prob` is `NA` where censoring leaves the order of some
  pairs undetermined, so code reading `prob` must handle `NA` (#404).

- `ecx()`, `nsec()`, `compare_estimates()` and `compare_posterior()` no longer
  take `hormesis_def`. Every ECx and NSEC is measured from the control, the
  predicted mean at the lowest observed concentration, which is what the
  default `hormesis_def = "control"` selected. `ecx()` and
  `nsec()` stop with an error that names the argument; remove it from the
  call. Nothing replaces `hormesis_def = "max"` (#195).

- `ecx(type = "relative")` now measures from the control to the equation's
  lower asymptote, `bot`, or to 0 for an equation without one, and warns that
  its meaning has changed. The quantity 2.1.3.1 reported, from the top of the
  fitted curve to its lowest point, is now `type = "range"`, measured from the
  control. `type = "relative"` is an error for an equation without `bot` on a
  family unbounded below, such as `nec3param` on a gaussian response. The same
  applies to `compare_estimates()` and `compare_posterior()` (#195).

- `bnec()` fits on the identity link unless the call writes a link. A family
  object such as `Beta()`, `Gamma()`, `binomial()` or `poisson()` previously
  kept the link of its constructor (logit, inverse, logit, log), and now fits
  on the identity link, so estimates and default priors change. Write
  `Beta(link = "logit")` to keep the earlier model. A link other than
  identity, log or logit is refused. `update()` applies the same rules to a
  supplied family. See the link section of `?bnec` (#256).

- `bnec()` refuses a `brms` additional-response term other than `trials()`,
  `weights()`, `cens()` and `rate()`. 2.1.3.1 passed such a term on after a
  message. Remove the term, or fit the model with `brms::brm()` (#136).

- `bnec()` refuses a `crf()` predictor written with a formula operator at its
  top level, such as `crf(-x)`, `crf(x - 1)` or `crf(x^2)`. 2.1.3.1 fitted
  such a model on `x` alone while `ecx()` and `nsec()` applied the expression
  to the estimate, so the reported values were wrong. Write
  `crf(I(x - 1))` or compute a column. `ecx()`, `nsec()` and `autoplot()`
  refuse an earlier fit with such a predictor (#438, #479).

- `bnec()`, `bnec_group()`, `get_priors()`, `amend()` and `update()` refuse a
  missing value in any variable the formula names, and report the rows. Apply
  `na.omit()` to the data first. 2.1.3.1 stopped with an unrelated error on a
  bare column and, under an inline transformation, fitted the complete rows
  without saying so (#278).

- A fitted object no longer holds `pred_vals$posterior`, and a model set no
  longer holds `w_pred_vals$posterior`. Obtain the matrix with
  `posterior_epred(fit, newdata = bnec_newdata(fit, resolution = 1000),
  re_formula = NA)`. Objects saved by 2.1.3.1 keep it (#180, #213).

- Group-level deviations on a family whose mean is bounded, fitted on the
  identity link, are applied multiplicatively: on the odds for `Beta`,
  `binomial`, `bernoulli` and `beta_binomial`, and on the value for `Gamma`,
  `poisson` and `negbinomial`. This applies to an `ogl()` deviation, for the
  equations whose curve stays inside the bounds of the family, and to a
  `pgl()`, `(top | g)` or `(bot | g)` term on `top` or `bot`. The standard
  deviation of a term on `top` or `bot` is then reported as
  `sd(topgl_Intercept)` or `sd(botgl_Intercept)`, on the log or log-odds
  scale, so code that reads those draws by name must use the new names. A user
  prior on `top` or `bot` without the bounds of the family is refused where
  such a term applies to it. A data column named `bnecmu`, `ogl`, `topgl`,
  `bnectop`, `botgl` or `bnecbot` is refused. See `?bayesnecformula` (#257,
  #294).

- bayesnec requires brms 2.23.0 or later and imports modelbased. It no longer
  imports evaluate. `bnec_hurdle()` with a count growth family needs brms
  2.23.2 (#249).

## Grouped fits

- `bnec_group()` fits a model set separately at each level of a factor and
  returns a `bayesnecgroupfit`. Its `nec()`, `nsec()` and `ecx()` methods
  return one row for each level, with `bound_` columns marking censored
  entries. It also has `print()`, `summary()`, `plot()`, `autoplot()`,
  `ggbnec_data()`, `curve_params()` and `exceedance()` methods. Each level is
  fitted with a seed derived from `seed` and recorded in `level_seeds`. Under
  a `future` plan the levels or the equations take the workers, whichever
  needs fewer rounds. See `?bnec_group` and `vignette("example8")` (#33,
  #338).

- `crossed_group_weights()` gives the model weight of every combination of
  per-level equations in a `bayesnecgroupfit` (#33).

- `compare_posterior()` is now generic. Its `bayesnecgroupfit` method compares
  the levels of the factor, and a named list of fits is compared as before
  (#33).

- `ggbnec_data()` and `autoplot()` gain `group` and `group_aes`, which join
  the observations of each level of a grouping variable or colour them (#368).

## Hurdle and zero-inflated models

- `bnec()` accepts the two-block families `hurdle_gamma`,
  `zero_inflated_beta`, `hurdle_poisson` and `hurdle_negbinomial`, and the
  single-block families `zero_inflated_poisson` and
  `zero_inflated_negbinomial`. A two-block family fits one curve to the
  positive response and a second, the zero block, to the probability of a
  zero, while a single-block family holds that probability constant. Each
  family must be named in `family`; `bnec()` never chooses one. The new
  `model_survival` argument names the equation for the zero block. The zero
  block is fitted on the identity link, and another link for it is refused.
  `neclin`, `neclinhorme`, `ecxlin`, `nechormepwr` and `nechorme4pwr` are
  excluded for every two-block family, and `nechormepwr01` as well where the
  positive block is a `Gamma` or count response. For a two-block family
  `posterior_epred()` returns the mean of the positive block multiplied by
  the probability of a non-zero response (#104, #173, #209, #249).

- `ecx()` and `nsec()` gain `dpar`, which reports one block of a joint
  two-block fit. Under `dpar = "mu"` a count hurdle reports the mean of the
  positive counts. `type = "relative"` with `dpar = NULL` is refused on a
  two-block fit (#249, #491).

- `bnec_hurdle()` fits a hurdle model as two `bnec()` fits: the growth of the
  units with a non-zero response, and a `bernoulli` model for survival. It
  returns a `bayesnechurdlefit`, whose methods take
  `which = "combined"`, `"growth"` or `"survival"`. The combined NEC is the
  per-draw minimum of the two components. A `disp()` term applies to the
  growth component, and so does a `cens()` term where the growth family is
  continuous. `trials()` and `weights()` are refused, as are `binomial`,
  `beta_binomial`, zero-inflated count and two-block growth families.
  `crossed_weights()` gives the weight of every growth and survival pair, and
  `best_crossed()` names the highest. See `?bnec_hurdle` and
  `vignette("example6")` (#188, #249, #412, #415).

## Joint refits

- `bnec_joint()` refits a factorised fit as one model. For a
  `bayesnechurdlefit` it refits the pair `best_crossed()` names with the
  matching two-block family. For a `bayesnecgroupfit` it composes the equation
  each level favours into one model in which every curve parameter takes a
  value per level, and returns a `bayesnecjointfit` with `nec()`, `nsec()`,
  `ecx()`, `ecnsec()`, `exceedance()`, `autoplot()`, `ggbnec_data()` and
  `print()` methods. With `disp_by_level = TRUE`, the default, the dispersion
  parameter also takes a value per level. Each level is read over its own
  concentrations, with its own lowest concentration as the control.
  `prior_type`, `asymptote_observed` and `predictor_scale` are taken from the
  fit unless supplied. See `?bnec_joint` and `vignette("example8")` (#382,
  #388, #448, #500, #516).

- `nec()` gains `no_effect` for a `bayesnecjointfit`. `TRUE` reports for each
  level the no-effect estimate its own equation supports, the NEC or the NSEC,
  labelled in an `ne_type` column. The other classes refuse the argument
  (#388).

## Estimates beyond the prediction range

- `nec()` and `nsec()` gain `extrapolate`, which widens or removes the bound at
  which a draw is censored. See `?nec` (#392).

- `exceedance()` gives the posterior probability that a NEC, NSEC or ECx lies
  above a threshold concentration, such as a guideline value. Censored draws
  are counted from their `censored` attribute, and the probability is
  reported with the lower and upper limits they allow (#44).

## Diagnostics

- `check_fit()` compares the observed location and spread of the response at
  each predictor value against the values the fit simulates, and gives a
  posterior predictive p-value for each comparison. It has `print()` and
  `plot()` methods (#56, #148).

- `pp_check()` has methods for `bayesnecfit`, `bayesmanecfit` and
  `bayesnechurdlefit` objects. A model set returns one plot per equation
  (#56, #148).

- `check_sampling()` reports the largest Rhat, the smallest effective sample
  size and the number of divergent transitions of each equation, and
  `screen_models()` drops the equations that fail and reports why (#148).

- `summary()` reports the number of divergent transitions of each equation,
  and its printed output warns where one exceeds the new `divergence_cutoff`,
  10 by default (#447).

- `bnec()` reports an equation that does not reproduce the mean or spread of
  the control. `summary()` of a model set does the same, controlled by the new
  `check_fit` and `fit_ratio_cutoff` arguments (#148).

- `failed_models()` returns each equation that `bnec()` or `amend()` could not
  fit, with its error, priors and initial values, and `summary()` lists them
  (#133).

## Priors

- `bnec()` and `amend()` gain `prior_type`. `prior_type = "regularizing"`
  builds narrower default priors than the default `"uninformative"`, centring
  `top` on the response at the lowest concentrations and `bot` on the response
  at the highest. See `?bnec` and `vignette("example3")`.

- `bnec()`, `get_priors()` and `amend()` gain `asymptote_observed`. `FALSE`
  declares that the highest concentration did not reach the lower asymptote.
  It places the central 95 per cent of the `bot` prior between the floor of
  the response and the mean response at the end of the series, removes the
  upper bound of the `nec` and `ec50` priors, and extends the initial-value
  search to the floor. It is refused where the response has no floor, unless
  `prior` supplies `bot`. `bnec()` reports, before fitting, a response still
  declining between the two highest concentrations (#390, #394, #449, #483,
  #516).

- `bnec()`, `get_priors()` and `bnec_group()` gain `predictor_scale`, which
  declares whether the predictor is a concentration or already on the log
  scale for the default `nec` and `ec50` priors. The default `"auto"` treats a
  predictor with a negative value as logged (#317).

- `get_priors()` returns priors in the form `bnec(prior = )` accepts, from a
  fit or from a formula and data before anything is fitted (#141).

- `bnec()` takes `prior` as a named argument, and accepts a prior on the
  family's dispersion parameter (`sigma`, `shape` or `phi`) (#207).

## Dispersion

- `bnec()` accepts a `disp()` term in the formula. `disp(~x)` models the
  dispersion parameter on the predictor, and `disp("power")`,
  `disp("twosided")` and `disp("loglinear")` model it as a function of the
  mean. It is refused for a family with no free dispersion parameter and for
  `hurdle_poisson` and the zero-inflated count families, and a function of the
  mean is refused under a link other than the identity. On `hurdle_gamma`,
  `zero_inflated_beta` and `hurdle_negbinomial` it models the positive block.
  See `?bnec` and `vignette("example9")` (#191, #397, #410, #443, #468,
  #472).

- `dispersion()` accepts a model set, returning one row per equation, and a
  `bayesnechurdlefit`. With `summary = TRUE` it adds `P(>1)`, the posterior
  probability of over-dispersion, and the weights table of `summary()` gains
  `dispersion_P_over_1` (#262).

## Families, formulas and equations

- `bnec()` accepts a `rate()` term for `poisson` and `negbinomial` responses.
  The curve, its estimates and the plots are then per unit of exposure, and
  the default `top` and `bot` priors are built on that scale. The denominator
  must be a column of the data (#136, #389).

- `bnec()` accepts a `cens()` term. A censored observation keeps its recorded
  value rather than being shifted off a boundary, and censoring at a bound the
  family excludes is refused. See `vignette("example1")` (#181).

- `ecxflat` is a new equation whose mean is constant over the predictor. It
  has no `nec` parameter, so its no-effect estimate is an NSEC, and every ECx
  it gives is censored above the prediction range (#419).

- `bayesnecformula()` and `bnf()` gain `env`, the environment in which a
  formula given as a string is resolved (#319).

- `models()` gains `max_pars`, the largest number of curve parameters an
  equation may have.

- `make_brmsformula()` gains `family`, used to add the second block of a
  two-block family.

## Other new features

- `predict()`, `plot()` and `autoplot()` on a model set gain `model`, which
  names equations to show alongside or instead of the model average, and
  `average`. `predict()` then returns a list. `all_models` is deprecated:
  `all_models = TRUE` still works in `plot()` and `autoplot()`, with a
  warning, and is written `model = fit$success_models, average = FALSE`
  (#120).

- `bnec_record()` reports the equations requested, those attempted, those
  excluded with the reason, and any change made to the response before
  fitting (#93, #261).

- `bnec_version()` reports the bayesnec version, its source and commit, and
  the R, brms and Stan interface versions that fitted each equation of an
  object. Objects fitted earlier report `"unknown"` (#464).

- `pull_best()` returns the equation of a model set with the most model weight
  as a `bayesnecfit` (#324).

- `curve_params()` reports the curve parameters of each equation with their
  credible intervals and model weight (#297).

- `ecnsec()` reports the percent effect at a given NSEC, comparing each
  posterior draw of the control with the same draw of the curve. It refuses a
  value outside the prediction range (#195, #437, #488).

- `amend()` has a method for `bayesnecfit`. Adding an equation returns a model
  set (#176).

- `bnec()` and `amend()` fit the equations of a model set in parallel under
  the `future` plan in force. Without a plan they are fitted in sequence, as
  before (#184, #216).

- `bnec()` and `amend()` gain `timeout`, a time limit for each equation, after
  which it is recorded as failed. A finite limit needs the R.utils package
  (#157).

- New datasets: `alga`, `coral_colour`, `coral_pam`, `lum31` and `nassarius`
  (#6, #33, #173, #181, #301).

## Changed defaults and behaviour

- The default `nec` and `ec50` prior is now a lognormal centred on the median
  of the log of the distinct positive concentrations, or a normal for a
  predictor already on the log scale, so replicates no longer shift it. It is
  still bounded to the tested range. Default fits therefore give different
  estimates from 2.1.3.1 (#269, #302, #314, #483).

- The default `top` and `bot` priors for a `poisson` or `negbinomial`
  response that contains zeros are scaled from its positive values. A
  response with no positive value is refused (#210, #232).

- `bnec()` sets default priors on group-level standard deviations and on the
  `ogl()` intercept, which 2.1.3.1 left at the brms defaults (#245, #257).

- A user prior that omits a parameter is completed from the default priors,
  with a warning naming each parameter filled, rather than leaving it on the
  flat prior of brms (#207).

- `bnec()` fits a model set with a group-level term at `adapt_delta = 0.99`
  where the family bounds the mean and a group-level term could take it
  outside that bound. A value supplied in `control` takes precedence (#257,
  #294).

- The initial-value search accepts each chain separately, checks the curve
  against a band built from the level means, and re-draws a single parameter
  when a full draw fails. A fit with the same seed therefore starts from other
  initial values than under 2.1.3.1, and fewer fits fall back to Stan's random
  initialisation. A search still running after 20 seconds says so (#162,
  #266, #309).

- The default model set and the `"ecx"` and `"decline"` groups include
  `ecxflat`, so model weights and model-averaged estimates change (#485).

- A gaussian model set keeps the equations of the `zero_bounded` group, which
  2.1.3.1 excluded, so a default gaussian fit includes eleven more equations
  (#206).

- `nechormepwr` and `nechorme4pwr` are excluded for `bernoulli`, `binomial`,
  `beta_binomial` and `Beta` responses on the identity link, and
  `ecxhormebc5` for a predictor with negative values on a family whose mean
  cannot be negative (#177, #344).

- `bnec()` and `bnec_group()` fit `ecxflat` alone, with a message, to a
  `bernoulli`, `binomial`, `beta_binomial` or `Beta` response at one bound in
  every observation. A `Beta` response of 0 throughout is refused
  (#400, #419, #441).

- `bnec()` no longer replaces a zero in the predictor with a tenth of the
  smallest positive value. The control is fitted at 0, which changes the
  estimates of such fits. `c()` refuses to combine a fit from 2.1.3.1 with a
  corrected zero and a new fit of the same data; refit it (#269).

- `ecx()` and `nsec()` measure every estimate from the control. They find
  where the curve reaches the target by interpolation rather than at the
  nearest grid point, and their default `resolution` is 200 instead of 1000.
  `ecx()` accepts any `ecx_val` above 0, and no longer refuses an absolute
  ECx of a gaussian equation without `bot`. The `ecnsec_relativeP` attribute
  of `nsec()` is now the percent effect measured from the control (#39, #195,
  #206, #325, #395).

- `rhat()` uses a cutoff of 1.01 instead of 1.05 and ignores the `prior_*`
  draws, and `summary()` of a model set flags an equation by the same rule,
  set by its new `rhat_cutoff` argument (#148).

- Model-averaged estimates and predictions are drawn once when a model set is
  built and recorded in `w_draw_index`, so `fitted()`, `posterior_epred()`,
  `ecx()` and `nsec()` give the same result on every call (#216).

- `compare_estimates()`, `compare_fitted()`, `compare_posterior()` and
  `sample_priors()` gain `seed`, 10 by default, and leave the session's
  random number stream unchanged, so a preceding `set.seed()` no longer
  changes their results. `compare_estimates()` and `compare_fitted()` pair a
  random subset of the longer posterior rather than its first draws. Results
  differ from 2.1.3.1 (#218, #343).

- `amend()`, `update()`, `pull_out()` and `c()` reuse the `x_range`,
  `resolution`, `sig_val` and `loo_controls` a model set was fitted with,
  where 2.1.3.1 reset them to the defaults. An equation added by `amend()`
  takes the set's `prior`, `prior_type`, `asymptote_observed`,
  `predictor_scale`, `control` and `seed`. `pull_out()` gains `x_range`,
  `resolution` and `sig_val`. A set fitted by 2.1.3.1 records none of these
  settings, so the defaults still apply to it (#489, #522).

- `bnec()` reports, once per call, each value of the response it shifts off a
  boundary of the family (#93).

- `bnec()` reports a response that appears normalised to the control or to
  its maximum (#173).

- `bnec()`, `amend()` and `update()` refuse a `resolution` below 2, and an
  `x_range` with one end missing (#211, #325).

- A fit no longer stores the environment its formula was written in, only the
  objects the formula uses that the data do not supply, so saved fits are
  smaller (#329).

## Bug fixes

- `bnec()` excludes, before fitting, an equation left with no group-level term
  on a grouping variable the formula names, and records why. 2.1.3.1 fitted it
  and then stopped after sampling with "Group-level variable(s) ... not found
  in dataset" (#470).

- `bnec()` drops a parameter an equation lacks from every `(par | group)` term
  that names it, not only the first (#470).

- `ogl()` and `pgl()` fit `a/b`, `a:b` and `a*b` as the nested or crossed
  groupings written, rather than as separate terms on `a` and `b` (#466).

- `check_formula()` refuses a missing grouping variable in any `(par | group)`
  term, not only the first (#466).

- `bnec()` accepts a user prior on a group-level standard deviation or on
  `ogl()`, which the initial-value search of 2.1.3.1 refused (#245).

- `bnec()` accepts a `lognormal` or `constant()` prior on a curve parameter,
  and `sample_priors()` draws from both (#84, #244, #302).

- `sample_priors(plot = NA)` returns the draws, as documented (#244).

- A prior taken from a fitted object, whose bounds are stored as `""`, no
  longer stops the initial-value search (#141).

- `set.seed()` before `bnec()` makes the fit repeatable, and `seed = NA` is
  accepted (#310).

- `bnec()` clamps a `bernoulli`, `binomial`, `beta_binomial` or `Beta`
  response of exactly 0 or 1 inside the unit interval when it searches for
  initial values on the identity link, as it already did on the logit link
  (#162).

- `bnec()` finds a model set or a predictor transformation defined in the
  calling function or chunk (#319).

- `bnec()` keeps a boundary shift of the response where `crf()` transforms the
  predictor, and refuses a transformed response that sits on a boundary
  (#258).

- `bnec()` uses the intended number of trials where `trials()` contains
  arithmetic; 2.1.3.1 applied it twice.

- `bnec()` fits an integer response with negative values as gaussian without a
  `family` argument (#272).

- `bnec()` with `family = "beta"` fits a Beta model, and a string naming a
  function that is not a family is refused (#256).

- `bnec()` refuses a `binomial` or `beta_binomial` formula without `trials()`
  once, naming the missing term (#442).

- `bnec()` refuses a column name that is not syntactic, and names its
  `make.names()` form (#398).

- `bnec()` no longer gives two warnings from `min()` when it builds the
  priors of a gaussian response with no positive value (#210, #229).

- `ecx()`, `nsec()` and the stored NSEC apply arithmetic inside an inline
  transformation, such as `crf(log(x + 1))`, to the estimate correctly (#196).

- `crf((log(x)))` and `crf(sqrt(log(x) + 1))` are read as written, and a
  predictor wrapped in `I()` compiles (#479).

- `update()` keeps the fitted family when given new data, and treats a family
  equal to the fitted one as unchanged. 2.1.3.1 asked for `force_fit = TRUE`
  on such calls even where the family was unchanged. `update()` also fits the
  boundary-corrected response it checked (#256, #274, #435).

- `update()` on a model set with one equation left returns a `bayesnecfit`
  (#288).

- `amend(add = )` works on a `Gamma` fit, excluding the equations `Gamma`
  cannot fit on the identity link (#436).

- `amend(add = )` samples an added equation with the set's backend, algorithm
  and `save_pars`, and with a named list in `priors` gives each added
  equation its own element (#489).

- `amend()`, `c()` and `+` recompute the LOO and WAIC criteria of every
  equation, so the weights of a combined set come from the same settings
  (#522).

- `c()`, `+` and `update()` weight a model set by pseudo-BMA, as `bnec()`
  does; 2.1.3.1 used stacking (#320).

- The `sample_size` of a model set counts the draws each equation holds where
  `thin` does not divide the post-warmup iterations (#477).

- `fitted()`, `predict()`, `posterior_epred()` and `posterior_predict()` on a
  model set return one row for a one-row `newdata`.

- `summary(ecx = TRUE)` reads each ECx over the fit's prediction range and
  passes `x_range`, `resolution` and `xform` to `ecx()` (#395, #439).

- `dispersion()` uses the link the model was fitted with, rather than the
  family's default link, which understated the dispersion of a `poisson` fit.
  It reports a value where a fitted variance is zero, and leaves the
  session's random number stream where it was (#247, #298, #337).

- `models()` accepts every model group, including `"decline"` and
  `"hormesis"`. Given a numeric range it returns the equations `bnec()` fits
  for that range, and it gives an informative error for input it cannot map
  (#170).

- `plot()` and `autoplot()` draw the NEC and EC10 annotations at the matching
  point of the curve where the predictor is transformed, apply `xform` to the
  predictor axis whenever the predictor itself is untransformed, and order
  the interval correctly under a decreasing `xform` (#160, #161, #268, #417).

- `plot()` and `autoplot()` annotate the same EC10 for a gaussian fit, and
  `autoplot(ecx = TRUE)` no longer fails for a gaussian equation without
  `bot`.

- `autoplot()` on a model set with `multi_facet = FALSE` honours
  `plot = FALSE` and restores the page prompt, and no longer gives the
  tidyselect deprecation warning (#444).

- `plot()` accepts `position_legend` as x and y coordinates, as documented,
  and refuses an invalid keyword (#278).

- `ggbnec_data()` refuses an `xform` that is not a function on a model set,
  as it does on a single fit (#278).

- `check_priors()` on a model set restores the page prompt and closes its PDF
  device on exit (#459).

- extraDistr is listed in Suggests, since brms needs it to evaluate a
  `beta_binomial` likelihood.

## Documentation and vignettes

- New vignettes: `vignette("example6")` on hurdle and zero-inflated models,
  `vignette("example7")` on growth data and other responses that can be
  negative, `vignette("example8")` on grouping and factor covariates, and
  `vignette("example9")` on a complete analysis workflow (#175, #193, #219,
  #301, #367).

- `vignette("example5")` is rewritten as "Installation and setup", and
  `vignette("example1")`, `vignette("example2")` and the README refer to it
  (#342).

- `vignette("example1")` gains sections on the scale of the predictor,
  preparing the response, censoring and non-constant dispersion (#173, #181,
  #191, #193, #194).

- `vignette("example2")` demonstrates `screen_models()` (#248).

- `vignette("example2b")` and `?models` state that `NEC.3()` and `NEC.4()` in
  the drc package correspond to `nec3param` and `nec4param`, and that
  `NEC.2()` has no counterpart (#139).

- `?models` and `?bnec` state that a model group names a response shape, and
  which equations each family excludes (#170, #285).

- `?autoplot` states that a model set plotted with `multi_facet = FALSE`
  returns a list of plots (#459).

- Vignette figures are PNG files, which render in the HTML vignettes (#178).

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
