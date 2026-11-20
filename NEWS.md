# bayesnec 3.0.0

## Breaking changes

### Removed functions, methods and arguments

- `average_estimates()` is removed. To combine estimates from several fits,
  call `nec()`, `nsec()` or `ecx()` with `posterior = TRUE` on each fit and
  combine the draws (#403).

- `nsec()` no longer has methods for `brmsfit` and `drc` objects. Use
  `toxval::nsec()` for those fits (#555).

- `ecx()`, `nsec()`, `compare_estimates()` and `compare_posterior()` no longer
  take `hormesis_def`. Every ECx and NSEC is measured from the control, which
  is what the default `hormesis_def = "control"` selected. `ecx()`, `nsec()`,
  `nec()`, `ecnsec()` and `compare_posterior()` stop with an error that names
  the argument, and `compare_estimates()` with "unused argument"; remove it
  from the call. Nothing replaces `hormesis_def = "max"`. Its removal changes
  the position of the arguments after it in `ecx()`, `nsec()` and
  `compare_estimates()`, so a call that passes them by position must name them
  (#195).

### Changed defaults

- `nec()`, `nsec()`, `ecx()`, `ggbnec_data()`, `plot()` and `autoplot()`
  default to `xform = NULL`, and `summary()` reports on the same scale. Where
  `crf()` transforms the predictor inline, as in `crf(log(concentration))`, an
  estimate is returned on the scale the predictor was recorded on, a
  concentration, where 2.1.3.1 returned it on the transformed scale.
  `compare_estimates()` and `compare_posterior()` compare on the same scale.
  Calls that pass `xform = exp` return estimates on the scale they did
  before. Write `xform = identity` to keep the transformed scale. A fit whose
  predictor is an untransformed column is unaffected. See `?nec` (#299).

- The default `resolution` of `ecx()` and `nsec()` is 500 instead of 1000,
  and that of `compare_estimates()` 500 instead of 100. `ecx()` and `nsec()`
  interpolate between grid points to find the concentration at which the
  curve crosses the target. The grid `bnec()` stores with a fit is unchanged
  (#325, #527, #578).

- `bnec()` fits on the identity link unless the call writes a link or passes
  a family object held in a variable, which keeps its links. A family
  object such as `Beta()`, `Gamma()`, `binomial()` or `poisson()` kept the link
  of its constructor in 2.1.3.1 (logit, inverse, logit, log), and now fits on
  the identity link, so estimates and default priors change. Write
  `Beta(link = "logit")` to keep the earlier model. `update()` applies the same
  rules to a supplied family. See the link section of `?bnec` (#256).

- `rhat()` uses a cutoff of 1.01 instead of 1.05 and ignores the `prior_*`
  draws (#148).

- `compare_estimates()`, `compare_fitted()`, `compare_posterior()` and
  `sample_priors()` gain `seed`, 10 by default, and leave the session's random
  number stream unchanged, so a preceding `set.seed()` no longer changes their
  results. `compare_estimates()` and `compare_fitted()` pair a random subset of
  the longer posterior rather than its first draws. Results differ from
  2.1.3.1 (#218, #343).

- `amend()`, `update()`, `pull_out()` and `c()` reuse the `x_range`,
  `resolution`, `sig_val` and `loo_controls` a model set was fitted with,
  where 2.1.3.1 reset them to the defaults. An equation added by `amend()`
  takes the set's `prior`, `prior_type`, `asymptote_observed`,
  `predictor_scale`, `control` and `seed`. A set fitted by 2.1.3.1 records none
  of these settings, so the defaults still apply to it (#489, #522).

### Changed estimates for the same call

- For a posterior draw whose curve does not reach the target within the
  prediction range, `nsec()` and `ecx()` no longer return the grid point
  nearest the target, and `nec()` no longer reports a sampled `nec` beyond
  that range as a value. Such a draw is censored instead. A summary quantile
  that falls among censored draws is reported as the bound, `summary()` and
  the plot annotations mark it `>=` or `<=`, and a warning gives the number of
  draws affected. Once any draw is censored, the quantiles are order
  statistics of the draws. With `posterior = TRUE`, a draw censored above
  the search range is `Inf` and one censored below it is `-Inf`, and a
  curve-read draw stored in a fit's `ne_posterior` or `w_ne_posterior` takes
  the same form. The `attr(, "censored")` record retains the bounds and
  affected draws. These infinite values keep their positions in `sort()` and
  `quantile()`. A posterior with no censored draw is summarised as before. See
  `?nec` (#39, #395).

- `ecx()` and `nsec()` measure every estimate from the control, the predicted
  mean at the lowest observed concentration or at the `x_control` given to
  `bnec()`, and take the first concentration at or above the control where a
  draw's curve crosses the target. 2.1.3.1 measured `type = "absolute"` from
  the highest point of the curve, read the control at the first point of the
  prediction grid, so that `x_range` changed it, and took the grid point
  nearest the target anywhere in the range. An NSEC draw already past the
  reference at the control takes a later decreasing crossing where there is
  one; otherwise it takes the control concentration, or is censored above the
  range where the curve later rises above the reference (#195, #527, #586,
  #589).

- `ecx(type = "relative")` measures from the control to the equation's lower
  asymptote, `bot`, taken on the response scale under a log or logit link, and
  warns that its meaning has changed. For an equation without `bot` it
  measures towards 0, the value the mean declines to under each link. The
  quantity 2.1.3.1 reported,
  from the top of the fitted curve to its lowest point, is now `type =
  "range"`, measured from the control. `type = "relative"` is an error for an
  equation without `bot` on a family unbounded below, such as `nec3param` on a
  gaussian response. The same applies to `compare_estimates()` and
  `compare_posterior()` (#195, #578).

- The `ecnsec_relativeP` attribute of `nsec()` is the percent effect at the
  NSEC measured from the control (#195).

- `nec()`, `summary()` and the plots report the NSEC as the no-effect estimate
  of the six hormesis threshold equations, `nechorme`, `nechorme4`,
  `neclinhorme`, `nechormepwr`, `nechorme4pwr` and `nechormepwr01`, where
  2.1.3.1 reported their `nec` parameter. That parameter is the concentration
  at which the decline term starts, where the response can still be above the
  control, and `curve_params()` returns it. The model-averaged N(S)EC changes
  wherever these equations contribute. `nec()` still returns the sampled `nec`
  of `nec3param`, `nec4param`, `neclin` and `necsigm` (#570).

- For a group-level term whose levels span concentrations, `nec()`,
  `nsec()` and `ecx()` read the curve of each observed level, with its fitted
  deviation, and by default pool the levels as an observation-weighted
  mixture. 2.1.3.1 read the population curve, with the deviations at zero.
  See "Estimates from observed group levels" below (#554, #557).

- The default `nec` and `ec50` prior is a lognormal centred on the median of
  the log of the distinct positive concentrations, or a normal for a predictor
  already on the log scale, so replicates no longer shift it. It is still
  bounded to the tested range (#269, #302, #314, #483).

- The default `top` and `bot` priors for a `poisson` or `negbinomial`
  response that contains zeros are scaled from its positive values (#210,
  #232).

- `bnec()` sets default priors on group-level standard deviations and on the
  `ogl()` intercept, which 2.1.3.1 left at the brms defaults (#245, #257).

- The initial-value search accepts each chain separately, checks the curve
  against a band built from the level means, and re-draws a single parameter
  when a full draw fails. It rejects a draw at which an `exp()` in the curve
  would overflow, keeps refined values inside the prior bounds, and stops
  with an error after a set number of redraws. A fit with the same seed
  therefore starts from other initial values than under 2.1.3.1 (#162, #266,
  #309, #578).

- `bnec()` no longer replaces a zero in the predictor with a tenth of the
  smallest positive value. The control is fitted at 0, which changes the
  estimates of such fits (#269).

- The default model set and the `"ecx"` and `"decline"` groups include
  `ecxflat`, a new equation whose mean is constant, so model weights and
  model-averaged estimates change (#419, #485).

- A gaussian model set keeps the equations of the `zero_bounded` group, which
  2.1.3.1 excluded, so a default gaussian fit includes eleven more equations
  (#206).

- On the identity link, `nechormepwr` and `nechorme4pwr` are excluded for
  `bernoulli`, `binomial`, `beta_binomial` and `Beta` responses, and
  `ecxhormebc5` for a predictor with negative values on a family whose mean
  cannot be negative (#177, #344).

- Where each level of a group-level term is observed at a single
  concentration, such as `ogl(conc_group)` on `lum31`, model weights are
  computed by leaving out each level whole rather than each observation.
  `loo_controls = list(leave_out = "observation")` restores the earlier score
  (#529, #531).

- Model-averaged estimates and predictions are drawn once when a model set is
  built and recorded in `w_draw_index`, so `fitted()`, `posterior_epred()`,
  `ecx()` and `nsec()` give the same result on every call (#216).

- `bnec()` fits an equation with a group-level term at `adapt_delta = 0.99`
  where the family bounds the mean and that term could take it outside the
  bound. A value supplied in `control` takes precedence (#257, #294).

- Group-level deviations on a family whose mean is bounded, fitted on the
  identity link, are applied multiplicatively: on the odds for `Beta`,
  `binomial`, `bernoulli` and `beta_binomial`, and on the value for `Gamma`,
  `poisson` and `negbinomial`. This applies to an `ogl()` deviation, for the
  equations whose curve stays inside the bounds of the family, and to a
  `pgl()`, `(top | g)` or `(bot | g)` term on `top` or `bot`. The standard
  deviation of a term on `top` or `bot` is then reported as
  `sd(topgl_Intercept)` or `sd(botgl_Intercept)`, on the log or log-odds
  scale, so code that reads those draws by name must use the new names. See
  `?bayesnecformula` (#257, #294).

- A user prior that omits a parameter is completed from the default priors,
  with a warning naming each parameter filled, rather than leaving it on the
  flat prior of brms (#207).

- `bnec()` and `bnec_group()` fit `ecxflat` alone, with a message, to a
  `bernoulli`, `binomial`, `beta_binomial` or `Beta` response at one bound in
  every observation (#400, #419, #441).

- `manec_example` is refitted under this release from
  `data-raw/manec_example.R`, with two chains of 1200 post-warmup draws and
  seed 10, where the 2.1.3.1 object held two chains of 50. Both equations pass
  `screen_models()` at its defaults, `bnec_version()` returns the installation
  that fitted it, and the object no longer stores the compiled Stan model.
  Values computed from it differ from those of the 2.1.3.1 object (#579).

### Corrected results

The defects of 2.1.3.1 below are corrected, and each correction changes what
the same call returns.

- `ogl()` and `pgl()` fit `a/b`, `a:b`, `a*b` and a `gr()` grouping as
  written, rather than as separate terms on each variable they name. A
  `gr()` grouping with `by`, such as `pgl(gr(plate, by = toxicant))`, fits
  one standard deviation for each level of `by`. A `gr()` grouping in a
  `(par | group)` term is refused with a message naming the `pgl()` and
  `ogl()` form (#466, #550).

- `bnec()` keeps a boundary shift of the response where `crf()` transforms the
  predictor, and refuses a transformed response that sits on a boundary
  (#258).

- `bnec()` uses the intended number of trials where `trials()` contains
  arithmetic; 2.1.3.1 applied it twice.

- `ecx()`, `nsec()` and the stored NSEC apply arithmetic inside an inline
  transformation, such as `crf(log(x + 1))`, to the estimate correctly (#196).

- `amend()`, `c()` and `+` recompute the LOO and WAIC criteria of every
  equation, so the weights of a combined set come from the same settings
  (#522).

- The `sample_size` of a model set counts the draws each equation holds where
  `thin` does not divide the post-warmup iterations (#477).

- `fitted()`, `predict()`, `posterior_epred()` and `posterior_predict()` on a
  model set return one row for a one-row `newdata`, where 2.1.3.1 returned
  one for each point of the stored grid, with a warning.

- `summary(ecx = TRUE)` reads each ECx over the fit's prediction range and
  passes `x_range`, `resolution` and `xform` to `ecx()` (#395, #439).

- `dispersion()` uses the link the model was fitted with, rather than the
  family's default link, so its value changes for a `poisson` or binomial fit
  on the identity link. It returns a value where a fitted variance is zero,
  and leaves the session's random number stream where it was (#247, #298,
  #337).

- `models()` given a numeric range returns the equations `bnec()` fits for
  that range: all 24 for `c(-Inf, Inf)`, where 2.1.3.1 returned 12 (#170).

- `plot()` and `autoplot()` draw the NEC and EC10 annotations at the matching
  point of the curve where the predictor is transformed, apply `xform` to the
  predictor axis whenever the predictor itself is untransformed, and order
  the interval correctly under a decreasing `xform` (#160, #161, #268, #417).

- `plot()` and `autoplot()` annotate the same EC10 on a gaussian fit,
  measured with `type = "range"`. 2.1.3.1 annotated a relative ECx in
  `plot()` and an absolute one in `autoplot()`.

- `c()`, `+` and `update()` weight a model set by pseudo-BMA, as `bnec()`
  does; 2.1.3.1 used stacking (#320).

### Changed return values

- A fitted object no longer contains `pred_vals$posterior`, and a model set
  no longer contains `w_pred_vals$posterior`. Obtain the matrix with
  `posterior_epred(fit, newdata = bnec_newdata(fit, resolution = 1000),
  re_formula = NA)`. Objects saved by 2.1.3.1 keep it (#180, #213).

- `compare_estimates()` and `compare_posterior()` compare a censored draw
  through its `censored` attribute. `prob_diff` gains `prob_lower`,
  `prob_upper`, `censored_first` and `censored_second`, and `diff_data` gains
  `positive` and `indeterminate`. `prob` is `NA` where censoring leaves the
  order of some pairs undetermined, so code reading `prob` must handle `NA`
  (#404).

- `dispersion(summary = TRUE)` returns a fourth element, `P(>1)`, the
  posterior probability of over-dispersion (#262).

- The `"toxicity_estimate"` attribute of a `nec()` result is `"nec"` where
  every draw is a sampled NEC, `"nsec"` where every draw is an NSEC, and
  `"n(s)ec"` where a model set or a hurdle fit combines the two. 2.1.3.1 set
  it to `"nec"` whatever the equation (#570).

### Newly refused input

- `bnec()` refuses a `brms` additional-response term other than `trials()`,
  `weights()`, `cens()` and `rate()`. 2.1.3.1 passed such a term on after a
  message. Remove the term, or fit the model with `brms::brm()` (#136).

- `bnec()` refuses a `crf()` predictor written with a formula operator at its
  top level, such as `crf(-x)`, `crf(x - 1)` or `crf(x^2)`. 2.1.3.1 fitted
  such a model on `x` alone while `ecx()` and `nsec()` applied the expression
  to the estimate, so the reported values were wrong. Write
  `crf(I(x - 1))` or compute a column. `ecx()`, `nsec()` and `autoplot()`
  refuse an earlier fit with such a predictor (#438, #479).

- `bnec()`, `bnec_group()`, `bnec_hurdle()`, `get_priors()` and `update()`
  refuse a missing value in any variable the formula names, and report the
  rows. Apply `na.omit()` to the data first. 2.1.3.1 stopped with an
  unrelated error on a bare column and, under an inline transformation,
  fitted the complete rows (#278).

- `bnec()` refuses a link other than identity, log or logit (#256).

- `bnec()` refuses a data column named `bnecmu`, `ogl`, `topgl`, `bnectop`,
  `botgl` or `bnecbot`, and a user prior on `top` or `bot` without the bounds
  of the family where a group-level term applies to it (#257, #294).

- `bnec()` and `amend()` refuse, before fitting, a prior that is not a
  `brmsprior` or a named list of them with unique names. 2.1.3.1 replaced such
  a prior with the defaults without a message (#543, #552).

- `bnec()`, `bnec_group()`, `amend()` and `update()` check the data once
  before fitting. They refuse a value of the predictor or response that is
  not finite, a `rate()` denominator of zero or less, a predictor with a
  single value and data with no rows, and under `binomial` or
  `beta_binomial` trials that are not a positive whole number or are fewer
  than the response. `bnec_group()` refuses a level observed at a single
  concentration, and warns of one observed at two (#578).

- `bnec()`, `bnec_group()`, `amend()` and `update()` refuse, before fitting,
  a `sig_val` outside (0, 1), a `resolution` that is not a whole number of at
  least 2, an `x_range` that is not `NA` or an increasing pair, and an
  equation name that is not one of the 24. `x_var`, `y_var`, `trials_var`,
  `model`, `random` and `random_vars`, removed in 2.0, are refused where
  2.1.3.1 ignored them with a warning, and so is a second `crf()` term, which
  2.1.3.1 ignored. A `crf()` term without an equation is refused (#211, #325,
  #578).

- `nsec()` refuses a `sig_val` outside (0, 1). `ecx()`, `nsec()` and
  `ecnsec()` refuse a `resolution` that is not a whole number of at least 2,
  `ecx()` refuses a missing or vector `ecx_val`, and all four estimators
  refuse a `posterior` that is not `TRUE` or `FALSE`.
  `nsec()` refuses `type` and any other argument it does not use, which
  2.1.3.1 ignored. `nec()` says that it does not use `sig_val`,
  `resolution` or `x_range`, and its refusal of a smooth equation points to
  `nsec()`. `bnec_newdata()` refuses a `resolution` that is not a whole
  number (#578).

- `compare_posterior()` refuses an argument in `...` except under
  `comparison = "fitted"`, which 2.1.3.1 ignored. `compare_estimates()`,
  `compare_posterior()` and `compare_fitted()` refuse a single fit or a list
  of fewer than two fits. `compare_estimates()` and `compare_posterior()`
  refuse an unknown `comparison` and a NEC comparison of a smooth equation,
  and `compare_fitted()` an `ndraws` or `draw_ids` outside the draws (#578).

- `pull_out()` refuses an argument in `...`, which 2.1.3.1 ignored (#578).

- `bnec()` refuses a name in `loo_controls$weights` that the weighting
  method does not read, which 2.1.3.1 ignored (#578, #583).

- `bnec()` refuses a `poisson` or `negbinomial` response with no positive
  value, and a `Beta` response of 0 in every observation (#210, #232, #400).

- `autoplot()` and `ggbnec_data()` refuse `dpar`, and a `which` they cannot
  draw. 2.1.3.1 ignored both, or stopped with "unused argument" under
  `ecx = TRUE` (#564, #565).

- `c()` refuses to combine a fit from 2.1.3.1 whose zero predictor was
  replaced with a new fit of the same data; refit it (#269).

### Dependencies

- bayesnec requires brms 2.23.0 or later, and no longer imports evaluate
  (#249, #578).

## Grouped fits

- `bnec_group()` fits a model set separately at each level of a factor and
  returns a `bayesnecgroupfit`. Its `nec()`, `nsec()`, `ecx()`, `ecnsec()`,
  `exceedance()` and `curve_params()` methods return one row for each level,
  with `bound_` columns marking censored entries, and take `pool` to combine
  the levels. It also has `print()`, `summary()`, `plot()`, `autoplot()`,
  `ggbnec_data()`, `amend()`, `compare_posterior()` and `failed_models()`
  methods, and `dispersion()` refuses it in favour of a call on each level.
  `bnec_newdata()`, `fitted()`, `predict()`, `posterior_epred()`,
  `posterior_predict()`, `pp_check()`, `rhat()`, `check_chains()`,
  `check_priors()`, `pull_brmsfit()`, `pull_best()`, `get_priors()`,
  `formula()` and `model.frame()` accept a checked `level` argument.
  Without it, matrices, plots, priors and brms fits are named by level;
  tables with matching columns include a `level` column. `update()` refits
  every level, while `c()` and `+` combine matching grouped fits within
  each level. `check_sampling()`, `group_level_sd()` and `pull_prior()`
  also report results by level.
  With `family = NULL` the family is chosen once from the whole response. A
  numeric grouping column, fewer than two levels, or a level with fewer than
  four observations is refused before fitting. Each level's seed is derived
  from a `seed` passed through `...`, or else from the session's random number
  stream, and is stored in `level_seeds`. Under a `future` plan the levels or
  the equations take the workers, whichever needs fewer rounds. See
  `?bnec_group` and the article
  [Grouping and factor covariates](https://open-aims.github.io/bayesnec/articles/example8.html) (#33, #338, #557).

- `check_fit()` on a `bayesnecgroupfit` checks each level and returns a
  `checkfit_group` object, with `print()` and `plot()` methods (#562, #563).

- `crossed_group_weights()` gives the model weights of each level of a
  `bayesnecgroupfit`, the best combination of per-level equations with its
  weight, and the weights of the equations common to every level. Given a
  pooled fit, it compares the grouped and pooled fits on the
  leave-one-group-out score, as LOOIC, where that score was computed for
  every fit over the same levels, and on WAIC otherwise. It refuses a set
  weighted by stacking (#33, #531).

- `compare_posterior()` accepts a `bayesnecgroupfit`, whose levels it
  compares, as well as a named list of fits (#33).

- `group_level_sd()` tabulates the group-level standard deviations of each
  equation, such as those of an `ogl()` or `pgl()` term, beside the
  equation's model weight. It accepts a `bayesnecfit`, a `bayesmanecfit`, a
  `bayesnecgroupfit`, with a `level` column, and a `bayesnechurdlefit`, with
  one table per component (#532).

- `ggbnec_data()` gains `group`, and `autoplot()` gains `group` and
  `group_aes`, which joins the means of each level of a grouping variable
  or colours its observations (#368).

## Estimates from observed group levels

- For a group-level term whose levels span concentrations, `nec()`, `nsec()`,
  `ecx()` and `ecnsec()` read the curve of each observed level, including its
  fitted deviation. A level observed at only one concentration is excluded,
  with a message naming it. This applies to single fits, model sets, hurdle
  fits and joint refits, and to the no-effect estimate stored at fitting
  (#554, #557).

- `pool` chooses how the levels are combined: `"none"` returns per-level
  estimates, `"mixture"` gives each posterior draw to one level in proportion
  to its observation count, `"mean_estimate"` averages the levels' estimates
  at each draw, and `"mean_curve"` estimates from their weighted mean curve.
  A joint refit and a grouped fit default to `"none"`, and a single fit,
  model set or hurdle fit with a spanning group term to `"mixture"`. `nec()`
  refuses `"mean_curve"`. `x_control = "own"`, the default, reads each level
  from the control recorded at fitting, or else from its lowest observed
  concentration, `"shared"` reads every level from the lowest control, and a
  number gives the control concentration, with a warning where it
  extrapolates a level's curve and a message where it replaces the lower end
  of `x_range` (#557, #578).

- The curve and credible band of `autoplot()` and `ggbnec_data()` remain the
  population curve, with the group deviations at zero, so the estimate lines
  need not fall where that curve equals the corresponding response (#576).

## Hurdle and zero-inflated models

- `bnec()` accepts the two-block families `hurdle_gamma`,
  `zero_inflated_beta`, `hurdle_poisson` and `hurdle_negbinomial`, and the
  single-block families `zero_inflated_poisson` and
  `zero_inflated_negbinomial`. A two-block family fits one curve to the
  positive response, on the untruncated mean for a count hurdle, and a second,
  the zero block, to the probability of a non-zero response, while a
  single-block family holds that probability constant. Each family must be
  named in `family`; `bnec()` never chooses one. The new `model_survival`
  argument names the equation for the zero block. The zero block is fitted on
  the identity link, and another link for it is refused. `neclin`,
  `neclinhorme`, `ecxlin`, `nechormepwr` and `nechorme4pwr` are excluded for
  every two-block family, and `nechormepwr01` as well where the positive block
  is a `Gamma` or count response on the identity link. For a two-block family
  `posterior_epred()` returns the mean of the positive block multiplied by the
  probability of a non-zero response. A `zero_inflated_beta` response at one
  bound in every observation is treated as a `Beta` response is (#104, #173,
  #209, #249).

- `ecx()` and `nsec()` gain `dpar`, which reports one block of a joint
  two-block fit. Under `dpar = "mu"` a count hurdle reports the mean of the
  positive counts. On a two-block fit `ecx(type = "relative")` without `dpar`
  is refused, and so is `ecnsec(type = "relative")` (#249, #491).

- `autoplot()` and `ggbnec_data()` on a two-block fit take `which`. `which =
  "all"` draws the combined curve, the response of survivors and the
  probability of survival in three panels, each with the no-effect estimate
  and ECx of its own block, the NSEC for a hormesis threshold equation, and
  titled with the equation of each block it reads; `"growth"` or `"survival"`
  draws one of them. `ggbnec_data()` returns a `panel` column naming the curve
  of each row (#564, #565).

- `bnec_hurdle()` fits a hurdle model as two `bnec()` fits: the growth of the
  units with a non-zero response, and a `bernoulli` model for survival. It
  returns a `bayesnechurdlefit`, whose estimators, plots and predictions take
  `which = "combined"`, `"growth"` or `"survival"`. The combined NEC is the
  per-draw minimum of the two components. `formula()` returns the growth
  and survival formulas. Prediction methods pass draw selection and
  `newdata` to both components. A `disp()` term applies to the
  growth component, and so does a `cens()` term where the growth family is
  continuous. `trials()` and `weights()` are refused, as are `binomial`,
  `beta_binomial`, zero-inflated count and two-block growth families.
  `crossed_weights()` gives the weight of every growth and survival pair, and
  `best_crossed()` names the highest. `autoplot()` marks each panel's
  no-effect estimate and its credible limits, and the ECx when `ecx = TRUE`,
  and `plot()` labels each estimate NEC, NSEC or N(S)EC. Its `ggbnec_data()`
  and `autoplot()` refuse `add_nec` and `add_ecx`. See `?bnec_hurdle` and
  the article [Hurdle and zero-inflated concentration-response models](https://open-aims.github.io/bayesnec/articles/example6.html)
  (#188, #249, #412, #415, #547).

- `bnec_hurdle()` fits count survivors, with a `poisson` or `negbinomial`
  growth family, by a zero-truncated likelihood, which needs brms 2.23.2 or
  later. With an earlier brms it stops before fitting and names the joint
  count hurdle, `bnec(family = "hurdle_poisson")` or `bnec(family =
  "hurdle_negbinomial")`, which brms 2.23.0 fits (#249).

## Joint refits

- `bnec_joint()` refits a factorised fit as one model. For a
  `bayesnechurdlefit` it refits the pair `best_crossed()` names with the
  matching two-block family. For a `bayesnecgroupfit` it composes the equation
  each level favours into one model in which every curve parameter takes a
  value per level, and returns a `bayesnecjointfit`. With
  `disp_by_level = TRUE`, the default, the dispersion parameter also takes a
  value per level. Each level is read over its own concentrations, from the
  control recorded at fitting or given as `bnec_joint(x_control = )`, or else
  from its lowest concentration. `prior_type`, `asymptote_observed` and
  `predictor_scale` are taken from the fit unless supplied, and an argument
  that applies only to the other kind of fit is refused. See `?bnec_joint`
  and the article [Grouping and factor covariates](https://open-aims.github.io/bayesnec/articles/example8.html)
  (#382, #388, #448, #500, #516, #557, #578).

- A `bayesnecjointfit` has explicit methods for its shared posterior, and a
  method that returns one result per fit returns one per level. Its internal
  level adapter exposes the four toxicity estimators without inheriting the
  ordinary-fit prediction and diagnostic methods. `summary()` gives each
  level's no-effect estimate, labelled NEC or NSEC, its ECx values and the
  Bayesian R-squared of its rows. `plot()`, `pp_check()`, `check_chains()`
  and `check_priors()` draw one panel or page per level. `rhat()`,
  `check_fit()`, `check_sampling()`, `curve_params()`, `dispersion()`,
  `formula()` and `model.frame()` return one element, row or table per
  level, and `screen_models()` passes or stops the refit as a whole.
  `fitted()`, `predict()`, `posterior_epred()` and `posterior_predict()`
  predict the fitted rows of every level, or a `newdata` naming one
  level's rows. These methods, and those that return one result per level,
  refuse a `level` argument, because all levels share one posterior.
  `group_level_sd()` reads the refit as one fit. `update()` refits the same
  model, `get_priors()` returns its priors, `pull_prior()` returns its full
  prior table, and `pull_best()` returns it unchanged.
  `amend()`, `pull_out()`, `c()` and `+` refuse it, because each of its
  levels has one equation and it is not a model set (#578).

- Where a factorised `Gamma` growth fit includes a left-censored survivor,
  `bnec_joint()` fits that row as an interval from zero to the recorded
  bound, so that the joint likelihood conditions on known survival. A
  `zero_inflated_beta` joint refit with such a row is refused (#544, #546).

- `compare_posterior()` on a `bayesnecjointfit` contrasts its levels at the
  same posterior draw, for the NEC, N(S)EC, NSEC, ECx and fitted curve, so
  that each contrast keeps the dependence between levels estimated in one
  posterior. A group term the levels share, such as a batch in which both
  were tested, is read at the same value for every level at each draw.
  Separate fits are paired at random (#571, #574).

- `nec()` on a `bayesnecjointfit` names each level's equation in a `model`
  column and the kind of its estimate in an `ne_type` column. It gives the
  NEC of a level fitted with a monotone threshold equation, the NSEC of a
  hormesis threshold equation, and `NA`, with a message, for a smooth
  equation. `nec(no_effect = TRUE)` gives the NSEC of a smooth level as well.
  Where every level has a smooth equation, `nec()` stops, as it does for a
  single smooth equation. The other classes refuse `no_effect` (#388, #570).

- `nec()` and `exceedance()` on a `bayesnecgroupfit` return `NA` for a level
  fitted with a single smooth equation, which has no `nec` parameter, and say
  so in one message that names the level and points to `nsec()`, as they do
  for a `bayesnecjointfit`. Where every level is of that kind they stop. A
  level holding a model set is unchanged.

## Estimates beyond the prediction range

- `nec()`, `nsec()`, `ecx()` and `ecnsec()` gain `extrapolate`, which widens
  the bound at which a draw is censored. A finite limit appends points beyond
  the prediction grid at its spacing, so an estimate found inside the range
  is unchanged by a limit beyond it, and a limit inside the range is refused.
  `nec(extrapolate = TRUE)` removes the bound only where every equation
  samples its NEC, and is refused for a fit or set that includes a smooth or
  hormesis threshold equation; the other three refuse `TRUE`. The default
  `nec` prior is bounded to the tested range, so a sampled NEC above it needs
  `asymptote_observed = FALSE`. See `?nec` (#392, #570, #589).

- `exceedance()` gives the posterior probability that a NEC, NSEC or ECx lies
  above a threshold concentration, such as a guideline value. Censored draws
  are counted from their `censored` attribute, and the probability is
  reported with the lower and upper limits they allow (#44).

## Diagnostics

- `check_fit()` compares the observed location and spread of the response at
  each predictor value against the values the fit simulates, and gives a
  posterior predictive p-value for each comparison, counting ties as one half.
  Predictor values without replicates are binned, with a warning. Where the
  observed statistic and the median simulated one are both zero their ratio is
  reported as 1. A group with one observation gets the mean statistics only.
  In a group with readings censored through `cens()`, the observed and
  simulated values beyond the bound are set to it, and `n_cens` gives the
  number censored. It accepts a `bayesnecfit`, `bayesmanecfit`,
  `bayesnecgroupfit` or `bayesnechurdlefit`, prints the Pareto *k* summary of
  the stored LOO criterion separately, and returns it in the `"pareto"`
  attribute. A `group` argument with missing values or of the wrong length
  is refused. It has `print()` and `plot()` methods (#56, #148, #542, #558,
  #566, #568, #578).

- `pp_check()` has methods for `bayesnecfit`, `bayesmanecfit` and
  `bayesnechurdlefit` objects. A model set returns one plot per equation
  (#56, #148).

- `check_sampling()` reports the largest Rhat, the smallest effective sample
  size and the number of divergent transitions of each equation, and
  `screen_models()` drops the equations that fail and reports why (#148).

- `summary()` of a single fit or a model set reports the number of divergent
  transitions of each equation, and its printed output warns where one exceeds
  the new `divergence_cutoff`, 10 by default (#447).

- `bnec()` reports an equation that does not reproduce the mean or spread of
  the control. `summary()` of a model set does the same by default, which
  simulates from each equation, controlled by the new `check_fit` and
  `fit_ratio_cutoff` arguments (#148).

- `nec()`, the printed `summary()` and `curve_params()` report a sampled `nec`
  or `ec50` posterior with more than 7.5 per cent of its draws within the
  outer 5 per cent of its prior range, where the prior bound may set the
  credible limit. `nec()` and `summary()` give a note and a
  `prior_bound_summary` attribute; for a model set it gives each equation's
  share and the model-weighted share. The estimates are unchanged (#584).

- `failed_models()` returns each equation that `bnec()` or `amend()` could not
  fit, with its error, priors and initial values, and `summary()` lists them
  (#133).

## Model weights

- `loo_controls` takes three new elements. `leave_out` is `"group"`, the
  default, or `"observation"`. `"group"` leaves out each level of a
  group-level term whole where every level is observed at a single
  concentration, and otherwise scores each observation, with a warning where
  such a term exists but cannot be left out. `refit = TRUE` refits each
  equation without each level whose Pareto *k* is at or above 0.7 and scores
  that level from the refit, and a message says when it has no effect.
  `report_k = FALSE` silences the Pareto *k* report. `pull_out()` says when
  it ignores `leave_out` or `refit` (#529, #531, #536, #578).

- When a model set is built, a message gives the number of Pareto *k* values
  at or above 0.7 among the scores behind its weights, for each equation with
  at least 0.01 of the model weight. A second message gives the values below
  0.7 that exceed the threshold `loo` sets for the number of draws (#536,
  #553).

- `mod_stats` gains `elpd`, the score the weights were computed from, and
  `summary()` prints what the score left out. A set fitted by 2.1.3.1 keeps
  its score when `amend()`, `pull_out()`, `c()` or `update()` rebuilds it;
  `amend(x, loo_controls = list(leave_out = "group"))` recomputes its weights
  (#531).

## Priors

- `bnec()` and `amend()` gain `prior_type`. `prior_type = "regularizing"`
  narrows the default `top`, `bot`, `nec`, `ec50` and group-level priors of
  the default `"uninformative"`, centring `top` on the response at the lowest
  concentrations and `bot` on the response at the highest, and places
  `normal(0, 1)` on the curve-shape exponents `d` and `f`. `beta` and `slope`
  keep `normal(0, 5)` under both. See `?bnec` and `vignette("example3")`
  (#535, #537).

- `bnec()` and `amend()` gain `asymptote_observed`. `FALSE` declares that the
  highest concentration did not reach the lower asymptote. It places the `bot`
  prior between the floor of the response and the mean response at the end of
  the series, removes the upper bound of the `nec` and `ec50` priors, and
  extends the initial-value search to the floor. For an equation that
  estimates `bot` on its default prior, it is refused where the response has
  no floor, as for a gaussian response with a negative value or a log or logit
  link, unless `prior` supplies `bot`. `bnec()` reports, before fitting, a
  response still declining between the two highest concentrations, with the
  highest mean as a percentage of the control mean. Where that is at most 5
  per cent, the report omits the advice to declare the asymptote unobserved.
  Grouped levels and hurdle blocks are assessed separately (#390, #394, #449,
  #483, #516, #561, #585).

- `bnec()` and `amend()` gain `predictor_scale`, `"concentration"` or `"log"`,
  which declares the scale of the predictor for the default `nec` and `ec50`
  priors. The default `"auto"` treats a predictor with a negative value as
  logged, and `"concentration"` refuses a negative predictor (#317).

- `get_priors()` returns priors in the form `bnec(prior = )` accepts, from a
  fit or from a formula and data before anything is fitted. It refuses a
  grouped or joint fit and an argument it does not use (#141, #578).

- `bnec()` takes `prior` as a named argument, and accepts a prior on the
  family's dispersion parameter (`sigma`, `shape` or `phi`) (#207).

## Dispersion

- `bnec()` accepts a `disp()` term in the formula. `disp(~x)` models the
  dispersion parameter on the predictor, and `disp("power")`,
  `disp("twosided")` and `disp("loglinear")` model it as a function of the
  mean. It is refused for a family with no free dispersion parameter and for
  `hurdle_poisson` and the zero-inflated count families, and a function of the
  mean is refused under a link other than the identity, as is a `disp(~x)`
  term whose values are not finite. On `hurdle_gamma`, `zero_inflated_beta`
  and `hurdle_negbinomial` it models the positive block. See `?bnec` and
  the article [A complete analysis workflow](https://open-aims.github.io/bayesnec/articles/example9.html)
  (#191, #397, #410, #443, #468, #472).

- `dispersion()` accepts a model set, returning a named list of draws, or
  with `summary = TRUE` one row per equation, and a `bayesnechurdlefit`, for
  which it returns the growth and survival components. `mod_stats` gains
  `dispersion_P_over_1`, which the weights table of `summary()` shows where
  the test applies (#262).

## Families, formulas and equations

- `bnec()` accepts a `rate()` term for `poisson` and `negbinomial` responses.
  The curve, its estimates and the plots, observations included, are then per
  unit of exposure, and the default `top` and `bot` priors are built on that
  scale. The denominator must be a column of the data (#136, #389).

- `bnec()` accepts a `cens()` term. A censored observation keeps its recorded
  value rather than being shifted off a boundary, and censoring at a bound the
  family excludes is refused. See `vignette("example1")` (#181).

- `ecxflat` is a new equation whose mean is constant over the predictor. It
  has no `nec` parameter, so its no-effect estimate is an NSEC, and every ECx
  it gives is censored above the prediction range (#419).

- `bayesnecformula()` and `bnf()` gain `env`, the environment in which a
  formula given as a string is resolved (#319).

- `models()` gains `max_pars`, the largest number of curve parameters an
  equation may have (#301).

- `make_brmsformula()` gains `family`, which a two-block family and a
  `disp()` term need.

## Other new features

- `predict()`, `plot()` and `autoplot()` on a model set gain `model`, which
  names equations to show alongside or instead of the model average, and
  `average`. `predict()` then returns a list. `all_models` is deprecated and
  gives a warning whenever it is supplied; `all_models = TRUE` still works in
  `plot()` and `autoplot()`, and is written `model = fit$success_models,
  average = FALSE` (#120).

- `bnec()` gains `x_control`, a stated control concentration, given on the
  scale the predictor was recorded on. It sets the concentration at which
  the estimators read the control, the lower bound of the default `nec` and
  `ec50` priors, the start of the prediction grid, the stored NSEC and the
  control of the fit checks. Without it the control is the lowest observed
  concentration. A value that is not a single finite number, a negative value
  where the predictor is a concentration, a value at or above the highest
  observed concentration, or one that differs from the lower end of `x_range`
  is refused. A control below the lowest concentration tested is reported as
  an extrapolation, and under `prior_type = "regularizing"` a control with no
  observation needs a supplied `top` prior. `bnec_group()` and `bnec_hurdle()`
  apply it to every level or component, `bnec_joint()` takes the recorded
  control unless given its own, `amend()` and `update()` reuse it, and `c()`
  refuses fits with different controls (#586).

- `ggbnec_data()` and `autoplot()` accept `x_scale = "fitted"` where `crf()`
  transforms the predictor inline. The curve and observations are then drawn
  on the fitted scale, and the axis is labelled in recorded concentrations,
  including the ends of the range and a zero control where the transformation
  is finite at zero. It is refused with an `xform` other than the identity
  (#547).

- `autoplot()` gains `point_alpha`, the opacity of the observation points,
  and every `autoplot()` method takes `xlab` and `ylab` (#533, #565).

- `bnec_record()` reports the equations requested, those attempted, those
  excluded with the reason, and any change made to the response before
  fitting (#93, #261).

- `bnec_version()` reports the bayesnec version, its source and commit, and
  the R, brms and Stan interface versions that fitted each equation of an
  object. For an object fitted earlier the source is `"unknown"` (#464).

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

- `bnec()` and `amend()` gain `timeout`, a time limit for each equation. An
  equation of a set that exceeds it is recorded as failed, and a single
  equation stops with an error. A finite limit needs the R.utils package,
  whose absence is reported before any equation is fitted (#157, #578).

- New datasets: `alga`, `coral_colour`, `coral_pam`, `lum31` and `nassarius`
  (#6, #33, #173, #181, #301).

## Other changes

- `summary()` of a model set flags an equation whose Rhat exceeds the new
  `rhat_cutoff` argument, 1.01 by default (#148).

- `pull_out()` gains `x_range`, `resolution` and `sig_val` (#489).

- The initial-value search says so when it is still running after 20 seconds
  (#309).

- `bnec()` reports, once per call, each value of the response it shifts off a
  boundary of the family (#93).

- `bnec()` reports a response that appears normalised to the control or to
  its maximum (#173).

- A fit no longer stores the environment its formula was written in, only the
  objects the formula uses that the data do not supply, so saved fits are
  smaller (#329).

- `ecx()` accepts any `ecx_val` above 0, and an absolute ECx of a gaussian
  equation without `bot`, both of which 2.1.3.1 refused (#206, #325).

- `ggbnec_data()` reads the credible band of a single fit from its stored
  `pred_vals` rather than from `brms::conditional_effects()`, so its values
  can differ slightly from 2.1.3.1 (#557).

## Bug fixes

- `models()` accepts every model group, including `"decline"` and
  `"hormesis"`, and gives an informative error for input it cannot map
  (#170).

- `bnec()` excludes, before fitting, an equation left with no group-level term
  on a grouping variable the formula names, and records why. 2.1.3.1 fitted it
  and then stopped after sampling with "Group-level variable(s) ... not found
  in dataset" (#470).

- `bnec()` drops a parameter an equation lacks from every `(par | group)` term
  that names it, not only the first (#470).

- `check_formula()` refuses a missing grouping variable in any `(par | group)`
  term, not only the first (#466).

- `bnec()` accepts a user prior on a group-level standard deviation or on
  `ogl()`, which the initial-value search of 2.1.3.1 refused (#245).

- `bnec()` accepts a `lognormal` or `constant()` prior on a curve parameter,
  and `sample_priors()` draws from both (#84, #244, #302).

- `sample_priors()` draws a bounded prior from the truncated distribution.
  2.1.3.1 resampled the draws that fell inside the bounds with replacement,
  which repeated values under tight bounds.

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

- A model set can combine equations with different numbers of posterior
  draws, where `c()` in 2.1.3.1 stopped with "subscript out of bounds". The
  model average contains as many draws as the equation with the fewest, and each
  equation's share is sampled from all of its draws (#548, #583).

- `compare_estimates()` and `compare_posterior()` return the names of the
  fits unchanged in `posterior_data`. 2.1.3.1 turned a name that is not a
  syntactic R name, such as `"2018"`, into `"X2018"`, which then did not
  match `prob_diff` and `diff_data`.

- `autoplot()` on a model set with `multi_facet = FALSE` honours
  `plot = FALSE` and restores the page prompt (#444).

- `plot()` accepts `position_legend` as x and y coordinates, as documented
  (#278).

- `ggbnec_data()` refuses an `xform` that is not a function on a model set,
  as it does on a single fit (#278).

- `check_priors()` on a model set restores the page prompt and closes its PDF
  device on exit. It now returns named plots invisibly; `check_priors()` and
  `check_chains()` no longer print a model extraction message for each
  equation (#459).

- extraDistr is listed in Suggests, since brms needs it to evaluate a
  `beta_binomial` likelihood.

- `autoplot(ecx = TRUE)` no longer fails for a gaussian equation without
  `bot`.

## Documentation and vignettes

- `example1`, `example2`, `example2b` and `example3` remain vignettes and are
  installed with the package. The other worked examples, including `example4`
  and `example5`, which were vignettes in 2.1.3.1, are now articles on the
  package website (<https://open-aims.github.io/bayesnec/articles/>) and are
  not installed, so `vignette("example4")` and `vignette("example5")` no longer
  find them.

- The article [Hurdle and zero-inflated concentration-response models](https://open-aims.github.io/bayesnec/articles/example6.html) is new and fits `nassarius` with `bnec_hurdle()` and the
  two-block families (#175).

- The article [Modelling growth data and other potentially negative response values](https://open-aims.github.io/bayesnec/articles/example7.html) is new. It compares approaches on simulated
  growth data and then fits `alga` (#193).

- The article [Grouping and factor covariates](https://open-aims.github.io/bayesnec/articles/example8.html) is new. It fits
  `lum31` with group-level terms, `bnec_group()` and `bnec_joint()` (#33).

- The article [A complete analysis workflow](https://open-aims.github.io/bayesnec/articles/example9.html) is new. It takes
  one `lum31` zinc plate read at 15 minutes from data to estimates, fitting
  `model = "all"` with `ogl(conc_group)` and `disp("power")` (#219, #367).

- The article [Hormesis and the no-significant-effect concentration](https://open-aims.github.io/bayesnec/articles/example10.html) is new. It sets out the candidate no-effect estimates of a
  hormetic curve and fits the hormesis equations to published data,
  including the lettuce series of van Ewijk and Hoekstra (1993), which
  bayesnec now distributes as `inst/extdata/hormesis_lettuce_1993.csv`
  (#570, #573).

- `vignette("example3")`, "Priors", is rewritten. It covers the default
  priors, the choice between `prior_type` sets, fixing a parameter, and
  priors in models with group-level terms.

- The article [Comparisons and exceedance probabilities](https://open-aims.github.io/bayesnec/articles/example4.html),
  formerly the vignette "Comparing posterior predictions", is rewritten on
  published data. It compares the seven herbicides of `herbicide`, and the
  three climate scenarios of `coral_colour` and `coral_pam`, in place of
  simulated data, and uses `exceedance()` to compare the N(S)EC of each
  herbicide with a guideline value for diuron (#534).

- `example5` is rewritten as the article [Installation and setup](https://open-aims.github.io/bayesnec/articles/example5.html),
  and `vignette("example1")`, `vignette("example2")` and the README refer to it
  (#342).

- `vignette("example1")` gains sections on the scale of the predictor, counts
  observed over an exposure, preparing the response, censoring and
  non-constant dispersion (#173, #181, #191, #193, #194).

- `vignette("example2")` demonstrates `screen_models()` and fitting a model
  set in parallel (#184, #248).

- `vignette("example2b")` and `?models` state that `NEC.3()` and `NEC.4()` in
  the drc package correspond to `nec3param` and `nec4param`, and that
  `NEC.2()` has no counterpart (#139).

- `?models` states that a model group names a response shape, and which
  equations each family excludes (#170, #285).

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

- An additional family has been added beta_binomial2 for over-dispersed binomial data.

- The package supports using link functions for generalised modelling, which appears to be more stable and is also in line with more typical generalised modelling approaches.

- There are multiple options for model weights calculation from the loo package. The default is "pseudobma BB".

- There is now a compare_posterior function that also includes a bootstrapping procedure. This can be used to compare model fits across different datasets, or even different model sets for the same dataset (ie nec v ecx models). Please see the vignette for examples of usage.

- There is a vignette detailing the models available in bayesnec. Note that not all models are suitable for all families, and also depending if link functions are used.

- A new check_chains function has been added to allow chain plotting in base R and that works more smoothly with plotting chains for multiple fits for bayesmanec objects.
