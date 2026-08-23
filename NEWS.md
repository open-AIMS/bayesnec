# bayesnec 2.2.0

- A control lack-of-fit is now surfaced rather than waiting to be looked for:
  once at the end of `bnec()`, and as a line in `summary()` alongside the
  convergence verdict. Both threshold on the **ratio** of observed to simulated,
  not on the posterior predictive p-value. That is deliberate and measured: on
  two independently fitted parameterisations of the same data the simulated
  control mean overshot the observed by ~19%, reproducing across fits, while
  both p-values sat at about 0.82 and neither came near flagging. A p-value
  threshold would stay silent on exactly the case the check exists to catch.
  `nsec()` reads its reference from the control, so this is the region most
  likely to move a reported no-effect concentration. See #148.

- New `check_fit()`, reporting per group of the predictor the observed location
  and scale of the response against what the fitted model simulates, with a
  posterior predictive p-value for each and the control group flagged. It sits
  alongside `check_chains()`, which checks the sampler, and `check_priors()`,
  which checks the priors: this checks the fit against the data.

  It is deliberately **local**. `dispersion()` reports one global statistic, and
  for any family with a free dispersion parameter that parameter absorbs exactly
  the discrepancy the global statistic measures — on the packaged
  `manec_example` the global Pearson ratio is a healthy 1.011 [0.71, 1.44] while
  the same fit simulates about 26% more variability than the data show in the
  control region. That matters because `nsec()` sets its reference from the
  posterior of the control mean, so mis-stating control variability moves a
  reported no-effect concentration, and nothing previously reported it.

  The scale statistic is computed on residuals, not raw values: within a group
  the raw standard deviation mixes residual variability with the slope of the
  curve across that group, which would make every steep region look
  overdispersed. Grouping prefers genuine replication and falls back to binning
  with a warning. For a `bayesmanecfit` the per-candidate-model rows are
  reported with their stacking weights, because weights come from a global
  `elpd` and a candidate can hold high weight while fitting the control badly.
  For the mixture families the observed and simulated proportion of zeros is
  reported too — the question those families exist to answer, which nothing
  else reported. \code{plot()} shows the same table graphically: per group, the
  observed statistic against the 95% span of what the fit simulates, in
  separate location and scale panels with the control drawn apart. See #148,
  which also closes #56.

- New `pp_check()` methods for `bayesnecfit`, `bayesmanecfit` and
  `bayesnechurdlefit`, so posterior predictive checks no longer require
  unwrapping the underlying `brmsfit`. `pp_check(x, type = "loo_pit_overlay")`
  gives a LOO-PIT check — the Bayesian counterpart of a uniform quantile
  residual — using the `loo` criterion `bnec()` already adds, so it needs no
  extra step and no new dependency. See #148 and #56.

- `hurdle_poisson` and `hurdle_negbinomial` are now available as two-block
  families, the count analogues of `hurdle_gamma`. Use them where the non-zero
  response is a count and the zeros are *observed* to be structural — the
  individual died, the replicate failed. `brms` writes the positive part
  zero-truncated, so the mean block estimates the mean of the surviving counts
  rather than the mean of counts conditioned on being non-zero, and both blocks
  carry an interpretable concentration-response curve. This is deliberately not
  the same decision as for `zero_inflated_poisson` and
  `zero_inflated_negbinomial`, which stay on the single-block path: there the
  zeros are latent, so the zero-probability and the mean are weakly separated
  exactly where the mean is small — the high-concentration end that determines
  the *NEC*. See #209.

- **Behaviour change:** `bnec_hurdle()` now refuses `poisson` and `negbinomial`
  as the growth family, on the automatically selected path as well as a supplied
  one. It fits the growth component to the non-zero rows with an *untruncated*
  count family, which estimates `mu / (1 - exp(-mu))` rather than `mu`; the bias
  is negligible for large means and grows as the mean falls towards zero, which
  is the high-concentration end the *NEC* and ECx are read off. For
  `hurdle_gamma` the same construction is exact, because a Gamma has no mass at
  zero, which is why this only surfaced once counts were in scope. The separate
  fits cannot express the truncation, so the error points at
  `bnec(family = "hurdle_poisson")`, which is correct by construction. See #209.

- `bnec()` now supports the `rate()` aterm for the `poisson` and `negbinomial`
  families, so a count observed over an exposure — animals per unit time,
  larvae per unit area — can be modelled directly. Because `bnec()` forces
  `link = "identity"`, `brms` writes the denominator multiplicatively on the
  response scale rather than as a log offset, so the mean *is* the rate and
  `top`, `bot` and `nec` stay interpretable as counts per unit exposure. The
  prediction grid holds the denominator at 1, so the fitted curve, `ecx()` and
  `nsec()` are all read on the rate scale, and plots divide the observations
  through to match. `rate()` was previously neither supported nor refused: it
  emitted a generic message, fitted, and then failed in post-processing with a
  `brms` error about a variable it could not find, and the priors for `top` and
  `bot` were built from raw counts rather than rates — a prior mean of ~61
  against a true `top` of 20 on the reprex in the issue, silently. `offset` is
  deliberately not offered as an alternative: under an identity link it would be
  additive on the mean. See #136.

- **Breaking:** an `aterm` `bayesnec` has not validated is now an **error**
  rather than a message. The validated set is `trials()`, `weights()`, `cens()`
  and `rate()`. Passing anything else previously printed a warning and carried
  on, which is how `rate()` came to fit and then fail tens of seconds later,
  long after the message had scrolled past. An aterm the package has not
  validated cannot be assumed to behave sensibly through prior generation, the
  initial-value search and post-processing. See #136.

- New `check_sampling()` and `screen_models()`. `check_sampling()` reports, per
  candidate model, the largest Rhat, the smallest effective sample size and the
  number of divergent transitions; `screen_models()` drops the failures and
  **messages what went and why**, which is what a methods section has to cite.
  They screen on the sampler only — a poor `check_fit()` is a modelling result,
  and dropping on it silently would hide exactly what the user needs to see.

  Effective sample size is reported as an absolute (`min_ess`) as well as a
  ratio, so the default `ess_cutoff = 400` is directly Vehtari's recommendation
  that both bulk and tail ESS exceed 100 per chain, at the four chains `bnec()`
  fits by default. Note that a heavily thinned fit can fail a cutoff a ratio
  would have passed; the answer is to retain more draws, not to lower the
  cutoff, because thinning lowers ESS by construction. The divergence default of
  10 has no literature behind it — Stan's guidance is that *any* divergence
  means the sampler failed to explore the posterior — and is a working default
  from practice with these non-linear models, documented as such. See #148.

- **Behaviour change:** the default `rhat_cutoff` is now **1.01** rather than
  1.05, in `rhat()` for all three fit classes and in `summary()`, following
  Vehtari et al. (2021) — which is the reference `vignette("example2")` already
  cited while the code used the looser value. `print()` on a summary now reports
  the cutoff actually in use rather than the hard-coded 1.05 it printed before.

  Relatedly, `summary()` now *computes* its convergence verdict with `rhat()`
  instead of searching `brms`'s captured warning text for the literal string
  `"some Rhats are > 1.05"`. That made the threshold `brms`'s to set rather than
  `bayesnec`'s, and it would have failed silently: `brms (>= 2.23.0)` is a floor,
  not a ceiling, so a reworded warning would have made every model report no
  issue and the summary quietly stop warning. See #148.

# bayesnec 2.1.4

- The vignette precompilation workflow added in 2.1.4 can now actually be run,
  and the documentation site configuration records what `pkgdown` really does.
  `workflow_dispatch` only takes effect once a workflow file is on the
  repository's default branch, so the job was invisible and undispatchable while
  it lived on `dev` alone; it now also triggers on pushes to a `precompile/**`
  branch, which makes it exercisable before it reaches `master`. Both workflows
  declare the `permissions` they need rather than depending on the repository
  default. The step that rebuilds a subset of vignettes now restores the ones it
  held back — the previous `on.exit()` never fired, because `shell: Rscript {0}`
  runs at top level. The cache on `~/.cmdstan` was removed: `brms` uses the
  `rstan` backend here, so nothing is ever written there and the step cached an
  empty directory while reading as though compilation were cached. The workflow
  also gained a dry-run mode and branch-suffix selection, so that a run which
  will take hours can have its plumbing — permissions, checkout, and the
  pull-request step — verified first. See #230.


- `bnec()` now accepts a prior on the family's own dispersion parameter —
  `sigma`, `shape` or `phi`. Supplying one previously failed in the
  initial-value search, because it compared the prior's parameter names against
  the *curve's* arguments as an exact set, so any row naming something the mean
  curve does not have killed the call. There is now no way to regularise
  dispersion where the data are sparse and the `brms` default is too vague, and
  `get_priors()` could not report it either, so a fit's prior set was a complete
  record of everything except dispersion. No initial value is generated for it:
  Stan random-initialises any parameter absent from an init list. See #207.

- A prior set that is missing parameters the model needs is now filled from the
  `bayesnec` defaults, with a warning naming each one, instead of being accepted
  as though it were complete. Previously the unmentioned parameters fell through
  to `brms`, which means a **flat** prior — the opposite of what `bayesnec` is
  for, since it generates weakly informative priors precisely because flat ones
  are rarely useful in non-linear modelling. The case this bit hardest is the
  one `define_disp_prior()` exists to prevent: drop the `c0` and slope rows a
  route B `disp()` term adds and the fit ran on flat priors for parameters its
  own documentation calls "near-perfectly confounded", with nothing to say so.
  Editing a returned prior set and handing it back is exactly the workflow
  `get_priors()` invites, so this was easy to hit. **This changes results**: a
  fit that previously ran on flat priors for the parameters it did not mention
  will now run on `bayesnec` priors and give different numbers. See #207.

- `define_prior()` no longer collapses the `top` and `bot` priors when a large
  share of the response is exactly zero. The gamma rates for those parameters
  are set from quantiles of the response, and those quantiles are zero once
  enough of the response is — so `bot` was pinned near zero whatever the real
  lower asymptote was (from 25% zeros under the default `prior_type`, and from a
  *single* zero under `"regularizing"`, which uses the minimum), and past 75%
  zeros the `top` rate divided by zero and produced the literal prior string
  `gamma(2, Inf)`. Where a quantile has collapsed, the scale now falls back to
  the same quantile of the positive part of the response. This is not new to the
  zero-inflated count families added in 2.1.4 — a `poisson` or `Gamma` response
  with many zeros always hit it — but those families exist precisely for that
  regime, which made it the normal case rather than an accident. A response with
  no positive values at all now raises an informative error instead. Priors for
  a response containing no zeros are unchanged. See #210.

- `?models` and `vignette("example2b")` now document how the `drc` package's
  `NEC.2()`, `NEC.3()` and `NEC.4()` map onto the `bayesnec` model set.
  `NEC.4()` and `NEC.3()` are `nec4param` and `nec3param` — not approximations
  of them: given `b = exp(beta)` the two implementations agree to the last bit.
  The documentation also records the one substantive difference (`bayesnec`
  estimates `beta` and uses `exp(beta)`, so the decay rate is positive by
  construction, and `drc`'s `b < 0` region is deliberately unreachable here),
  that the extra log-logistic term in `?drc::NEC` appears in that help page but
  not in the code `drc` fits, and why `NEC.2()` has no equivalent here. See
  #139.

- The package website now publishes a **development preview** alongside the
  released documentation. `master` continues to publish to the site root;
  `dev` publishes to `/dev/`, carrying pkgdown's own development banner so the
  two cannot be confused. This makes a vignette written or changed on `dev`
  readable in rendered form without a contributor precompiling locally, and
  makes drift between `dev` code and `master` vignette output visible before
  release rather than at it. A separate manually-dispatched workflow rebuilds
  the precompiled vignettes and opens a PR with the result, so a refresh no
  longer requires a local Stan toolchain. See #215.

- Model-averaged output from a `bayesmanecfit` is now reproducible. Averaging
  keeps `round(sample_size * wi)` of each component model's draws, and which
  draws those were used to be settled by an unseeded `sample()` at every call
  site independently. `posterior_epred()`, `posterior_predict()`, `ecx()`,
  `nsec()` and everything built on them therefore returned a different answer on
  every call, and none of them agreed with the summaries stored on the object.
  For `nsec()` the instability landed almost entirely on the lower bound, which
  is the end used to set a protective concentration: over six replicate calls it
  spanned 0.735–0.844 while the median moved by less than 0.5%. The draw is now
  realised once, when the object is built, and kept on the object as the new
  `w_draw_index` slot, with the seed behind it in `w_draw_seed`; every later call
  reuses it. Realisation *i* now means "component *m*[*i*], iteration *j*[*i*]"
  for every quantity taken from one object. The averaging method is unchanged —
  only where the randomness is drawn. Three consequences worth knowing.
  Model-averaged numbers will differ from those produced by earlier versions;
  the old ones were not reproducible, so there is no "before" to match.
  `posterior_epred()` over the build grid now returns exactly the draws that
  `w_pred_vals` summarises rather than an independent redraw of them. And an
  `nsec()` and its `ecnsec` are now the same draw of the same component model,
  where before they came from two independent `sample()` calls and a pair could
  be two unrelated draws of two different models. `predict()` and
  `posterior_predict()` still vary between calls unless a seed is set, because
  they simulate new observations from the likelihood — the same behaviour `brms`
  has for a single fit, and unrelated to model averaging. Objects saved before
  this version carry neither field and fall back to a fixed seed, which is
  equally reproducible. See #216.

- Model-averaged predictions from a `bayesmanecfit` no longer collapse when
  `newdata` has a single row. Asking what the averaged curve predicts at one
  concentration — `fitted(fit, newdata = data.frame(x = 3))` — returned one row
  per retained draw instead of one row per prediction, because the weighted row
  selection dropped to a vector and the per-model results were then stacked as
  rows rather than columns. The point estimate looked plausible, but its
  `Est.Error` and quantiles were computed across models rather than over draws.
  A `bayesnecfit` was unaffected, as was any `newdata` with more than one row.

- `brms (>= 2.23.0)` is now required. Earlier versions generate the
  `beta_binomial` likelihood by passing the whole `trials` array to
  `beta_binomial_lpmf` instead of `trials[n]`, so each response is evaluated
  against every trial count. With varying `trials` the density is `-Inf`
  everywhere and the model cannot initialise; with constant `trials` it samples
  on a likelihood inflated by a factor of `N`, which is silent and leaves
  posteriors overconfident by roughly `sqrt(N)`. Fixed upstream in 2.23.0. See
  `notes/beta_binomial_varying_trials.md`.
- Fixed a doubled roxygen marker in the references section of `?nsec`, which
  left a stray `#'` in the rendered help.
- New `disp()` term in `bayesnecformula`, allowing a family's dispersion
  parameter to vary across the concentration-response curve instead of being
  held constant. Two forms. `disp(~x)` models dispersion on the **predictor** —
  an ordinary `brms` distributional formula, so `disp(~log(x))` and
  `disp(~s(x))` also work — and says only that noise is larger at one end of the
  dose axis. `disp("power")` models it on the **fitted mean**, a variance
  function in the GLM sense, which is a statement about the measurement process
  rather than about the dose axis. The two coincide for a monotone curve, where
  `mu` is itself a monotone function of `x`, and separate under hormesis or
  where a design revisits the same `mu`.
- Variance functions are named and registered in the same spirit as the models,
  so adding one later is a registry entry and a prior rather than a change to
  the generator. `"power"` gives `log(dpar) = c0 + c1 * (log(mu) - log(m))`,
  which is `dpar = exp(c0) * (mu/m)^c1` because every eligible family puts a log
  link on its
  dispersion parameter and `validate_family()` forces identity on `mu` only.
  `"twosided"` adds a `c2 * (log(1 - mu) - log(1 - m))` term for `Beta` and `beta_binomial`, so
  that variance can shrink toward both boundaries of `(0, 1)` — the shape a
  bounded family asserts, without asserting a ceiling with it. `c1 = 0` is the
  constant-dispersion model in every case, and is where the prior on `c1` sits.
- The covariate in every variance function is **centred** on a reference `m`
  computed from the response before fitting — its median, or its geometric
  median for the forms taking `log(mu)`. `m` is a constant, not an estimated
  quantity, so only the coordinates change and not the likelihood. Without it
  `c0` would be the dispersion parameter at `mu = 1` (or `mu = 0` for
  `"loglinear"`), which is nowhere near the data unless the response happens to
  be of order one: fitting algal cell density (`mu ~ 1.8e4`) uncentred gave a
  posterior correlation between `c0` and `c1` of 0.99, a `c1` of the wrong sign
  and an implied CV of `1e6` against an observed 0.03–0.6. Centred, that fit
  behaves and `c0` is interpretable as the dispersion at a typical response.
- The parameters a variance function introduces are now initialised at the
  constant-dispersion null (every slope at zero) rather than left to Stan's
  default draw. The sign of a slope is tied to the direction of the mean curve,
  so a chain started in the mirror-image basin converges to an inverted
  solution; leaving these to chance produced an R-hat of 1.85 with one chain
  reporting the wrong sign, where seeding from the null gives 1.001.
- `"loglinear"` gives `log(dpar) = c0 + c1 * (mu - m)`, linear in the mean rather than
  in its logarithm, and so is the one form defined where the fitted mean reaches
  or crosses zero. It is also what a log-transformed endpoint inherits from a
  power law on its original scale: if a density has `sd ~ mu^p` then
  `sd(log N) ~ mu^(p - 1)` by the delta method, and substituting
  `mu_N = N0 * exp(days * mu_sgr)` for an average specific growth rate leaves
  `log sd(sgr) = const + days * (p - 1) * mu_sgr`. A growth rate is therefore
  not a case a variance function cannot reach, only one the power law cannot,
  and `p < 1` implies `c1 < 0` — dispersion falling as the growth rate rises,
  which is the pattern the algal tests show.
- `c1` is dimensionless where it multiplies `log(mu)` but carries units of
  `1/response` where it multiplies `mu`, so the default prior for `"loglinear"`
  is scaled by the observed spread of the response. A fixed `normal(0, 2)` would
  be near-flat for a response spanning thousands and sharply informative for one
  spanning a fraction.
- Route B needs the curve expression written out a second time inside the
  dispersion formula, because `mu` is not in scope for another distributional
  parameter's formula in `brms`. Only the source is duplicated, not the fitted
  quantity: the curve parameters are shared, being declared once for the whole
  formula. This is mechanical here because `bayesnec` already owns every curve
  expression in `sysdata`.
- What the exponent means depends on the family, since each already imposes its
  own mean-variance link — `c1 = 1` is constant CV under `gaussian`, while under
  `Gamma` a constant `shape` is already constant CV, so `c1 = 0` is. The form is
  defined on the dispersion parameter, which is what `brms` fits, and the
  implied variance per family is tabulated in `?bayesnecformula` rather than
  algebraically normalised away.
- `disp()` requires a family with a free dispersion parameter and is refused
  with an explanation for `poisson`, `bernoulli` and `binomial`, whose variance
  is a deterministic function of the mean. Those are exactly the families the
  existing `dispersion()` diagnostic applies to, so the two cover disjoint sets
  of families and are complements rather than alternatives. The two-block
  families are also refused for now, since coupling a variance function to one
  block of a joint fit needs a decision about the other.
- A `log(mu)` variance function is refused where the fitted mean crosses zero,
  rather than failing at initialisation, with the error pointing at
  `disp("loglinear")` or `disp(~x)`.
  See [#191](https://github.com/open-AIMS/bayesnec/issues/191).
- New *Non-constant dispersion* section in `vignette("example1")`, presenting
  `disp()` as a diagnostic rather than a routine addition. It gives three
  explanations to exclude before concluding that dispersion is genuinely
  non-constant — substituted or censored values, a misspecified family, and
  lack of fit in the mean — then works a simulated example through to the
  consequences for the toxicity estimates. Across nineteen datasets screened
  with adequate replication, no real dataset showed a dispersion relationship
  that survived its own confound, which is why the example is simulated.
- New *The scale of the predictor* section in `vignette("example1")`, placed
  before any model is fitted. The scale on which the predictor enters the model
  is the user's decision and `bnec` takes it as given, yet it typically matters
  more than the choice of equation. The section covers how to judge it from the
  spacing of the design rather than the response, the `crf(log(x))` syntax,
  offsets for zero concentrations, and back-transforming the toxicity estimates.
  See [#194](https://github.com/open-AIMS/bayesnec/issues/194).
- Every `bnec()` call in `vignette("example1")` now passes `seed` explicitly.
  `set.seed()` governs the initial values `bayesnec` generates but not the seed
  Stan's sampler uses, so repeated builds of the vignette drifted; passing
  `seed` makes them bit-identical.
- `amend()` now has a method for `bayesnecfit`, so models can be added to a
  single-model fit. `?bnec` recommends testing a fit with one likely model
  before committing to a set, and `amend(fit, add = )` is the natural next step;
  previously it errored and the fit already paid for had to be discarded, or
  combined by hand with `+`. Adding models promotes the object to a
  `bayesmanecfit`, as `+` and `c()` already do. `drop` is an error for a
  single-model fit rather than a silent no-op, since honouring it would leave
  nothing to return. Both methods now share one implementation, so the single-
  and multi-model paths cannot drift apart. See
  [#176](https://github.com/open-AIMS/bayesnec/issues/176).
- `bnec_hurdle()` now accepts a `cens()` aterm on the response, which unblocks
  the combination the censoring work was raised for: a growth endpoint that is
  both zero-bounded with structural zeros (deaths) and left-censored at the
  recording resolution (survivors measured below the limit). Only a two-part
  model with a censored response component can tell those two zeros apart. The
  guard was stricter than the constraint it protected — `hurdle_response_var()`
  required the whole left-hand side to deparse to a bare name, when what the
  zero-as-death convention needs is a bare *response*; an aterm alongside it
  does not threaten that.
- The declaration is routed rather than merely allowed. It is passed to the
  growth component, where the censoring indicator travels as an ordinary data
  column and is subset with everything else, and dropped from the survival
  component, whose alive/dead response is observed exactly. That was already the
  behaviour of `swap_response()`, incidentally; it is now deliberate and tested.
- A row that is both zero and censored is refused. A structural zero and a
  censored observation are the two things a hurdle model exists to separate, and
  accepting a row claiming to be both would reproduce the confusion
  `vignette("example6")` warns about. Under a Gamma growth component the
  censoring bound cannot be `0` — `bnec()` already rejects that — so the
  encoding is "at most the smallest resolvable value", which `?bnec_hurdle` now
  says.
- Other aterms keep erroring, but by name and with the reason. `trials()` has no
  meaning for either component; `weights()` is refused because whether a weight
  applies to the growth component, the survival component or both is a modelling
  decision `bnec_hurdle()` will not make on the user's behalf. See
  [#188](https://github.com/open-AIMS/bayesnec/issues/188).
- `models()` and `check_models()` no longer disagree about which equations are
  available for a given response range. `models(c(0, 1))` was dropping
  `nechorme` and `nechorme4`, which `bnec()` fits happily for a 0-1 bounded
  identity family, and `models(c(0, Inf))` was keeping `nechormepwr01`, which
  `bnec()` drops for a zero-bounded one. The numeric branch of `models()` now
  asks `check_models()` — the function the fitting path itself uses — rather
  than restating the rules, so the advertised set is the set that will be
  fitted. A test asserts the two agree for every response range and every family
  sharing it. **No restriction has been relaxed:** the behaviour of `bnec()` is
  unchanged, and `models()` has been corrected to describe it. See
  [#170](https://github.com/open-AIMS/bayesnec/issues/170).
- `models()` now errors informatively on input it cannot map. A numeric range
  that is neither unbounded, 0-1 bounded nor zero-bounded — `models(c(0, 100))`
  — and an unrecognised character argument both used to fail with "object
  'use_mods' not found". `models()` also accepts every model group `bnec()`
  does: `"decline"` and `"hormesis"` are valid in `bnec(model = )` through
  `handle_set()` but were missing from the list `models()` recognised.
- New `failed_models()`, returning the models `bnec()` or `amend()` attempted
  and could not fit, each with the error, the priors and the initial values used
  in the attempt. A set fitted with `model = "all"` regularly loses a model or
  two and the error scrolls past mid-run; what is needed to diagnose one is the
  prior and the starting values it was given, and both are constructed inside
  `bnec()` rather than supplied by the user, so re-running the whole set was
  previously the only way to see them. The returned prior is a `brmsprior` and
  is directly usable, so the natural next step —
  `bnec(..., model = "nechormepwr", prior = failed_models(fit)$nechormepwr$prior)`
  — needs no reconstruction. `summary()` reports how many models failed and
  which. See [#133](https://github.com/open-AIMS/bayesnec/issues/133).
- The record is attached to a single-model `bayesnecfit` as well as to a
  `bayesmanecfit`: where all but one model of a set failed, `bnec()` returns the
  one that worked, and that is the case where knowing what happened to the
  others matters most. It is attached only where something did fail, so
  `names()` on a fitted object is otherwise unchanged, and objects saved before
  this release report no failures rather than erroring.
- `summary()` of a `bayesnechurdlefit` reports failures per component, labelled
  `growth` or `survival`. A hurdle fit runs two independent model sets through
  `bnec()`, so a model can fail on one and fit on the other, and the summary
  otherwise printed a short model list with no explanation for it.
- `pull_out()` carries the record across. Unlike `amend()` it refits nothing,
  only subsets a set already fitted, so what `bnec()` attempted is still an
  accurate description of the object returned.
- New `get_priors()`, returning priors in the form `bnec()` accepts them, from
  either end of a fit. **Given a fit** it returns the priors that fit actually
  used, user overrides included, so `bnec(formula, data = data, prior =
  get_priors(fit))` reproduces the model. **Given a formula and data** it returns
  the priors `bayesnec` would generate, without fitting anything, so they can be
  inspected and edited before the first run — the family is chosen from the data
  and invalid models dropped exactly as `bnec()` would, because the same
  functions do it. A single model returns a `brmsprior`, a model set a named list
  of them, both directly usable as `prior =`. The two entry points answer
  different questions and can disagree once a prior has been overridden, which is
  documented and tested. Where a `disp()` variance function is in the formula its
  parameters come back with the curve's, from both entry points: `bnec()` takes a
  supplied prior whole, so a set returned without them would leave `brms` to put
  a flat prior on each. What is left to `brms` is the family's own dispersion
  parameter where dispersion is constant, and the linear predictor of a
  `disp(~x)` sub-model — neither is accepted by `bnec(prior = )` today. See
  [#141](https://github.com/open-AIMS/bayesnec/issues/141).
- `pull_prior()` is unchanged and still returns the whole `brmsprior` a fit
  carries — `brms` defaults, duplicated vectorized rows and all. That object is
  for looking at, and is not accepted by `bnec(prior = )`: the extra `sigma` row
  fails the parameter-name check in `make_inits()`. `?pull_prior` now says so and
  points at `get_priors()`.
- Fixed: the prior a fitted object carries could not be fed back to `bnec()`
  even once the extra rows were removed. `brms` records an absent bound as `""`
  in a fit's own `prior` slot, where `define_prior()` and `brms::prior()` use
  `NA`. All three mean unbounded, but the bound-respecting redraw in
  `make_inits()` tested with `is.na()`, so `""` was read as a bound, coerced to
  `NA` by `as.numeric()`, and then evaluated as `while (NA)` — "missing value
  where TRUE/FALSE needed". Blank bounds are now normalised before use, which is
  the second half of what `get_priors()` needed to round trip.
- `nechormepwr` and `nechorme4pwr` are now excluded up front for 0-1 bounded
  families under an identity link, and for the zero-probability block of the
  two-block families, instead of failing after minutes of fruitless
  initialisation. Their hormesis term is `x^(1 / (1 + exp(slope)))`, which
  carries no coefficient: the exponent lies in (0, 1), so at `x = 1` the term
  contributes exactly 1 whatever `slope` is, and below the threshold — where the
  decay factor is exactly 1 — the fitted mean is at least `top + 1`. No
  parameter value keeps that inside (0, 1) for a predictor that reaches 1, so
  there is nothing for a better initial-value search to find. `nechormepwr01` is
  the bounded hormesis form and is unaffected, as are `nechorme` and
  `nechorme4`, whose increment is scaled by `exp(slope)`.
- The consequence in practice: `model = "zero_bounded"` under `hurdle_gamma`
  reported 11 models and averaged over 9 — one dropped by design and one lost
  silently to an initialisation failure that cost roughly eight minutes per run.
  It now reports 9, and says why each was dropped. The message for these two
  names the reason rather than sharing the generic one, and points at the
  hormesis models that do work on a bounded response. See
  [#177](https://github.com/open-AIMS/bayesnec/issues/177).
- New families `"zero_inflated_poisson"` and `"zero_inflated_negbinomial"`, for
  count data with more zeros than the count distribution alone can account for.
  They take the ordinary single-block path: the concentration-response curve is
  fitted to `mu` and `brms` estimates a single constant `zi` alongside it. See
  [#104](https://github.com/open-AIMS/bayesnec/issues/104).
- They are deliberately **not** two-block families, which is worth stating
  because `zero_inflated_beta` is one and the names suggest they should match.
  Zero-inflation differs from a hurdle only when the base distribution can
  itself produce a zero. Neither Gamma nor Beta can, so for those the two
  coincide — every zero came from the inflation component, the likelihood
  separates exactly, and `brms` generates the hurdle density with no mixture at
  zero. Poisson and negative binomial **can** produce a zero, so the equivalence
  fails: an observed zero is evidence about both components at once, the
  likelihood carries a `log_sum_exp` over them and does not factorise. That
  rules out `bnec_hurdle()`, which is that factorisation performed as two fits.
  It does not by itself rule out a joint fit with a curve on `zi`, which `brms`
  can express; `zi` is held constant for the separate reasons `?bnec` now sets
  out — `zi` and `mu` are weakly separated exactly where `mu` is small, which is
  the end of the range that sets the threshold, and `zi` is a latent class
  rather than anything the experiment observed. The rule that follows: if you
  can tell which zeros are structural, you have a hurdle, not zero-inflation.
- Consequently `bnec_hurdle()` refuses them as `family_growth`, with an error
  saying why and pointing at `bnec(family = )` for the mixture. It also now
  refuses a two-block `family_growth`, which was previously accepted and would
  have nested one two-part model inside another. `model_survival` is refused for
  them as for any other single-block family, and `disp()` is refused with the
  reason that applies — the dispersion parameter describes the count component
  while the response is the mixture — rather than the generic "no free
  dispersion parameter", which would be wrong for `zero_inflated_negbinomial`.
- Predictions for these families are on the scale of the mixture, not of `mu`:
  `posterior_epred()` returns `mu * (1 - zi)`, so `predict()`, `fitted()`,
  `autoplot()` and `pred_vals` sit a factor `1 - zi` below the `top` and `bot`
  that `summary()` reports. `ecx()` and the no-effect threshold are unaffected,
  since a constant factor cancels from both. Documented under `?bnec`.
- Neither family is selected automatically. A count response is still read as
  `poisson`; the zero-inflated forms have to be asked for.
- Known limitation: a genuine hurdle on a *count* response needs a
  zero-truncated count family, which is not yet available
  ([#209](https://github.com/open-AIMS/bayesnec/issues/209)). `bnec_hurdle()`
  accepts a count `family_growth` but fits it untruncated to the non-zero
  subset, which overestimates the mean where the mean is small. The prior
  tables also degrade when a large share of the response is zero
  ([#210](https://github.com/open-AIMS/bayesnec/issues/210)).
- A fitted object no longer stores `pred_vals$posterior`, the
  `n_draws x resolution` matrix of posterior predictions over the plotting grid.
  It dominated the size of every fit — **31.8 MB against a 1.2 MB `brmsfit`** at
  the package defaults, 96% of the object — and the cost multiplied with the
  model set and with `iter`: `model = "all"` retained roughly 1.5 GB in a single
  `bnec()` call. The measured reproducer from the issue now returns **1.28 MB**.
  `pred_vals$data`, the small summary that `plot()` and `autoplot()` use, is
  unchanged. See [#180](https://github.com/open-AIMS/bayesnec/issues/180).
- The matrix had exactly one reader in the package, the model-averaging path in
  `expand_manec()`, which now builds each model's posterior itself and thins it
  to the weighted draws one model at a time. The weighted draws are the same.
  Nothing else read it: `predict()` did not, and the plot methods use the
  summary.
- This is a net saving in computation, not a trade. `expand_nec()` used to
  compute the matrix for every model whether or not anything needed it; it now
  computes it only where it does — for smooth (`ecx`-type) models, whose NSEC is
  read off the curve, and for the two-block families. A threshold model fitted
  on its own no longer computes it at all. The one place it is computed twice is
  a model set containing smooth models, at about 0.2 s per model against fitting
  times measured in minutes.
- **This is a breaking change for code that read the matrix off a fitted
  object.** `x$pred_vals$posterior` now returns `NULL` rather than erroring, so
  downstream code will fail a step later. Replace it with
  `posterior_epred(x, newdata = bnec_newdata(x, resolution = 1000), re_formula = NA)`,
  which returns the same thing computed on demand. Objects saved before this
  change still carry the matrix and are unaffected; every accessor works on
  both, which is tested.
- A `bayesmanecfit` no longer stores `w_pred_vals$posterior` either, the single
  `sample_size x resolution` matrix of model-weighted draws. Once the per-model
  matrices were gone this was 77% of what remained — 7.65 MB of a 9.90 MB
  five-model set, and roughly 64 MB on its own at the package defaults. Nothing
  in the package read it. `w_pred_vals$data` is unchanged, and the same
  `posterior_epred()` call above reproduces the draws, sampling the component
  models in the same weighted proportions. See
  [#213](https://github.com/open-AIMS/bayesnec/issues/213).
- A two-block fit no longer stores `hurdle$mu_pred` and `hurdle$hu_pred`, the
  two component curves. They were written on every hurdle fit and never read
  anywhere, so they are no longer computed at all — removing two
  `posterior_epred()` calls per two-block fit, and four per `bnec_hurdle()`
  pair. See [#214](https://github.com/open-AIMS/bayesnec/issues/214).
- The prediction grid is now built in exactly one place. `bnec_newdata()`,
  `expand_nec()` and the internal `posterior_on_grid()` had three separate
  copies of the same code, which disagreed on a partially specified `x_range`:
  `bnec_newdata()` silently ignored it, the other two produced `seq(NA, NA)`.
  A partially specified `x_range` is now rejected with a clear error. See
  [#211](https://github.com/open-AIMS/bayesnec/issues/211).

# bayesnec 2.1.3.6

- New *Censoring* section in `vignette("example1")`, giving the `cens()` aterm
  added in the previous version a user-facing home: the syntax, the values the
  censoring indicator takes, that the response column carries the bound rather
  than a substitute for it, and why a censored row saturates as the fitted curve
  descends where a substituted value keeps pulling it back up.
- Guidance against normalising the response to the control before fitting.
  Ritz, Gerhard & Streibig (2026) show that the conventional
  `1 - y / mean(y_control)` pre-processing step divides every observation by the
  same random quantity, which correlates them, discards the uncertainty in the
  divisor and — by Jensen's inequality — biases effective doses upwards: for an
  ED10 with six controls, 6.8% bias and 26.4% CV against 2.1% and 12.7% from the
  raw response, with nominal 95% intervals covering at 88–91%. Nothing is gained
  by it, because `ecx(type = "absolute")` — already the default — measures the
  decline relative to the *fitted* control value one posterior draw at a time,
  which is the estimand that paper recommends. Documented in `?bnec` and in a new
  *Preparing the response* section of `vignette("example1")`, which also covers
  why dividing by the observed maximum is worse than dividing by the control
  mean and what to do when the Beta families make a divisor unavoidable.
- `bnec()` now detects both practices and says so. Dividing by the observed
  maximum leaves a maximum of exactly `1` attained by exactly one observation;
  dividing by the control mean leaves the control observations averaging to
  exactly `1`. Either emits a message pointing at the paper. The first check is
  suppressed where the response lies on a rational grid, because genuine
  count-derived proportions such as 19/20 surviving produce a single observation
  at `1` often enough that the unguarded check fires on up to 38% of simulated
  fertilisation assays. With the guard, neither check produced a false positive
  in 2000-draw simulations across eight genuine-data scenarios or in a sweep of
  1269 columns and column pairs from 33 real datasets, in which it did correctly
  flag four normalised datasets. See `notes/normalisation_detection.md`.
  See [#173](https://github.com/open-AIMS/bayesnec/issues/173).

- `bayesnecformula` now supports the `brms` `cens()` aterm, so a response that is
  rounded at the recording resolution, reported below a limit of detection, or
  deliberately coarsened at a boundary can be fitted as censored rather than
  substituted. Both the `cens(indicator)` and the interval
  `cens(indicator, upper_bound)` forms are carried through `check_formula()` into
  `model.frame()` output and registered in the `bnec_pop` attribute as
  `cens_var` and `cens_y2_var`, alongside `trials_var`. Previously the term was
  accepted with a warning and then silently dropped, leaving an ordinary
  uncensored fit. See
  [#181](https://github.com/open-AIMS/bayesnec/issues/181).
- A censored row is exempt from the boundary shifts `bnec()` applies to zeros in
  Gamma and Beta responses and to ones in Beta responses. The recorded value on
  a censored row is a declared bound, not a boundary artefact, so shifting it
  would restate the bound the user chose. Uncensored rows are shifted exactly as
  before. Where a row is censored at a value the family excludes — left-censored
  at 0 under Gamma or Beta, right-censored at 1 under Beta — `bnec()` now stops
  with an explanation instead, since the censored likelihood contribution there
  is `F(0) = 0` and Stan would otherwise fail at initialisation with nothing to
  point at.
- Fixed `bayesnecformula` losing any two-argument aterm written on its own. The
  aterm chain was split by treating every length-three call as an `a + b` pair,
  which destructured the aterm call itself, so `y | cens(indicator, upper_bound)`
  parsed as though no aterm had been given. Only formulas with a single
  two-argument aterm were affected.
- `check_formula()` no longer reports `cens()` as an unvalidated aterm, and now
  warns when a `cens()` term contains no variable — as in `cens("left")`, which
  `brms` recycles into a declaration that every row of the response is censored.
- New dataset `alga`: growth inhibition tests on *Cladocopium proliferum* and
  *Rhodomonas salina* against two contaminants, consolidated from four tests.
  Cell density is counted to a resolution of 10, so a recorded density of zero
  is a censored count rather than an absence, and the source substituted a
  growth rate of zero for those cultures — placing total loss of the population
  mid-range, above every genuinely negative value, and turning a monotonic
  concentration-response into a non-monotonic one. Both features are retained
  rather than cleaned away, for
  [#173](https://github.com/open-AIMS/bayesnec/issues/173) and
  [#181](https://github.com/open-AIMS/bayesnec/issues/181).

- Vignette figures are now written as png rather than pdf. Every vignette is an
  `rmarkdown::html_vignette`, but the five older ones set `dev = "pdf"`, so
  pandoc embedded each figure with `<embed type="application/pdf">` and browsers
  rendered it through their built-in pdf viewer — inside a dark panel with its
  own page-number box, zoom controls and toolbar, at whatever size the viewer
  chose rather than the requested width. `example2b` was worse: it hand-writes
  its two theoretical-curve figures as `<img src="....pdf">`, which browsers do
  not render at all. This affected the published documentation, not just locally
  built vignettes. `example6` already used the png device and is unchanged. The
  LaTeX article keeps vector figures, now taken from the pdf copies
  `article/render_tex_pdf.R` writes alongside it rather than from the vignettes.
  See [#178](https://github.com/open-AIMS/bayesnec/issues/178).

# bayesnec 2.1.3.5

- New vignette, *Hurdle and zero-inflated concentration-response models*
  (`vignette("example6")`), covering the two implementations of these models,
  what the combined endpoint gives that a survivors-only analysis cannot, when
  a hurdle Beta is appropriate, and — using the shipped `herbicide` data as a
  counter-example — how to tell a structural zero from a rounded or floored one
  before choosing a model. The hurdle Beta section carries a simulated worked
  example, constructed to satisfy all four of the conditions the section sets
  out; it is simulated because no real dataset meeting them could be found, and
  the section retains the argument for why the mass-at-1 condition is the one
  that keeps failing. See
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

# bayesnec 2.1.3.4

- Added a `timeout` argument to `bnec()` and `amend()` to cap the time allowed
  for any single model fit, so that a model with highly divergent (slow) chains
  can be abandoned while the remaining models still fit
  ([#157](https://github.com/open-AIMS/bayesnec/issues/157)).

# bayesnec 2.1.3.3

- Fixed initialisation failure for 0, 1 bounded families under an identity
  link. `response_link_scale()` handled only the `logit` and `log` links, so
  under `link = "identity"` the response was returned unchanged — exact `0`s
  and `1`s included. Those bounds propagated into the init-finder's range
  check, which a decaying curve can essentially never satisfy, and into the
  likelihood itself, where a `beta_binomial` mean of exactly `0` gives an
  infinite log-probability. Every initialisation attempt was rejected. The
  identity link now applies the same clamping the `logit` branch already did.
  See [#162](https://github.com/open-AIMS/bayesnec/issues/162).
- `make_good_inits()` gains a rescue step. When a full random draw falls
  outside the valid response range, individual parameters are re-drawn from
  their own priors while the rest are held fixed, rather than the whole trial
  being counted as a loss. Models that previously fell back to Stan's random
  initialisation — `nechorme` among them — are now reliably initialised.

# bayesnec 2.1.3.2

- Added a `prior_type` argument to `bnec()` and `amend()` for selecting the set
  of default priors. The default, `"uninformative"`, reproduces the
  weakly-informative priors described in Fisher et al. (2024, JSS). The new
  `"regularizing"` option builds narrower priors, with the no-effect (`top`)
  parameter centred on the control mean (the upper end of the response range
  for these monotonically decreasing models).
- `bnec()` now takes `prior` as an explicit argument (previously passed through
  `...`). This ensures a user-supplied `prior =` is matched exactly rather than
  being partial-matched to the new `prior_type` argument.

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

