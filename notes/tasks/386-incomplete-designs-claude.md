# Implementation specification for designs that do not reach the lower asymptote (#386)

Companion to `386-incomplete-designs-human.md`, which states each decision below
in one sentence for a reader who will not implement it. This document holds the
evidence, the signatures, the edge cases and the rejected alternatives.

Written 2026-09-19. Line references are against `dev` at `76d07a13`.

---

## 1. Terms

**Incomplete design.** A concentration-response experiment whose highest
concentration has not reached the lower asymptote of the curve. The term covers
a whole effluent toxicity dilution series set by the sample available, and any
design capped by a solubility limit, a supply limit or an ethics constraint.

**Censored estimate.** A reported estimate that is known to lie at or beyond a
bound, and is reported as that bound with the direction stated, rather than as a
point. `bayesnec` already uses the term this way in `warn_censored_draws()`,
`R/helpers.R:2084`, and in the `bayesnec_censored` warning class.

**Extrapolation limit.** The largest predictor value `nec()` will return when
the caller asks for an uncensored estimate. Infinite by default, meaning the
posterior is returned as sampled.

**The flatness rule.** The test introduced in §2.2, which asks whether the mean
response is still declining between the two highest predictor levels, measured
against the within-level standard error.

---

## 2. Items from the review of PR #387 (phases 1 and 2, #389 and #390)

### 2.1 The rate denominator in the default priors

`fit_bayesnec()` at `R/fit_bayesnec.R:59-64` builds `response` for
`add_brm_defaults()` from `y` directly, dividing only by `trials` for the
binomial families. A `rate()` denominator is not applied.

`amend()` does apply it, at `R/amend.R:326-332`, with the recorded reason that
the priors must be on the scale the original fit used (#136).

Under the identity link `bnec()` assigns, brms writes a rate denominator
multiplicatively on the response scale, so the mean is the rate and `top`, `bot`
and `nec` are counts per unit exposure. This is stated in the `rate()` section
of `?bayesnecformula`. The defaults are therefore built on counts while the
parameters they describe are rates, and the two differ by the exposure.

The fix is the three lines PR #387 already contains:

```r
denominator <- retrieve_var(bdat, "rate_var")
# and, after check_data(), denominator <- checked_df$mod_dat$denom
if (!is.null(denominator)) {
  response <- response / denominator
}
```

It is correct and it belongs in its own pull request, because it changes the
prior, the initial-value band and therefore the posterior of every poisson and
negbinomial `rate()` fit. PR #387 contains it with no NEWS entry, no mention in
the pull request body, and no test asserting the resulting prior. Its only rate
test exercises `check_response_range()`.

Required with it:

- A test in `tests/testthat/test-define_prior.R` asserting the `top` and `bot`
  entries for a `rate()` fit with a non-unit denominator, against the entries
  the same data produce with the denominator divided out by hand.
- A test that `bnec()` and `amend()` produce the same default prior for the same
  `rate()` data. That equality is the defect's own statement and nothing
  currently asserts it.
- A NEWS entry under a behaviour-change heading, naming the affected families.
- The before and after entries for one worked example in the pull request body.

### 2.2 The flatness rule

`check_response_range()` in PR #387 computes `1 - mean(y at max(x)) / mean(y at
min(x))` and warns below 0.5.

Two defects.

It measures the magnitude of the effect and not whether the response has
flattened. A sublethal endpoint whose true `bot` is 0.6 of the control is a
complete design that will warn on every fit. A design that has fallen 92% and is
still declining steeply is incomplete and will not warn. The #386 measurements
do not separate the two either, because every design there was simulated with a
true `bot` of 0.1: a low declared effect and an unobserved asymptote are
confounded in that simulation.

It reads the control as the denominator, so on a hormesis design, where the
response rises before it falls, the declared decline is measured against a level
that is not the peak and the check over-reports. #386 raised this as an open
question and PR #387 does not address it.

The replacement:

```r
# R/check_data.R
check_response_flattened <- function(data, family, group = NULL,
                                     blocks = c("response", "survival"),
                                     se_multiple = 2) { ... }
```

For each block, take the two highest distinct predictor levels, `x_k` and
`x_{k-1}`. Let `d = mean(y at x_{k-1}) - mean(y at x_k)` on the same response
scale the default priors are built from (proportions for the binomial families,
per unit exposure for a `rate()` fit, each hurdle block separately). Let `se` be
the standard error of that difference, from the pooled within-level variance of
the two levels. Report where `d > se_multiple * se`, that is, where the response
is still declining at the top of the series.

Properties this gives:

- It never reads the control, so a hormesis design is assessed correctly with no
  special case.
- It is scale-free in the sense that matters: it compares a decline against the
  noise in that decline, not against an arbitrary fraction.
- It is defined where the control mean is not positive, which the PR #387 rule
  is not. That rule returns `NA` and skips silently, removing the check for
  gaussian responses on log-ratio or centred scales, which the package supports
  (#229).

Edge cases:

- Fewer than two distinct predictor levels: return without reporting.
- No replication within either of the two highest levels: `se` is undefined.
  Fall back to the pooled within-level variance over all levels, and where the
  design is wholly unreplicated, report that the check could not be applied
  rather than passing silently.
- A single distinct response value at both levels: `d` is zero and the design
  passes, which is correct.

`warning()` against `message()` is decided by the measured false-positive rate
from phase 3, not by preference. Use `message()` unless that rate on complete
designs is below 5%.

Keep, unchanged from PR #387:

- The placement. Raised once in `bnec()` before the equation loop, and once in
  `bnec_group()` before the level loop, with the private marker that stops the
  inner calls repeating it. That is the rule in `bayesnec/CLAUDE.md` and PR #387
  applies it correctly.
- `uses_response_range_defaults()`. Suppressing the report only where every
  sensitive parameter row is user-supplied is right, and the partial-prior case
  it handles is real, because `fill_missing_priors()` fills the omitted rows
  from the same defaults.
- The per-block treatment of a hurdle fit.

Change the message text to name the remedy once phase 6 exists:

```
The response at the highest predictor value is still declining (the mean falls
by <d> between <x_{k-1}> and <x_k>, against a standard error of <se>). The
lower asymptote may not be identified by this design. See ?bnec for
`response_complete`, and ?nec for how a threshold above the tested range is
reported.
```

### 2.3 `report_nec_prior_bound()`

Phase 4 replaces it. Two properties of it are worth keeping and one is not.

Keep the assessment on the fitted scale before `xform`, with `xform` applied to
the bound for display only. A decreasing transformation would otherwise reverse
which quantile is compared against the bound. PR #387 states this and is right.

Keep reading the bound from the prior stored on the fitted object rather than
recomputing it from the data, so a user-supplied bound is handled.

Do not keep the exact-equality test, `signif(estimate, 3) == signif(bound, 3)`.
A median at 0.997 of the bound is not reported by it. Phase 4 counts draws at or
above the bound instead, which needs no tolerance.

Do not keep the refusal to report for a mixed NEC/NSEC model average. That is
the common default `bnec()` path, so the report mostly does not fire where it is
wanted. Phase 4 reports per component and combines, which is what
`nec.bayesnechurdlefit()` already does for the two blocks of a hurdle fit.

---

## 3. Removal of the tested-range truncation (phase 5, #393)

### 3.1 What changes

`R/define_prior.R:1277-1281`:

```r
pr_nec <- prior_string(x_pr, nlpar = "nec",
                       lb = min(prior_predictor), ub = max(prior_predictor))
pr_ec50 <- prior_string(x_pr, nlpar = "ec50",
                        lb = min(prior_predictor), ub = max(prior_predictor))
```

becomes, with the bound taken from the support of the prior distribution rather
than from the data:

```r
# The distribution predictor_prior() returns is proper on its own support, so
# the posterior stays proper with no truncation. The lower bound is kept only
# where the distribution requires it: a lognormal has zero density below zero,
# and brms would otherwise declare an unconstrained parameter whose every
# negative proposal Stan rejects. See predictor_prior() for which branch is
# which, and section 3.2 for what the bound was doing.
x_lb <- if (grepl("^lognormal", x_pr)) 0 else NA
pr_nec <- prior_string(x_pr, nlpar = "nec", lb = x_lb)
pr_ec50 <- prior_string(x_pr, nlpar = "ec50", lb = x_lb)
```

`predictor_prior()`, `R/define_prior.R:950`, returns `lognormal(mu, sigma)` on a
predictor supplied as a concentration and `normal(mu, sigma)` on one already
logged. The lognormal is supported on `(0, Inf)` and the normal on the whole
line, and a logged concentration may legitimately be negative, so the branch
test above is the right one and is not a proxy for the sign of the data.

### 3.2 Why the bound is not replaced with a wider one

Relaxing the truncation does not identify a threshold above the series. It
changes the value the posterior piles against from `max(x)` to whatever bound
replaces it. Choosing a finite replacement, such as a multiple of the highest
concentration, would put an invented number into the output where the truncation
at least came from the design.

What the removal does buy is the shape of the posterior. At present a threshold
above the series produces a spike at `max(x)` and a narrow credible interval,
which reads as a precise estimate. Without the truncation the posterior spreads
over the region the data cannot distinguish, and the interval is wide, which is
the correct statement. Phase 4 is what turns that into a report rather than a
number.

The prior's own spread still concentrates inside the tested range. `sigma` is
set so that the central 95% interval reaches the farthest concentration tested
under `"uninformative"`, and the central 98% under `"regularizing"` (#314). So
after this change roughly one part in forty of the prior mass sits above the
highest concentration on the lognormal branch. That is enough for the posterior
to be shaped by the likelihood where the data say anything, and it is not enough
on a wholly flat design, where the answer is prior-dominated by construction.
Widening it is part of phase 6 and not of this phase.

### 3.3 The lower bound

`lb = min(prior_predictor)` is removed by the same change, so a threshold below
the lowest concentration tested is admissible. That is the mirror case and it is
real: a sample toxic at every dilution has its threshold below the series.

The reporting for it is not built in this plan. `warn_censored_draws()` already
holds the wording, in `below_msg()` at `R/helpers.R:2087`, for an NSEC whose
curve reached the reference below the lowest concentration in the prediction
range. Phase 4 should reuse it for the lower end of the `nec` posterior, and the
`extrapolate` argument should apply to both ends. Flagged in the human document
as an open decision because #386 measured only the upper end.

### 3.4 What must be measured

Against the phase 3 audit, before and after:

- Every complete-design cell: the truncated prior CDF at the true `nec` and
  `ec50` must not change by more than the Monte Carlo error of the audit. The
  truncation is not binding on those designs, so a change there is a defect in
  this phase.
- Every incomplete-design cell: the CDF at the truth must no longer be exactly
  1.000.
- The initial-value search. `make_inits()`, `R/inits_functions.R:156`, draws
  from the priors including their bounds, so removing the upper bound widens the
  `nec` draws, and `check_init_predictions()` rejects any whose curve falls
  outside the band. Record the number of proposals the search makes on each
  complete design before and after. A rise there must be quantified rather
  than assumed absent.

### 3.5 Rejected alternatives

*Keep the truncation and report the estimate as censored.* This was the option
PR #387 took. It leaves the posterior itself wrong: a truncated prior does not
merely bound the estimate, it concentrates mass at the bound, so the reported
interval is narrow as well as displaced. Reporting a narrow wrong interval as
censored is an improvement on reporting it unqualified, and it is not the fix.

*Replace `max(prior_predictor)` with a multiple of it.* Rejected in §3.2.

*Require the user to supply the bound on an incomplete design.* Rejected because
it makes the common case an error. The bound is available to a user who wants it
through the `prior` argument, which is where a user-supplied bound belongs.

---

## 4. Censoring and `extrapolate` (phase 4, #392)

### 4.1 The censoring

`nec()` gains an `extrapolate` argument on the generic and both methods,
`R/nec.R:73`, `:92` and `:147`:

```r
nec(object, posterior = FALSE, xform = identity,
    prob_vals = c(0.5, 0.025, 0.975), extrapolate = FALSE, ...)
```

The bound is `max(object$pred_vals$data$x)` for a `bayesnecfit`, which is the
upper end of the prediction grid `expand_nec()` built, `R/expand_classes.R:116`.
It is the value `ecx()` already censors at, so the two report against the same
number and a user who widened `x_range` at fit time gets a correspondingly wider
`nec()`. For a `bayesmanecfit`, take it from the component fits and require them
to agree; where they do not, report per component.

With `extrapolate = FALSE`, draws at or above the bound are set to the bound.
The returned estimate then gains the attribute:

```r
attr(nec_estimate, "censored") <- list(bound = bound,
                                       fraction = mean(draws >= bound),
                                       direction = "above")
```

and a classed warning is raised through the existing mechanism:

```r
warning(structure(class = c("bayesnec_censored", "warning", "condition"),
                  list(message = msg, call = NULL)))
```

so that a method summarising several components can muffle the inner reports
with `without_censored_warning()`, `R/helpers.R:2123`, exactly as
`nec.bayesnechurdlefit()` already does.

Order of operations, which is not negotiable: censor on the fitted predictor
scale, then apply `xform`. PR #387 established this and the reason holds — a
decreasing `xform` would otherwise map the upper tail to the lower one and the
comparison against the bound would select the wrong quantile.

### 4.2 `extrapolate`

| value | behaviour |
|---|---|
| `FALSE` (default) | censor at the upper end of the prediction grid |
| `TRUE` | no censoring; the extrapolation limit is infinite and the posterior is returned as sampled |
| a number | censor at that value instead of the grid bound |

A numeric value below the grid bound is an error, not a silent tightening: it
would report an estimate as censored at a value the fit had no trouble
identifying, and the user has almost certainly confused it with `x_range`.

`extrapolate = TRUE` on a fit whose prior is still truncated returns the
truncated posterior, which is not an extrapolation. Detect that case, by
comparing the grid bound against the `ub` on the stored `nec` prior, and message
that the fit itself is bounded so the argument has nothing to release. This
matters for the phase ordering: between phase 4 and phase 5 every fit is in that
state, and afterwards only fits made by an earlier version or with a
user-supplied bound are.

### 4.3 Why `nec()` caps and `ecx()` drops

The two are both called censoring and they are not the same operation, so the
difference is stated here and in the roxygen.

`ecx()` reads its estimate off a fitted curve on a grid. A draw whose curve
never reaches the target has no ECx in the range, and may have none at any
concentration, so `NA` is the correct value and the draw is excluded. Capping it
at the grid bound would assert that its ECx equals that bound, which is a
statement the draw does not make.

`nec()` reads a sampled parameter. A draw whose `nec` is above the grid bound
has a value, and that value is known to exceed the bound, so setting it to the
bound is right-censoring in the ordinary sense and the resulting quantile is a
censored quantile. Where more than half the draws are censored the median is
exactly the bound, and the correct report is that the estimate is at or above
it.

### 4.4 The printed form

`summary()` and `print()` show a censored estimate as `>= <bound>` rather than
as a number, so that a value at the bound cannot be read as a point estimate.
`autoplot()`'s N(S)EC annotation takes the same treatment. Where only the upper
interval limit is censored, the estimate prints normally and the upper limit
prints as `>= <bound>`.

### 4.5 Tests

`tests/testthat/test-nec.R`:

- A fit whose posterior lies wholly above the bound: estimate equals the bound
  at all three quantiles, `censored` attribute records a fraction of 1, one
  `bayesnec_censored` warning is raised.
- The same fit under `extrapolate = TRUE`: the posterior is returned unaltered
  and no censoring warning is raised.
- `extrapolate = <number>` above the grid bound: censoring at that number.
- `extrapolate = <number>` below the grid bound: an error naming `x_range`.
- A decreasing `xform`: the same draws are censored as with `identity`, and the
  reported bound is the transformed one.
- A `bayesmanecfit` whose components have different grid bounds: reported per
  component rather than refused.
- A fit with no censored draws: no warning, no attribute, output identical to
  the current release.

---

## 5. The incomplete-design prior set (phase 6, #394)

### 5.1 The declaration is the user's

The data cannot separate a response that is flat because the asymptote is high
from one that is flat because the series stopped early. The flatness rule of
§2.2 detects that the response is still declining, which is evidence the design
is incomplete; it cannot detect that a flat design is complete. So the check
prompts and the user declares.

This also makes each half defensible on its own. The check may be approximate,
because its only consequence is a prompt. The prior change is never applied
without the user asking for it, so no released analysis changes.

### 5.2 A separate argument, not a third `prior_type`

`bnec()` gains:

```r
bnec(formula, data, ..., prior_type = "uninformative", response_complete = TRUE)
```

`response_complete = FALSE` states that the lower asymptote was not observed. It
is plumbed to `define_prior()` and to `make_good_inits()` alongside
`prior_type`, through `fit_bayesnec()` and `add_brm_defaults()`, and to
`get_priors()` and `amend()` so that the generated entries can be inspected
without fitting.

`prior_type` selects how strongly to regularise, given a design assumed
complete. Whether the design is complete is an orthogonal question. A third
`prior_type` value would mean a user with an incomplete design could not also
have the `"regularizing"` set, and would mean writing the incomplete adjustment
twice, once into each existing branch. As a separate argument it is one
adjustment applied to whichever branch produced the entry.

One mechanical trap either way. `R/define_prior.R:1537` reads
`if (prior_type == "regularizing") ... else 1`, and every such `else` in the
package currently means `"uninformative"`. A third value would be absorbed by
them silently. Grep for `prior_type ==` before touching any of this.

The name is not settled. `asymptote_observed = TRUE` is the alternative and is
more precise about what is being declared. `prior_type` does not exist on CRAN,
so neither name has a compatibility claim on it.

### 5.3 The `bot` entry

Under `response_complete = FALSE`, one rule replaces the location-and-spread
construction for `bot`, on every family:

> The central 95% of the `bot` prior spans from the support floor to the mean
> response at the highest predictor level.

The endpoint mean is an upper bound on `bot` on such a design rather than an
estimate of it, so it becomes the top of the prior's central interval instead of
its centre.

The floor by family:

| family | floor | entry |
|---|---|---|
| `beta`, `bernoulli`, `binomial`, `beta_binomial` | 0 | beta, through the existing `beta_from_mode_sd()` |
| `Gamma`, `poisson`, `negbinomial` | 0 | gamma at shape 2, mean at half the endpoint mean |
| `gaussian`, response non-negative | 0 | `normal((e)/2, e/(2 * qnorm(0.975)))` for endpoint mean `e` |
| `gaussian`, response spanning negative values | none available | refuse |

The gamma row cannot satisfy the rule exactly. At a fixed shape of 2 the central
95% of a gamma cannot both start at zero and end at a chosen value, so the rule
is applied as "mean at half the endpoint mean", which keeps the existing
`gamma(2, 2/q)` idiom and places the 97.5th percentile near the endpoint. Record
the realised coverage per family in the phase 3 audit rather than asserting it
here.

The gaussian row with negative values has no derivable floor. Refuse, naming
`bot` and the `prior` argument, rather than inventing one. A response expressed
as a log ratio or a growth increment is ordinary input to this package (#229),
so the refusal will be reached.

The cap in `regularizing_entry()`, `R/define_prior.R:657`, holds the spread at
or below the uninformative width. It must not apply here. The new entry is
deliberately wider than both existing sets, and the cap is precisely what holds
the prior at a width that excludes the truth, as #386 states.

### 5.4 The initial-value band

`init_limits()`, `R/inits_functions.R:647`, builds the band from the observed
response and intersects it with `init_support()`. `make_good_inits()`,
`R/inits_functions.R:996`, rejects any initial curve whose predictions fall
outside it, and after 1e4 proposals hands the whole fit to Stan's own
initialisation.

A `bot` prior placed below the observed endpoint therefore produces draws the
band rejects, and the fit falls back to random initialisation on exactly the
designs the new prior set exists for. The band's own documentation already
records that no width reaches a true `bot` below the observed endpoint: coverage
was 0.160 at a width of four and 0.172 at five, against 0.003 at one.

So `response_complete` is plumbed to `make_good_inits()` as well, and under
`FALSE` the band's lower limit becomes the same floor §5.3 uses, still
intersected with `init_support()`. The upper limit is unchanged.

This is the item most likely to be missed. Its failure mode is not a wrong
number but "Initialization failed", or the fallback message from
`R/inits_functions.R:1080`, neither of which names the prior.

### 5.5 The model set

Fourteen of the 23 equations have no `bot` parameter and so assert that the
response reaches the support floor. On an incomplete design the data cannot
distinguish them from the equations that estimate `bot`, because the region that
would distinguish them was not observed. A model average over both is therefore
averaging over the assumption in question, with weights the data cannot inform.

Recommendation: report it and do not alter the set. Under
`response_complete = FALSE` with a set containing both, message that the
`bot_free` equations assert a complete decline to the support floor and name
`mod_groups` for restricting the set. Silently dropping them would decide a
modelling question on the user's behalf, and silently keeping them leaves a
model average whose weights are uninformative on the point at issue.

Open for RF's decision, recorded in the human document.

### 5.6 Tests

`tests/testthat/test-define_prior.R`, on simulated designs whose response stops
at a stated fraction of its range, asserting direction rather than digits:

- Under `response_complete = FALSE` the `bot` prior's 97.5th percentile is at or
  near the endpoint mean and its 2.5th percentile is at or near the floor, for
  each family in the table of §5.3.
- Under the default the entries are bit-identical to the current ones, for both
  `prior_type` values. This is the regression guard for every released analysis.
- A gaussian response spanning negative values under `response_complete = FALSE`
  is refused with a message naming `prior`.
- `get_priors()` and `amend()` honour the argument.
- The initial-value band under `response_complete = FALSE` extends to the floor,
  and a fit on a simulated incomplete design initialises from the search rather
  than falling back.

---

## 6. The audit (phase 3, #391)

`notes/scripts/prior_audit.R` simulates every design so that it descends to
within 5% of `bot` at the highest dose, and states that `bot` is therefore
identified in every cell. `notes/scripts/init_search_audit.R` excludes the cells
whose predictor stops short of the crossing from its width rule and reports them
separately.

Extend `prior_audit.R` with a design axis crossing the existing cells:

- complete, as now;
- incomplete, the same generating curve with the series truncated so that the
  response at the highest retained concentration is a stated fraction of the way
  from `top` to `bot`. Use the three settings #386 used, at 92%, 36% and 2% of
  the maximum effect, so that the new cells reproduce the figures that issue
  quotes.

Report, per cell, the truncated prior CDF at the true value for `top`, `bot`,
`nec` and `ec50`, as now, and add two columns the later phases need: the number
of proposals `make_good_inits()` makes, and whether the flatness rule of §2.2
reports.

The false-positive rate of the flatness rule is read off the complete cells. It
is what decides `warning()` against `message()` in phase 2, so phase 3 precedes
phase 2 in the measurement even though the code may land in either order.

Keep the script's existing provenance header convention: the commit, the R
version and the date of the run that produced the archived figures.

---

## 7. Items outside this plan

The roxygen at `R/define_prior.R:385` states that `"uninformative"` is the set
to use on a design that does not reach its asymptote. #386 measured that this is
true on the positive branch and false on the gaussian one, where the
uninformative entry held 6.43e-14 of its mass below the truth. That sentence is
corrected as part of phase 6, and the correction is noted here so it is not lost
if phase 6 is deferred.

`bnec_hurdle()` refuses an `NA` response up front and primes its second block
from one survival proportion per concentration. The flatness rule must be
checked against that block, which PR #387 already does through
`split_hurdle_response()`. Nothing further is planned for it.

`nsec()` is unchanged. It reads its estimate off the curve like `ecx()` and
already censors to the prediction grid.
