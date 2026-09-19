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
                                     alpha = 0.05) { ... }
```

For each block, take the two highest distinct predictor levels, `x_k` and
`x_{k-1}`. Let `d = mean(y at x_{k-1}) - mean(y at x_k)` on the same response
scale the default priors are built from (proportions for the binomial families,
per unit exposure for a `rate()` fit, each hurdle block separately). Let `se` be
the standard error of that difference, from the pooled within-level variance of
the two levels where both are replicated, falling back to the variance pooled
over all levels where they are not. Report where `d / se(d) > t(1 - alpha, df)`,
one-sided: where the response is still declining at the top of the series.

The level is stated rather than a multiple of the standard error, so the number
in the code has a meaning. On a design whose top is flat the true difference is
zero and the rule reports at `alpha`, which is its false-positive rate by
construction; phase 3 measures the realised rate rather than choosing a
threshold from it.

The local variance is preferred over the pooled one wherever both levels are
replicated, because on proportions and counts the variance is not homogeneous
across the series. Pooling would overstate the standard error exactly where the
response has flattened, and the rule would then report an incomplete design less
often than its level says.

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
- No replication within either of the two highest levels: the local variance is
  undefined, so the pooled one over all levels is used, with `df = N - L`. Where
  the design is wholly unreplicated, report that the check could not be applied
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

Change the message text to name the remedy once phases 5 and 7 exist:

```
The response at the highest predictor value is still declining (the mean falls
by <d> between <x_{k-1}> and <x_k>, against a standard error of <se>). The
lower asymptote may not be identified by this design. See ?bnec for
`asymptote_observed`, and ?nec for how a threshold above the tested range is
reported.
```

### 2.3 `report_nec_prior_bound()`

Phases 4 and 5 replace it. Two properties of it are worth keeping and two are
not.

Keep the assessment on the fitted scale before `xform`, with `xform` applied to
the bound for display only. A decreasing transformation would otherwise reverse
which quantile is compared against the bound. PR #387 states this and is right.

Keep reading the bound from the prior stored on the fitted object rather than
recomputing it from the data, so a user-supplied bound is handled.

Do not keep the exact-equality test, `signif(estimate, 3) == signif(bound, 3)`.
A median at 0.997 of the bound is not reported by it. §4.3 counts draws at or
above the bound instead, which needs no tolerance.

Do not keep the refusal to report for a mixed NEC/NSEC model average. That is
the common default `bnec()` path, so the report mostly does not fire where it is
wanted. §4.3 reports per component and combines, which is what
`nec.bayesnechurdlefit()` already does for the two blocks of a hurdle fit.

---

## 3. Removal of the tested-range truncation (phase 6, #393)

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
the correct statement. Phases 4 and 5 are what turn that into a report rather
than a number.

The prior's own spread still concentrates inside the tested range. `sigma` is
set so that the central 95% interval reaches the farthest concentration tested
under `"uninformative"`, and the central 98% under `"regularizing"` (#314). So
after this change roughly one part in forty of the prior mass sits above the
highest concentration on the lognormal branch. That is enough for the posterior
to be shaped by the likelihood where the data say anything, and it is not enough
on a wholly flat design, where the answer is prior-dominated by construction.
Widening it is part of phase 7 and not of this phase.

### 3.3 The lower bound

`lb = min(prior_predictor)` is removed by the same change, so a threshold below
the lowest concentration tested is admissible. That is the mirror case and it is
real: a sample toxic at every dilution has its threshold below the series.

Its reporting is in §4.4. `warn_censored_draws()` already
holds the wording, in `below_msg()` at `R/helpers.R:2087`, for an NSEC whose
curve reached the reference below the lowest concentration in the prediction
range. §4.11 should reuse it for the lower end of the `nec` posterior, and the
`extrapolate` argument should apply to both ends. #386 measured only the upper end; the
reporting covers both.

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

## 4. Censoring and `extrapolate` (phases 4 and 5, #395 and #392)

### 4.1 The realisation of the no-effect estimate

The reported N(S)EC is not computed by `nec()`. `expand_nec()` builds
`ne_posterior` at `R/expand_classes.R:111-114` and stores its summary as `ne`;
`expand_manec()` builds `w_ne_posterior` at `:403` and stores `w_ne` at `:420`.
`summary()` reads those stored summaries through `clean_nec_vals()`, and
`autoplot()` reads them again through `summ$nec_vals` at `R/autoplot.R:83`,
`:144`, `:177` and `:199`. Nothing on that path calls `nec()`.

So the censoring is recorded where the posterior is realised, and `nec()` and
`nsec()` read the record rather than deriving it. An earlier draft of this plan
put it in `nec()` alone, which would have left the number users report
unqualified.

### 4.2 The scope of the censored summary

Every estimator that can return `NA` for an out-of-range draw takes it, not only
the stored N(S)EC. `ecx()` summarises with `quantile(..., na.rm = TRUE)` at
`R/ecx.R:232`, and `nsec()` does the same at `R/nsec.R:196`, `:243`, `:314`,
`:316`, `:409` and `:424`. The draws deleted are those whose curve did not reach
the target, which are the largest, so each reported estimate is lower than the
quantity it is labelled as and its interval is narrower. Lower is the protective
direction, which is why this has not caused trouble; the narrow interval is the
more serious half.

A single `ecx`-type fit and the one-model average of it are the same quantity, so
reporting one as censored and the other as a deleted-draw summary would make
`pull_out()` change a number without changing a model. That is the argument that
settles the scope.

`nsec()` and `ecx()` build the record for the vector they return rather than
reading a stored one, because they recompute on a grid the caller may have
changed through `x_range`.

### 4.3 The two kinds of beyond-range draw

`ne_posterior` holds different quantities for the two equation classes, and they
handle the same condition in opposite ways.

For an `ecx`-type equation it is an NSEC read off the prediction grid by
`nsec_off_curve()`. A draw whose curve does not fall to the control's `sig_val`
quantile within the grid is `NA`, and `estimates_summary()` deletes it with
`na.rm = TRUE`. The reported quantiles are therefore conditional on the draws
that did reach the reference, which is lower than the quantity they are labelled
as wherever any draw did not. `nsec_off_curve()` reports the count per equation
at fit time; `expand_manec()` reports nothing for the mixture it assembles.

For a `nec`-type equation it is `b_nec_Intercept`, a sampled parameter currently
held inside the range by the prior truncation §3 removes.

After §3, one half of a model-averaged mixture would delete its beyond-range
draws while the other kept them at large values, and the effective contribution
of a component that deleted draws would be smaller than the stacking weight
allocated to it. That is why this section precedes §3 in the order of work.

### 4.4 The record

In `expand_nec()` and `expand_manec()`, on the posterior:

```r
attr(ne_posterior, "censored") <- list(
  upper = max(pred_data$x),   # the prediction grid, not max(x) of the data
  lower = min(pred_data$x),
  above = <logical, one per draw>,
  below = <logical, one per draw>
)
```

`above` is `draw >= upper` for a block that samples `b_nec_Intercept`, and for a
block read off the curve it is the `NA` draws that did not reach the reference
within the grid. `below` is the mirror, and for a curve-read block it is the
count `nsec_from_posterior()` already separates and `warn_censored_draws()`
already reports through `below_msg()`. The two ends are recorded separately
because an `NA` alone does not say which one produced it.

Both mean the same thing for the reported quantity: the no-effect value is beyond
that end of the prediction range.
`expand_manec()` combines the per-component vectors under the draw index it
already uses, so the combined fraction is the weighted one and not a count over
components.

The bounds are the prediction grid rather than the range of the data, because
that is what `ecx()` already censors at and a user who passed `x_range` to
`bnec()` meant it.

Both ends are in scope because §3 removes `lb` and `ub` in the same two lines. A
threshold below the lowest concentration tested becomes admissible at the same
moment, and admitting it without reporting it would recreate #386's defect at the
other end. That end binds on a logged predictor or a design with no control;
where the predictor is a concentration with a zero control, `lb` was already the
lognormal's own support and nothing changes.

### 4.5 The censored summary

`estimates_summary()` gains a censored form. With a censored fraction `f`, the
quantile at probability `q` is the ordinary quantile of the uncensored draws
where `q < 1 - f`, and is reported as `>= bound` otherwise.

A censored draw is never given a numeric value. It contributes its rank and
nothing else.

This is not the treatment #39 and D15 ruling 3 removed, and the distinction goes
in the code comment at `estimates_summary()` so that the two are not later
confused. That treatment assigned `max(x_vec)` to a censored draw and used it as
a number, so it raised the point estimate without saying so. Here a quantile
falling among the censored draws is reported as a bound rather than as a number,
and the fraction is stated.

`summary()` and `print()` show a censored entry as `>= <bound>` at the upper end
and `<= <bound>` at the lower. Where only the upper interval limit is censored,
the estimate prints normally and the limit prints as `>= <bound>`. `autoplot()`
draws a censored N(S)EC as a bound rather than as a line with an interval.

### 4.6 `extrapolate`

```r
nec(object, posterior = FALSE, xform = identity,
    prob_vals = c(0.5, 0.025, 0.975), extrapolate = FALSE, ...)

nsec(object, sig_val = 0.01, resolution = 200, x_range = NA,
     xform = identity, prob_vals = c(0.5, 0.025, 0.975),
     extrapolate = FALSE, ..., dpar = NULL)
```

| value | behaviour |
|---|---|
| `FALSE` (default) | report the censored summary of §4.5 |
| `TRUE` | no censoring at either end; the limit is infinite |
| a number | the upper extrapolation limit; censor at that value instead |
| a pair of numbers | the lower and upper limits |

`TRUE` is accepted only where every component of the reported estimate samples a
NEC, which `ne_type` records. A curve cannot be read off an infinite grid, so
where any component is an NSEC, `TRUE` is an error naming the finite form. On the
default `bnec()` set, which mixes both classes, that is the common case.

A finite limit does both jobs on a mixed set. The NSEC components are recomputed
on a grid extended to it, through the `x_range` argument `nsec()` and `ecx()`
already take at `R/nsec.R:129` and `R/ecx.R:128`, which recompute from the stored
fit and need no refit. The NEC components are released to it. The roxygen states
that the first of those re-evaluates the curve and so takes time proportional to
`resolution`.

A number below the current grid bound is an error naming `x_range`, not a silent
tightening: it would report an estimate as censored at a value the fit had no
trouble identifying. The same applies to a lower limit above the grid's lower
end.

### 4.7 The difference between capping and dropping

The two are both called censoring and they are not the same operation, so the
difference is stated in the roxygen of `nec()`, `nsec()` and `ecx()`.

`ecx()` reads its estimate off a fitted curve. A draw whose curve never reaches
the target has no ECx in the range, and may have none at any concentration, so
`NA` is the correct value and the draw is excluded. Capping it at the grid bound
would assert that its ECx equals that bound, which is a statement the draw does
not make.

`nec()` reads a sampled parameter. A draw whose `nec` is above the grid bound has
a value, and that value is known to exceed the bound, so setting it to the bound
is right-censoring in the ordinary sense.

§4.5 unifies the two at the reporting stage by counting both as "at or above the
bound" without giving either a number, which is available to both because neither
claims a value for a censored draw.

### 4.8 Order of operations

Censor on the fitted predictor scale, then apply `xform`. PR #387 established
this and the reason holds: a decreasing `xform` would map the upper tail to the
lower one and the comparison against the bound would select the wrong quantile.
`xform` applies to the bound for display only.

### 4.9 The hurdle route

`expand_nec()` builds `combined_ne` as `pmin()` of the two blocks where both are
threshold blocks, `R/expand_classes.R:138`, and off the combined curve otherwise.
`pmin()` propagates `NA`, so a censored draw in either block censors the
combination. That is correct and needs recording rather than changing.

### 4.10 Interaction with a still-truncated prior

Between these phases and §3, every fit has a `nec` prior bounded at the grid, so
`extrapolate = TRUE` returns a truncated posterior and is not an extrapolation.
Detect it by comparing the grid bound against the `ub` on the stored `nec` prior
and message that the fit itself is bounded. Afterwards the same applies to a fit
made by an earlier version or with a user-supplied bound.

### 4.11 Tests

`tests/testthat/test-summary.R`, `test-autoplot.R`, `test-nec.R`, `test-nsec.R`
and `test-ecx.R`:

- a single `ecx`-type fit with censored draws: `summary()` prints `>= bound` and
  states the fraction;
- a model-averaged set mixing `nec` and `ecx` equations: the combined fraction is
  the weighted one;
- a hurdle fit where one block is censored and the other is not;
- `nsec()` and `ecx()` on a fit with out-of-range draws: the censored summary,
  agreeing with what `summary()` prints for the same object;
- a single `ecx`-type fit and the one-model average of it: the same number, which
  is what `pull_out()` must not change;
- a draw beyond the lower end of the grid: recorded as `below` and printed as
  `<= bound`;
- `extrapolate = TRUE` on a pure NEC set: the posterior unaltered;
- `extrapolate = TRUE` on a mixed set: an error naming the finite form;
- a finite limit on a mixed set: the NSEC components recomputed on the extended
  grid, the NEC components released, and the combined fraction falling;
- a number below the grid bound, and a lower limit above the grid's lower end:
  an error naming `x_range`;
- a decreasing `xform`: the same draws censored as under `identity`, the bound
  reported transformed;
- a fit with no censored draws: output identical to the current release, which is
  the regression guard for every existing analysis.

### 4.12 The behaviour change

Censoring the per-equation summaries changes the reported NSEC and ECx for every
existing fit with any out-of-range draw, the vignettes included. RF accepted that
on 2026-09-19, on the condition that its size is measured on the vignette fits
and recorded in `NEWS.md` before the change merges, rather than afterwards.

A fit with no out-of-range draw is unaffected, and the last test above is the
guard for that.

## 5. The incomplete-design prior set (phase 7, #394)

### 5.1 The declaration is the user's

The data cannot separate a response that is flat because the asymptote is high
from one that is flat because the series stopped early. The flatness rule of
§2.2 detects that the response is still declining, which is evidence the design
is incomplete; it cannot detect that a flat design is complete. So the check
prompts and the user declares.

This also makes each half defensible on its own. The check may be approximate,
because its only consequence is a prompt. The prior change is never applied
without the user asking for it, so no released analysis changes.

### 5.2 The declaration argument

`bnec()` gains:

```r
bnec(formula, data, ..., prior_type = "uninformative", asymptote_observed = TRUE)
```

`asymptote_observed = FALSE` states that the lower asymptote was not observed. It
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

The name states exactly what the argument changes. After §3 the threshold priors
are no longer truncated for anyone, so this argument governs the `bot` prior and
the initial-value band and nothing else, both of which are the lower asymptote. A
name such as `response_complete` would promise control over the `nec` and `ec50`
priors that it does not have. `prior_type` does not exist on CRAN, so nothing
constrains the choice.

### 5.3 The `bot` entry

Under `asymptote_observed = FALSE`, one rule replaces the location-and-spread
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

So `asymptote_observed` is plumbed to `make_good_inits()` as well, and under
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

Report it and do not alter the set. Whether the response can reach the support
floor is a property of the endpoint, which the user knows and the package cannot
infer: for a lethality endpoint zero is attainable and `nec3param` is the right
equation, and for a growth or photosynthetic yield endpoint it usually is not.
Silently dropping the group would decide a modelling question on the user's
behalf, and silently keeping it leaves a model average whose weights are
uninformative on the point at issue.

So the message states both branches, because the data decide neither:

```
This set mixes equations that estimate a lower asymptote with ones that assert
the response falls to <floor>. With the asymptote unobserved the fit cannot
distinguish them. Restrict the set to mod_groups$bot_free if the response can
reach <floor> for this endpoint, or away from it if it cannot.
```

### 5.6 Tests

`tests/testthat/test-define_prior.R`, on simulated designs whose response stops
at a stated fraction of its range, asserting direction rather than digits:

- Under `asymptote_observed = FALSE` the `bot` prior's 97.5th percentile is at or
  near the endpoint mean and its 2.5th percentile is at or near the floor, for
  each family in the table of §5.3.
- Under the default the entries are bit-identical to the current ones, for both
  `prior_type` values. This is the regression guard for every released analysis.
- A gaussian response spanning negative values under `asymptote_observed = FALSE`
  is refused with a message naming `prior`.
- `get_priors()` and `amend()` honour the argument.
- The initial-value band under `asymptote_observed = FALSE` extends to the floor,
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
corrected as part of phase 7, and the correction is noted here so it is not lost
if phase 7 is deferred.

`bnec_hurdle()` refuses an `NA` response up front and primes its second block
from one survival proportion per concentration. The flatness rule must be
checked against that block, which PR #387 already does through
`split_hurdle_response()`. Nothing further is planned for it.

`nsec()` is unchanged. It reads its estimate off the curve like `ecx()` and
already censors to the prediction grid.
