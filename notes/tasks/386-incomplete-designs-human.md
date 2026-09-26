# Plan for designs that do not reach the lower asymptote (#386)

Companion specification: `386-incomplete-designs-claude.md`. Every decision
below is stated in full there, with the evidence behind it and the alternatives
that were rejected.

Written 2026-09-19, from the measurements in #386 and a review of PR #387.

Status, 2026-09-26: complete. All seven phases reached `dev`, the last four
through PR #409 (`c3824643`), and #386 and its issues are closed. PR #387, whose
rule phase 2 replaced, closed unmerged on 2026-09-25. The follow-up work is in the
backlog run, `backlog-run-human.md`.

---

## Summary

A concentration-response experiment whose highest concentration has not reached
the lower asymptote of the curve is called an **incomplete design** in this
document. Three `bayesnec` defaults describe the design rather than the curve on
such data, and the fit reports none of it.

The plan makes three changes. The first removes the truncation that places a
threshold above the tested range outside the prior support. The second reports a
no-effect estimate that lies above the tested range as censored, and adds an
argument that returns it uncensored where the user asks for that. The third adds
a declaration that the lower asymptote was not observed, which changes the `bot`
prior and the initial-value band to match.

Two smaller items go first. One is a defect found while reviewing PR #387: the
default priors for a `rate()` fit are built from counts rather than from rates.
The other is the warning PR #387 adds, which identifies the design for the user
and is what tells them to declare it.

---

## The defects

The measurements are in #386 and are not repeated here.

The `bot` prior is located at the mean response over the highest concentrations
tested. On an incomplete design that mean sits above the true asymptote, and the
spread narrows as the response flattens, so the prior is at its narrowest where
`bot` is least identified. On the flattest simulated design the prior held
2.3e-90 of its mass below the true value.

The `nec` and `ec50` priors are truncated to the range of the predictor. A
threshold above the highest concentration tested is therefore outside the prior
support, not in its tail, and the posterior piles against the bound. The
reported credible interval is then narrow, which reads as precision and is the
opposite of what the data support.

The initial-value band is anchored on the observed response in the same way. Its
own documentation records that no width of band reaches a true `bot` below the
observed endpoint.

---

## The decisions

Each decision is stated once here and in full in the specification.

### No truncation of the prior to the tested range

Truncation is what turns a threshold above the series into a spike at the
highest concentration with a narrow interval. Specification §3.

### Censoring of the reported estimate at the prediction range

Removing the truncation without this would make `bnec()` report concentrations
above the highest tested by default, which is a change to what the package
claims. Specification §4.

### The censoring recorded where the posterior is realised

The reported N(S)EC is summarised inside `bnec()` and read from that stored
summary by `summary()`, `print()` and `autoplot()`, so a report added to `nec()`
alone would not reach the number users report. Specification §4.1.

### A single treatment for a beyond-range draw

A model-averaged N(S)EC mixes a sampled NEC with an NSEC read off the grid, and
the second currently deletes its beyond-range draws while the first cannot
produce one, so removing the truncation would leave the two halves of one mixture
handling the same condition in opposite ways. Specification §4.2.

### A contrast on the family's own mean-variance relationship

A difference of means tested with a pooled standard error assumes a variance that
does not depend on the mean, which holds for gaussian and for no other family the
package fits, and it cannot be computed at all on a hurdle survival block.
Specification §2.2.

### The censored summary applied to `nsec()` and `ecx()` as well

A single `ecx`-type fit and the one-model average of it are the same quantity, so
reporting one as censored and the other as a deleted-draw summary would make
`pull_out()` change a number without changing a model. Specification §4.2.

### Both ends of the prediction range

Removing the upper truncation removes the lower one in the same two lines, so
admitting a threshold below the lowest concentration without reporting it would
recreate this problem at the other end. Specification §4.3.

### An infinite extrapolation limit only where every component samples a NEC

A curve cannot be read off an infinite grid, so a set containing an `ecx`-type
equation requires a finite limit. Specification §4.5.

### The `extrapolate` argument on `nec()`

A user who wants the uncensored threshold asks for it by name, and the limit is
infinite unless they set one. Specification §4.2.

### The design declared by the user rather than inferred from the data

No rule computed from the response can separate a response that is flat because
the asymptote is high from one that is flat because the series stopped early, so
the package reports what it sees and the user decides. Specification §5.1.

### A separate argument rather than a third `prior_type`

`prior_type` selects how strongly to regularise a design assumed complete, and
whether the design is complete is a different question, so a third value would
prevent a user from having both. Specification §5.2.

### The name `asymptote_observed`

After the truncation is removed for everyone, this argument governs the `bot`
prior and the initial-value band and nothing else, both of which are the lower
asymptote, so the name states its whole scope. Specification §5.2.

### The `bot` prior from the support floor to the observed endpoint

On an incomplete design the endpoint mean is an upper bound on `bot` rather than
an estimate of it. Specification §5.3.

### The `bot_free` equations reported, with both branches stated

Whether the response can reach the support floor is a property of the endpoint,
which the user knows and the package cannot infer, so the message names both
ways of restricting the set rather than choosing one. Specification §5.5.

### The initial-value band under the same declaration

The band rejects an initial curve outside it, so a `bot` prior placed below the
observed endpoint would otherwise have its draws rejected and the search would
fall back to Stan's own initialisation on exactly the fits that need it.
Specification §5.4.

---

## The order of work

Seven phases, one issue each.

| phase | issue | what it changes |
|---|---|---|
| 1 | #389 | the default priors for a `rate()` fit |
| 2 | #390 | the report that a design has not flattened |
| 3 | #391 | incomplete designs in the prior audit |
| 4 | #395 | the censoring record on the no-effect posterior, and its reporting |
| 5 | #392 | `extrapolate` on `nec()` and `nsec()` |
| 6 | #393 | removal of the tested-range truncation |
| 7 | #394 | the incomplete-design prior set |

Phases 4, 5 and 6 must land in that order. Phase 6 removes a constraint that
phases 4 and 5 put the reporting in place for.

### Phase 1. The rate denominator in the default priors (#389)

`fit_bayesnec()` builds the default `top` and `bot` priors from the raw counts
of a `rate()` fit, while `amend()` divides by the denominator first. Under the
identity link the mean is the rate, so the priors are on the wrong scale by a
factor of the exposure.

This changes the fitted result of every poisson and negbinomial `rate()` model,
so it goes first and alone: every measurement in the later phases is taken
against it. It is done when the two routes produce the same prior for the same
data and a test asserts the entry.

### Phase 2. The report that a design has not flattened (#390)

A user is told, before anything is fitted, that the response at the top of the
series is still declining. Without this nobody knows to declare the design in
phase 7.

PR #387 implements this and its rule is replaced. That rule triggers on the
magnitude of the effect, which does not separate an incomplete design from a
complete one with a high asymptote. The replacement compares the two highest
concentrations tested, as a one-sided contrast in a generalised linear model that
uses the fitted family's own mean-variance relationship, at a stated significance
level. It never reads the control, which also answers the question #386 raised
about the hormesis equations.

The replacement is what makes the check computable on a hurdle fit at all. The
survival block holds one proportion per concentration, so any rule built on
replication within a concentration is undefined there, and a survival curve still
falling at the undiluted end is the commonest incomplete design in whole effluent
testing. That block takes a binomial contrast on the counts instead, where the
denominator supplies the information replication would have.

It is done when the check reports on the simulated incomplete designs and on a
hurdle design whose survival is still falling, its realised false-positive rate
on the complete designs of phase 3 agrees with its stated level, and it is raised
once per `bnec()` call rather than once per equation.

### Phase 3. Incomplete designs in the prior audit (#391)

`notes/scripts/prior_audit.R` simulates every design so that it descends to
within 5% of `bot`, so it cannot measure any of this. It is extended with
incomplete cells before the priors change, so that the later phases have a
baseline and can show the complete-design cells did not regress.

It also measures the two rates the phase 2 rule needs. The complete cells give
its false-positive rate, which should agree with its stated level. The incomplete
cells give its miss rate, which is what decides whether two approximations in the
variance handling are tolerable or whether `MASS` belongs in `Suggests`.

It is done when the script reports the truncated prior CDF at the true value for
complete and incomplete designs side by side, reproduces the figures #386 quotes,
and reports both rates.

### Phase 4. The censoring record on the no-effect posterior (#395)

The no-effect posterior records which of its draws lie beyond the prediction
range and at what bound, and `summary()`, `print()` and `autoplot()` report it.

This phase exists because the reported N(S)EC is not computed by `nec()`. It is
realised inside `bnec()`, by `expand_nec()` for one equation and `expand_manec()`
for a model-averaged set, and `summary()` reads the stored summary. A censoring
report added to `nec()` alone would not reach the number a user reports.

It also settles a difference between the two halves of a model-averaged N(S)EC.
For an `ecx`-type equation the no-effect value is an NSEC read off the prediction
grid, and a draw whose curve does not reach the reference is `NA` and is deleted
from the summary. For a `nec`-type equation it is a sampled parameter, currently
held inside the range by the truncation phase 6 removes. Without this phase, one
half of the mixture would delete its beyond-range draws while the other kept
them.

It is done when the reported N(S)EC states its censored fraction and its bound,
the numbers `summary()` prints agree with what `nec()` and `nsec()` return for
the same object, and nothing changes for a fit with no censored draws.

### Phase 5. The `extrapolate` argument (#392)

`nec()` and `nsec()` gain an argument that returns a no-effect estimate beyond
the prediction range for a caller who asks for it. The default reports the
censored summary from phase 4.

The two halves of a mixed set cannot be extrapolated the same way, and that
decides what the argument accepts. A NEC is a sampled parameter, so releasing it
to an infinite limit is free. An NSEC is read off a fitted curve, and a curve
cannot be read off an infinite grid. So an infinite limit is available only where
every component samples a NEC, and a mixed set requires a finite limit, which
extends the grid for the NSEC components and releases the NEC components.

It is done when `extrapolate = TRUE` returns the posterior unaltered on a pure
NEC set and errors on a mixed one, a finite limit extends both halves, and no
output changes for a fit with no censored draws.

### Phase 6. Removal of the tested-range truncation (#393)

The default `nec` and `ec50` priors are no longer truncated at the highest
concentration tested. The posterior may then place mass above the series, and
phases 4 and 5 are what stop that reaching the user as an unqualified number.

It is done when the audit shows the complete-design cells unchanged, the
incomplete-design threshold cells no longer at a prior CDF of exactly 1, and the
initial-value search taking no more proposals than before on the complete
designs.

### Phase 7. The incomplete-design prior set (#394)

`bnec()` gains the declaration that the lower asymptote was not observed. It
changes the `bot` prior and the initial-value band. This is the largest phase and
it depends on all six before it.

It is done when the audit shows the `bot` prior covering the true value on the
incomplete designs, the complete-design cells unchanged under the default, and a
fit on an incomplete design initialising from the search rather than falling back
to Stan.

---

## The user-visible change

On a complete design, nothing changes except the reporting in phases 4 and 5.
The priors are the same and the estimates are the same.

On a design that has not flattened, `bnec()` reports it before fitting:

```
The response at the highest concentration is still declining. The lower
asymptote may not be identified. See ?bnec for `asymptote_observed`.
```

The user then fits with the declaration. The N(S)EC is reported as censored,
both where they read it off the summary and where they call the estimator:

```r
fit <- bnec(y ~ crf(x, "nec4param"), data = dat, asymptote_observed = FALSE)

summary(fit)
#  N(S)EC   Estimate  Q2.5 Q97.5
#              >= 40 >= 40 >= 40
#  87% of draws place the no-effect value at or above 40, the top of the
#  prediction range. The estimate is censored there.

nec(fit, extrapolate = TRUE)
#  Estimate  Q2.5 Q97.5
#      61.4  38.2   412
```

On a model-averaged set containing an `ecx`-type equation, the second call needs
a limit, because half the mixture is read off a fitted curve and a curve cannot
be read off an infinite grid:

```r
nec(manec_fit, extrapolate = TRUE)
#  Error: `extrapolate = TRUE` needs every model in the set to estimate a NEC.
#  This set includes ecx4param, whose N(S)EC is read off the fitted curve.
#  Supply a finite limit, for example extrapolate = 400.

nec(manec_fit, extrapolate = 400)
#  Estimate  Q2.5 Q97.5
#      58.9  36.4   331
```

The interval in the second call is wide because the data do not identify the
threshold. That width is the point of the change: the present output reports a
narrow interval at the highest concentration tested, which reads as a precise
estimate of something the experiment did not measure.

---

## Decisions taken since this plan was written

RF settled six points on 2026-09-19. Each is stated above with the others; they
are collected here because the earlier version of this section left them open.

The declaration is named `asymptote_observed`, defaulting to `TRUE`.

The censored summary applies to `nsec()` and `ecx()` as well as to the stored
N(S)EC, so every estimator that can return `NA` for an out-of-range draw reports
the same way.

Both ends of the prediction range are reported, not the upper end alone.

The flatness rule states a significance level rather than a multiple of the
standard error, and reports through `message()`.

The `bot_free` equations are reported and the model set is not altered, with the
message naming both ways of restricting it.

The change to existing reported NSEC and ECx figures is accepted, on the
condition that its size is measured on the vignette fits and recorded in
`NEWS.md` before the change merges.

One thing remains open. The `bot` rule cannot be satisfied exactly on the
zero-bounded branch, because a gamma at a fixed shape of 2 cannot have its
central 95% both start at zero and end at a chosen value. The realised coverage
per family is measured in phase 3 rather than asserted, and the rule is revisited
if a family falls far from it. Specification §5.3.

---

## Provenance

The measurements quoted here are from #386, taken prior-only on
`issue-382-example8-group-terms` at `30fcdbfa` under R 4.6.1 and bayesnec
2.1.3.39, on 2026-09-19. Nothing was fitted for them.

The review of PR #387 that phases 1 and 2 come from was made against that pull
request at its sixth commit, `check prior bounds before transformation`, on
2026-09-19.
