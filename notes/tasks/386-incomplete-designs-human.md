# Plan for designs that do not reach the lower asymptote (#386)

Companion specification: `386-incomplete-designs-claude.md`. Every decision
below is stated in full there, with the evidence behind it and the alternatives
that were rejected.

Written 2026-09-19, from the measurements in #386 and a review of PR #387.

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
and is what tells them to reach for the new prior set.

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

Removing the truncation without this would make `nec()` return concentrations
above the highest tested by default, which is a change to what the package
claims. Specification §4.

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

### The `bot` prior from the support floor to the observed endpoint

On an incomplete design the endpoint mean is an upper bound on `bot` rather than
an estimate of it. Specification §5.3.

### The initial-value band under the same declaration

The band rejects an initial curve outside it, so a `bot` prior placed below the
observed endpoint would otherwise have its draws rejected and the search would
fall back to Stan's own initialisation on exactly the fits that need it.
Specification §5.4.

---

## The order of work

Six phases, one issue each.

| phase | issue | what it changes |
|---|---|---|
| 1 | #389 | the default priors for a `rate()` fit |
| 2 | #390 | the report that a design has not flattened |
| 3 | #391 | incomplete designs in the prior audit |
| 4 | #392 | censoring of the reported estimate, and `extrapolate` |
| 5 | #393 | removal of the tested-range truncation |
| 6 | #394 | the incomplete-design prior set |

Phases 4 and 5 must land in that order, because phase 5 removes a constraint
that phase 4 puts the reporting in place for.

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
phase 5.

PR #387 implements this and needs three changes. The rule triggers on the
magnitude of the effect, which does not separate an incomplete design from a
complete one with a high asymptote, so it is replaced by a test of whether the
response is still declining at the top of the series. That replacement also
answers the question #386 raised about the hormesis equations, because it never
reads the control. Whether the report is a warning or a message follows from its
measured false-positive rate on complete designs rather than from preference.

It is done when the check fires on the simulated incomplete designs, its
false-positive rate on complete designs is measured and recorded, and it is
raised once per `bnec()` call rather than once per equation.

### Phase 3. Incomplete designs in the prior audit (#391)

`notes/scripts/prior_audit.R` simulates every design so that it descends to
within 5% of `bot`, so it cannot measure any of this. It is extended with
incomplete cells before the priors change, so that the later phases have a
baseline and can show the complete-design cells did not regress.

It is done when the script reports the truncated prior CDF at the true value for
complete and incomplete designs side by side, and reproduces the figures #386
quotes.

### Phase 4. Censoring and the `extrapolate` argument (#392)

`nec()` reports a no-effect estimate at or above the top of the prediction range
as censored, and gains an argument that returns it uncensored. Nothing else
changes yet, so this phase is safe on its own: the prior is still truncated, so
the behaviour is a clearer report of what the package already returns.

It is done when a fit whose posterior is against the bound reports the fraction
of draws censored and the bound, `extrapolate = TRUE` returns the posterior
unaltered, and `summary()` shows the estimate as bounded below rather than as a
point.

### Phase 5. Removal of the tested-range truncation (#393)

The default `nec` and `ec50` priors are no longer truncated at the highest
concentration tested. The posterior may then place mass above the series, and
phase 4 is what stops that reaching the user as an unqualified number.

It is done when the audit shows the complete-design cells unchanged, the
incomplete-design threshold cells no longer at a prior CDF of exactly 1, and the
initial-value search taking no more proposals than before on the complete
designs.

### Phase 6. The incomplete-design prior set (#394)

`bnec()` gains the declaration that the lower asymptote was not observed. It
changes the `bot` prior and the initial-value band. This is the largest phase and
it depends on all five before it.

It is done when the audit shows the `bot` prior covering the true value on the
incomplete designs, the complete-design cells unchanged under the default, and a
fit on an incomplete design initialising from the search rather than falling back
to Stan.

---

## The user-visible change

On a complete design, nothing changes except the reporting in phase 4. The
priors are the same and the estimates are the same.

On a design that has not flattened, `bnec()` reports it before fitting:

```
The response at the highest concentration is still declining. The lower
asymptote may not be identified. See ?bnec for `response_complete`.
```

The user then fits with the declaration, and `nec()` reports the estimate as
censored unless they ask for extrapolation:

```r
fit <- bnec(y ~ crf(x, "nec4param"), data = dat, response_complete = FALSE)

nec(fit)
#  Estimate  Q2.5 Q97.5
#     >= 40 >= 40 >= 40
#  The no-effect estimate is at or above 40, the highest concentration in the
#  prediction range, for 87% of draws. Reported as censored.

nec(fit, extrapolate = TRUE)
#  Estimate  Q2.5 Q97.5
#      61.4  38.2   412
```

The interval in the second call is wide because the data do not identify the
threshold. That width is the point of the change: the present output reports a
narrow interval at the highest concentration tested, which reads as a precise
estimate of something the experiment did not measure.

---

## Open decisions

Three things are not settled and are flagged where they arise in the
specification.

The name of the declaration. `response_complete = FALSE` is used throughout and
`asymptote_observed = FALSE` is the alternative. Nothing is released, so this is
a one-line change at any point before phase 6 merges.

The lower bound. This plan removes the truncation at the top of the series only,
because that is the case #386 measured. A threshold below the lowest
concentration tested is the mirror case and the same mechanism applies to it.
Specification §3.3 states what it would take.

The model set on an incomplete design. Fourteen of the 23 equations have no
`bot` parameter and so assert that the response reaches the support floor. On an
incomplete design the data cannot distinguish them from the equations that
estimate `bot`, so a model average over both is averaging over the assumption in
question. The recommendation is to report this and not to alter the set
silently. Specification §5.5.

---

## Provenance

The measurements quoted here are from #386, taken prior-only on
`issue-382-example8-group-terms` at `30fcdbfa` under R 4.6.1 and bayesnec
2.1.3.39, on 2026-09-19. Nothing was fitted for them.

The review of PR #387 that phases 1 and 2 come from was made against that pull
request at its sixth commit, `check prior bounds before transformation`, on
2026-09-19.
