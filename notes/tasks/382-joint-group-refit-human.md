# A joint refit across factor levels

## Summary

`bnec_group()` fits each level of a factor separately and `ogl()`, `pgl()` and
`(par | group)` pool the levels towards a common curve. Neither route fits one
model in which every level has its own curve parameters estimated together.
This document decides to add that third route, as a method of the existing
`bnec_joint()`, and sets out what it changes and in what order.

The route is called a **joint refit** here: one `brms` model in which each
parameter of the concentration-response equation takes a separate value per
level, estimated in a single posterior. Statisticians call this dummy coding,
and the two names mean the same thing.

## The gap in the supported routes

Issue #33 proposed it in 2020 and it was never resolved. Issue #382 requires
the package either to provide the route or to state that it does not. Three
things the two supported routes cannot do, which a user reasonably asks for:

- **A contrast between two levels within one posterior.** `bnec_group()` gives
  independent posteriors per level, so a difference between them is a
  difference of independent draws. That is what `compare_posterior()` computes
  and it is valid, but it cannot condition on anything shared.
- **A dispersion parameter estimated across levels.** Separate fits give each
  level its own, which `bnec_group()` documents as deliberate because a shared
  one would break the factorisation its crossed weights depend on. A user who
  believes the measurement error is a property of the assay rather than of the
  level currently has no way to say so.
- **No pooling with shared structure.** A group-level term shrinks each level
  towards the common curve. Where the levels are a small number of fixed
  conditions chosen for their interest, rather than a sample from a population,
  that shrinkage is not wanted and the partially pooled route is the wrong
  model.

## The user-facing call

```r
fits <- bnec_group(y ~ crf(x, "all"), data = d, group_var = "site")
joint <- bnec_joint(fits)                      # the favoured equation, per level
ecx(joint, ecx_val = 10)                       # one row per level
```

`bnec_joint()` already exists for a different purpose: it refits the favoured
combination of a two-block hurdle fit as one model. This adds a method for a
grouped fit, because the two do the same thing — take what a factorised fit
favoured and refit it as one model that expresses structure the factorisation
could not.

## The phases

### Phase 1

This phase makes the joint refit fit at all. It adds `bnec_joint.bayesnecgroupfit()`. It picks one equation, builds the model
in which each curve parameter varies by level, and fits it.

One equation has to be chosen for all levels, because a single model has a
single functional form, and the levels of a `bnec_group()` fit may each favour
a different one. The default is the equation with the highest total weight
across levels, and the user can name another. Where the levels disagree
strongly about the equation, that is a result about the data and the function
says so rather than choosing silently.

Done when the fit returns and its parameters can be read per level.

### Phase 2

This phase makes every estimate report per level.

`ecx()`, `nsec()`, `nec()` and `autoplot()` predict on a grid built by
`prediction_grid()`, which includes the predictor and nothing else. That works
for the existing routes because a group-level term is dropped from the
prediction, but a level term in a joint refit is not droppable: it is part of
the mean. The grid therefore needs a level column, and each estimator needs to
return one value per level rather than one value.

The risk sits here. Until it is done the estimators do not silently return the
wrong level — `posterior_epred()` stops on the column it
cannot find — but neither do they work.

Done when every estimator returns one row per level, and the per-level values
from a joint refit of independent data agree with `bnec_group()` on the same
data to within Monte Carlo error.

### Phase 3

This phase tells a reader which route to choose. It adds the `example8` section demonstrating a question this route answers that
the other two cannot, and stating which route answers each kind of
factor-covariate question.

Done when a reader can choose between the three routes from the vignette alone.

## Out of scope

No new formula syntax. A user cannot write a level term in a
`bayesnecformula`, and this does not add one: `bnec_joint()` builds the model
from a fitted grouped object, so the only supported way to reach a joint refit
is through `bnec_group()` first. That is deliberate. The equation choice should
be made against the per-level model weights, and requiring the grouped fit
first means it always is.

Model averaging is not available in the joint route. It fits one equation. The
averaging happens in the `bnec_group()` call that precedes it, which is the
same division of labour `bnec_joint()` already documents for hurdle fits.

## Evidence and rejected alternatives

In the specification, `382-joint-group-refit-claude.md`, with the measurements
that support each decision.
