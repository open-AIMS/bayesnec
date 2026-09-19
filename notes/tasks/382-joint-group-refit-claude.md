# Joint refit across factor levels

Implementation specification for `notes/tasks/382-joint-group-refit-human.md`.
That document holds the decisions; this one holds the evidence, the signatures
and the edge cases. Where the two disagree the human document is wrong and
should be corrected, not worked around.

## Terms

**Joint refit** --- one `brms` model in which each parameter of the
concentration-response equation takes a separate value per level of a factor,
estimated in a single posterior. Equivalent to dummy coding the factor onto
every curve parameter.

**Level term** --- the population-level term `~ 0 + <group_var>` that replaces
a parameter's `~ 1` in the joint refit.

## Measurements behind the decisions

### No user-facing syntax exists for a level term

Measured on `issue-382-example8-group-terms` at `48c991bc`, R 4.5.2:

```r
make_brmsformula(bayesnecformula(y ~ crf(x, "nec3param") + grp),
                 data = d, family = Beta(link = "identity"))
#> Error: object 'grp' not found
```

A bare factor term outside `crf()` is not parsed as a population-level term on
the curve parameters. `?bayesnecformula` describes `pterms` outside `crf` as
"largely untested"; this is the specific failure. The same call with `ogl(grp)`
succeeds and emits `ogl ~ 1 + (1 | grp)`.

The joint refit therefore cannot be reached by writing a formula, and
`bnec_joint()` constructs the `brmsformula` directly. This is why the human
document rules out new formula syntax: it is not a preference, it is that
adding the syntax is a second and larger piece of work.

### The prediction grid has no level column

`prediction_grid()` (`R/expand_classes.R:230`) builds `newdata` from the
predictor alone, adding a trials column for binomial families and pinning a
`rate()` denominator at 1. It adds nothing for a grouping variable.

`posterior_on_grid()` and `ecnsec.R` both predict with `re_formula = NA`, which
drops group-level terms. That is what makes the grid sufficient for `ogl()`,
`pgl()` and `(par | group)`: the terms are not in the prediction at all.

A level term is population-level, so `re_formula = NA` does not drop it and
`posterior_epred()` stops on the missing column. The failure is loud, not
silent, which is why phase 1 can land before phase 2 without producing a wrong
number. Confirm this rather than assuming it: the first joint refit fitted must
have `ecx()` called on it, and the error recorded here.

### The parameter sub-formula accessor

`add_formula_glef()` (`R/bayesnecformula.R:923`) reaches `brmform[[2]][[p]]`
for each parameter `p` in `names(brmform[[2]])` and appends to its right-hand
side. The joint refit replaces rather than appends, at the same point in the
same structure. Reuse the accessor pattern; do not re-derive the parameter list
from the equation name, because `model_pars` is read from the built formula for
the reason the comment there gives.

## Phase 1

### Signature

```r
bnec_joint(object, model = NULL, formula = NULL, ...)
```

dispatched on `bayesnecgroupfit`. `bnec_joint()` is already generic in effect
but is written as a plain function that tests `is_bayesnechurdlefit()` and
stops otherwise (`R/bnec_joint.R:119`). Convert it to an S3 generic with
`bayesnechurdlefit` and `bayesnecgroupfit` methods. The hurdle method keeps its
current body verbatim, including `model_survival`, which has no meaning here.

### Choosing the equation

One equation is fitted for all levels. The default is the equation maximising
the sum of per-level stacking weights, which is the `bayesnecgroupfit` analogue
of `best_crossed()`.

Where the favoured equation holds less than half the summed weight, message
which equations the levels favour and what was chosen. Do not stop: a spread of
weight across equations is a result about the data, and the user asked for a
joint fit. Record it in the returned object so that phase 3 can report it.

`model` names an equation directly and skips the choice.

### Building the formula

Take the `bayesnecformula` from `object`, swap in the chosen equation with
`swap_crf_model()` as the hurdle method does, build the `brmsformula`, then for
each `p` in `names(brmform[[2]])` replace the right-hand side of
`brmform[[2]][[p]]` with `0 + <group_var>`.

The dispersion parameter takes the same treatment where the family has one, so
that dispersion is estimated per level. This is what the human document means
by answering the dispersion question; a joint refit with one shared dispersion
is available by not applying the replacement to it, and should be reachable
through an argument rather than by editing code. Name it `disp_by_level`,
default `TRUE`.

### Priors

`get_priors()` derives priors per parameter from the data. With `0 + <group>`
there are now as many coefficients per parameter as there are levels, and the
prior has to apply to each. Check what `get_priors()` returns for the
constructed formula before fitting, and where it returns a prior for a
coefficient that no longer exists, that is the bug to fix first.

This is the part most likely to be wrong in a way that samples cleanly. A prior
derived from the whole response applied to a per-level coefficient is not
obviously wrong from the output. Compare the per-level posteriors against the
`bnec_group()` fit of the same equation; they should agree to Monte Carlo error
where the priors are right, because the models are then the same model.

### Family

Taken from `object`, which chose it once from the whole response for the reason
`?bnec_group` gives. Do not re-select it.

## Phase 2

### The grid

`prediction_grid()` gains the level column. It must not gain it unconditionally:
every existing caller passes a fit with no population-level factor, and adding a
column those models do not use would change nothing for them but is an untested
change on every path in the package.

Add it from the formula rather than from the fit, so that the condition is a
property of what was requested. A `group_var` recorded on the returned object in
phase 1 is the cleanest source.

The grid becomes `resolution` rows per level rather than `resolution` rows.
Every consumer that assumes `nrow(newdata) == resolution` has to be found. Grep
for `resolution` across `R/` in one pass rather than fixing them as they fail;
an assumption about grid length is exactly the kind of thing pinned in a test
whose failure message names something else.

### The estimators

`ecx()`, `nsec()`, `nec()` return one row per level, named by level. The return
shape changes from a named vector of three to a data frame, which is a breaking
change for a joint refit only --- no existing object can reach this path,
because no existing object is a joint refit.

`autoplot()` draws one curve per level. `ggbnec_data()` gains a level column.
Both already handle a per-level structure for `bayesnecgroupfit`; read
`group_curves()` in the `example8` source for the shape the vignette expects.

### The agreement test

The test that phase 2 is right: fit a joint refit and a `bnec_group()` fit of
the same single equation on the same data, with levels that genuinely do not
share information, and compare per-level `ecx()`. They estimate the same
quantity and must agree to Monte Carlo error. A disagreement is a prior problem
from phase 1 or a grid problem here, and the test does not distinguish them ---
so run it with the priors pinned explicitly the first time.

## Phase 3

The `example8` section demonstrating a question the other two routes cannot
answer. The contrast within one posterior is the clearest: `compare_posterior()`
on a `bnec_group()` fit differences independent posteriors, and the joint refit
conditions on a shared dispersion, so the two give different intervals for the
same contrast on the same data. Report both.

State which route answers which question, in the vignette, as a list a reader
can choose from.

## Tests

`tests/testthat/test-bnec_joint.R` exists for the hurdle method. Add to it
rather than starting a file, since the generic is shared.

- dispatch: a `bayesmanecfit` is refused with a message naming both accepted
  classes;
- equation choice: a grouped fit whose levels favour different equations gets
  the summed-weight winner, and messages;
- `model` overrides the choice;
- `disp_by_level = FALSE` produces one dispersion coefficient, `TRUE` produces
  one per level;
- the agreement test above, as the phase 2 gate;
- `ecx()` on a phase 1 fit, before phase 2, errors rather than returning a
  number. Remove this test when phase 2 lands, and say so in its comment.

Every fitting test is `skip_on_cran()` and reuses one fixture. Compiling Stan
programs is what makes the suite slow, not the number of assertions.

## Rejected alternatives

### Formula syntax for a level term

A user would write
`crf(x, "nec3param") + lvl(group)` and reach the joint fit directly. Rejected
because the equation choice should be made against per-level weights, and
requiring `bnec_group()` first means it always is. It also doubles the work:
the syntax needs parsing, validation against each equation's parameter list,
and its own interaction with `ogl()`, `pgl()` and `disp()`.

### A combination of per-level fits through `c.bnecfit()`

Rejected because it does not fit anything jointly. The posteriors would still be
independent, which is what `bnec_group()` already returns, so it would add a
class without adding inference.

### A dispersion fixed at the `bnec_group()` estimate

Rejected
because it reports a dispersion with no uncertainty, and the resulting intervals
on every other parameter are too narrow by an amount nothing on the object
records.

### A `level` argument on `ecx()`, keeping its vector return

Rejected because the default would have to be a level, and any
default silently answers for one level a question asked about the fit. A data
frame with every level is the answer to the question that was asked.
