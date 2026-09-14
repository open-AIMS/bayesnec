# bayesnec package notes

Project-specific context, additional to `C:/Rworking/CLAUDE.md`. That file's
conventions apply here; this one records only what is particular to this
repository.

## Repo type

R package, standard `usethis`/`devtools` structure. `DESCRIPTION` is the
canonical dependency list, `Imports` and `Suggests` together; ask before adding
to it. There is no `air.toml`, so R code here is not formatted with `air`.

GitHub Actions run `R CMD check` on four platforms, pkgdown, test coverage,
vignette precompilation and the README render. A new top-level file must be
listed in `.Rbuildignore` or `R CMD check` reports it as a non-standard file at
the top level of the package.

## Branches and pull requests

`dev` is the integration branch. Pull requests target `dev`; `master` holds the
CRAN release. `R-CMD-check.yaml` runs on push only for those two branches, and
every other branch is checked through its pull request instead, so a push to a
branch with no pull request open is not checked.

A fix that has to be propagated through a stack of branches queues two workflow
runs per branch on a repository that runs one job at a time. Merge it down the
whole stack first, then push once. Never call `gh pr merge --delete-branch` on
the base of a stacked pull request: deleting the base closes the dependent pull
request, and it cannot be reopened until the branch is restored.

`gh pr merge --auto` does not wait here. Auto-merge is disabled on the
repository, so the flag merges immediately; poll the checks instead.

A change to `notes/`, `prompts/` or other prose that cannot alter package
behaviour goes straight to `dev`. A pull request for it spends a four-platform
check matrix on a file `R CMD check` never reads.

## The symlinked checkout

`C:/Rworking/bayesnec` and `/home/rfisher/Rworking_wsl/bayesnec` are the same
directory through a symlink, not two clones. Two sessions started in the two
paths share one working tree and one branch, and will overwrite each other's
edits. Parallel work needs `git worktree`.

## Vignettes

The vignettes are R Markdown, which is the exception to the global rule against
it: they are package vignettes built by `knitr`, and Quarto is not a vignette
engine here. Each `vignettes/exampleN.Rmd` is generated from the `.Rmd.orig`
beside it by `vignettes/precompile.R`, so that a CRAN build renders stored output
rather than fitting models. Edit the `.Rmd.orig`; an edit to the `.Rmd` is lost
at the next precompile.

A precompiled vignette embeds whatever the render printed. A home directory, a
library path or compiler output reaches CRAN in the built vignette, so check the
rendered `.Rmd` for them before committing it.

## The test suite

`notes/running_the_test_suite.md` holds the measurements and the four traps.
Three things are worth repeating here because each one fails quietly.

`NOT_CRAN=true` is required. Without it a local run reports zero failures while
skipping nearly every assertion, because the fitting tests are wrapped in
`skip_on_cran()`.

`devtools::load_all()` attaches internal functions as well as exported ones, so
a change that depends on an unexported function being visible passes under
`load_all()` and fails under `R CMD check`. Confirm a fix against an installed
build before calling it done.

Compiling Stan programs is what makes the suite slow, not the number of tests.
Reuse a fixture across tests rather than refitting it; `rstan`'s `auto_write`
does not prevent the recompilation. `Config/testthat/parallel: true` runs each test file in a fresh
subprocess, which inherits nothing from the parent session.

## The record of a decision

The pull request or the issue holds the reasoning. `notes/` keeps the
measurements and the pointers, not a second copy of the argument. Planning
documents follow section 13 of the global file and live in `notes/tasks/` as a
`-human.md` and `-claude.md` pair.

## Naming the models in prose

Section 12 rule 6 of the global file applies with these specifics. `bayesnec`
has 23 model equations and ten of them are NEC models, so "the nec model" never
identifies one: name `nec3param`, `nec4param` or `nechorme`. Distinguish the
equations whose names begin `nec`, the `nec` parameter they estimate, and
`mod_groups$nec`, the group of equations. The same care applies to `ecx`.
`bayesnec` and `brms` are packages; `bnec()` and `brm()` are functions.

## Modelling gotchas

Traps found in this package's behaviour. Check them before comparing a
`bayesnec` fit against anything else, or reimplementing one.

### The identity link forced on every family

`bayesnec::validate_family()` sets `link = "identity"` for *every* family it
accepts, so that `top`, `bot` and `nec` stay on the natural response scale and
remain directly interpretable. Several `brms` families default to something
else — `Beta()` and `binomial()` default to **logit**, `Gamma()` to **inverse**,
`poisson()` to **log**.

**Consequence:** a `bnec()` fit and a hand-rolled `brms::brm()` fit of the
"same" model are *different models* unless you pass the link explicitly. With
the default logit link a non-linear concentration-response curve is fitted to
the *logit* of the mean, not the mean — a curve spanning 0.04–0.70 becomes a
mean spanning only 0.51–0.67, which is a catastrophic misspecification that
still samples cleanly and reports healthy R-hat.

**Rule:** when writing raw `brms` code to compare against, replicate, or
prototype for `bayesnec`, always pass the link explicitly:

```r
brm(..., family = Beta(link = "identity"))   # not Beta()
```

Symptom to watch for: one family beating another by an implausibly large
`elpd_diff` (tens to hundreds of units on a small dataset). Before believing
any such result, check that every model in the comparison uses the same link.

This behaviour is documented in the JSS paper, so it does not need repeating in
the package docs — it is recorded here only so the mistake is not repeated.

### Estimates and plot data on different scales

Where the formula transforms the predictor — `crf(sqrt(concentration), ...)` —
the two halves of the output are on different scales:

- `nec()`, `nsec()` and `ecx()` return values on the **transformed** scale.
  Supply the inverse to get them back: `ecx(fit, xform = function(x) x^2)`.
- `autoplot()` and `ggbnec_data()` return `x` on the **recorded** scale, having
  back-transformed it already.

**Consequence:** placing axis breaks at `sqrt(breaks)` crams every label into
the left sixth of the axis, and reporting `nec()` without `xform` gives the
square root of a concentration labelled as a concentration. Neither produces a
warning.

**Rule:** back-transform estimates with `xform`. Apply axis spacing with
`scale_x_continuous(transform = "sqrt")` to the untransformed plot data, never
by transforming the breaks.

`autoplot()`'s own *NEC* annotation does **not** need suppressing. An earlier
version of this rule said to pass `nec = FALSE` because the annotation was drawn
on the fitted scale and would contradict a back-transformed table. Measured on
`dev` at `1ef66e15`, on the `example9` simazine fit with `crf(log(concentration))`:
`ggbnec_data()` returns `nec_vals` of 4.34 against `nec(fit, xform = exp)` of
4.3425, so the annotation is on the recorded scale and agrees with the table.
`to_axis_scale()` inverts numerically on the prediction grid where no `xform`
was supplied. The figure is internally consistent; what disagrees is the figure
against the estimators, which is #299.

### `dispersion()` on a model average

`bayesnec::dispersion()` expects a `bayesnecfit`. Called on a `bayesmanecfit`
it stops with "argument is not a valid model". There is no need to call it per
equation either: `summary(fit)$mod_weights` already carries
`dispersion_Estimate`, `dispersion_Q2.5` and `dispersion_Q97.5` for the
families the test applies to. Recomputing requires a `posterior_predict()` draw
for every equation in the set.

### The placement of a new data refusal

`check_data()` runs once per model, from inside `fit_bayesnec()`, and `bnec()`
wraps that call in `try(..., silent = FALSE)` whenever more than one model is
requested. An error raised from `check_data()` is therefore printed once per
model, every model is recorded as failed, and the call ends on "None of the
models fit successfully, try using bnec with a single model ... or check
?show_params", which names neither the cause nor the remedy. The default `model`
argument is a set, so this is the common path, not an edge case.

Measured on R 4.6.1: a two-model set with one `NA` printed the refusal twice and
then that message; the default 23-model set would print it 23 times.

**Rule:** a check that is a property of the data and the formula together, fixed
for the whole call, is raised in `bnec()` before the model loop, and kept in
`check_data()` only as the backstop for the routes that do not come through
`bnec()` — `get_priors()`, which checks each model of the set in turn, and
`amend()`. `fit_bayesnec()` is not exported, so it is not one of those routes.
`R/bnec.R` records the same reasoning for `check_normalisation()` and
`check_inline_boundary()`; follow it.

`bnec_group()` needs the check separately, before its level loop. It fits each
level with `bnec()` in sequence, so a refusal reached at level *k* comes only
after levels 1 to *k*-1 have compiled and sampled. `bnec_hurdle()` does not: it
refuses an `NA` response up front with a message of its own.

Two mechanics that go with this. `stats::model.frame()` does not propagate an
input data frame's `na.action` attribute, so telling a user to run `na.omit()`
before `bnec()` does not make the next call fail on the attribute their own
`na.omit()` left. And on the model frame, `attr(data, "na.action")` names each
element with the **row name** and holds the **position** as its value — report
the names, because a position indexes whatever subset the frame was built from,
which for one level of a `bnec_group()` call is not a row of the data the user
supplied.

### Diagnostics against the weight an equation carries

In a model-averaged set every equation is fitted, including ones whose shape
suits the data badly. Such an equation fails the over-dispersion test whatever
the data are doing, because the residual variation its curve cannot account for
is counted as dispersion, and it is often the one the sampler struggles with as
well. It is given almost no stacking weight for the same reason, so a check it
fails says something about that curve and nothing about the model-averaged
estimates.

Report either check against the weight, and warn only where a contributing
equation fails. Measured on the four sea urchin copper reference tests
(`toxtools`, `decline` set, eleven equations retained): every equation failing
the dispersion test held under one per cent of the weight, while the dominant
equation had a dispersion of 0.61–1.05 on all four sheets. `ecxexp` sat between
30 and 41 on every sheet with an R-squared of 0.54. The same equation can pass
and fail across sheets — `ecxwb1p3` was dominant and passing on two, and failing
at weight 0.0001 on another.
