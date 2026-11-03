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

A vignette describes the version it ships with. It names no version or issue
number and does not say what changed. Changes since the last CRAN release go in
`NEWS.md`, and development history stays on the pull request or issue. A figure
computed outside the build names the commit it ran on. Only a CRAN release
number means anything to a user: the development numbers in `DESCRIPTION` were
never released. Where a vignette follows a published paper that the package has
since departed from, it says so once, citing the paper, without a version
number.

### Reading a rendered vignette

The committed `.Rmd` already holds every chunk's output as text, so rendering it
fits nothing and takes seconds:

```r
rmarkdown::render("vignettes/example9.Rmd",
                  output_file = "example9_20260917-0725.html",
                  output_dir = "<main checkout>/ignore/vignette_preview")
```

Every preview goes in `bayesnec/ignore/vignette_preview/` in the main checkout,
named `exampleN_YYYYMMDD-HHMM.html`, which is the convention already
in that directory and keeps successive renders of one vignette side by side.
`ignore/` is git-ignored and matched by `^ignore$` in `.Rbuildignore`, so nothing
written there reaches a commit or a build.

A worktree has an `ignore/` of its own, and a preview written to it is invisible
from the checkout RF works in --- the same trap section 10 of the global file
records for `prompts/`. Where the session is in a worktree, take the main
checkout from the first entry of `git worktree list` and write there.

Copy the `vignette-fig-*.png` files the vignette references into the output
directory first, or the figures come out broken. `html_vignette` embeds them, so
the result is one self-contained file and the copies can then be deleted.

Do not precompile in order to read a vignette. `vignettes/precompile.R` refits
every model --- `example9` alone was 3902 s on 2026-09-08 --- and that is a
release operation, not a review one.

The catch is that a branch which edits only the `.Rmd.orig` leaves the `.Rmd`
stale, which is the normal state, since precompiling is not a merge
precondition. Rendering it then shows the vignette without the change. To read a
new section in place, knit that section on its own against the fits it
describes, splice the knitted markdown into the committed `.Rmd`, and render
that. The preview must then say that its parts come from different renders, so
that no one reads numbers across them.

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

[`notes/comment_reasoning/README.md`](notes/comment_reasoning/README.md)
indexes the rules, evidence and qualifications retained from the comment
cleanup under #484. Historical measurements describe the runs that produced
them; the current implementation and tests determine what is supported now.

## The reference library

The papers behind this package and the training material are in
`C:/Rworking/references/`, outside every repository so that they are never
committed, and reached from here through `ignore/references`. Read one by that
path rather than the absolute one, so the read stays inside the working
directory. `notes/references.md` lists what is there and gives the key in
`vignettes/bayesnec.bib` for each paper the package cites.

## Naming the models in prose

Section 12 rule 6 of the global file applies with these specifics. `bayesnec`
has 24 model equations, all of them in the `all` group, and ten of them are NEC
models, so "the nec model" never identifies one: name `nec3param`, `nec4param`
or `nechorme`. `ecxflat`, the constant equation, is an ECx-type equation in the
`ecx` group with no `nec` parameter, so its no-effect estimate is an NSEC.
Distinguish the equations whose names begin `nec`, the `nec` parameter they
estimate, and `mod_groups$nec`, the group of equations. The same care applies
to `ecx`.

## Agreement with `toxval`

`toxval` has its own `ecx()` and `nsec()`, with methods for `bayesnec` fits, and
`bayesnec` keeps its own (D46 in `notes/implementation/03_decisions.md`). RF
develops both and keeps their results in agreement.

The comparison with `toxval` applies only to a change that edits `ecx()`,
`nsec()`, `nec()` or `ecnsec()`, or an internal function they call, and that
changes a value they return for the same fitted object. Compare such a change
with `toxval` under matching options before it merges. Record the result, and
whether `toxval` is to follow, on a `toxval` issue. `toxval` has no `nec()`
yet, so a change confined to `nec()` needs the comparison only once `toxval`
has one.

No other change needs the comparison, and a session that has not edited those
functions does not consider it. This includes changes to fitting, priors, model
weights, `summary()`, `autoplot()` and documentation, even where they change
the estimates. Such a change alters the fitted object, and `toxval` computes
its estimates from that same object. A new message or refusal changes no
returned value, so it needs no comparison either.

Both packages export `ecx()` and `nsec()`, and the one attached last masks the
other without a warning. With both attached, call each with its package prefix.
`bayesnec` and `brms` are packages; `bnec()` and `brm()` are functions.

## Modelling gotchas

Traps found in this package's behaviour. Check them before comparing a
`bayesnec` fit against anything else, or reimplementing one.

### Default and explicitly chosen links

`bnec()` assigns the identity link when the caller names a family without
choosing a link, for example `family = "Beta"` or `family = Beta()`. An explicit
supported link is honoured, as in `Beta(link = "logit")`. A supplied variable
holding a family object retains that object's links. The supported mean links
are `identity`, `log` and `logit`; see `R/validate_family.R` and
`tests/testthat/test-validate_family.R`.

With the identity link, the curve's `top` and `bot` parameters are on the
response scale. With a log or logit link they are on the linear-predictor
scale. The `nec` parameter is on the fitted concentration scale, including
any transformation in `crf()`; it is not a response-scale parameter.

Several `brms` families have different default links: `Beta()` and
`binomial()` use logit, `Gamma()` uses inverse, and `poisson()` uses log.
A comparison or replication with `brms::brm()` must specify the link recorded
in the fitted object. For a `bnec()` fit using its default identity link:

```r
brm(..., family = Beta(link = "identity"))   # not Beta()
```

Passing the same curve expression to `brm()` with its default logit link
changes a curve spanning 0.04–0.70 into a response mean spanning about
0.51–0.67. Good sampler diagnostics do not detect that change of model.
Check the fitted links before interpreting a large difference in `elpd_diff`.

### The scale of a returned estimate

Where the formula transforms the predictor inline, as in
`crf(log(concentration), ...)`, every estimator returns the **recorded** scale
by default (`xform = NULL`). This covers `nec()`, `nsec()`, `ecx()`, `ecnsec()`,
`curve_params()`, `exceedance()` and the `summary()` rows, and matches
`autoplot()` and `ggbnec_data()`. A supplied `xform` is applied to the estimate on
the **fitted** scale. So `xform = identity` returns the transformed scale, and
`xform = exp` on a `crf(log(x))` fit returns what the default returns.

Applying the inverse to the default output transforms twice: `exp(nec(fit))` on
a `crf(log(x))` fit exponentiates a concentration, with no warning. Before #299
that was the correct idiom, so code written against an older `predev` or a
released version needs checking. Code that compares with `toxval` or with a
hand-written fit on the fitted scale passes `xform = identity`.

Take estimates at their default, and pass `xform = identity` only to read the
fitted scale. Apply axis spacing with `scale_x_continuous(transform = "sqrt")`
to the untransformed plot data, never by transforming the breaks.

### `dispersion()` on a model average

`bayesnec::dispersion()` accepts a `bayesmanecfit` and computes the statistic
separately for each equation. With `summary = FALSE` it returns a named list
of posterior vectors; with `summary = TRUE` it returns a data frame with one
row per equation. It does not define one dispersion statistic for the model
average. See `R/dispersion.R` and `tests/testthat/test-dispersion.R`.

`summary(fit)$mod_weights` already includes `dispersion_Estimate`,
`dispersion_Q2.5` and `dispersion_Q97.5` for the families the test applies to.
Use those stored results when sufficient: calling `dispersion()` again
simulates posterior predictions for each applicable equation.

### The placement of a new data refusal

`check_data()` runs once per model, from inside `fit_bayesnec()`, and `bnec()`
wraps that call in `try(..., silent = FALSE)` whenever more than one model is
requested. An error raised from `check_data()` is therefore printed once per
model, every model is recorded as failed, and the call ends on "None of the
models fit successfully, try using bnec with a single model ... or check
?show_params", which names neither the cause nor the remedy. The default `model`
argument is a set, so this is the common path, not an edge case.

Measured on R 4.6.1: a two-model set with one `NA` printed the refusal twice and
then that message; the default 24-model set would print it 24 times.

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

### Model weights are pseudo-BMA

`bayesnec` weights the equations of a model set by pseudo-BMA unless the caller
names another method in `loo_controls`. `bnec()` sets
`loo_controls$weights$method = "pseudobma"` when none is given
(`R/helpers.R:1075`), and every function that assembles a set resolves to the
same default (`?bnec`, the `loo_controls` argument). Stacking is used only where
a user asks for it, and `crossed_group_weights()` refuses a set weighted by it,
because the crossed table of a grouped fit does not factorise under stacking.

Write "model weight", or "pseudo-BMA weight" where the method matters, when
describing a set's weights. "Stacking weight" names a method the package does
not use by default, and reads as a claim about how the average was formed. The
error reached this file and the global one, and from them a vignette figure
axis and a planning conversation, before RF corrected it on 2026-09-26.

### Diagnostics against the weight an equation carries

In a model-averaged set every equation is fitted, including ones whose shape
suits the data badly. Such an equation fails the over-dispersion test whatever
the data are doing, because the residual variation its curve cannot account for
is counted as dispersion, and it is often the one the sampler struggles with as
well. It is given almost no model weight for the same reason, so a check it
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
