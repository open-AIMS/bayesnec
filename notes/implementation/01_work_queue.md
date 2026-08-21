# Work queue — the pre-migration stack

Read `00_protocol.md` first, then `03_decisions.md`. The run instructions are in
`04_stack_run.md`.

**Refreshed 2026-08-21, second pass.** The goal has changed: get as much as
possible onto CRAN *before* the toxval migration, rather than parking work until
after it. Every item below has been checked against the files the migration
moves, and RF has settled the decisions that were blocking them.

**This queue was rebuilt after a triage error.** The first pass read issue
*bodies* only, via `gh issue view --json body`, and never the comments. Several
issues listed as "needs a design decision" had in fact been fully scoped in
comment threads — #148, #136, #139, #33 and #6 among them. Everything below is
based on bodies **and** comments. If you extend this queue, read both.

---

# The stack

Branches stack: each is cut from the one above it, and its PR targets the one
above it. See `00_protocol.md`. Order is least-risky first, so a stall late in
the run still leaves a clean, mergeable run of PRs at the bottom.

| # | issue | what | size | tier |
|---|---|---|---|---|
| 0 | #79, #212, #166 | close, no PR | — | — |
| 1 | #215 | publish `dev` vignettes from CI | S | 2.1.4 |
| 2 | #139 | document the `drc` NEC equivalence | S | 2.1.4 |
| 3 | #210 | `define_prior()` collapse on many zeros | S | 2.1.4 |
| 4 | #207 | dispersion priors; fill incomplete prior sets | M | 2.1.4 |
| 5 | #136 | `rate()` aterm for Poisson / negbinomial | M | 2.2.0 |
| 6 | #209 | `hurdle_poisson`, `hurdle_negbinomial` | M–L | 2.2.0 |
| 7 | #148 | `check_fit()`, `pp_check()` methods, LOO-PIT | L | 2.2.0 |
| 8 | #33 | factor covariate, stage 1 | XL | 2.3.0 |
| 9 | #6 + #33 | the grouping vignette | L | 2.3.0 |
| — | #190 | full `precompile.R` | L | **attended, per release** |

---

## 0. Close three issues — no branch, no PR

Do this first; it costs minutes and shrinks the tracker.

- **#79** — checked on `dev` 2026-08-21 and it no longer reproduces. The reprex,
  translated from the retired `model()` syntax to `crf()` and run at
  `iter = 200, warmup = 100, chains = 2, seed = 1`, **fitted successfully at
  `trials = 500`**, response proportions 0.200 / 0.200 / 0.074 / 0.014 / 0.002
  and zeros thereafter. Re-run once at the issue's own default `iter`, then close
  with the evidence. If it *does* fail at full `iter`, stop and requeue it rather
  than forcing a close.
  Record either way: the fit took several minutes on ten data points, consistent
  with `make_good_inits()` retrying many times before succeeding. The symptom is
  gone; the cause may only be softened.
- **#212** — close. RF, 2026-08-21: the issue's own *Note on motivation* says
  this is design consistency rather than a measured performance fix, and names
  `add_ecx = TRUE` and `plot(all_models = TRUE)` as the likelier causes of slow
  plotting. Not worth the four output changes it would force.
- **#166** — close as a duplicate of **toxval#29**.

---

## 1. #215 — publish `dev` vignettes from CI

**Why first.** The only item that touches no R code at all, so it cannot conflict
with anything above or below it, and it can be merged independently if the rest
slips.

**Scope.** `.github/workflows/pkgdown.yaml` triggers on `master` only — verified,
as do `test-coverage.yaml` and `readme-renderer.yaml`. Publish `master` to the
released articles as now, and `dev` to a distinct URL prefix (`articles/dev/`) so
the two cannot be confused. Build vignettes on PRs if that is possible without a
prohibitive cache miss.

**Protocol exemption.** RF lifted the `.github/` prohibition **for this issue
only** (2026-08-21). Do not treat it as general.

**Done when** a push to `dev` publishes rendered articles at a separate prefix
and `master` output is unchanged.

**Hazards.** `precompile.R` re-knits **every** `.Rmd.orig` and needs the full
fitting stack plus `NOT_CRAN=true` — without that env var every chunk is silently
skipped and the build produces **empty** vignettes. Figures are written to the
working directory and moved into `vignettes/` by `precompile.R`, so the job needs
that step or the images 404. Cache compiled Stan models across runs.

**Also do here:** rename the `# bayesnec 2.1.3.7` heading in `NEWS.md` to
`# bayesnec 2.1.4`. That is the release this stack's bug-fix tier becomes.

---

## 2. #139 — document the `drc` NEC equivalence

**Documentation only.** Option A, per RF 2026-08-21. The scoping comment
establishes, numerically rather than by eye, that `drc`'s `NEC.4` and `NEC.3` are
**exactly** `nec4param` and `nec3param` — max absolute difference `0`, not
"small" — and that the two-component formula quoted in the issue body appears
only in `drc`'s Rd, not in the code `drc` fits.

Add the equivalence table to `?models` and `vignettes/example2b.Rmd.orig`,
including that `b = exp(beta)` in our parameterisation, that ours is positive by
construction, and that `drc`'s `b < 0` region is a threshold followed by
unbounded growth which we deliberately cannot represent.

**Say explicitly that `NEC.2` has no `bayesnec` equivalent by choice**, pointing
at #84, closed as won't-implement: the motivating case is control-normalised
data, which `check_normalisation()` now warns about (#173), and the legitimate
structural case is served by a `constant()` or informative prior on `top`.

**Done when** a `drc` user can read the table and map their fit onto ours.

**Not in scope.** Option B (`nec2param`) is off the table on the #84 grounds.
Option C (the Pires et al. 2002 two-component model) stays open and needs someone
to read *Environmetrics* 13:15–27 to establish whether it is a real model or an
Rd artefact — **do not implement it**, and do not close #139 in a way that buries
it. Raise it as its own issue if closing.

**Hazard.** #170 rewrote the adjacent `example2b` paragraph. Read it before
editing so the two do not contradict each other. Editing `.Rmd.orig` is fine;
**do not run `precompile.R`** — the rendered `.Rmd` stays stale until #190.

---

## 3. #210 — `define_prior()` collapses when the response has many zeros

**Scope.** `R/define_prior.R` only. `top` and `bot` are built from quantiles of
the raw response; where a large share of it is exactly zero those quantiles are
zero and the gamma priors collapse onto the fudge term. Three failures, all with
reproducers in the issue: `bot` pinned near zero once 25% of the response is zero
(`:64`); `prior_type = "regularizing"` worse and unconditional because it uses
`quantile(response, 0)` (`:92`); and the literal prior string `gamma(2, Inf)`
past 75% zeros (`:61`).

Guard against a zero or non-finite quantile: where a quantile used to set a gamma
rate is zero, fall back to a scale derived from the positive part of the
response.

**Done when** the priors stay finite and sensibly centred at 30%, 50% and 80%
zeros, with a test asserting each.

**Hazard.** `define_hurdle_prior()` sidesteps this by computing the mu-block
priors from the non-zero subset. That is **not** transferable to the
zero-inflated count families — conditioning on the non-zeros biases `mu` upward,
the same truncation argument as #209. Use the guard.

**Why here.** Not new in #104, but #104 made it the normal case rather than an
accident, and #209 later in this stack adds two more families that land in it.

---

## 4. #207 — dispersion priors, and prior sets that are missing rows

**Part 1 — a supplied dispersion prior is rejected.** `make_inits()`
(`R/inits_functions.R`) tests exact set equality between the prior's parameter
names and the curve's arguments, so any row naming `sigma`, `shape` or `phi`
kills the call. Filter the supplied prior to the model's own `class == "b"` rows
before the name check; pass the whole prior, dispersion row included, to `brm()`;
supply no initial value for it; drop the `class == "b"` filter in
`usable_prior()` (`R/get_priors.R`) so the row round trips. `add_brm_defaults()`
already does exactly this filtering for `disp()` parameters via `disp_pars()`.

**Part 2 — RF's Q1 answer, and a correction to the premise.**

RF's reading was that automatic prior building is lost when `disp()` is used.
**Checked on `dev` (9ef03b85): it is not.** `get_priors()` on a `disp("power")`
formula returns `c0 ~ normal(-1.515, 2)` and `c1 ~ normal(0, 2)` — the
`define_disp_prior()` machinery works, and every parameter gets a bayesnec prior:

```
prior                          nlpar   source
normal(0, 5)                   beta    user
normal(0.9329..., 0.5495...)   top     user
normal(0.3833..., 0.5495...)   bot     user
gamma(5, 2.2831...)            nec     user
normal(-1.515, 2)              c0      user
normal(0, 2)                   c1      user
```

The automatic priors are lost **only when the user supplies an incomplete prior
set**, which `validate_priors()` then accepts wholesale — so the missing
parameters fall through to `brms`'s flat defaults with no warning.

RF's stated principle decides the open question directly: *bayesnec is proactive
about weakly informative priors because flat priors are rarely useful in
non-linear modelling.* An error would refuse the user's partial set and produce
no fit; **filling the gaps from bayesnec's own defaults and warning about it is
the behaviour that principle asks for.** So:

> **Q1 answered: warn and fill from bayesnec defaults.** Never leave a parameter
> on a `brms` flat prior because the user's supplied set did not mention it. The
> warning must name every parameter that was filled.

**Done when** a dispersion prior supplied through `bnec(prior = )` reaches the fit
and round trips through `get_priors()`; a prior set missing rows fits with
bayesnec defaults for the missing ones and a warning naming them; tests cover
both, plus the case where the user's partial set overrides a default bayesnec
would otherwise have generated.

**Hazard.** *Generating a default dispersion prior* remains out of scope — it
would change every existing fit. This is about accepting a supplied one, and
about not silently discarding defaults for rows the user omitted.

**Flag for RF at review**, since it is a behaviour change: a fit that previously
ran on flat priors will now run on bayesnec priors and give different numbers.
That is the intended fix, but it is not a silent one. Say so in the PR.

---

## 5. #136 — `rate()` aterm for Poisson and negative binomial

Fully scoped in the issue comments; **RF settled both open calls on 2026-08-21**:
refuse `rate()` on the zero-inflated count families with a clear message, and make
unrecognised aterms an **error** rather than a message.

**Scope.** `rate()` is currently half-supported and fails late: `check_formula()`
emits a generic message, `brm()` fits fine, and `bnec()` then errors in
`posterior_epred()` at `R/expand_classes.R:46` because the prediction grid has no
denominator column. There is a quieter defect upstream — `get_priors()` derives
`top` from raw counts rather than `y / ex`, giving a prior mean of ~61 against a
true `top` of 20.

Because `bnec()` forces `link = "identity"`, `brms` writes the denominator
multiplicatively on the response scale, so `mu` *is* the rate and
`top`/`bot`/`nec` stay interpretable as counts per unit exposure. `offset` is the
wrong tool under an identity link — drop that half of the original request.

The implementation is enumerated in the scoping comment: a `rate` branch in
`split_calls()` following the **`cens()`** precedent, `simplify_formula()` slots,
the denominator set to 1 in `bnec_newdata.R:57` and `expand_classes.R:44`,
`y / denom` in `get_priors.R:165` and `amend.R:253`, plus the plot paths.

**Hazards, both named in the comment.**

- `retrieve_var()` indexes the model frame **by position** in `bnec_pop`, so the
  term order in `short_form` and the order of the `pop_vars` name vector must stay
  in lockstep. The comment at `R/bayesnecformula.R:569-573` says why.
- `dispersion()` (`R/dispersion.R:69`) is **not** a copy-paste of the binomial
  branch. For Poisson the analogue is exact; for negative binomial it is not,
  because `brms` scales the shape too, so the count-scale variance is
  `mu_c + mu_c^2 / (shape * denom)`. Work it through rather than assuming
  symmetry.

**Making unrecognised aterms an error is a user-visible breaking change.** File it
under 2.2.0, give it its own `NEWS.md` bullet rather than burying it in the
`rate()` entry, and check the vignettes and tests for any formula that would now
error.

---

## 6. #209 — `hurdle_poisson` and `hurdle_negbinomial`

**Scope.** The count analogues of `hurdle_gamma`. Add both to `hurdle_fams` and
`hurdle_mu_fams` in `data-raw/sysdata.R`, extend the `switch()` in
`hurdle_mu_family()` (`R/hurdle_family.R`), and admit them through
`check_models()` and `bnec_hurdle()`. Rebuild `R/sysdata.rda`.

**This also carries a correctness fix.** `bnec_hurdle()` currently fits the growth
component with an **untruncated** count family on `data[y > 0, ]`. For
`hurdle_gamma` that is exact — the Gamma has no mass at zero. For counts it is
not: fitting `Poisson(mu)` to data conditioned on `y > 0` estimates
`mu / (1 - exp(-mu))`. The bias grows as `mu` falls towards zero, which is the
high-concentration end where the *NEC* and ECx are read off. Fix it and say so —
it is a wrong answer today, not a missing feature.

**Verified clear of toxval.** `is_hurdle_family()` and `hurdle_dpar()` are
table-driven off `hurdle_fams`, so `ecx()` and `nsec()` pick the new families up
with no code change. The only contact is the roxygen family list at
`R/ecx.R:25-26` and `R/nsec.R:18-19` — a one-line doc touch in a migrating file.
Acceptable; note it in the PR so the migration carries it.

**Hazard.** Do not reuse D4's reasoning in reverse. D4 says zero-inflated counts
are a genuine mixture and must not get a second block. That argument does not
apply to a hurdle on counts: where the zeros are *observed* to be structural the
likelihood factorises exactly and both blocks carry an interpretable curve. Say
which of the two you are building and why.

---

## 7. #148 — `check_fit()`, `pp_check()` methods, LOO-PIT

Fully scoped across two long comments, RF's decisions settled 2026-08-21. **Read
both comments in full before starting** — the scoping comment carries a worked
demonstration that is the whole justification for the design.

**Settled:** part B in scope and extended to control lack-of-fit; the averaged
check built in `ggplot2` directly with **no `bayesplot` dependency**; require
replication but **fall back to automatic binning with a warning**; **#56 folds
in**, LOO-PIT via `pp_check`, **no `DHARMa`**; deliver **both** a numeric test and
a plot; the two-block families need nothing special, because `posterior_predict`
draws the full mixture including the point mass at zero.

**Three parts.**

- **A — `pp_check()` methods.** `pp_check(pull_brmsfit(fit))` already works; what
  is missing is dispatch. One-line delegation for `bayesnecfit`. `brms` is in
  `Depends`, so `importFrom(brms, pp_check)` adds no dependency.
  `bayesnechurdlefit` returns one result per component, following `dispersion()`.
- **B — `check_fit()`, the substance.** Per concentration group, report observed
  against model-simulated **location and scale**, with a posterior predictive
  p-value, and the control row flagged. Name it `check_fit()`, not
  `check_variance()` — it reports the fit against the data, sitting alongside
  `check_chains()` (the sampler) and `check_priors()` (the priors).
- **C — LOO-PIT** through the part A methods; `add_criterion(fit, "loo")` is
  something `bnec()` already does.

**The two findings that shape it, both from the scoping comment.**

- **A global dispersion statistic cannot see this.** On `manec_example` the global
  Pearson ratio is 1.011 [0.71, 1.44] — a clean bill of health — while the same
  fit binned by `x` simulates 27% more variability than the data show in the
  control region, which is exactly the quantity `nsec` keys off. The diagnostic
  has to be **local**.
- **The statistic must be residual-based, not raw.** Within a bin the raw SD of
  `y` mixes residual variability with the slope of the curve across the bin. On
  `manec_example` the top bin's raw SD is 1.72 against a residual SD of 0.88.
  Getting this wrong makes every steep-region bin look overdispersed.

**Report per-candidate-model rows for a `bayesmanecfit`, not just the averaged
row.** Stacking weights come from a global `elpd`, so a candidate can hold high
weight while fitting the control badly — it wins on the bulk of the curve and pays
almost nothing for the control. Without per-model rows the table cannot say which
model is doing the damage.

**Zero fraction.** For the four mixture families, report `mean(y == 0)` against
`mean(yrep == 0)`. Whether the zero fraction is right is the whole question those
families exist to answer (#104), and nothing currently reports it.

**One decision RF flagged but did not answer — decision (d).** Whether
`bayesnechurdlefit` also gets a **combined** check: draw alive/dead from the
survival fit, then a growth value per survivor, reconstructing the full observed
response including its zeros. It is the only check of the hurdle fit *as a model
of the data the user handed in*, rather than of its two halves separately. RF
leaned toward including it. **Build A, B, C without it; if time remains, add it
behind an argument and flag it in the PR for a decision.** Do not let it hold up
the rest.

**Vignette.** The natural home is a model-checking section, but anything added to
`example1.Rmd.orig` will not render until #190. Author the `.Rmd.orig`, note it in
the PR, do not run `precompile.R`.

---

## 8. #33 — factor covariate, stage 1 only

Feasibility assessed in the issue comment; RF cleared it for the stack on
2026-08-21. **Stage 1 only.**

**Stage 1 — in scope.** Fit each factor level independently, model-averaged within
level; read crossed model weights off the per-level weights; expose a
`bayesnecgroupfit` class with `print` / `nec` / `ecx` / `plot` dispatching per
level. Structurally this is the existing `bnec_hurdle()` / `crossed_weights()`
pattern renamed: levels partition the data disjointly and share no parameters, so
`elpd` is additive, and under pseudo-BMA — the package default at
`R/helpers.R:538` — the crossed weights are the outer product of the per-level
vectors.

Report both readings of the crossed table: the **unrestricted maximum**, which
answers this issue's premise that the functional form may change across levels;
and the **diagonal** maximum `w_m ∝ Π_g w_{g,m}`, which asks which single equation
best describes every level and which the package cannot answer today. A pooled fit
ignoring the factor is scored on the same observations, so "does the factor matter
at all" falls out of the same arithmetic.

**Stage 2 — out of scope, toxval-gated.** The joint dummy-coded refit lands in
`nec()` / `nsec()` / `bnec_newdata()` / `predict()`, which is the territory the
migration moves. The assessment says so explicitly. Do not start it.

**Hazards.**

- With 23 models the crossed array is `23^G`. Store per-level weight vectors and
  compute cells on demand; never materialise it.
- The stacking caveat at `R/bayesnechurdlefit-class.R:245-250` applies unchanged:
  stacking optimises a different objective whose solution is not an outer product.
  Carry the caveat across rather than restating it wrongly.
- **Family must be chosen once from the whole response and passed down.**
  `set_distribution()` applied per subset could otherwise select different families
  at different levels, making the levels incomparable.
- Per-level dispersion is what separate fits give you. Do not quietly introduce a
  shared one.

**Stop condition specific to this issue.** If stage 1 turns out to require changing
`nec()`, `nsec()`, `ecx()` or `bnec_newdata()` beyond adding a level-aware wrapper
around them, **stop and report** — that means the stage boundary is in the wrong
place, which is a finding worth more than a partial implementation.

---

## 9. #6 + #33 — the grouping vignette

**One vignette covering both**, per RF on both issues (2026-08-21): they are two
ways of dealing with data grouping and belong together. Cut this branch from #33's,
since it demonstrates #33's output.

**#6 is otherwise complete.** The capability landed in v2.0 and is current:
`ogl(group)`, `pgl(group)` and `(par | group)` in a `bayesnecformula`, validated by
`check_formula()`, translated by `make_brmsformula()`, with coverage across six
test files. The old `random` / `random_vars` arguments are deprecated. The **only**
thing holding #6 open is this vignette.

**Content, from the #6 comment.**

- all three group-level term types on a real fit, and when each is right —
  `ogl()` for a shifting response level, `pgl()` where grouping plausibly affects
  the whole curve, `(par | group)` where there is prior reason to expect a specific
  parameter to vary;
- what the terms do to the fitted curve and to the `nec`/`ecx` estimates, relative
  to the same fit without them;
- prior specification for the group-level standard deviations, since the defaults
  are not tailored to these terms, and `check_formula(..., run_par_checks = TRUE)`;
- the diagnostics that matter and the **failure modes** — hierarchical structure on
  non-linear parameters is easy to specify and hard to identify, so be honest about
  when a group-level term is not supported by the data;
- the #33 route alongside it, as the other way to handle grouping;
- a dataset with genuine grouping structure.

**Source material.** RF points at
[open-AIMS/cr_modelling_training](https://github.com/open-AIMS/cr_modelling_training),
particularly `vignettes/8Factor_covariates_and_groupings.Rmd`. Read it before
authoring; it may supply both the worked examples and a dataset.

The caution already in the JSS paper — that hierarchical effects in a non-linear
setting are non-trivial and need careful thought about structure and priors —
should carry into the vignette rather than being asserted and left there.

**Do not run `precompile.R`.** Author the `.Rmd.orig`; it renders under #190.
**Do not touch `vignettes/example7*`** — #193 is another session's.

---

## #190 — full `precompile.R`, attended, once per release

RF, 2026-08-21: *"do this immediately before the CRAN submission, so all vignette
changes and anything that might change vignette numbers is completed."*

**A consequence that needs a decision at review time.** If 2.1.4 ships to CRAN
before 2.2.0, `precompile.R` has to run **twice** — once before each submission —
because #136, #148 and the #6/#33 vignette all change vignette content after the
2.1.4 boundary. It takes hours and needs the full fitting stack. The alternatives
are to accept the double cost, or to hold the CRAN submission until 2.2.0 is ready
and precompile once.

Everything merged in this stack changes vignette numbers: #216 changed every
model-averaged value; #210 and #207 change priors and therefore fits; #139, #148
and item 9 add or edit vignette prose.

**Not for the unattended run.** Needs `NOT_CRAN=true`, network access at authoring
time, and hours of machine that the simulation study currently owns.

---

# Deferred — toxval, unchanged

| | |
|---|---|
| #120 | changes `predict`/`plot`/`autoplot` for `bayesmanecfit`; toxval registers the same methods. D5 stands. |
| #93 | the `check_data()` shift correction applies to `ecx`/`nsec` too; doing half strands the other half. |
| #160 | *NEC* mis-plotted when a function is called for `x`; likely shared post-processing with #196. |
| #161 | probably #195 or #196, in which case it is toxval's. |
| #206 | **RF chose option 3** — add the zero-bounded models to the default Gaussian set — but **deferred to after the migration**, with a significant version bump and a NEWS entry when it lands. Options 2–4 all need the `R/ecx.R:161` absolute-ECx guard resolved, and that coupling disappears with the migration. |

Out of scope entirely: #39, #44, #166, #195, #196 — see `02_deferred.md`. #193 is
another session's.

---

# Open, not in this stack

| | why |
|---|---|
| #218 | the three unseeded permutations. Documentation-and-constraint outcome, not a code fix — see the issue. Cheap; add to a later stack. |
| #184 | `future_apply`. **Attended, and blocked on the machine.** RF has had trouble with `future_apply` on WSL and wants a testing pass with the findings posted as a comment before any implementation. Must wait until the simulation study finishes — it is the same resource. |
| #139 option C | the Pires et al. (2002) two-component model, conditional on reading *Environmetrics* 13:15–27. |
| #148 decision (d) | the combined hurdle check — see item 7. |
