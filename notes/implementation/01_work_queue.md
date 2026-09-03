# Work queue — coverage first

Read `00_protocol.md` first, then `03_decisions.md`.

**Rebuilt 2026-09-03.** The queue that stood here was written on 2026-08-25 for
a stack of nine issues that has since merged, and it was organised around a
CRAN release gate that no longer applies. Two decisions changed it:

- **The release date is flexible** (RF, 2026-09-03), and the toxval migration
  is not blocked by it. So there is no release gate to order the work around,
  and `06_review_run.md`'s phase 4 is no longer the destination.
- **The work is ordered by test coverage of the paths that keep producing
  defects**, not by issue number, not by release tier, and not by the order in
  which issues happen to be reported.

The rest of this file states why that ordering was chosen, and what is in it.

---

# Why coverage is the ordering

Nineteen issues were opened between 2026-08-24 and 2026-09-03 and six were
closed. The question that ordering had to answer is whether that is a
pre-existing defect population being surfaced, or work generating its own work.
It was measured rather than argued, on 2026-09-03, against `origin/master`
(2.1.3.1, the current CRAN release):

| issue | present in `origin/master` |
|---|---|
| #256 identity forced only for a character string | `R/validate_family.R:14-15` |
| #258 write-back decided per formula | `R/fit_bayesnec.R:36` |
| #265 `x_type == "beta"` unreachable | `R/check_data.R:55`, `:63` |
| #266 `n_trials = 1e4` | `R/inits_functions.R:101` |
| #267 `has_family_changed()` called positionally | `R/bnecfit-methods.R:150` |
| #268 all-or-nothing `xform` guard | 2 sites in `plot.R`, 2 in `autoplot.R` |
| #269 predictor zero shift | `R/check_data.R:48`, `:56` |
| #272 integer branch with no `else` | `R/set_distribution.R:48` |
| #244 no `constant()` handling | absent from `R/inits_functions.R` |

Nine of nine. One issue in the whole run was caused by the run's own work:
**#271**, which #270 records as becoming reachable when the predictor
substitution was removed. Everything else predates the work by between two and
six years; `check_data.R` dates from `89a15d03`, 2020-05-25.

**The code producing the defects is the code with no test file.** `check_data.R`
had none in either version, and it is the origin of #258, #265, #269, #271 and
#274. `plot.R` and `autoplot.R` had none, and they are #268, #160 and #161.
Where a test file was written, discovery stopped:

| file | `expect_` calls, master → dev |
|---|---|
| `test-validate_family.R` | 0 → 99 (did not exist) |
| `test-inits_functions.R` | 0 → 114 (did not exist) |
| `test-fit_bayesnec.R` | 0 → 34 (did not exist) |
| `test-define_prior.R` | 33 → 103 |

Suite-wide the assertion density per line of `R/` has roughly doubled, from
0.052 (380 assertions, 7,338 lines) to 0.101 (1,678 assertions, 16,586 lines).

**The consequence for ordering.** A defect found by a user report is one
defect; a test file written over the path that produced it is the whole
population on that path. So the queue puts the test file before the fixes it
will inform, and expects each test file to open two to three further issues
rather than treating that as a failure.

**The one instance of genuine re-work, and why it is not the pattern.**
`define_prior()`'s zero guard was written three times in eight days: #210 wrote
it, #229 found it errored for families that never use the guarded quantiles,
#232 found it was a step function on a continuous collapse. Nothing has touched
that guard since 2026-08-22. The cause was diagnosed at the time — #210's tests
asserted only that the prior rate was finite and positive, so every row of the
broken sweep passed them — and the review question that came out of it, *do the
tests constrain the thing that matters or the thing that is easy to assert*, has
held since.

---

# 0. Blocking everything — #275

**`R-CMD-check` is red on `dev` and the cause is the workflow, not the package.**
The **Use Cmdstan to Fix** step force-installs `StanHeaders` from whichever
repository answers, and when `mc-stan.org` is unreachable it installs 2.39.1
over the 2.32.10 `pak` matched to `rstan` 2.32.7. Seventeen tests then fail with
`invalid connection` from `brm()`, identically on `dev` and on any branch.

Nothing else in this queue can be verified while the matrix cannot distinguish a
real regression from this one, and the failure is intermittent by construction —
the same commit passes or fails depending on whether the index answers. **Do
this first.** `.github/` is open for #275 only, on the same narrow exemption
#215 and #230 had.

---

# Tier A — coverage of the paths that keep producing defects

Ordered so that the test file for a path precedes the fixes on it.

| # | item | what | size | status |
|---|---|---|---|---|
| A1 | `test-check_data.R`, `test-plot.R`, `test-autoplot.R` | the three missing test files | M | **PR #276 open** |
| A2 | #274 | `update()` with `newdata` discards the correction it reports | S | ready |
| A3 | #268 | `xform` skipped on the predictor axis when the response is transformed | S | ready; A1 pins the reproduction |
| A4 | #271 | no `disp()` sub-model is checked for finiteness before `brm()` | M | ready |
| A5 | #272 | `set_distribution()` returns `NULL` for a negative integer response | S | ready |
| A6 | #266 | `make_good_inits()` spends 561 s before falling back | M | ready |

**A1 is the gate on A2 and A3 and nothing else.** A2 and A3 are both pinned as
current behaviour by the test file, with the assertion to invert named in a
comment, so the fix is a one-line inversion plus the code change rather than a
new reproduction.

**A4 and A5 are independent** and can be taken in either order.

**A6 is a performance defect, not a correctness one.** It is in this tier
because it is on the same path and because #79 measured the same mechanism at
1050 s and closed it as *not reproducible*, which is the outcome to avoid
repeating.

## What A1 found while being written

Recorded here because they have no issue yet and are too small to warrant one
each. Raise them as one issue when A1 merges.

- **Two branches of `check_data()` cannot be reached.** The non-numeric
  predictor branch (`:112-118`) is preceded by `retrieve_var(error = TRUE)` at
  `:108`, which raises first with a different message; the numeric group-level
  branch (`:203-209`) is preceded by `model.frame()`, which refuses first. Both
  are the shape of #265 — a condition written, never fired, unnoticed.
- **`NA` and `NaN` are dropped rather than refused.** The finiteness guard sees
  only what `model.frame()` passes it, and incomplete cases are removed first.
  `Inf` reaches the guard and is refused; `NA` and `NaN` are removed silently
  and the fit proceeds on fewer rows than the user supplied.
- **`ggbnec_data(x, nec = FALSE)` does nothing.** The argument is `add_nec`;
  `autoplot()` takes `nec` and forwards it. `ggbnec_data()` is exported and
  documented separately, so the obvious transfer of the argument name is
  absorbed by `...` and the annotation is still returned.

---

# Tier B — decisions that are RF's, not the session's

Each is a legitimate change whose *correct statistical behaviour* is the
undetermined part, which `00_protocol.md` makes a stop-and-ask. None is blocked
on anything in tier A.

| # | the decision |
|---|---|
| #273 | the default `nec`/`ec50` gamma prior peaks at `2m`, not at `m`. Two candidate corrections are on the issue and the choice needs a measurement, not an argument. Worst on the linearly spaced designs this field uses |
| #93 | narrowed by #270. The predictor half is removed; what remains is whether the two silent response corrections should speak, and whether substitutions should be recorded on the fit. Two resolutions are on the issue |
| #262 | report `P(dispersion > 1)`, and state that `beta_binomial` does not address under-dispersion |
| #261 | record which equations `bnec()` excluded, and report the candidate set as fitted |
| #257 | **unblocked.** It depends on #256, which closed on 2026-09-02 via #259 and #260. Not previously in any queue |
| #206 | **re-check whether it is still deferred.** Its own comment records that #170 is discharged and that #256 settles the link policy it needed. `02_deferred.md` defers it on a coupling that may no longer bind |

---

# Tier C — the vignette pull requests

**Kept open deliberately.** RF, 2026-09-03: reviewing them and their related
issues is what has raised the defects, and they should not be closed until the
software has stabilised. They are also the only end-to-end exercise of the
fitting stack that exists — #242 removed `--ignore-vignettes`, but CI checks the
rendered markdown in about sixteen seconds and re-fits nothing.

| PR | issue | note |
|---|---|---|
| #228 | #6, #33 | the grouping vignette |
| #238 | #219 | the workflow vignette; still needs rewriting onto `screen_models()` |
| #243 | #193 | example7, another session's |
| #225 | #209 | `CONFLICTING`, blocked on `brms` via #249 |

**The example8 diagnosis in `06_review_run.md` is wrong and should not be acted
on.** That file records both halves of example8 failing on 2026-08-24 and
concludes the cause is the data, leaving an open decision about importing
datasets from `cr_modelling_training`. Two package defects fixed since then
explain both halves:

- `bnec_group(fvfm ~ crf(log(concentration), "decline"), herbicide, ...)` failed
  with *None of the models fit successfully*. That is #258's reproduction — an
  inline `log()` on the predictor discarded the boundary correction on the
  response, and ametryn, irgarol and hexazinone all contain exact zeros in
  `fvfm`. Fixed by #264 on 2026-09-02.
- `bnec(suc | trials(tot) ~ crf(dose, "nec3param") + ogl(tank), ...)` failed with
  *Failed to fit model nec3param*. That is #245 — a binomial response with an
  `ogl()` term got no group-level prior, so the mean started outside its support.
  Fixed by #250 on 2026-08-26.

**Re-run example8 against current `dev` before deciding anything about
datasets.** The separate objection that the `nassarius` contaminant levels sit
on non-overlapping dose grids, one with three doses, is a data judgement that
neither fix addresses and stands on its own.

**The rebase overhead is the price of having no integration test.** The cheapest
reduction is to precompile one vignette per merge to `dev` rather than the whole
set at release; #251's fan-out already makes that possible.

---

# Tier D — the release, whenever it is cut

Not a gate on anything above it any more.

| # | what |
|---|---|
| #190 | the full `precompile.R`. Attended, once, immediately before submission |
| #248 | the rendered `example2` still documents the 1.05 Rhat default; rides on #190 |

`DESCRIPTION` `Version` is a running dev counter, 2.1.3.25 today, incremented by
one in the fourth component per PR. **RF sets the release version.** The `NEWS.md`
tier headings are `# bayesnec 2.2.0` for everything current; the 2.1.4 tier
closed when the stack merged.

---

# Not in this queue

| # | why |
|---|---|
| #255 | the toxval migration tracker. Attended, spans two repos, ordering constraint in D8 |
| #249 | the factorised count hurdle, blocked on `brms`. Holds #209 and PR #225 |
| #218 | unseeded permutation in `compare_posterior()`. Documentation-and-constraint outcome; cheap, add to a later pass |
| #184 | `future_apply`. Attended: RF wants a testing pass posted as a comment before any implementation |
| #245 | **merged and needs closing by hand.** PR #250 states `Closes #245` but targeted `dev`, so the keyword never fired |
| #265, #269 | **merged and need closing by hand.** PR #270 states `Closes` for both, same reason |
| #39, #44, #166, #195, #196 | toxval's. See `02_deferred.md` |
| #120, #160, #161, #206, #93 | see `02_deferred.md`; #206 and #93 are also in tier B |
