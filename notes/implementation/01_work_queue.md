# Work queue — the training-course run

Read `00_protocol.md` first, then `03_decisions.md`.

**Rebuilt 2026-09-06.** The queue that stood here was written on 2026-09-03 and
ordered the work by test coverage of the paths producing defects. That ordering
was correct and most of its tier A has merged. Four decisions taken by RF on
2026-09-06 change what follows it, so the file is rebuilt rather than amended.

## The goal this queue serves

A training course is to be written against the development version of
`bayesnec`. That fixes an end state the previous queue did not have: `dev` must
be installable, its estimators must return correct values, and its vignettes
must be finalised and re-rendered, because the course teaches from them. The
release to CRAN is still not a gate on anything here.

## The four decisions

| | decision |
|---|---|
| 1 | **`ecx()`, `nsec()` and `ecnsec()` are fixed in `bayesnec` now.** The migration to `toxval` is deferred, not cancelled. See D11 |
| 2 | **PR #228 is deferred within this run, not dropped.** PRs #243 and #238 are finalised first; #228 follows once the divergence question is settled. See D12 |
| 3 | **The full precompile runs once, after all three vignettes are settled.** See D13 |
| 4 | **The run is autonomous and stacked**, on the terms already in `00_protocol.md` |

## What changed since 2026-09-03

| | |
|---|---|
| #275 | fixed by PR #280. `R-CMD-check` is green on `dev` for the first time in the run. Nothing in this queue is blocked on the matrix any more |
| #277 | tier A item A1, merged as PR #276. `test-check_data.R`, `test-plot.R` and `test-autoplot.R` exist |
| #278 | merged as PR #279. The three argument behaviours and four dead guards A1 found |
| #245, #265, #269 | closed |

`dev` is at 2.1.3.26. `NEWS.md` headings are `# bayesnec 2.2.0` and
`# bayesnec 2.1.4`; new entries go under 2.2.0.

---

# 0. Housekeeping — do first, it takes minutes

- **Close #275, #277 and #278 by hand.** All three merged; every PR here targets
  `dev` rather than the default branch, so `Closes #n` does not fire. This is the
  sixth occurrence of that pattern.
- **Prune the stale worktrees.** Twenty-two are registered and most are on
  branches that have merged. `00_protocol.md` names the ones that must not be
  touched; the rest are removable with `git worktree remove`. This matters
  because a stale worktree holds a branch checked out, and a branch held by a
  worktree cannot be checked out again.

---

# Tier A — the remaining coverage work

Unchanged from the 2026-09-03 queue except that A1 has merged. A1 wrote the test
files that pin A2 and A3 as current behaviour, with the assertion to invert named
in a comment, so each of those is a one-line inversion plus the code change.

| # | item | what | size |
|---|---|---|---|
| A2 | #274 | `update()` with `newdata` reports a boundary correction and then discards it | S |
| A3 | #268 | `xform` skipped on the predictor axis when the response is transformed | S |
| A4 | #271 | no `disp()` sub-model is checked for finiteness before `brm()` sees it | M |
| A5 | #272 | `set_distribution()` returns `NULL` for an integer vector with negative values | S |
| A6 | #266 | `make_good_inits()` spends up to 561 s on one model before falling back | M |

A4 and A5 are independent and can be taken in either order. A6 is a performance
defect rather than a correctness one; it is here because #79 measured the same
mechanism at 1050 s and closed it as *not reproducible*, which is the outcome to
avoid repeating.

---

# Tier B — the estimators

**These are in scope for the first time.** They were deferred on the grounds that
`R/ecx.R`, `R/nsec.R` and `R/ecnsec.R` are moving to `toxval` and fixing them
here would be work discarded. Decision 1 reverses that: the migration has not
started, the training course needs correct estimates within days, and the
corrected code relocates with the files when the migration runs.

**B1 comes first because it changes a published number.** `example1.Rmd.orig:253`
fits `resp ~ crf(log(raw_x + 1), model = "nec4param")` and line 109 calls
`ecx(exp_2, xform = function(x) exp(x) - 1)` on it. That is the exact
reproduction in #196: the internal back-transform substitutes into the first
argument slot of the parsed call, so `log(raw_x + 1)` is inverted as
`log(raw_x)` and the `+ 1` is discarded. The introductory vignette therefore
reports an ECx from a back-transform that drops a term, and the course would
teach it. `example6` uses a pre-computed `log_dose` column and is unaffected;
`example1` is the only vignette using an inline transformation with arithmetic.

| # | item | what | size |
|---|---|---|---|
| B1 | #196 | inline `crf()` arithmetic discarded when back-transforming, in both `R/ecx.R:198-201` and `R/nsec.R:173-175` | S |
| B2 | #160 | *NEC* mis-plotted when a function is called for `x`. `test-autoplot.R` now exists, which is the instrument the 2026-09-03 deferral said was the precondition | M |
| B3 | #161 | post-processing failure on a case-study dataset. `02_deferred.md` records it as probably #195 or #196; B1 and B4 determine whether anything is left | S once B1 and B4 land |
| B4 | #195 | `hormesis_def` is inert in `ecx()` — its only consumer is commented out; the documented default is not the effective default; one documented sentence is implemented three ways | M |
| B5 | #206 | zero-bounded models excluded for `gaussian`. Re-check the coupling first: it was deferred on `R/ecx.R:161` refusing `type = "absolute"` for a Gaussian response with no `bot`, and on #170, which closed on 2026-08-17 | M |

**B2 and B3 may collapse into B1.** Both are suspected to share its root cause
and neither has been settled, because until 2026-09-03 there was no test over the
plotting path that would settle one. Take B1 first, then re-run each reproduction
before writing any further fix.

**B5 carries a premise that has changed.** The issue argues from
`validate_family()` forcing the identity link only for a character string, so
that `gaussian` as a bare symbol takes its own default. #256 changed that: PR
#260 assigns the identity link unless the caller wrote a `link` argument, so a
bare symbol now gets identity too. The half of #206's argument that survives is
that `gaussian(link = "log")` is still honoured and `R/check_models.R:20` is
still reachable. Re-establish the reproduction against current `dev` before
implementing.

**Record each fix on the matching `toxval` issue as it lands**, so the migration
re-lands the corrected code rather than the version `toxval` forked. The
correspondences are #196 to toxval#19, #195 to toxval#8 and toxval#12, and #39 to
toxval#40.

---

# Tier C — decisions that are RF's, not the session's

Each is a legitimate change whose correct statistical behaviour is the
undetermined part, which `00_protocol.md` makes a stop-and-ask. None is blocked
on anything in tiers A or B.

| # | the decision |
|---|---|
| #273 | the default `nec` and `ec50` gamma prior peaks at twice the median predictor, so on a linearly spaced series its mode sits on the upper truncation bound. Two candidate corrections are on the issue; the choice needs a measurement. Worst on the linearly spaced designs this field uses, so it is the one most likely to affect a course exercise |
| #257 | apply group-level deviations on a scale where the mean cannot leave its support. Unblocked by #256. **Also the likeliest explanation for PR #228's divergences**, so taking it may settle decision 2 |
| #262 | report the posterior probability of over-dispersion, and state that `beta_binomial` does not address under-dispersion |
| #261 | record which equations `bnec()` excluded, and report the candidate set as fitted |
| #93 | narrowed by #270. What remains is whether the two silent response corrections should speak, and whether substitutions should be recorded on the fit. Two resolutions are on the issue |

---

# Tier D — the vignettes

All three are finalised in this run. The ordering is decision 2: the two that are
close go first, and the one blocked on a scientific question goes last.

| order | PR | issue | state, and what remains |
|---|---|---|---|
| D1 | #243 | #193 | example7. Reported mergeable at `0a8b5d35` with `dev` merged in. Three open items: CI has never been seen green on the branch, a follow-up issue was scoped and never opened, and the 14,700-word length was flagged for a judgement never made |
| D2 | #238 | #219 | example9. A full review is on the PR and unactioned. It needs restructuring so `screen_models()` sits with the sampler diagnostics, and `summary(fit)` shown after the candidate set is fitted. **Two of its errors of fact have expired** — see below |
| D3 | #228 | #6, #33 | the grouping vignette. Blocked on a scientific question, not on code |

**Re-check the #238 review before acting on it.** It was written before #260
merged. Its statement that `bnec()` does not force the identity link, and that
`family = binomial` takes logit, described `dev` at the time and does not
describe `dev` now: the link is assigned unless the caller wrote a `link`
argument. Its other findings — the untransformed dose justification refuted by
`example6`, and `summary()` already reporting per-model dispersion — are
unaffected. Note that the review's closing observation that `CLAUDE.md` §11 is
wrong as written still stands and is now wrong for the opposite reason.

**What blocks #228.** The dataset has 72.7% of observations on a boundary, so
`Beta` requires nudging three-quarters of the data and is hard to defend, while
`binomial` and `beta_binomial` — the families the source paper used — give
2000/2000 divergences and R-hat 2.7 as soon as `ogl(chamber)` is added. Three
explanations were proposed and all three refuted by test; the record is on the
PR. It is material beyond the vignette, because PR #250 claims group-level terms
work for bounded families generally and this is evidence that the claim holds for
`Beta` and not for the binomial families.

**Open it as its own issue rather than continuing to hold it on the PR**, and
take #257 before deciding anything: a group-level deviation applied on a scale
where the mean can leave its support is the mechanism that would produce exactly
this signature on a binomial response near the ceiling, and it is the one
explanation not yet tested.

---

# Tier E — the precompile

Runs once, after tier D. Decision 3.

| # | what |
|---|---|
| #190 | the full `precompile.R`. Attended. The #251/#252 fan-out makes it a matrix rather than one serial run |
| #248 | the rendered `example2` still documents the 1.05 Rhat default; rides on #190 |

Two constraints on the run. The rendered `example1` changes if B1 lands, because
its ECx values are computed from the defective back-transform. And `example7`
alone takes about 137 minutes locally and about 2 h 56 m in CI, inside the
350-minute ceiling — PR #243's handoff note records the two traps that cost a
previous session time, and should be read before starting.

---

# Not in this queue

| # | why |
|---|---|
| #255 | the toxval migration tracker. Deferred by decision 1, not cancelled. D11 records what changed |
| #44 | hypothesis method for *NEC*/*NSEC*/*ECx* exceedance. A new API rather than a fix, and larger than the run. Tracked as toxval#41 |
| #39 | `uniroot.all` for `ecx()` and `nsec()`. It requires the `type` reference semantics to be pinned down first, which is B4, and it is an enhancement rather than a defect. Take it after B4 if the run has room |
| #120 | replacing `all_models`. D5 requires that no existing user script breaks; `test-plot.R` and `test-autoplot.R` now make that detectable, so the precondition is met, but it is behaviour change rather than a fix |
| #209, #249, PR #225 | the factorised count hurdle, blocked on `brms` upstream. `issue-136-rate-aterm` is PR #225's base and **must not be deleted** |
| #184 | `future_apply`. Attended: RF wants a testing pass posted as a comment before any implementation |
| #218 | unseeded permutation in `compare_posterior()`. Documentation-and-constraint outcome; cheap, add to a later pass |
| #27 | zero-truncated gaussian. The body is empty, and a title is not a specification |
