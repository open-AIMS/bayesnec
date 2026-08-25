# Review run — the pre-CRAN loop

> ## START HERE — 2026-08-25
>
> **Do not look for the state of the stack in this file.** It lives on GitHub,
> and the one-line-per-item index is the **status column** in the stack table in
> `01_work_queue.md`. Everything else — why a PR did what it did, what it left
> undone — is in the PR body or the issue thread. RF, 2026-08-25: the same facts
> were being written into three notes files and the tracker, and that is what
> makes it hard to follow.
>
> ```bash
> gh pr list   --repo open-AIMS/bayesnec --state open
> gh issue list --repo open-AIMS/bayesnec --state open
> ```
>
> Two things that are **not** on GitHub and will bite you:
>
> - **Every PR here targets `dev`, not the default branch, so `Closes #n` never
>   fires.** Close issues by hand after verifying the work is on `dev`.
> - **`issue-136-rate-aterm` and `issue-148-check-fit` have merged but must not
>   be deleted** — they are the bases of PRs #225 and #238, and deleting a merged
>   base closes the PR above it irrecoverably.
>
> The 2026-08-23 handoff below is kept for its lesson, not its PR list.

> ## The lesson from phases 0–3, 2026-08-23
>
> Four of the five phase 2 findings were gaps between what the code did and what
> was *claimed* about it. The review then made the same mistake one level up: it
> validated PRs against a **summary** of the requirements — `01_work_queue.md`
> item 7 — instead of against the requirements. Item 7 was wrong in four ways,
> and #226 was built to it. **Check claims against sources, including claims
> made by these notes.**
>
> The rest of that handoff has been cut: its PR list, branch advice and
> outstanding-work table are all either stale or now recorded on the PRs and
> issues themselves.

How a Claude Code session works through the *review* of the stack `01_work_queue.md`
built, and the issues that review generated. Read `00_protocol.md` first — every
working rule in it still applies unchanged, in particular the machine budget, the
scope boundaries, and the definition of done per issue.

**Written 2026-08-22, after the 2.1.4 tier merged.** That tier (#220–#223) was
merged on manual review only; an independent review afterwards found fourteen
issues, two of them user-visible regressions (#229, #230). This file exists so
that does not happen again to the remaining five PRs.

---

## The goal

`bayesnec` CRAN-ready **before** the toxval migration, in **one release**
containing every tier. RF reviews and cuts it. Ideally the only issues left open
afterwards are the ones toxval resolves.

## The three rules that govern the loop

**The loop prepares; RF merges.** Unchanged from `00_protocol.md`. The loop may
push branches, open PRs, comment, and fix. It may not merge to `dev`. #229 came
from a merge that outran its review — that is the failure mode this rule exists
for.

**Self-resolve implementation, escalate statistics.** RF, 2026-08-22: pure
programming decisions the session settles itself; anything that decides what the
*correct statistical behaviour* is gets a comment on the issue recording the
decision and its context, and the session moves on. This is the same boundary as
`00_protocol.md`'s "when to stop rather than guess", stated as a positive rule.

**Review is independent of authoring.** A PR is reviewed in a session with no
authoring context, and the review is posted as a PR comment **before** any code is
touched. A session that wrote the code does not review it.

---

## What "not changing existing fits" does and does not cover

RF, 2026-08-22: most of what is on `dev` is new work in the current development
phase, so prior changes do not disturb released results.

**True for:** `zero_inflated_poisson`, `zero_inflated_negbinomial` (new in 2.1.4,
#104), `hurdle_poisson`, `hurdle_negbinomial` (#209), everything behind
`bnec_group()` (#33), the `rate()` aterm (#136), `check_fit()` (#148).

**Not true for:** `poisson` and `negbinomial`, which are on CRAN today and route
through the *same* `u_t_g` / `u_b_g` code that #210 changed and #232 proposes to
change again — checked against `origin/master:R/define_prior.R:40-52`. A plain
`poisson` response with 30–70% zeros is a released code path, and it is the regime
#232's table covers. Also not true for `Gamma`, `gaussian`, `beta`, `binomial`,
`beta_binomial`, `bernoulli`, `hurdle_gamma`, `zero_inflated_beta`.

Do not use the phrase as a blanket exemption. It licenses #231 and the
zero-inflated half of #232; it does not license changing the poisson prior path
without saying so in `NEWS.md`.

---

## Phase 0 — make the stack coherent

Nothing else can be trusted until this is done.

1. **PR 224 retargeted to `dev`** — done by RF, 2026-08-22. It targeted
   `issue-207-dispersion-priors`, which PR 223 had merged but not deleted, so
   GitHub had not retargeted it and merging would have landed the work on a dead
   branch. **Check the same thing for 225–228 before touching them**; the trap
   recurs every time a base branch merges without being deleted.
2. Delete merged branches: `issue-215-dev-vignette-ci`,
   `issue-139-drc-nec-equivalence`, `issue-210-define-prior-zeros`,
   `issue-207-dispersion-priors`.
3. Close #215, #139, #210, #207 with a line pointing at the merging PR. All four
   merged and all four are still open, which puts four false positives into the
   remaining-issues list phase 3 works from.
4. Triage **#219** (complete-analysis-workflow vignette, opened 2026-08-21). It is
   the one open issue in neither `01_work_queue.md` nor `02_deferred.md`. RF,
   2026-08-22: **build a first draft.** Author `.Rmd.orig` only — `precompile.R`
   is #190's, per `00_protocol.md`.
5. ~~Restack 224→228 onto current `dev`.~~ **Deferred to the end of phase 1**
   (2026-08-22). Checked: all five are `MERGEABLE`, and the four commits they are
   behind are only the tier merge commits, whose content is already in their base
   — `issue-207-dispersion-priors` is an ancestor of all five. So each PR's diff
   against `dev` is already exactly its own work, and restacking now buys nothing.
   Phase 1 lands four more PRs on `dev`, which would force a second restack.
   Restack **once**, at the end of phase 1, immediately before the phase 2 review.
   Sequencing call, self-resolved.

**Done when** all five PRs target the right base and the open-issue list contains
only genuinely open work. (Being *current* with `dev` moves to the end of phase 1.)

**Phase 0 outcome, 2026-08-22.** Bases checked: 224 → `dev` (RF), and 225–228 each
target the live branch below it, so no dead-base problem today — but the trap
recurs every time a base merges without being deleted, so re-check before each
merge. Four merged branches deleted. #215, #139, #210, #207 closed with pointers
to the merging PR and to the follow-ups they generated. #219 triaged and scheduled
as a phase-1 branch. #232's decision recorded, with a correction: the proposed
`1 - (1 - probs) / (1 - zero_frac)` inverts an operator and must be
`1 - (1 - probs) * (1 - zero_frac)`; as written it moves the level the wrong way
and goes negative past 75% zeros.

**One blocker found, on #230 — see the phase 1 note below.**

---

## Phase 1 — clear `dev` before reviewing anything against it

Three small branches cut from `dev`, each its own PR. Deliberately **before** the
stack review: #229 is a live regression in the base all five PRs build on, and
PR 224 touches the prior derivation next door to it. Reviewing five large PRs
against knowingly-broken code wastes the reviews.

| issue | what | kind |
|---|---|---|
| #229 | `define_prior()` errors on responses with no positive values, for families that never use the guarded quantiles; `add_brm_defaults()` builds defaults eagerly so there is no workaround | implementation — **self-resolve** |
| #230 | pkgdown `mode: auto` resolves `master` to devel; plus the missing `permissions:` blocks, the no-op `~/.cmdstan` cache, and the no-op top-level `on.exit()` | implementation — **self-resolve** |
| #231 | `get_priors()` drops a `zi` prior for the ZI count families | **split**: adding `"zi"`/`"hu"` to the kept classes is implementation, self-resolve. Whether bayesnec should *generate* a `zi` prior is statistics — comment and move on |
| #232 | the #210 zero guard is a step function on a continuous collapse | **statistics — escalate.** Three candidate approaches, and per the section above one of them changes released `poisson` behaviour. Comment on the issue with the trade-off; do not choose |

**#230 is on the critical path. It is not blocked — corrected 2026-08-22.**

Two mechanisms were initially run together. Separated:

*Publishing the `dev` site needs nothing on `master`.* `pkgdown.yaml` on `dev`
triggers on push to `dev` and deploys with `clean: false`. RF's counter-examples
(`ssdtools`, `ssddata`) are right and the earlier recommendation to carry the
pkgdown files to `master` is withdrawn.

*`workflow_dispatch` genuinely requires the default branch*, confirmed by
GitHub's own error — `HTTP 404: workflow precompile-vignettes.yaml not found on
the default branch`. That is specific to the precompile workflow and unrelated to
pkgdown. **Resolved without touching `master`:** give it a `push` trigger on a
dedicated `precompile/*` branch namespace alongside `workflow_dispatch`, so it can
be exercised from `dev` now and becomes dispatchable normally once it reaches
`master` at release. Self-resolved; no RF decision needed.

*The pkgdown `mode: auto` defect is a versioning convention, not a code change.*
`ssdtools` carries `2.6.0.9002` on `main` with `mode: auto`, and its `/dev/` site
is built from `main` precisely because a four-component version resolves to devel;
the root publishes on release, at a three-component version. `mode: auto` can only
separate the two sites if **`master` carries a three-component version**.
`master` is at `2.1.3.1`, which is why it resolves to devel. `bayesnec` has
historically released four-component versions, and that is the incompatibility.
Cutting 2.1.4 as `2.1.4` fixes it and keeps it fixed while releases stay
three-component; a `2.1.4.1` on `master` would silently undo it. **A policy call
for RF at release time**, recorded rather than decided here.

`ssdtools` also declares `permissions: contents: write`, corroborating the missing
`permissions:` finding.

`.github/` is open for #230 only — the same narrow exemption #215 had.

**Done when** `dev` has no known regressions, #232 is implemented, and the
precompile workflow has demonstrably run.

### Phase 1 progress, 2026-08-22

| issue | branch / PR | state |
|---|---|---|
| #230 | `issue-230-ci-fixes` → **PR 233** | **done and verified.** Dry run 32579445161 succeeded through every step including PR creation, which settles the permissions question empirically. Throwaway artifacts cleaned up. |
| #229 | `issue-229-prior-scope` | code + tests written; test run in progress |
| #231 | `issue-231-zi-prior` | code + tests written, committed locally; untested |
| #232 | `issue-232-zero-guard-rescale` → **PR 236** (on 235) | done; 117/117 in test-define_prior.R |
| #231 | `issue-231-zi-prior` → **PR 237** | done; get_priors 47, inits_functions 260, helpers 37 |
| #219 | `issue-219-workflow-vignette` → **PR 238 (draft)** | drafted on PR 226; **rescoped, see below** |

**Phase 1 is complete as far as an unattended session can take it.** Five PRs
are open and none is merged, per the loop-prepares rule. **R CMD check is green
on every platform for all four non-draft PRs** (233: 10/10, 235: 9/9, 236: 8/8,
237: 9/9 — 236 runs eight rather than nine because pkgdown does not trigger on a
PR whose base is not `dev` or `master`).

**The restack does not belong at the end of phase 1 after all.** It was moved
here from phase 0 on the reasoning that phase 1 would land four more PRs on
`dev`. It does not: the loop does not merge, so `dev` is still at 9ed1bb8c and
there is nothing new to restack onto. All five stack PRs remain `MERGEABLE` with
correct diffs. **The restack belongs at the start of phase 2, after RF has
merged whatever they choose to merge from phase 1** — and it should be done
once, then, against whatever `dev` has actually become.

**#232's approach is settled** (RF, 2026-08-22): rescale the probability rather
than the vector. One correction was needed and is recorded on the issue — the
proposed `1 - (1 - probs) / (1 - zero_frac)` inverts an operator and must be
`1 - (1 - probs) * (1 - zero_frac)`; as written it moves the level the wrong way
(recovering 0.5 rather than 37.75 on a 50%-zero test vector) and goes negative
past 75% zeros. Correcting algebra to match a stated intent is implementation,
so it is self-resolved, but flagged for override.

**#219 cannot be drafted as its issue specifies.** It gates steps 3–5 on a
"Part D of #148" that `01_work_queue.md` never scoped and PR 226 does not
deliver — 226 is `R/check_fit.R` and `R/pp_check.R` and nothing else. Not fatal:
`rhat()`, `check_chains()` and `amend(drop = )` are already on `dev` and cover
steps 3 and 5; a Part D would be convenience, not capability. Two consequences,
both recorded on #219: the draft branches from `issue-148-check-fit` rather than
`dev`, because step 4 genuinely needs `check_fit()`; and **whether a Part D
should exist as package API is now an open question**, not a settled dependency.
Also carry it into PR 226's phase 2 review.

### Carry into the phase 2 review

**"Do the tests constrain the thing that matters, or the thing that is easy to
assert?"** is now an explicit review question. The #210 tests asserted only that
the prior rate was finite and positive — every row of the broken sweep, including
the prior centred at a sixth of the true asymptote, passes them. That is why the
defect survived review and merge. #232's tests constrain the prior to be on the
scale of the data instead.

**#226 does not deliver a "Part D"** and does not say so. Whether one is wanted
is open — see #219.

### Two test-authoring traps hit in this phase, worth not repeating

- `validate_family()` dispatches on the **brms constructor name**, so it is
  `"Beta"`, not `"beta"` — the latter errors with `unused argument
  (link = "identity")`, which does not obviously point at the cause.
- **Do not let a test reach `make_good_inits()` on a degenerate response.** A
  test that called `add_brm_defaults(skip_check = FALSE)` on an all-zero poisson
  response ran for 27 minutes before being killed: no draw can put the curve
  inside a zero-width response range, so the search retries until it gives up.
  Pass `init =` to skip the search when the test is about something else.
- Related: an all-zero response is not realistic input for the bounded families
  and trips a pre-existing `min()` warning in `response_link_scale()`. Test
  those with a response containing zeros, not one that is entirely zeros.

---

## Phase 2 — review the stack, read-only

### Phase 2 outcome, 2026-08-23 — complete

All five reviewed, read-only, one comment each. No code was changed on any of
them, per the phase rule.

| PR | issue | verdict | finding |
|---|---|---|---|
| 224 | #136 | sound | `?bayesnecformula` overstates the family check — it is skipped when `make_brmsformula()` is called without a family |
| 225 | #209 | sound; better than scoped | `?ecx`/`?nsec` still say `dpar` applies only to `hurdle_gamma`/`zero_inflated_beta`; it now applies to the count hurdles too |
| 226 | #148 | sound | the **plot** half of part B is missing and is not declared; `skip_if(NOT_CRAN == "")` deviates from `skip_on_cran()` used by 17 other files |
| 227 | #33 | boundary correct | **real defect** — the pseudo-BMA identity is documented but not enforced, and `loo_controls` passes through `...`, so stacking weights silently produce a wrong crossed table |
| 228 | #6/#33 | complete | filename collision with PR 238 — resolved by renaming 238's vignette to `example9` |

**Both hazard-verifications the queue asked for came out clean**, checked
empirically rather than by reading: the `retrieve_var()`/`bnec_pop` lockstep
holds across six formula shapes including `rate+group`, and #33's stop
condition is not triggered — PR 227 touches none of `ecx`/`nsec`/`nec`/
`bnec_newdata`.

**The pattern worth carrying forward.** Four of the five findings are gaps
between what the code does and what is *claimed* about it — stale docs, an
undeclared omission, an unenforced caveat — rather than defects in the code
itself. The single real defect (227) is of the same family: a caveat that was
written down instead of being checked. This tier's code is careful; its
declarations drift.

**Two hypotheses were checked and retired before being reported**, recorded so
nobody repeats the work: the `check_fit` fitting tests *do* run in CI
(`SKIP 3 | PASS 1600` on `issue-148-check-fit`, 62 more than the branch below),
and PR 228's `library(dplyr)` is already declared (`Imports`), so it is not an
undeclared vignette dependency.

**For phase 4:** `WARN 10` appears on every platform on every stack branch —
pre-existing, but ten testthat warnings should be cleared before submission.
Also note `R-CMD-check.yaml` passes `--ignore-vignettes`, so nothing in CI
checks the vignettes; the first real check of them is #190.

## Phase 2 — the original plan

PRs **224 → 225 → 226 → 227 → 228**, oldest first. One review comment per PR.
**No code is touched in this phase.**

Review all five before fixing any. Fixing as you go means five rebases, five
rounds of invalidated CI, and review comments written against a base that moved
underneath them — and it forecloses the case where a finding on 226 changes what
should happen to 224.

Each review comment covers: correctness against the issue's stated scope, the
hazards named in that item's `01_work_queue.md` entry (they are specific and were
written for this purpose), test coverage of the edge cases, and whether anything
crossed a scope boundary — `ecx()`/`nsec()`/`ecnsec()`/`zero_crossings()` above
all.

Findings are classified as they are written:

- **in scope, straightforward** → fixed in phase 3 on the existing PR;
- **out of scope, straightforward** → one new issue, with a reprex and the steps to
  resolve. Prefer folding several small findings into one issue over opening
  several; #230 carries three that way;
- **statistics** → comment on the PR and on the issue, no code.

**Done when** five review comments are posted and every finding is classified.

---

## Phase 3 — one fix pass, one restack

Bottom-to-top: 224, then 225, and so on. Fix the in-scope findings on the existing
PR, comment on the PR saying what changed, and only restack the branches above
when a fix actually touches shared files. One restack at the end of the phase, not
one per PR.

Then reassess the issue list — #229–#232, anything phase 2 opened, and #219 — and
work it in dependency order rather than numeric order.

**Done when** every PR is either merge-ready with a review comment saying why, or
explicitly parked with a reason.

---

## Phase 3 outcome, 2026-08-23 — complete

All four findings fixed on their own PRs; no new issues opened, which was the
goal of minimising them.

| PR | fix | tests |
|---|---|---|
| 224 | `?bayesnecformula` no longer overstates the rate family check | doc only |
| 225 | `?ecx`/`?nsec` list all four two-block families | doc only |
| 226 | `plot.checkfit` implemented; all 11 skips normalised to `skip_on_cran()` | 48/48 |
| 227 | **pseudo-BMA enforced** rather than documented | 30/30 |
| 228 | restacked over the four | — |

**Why 227 had to capture the method at fit time:** a `bayesmanecfit` does not
record which weighting method produced its `wi` — its fields are `mod_fits`,
`success_models`, `mod_stats`, `sample_size`, the `w_*` slots and `ne_type`.
So it cannot be recovered afterwards; `bnec_group()` reads it off
`loo_controls` and stores it, and `crossed_group_weights()` refuses anything
else. An error, not a warning: under stacking there is no correct crossed table
to return.

### Version conflicts are structural, not accidental

Every branch that targets `dev` independently and bumps `Version` will conflict
on that line — unavoidable with parallel branches, and the reason the 224–228
stack does not suffer it. After #233 merged, #235, #237 and #224 all conflicted
at once. Versions are now pinned monotonically along the intended merge path:

```
dev .14 → 235 .15 → 236 .16 → 237 .17 → 224 .18 → 225 .19
       → 226 .20 → 227 .21 → 228 .22 → 238 .23
```

Merging in that order costs no further resolves. Out of order costs one line
each time.

**Delete each branch as it merges.** `issue-230-ci-fixes` was merged and left in
place, which is the same dead-base trap that left PR 224 targeting a deleted
branch in phase 0. An undeleted merged base stops GitHub retargeting the PR
above it.

### Two process failures worth not repeating

- **A resolved conflict was reported as fixed before it was pushed.** #235 was
  resolved locally and left unpushed; GitHub still showed it conflicting. Verify
  against the remote, not the working tree.
- **A resolve script assumed its `git checkout` had succeeded.** It had not —
  the target branch was checked out in another worktree — so the script rewrote
  the *previous* branch's `DESCRIPTION`. Nothing was committed, but it was one
  step from corrupting a just-fixed branch. **Work detached** when scripting
  across branches; `--detach` cannot collide with another worktree.

## Phase 4 — the release gate

Not reached until RF has reviewed and merged the stack.

1. `R CMD check --as-cran` clean, on the platforms the workflow covers.
2. `NEWS.md`: the three tier headings collapsed into the single release RF is
   cutting. **RF sets `DESCRIPTION` `Version`** — `00_protocol.md`, unchanged.
3. **#190, the full `precompile.R` run.** **The machine is free as of
   2026-08-23** — load average 0.29 on 22 cores, down from ~18; the simulation
   study in `/mnt/c/Rworking/negative-sgr` has finished. The 2-core budget in
   `00_protocol.md` no longer binds, and #190 is unblocked. Re-check `uptime`
   before starting in case another run has begun. One release means this runs **once**, which is the saving RF's
   answer to (a) buys.
4. Confirm the published site: `master` at the root, `dev` under `/dev/`, per the
   fix to #230. Verify by looking at the deployed site, not at the config.
5. Re-read `02_deferred.md` and confirm every remaining open issue is either
   toxval's or deliberately deferred with a reason.

---

## Open decision

**(b) Are #33 and #6 (PRs 227, 228) in the pre-migration release?** Put to RF
2026-08-22, not yet answered.

Stage 1 is additive — `bnec_group()` and `bayesnecgroupfit` are a new entry point
and a new class; `bnec()` and every existing class are untouched. So shipping it
does not constrain stage 2's design much: stage 2 arrives as a formula or argument
on `bnec()`, and `bnec_group()` stays a coherent thing to have.

What remains is expectation risk — "supports factor covariates" will be read as the
joint model unless NEWS and `?bnec_group` say *independently fitted levels* plainly
— and the naming overlap with #6's existing `ogl()`/`pgl()`/`(par | group)`
vocabulary, which the #6/#33 vignette has to resolve.

**Recommendation: include them**, with those two obligations written into the
review standard for PRs 227 and 228. If RF prefers not to, the cheaper alternative
is to merge them to `dev` and cut the release from a tag below them, rather than
leaving the PRs to drift unmerged across the whole migration.

Until this is answered, phases 0–2 proceed unchanged; only the review *standard*
for 227 and 228 depends on it, and phase 3 for those two PRs.

---

# Phase 3b — the CI tidy-up, 2026-08-24

A fresh session picked the run up here. **All seven open PRs were red, and not
one of them was red for a reason to do with its own subject matter.** Every
platform reported `FAIL 0`; the red X in each case was
`Error: R CMD check found WARNINGs` from one of two roxygen slips.

| cause | PRs affected | fixed on |
|---|---|---|
| `check_sampling.Rd` / `screen_models.Rd` link to `check_fit`, which does not exist until 226 | 240, 224, 225 | **240**, the base |
| `@param` missing for `combined` (`check_fit`), `fit_ratio_cutoff` and `check_fit` (`summary`) | 226, 227, 228, 238 | **226** |

Both were introduced by the phase 3 fixes themselves — decision (b) added two
`summary()` arguments and decision (d) added one to
`check_fit.bayesnechurdlefit()`, and neither carried a roxygen entry. The
`\link{check_fit}` references are demoted to `\code{check_fit()}` at the base of
the stack and **restored on 226**, the branch that introduces the function, so
the documentation is correct at every point along the merge path rather than
only at the end.

**Versions were deliberately not bumped.** The pinning
`dev .17 → 240 .18 → 224 .19 → 225 .20 → 226 .21 → 227 .22 → 228 .23 → 238 .24`
is what makes the merge path conflict-free, and a doc fix on an open PR does not
earn a new development version. The restack merged with zero conflicts on all
six branches.

## The `WARN 10` is one line

The handoff asked for this to be characterised. All ten testthat warnings, on
every platform on every branch, come from **`R/autoplot.R:188`** —
`select(.data$x_e, ...)`. `.data` in a *tidyselect* expression was deprecated in
tidyselect 1.2.0. Every other `.data$` in the package sits in a data-masking
context (`aes()`, `filter()`, `mutate()`, `arrange()`) and is correct.

Replaced with quoted names. `test-expand_classes.R` went from ten warnings to
none. Fixed at the base of the stack so it propagates to all seven PRs.

## Four issues were merged but never closed

#229, #230, #231 and #232 were fixed by #235, #233, #237 and #236, all merged to
`dev`, but the PR bodies referenced them as `(#230)` rather than with a closing
keyword. Closed with a pointer to the merging PR.

**Worth adopting:** write `Closes #n` in the PR body, not `(#n)`.

## The precompile rehearsal of 2026-08-24

`/mnt/c/Rworking/bayesnec-precompile` was found holding **an uncommitted,
unpushed, detached-HEAD tree with 39 modified files** — a full precompile run
across example1–6, 8 and 9 on an integration branch merging the 224–238 stack,
executed 23 Aug 21:50 – 24 Aug 01:53. A stray `git checkout` there would have
destroyed about four hours of fitting.

Preserved on **`precompile-rehearsal-2026-08-24`**. It is *not* a #190 run:

- **example7 is absent** — `negsgr-cens-vignette` is not merged into that tree.
- **example8 does not execute at all** (see below).
- It predates the R CMD check fixes above.
- It was killed before `precompile.R` reached its figure-move step, leaving 29
  images stranded in the repository root. That move has been completed as the
  script would have done it.

### Two findings came out of it, and neither is an artefact

**1. A `Beta` response that reaches exactly zero can fail the whole candidate
set.** The rehearsal carried an uncommitted edit to `example9.Rmd.orig` moving
the beta example off `irgarol`, which has **eight of its twelve
top-concentration replicates at exactly zero**, onto `simazine`, which spans
0.16–0.71 and never touches a boundary. Zeros by herbicide:

| herbicide | n | zeros | at the top concentration |
|---|---|---|---|
| ametryn | 96 | 12 | 7 of 12 |
| irgarol | 72 | 10 | 8 of 12 |
| hexazinone | 76 | 1 | 1 of 5 |
| atrazine, diuron, simazine, tebuthiuron | — | 0 | — |

The boundary handling `check_data()` applies is evidently not rescuing these
fits, which is worth knowing before any vignette leans on `herbicide` + `Beta`.

**2. That is very likely why example8's `bnec_group()` call fails.**
`bnec_group()` fits all seven herbicides, so it takes in all three of the
zero-carrying ones. So the example8 failure is a **data-and-family question, not
a `bnec_group()` defect** — but that should be confirmed by fitting one clean
level and one zero-carrying level singly before #33 is judged on it.

## example8 does not execute — both halves

The rendered rehearsal output shows the vignette is a sequence of errors:

```
fit_ogl <- bnec(suc | trials(tot) ~ crf(dose, "nec3param") + ogl(tank), ...)
#> Error: Failed to fit model nec3param.

fit_grp <- bnec_group(fvfm ~ crf(log(concentration), "decline"), herbicide, ...)
#> Error in expand_manec(...): None of the models fit successfully
```

Every later chunk then cascades on `object 'fit_ogl' not found`, and the
vignette produced **no figures at all**.

The rewrite at `0eea2b55` answered the conditioning objection correctly — it
moved Part 1 from nassarius *growth* to nassarius *survival*, so no animal is
filtered out. But survival turns out not to be modellable either:

- **There is almost no concentration-response in survival.** Contaminant A runs
  12% dead at the control, then 0–6% dead across seven doses spanning
  0.01–1.25, then 83% at 2.5 and 100% at 20. That is a step, not a curve — and
  the control has *higher* mortality than the next six doses.
- **The levels are not comparable.** The four contaminants sit on largely
  non-overlapping dose grids: A 0–2.5 (11 doses), **B 0.02/0.05/0.1 (3 doses)**,
  C 0.02–15 (12), D 0.001–15 (13). Three dose levels will not support a
  four-parameter model, let alone crossed weights over a 23-model set.

So `nassarius` fails for **both** halves of the vignette, for two unrelated
reasons, and the failure is not the one the PR thread diagnosed.

### `cr_modelling_training` ships data built for this

Cloned at `/mnt/c/Rworking/cr_modelling_training`. Its
`vignettes/8Factor_covariates_and_groupings.Rmd` uses three CSVs, one per thing
this vignette has to teach:

| file | n | structure | teaches |
|---|---|---|---|
| `example_ogl.csv` | 100 | coral tile % colour change, grouped by tile colour | `ogl()` — intercept only |
| `example_pgl.csv` | 176 | `log_x`, `y`, `plateRep` | `pgl()` — slope and intercept |
| `example_fi.csv` | 151 | Chlorella / Zn across hardness × pH | the factor interaction (#33) |

**The open decision is whether to bring one or more of these into the package as
data** — which needs provenance, documentation and permission — or to keep
looking in the packaged data. RF's call.

## Also standing, and not yet acted on

- **`R-CMD-check.yaml` passes `--ignore-vignettes`.** Nothing in CI has ever
  checked a vignette; #190 is the first real check of them. Do not read a green
  stack as evidence that the vignettes build.
- **example7** (`negsgr-cens-vignette`, 8 commits) is pushed with no PR. It
  commits the *rendered* `example7.Rmd` and its figures, edits `R/nsec.R`, and
  edits the rendered `example1.Rmd` and `example6.Rmd` — all three of which
  cross rules this stack has been keeping. Needs review and a targeting
  decision.
- **Vignette numbering**, settled but previously unrecorded: **7** negative
  growth (#193), **8** grouping (#6/#33), **9** workflow (#219).
- Stale worktrees `bayesnec-review` and `bayesnec-review2` can be removed.
- **The open decision from phase 3 is still open:** are #33 and #6 (PRs 227,
  228) in the pre-migration release? It now matters more, because example8's
  data problem sits on 228.
