# Autonomous implementation protocol

How a Claude Code session should work through `01_work_queue.md` without
supervision. Read this first, then the queue.

## Working rules

**One worktree for the whole run.** Create it once, at the start, and do every
issue in it. Do **not** create a worktree per issue, and do **not** work in
`/mnt/c/Rworking/bayesnec` — that is the author's main checkout.

`dev` is checked out in RF's main checkout, so `worktree add <path> dev` fails
with *"'dev' is already used by worktree at ..."*. Create the worktree directly
on the stack's **first branch** instead:

```bash
git -C /mnt/c/Rworking/bayesnec fetch origin dev
git -C /mnt/c/Rworking/bayesnec worktree add -b issue-<n1>-<slug> \
    /mnt/c/Rworking/bayesnec-stack dev
cd /mnt/c/Rworking/bayesnec-stack
uptime; nproc          # confirm the machine budget still holds
```

**Notes-only changes go straight to `dev`. No branch, no PR.** RF, 2026-08-25:
a PR that touches nothing but `notes/` still triggers the full check matrix, and
the volume of those runs had got out of hand. Commit to `dev` and push. This
covers `notes/**` and nothing else — **if a commit touches one line of `R/`,
`tests/`, `man/`, `DESCRIPTION`, `NAMESPACE`, `vignettes/` or `.github/`, it is
not a notes change** and the rule below applies in full. PRs #239 and #241 were
notes-only and went through the stack; #254 was the same and was closed unmerged
in favour of a direct push.

**Write it once, and write it in the PR body or the issue comment.** RF,
2026-08-25: the same facts were being restated in `01_work_queue.md`,
`05_run_log.md`, `06_review_run.md` *and* the tracker, and the duplication is
what makes the run hard to follow. Reasoning, evidence and what-was-left-undone
belong on the PR or the issue, where they stay attached to the change. These
notes carry only what GitHub cannot: the plan not yet started, and the
one-line-per-item **status column** in the stack table above, which points at
the PR rather than summarising it. If you find yourself writing a paragraph here
that repeats a PR body, delete it and link the PR.

**One issue, one branch, one PR, and the branches STACK.** This reverses the
rule that stood for the 2026-08-14 run. Each branch is cut from the *previous*
issue's branch, not from `dev`, and its PR targets the previous branch so the
diff shows only that issue's work:

```bash
# first issue in the stack only
git checkout dev && git pull --ff-only origin dev
git checkout -b issue-<n1>-<slug>
# ... work, commit, push ...
gh pr create --repo open-AIMS/bayesnec --base dev

# every subsequent issue
git checkout -b issue-<n2>-<slug>          # from the branch you are already on
# ... work, commit, push ...
gh pr create --repo open-AIMS/bayesnec --base issue-<n1>-<slug>
```

GitHub retargets each PR to `dev` automatically as the one below it merges, so
RF can merge straight down the stack. **Do not merge** — RF reviews and merges.

**Why stacked and not parallel:** several items in this run touch the same
files (`check_models.R`, `bayesnecformula.R`, `data-raw/sysdata.R`, the prior
code), and a parallel fan-out would produce a pile of mutually-conflicting PRs
that all need rebasing before any can land. Stacking also keeps `DESCRIPTION`
and `NEWS.md` monotonic, which parallel branches cannot.

**The cost of stacking, and the rule that follows from it:** an issue that
stalls blocks everything cut from it. So **if an issue hits a stop condition,
abandon that issue, do not branch from it** — go back to the last *good* branch
in the stack and cut the next issue from there. Record the skip in
`notes/implementation/05_run_log.md`. Never leave a broken branch load-bearing.

**Other sessions are live.** The Windows path `C:/Rworking/...` and the WSL path
`/home/rfisher/Rworking_wsl/...` are the *same checkout*, so two sessions in one
directory corrupt each other.

**Never touch these** — `master`, and any worktree you did not create:
`/mnt/c/Rworking/bayesnec-negsgr` (**live, another session**),
`-issue173` (a frozen read-only pin that session relies on), `-dispersion`,
`-betaub`, `-cens`, `-hurdlegamma`, `-216` (PR 217, merged 2026-08-21).

## Machine budget — a simulation study is running

Checked directly on 2026-08-21: `/mnt/c/Rworking/negative-sgr` is running
`analysis/phase10_run.R` at **`WORKERS=16`** on a **22-core** machine, plus a
long-running phase 9 re-extraction. Load average was 18.8. Each fit takes about
25 s, and the sweep has hours to run.

**Budget for this stack: 2 cores.** Concretely:

- `chains = 2, cores = 1` for every fit in tests and in scratch verification.
  Never `cores = 4`.
- Run test files individually where you can; run the full `devtools::test()`
  **once** per issue, not per iteration.
- Never run more than one R process at a time from this session.
- Do not run anything in `/mnt/c/Rworking/negative-sgr`, and do not kill or
  renice its processes.
- `pkill`/`pgrep` on a pattern that could match your own shell has killed the
  invoking bash repeatedly in this project. Kill by PID, or match on
  `--file=<script>`.

If the machine looks idle later in the run, re-check with `uptime` and `nproc`
before taking more; the sweep restarts workers per chunk, so a momentary lull is
not the end of it.

That session holds `R/nsec.R` and the `example1`/`example6` vignettes on a
parked branch. Tier 1 does not touch vignettes at all, and touches `R/nsec.R`
only if an issue leads there — if one does, stop and report rather than edit it.

## Definition of done, per issue

1. The behaviour described in the issue is fixed or implemented.
2. `testthat` tests covering the main behaviour **and at least one edge case**,
   in `tests/testthat/test-<file>.R` matching the existing naming.
3. `devtools::document()` if roxygen changed, and the resulting `man/*.Rd`
   committed.
4. A `NEWS.md` entry **under the heading for this issue's release tier** — see
   *Versioning across the stack* below. Not under whatever heading happens to be
   at the top of the file.
5. `devtools::test()` passes locally. Push and let GitHub Actions run R CMD
   check; **if checks fail, fix them before opening the PR for review**.
6. `DESCRIPTION` `Version` bumped by one in its fourth component.
7. PR body: what changed, why, how it was verified, and anything left undone.
   State the release tier in the first line so RF can see where the boundary
   falls without opening the diff.

## Versioning across the stack

`DESCRIPTION` `Version` is a running dev counter — 2.1.3.7 today, incremented by
one in the fourth component on every PR in the stack. It does **not** encode the
release. Because the branches stack, the counter stays monotonic and never
conflicts.

The release boundary lives in `NEWS.md`, as headings, so it is visible in the
diff and reviewable:

| heading | tier | issues |
|---|---|---|
| `# bayesnec 2.1.4` | bug fixes, docs, CI, and the feature work that landed with them — **ships to CRAN first** | #216 (landed), #139, #210, #207, #136, #209, #148 |
| `# bayesnec 2.2.0` | the factor covariate | #33, and the #6/#33 vignette |

**Revised 2026-08-25, RF.** The tiers were originally 2.1.4 bug fixes / 2.2.0
features / 2.3.0 factor covariate. #136 and #148 merged into the 2.1.4 heading
when their branches were restacked onto `dev`, and #209 is on hold, so the
feature tier no longer exists as a separate release. Rather than unpick it, the
plan now treats 2.1.4 as everything up to and including the feature work, and
**2.2.0 as the factor covariate release** --- `bnec_group()` is significant new
functionality that never existed before and warrants a release of its own.
There is no 2.3.0 tier.

The first PR of each tier opens that tier's heading. Everything after it in the
tier files underneath. **RF sets the actual release version** in `DESCRIPTION`
when cutting each release — do not set `Version: 2.1.4` yourself.

The existing `# bayesnec 2.1.3.7` heading is renamed to `# bayesnec 2.1.4` by
the first PR in the stack; its contents (the #216 work) belong to that release.

## Hard constraints

- **No new package dependencies without a decision on the issue.** If an issue
  appears to need one, stop that issue, leave a note in the PR body, and move to
  the next. Do not add to `DESCRIPTION` `Imports`/`Suggests` on your own
  initiative. This is a stop-and-ask rule, not a prohibition — RF is happy to add
  a dependency that earns its place (recorded on #148, 2026-08-21), it just has
  to be an explicit call rather than a side effect of an implementation session.
- **Do not run `vignettes/precompile.R`,** and do not regenerate any `*.Rmd`
  from `*.Rmd.orig`. Vignette rebuilds take hours and are handled separately
  under #190. If a change alters vignette output, say so in the PR body.
  This rule still stands, and a precompile is **not** a precondition for
  merging: since #242 the rendered `.Rmd` files are display-only markdown that
  `R CMD check` builds in ~16s, and `dev` has no required checks. Where a
  rendered number genuinely has to be current before a merge — it is quoted in
  prose, or the published dev site would mislead — dispatch the
  `precompile vignettes` workflow with that one vignette named, which is ~20
  minutes and opens a PR with the diff. Since #251 it fans out one job per
  vignette, so the release run under #190 is also the slowest vignette rather
  than the sum. Do not run a full local rebuild per branch; that is the drift
  #251 was opened to stop.
- **Do not edit** `CLAUDE.md` or any settings/config file. The `.github/`
  prohibition is **lifted for #215 only** (RF, 2026-08-21) — that issue is
  entirely CI work. No other issue in the stack may touch `.github/`.
- **Base R inside `R/`** to minimise dependencies; tidyverse is fine in tests
  and scripts. Native pipe `|>` only. `snake_case`. No `air.toml` in this repo,
  so **do not** run a formatter.
- **Comment decision points** — why this approach over the plausible
  alternative — not what the code obviously does.
- `set.seed()` is **not** sufficient for reproducible fits. Pass `seed =` to
  `bnec()`/`brm()`; it reaches Stan's sampler, `set.seed()` does not.
- Requires `brms >= 2.23.0`. Earlier versions mis-generate `beta_binomial`.

## When to stop rather than guess

Stop the current issue, write what you found in the PR body (or a
`notes/blocked_<n>.md` if there is nothing to push), and move to the next issue
if any of these occur:

- the fix requires a **user-visible behaviour change** not sanctioned in the
  queue entry;
- it requires a **new dependency**;
- it requires **deciding what the correct statistical behaviour is** — as
  opposed to implementing behaviour the queue entry already specifies;
- the issue turns out to be a **duplicate**, or already fixed on `dev`;
- more than roughly **90 minutes** of wall clock goes by with no progress.

Do not open a PR that guesses at a modelling decision. A clear write-up of the
blocker is worth more than a speculative implementation.

## Scope boundaries

**Anything touching `ecx()`, `nsec()`, `ecnsec()` or `zero_crossings()` is out
of scope.** That code is migrating to
[open-AIMS/toxval](https://github.com/open-AIMS/toxval), which will become a
dependency of `bayesnec`. Issues #39, #44, #166, #195 and #196 belong there and
must not be worked here. If a queued issue turns out to require changing those
functions, stop and report it — that is a finding, and it changes the migration
plan.

**#193 (example7 rewrite) is being done by another session.** Do not touch
`vignettes/example7*`.

## Verification you can rely on

```r
devtools::load_all(".")
devtools::test()                       # full suite
testthat::test_file("tests/testthat/test-<file>.R")
```

Fitting anything real is slow. Prefer `manec_example` / `nec_data` and the
smallest `chains`/`iter` that exercises the code path. Long fits are rarely
needed to demonstrate a post-processing fix.
