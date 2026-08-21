# Stack run instructions

The literal procedure for the unattended run. Read `00_protocol.md` and
`01_work_queue.md` first; this file says how to execute them, not what to build.

**Written 2026-08-21 for a run starting that evening, reviewed by RF on Monday.**

---

## Before you start

```bash
git -C /mnt/c/Rworking/bayesnec fetch origin dev
# NOT `worktree add <path> dev` -- dev is checked out in RF's main checkout and
# git refuses. Create the worktree on the stack's first branch directly.
git -C /mnt/c/Rworking/bayesnec worktree add -b issue-<n1>-<slug> \
    /mnt/c/Rworking/bayesnec-stack dev
cd /mnt/c/Rworking/bayesnec-stack
uptime; nproc          # confirm the machine budget still holds
```

One worktree for the whole run. `/mnt/c/Rworking/bayesnec` is RF's checkout —
do not work in it. Do not touch any other `bayesnec-*` worktree.

Create `notes/implementation/05_run_log.md` in that worktree and append to it as
you go: one short block per item — issue, branch, PR URL, tier, outcome, and
anything RF needs to decide. It is the first thing RF reads on Monday, so keep
it current rather than writing it at the end.

---

## The loop

For each item in `01_work_queue.md`, in order:

1. **Read the issue AND its comments.** `gh issue view <n> --json body,comments`.
   Not `--json body`. The first version of the queue was wrong because of exactly
   this; several of these issues carry their real specification in a comment.
2. Cut the branch from the branch you are already on — **not from `dev`**, except
   for item 1.
3. Implement to the *Done when* in the queue entry.
4. Tests: main behaviour plus at least one edge case, in
   `tests/testthat/test-<file>.R`.
5. `devtools::document()` if roxygen changed; commit the regenerated `man/`.
6. `NEWS.md` entry **under the correct release heading** — see the versioning
   table in `00_protocol.md`. Bump `DESCRIPTION` `Version` by one in the fourth
   component.
7. Run the affected test files, then `devtools::test()` **once**.
8. Push, open the PR with `--base <the previous branch>`, wait for CI, fix
   anything red **before** leaving it for review.
9. Append to `05_run_log.md`. Move to the next item.

---

## Machine discipline

A simulation study owns most of this machine — see the budget section in
`00_protocol.md`. In this run that means:

- `chains = 2, cores = 1` everywhere. Never `cores = 4`.
- One R process at a time from this session. Do not background a second.
- `devtools::test()` once per item, not per iteration; use
  `testthat::test_file()` while iterating.
- The full suite takes ~40 min on the Linux filesystem and considerably longer
  under `/mnt/c`. Budget for it rather than polling it every 30 seconds.
- Kill by PID. A `pkill -f` pattern that matches your own shell has killed the
  invoking bash repeatedly in this project.

---

## CI is the real verification

`R-CMD-check.yaml` runs the test suite with only 3 skips, so a green CI run is
stronger evidence than a local pass — and the before/after comparison across a
PR is stronger still. Record the counts in the PR body:

```
before: [ FAIL 0 | WARN 12 | SKIP 3 | PASS 1443 ]
after:  [ FAIL 0 | WARN 10 | SKIP 3 | PASS 1449 ]
```

Pull them with:

```bash
gh run view <run-id> --repo open-AIMS/bayesnec --log \
  | grep -oE "\[ FAIL [0-9]+ \| WARN [0-9]+ \| SKIP [0-9]+ \| PASS [0-9]+ \]" | sort -u
```

An increase in `WARN` is a finding, not noise. Explain it or fix it.

---

## When an item stalls

The stop conditions in `00_protocol.md` apply unchanged. What is different in a
stack is what you do next:

- **Do not branch from a broken branch.** Go back to the last good branch and cut
  the next item from there.
- Push what you have to `issue-<n>-<slug>-wip` so the work is not lost, but do
  **not** open a PR for it.
- Write the blocker into `05_run_log.md` under a `BLOCKED` heading, with enough
  detail that RF can act on it without re-deriving anything.
- Move on. A clean run of five PRs plus one honest blocker note is a better
  Monday than eight PRs where three are guesses.

Specific to this run: **items 8 and 9 (#33 and the grouping vignette) are the two
that might not land.** They are last precisely so that this is survivable. If
either stalls, everything from items 1–7 is still a clean stack.

---

## Things that are easy to get wrong here

- **`.github/` is open for #215 only.** Nothing else in the stack may touch it.
- **Never run `vignettes/precompile.R`.** Author `.Rmd.orig`; the rendered `.Rmd`
  stays stale until #190. If a change alters vignette output, say so in the PR.
- **Do not touch `vignettes/example7*`** — #193 is another session's.
- **`ecx()` / `nsec()` / `ecnsec()` / `zero_crossings()` are out of scope.** #209
  needs a one-line roxygen touch in `R/ecx.R` and `R/nsec.R` to list two new
  families; that is allowed and should be flagged in the PR. Anything more than
  that is a stop-and-report.
- **`set.seed()` does not reproduce a fit.** Pass `seed =` to `bnec()`/`brm()`.
- **A dependency needs RF's say-so**, but it is a stop-and-ask, not a
  prohibition — RF is happy to add one that earns its place (#148, 2026-08-21).
  #148's decisions already rule out `bayesplot` and `DHARMa`; do not reopen them.

---

## What RF sees on Monday

- A stack of PRs, bottom-to-top in queue order, each targeting the one below it.
  GitHub retargets to `dev` as each merges, so they can be merged straight down.
- `05_run_log.md`, listing what landed, what did not, and every decision waiting
  on RF.
- Two release headings in `NEWS.md` (three if #33 landed), so the release
  boundaries are visible in the diff.
- Three issues closed with evidence and no PR: #79, #212, #166.

Decisions that will be waiting, known in advance:

1. **#207 changes numbers.** Fits that previously ran on flat priors will now run
   on bayesnec defaults. Intended, but not silent.
2. **#136 makes unrecognised aterms an error.** A user-visible breaking change,
   deliberately flagged in its own `NEWS.md` bullet.
3. **#190 may need to run twice** if 2.1.4 ships to CRAN before 2.2.0. See the
   #190 section of the queue.
4. **#148 decision (d)**, the combined hurdle check, if it was not reached.

---

## Running this as a loop

RF authorised an unattended self-paced loop on 2026-08-21, with **skip-and-
continue** on a stall. Each wake:

1. Re-reads `01_work_queue.md` and `05_run_log.md` to find the next unfinished
   item. The run log is the source of truth for where the stack has got to —
   never infer it from the branch list alone.
2. Does exactly one item, end to end, to the *Done when* in the queue entry.
3. Opens its PR against the previous branch and appends to `05_run_log.md`.
4. Schedules the next wake.

**On a stall: skip, do not halt.** Push the partial work to
`issue-<n>-<slug>-wip` with no PR, write a `BLOCKED` entry in the run log with
enough detail for RF to act on without re-deriving anything, and cut the next
item **from the last good branch** — never from the stalled one. Then continue.

**Pacing.** Most items involve a full `devtools::test()` run, which is ~40 min on
the Linux filesystem and longer under `/mnt/c`. Do not poll it in a tight loop.
Wake on the long side and let CI and the test suite run between wakes.

**Do not start an item you cannot finish** in the remaining budget — better to
end a wake having logged the state cleanly than to leave a half-built branch
load-bearing for the next one.

## Operational note: never launch a long job after a wait in the same task

**Corrected after more evidence.** The first version of this note said
backgrounding from inside a backgrounded task never works. That is too broad —
`git commit && nohup Rscript -e 'devtools::test()' &` in a backgrounded task
launched fine on #136.

The failure on #207 was narrower: the task ran a **long `until` loop first**,
then launched. Something about that sequence — most likely the harness reaping
the process group once the long-lived task finally exits — killed the job. It
reported success and left no log, which is the dangerous part: a launch that
silently does not happen looks exactly like one that did.

So the rule is about *sequence*, not nesting:

- **Never** put a launch after a wait in the same task.
- **Always** verify a launch actually happened — check the log file exists and
  the process is alive — before reporting it as running. A missing log is the
  tell.

Waiting and launching are two calls.

## Checking whether a suite is still alive

The R process for `devtools::test()` has this command line:

    /usr/lib/R/bin/exec/R --no-echo --no-restore -e devtools::test()

Match on `[d]evtools::test`, not on a flag order. A pattern like
`exec/R --no-echo -e devtools` looks right and matches nothing, because
`--no-restore` sits between the two — which reads as "the suite died" when it is
running perfectly well.

Cross-check with the log size before concluding anything: a complete run is
~34.7 KB. A log that is short *and* growing is a slow run, not a dead one.

## Check Rd links before pushing roxygen changes

`devtools::test()` never builds the Rd files, so a broken `\link{}` target is
invisible locally and turns **every** CI platform red with
`checking Rd cross-references ... WARNING`. It cost two round trips in this run:

- `1.011 [0.71, 1.44]` became `\link{0.71, 1.44}` --- square brackets around a
  numeric interval are markdown link syntax to roxygen. Spell intervals out.
- `\link{set_distribution}` --- **not exported**, so the link does not resolve.
  The same mistake was caught earlier with `check_normalisation` and then
  repeated, which is why there is now a script rather than a note.

Run `Rscript notes/scripts/check_rd_links.R` after `document()` and before
pushing. It exits non-zero and names the file and target.

Not every internal helper is exported; `\link{}` only works for exported
functions, aliases in `man/`, and base packages. For anything else use
`\code{}` without the link.
