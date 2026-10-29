# Specification for what remains of the backlog run

Companion to `backlog-run-human.md`, which states each step in a sentence and
refers here. Read `notes/implementation/00_protocol.md` for how to work, and D41
to D43 in `notes/implementation/03_decisions.md` for the rulings this document
implements.

Rewritten 2026-10-29 against `predev` at `a3368faa`, to describe only the work
that remains. The specification of the items the two runs fixed, with the
findings and the options behind each ruling, is in the history of this file:
`git show efce694a:notes/tasks/backlog-run-claude.md` prints it, and its §5 holds
the questions that `03_decisions.md` D19 to D31 answer. Each issue also holds a
comment headed "Findings on `predev`" and one headed "Decision".

## 1. The state of `predev`

Every issue the two runs covered is closed: items 1 to 16 and #418 of the first
run, and S1 to S18 of the second. Each closing comment names the pull request and
its merge commit, and each fix is an ancestor of `predev` at `a3368faa`. S19, the
second precompile, is §2 here. The only open issue is #255. `DESCRIPTION` `Version` is 2.1.3.44, and
`NEWS.md` opens with `# bayesnec 2.2.0`.

The last full precompile was HPC job 914176, nine array tasks submitted from
`predev` at `a806fb7c` on 2026-09-27, covering every vignette except `example8`.
Its output merged as PR #454 (`53f04f20`), in the commits `1fb714ab` and
`6fafbf38`. `example2b` and `example5` rendered unchanged and were not
recommitted. `notes/implementation/05_run_log.md` records the job.

Two pull requests merged after that render change the numbers a vignette prints.
Both reached `predev` through PR #526 (`276a43d9`) on 2026-10-29.

- PR #519 (#299, D41): where `crf()` transforms the predictor inline, `nec()`,
  `nsec()` and `ecx()` return the recorded scale by default, by the exact inverse
  where one is known and by numeric inversion on the prediction grid otherwise.
  `example1` fits `crf(log(raw_x), ...)` and `example9` fits
  `crf(log(concentration), ...)` and `crf(log(conc), ...)`.
- PR #523 (#485, D31 and D43): `ecxflat` joins the `all`, `ecx` and `decline`
  sets (`data-raw/sysdata.R`). `example2`, `example4` and `example9` fit the
  `decline` set; the `decline` fits in `example7` are not evaluated.

The prose was corrected for both changes before the render, in `cbe2f2ef`
(#299) and `0c4ae43d` (#485), so each `.Rmd.orig` it edited is newer than its
`.Rmd`. At `a3368faa`, the last commit to each file is:

| vignette | rendered `.Rmd` | `.Rmd.orig` |
|---|---|---|
| `example1` | `1fb714ab`, 2026-09-27 | `cbe2f2ef`, 2026-10-28 |
| `example2` | `1fb714ab`, 2026-09-27 | `0c4ae43d`, 2026-10-28 |
| `example2b` | `3179f9cb`, 2026-09-16 | `0c4ae43d`, 2026-10-28 |
| `example3` | `1fb714ab`, 2026-09-27 | `0c4ae43d`, 2026-10-28 |
| `example4` | `1fb714ab`, 2026-09-27 | `110740fe`, 2026-09-16 |
| `example5` | `a4952415`, 2026-09-14 | `a4952415`, 2026-09-14 |
| `example6` | `6fafbf38`, 2026-09-27 | `0c4ae43d`, 2026-10-28 |
| `example7` | `6fafbf38`, 2026-09-27 | `0c4ae43d`, 2026-10-28 |
| `example9` | `6fafbf38`, 2026-09-27 | `0c4ae43d`, 2026-10-28 |

`example4` is not newer in its source, but it fits the `decline` set, so its
printed numbers change as well. `example5` fits `nec4param` alone and is expected
to render unchanged.

## 2. The precompile

D43 requires the vignettes to be precompiled again after D41 and the `ecxflat`
change. `hpc/README.md` gives the commands and the settings.

1. Create a worktree at the tip of `predev`, clean. Copy `hpc/local.conf` into it
   from the main checkout. The file is git-ignored and is never committed.
2. Submit every vignette except `example8`:
   `./hpc/precompile-hpc.sh --no-wait example1 example2b example2 example3 example4 example5 example6 example7 example9`.
   `precompile-hpc.sh` sends the working tree, not the commit, so the tree must
   match `origin/predev`.
3. Record the job identifier and the `predev` commit in `05_run_log.md`.
4. When the job has finished, collect each vignette with
   `./hpc/precompile-hpc.sh --fetch <names>`.
5. Check every rendered `.Rmd` for a home directory, a library path, a memory
   address or compiler output, as `bayesnec/CLAUDE.md` requires.
6. Read each number the prose of a changed vignette quotes against the new
   output. The prose quotes numbers as literals, and a refit can change a
   conclusion as well as a value: PR #465 found 16 mismatches in `example8`
   after its refit. Where they differ, correct the `.Rmd.orig` and the `.Rmd`
   together, so that no second render is needed.
7. Open one pull request into `predev` with the rendered `.Rmd` files, their
   figures and any prose corrections. Its body names the job, the `predev` commit
   it rendered, and every pull request merged into `predev` after that commit.

Done when the pull request has merged, and every rendered `.Rmd` other than
`example8.Rmd` was rendered from a `predev` commit that contains `276a43d9`.

## 3. `example8` and its fit store

Not part of this plan. PR #465, open and worked on by another session, renders
`example8` against the `grouping-structures` fit store, refitted under
`bayesnec` 2.1.3.41 (HPC job 915155, 2026-09-30). D43 requires the store to be
refitted after the `ecxflat` change, because the store holds one fit per
equation of each model set, and 2.1.3.41 predates PR #523.
`notes/example8_fit_store.md` describes the store.

## 4. The merge into `dev` and the release

RF reviews `predev` as a whole, after §2 and PR #465 have merged. It reaches
`dev` as one pull request. RF renames the `# bayesnec 2.2.0` heading of
`NEWS.md` and sets `DESCRIPTION` `Version` for the release.

Before the release goes to CRAN:

- `R CMD check --as-cran` passes on the platforms `R-CMD-check.yaml` covers.
- The deployed site shows the release at its root and `dev` under `/dev/`.
  `_pkgdown.yml` sets `development: mode: auto`, which keys the destination on
  the version in `DESCRIPTION`. Read the deployed site rather than the
  configuration.

## 5. Not in this plan

#255, the toxval migration of `ecx()`, `nsec()`, `ecnsec()` and
`zero_crossings()`, is excluded by RF and holds its own inventory. D11 stands: a
fix to an estimator is made in `bayesnec` and recorded on the matching toxval
issue, so that the migration relocates the corrected file.

## 6. A correction to `CLAUDE.md` for RF

The section on branches in `bayesnec/CLAUDE.md` names `dev` and `master` and not
`predev`. The two other corrections this plan listed for RF, on `dispersion()`
on a model average and on the identity link, are already made on `predev`.
