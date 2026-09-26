# The `example8` fit store

`example8` fits 189 models on `predev`, more than a precompile job's walltime
allows. The compendium at `open-AIMS/grouping-structures` fits them as cluster
array tasks and writes the assembled fits to a store. `vignettes/precompile.R`
reads that store through `vignettes/fit_store.R` when `BAYESNEC_FIT_STORE`
names it.
`hpc/README.md`, under *Fits computed elsewhere*, gives the design; the
compendium's `README.md` and `CLAUDE.md` give its own. This note records the
version check, the refit the store needs, and the steps to refit, fetch and use
it. The decision is on #413.

## The version check

A stored fit is keyed on its call and its data, not on the code that fitted it.
A store fitted before a change to what `bayesnec` fits therefore resolves every
key and loads without an error. `fit_store_install()` in `vignettes/fit_store.R`
now reads the `bayesnec:` line of the store's `MANIFEST` and stops where it
records a version older than `FIT_STORE_MIN_BAYESNEC`, 2.1.3.40. The message
names both versions and the refit below, including the renaming in its step 2.

The minimum is raised by hand, and only by a change to what `example8` fits: a
default prior, initial value or sampler setting that its fits use. A change to
one of its calls needs no raise, because it changes the key and the render stops
at that call. A version bump for anything else leaves the minimum alone, which
is why the check is a minimum rather than equality with the installed version.

A store with no `MANIFEST`, or one without a `bayesnec:` line, is installed with
a message saying it was not checked. The compendium's key check
(`analysis/check_keys.R`) installs the shim against a store of empty files with
no `MANIFEST`, and a refusal would break that check.

The version in `MANIFEST` certifies the last assembly, not every file in the
store directory. Three cases arise where a refit writes into the previous run's
directories, each under a key the new vignette still asks for:

- A unit from the earlier run is reused, because the units array skips a unit
  whose file exists. `analysis/assemble_store.R` stops on it, since its version
  differs from the manifest's, and stops before it rewrites `MANIFEST`. The
  store keeps the old version and the check refuses it.
- A joint refit from the earlier run is kept, because `analysis/run_joint.R`
  skips a refit whose file exists. The assembly has rewritten `MANIFEST` with
  the new version, so the old refit passes the check.
- An incomplete assembly leaves the earlier run's set in place.
  `analysis/assemble_store.R` writes `MANIFEST` with the new version (line 158)
  before it exits on a call that is short a unit (line 167). That call's store
  file is not rewritten, so its older fit passes the check.

Renaming `units/` and `store/` on the cluster before a refit, step 2 below,
prevents all three.

`vignettes/fit_store.R` is generated in the compendium from `R/keys.R` and
`shim/fit_store_body.R`. The check was added to the copy in `bayesnec` first.
`shim/fit_store_body.R` has to take it before the file is regenerated, or the
regeneration drops it.

## The refit after PR #409

PR #409 changed the default priors and did not change `Version`, which was
2.1.3.39 before and after it. The local store at
`/mnt/c/Rworking/grouping-structures/store` (1.2 GB, fifteen fits, measured
2026-09-26) records `bayesnec: 2.1.3.39`, assembled 2026-09-18, and the
compendium's `hpc/bayesnec.lock` pins `dd9e2944`, a commit on PR #402's branch
from before PR #409. Its fits are not what the current code fits, and the check
above refuses it. It has to be refitted once PR #402 has merged into `predev`.
No other item of the backlog run changes what `example8` fits
(`notes/tasks/backlog-run-claude.md` §4, *The precompile and the store refit*).

## Refitting a store

Everything runs from a checkout of `grouping-structures` beside this one, with
the cluster reachable over SSH (the AIMS VPN).

1. Copy `hpc/local.conf.example` to `hpc/local.conf`, which is git-ignored, and
   set `HOST`. `BAYESNEC_REPO` defaults to `../bayesnec`.

2. After a change of `bayesnec` version, rename the previous run's output
   directories on the cluster before deploying. Neither key includes the
   version, and both the units array and the joint job skip a file that exists;
   *The version check* above gives the three cases this prevents. Renaming
   rather than deleting keeps the old store, which is what rendered output
   already published came from:

   ```sh
   ssh <host> 'cd /export/scratch/$USER/grouping-structures &&
     mv units units-2.1.3.39 && mv store store-2.1.3.39'
   ```

   `hpc/job-common.sh` describes the alternative, pointing `GRP_UNITS` and
   `GRP_STORE` at new directories, but `deploy.sh` does not pass them through
   and `fetch-store.sh` reads only `store/`.

3. Deploy at the `bayesnec` commit to be rendered:

   ```sh
   ./hpc/deploy.sh --ref <bayesnec commit>
   ```

   This exports `bayesnec` at that commit, records it in `hpc/bayesnec.lock`,
   copies both trees and the container if the cluster lacks it, and submits four
   chained jobs: install and key check, the units array, the assembly, and the
   joint refit. Commit `hpc/bayesnec.lock` in `grouping-structures`.

4. Check the array width. `submit.sh` takes it from the `manifest.csv` already
   on the cluster, which is the previous run's, before the install job rebuilds
   the manifest. The install log ends with `ready: <n> units`. Where that
   exceeds the width `submit.sh` printed, the assembly names each call that is
   short a unit and does not write it, and the joint job does not start. Once
   the chain has ended, submit it again with
   `ssh <host> 'cd /export/scratch/$USER/grouping-structures &&
   ./hpc/submit.sh'`: it reads the rebuilt `manifest.csv`, and the array skips
   the units already fitted. The count does change between runs: the local
   `manifest.csv`, fetched after the last run on PR #402's branch, has 226 rows,
   where the first run had 189.

5. `squeue -u $USER` on the cluster shows progress.

## Fetching and using a store

```sh
./hpc/fetch-store.sh                 # in grouping-structures
export BAYESNEC_FIT_STORE=$PWD/store
```

The render then reads the store instead of sampling. From a `bayesnec` checkout
at the commit the store was fitted at:

```sh
BAYESNEC_FIT_STORE=/path/to/grouping-structures/store \
  Rscript vignettes/precompile.R example8
```

`precompile.R` refuses the variable where more than one vignette is named. The
render writes `vignettes/example8.Rmd` and its figures into the working tree;
for a measurement rather than a release, discard them afterwards with
`git checkout -- vignettes/`. On the cluster the same render is
`BAYESNEC_FIT_STORE=/export/scratch/$USER/grouping-structures/store
./hpc/precompile-hpc.sh example8`, which leaves the store on scratch.

To read single estimates without a render, source the shim in an R session
started at the repository root and run the vignette's chunks:

```r
Sys.setenv(BAYESNEC_FIT_STORE = "/path/to/grouping-structures/store")
source("vignettes/fit_store.R")
fit_store_install()   # refuses a store older than the minimum
```

A call whose key is not in the store stops with the key, the normalised call
and the keys the store holds.
