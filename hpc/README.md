# Precompiling the vignettes on the AIMS HPC

The `bayesnec` vignettes are precompiled: `vignettes/precompile.R` knits each
`.Rmd.orig` to a `.Rmd`, and it is the `.Rmd` that is committed and shipped. A
full precompile is days of wall clock on a workstation, and #190 requires one
across every vignette before the next release. This directory moves that work
to the cluster.

The image holds the toolchain — R, cmdstan, `brms`, and the packages the
vignettes load — and does not hold `bayesnec`. The job installs `bayesnec` from
the checked-out source into a job-local library at start-up. One image therefore
precompiles a vignette from any branch, and the statement in `example7` that its
case studies are computed at build time so that they track the version of the
package they ship with remains true whichever branch is being built.

## The command

```sh
./hpc/build.sh                          # once, and again only when a dependency changes
./hpc/precompile-hpc.sh example7        # deploy, submit, wait, collect
```

`precompile-hpc.sh` copies the working tree — not the commit — to
`/export/scratch/$USER/bayesnec-precompile`, copies the image if the cluster does
not already hold that exact image, submits, waits, and copies back the vignette's
`.Rmd` and the figures the run wrote. Nothing else is copied back. What was sent
is recorded in `PROVENANCE` in the job directory.

Named vignettes are optional; with none it precompiles all of them. `--no-wait`
returns the job id instead of blocking, and `--fetch` collects a run submitted
that way.

Three environment variables change where it works: `HOST` (default
`rfisher@hpc-l001.aims.gov.au`), `DEST` (a directory on the cluster, by default
under `/export/scratch` for the account in `HOST`), and `SIF`, the local path to
the image. `SIF` matters under WSL, where the repository is on a 9p mount: both
building the image there and reading it for the copy are several times slower
than on the Linux filesystem, so build it elsewhere and point `SIF` at it.
Whatever it is called locally, it is copied to the cluster under one fixed name.

`APPTAINER_TMPDIR` chooses where `build.sh` unpacks the base image. It needs
about 15 GB on a local Linux filesystem; `build.sh` refuses a tmpfs or a 9p
mount, because the extraction of tens of thousands of small files is what
decides how long the build takes.

## Building the image

Built on a workstation with `apptainer` and copied across. That is not a
preference. Verified on `hpc-l001` on 2026-09-10: the cluster provides
singularity 3.6.3, and

```
FATAL: could not use fakeroot: no mapping entry found in /etc/subuid for rfisher
```

so a definition file with a `%post` section cannot be built there by an
unprivileged account. `singularity exec docker://…` does work, so an image that
needs no `%post` could be pulled on the cluster; this one needs cmdstan compiled
into it.

The consequence is small under this design. The image changes when a dependency
changes, not when the package or the branch changes, so the copy is rare rather
than per-run, and `precompile-hpc.sh` skips it when the cluster already holds a
byte-identical image.

`build.sh` writes `hpc/image.lock` from a manifest generated inside the image,
and appends the SHA-256 of the `.sif` file.

## Image identity

Precompiling in a container makes the committed `.Rmd` files depend on the
image's R, cmdstan and package versions rather than on whichever workstation ran
them. That is an improvement in reproducibility, and it makes the image part of
what the output should record.

`hpc/image.lock` is that record, and it is committed. It states the base image
digest, the R and cmdstan versions, the repository the packages were resolved
from, every installed package version, and the SHA-256 of the image file. Before
anything is fitted, `run.precompile` compares the SHA-256 of the image it has
been given with the one in the lock file and refuses to run on a mismatch.
Rebuilding the image is therefore a change that has to be made deliberately, on a
pull request, and cannot alter committed vignette output as a side effect.

`./hpc/build.sh --check` performs the same comparison locally.

## The Stan program cache

Every vignette is precompiled with the `cmdstanr` backend. `cmdstanr` writes each
Stan program to a file named after a hash of its source and keeps the compiled
executable beside it, so a program is reused only where its Stan source is
identical, and is recompiled otherwise. `rstan`, the `brms` default, has no
equivalent cache that survives a session.

Measured on `example7` over 2026-09-09 and 2026-09-10 — six data-handling
conventions across four datasets, model-averaged over the declining candidate
set, 320 individual model fits and 304 distinct Stan programs:

| run | Stan cache | result |
|---|---|---|
| first | cold, all 304 programs compiled | 309 min end to end |
| second | warm, nothing recompiled | 147 min of fitting; still running at 4 h 31 m |

Compilation is roughly half the elapsed time of a cold precompile and is
avoidable between runs. The cache lives at
`/export/scratch/$USER/bayesnec-stan-cache`, outside any job directory, and is
shared between vignettes, between runs and between branches. `BAYESNEC_STAN_CACHE`
overrides the location.

The second run also showed a cost that is not fitting: extracting `ecx()` and
`nsec()` from 24 model-averaged fits took over two hours on its own, because each
call solves across every posterior draw of thirteen equations. That is a property
of the vignette rather than of the build, but it is why the job is sized for a
day rather than for the sampling alone.

## One task at a time

The array is submitted with `%1`, so tasks run in sequence. `cmdstanr` does not
lock the compile cache: two tasks needing the same Stan program would write the
same `.stan` file and run `make` on the same executable path at the same time.
Vignettes share programs wherever a family and equation coincide, so this is not
hypothetical.

Serialising affects wall clock only on the first, cold run. Once the cache is
warm the tasks read it rather than write it, and the `%1` can be raised
deliberately.

## Failure and the exit status

`knitr` renders a chunk that errored as `#> Error…` and carries on, so a
precompile can report success while emitting a vignette that is a cascade of
errors. That happened to `example8` on 2026-08-24 and was not noticed until
someone read the output; `R CMD check` does not catch it either, because the
error text is only text in a rendered `.Rmd`.

`precompile.R` therefore greps what it knitted and stops. Under `Rscript` that
exits non-zero, `set -e` in `run.precompile` turns it into a failed job, and the
outputs are staged into `out/<vignette>/` only after that check has passed. A
failed run leaves nothing to collect, which is what `--fetch` reports.

## Changing a dependency

The image names the packages it installs. Adding a dependency to `bayesnec`
therefore requires a rebuild: `R CMD INSTALL` in the job fails, and says so.
Rebuild with `./hpc/build.sh`, commit the changed `hpc/image.lock`, and state on
the pull request that the image changed and what it changed to.

## Files

| file | what it is |
|---|---|
| `bayesnec-precompile.def` | the image definition; base pinned by digest |
| `build.sh` | builds the image locally and writes `image.lock` |
| `image.lock` | the image's identity, committed |
| `precompile-hpc.sh` | deploy, submit, wait, collect |
| `run.precompile` | the SLURM array task: install, knit, stage |
