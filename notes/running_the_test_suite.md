# Running the test suite

The suite fits models: 53 `bnec()` calls across 25 of the 57 test files, each
compiling and sampling a Stan program. That is what makes it slow, and it is the
only thing that does.

## In parallel, which is the default

`Config/testthat/parallel: true` in `DESCRIPTION` makes `testthat` run each test
**file** in its own subprocess. `devtools::test()` and `R CMD check` pick that up
with no further setup.

Measured on 2026-09-07, 22 cores, 8 workers: **21 minutes against 55 serial**, on
645 tests. The speed-up is 2.6x rather than the 8x the worker count suggests,
because parallelism is by file and the wall clock floors at the slowest single
file. Further gains would come from splitting the heaviest fitting files, not
from raising the worker count.

## Running it by hand, and the four traps

A parallel worker is a **fresh R process**. It does not inherit the parent's
loaded package, its library paths, or its attached namespaces. Each of the
following fails in a way that does not name the cause:

1. **`devtools::load_all()` is invisible to workers.** They start clean and call
   `library(<pkg>)`, so the package must be *installed*. The failure is
   `testthat subprocess failed to start ... attempt to use zero-length variable
   name`.

2. **`R_LIBS_USER` replaces the library path, it does not add to it.** Setting it
   to a scratch directory hides every other installed package; here that surfaces
   as `package 'brms' 2.22.0 was found, but >= 2.23.0 is required`. Prepend to
   `.libPaths()` inside the script instead.

3. **`test_dir()` does not infer the package name** when loading an installed
   build. Without `package =`, the workers call `library("")` and stop with
   `invalid package name`.

4. **`setup.R` runs in every worker**, where `utils` and `stats` are not
   attached. `data()` and `runif()` are not visible and the setup fails before
   any test runs. Everything in `setup.R` outside base is namespaced for this
   reason; keep it that way.

The incantation that works:

```bash
R CMD INSTALL --no-docs --library=/tmp/lib .

NOT_CRAN=true TESTTHAT_PARALLEL=TRUE TESTTHAT_CPUS=8 Rscript -e '
  .libPaths(c("/tmp/lib", .libPaths()))
  library(testthat); library(bayesnec)
  test_dir("tests/testthat", package = "bayesnec", load_package = "installed")'
```

## Choosing the worker count

**Memory-bound, not core-bound.** A worker with `brms` loaded holds about 1.4 GB,
so 8 workers is roughly 11 GB. `TESTTHAT_CPUS` sets it; `getOption("Ncpus")`
takes precedence if set. On a machine shared with a running analysis, pick a
number that leaves it room --- the suite is not the only thing that matters.

## `NOT_CRAN`

**Without `NOT_CRAN=true` the suite reports zero failures while skipping nearly
every assertion**, because most files open with `skip_on_cran()`. A clean run
that finishes suspiciously quickly is that, not success.
