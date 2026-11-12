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

`R-CMD-check.yaml` does not set it in its own `env:` block, and the only
`NOT_CRAN` under `.github/` is in `precompile-vignettes.yaml`, so the workflow
file reads as though CI skips the fitting tests. It does not:
`r-lib/actions/check-r-package` sets `NOT_CRAN: true` itself. Read the run's
testthat line rather than the workflow to settle it --- on `2026-09-22`,
`ubuntu-latest (release)` reported `FAIL 0 | WARN 19 | SKIP 4 | PASS 5061` with
`checking tests` taking 25 minutes. So the four-platform matrix is a full-suite
run and is better evidence than one local one.

## Green checks that mean less than they appear

Both were found in the #393 review.

### A deadlocked worker against a slow fit

Two consecutive parallel runs stalled on the same file for over an hour. The worker had accumulated **zero**
seconds of CPU time while blocked in `poll_schedule_timeout`, which is a
deadlock. Check `ps -o etime,times,wchan` on the worker before waiting or
timing; a run that has not finished is not necessarily a run that is working.

### The limits of `tools::checkRd()`

It validates; only rendering reveals. A man page silently dropped the second
half of four sentences and `checkRd()` passed it, as did four
`R CMD check` platforms. The cause was `\\%` in the generated Rd, which is valid
Rd: the backslash renders and the bare `%` after it opens a comment to end of
line. It comes from writing `\%` in a roxygen block under
`Roxygen: list(markdown = TRUE)`, where `%` is not an escapable character, so
roxygen2 emits a literal backslash and then escapes the percent itself. Write a
bare `%` in the roxygen. The detector is `git grep '\\\\%' -- man/` and not a
grep over `R/`, because the same source habit is harmless inside `@noRd`, where
no Rd is generated. After #393, `man/check_fit.Rd` and `man/nassarius.Rd` still
have it. Render a man page and read it before trusting a documentation change:
`Rscript -e 'tools::Rd2txt("man/<topic>.Rd", out = stdout())'`.
