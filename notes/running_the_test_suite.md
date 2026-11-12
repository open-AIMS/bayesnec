# Running the test suite

Stan compilation accounts for most of the full suite's duration. An earlier
inventory counted 53 `bnec()` calls across 25 of 57 test files; the suite has
since grown.

## Routine and full tests

The routine `devtools::test()` and `R CMD check` runs skip 30 files focused on
fitting at file entry. The other 51 test files still run their unit checks. Two
individual integration tests in `test-component-args.R` and
`test-zero-inflated-counts.R` also require the full suite because they fit
models. Set `BAYESNEC_FULL_TESTS=true` to run all tests locally. The full suite
is a release and targeted regression check; it is no longer part of every pull
request. The guards apply to `devtools::test()` as well as `R CMD check`.

Guard new tests that compile or sample a model with `skip_unless_full_suite()`.
Keep tests based on stored fits or mocked fitting calls in the routine suite.
Run the full suite before a release, especially on Windows: routine CI no longer
exercises Stan model compilation during tests.

On 2026-11-12, the routine suite took 13 minutes 17 seconds on WSL with two
workers: 4,444 passes, 14 warnings, 32 skips and no failures. This local time
does not estimate Windows runner speed.

On 2026-11-12, a three-job Windows split on [PR #595](https://github.com/open-AIMS/bayesnec/pull/595)
completed two 26-minute test jobs, while the main Windows package check was
still running when it was cancelled after 56 minutes. The Linux and macOS full
checks were also still running. Splitting files across jobs did not meet the
30-minute development target.

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

For a full local run against an installed build:

```bash
R CMD INSTALL --no-docs --library=/tmp/lib .

BAYESNEC_FULL_TESTS=true NOT_CRAN=true TESTTHAT_PARALLEL=TRUE TESTTHAT_CPUS=8 Rscript -e '
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

`r-lib/actions/check-r-package` sets `NOT_CRAN: true` itself. Read the run's
testthat line rather than the workflow to settle it --- on `2026-09-22`,
`ubuntu-latest (release)` reported `FAIL 0 | WARN 19 | SKIP 4 | PASS 5061` with
`checking tests` taking 25 minutes. At that date the four-platform matrix ran
the full suite in each job. The routine suite now skips the files guarded by
`skip_unless_full_suite()` even though `NOT_CRAN=true` remains set.

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
