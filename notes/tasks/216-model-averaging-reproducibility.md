# Task — bayesnec #216: model-averaged output is not reproducible

**Point a Claude Code session at this file from the `bayesnec` repo root.**
Read `notes/implementation/00_protocol.md` first for the working rules, then
this. Self-contained otherwise.

Issue: https://github.com/open-AIMS/bayesnec/issues/216

---

## The bug

Anything going through the model-averaged posterior of a `bayesmanecfit`
returns a different answer on every call:

```r
library(bayesnec)
nd <- bnec_newdata(manec_example, resolution = 20)
predict(manec_example, newdata = nd)[1, "Estimate"]   #> 2.183761
predict(manec_example, newdata = nd)[1, "Estimate"]   #> 2.229868
```

No seed is set by the user and none internally. It propagates to `plot()`,
`autoplot()`, `summary()`, `ecx()` and `nsec()`. Over six replicate calls the
`nsec` lower bound spanned 0.702–0.954 while the median moved by ~0.5%: the
instability lands almost entirely on the interval, which is the end used to set
a protective concentration.

## Cause

Model averaging resamples each component's draws in proportion to its stacking
weight, with an unseeded `sample()`, **independently at each site**:

| | |
|---|---|
| `R/helpers.R:90` | `w_nec_calc()` — averaged no-effect posterior |
| `R/helpers.R:105` | `w_post_pred_calc()` — averaged predicted curve |
| `R/helpers.R:112` | `w_pred_list_calc()` |
| `R/ecx.R:294` | `sample_ecx()` |
| `R/nsec.R:233-234` | `sample_nsec()` — **two** separate `sample()` calls |

The method is not wrong — this is `weighted_samples`, as used and recommended in
`ssdtools`. What is wrong is that the randomness is redrawn, independently, at
every site and on every call.

## Scope

**In scope — fix here:** the three sites in `R/helpers.R`. That is
model-averaging code and it **stays** in `bayesnec`.

**Out of scope — do not touch:** `R/ecx.R` and `R/nsec.R`. Those files are
migrating to `toxval` and `sample_ecx()` / `sample_nsec()` are expected to be
deleted rather than fixed. If the `helpers.R` fix makes their behaviour
inconsistent, **say so in the PR** rather than editing them.

## Suggested fix

Draw the component index **once** and reuse it, so realisation *i* means
"component `m[i]`, iteration `j[i]`" for every quantity:

```r
idx <- sample(seq_along(models), n, replace = TRUE, prob = weights)   # once
```

`sample_size` and `mod_stats$wi` are already stored on the `bayesmanecfit`, so
the cleanest option is probably to **store the realised index on the object when
it is built** — then every later call reuses it and no `seed` argument is
needed. A user-facing `seed` would also work but leaves the default
irreproducible.

This is not a change of averaging method. Only where the randomness is drawn.

## Hazards

- **Numbers will change**, and there is no "before" to match — the current
  values are not reproducible. Expect to rewrite any test that pins a
  model-averaged estimate. Say in the PR which ones and why.
- Two quantities that should be paired currently are not (`nsec` and its
  `ecnsec` come from different `sample()` calls). Fixing the index fixes that
  too. Do not present it as a separate change.
- Check whether `predict()`, `plot()`, `autoplot()` and `summary()` all become
  deterministic. If any is still not, that is a finding worth reporting.

## Working rules for this repo

- **Work in your own worktree** — `git worktree add /mnt/c/Rworking/bayesnec-216 dev`
  — not the main checkout, which other sessions share.
- Branch `issue-216-<slug>` off `dev`, PR `--base dev`. Never branch from
  another issue's branch.
- **No `air`.** `bayesnec` has no `air.toml` and formatting is forbidden.
- **Do not touch `vignettes/example7*`** — #193 is being worked in another
  session.
- `devtools::document()` if roxygen changed; commit the regenerated `man/`.
- A `NEWS.md` entry.

## Done when

Repeated identical calls to `predict()`, `ecx()` and `nsec()` on
`manec_example` return identical values; a test asserts it; the full suite
passes; `R CMD check` is clean.
