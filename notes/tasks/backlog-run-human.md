# Plan for what remains of the backlog run

Companion specification: `backlog-run-claude.md`. Every decision below is stated
in full there, with the evidence.

Rewritten 2026-10-29 to describe only the work that remains. Both runs of the
backlog have finished. Every fix they made has merged into `predev`, and every
issue they covered is closed. The pull requests and the issues hold that record.

---

## Summary

Three steps remain before `predev` becomes the 3.0 release. The vignettes are
rendered again, RF reviews `predev` and it is merged into `dev`, and RF makes the
release. The only open issue, #255, is the toxval migration, which is outside
this plan.

## The precompile

This step renders every vignette except `example8` again, from the tip of
`predev`.

Two changes merged after the last render alter numbers that the vignettes print.
RF ruled that the vignettes are rendered again after them (D43; specification
§2).

- Where a formula transforms the predictor, as in `crf(log(conc), ...)`, the
  estimators `nec()`, `nsec()` and `ecx()` now return values on the recorded
  scale by default (#299). `example1` and `example9` fit such formulas.
- `ecxflat`, the constant equation, is now a member of the `all`, `ecx` and
  `decline` model sets (#485). A vignette that fits one of those sets now fits
  one more equation, and the model weights of the others change.

The sources of seven rendered vignettes have also been edited since that render,
so their text is out of date as well.

The render runs on the AIMS HPC. It needs RF's VPN connection to submit the job
and to collect the output. The output returns as one pull request into `predev`.
That pull request also corrects each number the prose quotes that the new render
no longer prints.

Done when that pull request has merged.

## `example8`

`example8` is outside this plan. PR #465 renders it, and another session is
working on it. That render used a fit store made before #485 changed the model
sets, and D43 requires the store to be refitted after that change
(specification §3).

## The merge into `dev`

This step brings `predev` into `dev` as one pull request, after RF has reviewed
`predev` as a whole. It waits for the precompile and for PR #465.

RF renames the `# bayesnec 2.2.0` heading in `NEWS.md` and sets `Version` in
`DESCRIPTION` for the release.

Done when that pull request has merged.

## The release

This step releases 3.0 to CRAN, and RF makes it. Two checks come first
(specification §4):

- `R CMD check --as-cran` passes on the platforms the CI workflow covers;
- the deployed site shows the release at its root and `dev` under `/dev/`.

Done when CRAN has accepted the release.

## A correction to `CLAUDE.md` for RF

The section on branches in `bayesnec/CLAUDE.md` does not mention `predev`. A
session does not edit `CLAUDE.md` unless RF asks.
