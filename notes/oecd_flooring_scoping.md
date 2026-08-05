# Scoping: OECD guidance on flooring negative growth endpoints

Handoff note for a separate conversation. The question is whether the claims
`vignette("example6")` makes about regulatory guidance are correct, and whether
its **Recommended handling** list should change as a result.

## The question

Growth endpoints that can go negative — specific growth rate, yield, increment
— are routinely floored to zero (or nudged to a small positive value) so a
zero-bounded family will accept them. The vignette argues against this and
asserts that the regulatory standard does not do it. That assertion needs
checking against the guidance itself, with an ecotoxicology reader.

## What the vignette currently claims

Three claims, in `vignettes/example6.Rmd.orig`, section *Data preparation and
the meaning of zero*:

1. **A taxonomy of zeros** — structural (died/failed), floored (a substituted
   value standing for a negative), genuine (≈ 0). A hurdle model assumes the
   first; growth data frequently contain the second.

2. **The specific OECD claim, which is the one to verify.** Quoted verbatim:

   > The regulatory standard does not floor. OECD TG 201 defines the growth
   > endpoint as average specific growth rate,
   > `µ = (ln X_j − ln X_i)/(t_j − t_i)`, which lives on the real line, and
   > percent inhibition `%I_r = (µ_C − µ_T)/µ_C × 100`, which **exceeds 100%
   > when `µ_T < 0`**. Net decline is expressed as more than complete
   > inhibition rather than truncated away.

   This was written without consulting TG 201 directly. It needs checking:
   whether the formulae are stated as given, whether TG 201 says anything
   explicit about negative µ, and whether "the regulatory standard does not
   floor" is a fair characterisation or an inference from the formulae. TG 201
   is the *algal* growth inhibition test; whether it generalises to the
   invertebrate growth endpoints the vignette is really about (e.g. the
   `nassarius` snail data) is a separate question and is currently glossed.

3. **Recommended handling**, a three-item list that would be what changes:
   - do not floor — model on a real-line scale with a lower-asymptote model
     (`nec4param`, `ecx4param`), and drive any hurdle from a recorded failure
     indicator rather than `y == 0`;
   - if already floored and unrecoverable, use a left-censored (Tobit)
     likelihood asserting "true value ≤ 0";
   - if a strictly positive scale is required, model relative to each unit's
     own baseline, noting that ECx on a ratio and ECx on an increment are
     different quantities.

## What is already established and should not be relitigated

* The substitution argument rests on @helsel2006 (substitution fabricates data
  and biases statistics), with @blasco2019 and @martin2005 for establishing
  zero provenance before choosing a model, and @warton2005 for "many zeros does
  not imply zero inflation". All four are already in `vignettes/bayesnec.bib`.
* The normalisation caveat (@ritz2026, issue #173) is a *different* argument
  about dividing by a control mean, and is settled — do not merge the two.
* The distinction between structural zeros and rounding is settled and
  demonstrated with the `herbicide` data (gap-ratio diagnostic); left-censoring
  at the recording resolution is issue #181. Flooring is a third, separate
  cause of zeros.

## Practical notes for whoever picks this up

* **Prose-only vignette edits do not need a re-render.** `knitr` copies
  markdown verbatim, so a prose change can be applied to *both*
  `vignettes/example6.Rmd.orig` and `vignettes/example6.Rmd` in step and
  committed. This matters: a full re-render is a ~4 hour, ~13 GB job (48 model
  fits) and has failed on memory more than once. Only change the `.orig` alone
  if you have altered a code chunk, in which case the whole vignette must be
  rebuilt via `vignettes/precompile.R`.
* Changing **Recommended handling** is prose-only unless it introduces new
  worked code.
* If new references are needed, add them to `vignettes/bayesnec.bib`.

## Repository state at handoff

* Branch `hurdle-vignette`, PR #179, based on `zi-beta-impl` (not `dev`
  directly — that chain is `hurdle-vignette` → `zi-beta-impl` →
  `hurdle-gamma-impl` → `dev`).
* Six commits beyond the vignette's original: three package changes
  (`model_survival`/`bnec_joint`, N(S)EC labelling, the `ecx`/`nsec` `dpar`
  fix) and three vignette commits.
* Full test suite passes (950).
* Related open issues: #173 normalisation, #175 the vignette itself, #39 and
  #166 on `ecx`/`nsec` resolution, #180 cached prediction matrix, #181 `cens()`
  support.
* Companion notes in `notes/`: `hurdle_gamma_design.md`, `ecx_dpar_issue.md`,
  `ecx_resolution_findings.md`.
