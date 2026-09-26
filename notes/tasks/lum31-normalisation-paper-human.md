# Normalisation, grouping and model averaging in concentration-response analysis — plan

Status, 2026-09-26: not started, and outside the package. `vignette("example8")`
merged with PR #228 on 2026-09-17, which settles the question below of whether
the paper waits for it. The dispersion sub-model screen described as running has
its scripts under `notes/scripts/example8/`; its outcome is not recorded here.
The measurements cite `prompts/grouping-vignette-dataset.md` and
`cache/section-fits/`, which are git-ignored and exist only in RF's main
checkout. No issue tracks the paper.

Companion specification: `lum31-normalisation-paper-claude.md`. Written
2026-09-11 from measurements made while preparing `vignette("example8")` (#6,
#33). Every measurement cited is recorded in
`prompts/grouping-vignette-dataset.md` with the settings that produced it.

---

## Summary

Ecotoxicology routinely divides a response by its control mean before fitting.
Ritz et al. (2026) show that this biases effect concentrations downwards and
gives intervals that are too narrow, and recommend modelling the raw response
with the control level as a parameter. Work on the `bayesnec` grouping vignette
has produced three results that go beyond that recommendation and that are not,
as far as we have found, stated anywhere.

The first is that normalisation is sometimes unavailable rather than merely
inadvisable. The second is that a low effect concentration from a model-averaged
set is far more sensitive to which equations hold weight than a median effect
concentration is. The third is that the variance model decides both, and the
conventional choice misstates the variance at the control by an order of
magnitude.

Together these make a methods paper with a worked example, of use to anyone
fitting concentration-response models to plate-based or vessel-based assays.

---

## The three results

**Normalisation can be unavailable.** A global divisor preserves independence
because it is a change of units. A per-curve divisor does not, which is Ritz's
objection. The property that makes a normalised analysis work — every curve's
control landing near the top of the response scale, where a Beta variance is
small and the upper asymptote is therefore stable — requires the per-curve
divisor. Where an instrument sets its gain per read, as a plate reader with
auto-scale gain does, a global divisor leaves the curves spread across the
scale and buys nothing. Measured on the Lum-31 assay: dividing by the largest
reading in the dataset puts the seventeen zinc plates' controls between 0.171
and 0.898, a 5.2-fold spread.

**A low ECx from a model-averaged set is not robust to set composition.** The
candidate equations agree about EC50 and disagree about EC10. Measured on one
arm of the Lum-31 data: among the equations holding weight, EC50 spans 17 per
cent and EC10 spans a factor of four; across the whole retained set EC10 spans
ninety-fold. Any change that reallocates weight therefore changes EC10 and
leaves EC50 almost unmoved. This is a property of model averaging over a
heterogeneous set, not of any one equation, and it matters because EC10 is the
quantity most often used for guideline derivation.

**The variance model misstates the variance at the control.** A Gamma with an
identity link has one coefficient of variation for the whole curve. The observed
within-cell coefficient of variation on this assay runs from 0.051 at the
control to 0.362 at the bottom of the series. The fitted value is 0.58 with no
plate term and 0.26 with one — five to eleven times the observed value at the
control. Because NSEC is defined against the control's lower quantile, and
because the variance model decides which equation fits best and so which EC10 is
reported, this propagates into both estimates.

---

## What the paper would claim, and what it would not

It would claim that the three results above hold on a real published assay, that
they follow from the design rather than from the particular data, and that a
group-level term with a dispersion sub-model addresses all three.

It would not claim novelty for the normalisation argument itself, which is Ritz
et al. (2026), nor that the Lum-31 result generalises to assays without
per-read gain without further work. The second and third results are measured on
one dataset so far and need at least one more design before they can be stated
as general.

---

## What exists and what is missing

The dataset, the fits and the measurements for the first result exist. The
second result is measured on one arm and needs the same decomposition on a
second design. The third is measured but the remedy is not yet tested; a
dispersion sub-model screen is running.

Nothing is written. No repository exists for it.

---

## Decisions needed before any writing starts

**Where it lives.** A research compendium is a separate repository under
`C:/Rworking/`, following the `ssdinversionrr` pattern, not a directory inside
`bayesnec`. Naming, visibility and authorship are yours to set.

**Whether it waits for the vignette.** The vignette needs the same fits and is
already specified. Finishing it first costs nothing and produces the figures.

**How much more evidence.** One further design would turn the second and third
results from observations into claims. `alga` and `nassarius` are candidates
already in the package, with different families and different designs.
