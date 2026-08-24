# Vignette numbering — the register

**Check this file before creating a new `vignettes/exampleN.Rmd.orig`, and add
your row to it in the same commit that creates the file.**

This register exists because two parallel sessions both claimed `example8` in
August 2026 — the #6/#33 grouping vignette and the #219 workflow vignette — and
the collision was only caught at review, on PR #228. Branch names do not reveal
a number, and `dev` does not carry a vignette until its branch merges, so a
number can be taken without being visible anywhere a new session would look.

## Allocated

| # | file | subject | issue | where it lives |
|---|---|---|---|---|
| 1 | `example1` | response types and distributions | — | `dev` |
| 2 | `example2` | model averaging and multi-model inference | — | `dev` |
| 2b | `example2b` | the model set, and theoretical curves | — | `dev` |
| 3 | `example3` | priors | — | `dev` |
| 4 | `example4` | comparing posteriors | — | `dev` |
| 5 | `example5` | *NEC*, NSEC and ECx | — | `dev` |
| 6 | `example6` | hurdle families and zero-inflation | — | `dev` |
| 7 | `example7` | negative growth rates and the zero boundary | #193 | `negsgr-cens-vignette` |
| 8 | `example8` | grouping and factor covariates | #6, #33 | `issue-6-33-grouping-vignette` |
| 9 | `example9` | a complete analysis workflow | #219 | `issue-219-workflow-vignette` |

**Next free number: 10.**

## Rules

1. **Claim the number here first**, on a branch that goes to `dev` quickly, or in
   the same PR that adds the vignette. A number claimed only on a long-lived
   feature branch is invisible to everyone else.
2. **A number is not free just because `dev` has no such file.** Four of the ten
   rows above live on unmerged branches. Check this table, not `ls vignettes/`.
3. **Renumbering is expensive** once a vignette is rendered: `precompile.R`
   output, the figure names under `vignette-fig-`, every `vignette("exampleN")`
   cross-reference, and any published URL all carry the number.
4. `2b` is a historical exception. Do not create further letter suffixes.

---

# Writing style — scientific report, not blog post

**RF, 2026-08-24.** The vignettes drafted in August drifted into a conversational
register that reads as machine-written. Corrected across example1, 7, 8 and 9;
this section records the convention so it does not have to be corrected again.

The reference is the published vignettes, not an abstract style guide. Their
headings are **noun phrases**: *Background*, *Installation*, *The scale of the
predictor*, *Preparing the response*, *Censoring*, *Non-constant dispersion*,
*Choosing a variance function*, *Model definitions*, *Model suitability for
response types*. Match that.

## The five patterns that were corrected

| pattern | example found | replaced with |
|---|---|---|
| question as heading | *Which kind of grouping is it?* | *Types of grouping* |
| | *Does model averaging rescue the floored approaches?* | *Model averaging* |
| conversational what/where clause | *Where these numbers come from* | *Provenance of the reported results* |
| | *What saturation costs* | *Limits of the censored likelihood* |
| | *What to take from this* | *Interpretation* |
| em-dash or colon appositive | ``ogl()` --- an offset shared by a group`` | ``Group-level offsets: `ogl()``` |
| | *A second family: a continuous response* | *A continuous response* |
| comma-and tail | *Failure modes, and being honest about them* | *Failure modes* |
| | *Exclusion, and what it does to the answer* | *Exclusion and its effect on the estimates* |
| definite article plus count | *The eight approaches* | *Zero-handling approaches* |
| | *The twelve scenarios* | *Simulation scenarios* |

## In the body text

The same register shows up in prose. These were removed rather than reworded
wherever they carried no information:

- *it is worth stating / worth noting / worth being explicit that* — say the thing;
- *Read in order.* / *Two things follow, and the second is not what one would guess.*
  — announce structure only where the reader needs it, and without the flourish;
- *actually*, *plainly*, *honest* as intensifiers;
- bold lead-ins phrased as questions (**Why the link is identity.**) rather than
  as claims (**The link is the identity.**).

A residual count remains in example7 (*deliberate*, 13 occurrences) that is
mostly legitimate — a simulation study does need to say which choices were
deliberate — but it is worth a look on the next pass.

## Two structural rules

1. **Do not let a heading make an argument the body then contradicts.** example7
   carried *Why `nec4param` and not a model-averaged set*, and a later section
   model-averaged. State the scope of the main comparison and forward-reference
   the section that relaxes it.
2. **Say a thing once.** The same justification appeared three times in
   example7's Methods. If a point needs restating, the first statement was in
   the wrong place.
