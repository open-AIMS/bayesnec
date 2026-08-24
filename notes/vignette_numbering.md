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
