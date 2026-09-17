# Exploratory fits for example8 (#6, #33)

These scripts produced the measurements the `example8` sections are written
against. They are exploratory: `rstan`, the pre-#304 prior, and no dispersion
sub-model. **None of their numbers ship.** `precompile.R` re-runs every chunk, so
the vignette's own figures come from the precompile run.

They are kept because they are the analysis, not because the answers are final:
re-running any section's comparison after the prior, dispersion and backend
changes means editing the formula in one of these rather than reconstructing it.

Set `BAYESNEC_SRC` to the package source to load (default `.`) and `E8_OUT` to
where fits and CSVs are written (default `cache/section-fits`, git-ignored).

| script | what it fits |
|---|---|
| `fit_section4.R` | one plate, ungrouped and `+ ogl(conc_group)` |
| `fit_section5.R` | whole Zn 15 min arm; `ungrouped`, `toppl`, `pgl`, `oglplate` |
| `fit_section7_herbicide.R` | `bnec_group()` over the seven herbicides |
| `fit_s4_prior302.R` | section 4 with the regularising predictor prior supplied |
| `ec10_decompose.R` | per-equation EC10, EC50, `top`, `bot` and weight |
| `shape_check.R` | fitted Gamma shape and the CV it implies, per equation |
| `disp_screen.R` | constant vs `disp("power")` vs `disp(~log(conc))` |
| `disp_coefs.R` | dispersion coefficients and implied CV along the curve |
| `disp_smooth.R` | `disp(~s(log(conc)))`, model CV against observed |
| `s4_risk.R` | whether `ogl(conc_group)` survives a dispersion sub-model |

The measurements they produced, with settings, are in
`prompts/grouping-vignette-dataset.md`, which is git-ignored and therefore local
to whichever checkout ran them.
