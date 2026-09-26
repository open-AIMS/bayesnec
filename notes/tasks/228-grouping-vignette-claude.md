# Specification — the grouping vignette (#6, #33)

Status, 2026-09-26: complete, and kept as a record rather than a specification.
PR #228 merged on 2026-09-17. Section 1 describes a branch state that no longer
exists; the per-plate setting it lists as an open refinement is done at
`data-raw/lum31.R`; the equation choice for `lum31` it lists as not yet made has
been made. `notes/tasks/dataset-response-gradients.md`, cited below, exists only
on the unmerged branch `notes-dataset-gradients`. Remaining scope is #382 and
#388, implemented by PR #402.

Companion to `228-grouping-vignette-human.md`. Written 2026-09-09 against `dev`
at `eebccdb3` and branch `issue-6-33-grouping-vignette-v2` at `815b80c9`.
Supersedes the version of 2026-09-08, kept at `superceded/228-plan-2026-09-09/`.

All measurements below were made with `bayesnec` 2.1.3.33 installed from
`dev`, `seed = 228`, `rstan` backend, unless stated otherwise. Screening runs
used 2 chains and `iter = 2000` (800 retained draws); these are too short to
judge mixing and are marked as screens.

---

## 1. State of the branch

`issue-6-33-grouping-vignette-v2` was cut from `dev` and force-pushed over the
PR #228 head on 2026-09-09. The pre-rebuild tip `f7f3d751` is preserved on the
remote as `issue-6-33-grouping-vignette-pre-rebuild`.

Diff against `dev`: `vignettes/example8.Rmd.orig` and `vignettes/bayesnec.bib`
only. The `coral` dataset is not on the branch.

Section `# Group-level term syntax` is written and committed. Sections from
`# The data` onward are unchanged from the pre-rebuild vignette and still refer
to `coral`; they are replaced by tasks 3 to 5.

Bibliography entries added: `ritz2026`, `luter2025`. `brinkman2023` remains
because the unrewritten sections still cite it, and is removed with them if the
rewrite drops the citation. No `flores2021` entry was added: no volume, pages or
DOI for it is recorded anywhere on disk, and the fields must not be invented.

---

## 2. The dataset

### 2.1 Source

Luter HM, Damjanovic K, Thomas MC, Fisher R, Hoj L, Negri AP (2025). *A
bioluminescent bacterial toxicity assay for tropical marine environments.*
Environmental Toxicology. doi 10.1002/tox.70003.

Analysis code: `https://github.com/open-AIMS/Lum31-tox-assays` (public).
Genome: GenBank BioProject PRJNA1245604. AIMS data repository metadata record
`6aae0b3b-fe15-47cc-9a11-9fb669479f61`.

Source workbooks, in `data-raw/lum31/`, git-ignored via `.gitignore:48`:

| file | contents |
|---|---|
| `Cu Acute tests_measured_AIMSrepository.xlsx` | 5 dated sheets, 16 plates |
| `Zn Acute tests_measured_AIMSrepository.xlsx` | 5 dated sheets, 17 plates |
| `Cu_Zn_Chronic_measured_AIMSrepository.xlsx` | 3 plates per toxicant, `y24h` and `cells_ml` |
| `Lum31 Microtox_CuZn_AIMSrepository.xlsx` | Lum-31 against commercial Microtox, 3 plates each |
| `Luter et al 2025 ... .pdf`, `tox70003-sup-0001-supinfo.docx` | article and supplementary information |

### 2.2 Layout of an acute sheet

Each sheet is one dated batch. Column `x` is the measured dissolved metal
concentration in mg/L. Remaining columns are named `<plate>_y<minutes>`, e.g.
`Rep1B_y15`, with `minutes` in `{5, 15, 30}`. The workbook metadata sheet states
that **only 15 and 30 minute data were used in the analyses**; the 5 minute
columns are read and discarded.

Each sheet has **44 rows: 11 concentrations × 4 replicate wells**. Rows 1 to 4
are the control wells, which is how the published script identifies them
(`median(CuDat1[1:4, 4])`).

Batch and plate counts, 15 and 30 minutes, after discarding 5 minutes:

| toxicant | batches | plates | rows per plate | distinct measured concentrations |
|---|---|---|---|---|
| Cu | 5 | 16 | 44 | 53 across batches, 11 within |
| Zn | 5 | 17 | 44 | 54 across batches, 11 within |

Plates per batch: Cu 4, 2, 3, 5, 2; Zn 4, 3, 3, 5, 2.

**Measured concentrations differ between batches**, because each batch was
assayed separately (supplementary Table S1). `conc` is therefore not a common
grid across plates. This is realistic and `bnec()` handles it, but it means
plates cannot be compared point by point.

### 2.3 Response

Raw luminescence in relative light units, range 0 to about 1.1e6.

**Blank correction.** The methods state values were blank-corrected against
instrument blank wells containing artificial seawater (supplementary Figure S3).
A blank-corrected reading can be negative and was replaced by zero, so the zeros
are floored, not observed.

Zeros by arm:

| toxicant | minutes | rows | zeros | per cent | mid-band |
|---|---|---|---|---|---|
| Zn | 15 | 748 | 6 | 0.8 | 58% |
| Zn | 30 | 748 | 77 | 10.3 | 49% |
| Cu | 15 | 704 | 106 | 15.1 | 37% |
| Cu | 30 | 704 | 197 | 28.0 | 36% |

Mid-band is the count in the middle 60 per cent of the observed response range,
as defined in `superceded/228-plan-2026-09-09/` and
`notes/tasks/dataset-response-gradients.md`. For comparison, `herbicide` scores
35 per cent and the best `alga` cell 40 per cent.

**Zn at 15 minutes is the primary arm.** Highest mid-band, fewest zeros.

### 2.4 Censoring

The zeros are left-censored, not a hurdle. A hurdle asserts a distinct process
generating zeros; luminescence output is continuous and strictly positive, and a
zero is a blank-corrected negative, so the generating process is measurement
noise about a small positive value.

**The bound cannot be zero.** Verified: `brms` refuses the fit at data
validation with `Family 'gamma' requires response greater than 0`, before the
likelihood is evaluated. `pgamma(0, ...)` is exactly 0, so the censored
contribution would be `log(0)`.

**The bound is the smallest resolved reading in the arm.** RF's decision,
2026-09-09. For Cu at 30 minutes the smallest non-zero readings are 3, 6, 10,
12, 14, 19, 22, 27, with only two below 10 and 22 below 100, so there is no gap
above zero and the instrument resolves very small values. On a synthetic Gamma
fixture with 9 floored zeros of 60, a bound of 3 gave 0 divergent transitions of
400 and a maximum R-hat of 1.037.

**Open refinement.** With auto-scale gain the smallest resolved reading differs
between plates, so the bound is arguably per plate. Set it per plate and record
the choice; the sensitivity of the estimates to it scales with the censored
fraction, which is negligible for Zn at 15 minutes and material for Cu at 30.

Encode as a `cens()` aterm: `y | cens(censoring) ~ crf(...)`. Note
`R/bayesnecformula.R:247` — `cens()` terms are omitted from `model.frame()`
output. Verify this does not affect the grouped route before writing the
section.

### 2.5 Plate gain and normalisation

The methods state the plate reader procedure "included a 5-s linear shake step,
a 1-s integration time, a 1 mm read height, and **auto-scale gain adjustment**".
Gain is set per read, so absolute luminescence is not comparable between plates.

Measured consequence — median control-well luminescence per plate:

| batch | control RLU per plate | max/min |
|---|---|---|
| Cu 19Mar24 | 1,065,628 · 841,436 · 876,407 · 431,971 | 2.5 |
| Zn 19Mar24 | 1,110,610 · 812,734 · 433,038 · 396,861 | 2.8 |
| Zn 23Apr24 | 458,610 · 918,672 · 476,184 · 669,618 · 875,733 | 2.0 |
| Cu 1Oct24 | 230,976 · 342,642 | 1.5 |

Plates run on the same day differ up to 2.8-fold; the October batch sits 4.6-fold
below the March batches.

**The published analysis divides twice.** From `bayesNEC_30m_final.R`:

```r
Rep1B_y30_qtox  = (1 - (median(CuDat1[1:4, 4]) - Rep1B_y30) / median(CuDat1[1:4, 4]))
Rep1B_y30_qtoxB = Rep1B_y30_qtox / max(Rep1B_y30_qtox)
```

The first line simplifies to `RLU / median(control wells)`. The second divides by
the plate maximum, which forces exactly one observation to 1.000 — the signature
found in the 2023 extract `example_pgl.csv`, which is
`Lum31_15min_27C_r1-4.csv` multiplied by 0.999.

Gain is a per-plate multiplicative constant, so the first division removes it.
The second is a separate step with no such justification.

**Ritz et al. (2026) is the argument against the first division.** Dividing by
an estimated control mean induces correlation between all observations sharing
the denominator; ignoring it biases effect doses downwards and gives intervals
that are too narrow. Their simulation, log-logistic, 1000 runs, Table 4:

| approach | ED10 bias | CV | coverage (nominal 0.95) |
|---|---|---|---|
| normalise, fit assuming independence | 2.6–6.8% | ~26% | 0.88–0.91 |
| model the induced correlation | 1.7–5.1% | ~23% | 0.92–0.94 |
| model the raw rates | 0.7–2.1% | ~13% | 0.94–0.96 |

They state that weights do not suffice, and that per-curve normalisation on few
control measurements is the worst configuration. The Lum-31 assay normalises
each plate by four control wells, which is that configuration. Their three-control
row already shows 5.1 per cent ED10 bias and 0.88 coverage.

Their recommended approach models the raw response and derives inhibition from
the fitted control, `f0,raw(x) = 1 - f(x, β)/f(0, β)`, where `f(0, β)` is the
control level as a parameter. In `bayesnec` that parameter is `top`, and since
#281 `ecx()` is measured from the modelled control, so a `bnec()` fit on raw
luminescence is that approach.

**A group-level term is the division, done with uncertainty.** Verified by
generating the formula under `Gamma(link = "identity")`:

```
                            # (top | plate)
rlu     ~ bnectop * exp(-exp(beta) * (conc - nec) * step(conc - nec))
top     ~ 1
topgl   ~ 0 + (1 | plate)
bnectop ~ top * exp(topgl)

                            # ogl(plate)
rlu     ~ bnecmu * exp(ogl)
bnecmu  ~ top * exp(-exp(beta) * (conc - nec) * step(conc - nec))
ogl     ~ 1 + (1 | plate)
```

`exp(topgl)` and `exp(ogl)` are per-plate multiplicative factors, which is the
form of the gain. Division conditions on a point estimate of that factor;
the group-level term estimates it and propagates the uncertainty.

**Prefer `(top | plate)` for three-parameter equations.** For `nec3param` the
curve is `top × g(x)`, so scaling `top` scales the whole curve and the two terms
are equivalent in effect. `topgl ~ 0 + (1 | plate)` has no intercept, whereas
`ogl ~ 1 + (1 | plate)` does and is confounded with `top` at the population level
— `vignette("example3")` handles this with a zero-centred prior, but not needing
the correction is better. For four-parameter equations they differ and `ogl()` is
the match to gain, which multiplies the lower asymptote too.

**Limitation to state wherever the plate variance is reported.** Gain and genuine
plate-to-plate biological variation are confounded in `exp(topgl)`, and nothing
in these data separates them. This does not affect NEC or ECx, for which both are
nuisance plate effects, but the plate-level standard deviation is not a
biological quantity.

**Ritz's caution on treatments applies.** Normalisation, or a shared plate term,
is legitimate only within curves of the same treatment. Plates within one
toxicant-by-time arm qualify. Do not place a plate term across the Cu-versus-Zn
or 15-versus-30-minute contrasts; those are `bnec_group()` levels.

---

## 3. Measurements supporting the section plan

### 3.1 Group-level term screens

Two chains, 2000 iterations, 800 retained draws, `adapt_delta = 0.80`.

Within-concentration, `ogl()`:

| dataset | levels × obs | family | divergent | R-hat | min ESS |
|---|---|---|---|---|---|
| `Becky long` | 27 × ~19 | gaussian | 0 | 1.009 | 339 |
| `pamdat` | 54 × 8 | Beta | 0 | 1.018 | 170 |
| Antarctic vessel | 24 × ~4.5 | binomial | 0 | 1.006 | 248 |
| `CoralColour` | 45 × 4 | Beta | 291 | 1.098 | 29 |

Across-concentration:

| fit | levels | divergent | R-hat | min ESS |
|---|---|---|---|---|
| sea urchin `(nec \| test)` | 4 | 0 | 1.014 | 234 |
| sea urchin `pgl(test)` | 4 | 33 | 1.014 | 155 |
| sea urchin ungrouped | — | 9 | 1.864 | 3 |
| Antarctic `pgl(Test.ID)` | 5 | 11 | 1.063 | 47 |
| Antarctic `pgl + ogl` | 5 + 24 | 1 | 1.017 | 54 |
| multi-assay `pgl(random_effect)` | 4 | 2 | 1.047 | 46 |

`herbicide` `pgl(herbicide)` gave 0 divergent of 800 and R-hat 1.014, and
`(bot | herbicide)` 0 and 1.028, both clean. `(nec | herbicide)` gave 0
divergent but R-hat 1.836 and min ESS 3, chains in separate modes. **None of
these is used**: the seven herbicides are fixed levels of interest, not a sample
from a population, so a random structure across them has no scientific meaning
(RF, 2026-09-09). They are retained here only as evidence that #295 works.

### 3.2 `bnec_group()` on `herbicide`

Two chains, `nec4param`, `Beta(link = "identity")`, predictor `log(concentration)`.
All seven levels converge:

| level | divergent | R-hat | min ESS |
|---|---|---|---|
| ametryn | 0/800 | 1.009 | 317 |
| atrazine | 0/800 | 1.006 | 337 |
| diuron | 0/800 | 1.005 | 380 |
| hexazinone | 0/800 | 1.006 | 403 |
| irgarol | 0/800 | 1.005 | 333 |
| simazine | 0/800 | 1.010 | 364 |
| tebuthiuron | 0/800 | 1.018 | 406 |

This reproduces the JSS paper's final figure. `jss4916.tex:799-805`,
`fig:fullbayesmanecplot`, is "Full model averaged bayesmanecfits to seven
phototoxicity data sets, showing estimated no effect concentrations", decline set
only, supported by `tab:weightsTab` and `tab:probdiffs`. That figure was
assembled from seven separate calls. **Use the `decline` set, to match**, and use
`compare_posterior()` to reproduce `probdiffs`.

### 3.3 Parameter count and design

Not needed for `lum31`, recorded because it produced #301 and because
`CoralColour` is the reserve dataset.

`CoralColour` has five distinct concentrations. With `ogl(chamber)`, screens:

| equation | curve parameters | divergent | R-hat | min ESS | R² | `sd` |
|---|---|---|---|---|---|---|
| `nec4param` | 4 | 291/800 | 1.098 | 29 | — | 0.454 |
| `ecx4param` | 4 | 150/800 | 1.029 | 126 | — | 0.424 |
| `ecxll3` | 3 | 0/800 | 1.012 | 201 | 0.917 | 0.423 |
| `ecxwb2p3` | 3 | 0/800 | 1.014 | 165 | 0.917 | 0.420 |
| `ecxwb1p3` | 3 | 0/800 | 1.019 | 181 | 0.917 | 0.430 |
| `ecxexp` | 2 | 0/800 | 1.043 | 117 | 0.917 | 0.909 |
| `nec3param` | 3 | 0/800 | 1.083 | 17 | 0.918 | 0.421 |
| `ecxlin`, `neclin` | 2, 3 | refused — invalid for Beta with identity link | | | | |
| `ecxsigm` | 3 | refused — requires a non-negative predictor | | | | |

At 4 chains and 8000 iterations, `nec3param` gives 0 divergent of 16000, R-hat
1.011 and min ESS 499, and `ecxll3` gives 0, 1.001 and 2410. **The 800-draw
screen was too short to judge mixing**; an earlier reading of `nec3param` as
failing to mix was withdrawn.

Raising `adapt_delta` to 0.99 with 8000 iterations on `nec4param` reduced the
divergence rate from 36 to 1.4 per cent but left R-hat 1.381 and min ESS 5. The
failure is a near-saturated mean function against five concentrations, not a step
size. The group-level `sd` is 0.42 to 0.45 under every equation that samples, so
the effect estimated is stable.

---

## 4. Task specification

### Task 1 — `data-raw/lum31.R` and the dataset

Read the two acute workbooks. For each sheet, pivot the `<plate>_y<minutes>`
columns to long format. Derive:

| column | type | definition |
|---|---|---|
| `toxicant` | factor | `Cu`, `Zn` |
| `batch` | factor | sheet name, e.g. `19Mar24` |
| `plate` | factor | `paste(toxicant, batch, plate_label)` — labels repeat between sheets |
| `well` | factor | `paste(plate, conc, seq_len(4))` — four wells per concentration per plate |
| `minutes` | integer | 15 or 30; 5 is discarded |
| `conc` | numeric | measured dissolved metal, mg/L |
| `rlu` | numeric | raw luminescence, blank-corrected and floored at zero |
| `censoring` | character | `"left"` where `rlu == 0`, else `"none"` |
| `rlu_cens` | numeric | `rlu`, with zeros replaced by the per-plate smallest non-zero `rlu` |

Invariants to assert in the script: every plate has 44 rows per exposure time;
every concentration within a plate has exactly 4 wells; `rlu_cens > 0`
everywhere; `censoring` is `"left"` exactly where `rlu == 0`.

Do not include the 5 minute columns, the chronic file or the Microtox comparison
file in the shipped object unless a section needs them. If the factor-covariate
section uses the assay comparison, ship it as a second object rather than
rbinding it to a dataset with a different design.

Document in `R/data.R` following the `nassarius` block. State the source, the
design, the blank correction, the flooring, and that concentrations are measured
per batch. Do not state a detection limit; none is recorded.

### Task 3 — the normalisation section

Structure, following the outward-facing rules in `CLAUDE.md` §12: describe what
was done before giving the result.

1. The assay and its published analysis, including the two divisions, quoted from
   `bayesNEC_30m_final.R`.
2. Why the first division is not gratuitous: auto-scale gain, with the
   control-RLU table from §2.5.
3. The Ritz et al. (2026) result, with the simulation table.
4. The equivalence: generate the `(top | plate)` formula and show
   `bnectop ~ top * exp(topgl)`.
5. The limitation from §2.5 — gain and biology confounded.

Do not restate the scale argument from `vignette("example3")`; cross-reference it.

### Task 5 — fits

Fit at 4 chains, `iter = 8000`, `warmup = 4000`, `seed = 228`, and record
divergences, maximum R-hat and minimum effective sample size for each. The
screens in §3 are not sufficient for the vignette.

Arms:

| section | fit |
|---|---|
| 4 | `rlu_cens \| cens(censoring) ~ crf(log(conc), <eq>)`, ungrouped |
| 4 | `... + ogl(well)` |
| 5 | `... + (top \| plate)` |
| 5 | `... + pgl(plate)` |
| 6 | `bnec_group(fvfm ~ crf(log(concentration), "decline"), group = "herbicide")` |
| 6 | `bnec_group(... , group = "toxicant")` on `lum31` |

Equation choice for `lum31` is not yet made. Eleven concentrations admit
four-parameter equations, unlike `CoralColour`, so the `decline` set is the
starting point; record which equations `check_models()` retains under `Gamma`
with a log predictor before committing to a set.

---

## 5. Rejected alternatives

**`beta_ub`.** An estimated upper bound was explored and rejected; the outcome
was dispersion scaling (RF, 2026-09-09). Do not propose it. The plan document
`ignore/zib_ub_family_plan.md` and branch `beta-ub-impl` are a plan and an
abandoned implementation, not the decision.

**Normalising to the control and modelling `Beta`.** What the paper does, and
what Ritz et al. (2026) argue against. Retained in the vignette only as the
comparison in section 2.

**A hurdle for the zeros.** Asserts a distinct zero-generating process. The
zeros are blank-corrected negatives, so the process is measurement noise.

**A universal ceiling in RLU.** Auto-scale gain means RLU is a property of the
read, not of the assay, so no such ceiling exists. Supplementary Figure S1
measures 44 technical replicates of undosed cells but at one gain, so it bounds
that read only.

**`example_dat.csv`**, the Antarctic set. Traced as far as a copy dated
2023-03-31 in `XJUNK/xxbayesnec - Copy/`, content-identical, written by
`write.csv()` from a source object. No script writes it, no file on disk contains
its column names, and the only label is the comment `# Antarctic dataset` at
`ignore/random_effects.R:101`. RF could not identify the source. A manual page
would have to state a provenance that does not exist. A simulated replacement
with the same structure was proposed and is superseded by `lum31`.

**`Becky long.csv`.** The best response gradient found anywhere, 78 per cent
mid-band, 27 vessels, and `ogl(vessel)` gives 0 divergent of 800. Provenance
recorded nowhere; file dated January 2021.

**`sea_urchin`, four copper reference tests** (`toxtools`). **Out, RF
2026-09-09**, because only the first of the four tests is published and the
source workbook is not redistributed; see `notes/dataset_provenance.md`. Four
levels is also below the five-level convention. The measurement is kept because
it is the clearest demonstration found that a group term rescues a fit pooling
breaks — ungrouped R-hat 1.864 and min ESS 3, against `(nec | test)` 0 divergent,
R-hat 1.014, min ESS 234 — and it belongs in the training material if the
permission question is ever settled.

**`CR_workflows` datasets.** One row per replicate vessel, so no grouping
structure of any kind exists in them.

**The Brinkman coral colour-score endpoints**, `Surv_data.xlsx` sheets
`1mn_colour_`, `naph_colour_` and `tol_colour_`, of which `ignore/example_ogl.csv`
and the training repository's `example_ogl.csv` are the 1-MN sheet. Structurally
these are among the best found — 20 to 29 chambers within concentration, five
tile colours spanning the whole series, and 80 per cent mid-band for 1-MN — and
the 1-MN sheet is already public under CC0. **They are rejected on the direction
of the response.** Mean `T7/T0` colour ratio by concentration:

| sheet | control | ... | top concentration |
|---|---|---|---|
| `1mn_colour_` | 0.717 | 0.773, 0.780, 0.813 | **0.971** |
| `naph_colour_` | 0.941 | flat across nine concentrations | 0.994 |
| `tol_colour_` | 0.965 | flat across eight concentrations | 0.953 |

Colour loss *decreases* with dose in the 1-MN sheet, which is biologically
backwards and is almost certainly survivor bias: at the lethal concentrations
only the least affected fragments remain to be measured. That is the same
conditioning-on-survival problem that makes `nassarius` growth a `hurdle_gamma`
endpoint. The naph and tol sheets show no concentration response at all.

**A data-preparation trap that is not a defect.** `bnec()` refuses an integer
predictor (`R/set_distribution.R`, asserted by
`tests/testthat/test-fit_bayesnec.R:188`). Three screening fits failed on this
before it was noticed, because a concentration column read from a spreadsheet as
whole numbers arrives as `integer`. `lum31` measured concentrations are decimal
and so are unaffected, but any derived or nominal series must be passed through
`as.numeric()`.
