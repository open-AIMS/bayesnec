# Provenance and redistribution status of candidate datasets

Written 2026-09-09, during the case-study sweep for the grouping vignette
(#6, #33). This note records **where each candidate dataset came from and
whether it can be shipped**. It does not record structure, response gradient or
fit diagnostics; those are in `notes/tasks/228-grouping-vignette-claude.md`.

The purpose is to stop the same searches being repeated. Several of the
conclusions below are negative results that took a disk-wide search to reach.

---

## Cleared for use

### `lum31` — Luter et al. (2025)

Acute copper and zinc bioluminescence data from the Lum-31 assay.

| | |
|---|---|
| Publication | Luter HM, Damjanovic K, Thomas MC, Fisher R, Hoj L, Negri AP (2025). *A bioluminescent bacterial toxicity assay for tropical marine environments.* Environmental Toxicology. doi 10.1002/tox.70003 |
| Analysis code | `github.com/open-AIMS/Lum31-tox-assays`, public |
| Data repository | AIMS metadata record `6aae0b3b-fe15-47cc-9a11-9fb669479f61` |
| Genome | GenBank BioProject PRJNA1245604 |
| Authorship | RF is an author |
| **Redistribution** | **Agreed, RF 2026-09-09.** Shipping the acute data in the CRAN package is approved |

Source workbooks are in `data-raw/lum31/` and are excluded from version control
by `.gitignore:48`. They are the "AIMSrepository" files, plus the article and its
supplementary information.

An earlier extract of the same experiment circulated as `ignore/example_pgl.csv`
and `ignore/Lum31_15min_27C_r1-4.csv`. Both are superseded: they are the response
after division by the control median and then by the plate maximum, multiplied by
0.999. The raw workbooks are the correct source.

The training vignette `8Factor_covariates_and_groupings.Rmd` states of this data
that "it is not yet published, so don't share these data with others". That
request predates publication and is superseded by Luter et al. (2025).

### `CoralColour.csv`, `pamdat.csv` — Flores et al. (2021)

Coral colour score and PAM effective quantum yield, diuron crossed with three
climate scenarios (2018, 2050, 2100), *Acropora millepora*.

Identified 2026-09-09 from `ignore/paper.bib`, entry `flores2021`, which the JSS
paper cites for `compare_posterior()`: "this function has been used to compare
toxicity of herbicides across three different climate scenarios, to examine the
cumulative impacts of pesticides and global warming on corals".

> Flores F, Marques JA, Uthicke S, Fisher R, Patel F, Kaserzon S, Negri AP
> (2021). *Combined effects of climate change and the herbicide diuron on the
> coral* Acropora millepora. Marine Pollution Bulletin.

RF is an author. **No volume, page range or DOI for this paper is recorded
anywhere on disk.** `ignore/paper.bib` gives author, journal, title and year
only. Those fields must be obtained before a bibliography entry is written, and
must not be inferred — an earlier draft of the entry invented a volume, page
range and DOI and was withdrawn.

**Redistribution confirmed, RF 2026-09-11**, on the grounds that the paper is
published and RF is an author. Both files are now shipped, as `coral_colour`
(colour score, 180 rows, 45 chambers, five concentrations) and `coral_pam`
(effective quantum yield, 414 rows, 54 chambers, six concentrations), built by
`data-raw/coral_diuron.R`. The source CSVs are kept in `data-raw/coral_diuron/`
and are git-ignored.

The missing bibliography fields were obtained from the Crossref API rather than
inferred: Marine Pollution Bulletin, volume 169, article 112582,
doi 10.1016/j.marpolbul.2021.112582. The `flores2021` entry in
`vignettes/bayesnec.bib` uses those values.

### `herbicide` — Jones and Kerswell (2003)

Already shipped, already documented, no outstanding question. Recorded here only
so the list is complete.

---

## Blocked on provenance

### `ignore/example_dat.csv` — origin not established

109 rows, five tests `T01`–`T05`, binomial `suc`/`tot` with `tot = 10`
throughout, four to five replicate vessels per test, measured concentrations from
0 to 1146 in an approximately doubling series.

Structurally one of the strongest candidates found: it supports both
within-concentration and across-concentration grouping, and `pgl(Test.ID) +
ogl(vessel)` fits it at 1 divergent transition of 800.

**It cannot be shipped, because no source can be stated.** What was established:

- The earliest copy on disk is `XJUNK/xxbayesnec - Copy/ignore/example_dat.csv`,
  mtime **2023-03-31 16:46**, content-identical to the current file. The current
  file differs only by the absence of the `write.csv()` row-index column, and its
  2026-08-24 mtime is a re-save.
- The 2023 copy has a `write.csv()` signature — quoted header, `""` index column
  — so it was produced in R that day from a source object. No script on disk
  contains a `write.csv()` call producing these columns.
- It has never been in git. `ignore/` is git-ignored.
- The only descriptive label anywhere in any repository is the comment
  `# Antarctic dataset` at `ignore/random_effects.R:101`.
- The same day, `ignore/Lum31_15min_27C_r1-4.csv` arrived at 13:26 and
  `example_dat.csv` at 16:46, so 31 March 2023 was a session about group-level
  effects.
- RF searched their own email and could not identify the sender; the record does
  not go back far enough.

Searches performed, all negative:

| search | result |
|---|---|
| disk-wide grep for `Measured.Conc`, `Measured Conc`, `Test.ID` in `.csv`, `.R`, `.Rmd`, `.qmd`, `.txt`, `.md` | no source file; the hits are RF's house column vocabulary in unrelated projects |
| shared-strings scan of every `.xlsx` in `ignore/` | "measured conc" only in the two Brinkman coral workbooks, a different design |
| filename search for `antarct`, `amphipod`, `AAD` | nothing |
| **`3748 Vermilion/R_results/all_dat.csv`**, which shares the `Test.ID`/`suc`/`tot`/`conc` vocabulary | **not the source.** 1 of 24 concentrations matched (the control), species is *Aliivibrio fischeri*, `Test.ID` is of the form `154`, `VF220630`, and `tot` takes values 5, 10, 20, 40, 100, 175 |

If this structure is wanted, the plan is to simulate a replacement with the same
design from known parameter values, which also permits showing that `pgl()`
recovers the among-test variation it was given.

### `ignore/Becky long.csv` — origin not established

507 rows, 8 concentrations, 27 replicate vessels of about 19 organisms each,
continuous growth response. The best response gradient of any dataset screened
(78 per cent mid-band, against 35 per cent for `herbicide`), and `ogl(vessel)`
fits it at 0 divergent transitions of 800.

File dated **January 2021**. No script reads or writes it. No provenance recorded
anywhere. It is referenced only in the screening notes for this vignette.

Worth one direct check with RF, because as a simple `ogl()` teaching example it
is the strongest available.

### `ignore/Compiled_multi-assay_data.xlsx` — anonymised, source unrecorded

502 rows, columns `random_effect`, `factor`, `x`, `replicate`, `y`, `trials`,
`proportion`. Already anonymised and prepared as an example, which suggests it
was supplied for that purpose, but no source is recorded.

Separately unsuitable: 46 rows have `trials == 0`, all at the top concentrations
and almost all in one level of `factor`. These are vessels where nothing survived
to be scored. Dropping them truncates the upper end of the curve; retaining them
is impossible under a binomial.

---

## Blocked on redistribution

### `coral` and the rest of the Brinkman et al. (2023) family

> Brinkman DL, Flores F, Luter HM, Nordborg FM, Brooks M, Parkerton TF, Negri AP
> (2023). *Sensitivity of the Indo-Pacific coral* Acropora millepora *to aromatic
> hydrocarbons.* Environmental Pollution 332:121963.

Covers `ignore/Surv_data.xlsx` (all sheets), `ignore/Toxicity Test Data and
WQ_brinkman2022.xlsx`, `ignore/Naph.csv`, `ignore/tol_surv_data.csv` and the
`coral` dataset formerly on the #228 branch.

AIMS permission to redistribute has never been confirmed. `coral` was removed
from the branch on 2026-09-09 rather than retained, so this no longer
blocks #228.

`ignore/example_ogl.csv` is the 1-MN colour-score sheet of this workbook and is
**already public under CC0** in `open-AIMS/cr_modelling_training` (see below), so
for that one sheet the position is inconsistent and worth resolving.

### `toxtools::sea_urchin`, four copper reference tests

`toxtools` is public under GPL-3 and ships the **first of four** tests as
`data/sea_urchin.rda`. The source workbook holding all four is
`toxtools/inputs/Test data.xlsx`, and `toxtools/data-raw/sea_urchin.R` states:

> The workbook itself is not held in this repository: source laboratory records
> are excluded by .gitignore. Obtain it from the study authors.

Using all four tests in `bayesnec` therefore needs the same permission
conversation as the Brinkman data. **Out of scope for now, RF 2026-09-09.**

---

## Public under CC0

`open-AIMS/cr_modelling_training` is a **public repository under CC0 1.0**, and
these three files are tracked on its default branch:

| file | what it is | note |
|---|---|---|
| `example_ogl.csv` | Brinkman 1-MN colour score, wide | same data as `Surv_data.xlsx` sheet `1mn_colour`; **rejected on the response**, see the specification |
| `example_pgl.csv` | Lum-31 15 min, normalised and ×0.999 | superseded by the raw workbooks |
| `example_fi.csv` | Price et al. (2022), zinc and hardness, doi 10.1039/D2EM00063F | third-party data released by AIMS; worth confirming rather than assuming |

This was recorded in the August screen as "refused — unpublished", "third party"
and "unknown". That was wrong on the legal question for all three. It remains
right that the data owner's wishes are a separate matter from the licence.

---

## Simulated sources

The 14 datasets in `CR_workflows/pkg/data` are generated by
`pkg/data-raw/generate_datasets.R` and raise no permission question. They are
unusable for grouping work for a different reason: each has one row per replicate
vessel, so no within- or across-concentration structure survives in them.

`nec_data` (shipped) is likewise simulated.
