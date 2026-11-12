# Hormesis reference library

All 18 PDFs for this review are now available in `ignore/hormesis_references/` in the main `bayesnec` checkout. Seven were copied from the shared `ignore/references` library with SHA-256 verification, ten were supplied for this review, and the ECHA guidance was retrieved from a working copy hosted by the original author of that guidance. The folder is ignored by Git; none of these PDFs is included in the package or either pull request.

## Copied from the local library

| Local PDF in `ignore/hormesis_references/` | Source | Role |
| --- | --- | --- |
| `OECD_TG201_2026.pdf` | [OECD Test Guideline 201 (2026)](https://www.oecd.org/en/publications/test-no-201-alga-growth-inhibition-test_9789264069923-en.html) | Current international algal procedure; paragraph 62 addresses stimulation and model comparison. |
| `ANZG_Warne_2025.pdf` | [Warne et al. (2025)](https://www.waterquality.gov.au/sites/default/files/documents/warne-etal-2025.pdf) | Guideline method and the monotone qualification in the NSEC glossary. |
| `Fisher_Fox_2023_NSEC.pdf` | [Fisher and Fox (2023)](https://doi.org/10.1002/etc.5610) | Original NSEC method. |
| `Fisher_et_al_2024_NSEC.pdf` | [Fisher et al. (2024)](https://doi.org/10.1002/ieam.4809) | Methods and style template; records exclusion of the hormetic *Cladocopium* case. The source library filename uses its 2023 acceptance year. |
| `Ritz_et_al_2015_drc.pdf` | [Ritz et al. (2015)](https://doi.org/10.1371/journal.pone.0146021) | `drc` methods and software citation. The source library filename uses 2016, but the paper was published in December 2015. |
| `Mattson_2008_Hormesis_defined.pdf` | [Mattson (2008)](https://doi.org/10.1016/j.arr.2007.08.007) | Broader biological definition of hormesis; also cited in the model-details vignette. |
| `Vehtari_et_al_2017_LOO.pdf` | [Vehtari et al. (2017)](https://doi.org/10.1007/s11222-016-9696-4) | PSIS-LOO method used for the three-model Negri comparison. |

## Supplied for this review

| Local PDF in `ignore/hormesis_references/` | Source | Review use |
| --- | --- | --- |
| `En49-7-1-46-eng.pdf` | [Environment Canada EPS 1/RM/46](https://publications.gc.ca/collections/collection_2012/ec/En49-7-1-46-eng.pdf) | Section 10.3, true control, model options and biological interpretation. |
| `rm25-202nded-algaeenglish.pdf` | [Environment Canada EPS 1/RM/25](https://www.canada.ca/en/environment-climate-change/services/wildlife-research-landscape-science/biological-test-method-publications/growth-inhibition-test-freshwater-alga.html) | Algal hormesis definition and conditional ICPIN fallback. |
| `CELEX_32016R0266_EN_TXT.pdf` | [Commission Regulation (EU) 2016/266](https://eur-lex.europa.eu/eli/reg/2016/266/oj/eng) | Paragraph 59 on stimulation and low ECx. |
| `EPA-HQ-OPPT-2009-0154-0022_content.pdf` | [US EPA OCSPP 850.4000](https://downloads.regulations.gov/EPA-HQ-OPPT-2009-0154-0022/content.pdf) | Retaining individual replicates and evaluating stimulation. |
| `P1008S23.PDF` | [US EPA WET guidance](https://nepis.epa.gov/Exe/ZyPURL.cgi?Dockey=P1008S23.TXT) | Nonmonotone responses and NOEC/LOEC limitations. |
| `Weed Research - April 1989 - BRAIN - An equation to describe dose responses where there is stimulation of growth at low.pdf` | [Brain and Cousens (1989)](https://doi.org/10.1111/j.1365-3180.1989.tb00845.x) | Original modified log-logistic hormesis equation. |
| `1-s2.0-S0147651383710031-main.pdf` | [Van Ewijk and Hoekstra (1993)](https://doi.org/10.1006/eesa.1993.1003) | Effect concentration parameterisation under stimulation. The local copy is image-only; its abstract and model section were inspected visually. |
| `Enviro Toxic and Chemistry - 2009 - Cedergreen - Improved empirical models describing hormesis.pdf` | [Cedergreen et al. (2005)](https://doi.org/10.1897/05-014R.1) | More flexible hormesis curves and model sensitivity. The supplied filename has an incorrect year; the PDF is the 2005 paper. |
| `1-s2.0-S0025326X21009334-main.pdf` | [Negri et al. (2021)](https://doi.org/10.1016/j.marpolbul.2021.112899) | Experimental design and *Cladocopium goreaui* growth interpretation. |
| `s10646-012-1022-0.pdf` | [Jager et al. (2013)](https://doi.org/10.1007/s10646-012-1022-0) | Biological mechanisms and life-history trade-offs. |

## Retrieved ECHA document

| Local PDF in `ignore/hormesis_references/` | Source | Review use |
| --- | --- | --- |
| `ECHA_Metals_R7_13_2.pdf` | [ECHA Appendix R.7.13-2, 2008](https://arche-consulting.be/media/pages/tools/reach-guidance-metals-and-metal-compounds/7c2fbf068e-1673879755/information_requirements_r7_13_2_en.pdf) | Section 3.1 on essential-nutrient hormesis and metal exposure quality. The earlier Umweltbundesamt link is unavailable; this ECHA-authored PDF is hosted by [ARCHE Consulting](https://arche-consulting.be/en/tools/reach-guidance-metals-and-metal-compounds), which identifies itself as the appendix's primary author. The local PDF has 78 pages and matches the retrieved file by SHA-256. |

No PDFs remain to be obtained for the current review. The Negri workbook and `drc` source data are separate from this paper library.

The Negri individual-observation workbook is already available in the ignored local analysis folder, `ignore/hormesis_data/` in the manuscript worktree, from the [AIMS download](https://data.aims.gov.au/data-download/615e208d-f6c7-4b2f-b99e-d4fc50614e48/Negri_etal_2021_Experimental-data_Toxicity-thresholds-gas-condensate-oils.xlsx). Its [catalogue record](https://researchdata.edu.au/condensate-toxicity-testing/1702929) gives a CC BY-NC 3.0 Australia licence. The `drc` source, including `lettuce.rda` and its GPL-2 `DESCRIPTION`, is available from [CRAN](https://cran.r-project.org/src/contrib/drc_4.0-0.tar.gz). These are data sources, not papers. The FWO-12_a1 mercury observations remain outside the vignette until the separate reuse request is resolved.
