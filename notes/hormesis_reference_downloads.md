# Hormesis reference library

Seven PDFs have been copied from the shared `ignore/references` library into `ignore/hormesis_references/` in the main `bayesnec` checkout. The destination is ignored by Git and is separate from the shared library, to which `ignore/references` is a symbolic link. The copied files were checked against their sources by SHA-256. They are available locally but are not included in the package or either pull request.

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

## Still to obtain

These eleven sources were not found in the shared PDF library. The filename is a suggested name for a copy placed in `ignore/hormesis_references/`. Some official sources are web pages or text records; use their linked PDF where offered, or retain a local print-to-PDF copy.

| Suggested filename | Source | Reason to retain |
| --- | --- | --- |
| `EnvironmentCanada_StatisticalMethods_2005_2007.pdf` | [Environment Canada EPS 1/RM/46](https://publications.gc.ca/collections/collection_2012/ec/En49-7-1-46-eng.pdf) | Section 10.3 discusses true-control reference, hormesis models and alternative procedures. Direct PDF. |
| `EnvironmentCanada_AlgalGrowth_2007.pdf` | [Environment Canada EPS 1/RM/25](https://www.canada.ca/en/environment-climate-change/services/wildlife-research-landscape-science/biological-test-method-publications/growth-inhibition-test-freshwater-alga.html) | Algal definitions, stimulated observations and fallback interpolation. Official page. |
| `EU_Regulation_2016_266.pdf` | [Commission Regulation (EU) 2016/266](https://eur-lex.europa.eu/eli/reg/2016/266/oj/eng) | European advice on low ECx in stimulated algal tests. Official page. |
| `ECHA_Metals_R7_13_2.pdf` | [ECHA metals appendix](https://www.umweltbundesamt.de/sites/default/files/medien/2674/dokumente/information_requirements_r7_13_2_en.pdf) | EC10 and Brain–Cousens advice, relevant to Hg. Direct PDF. |
| `USEPA_OCSPP_850_4000_2012.pdf` | [US EPA OCSPP 850.4000](https://downloads.regulations.gov/EPA-HQ-OPPT-2009-0154-0022/content.pdf) | US definitions of hormesis and control-relative ICx. Direct PDF. |
| `USEPA_WET_Guidance_2000.pdf` | [US EPA WET guidance](https://nepis.epa.gov/Exe/ZyPURL.cgi?Dockey=P1008S23.TXT) | Investigation and reporting of unusual response shapes. The linked NEPIS record is text. |
| `Brain_Cousens_1989.pdf` | [Brain and Cousens (1989)](https://doi.org/10.1111/j.1365-3180.1989.tb00845.x) | Original modified log-logistic hormesis model. Article landing page. |
| `vanEwijk_Hoekstra_1993.pdf` | [Van Ewijk and Hoekstra (1993)](https://doi.org/10.1006/eesa.1993.1003) | Hormetic endpoint parameterisation. Article landing page. |
| `Cedergreen_Ritz_Streibig_2005.pdf` | [Cedergreen et al. (2005)](https://doi.org/10.1897/05-014R.1) | Flexible empirical model and stimulation inference. Article landing page. |
| `Negri_et_al_2021.pdf` | [Negri et al. (2021)](https://doi.org/10.1016/j.marpolbul.2021.112899) | Marine case-study experiment and interpretation. Article landing page. |
| `Jager_Barsi_Ducrot_2013.pdf` | [Jager et al. (2013)](https://doi.org/10.1007/s10646-012-1022-0) | Biological reasons why increased growth need not be beneficial. Article landing page. |

The Negri individual-observation workbook is already available in the ignored local analysis folder, `ignore/hormesis_data/` in the manuscript worktree, from the [AIMS download](https://data.aims.gov.au/data-download/615e208d-f6c7-4b2f-b99e-d4fc50614e48/Negri_etal_2021_Experimental-data_Toxicity-thresholds-gas-condensate-oils.xlsx). Its [catalogue record](https://researchdata.edu.au/condensate-toxicity-testing/1702929) gives a CC BY-NC 3.0 Australia licence. The `drc` source, including `lettuce.rda` and its GPL-2 `DESCRIPTION`, is available from [CRAN](https://cran.r-project.org/src/contrib/drc_4.0-0.tar.gz). These are data sources, not papers. The FWO-12_a1 mercury observations remain outside the vignette until the separate reuse request is resolved.
