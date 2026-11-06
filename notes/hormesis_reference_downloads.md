# Hormesis references for the private reference library

Suggested destination: the untracked `ignore/references/hormesis/` folder. The list records primary sources and the role of each in the proposed paper. Several are already in the existing reference library; keep one copy. A DOI landing page may require institutional access to the article PDF.

| Suggested filename | Source | Reason to retain |
| --- | --- | --- |
| `OECD_TG201_2026.pdf` | [OECD Test Guideline 201 (2026)](https://www.oecd.org/en/publications/test-no-201-alga-growth-inhibition-test_9789264069923-en.html) | Paragraph 62 gives the current international algal hormesis procedure. Already present in the local library; the landing page has the PDF download. |
| `EnvironmentCanada_StatisticalMethods_2005_2007.pdf` | [Environment Canada EPS 1/RM/46](https://publications.gc.ca/collections/collection_2012/ec/En49-7-1-46-eng.pdf) | Section 10.3 discusses the true control, hormesis models and alternative procedures in detail. |
| `EnvironmentCanada_AlgalGrowth_2007.pdf` | [Environment Canada EPS 1/RM/25](https://www.canada.ca/en/environment-climate-change/services/wildlife-research-landscape-science/biological-test-method-publications/growth-inhibition-test-freshwater-alga.html) | Algal definitions, handling of original stimulated data and fallback interpolation. |
| `EU_Regulation_2016_266.pdf` | [Commission Regulation (EU) 2016/266](https://eur-lex.europa.eu/eli/reg/2016/266/oj/eng) | Earlier European algal technical advice on low ECx. |
| `ECHA_Metals_R7_13_2.pdf` | [ECHA metals appendix](https://www.umweltbundesamt.de/sites/default/files/medien/2674/dokumente/information_requirements_r7_13_2_en.pdf) | Explicit EC10 and Brain–Cousens advice for hormesis, particularly relevant to Hg. |
| `ANZG_Warne_2025.pdf` | [Warne et al. (2025)](https://www.waterquality.gov.au/sites/default/files/documents/warne-etal-2025.pdf) | Guideline method and monotone qualification in the NSEC glossary. Already present in the local library. |
| `USEPA_OCSPP_850_4000_2012.pdf` | [US EPA OCSPP 850.4000](https://downloads.regulations.gov/EPA-HQ-OPPT-2009-0154-0022/content.pdf) | US definitions of hormesis and control-relative ICx. |
| `USEPA_WET_Guidance_2000.pdf` | [US EPA WET method guidance](https://nepis.epa.gov/Exe/ZyPURL.cgi?Dockey=P1008S23.TXT) | Investigation and reporting of unusual concentration–response shapes. |
| `Brain_Cousens_1989.pdf` | [Brain and Cousens (1989)](https://doi.org/10.1111/j.1365-3180.1989.tb00845.x) | Original modified log-logistic model. |
| `vanEwijk_Hoekstra_1993.pdf` | [Van Ewijk and Hoekstra (1993)](https://doi.org/10.1006/eesa.1993.1003) | Hormetic endpoint parameterisation. |
| `Cedergreen_Ritz_Streibig_2005.pdf` | [Cedergreen et al. (2005)](https://doi.org/10.1897/05-014R.1) | Flexible empirical model and stimulation inference. |
| `Fisher_Fox_2023_NSEC.pdf` | [Fisher and Fox (2023)](https://doi.org/10.1002/etc.5610) | Original NSEC method. Already present in the local library. |
| `Fisher_et_al_2024_NSEC.pdf` | [Fisher et al. (2024)](https://doi.org/10.1002/ieam.4809) | Style and methods template; documents exclusion of the hormetic *Cladocopium* case. Already present. |
| `Negri_et_al_2021.pdf` | [Negri et al. (2021)](https://doi.org/10.1016/j.marpolbul.2021.112899) | Primary marine case study and experimental description. |
| `Jager_Barsi_Ducrot_2013.pdf` | [Jager et al. (2013)](https://doi.org/10.1007/s10646-012-1022-0) | Biological reasons why increased growth need not be beneficial. |
| `Ritz_et_al_2015_drc.pdf` | [Ritz et al. (2015), *PLoS ONE*](https://doi.org/10.1371/journal.pone.0146021) | `drc` model and software citation; the `lettuce` dataset ships with `drc`. |

The Negri individual-observation workbook is public at the [AIMS data download](https://data.aims.gov.au/data-download/615e208d-f6c7-4b2f-b99e-d4fc50614e48/Negri_etal_2021_Experimental-data_Toxicity-thresholds-gas-condensate-oils.xlsx). Its [catalogue record](https://researchdata.edu.au/condensate-toxicity-testing/1702929) gives a CC BY-NC 3.0 Australia licence, so the extracted observations remain in the ignored local analysis folder while a compatible package distribution route is checked. The `drc` package source, including `lettuce.rda` and its GPL-2 `DESCRIPTION`, is available from [CRAN](https://cran.r-project.org/src/contrib/drc_4.0-0.tar.gz). These are data sources, not papers. No FWO-12_a1 mercury observations should enter the vignette until the separate reuse request is resolved.
