# Hormesis in concentration–response analysis

This review supports the hormesis vignette and issue [#570](https://github.com/open-AIMS/bayesnec/issues/570). It separates an observed change in an endpoint from a judgement about ecological adversity. The scope is aquatic toxicity and guideline value derivation, with the terrestrial lettuce assay used as a statistical comparator. The CR training repository is outside scope.

## Common terms and candidate estimands

For an endpoint in which a lower value means poorer performance, write the fitted mean as $m(c)$ at concentration $c$, with control response $m(0)$. Low concentration stimulation followed by inhibition is the descriptive pattern called **hormesis** in Canadian, US and OECD guidance. An isolated rise without subsequent inhibition is a stimulatory response, but does not establish this pattern. An observed rise can also reflect nutrients in a mixture, solvent effects, measurement variation or a different physiological process, so a fitted shape alone cannot establish mechanism.

| Term | Definition for a hormetic curve | Guideline interpretation |
| --- | --- | --- |
| Stimulation peak | $c_{peak}=\arg\max_{c>0}m(c)$ | A curve feature. A threshold equation may parameterise this location as `nec`, but response has already changed from the control below it. |
| Control return | First descending $c>c_{peak}$ where $m(c)=m(0)$ | A descriptive crossing. The endpoint has not yet fallen below the control. |
| ECx / ICx | First descending crossing of the specified reduction from $m(0)$ | The denominator and effect target must be declared. For `bayesnec`, ECx is referenced to the predicted control response. |
| Post-peak NSEC | First descending crossing of a lower posterior quantile of $m(0)$ by the predicted curve, at a declared `sig_val` | A proposed extension of Fisher and Fox's NSEC to a nonmonotone curve; it does not certify biological safety. |
| NEC | A parameterised change point in a threshold equation | For the current hormesis equations, the `nec` parameter locates the peak; it is not a no-change concentration. |
| N(S)EC | Model-averaged draws from threshold and smooth curve no-effect metrics | Its composition and estimand must be explicit when a candidate model is hormetic. |

The ordering $c_{peak}<c_{return}<c_{NSEC}$ follows for a fitted curve with a positive peak and a lower NSEC reference below $m(0)$, provided all crossings occur within the searched range. This is a property of the definitions, not evidence that the NSEC prevents ecological harm. A model may extrapolate a crossing beyond the highest tested concentration; such a result is censored and cannot serve as a precise guideline input.

## International and jurisdictional technical advice

| Authority and scope | Advice on stimulation | Implication for this work |
| --- | --- | --- |
| [OECD TG 201, 2026](https://www.oecd.org/en/publications/test-no-201-alga-growth-inhibition-test_9789264069923-en.html), freshwater algal growth | Paragraph 62 calls for fitting a monotone and a suitable hormesis model when stimulation is observed, selecting with fit diagnostics, and reporting the choice. Stimulation can be ignored only when it is below 10% **and** statistically non-significant. Paragraphs 55–56 require individual replicate regression, fit assessment and uncertainty for ECx. | A hormesis equation belongs in the candidate set; the response and choice of model must be documented. This test guideline does not prescribe a hormetic NSEC. |
| [Environment Canada EPS 1/RM/46, 2005 with 2007 amendments](https://publications.gc.ca/collections/collection_2012/ec/En49-7-1-46-eng.pdf), statistical methods | Section 10.3 recommends a hormesis regression with the *true control* as the reference for point estimates. It advises keeping stimulated observations, and reporting original data and method. It notes that no general answer resolves whether stimulation is biologically deleterious. | The control response is the strongest direct jurisdictional precedent for ECx and a proposed NSEC reference. The unresolved biological question should remain visible. |
| [Environment Canada EPS 1/RM/25, 2007](https://www.canada.ca/en/environment-climate-change/services/wildlife-research-landscape-science/biological-test-method-publications/growth-inhibition-test-freshwater-alga.html), algal growth | The glossary requires both low-dose stimulation and higher-dose inhibition for hormesis. Hormesis regression can use all observations; control replacement is a fallback for interpolation when regression fails. | Do not trim stimulated replicates or silently recode their responses. |
| [EU Commission Regulation 2016/266](https://eur-lex.europa.eu/eli/reg/2016/266/oj/eng), algal growth | It permits ignoring small stimulation for EC50, but calls for special procedures for low ECx and consideration of a hormesis model where stimulation is extreme. | The threshold of concern depends on the estimand; EC50 advice should not be transferred to NSEC or EC5. The 2026 OECD revision is more explicit. |
| [European Chemicals Agency metals guidance, Appendix R.7.13-2](https://www.umweltbundesamt.de/sites/default/files/medien/2674/dokumente/information_requirements_r7_13_2_en.pdf), PNEC for metals | It calls for considering hormesis when calculating EC10 and recommends models such as Brain–Cousens instead of conventional log-logistic models when positive effects appear. It also stresses measured metal concentrations and media chemistry. | A metal case needs chemical and exposure checks as well as a statistical model. The Hg dataset remains conditional on reuse permission. |
| [ANZG Warne et al., 2025](https://www.waterquality.gov.au/sites/default/files/documents/warne-etal-2025.pdf), aquatic guideline values | NEC/NSEC are preferred chronic inputs where defensible. The glossary specifically defines NSEC through a *monotonically decreasing* model. | A post-peak NSEC is a proposed extension, not yet an endorsed ANZG NSEC. Its performance and effect at the estimate should be reported before use in guideline derivation. |
| [US EPA OCSPP 850.4000, 2012](https://downloads.regulations.gov/EPA-HQ-OPPT-2009-0154-0022/content.pdf), plant test methods | It defines hormesis as low-dose stimulation with higher-dose adversity and defines ICx relative to a smoothed mean control response. | Supports a control-based effect reference, while providing no NSEC procedure. |
| [US EPA WET method guidance, 2000](https://nepis.epa.gov/Exe/ZyPURL.cgi?Dockey=P1008S23.TXT), whole effluent tests | Unexpected response shapes require investigation, consistent screening and reporting; a repeated unusual pattern may be real. | Check design, exposure and replication before labelling a curve hormetic. |

These sources agree more strongly on *retaining the observed response and comparing inhibition with the actual control* than on the status of any particular no-effect threshold. Canada explicitly separates the statistical solution from the biological interpretation. ANZG's monotone qualification makes validation of the proposed NSEC extension especially important before a hormetic estimate is supplied to an SSD.

## Statistical and biological literature

[Brain and Cousens (1989)](https://doi.org/10.1111/j.1365-3180.1989.tb00845.x) introduced a modified log-logistic representation of stimulation. [Van Ewijk and Hoekstra (1993)](https://doi.org/10.1006/eesa.1993.1003) developed endpoint parameterisation, while [Cedergreen, Ritz and Streibig (2005)](https://doi.org/10.1897/05-014R.1) proposed more flexible empirical curves and tests of stimulation. The models provide different shapes, so model choice should be checked against the observations and with a diagnostic of the descending limb. A comparison of a monotone model with at least one suitable hormesis model follows OECD TG 201; averaging may be defensible, but the estimand must have the same meaning across its components.

[Fisher and Fox (2023)](https://doi.org/10.1002/etc.5610) defined NSEC for smooth decline and [Fisher et al. (2024)](https://doi.org/10.1002/ieam.4809) compared NEC, NSEC and N(S)EC approaches. The latter excluded the *Cladocopium goreaui* curve because it showed hormesis, leaving a useful case for this extension. Their results cannot be assumed to validate the post-peak rule without a new simulation.

[Jager, Barsi and Ducrot (2013)](https://doi.org/10.1007/s10646-012-1022-0) described acquisition, allocation and medication as distinct explanations of a stimulatory life-history endpoint. Increased growth may therefore coincide with poorer performance in another trait. Conversely, treating every rise as an adverse effect would require a biological argument about the endpoint and its ecological context. The manuscript should call the observed pattern *low-dose stimulation* until the cause has been assessed, and reserve *hormesis* for the biphasic response pattern.

## Existing `bayesnec` examples and candidate data

I screened group means at each concentration in all eight shipped datasets. A ratio above one is descriptive only: it has not been tested for significance or a hormetic model fit. The table records why a dataset would or would not serve as the principal case study.

| Shipped dataset | Largest relevant rise over the lowest treatment mean | Suitability |
| --- | --- | --- |
| `alga` | Up to 5% across the four species–contaminant series | Individual marine growth data and a true zero control, but the rise is small; its main value is a monotone comparator. |
| `coral_colour` | Approximately 10% in the 2018 climate level; lower in 2050 and 2100 | Climate, chamber and fragment structure complicate interpretation; a rise in the derived proportion needs biological checking before it is labelled stimulation. |
| `coral_pam` | No mean rise above the zero-concentration group in any climate level | Useful monotone comparator, not a hormesis case. |
| `herbicide` | Up to 4% for hexazinone; about 1% for simazine | No true zero concentration in the package table, and rises are small. |
| `lum31` | Up to 14% in a zinc batch at 30 minutes; smaller in other batches | Potential secondary case, but plate effects, repeated time points and censoring require a separate analysis. The copper series did not show a comparable mean rise. |
| `nassarius` | Up to 8% in one contaminant series | Mortality is mixed with growth and the exposure units are anonymised; less useful for guideline interpretation. |
| `nec_data` and `manec_example` | Simulated example and stored fit | Useful for software checks, not independent empirical evidence. |

The public *C. goreaui* series in [Negri et al. (2021)](https://doi.org/10.1016/j.marpolbul.2021.112899) has a larger descriptive rise (about 22%), 65 individual observations, a no-WAF control identified in the paper, and a marine growth endpoint. It is therefore the strongest case for the manuscript. Its [AIMS catalogue record](https://researchdata.edu.au/condensate-toxicity-testing/1702929) assigns CC BY-NC 3.0 Australia to the workbook. The individual observations can be analysed locally and linked from a vignette; redistribution inside the GPL-2 package needs a compatible licence or separate permission. `drc::lettuce` has a rise of about 28% with a clear declining limb and reproducible package data, but only two observations at each level and a terrestrial endpoint; it is useful for illustrating identifiability, not for a water guideline value.

The mercury record that exposed issue #570 is **FWO-12_a1**, a *Chlamydomonas reinhardtii* growth-rate series from Cosio et al. (2026) whose individual flow-cytometry observations were supplied by the authors for Hg_DGVs. Its low-dose means were 125% and 133% of the control. **MWO-15_a1** is a different *Thalassiosira weissflogii* series from Wu et al. (2012), digitised from a figure: its low-dose means were only 100.2% and 101.1% of control. The [reuse request](https://github.com/darrenkoppel/Hg_DGVs/issues/17) therefore names FWO-12_a1. Neither mercury dataset is included in the vignette.

## Impact on the existing example vignettes

The change proposed in issue #570 reclassifies the six threshold hormesis equations as NSEC models for toxicity summaries and model averaging. Their sampled `nec` breakpoint remains a curve parameter. A fitted curve or an ECx calculated against its control therefore does not change merely because the reporting classification changes. Existing saved fits and precompiled estimates made with the earlier classification still need to be reviewed before reuse with the new code.

| Example | Fitted model set and expected impact |
| --- | --- |
| 1, single model usage | Fits `nec4param` and `ecx4param`; no threshold hormesis summary is used, so its numeric examples should be unchanged. Its general description of `nec()` and `nsec()` should allow for a hormesis breakpoint reported as NSEC. |
| 2, multi-model usage | Fits `decline`; no threshold hormesis candidate is included, so its numerical summaries are unchanged. A request for `model = "nec"` would include hormesis equations in a different candidate set and should be described with that distinction. |
| 2b, model details | Defines and plots all six threshold hormesis equations. Equations and curve plots are unchanged, but descriptions that call their breakpoint an NEC or their toxicity estimate an NEC need updating. |
| 3, priors | Fits only `nec3param` and `nec4param`; the prior examples and numeric results are unchanged. General references to hormesis parameters may need wording that distinguishes the parameter from the reported estimate. |
| 4, herbicide comparison | Fits `decline` for its analyses; model-averaged N(S)EC values and comparisons should be unchanged. |
| 5, installation | Fits `nec4param`; its worked result is unchanged. |
| 6, hurdle and grouped models | Fits `nec3param` or a six-model set with no hormesis equation; its worked results should be unchanged. The implementation of grouped and hurdle summaries still needs regression checks because these code paths can receive a hormesis fit from other users. |
| 7, censored observations | Fits `decline`; its numeric case studies and simulation are unchanged. |
| 8, luminescence | Several fits request `all`, which admits threshold hormesis equations, so their model-averaged N(S)EC and summary labels need review after the code change. In the detailed pooled-versus-plate comparison, the four named threshold equations, including `nechorme` and `nechorme4`, have zero model weight, so that particular contrast is unlikely to change materially; other `all` fits have not been recalculated. Recompilation of this large vignette is a separate validation task. |
| 9, model checking | Fits `decline` or `nec3param`; the reported example estimates are unchanged. Its explanation of model-averaged N(S)EC is general and should remain accurate for mixed candidate sets. |

The new example 10 is the first vignette to fit a threshold hormesis equation explicitly. Its source keeps the marine data import separate because of the AIMS licence, and its case-study fits remain marked as exploratory until the full diagnostic and model-comparison work is complete.

## Working recommendation and validation needs

For guideline-value use, an explicitly labelled post-peak NSEC is a more coherent default *no-effect estimator* than the hormesis model's `nec` peak parameter: the peak is already different from the control, whereas the proposed NSEC tests for a decline from that control on the descending limb. Canada gives a direct precedent for the true-control reference. This recommendation is conditional on evidence that the model fits both limbs, that the crossing is inside the tested range, that there is no unexamined adverse endpoint, and that posterior uncertainty is acceptable. It should not imply that stimulated values are ecologically beneficial or that the ANZG 2025 method already endorses the extension.

A publication-level validation needs simulation across stimulation amplitude, maximum response, concentration placement, replicates, dispersion, missing high-dose decline and wrong model shape. It should compare the peak, control return, post-peak NSEC and control-based ECx, and record bias, interval coverage, non-identification, censoring and fit diagnostics. The accompanying exploratory simulation is a Gaussian plug-in check of location and design sensitivity; it is not a coverage study of `bayesnec` posterior estimates.
