title: 'bayesnec: An R package for C-R modelling and estimation of No-Effect-Concentrations'
tags:
  - R
  - C-R
  - ecotoxicology
  - effect concentration
  - threshold derivation
  - nec
authors:
  - name: Rebecca Fisher
    orcid: 0000-0001-5148-6731
    affiliation: "1, 2"
  - name: Diego Barneche
    orcid: 0000-0002-4568-2362
    affiliation: "1, 2"
  - name: Gerard Ricardo
    orcid: 0000-0002-7761-0806
    affiliation: "1"
  - name: David Fox
    orcid: 0000-0002-3178-7243
    affiliation: "3, 4"
  
  
affiliations:
 - name: Australian Institute of Marine Science, Australia
   index: 1
 - name: The Indian Ocean Marine Research Centre, University of Western Australia, Australia
   index: 2
 - name: Environmetrics Australia, Beaumaris, Victoria, Australia
   index: 3
 - name: University of Melbourne, Parkville, Victoria, Australia
   index: 4

citation_author: Fisher et al.
date: "2021-06-14"
bibliography: paper.bib
output:
 my_modified_joss:
   fig_caption: yes
csl: apa.csl
journal: JOSS

---

# Summary

The `bayesnec` package in R has been developed to fit concentration(dose) - response curves (C-R) to toxicity data for the purpose of deriving No-Effect-Concentration (*NEC*), No-Significant-Effect-Concentration (*NSEC*), and Effect-Concentration (of specified percentage 'x', *ECx*) thresholds from non-linear models fitted using Bayesian MCMC fitting methods via `brms` [@Burkner2017; @Burkner2018] and `stan` [@rstan2020]. The package is an adaptation and extension of an initial package `jagsNEC` [@Fisher2020] which was based on the `R2jags` package [@Su2015] and `jags` [@Plummer2003]. In `bayesnec` it is possible to fit a single model, custom model-set, specific model-set or all of the available models. When multiple models are specified the `bnec` function returns a model weighted average estimate of predicted posterior values. A range of support functions and methods are also included to work with the returned single, or multi- model objects that allow extraction of raw, or model averaged predicted, *ECx*, *NEC* and *NSEC* values and to interrogate the fitted model or model-set. The statistical methods used mean that the uncertainty in derived threshold values can be robustly quantified, including uncertainty in individual model fits to the underlying observed data, as well as uncertainty in the model functional form.

# Statement of need

Statement of need

# Technical details and Usage

Technical details

## Model specification

Model spec

# Discussion and caveats

Discussion

# Future directions

Future direction
  
# Acknowledgements

Acknowledge


# References
