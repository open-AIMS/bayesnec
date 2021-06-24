<!-- README.md is generated from README.Rmd. Please edit that file -->

bayesnec <img src="man/figures/logo.png" width = 180 alt="bayesnec Logo" align="right" />
=========================================================================================

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R build
status](https://github.com/open-AIMS/bayesnec/workflows/R-CMD-check/badge.svg)](https://github.com/open-AIMS/bayesnec/actions)
[![Codecov test
coverage](https://codecov.io/gh/open-aims/bayesnec/branch/master/graph/badge.svg)](https://codecov.io/gh/open-aims/bayesnec?branch=master)
![pkgdown](https://github.com/open-AIMS/bayesnec/workflows/pkgdown/badge.svg)
[![license](https://img.shields.io/badge/license-GPL--2-lightgrey.svg)](https://choosealicense.com/)
[![packageversion](https://img.shields.io/badge/Package%20version-1.0.1-orange.svg)](https://github.com/open-AIMS/bayesnec/blob/master/DESCRIPTION)
[![Ask Us Anything
!](https://img.shields.io/badge/Ask%20us-anything-1abc9c.svg)](https://github.com/open-AIMS/bayesnec/issues/new)
![Open Source
Love](https://badges.frapsoft.com/os/v2/open-source.svg?v=103)
<!-- badges: end -->

Warning
-------

**This package is a work in progress. So, for the moment, we advise
cautionary usage as some of the functionalities may change or become
deprecated in future versions.**

Overview
--------

`bayesnec` is a No-Effect-Concentration estimation package that uses
[brms](https://github.com/paul-buerkner/brms) to fit
concentration(dose)-response data using Bayesian methods for the purpose
of estimating both ECX values, but more particularly NEC. Please see
`?bnec` for a more detailed help file.

Installation
------------

At this stage `bayesnec` is not hosted on the CRAN R package network. An
alternative method of installation is to use the R `devtools` package.

R `devtools` can be installed using the following command:

    install.packages("devtools")

After `devtools` has been installed `bayesnec` can be installed directly
from GitHub using the following command:

    devtools::install_github("https://github.com/open-AIMS/bayesnec")

Usage
-----

Usage and further information about `bayesnec` can be seen on the
[project page](https://open-aims.github.io/bayesnec/) and the
[vignettes](https://open-aims.github.io/bayesnec/articles/). Help files
for the individual functions can be found on the [reference
page](https://open-aims.github.io/bayesnec/reference/).

Further Information
-------------------

`bayesnec` is provided by the [Australian Institute of Marine
Science](https://www.aims.gov.au) under the GPL-2 License
([GPL-2](https://opensource.org/licenses/GPL-2.0)).
