<!-- README.md is generated from README.Rmd. Please edit that file -->

bayesnec <img src="man/figures/logo.png" width = 180 alt="bayesnec Logo" align="right" />
=========================================================================================

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/open-AIMS/bayesnec/workflows/R-CMD-check/badge.svg)](https://github.com/open-AIMS/bayesnec/actions)
![pkgdown](https://github.com/open-AIMS/bayesnec/workflows/pkgdown/badge.svg)
[![license](https://img.shields.io/badge/license-MIT%20+%20file%20LICENSE-lightgrey.svg)](https://choosealicense.com/)
[![packageversion](https://img.shields.io/badge/Package%20version-1.0-orange.svg)](commits/master)
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
`?bayesnec` for more details.

Installation
------------

At this stage `bayesnec` is not hosted on CRAN R package network. An
alternative method of installation is to use the R `devtools` package.

R `devtools` can be installed using the following command:

    install.packages("devtools")

After `devtools` has been installed `bayesnec` can be installed directly
from GitHub using the following command:

    devtools::install_github("https://github.com/open-AIMS/bayesnec")

Usage
-----

Usage and further information about `bayesnec` can be seen on the
[project page](https://open-aims.github.io/bayesnec) and the
[vignettes](https://open-aims.github.io/bayesnec/articles/). Help files
for the individual functions can be found on the [reference
page](https://open-aims.github.io/bayesnec/reference/).

Further Information
-------------------

`bayesnec` is provided by the [Australian Institute of Marine
Science](https://www.aims.gov.au) under the MIT License
([MIT](http://opensource.org/licenses/MIT)).

AIMS R package logos
--------------------

Our R package logos use a watercolour map of Australia, obtained with
the [ggmap](https://CRAN.R-project.org/package=ggmap) R package, which
downloads original map tiles provided by [Stamen
Design](http://stamen.com), under [CC BY
3.0](http://creativecommons.org/licenses/by/3.0), with data from
[OpenStreetMap](http://openstreetmap.org), under [CC BY
SA](http://creativecommons.org/licenses/by-sa/3.0).