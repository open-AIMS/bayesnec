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
[![CRAN
Version](https://www.r-pkg.org/badges/version/bayesnec)](https://cran.r-project.org/package=bayesnec)
[![Downloads](https://cranlogs.r-pkg.org/badges/bayesnec?color=brightgreen)](https://CRAN.R-project.org/package=bayesnec)
[![license](https://img.shields.io/badge/license-GPL--2-lightgrey.svg)](https://choosealicense.com/)
[![Ask Us Anything
!](https://img.shields.io/badge/Ask%20us-anything-1abc9c.svg)](https://github.com/open-AIMS/bayesnec/issues/new)
![Open Source
Love](https://badges.frapsoft.com/os/v2/open-source.svg?v=103)
<!-- badges: end -->

Overview
--------

`bayesnec` is a No-Effect-Concentration estimation package that uses
[`brms`](https://github.com/paul-buerkner/brms) to fit
concentration(dose)-response data using Bayesian methods for the purpose
of estimating both ECX values, but more particularly NEC. Please see
`?bnec` for a more detailed help file.

Installation
------------

To install the latest release version from CRAN use

    install.packages("bayesnec")

The current development version can be downloaded from GitHub via

    if (!requireNamespace("remotes")) {
      install.packages("remotes")
    }
    remotes::install_github("open-aims/bayesnec", ref = "dev")

Because `bayesnec` is based on `brms` and [Stan](https://mc-stan.org/),
a C++ compiler is required. The program
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) comes with a
C++ compiler for Windows. On Mac, you should install Xcode. See the
prerequisites section on this
[link](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started) for
further instructions on how to get the compilers running.

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
