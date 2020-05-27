
#require(rstantools)
#rstan_create_package()


library(devtools)
library(roxygen2)
library(knitr)
library(R.rsp)
library(digest)


devtools::document()

use_package("bmrs")

build()

