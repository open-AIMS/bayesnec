% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_priors.R
\name{check_priors.bayesnecfit}
\alias{check_priors.bayesnecfit}
\title{check_priors.bayesnecfit}
\usage{
\method{check_priors}{bayesnecfit}(object, ...)
}
\arguments{
\item{object}{An object of class \code{\link{bayesnecfit}}
returned by \code{\link{bnec}}.}

\item{...}{Unused.}
}
\value{
A plot of the prior and posterior parameter probability densities
}
\description{
Plots the prior and posterior parameter probability densities from an
object of class \code{\link{bayesnecfit}}.
}
\examples{
library(bayesnec)
data(manec_example)
check_priors(manec_example)

}
\seealso{
\code{\link{bnec}}
}
