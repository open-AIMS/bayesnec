% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_priors.R
\name{check_priors}
\alias{check_priors}
\title{Plots the prior and posterior parameter probability densities from an
object of class \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.}
\usage{
check_priors(object, filename = NA, ask = TRUE)
}
\arguments{
\item{object}{An object of class \code{\link{bayesnecfit}} or
\code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.}

\item{filename}{An optional \code{\link[base]{character}} vector to be used
as a pdf filename in the case of a \code{\link{bayesmanecfit}}. Any non
empty character string will indicate the user wants to save the plots.}

\item{ask}{Should the user be asked to hit enter for next page? Defaults to
\code{TRUE}. Only relevant if \code{object} is of class
\code{\link{bayesmanecfit}}.}
}
\value{
A plot of the prior and posterior parameter probability densities.
}
\description{
Plots the prior and posterior parameter probability densities from an
object of class \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
}
\examples{
\dontrun{
library(bayesnec)
data(manec_example)
check_priors(manec_example)
}

}
\seealso{
\code{\link{bnec}}
}
