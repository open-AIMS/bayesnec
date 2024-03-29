% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_chains.R
\name{check_chains}
\alias{check_chains}
\alias{check_chains.bayesnecfit}
\alias{check_chains.bayesmanecfit}
\title{Checking chain convergence}
\usage{
check_chains(x, ...)

\method{check_chains}{bayesnecfit}(x, ...)

\method{check_chains}{bayesmanecfit}(x, filename = NA, ...)
}
\arguments{
\item{x}{An object of class \code{\link{bayesnecfit}} or
\code{\link{bayesmanecfit}} as returned by \code{\link{bnec}}.}

\item{...}{Unused.}

\item{filename}{An optional \code{\link[base]{character}} vector to be used
as a pdf filename in the case of a \code{\link{bayesmanecfit}}. Any non
empty character string will indicate the user wants to save the plots.}
}
\value{
No return value, generates a plot or writes a pdf to file.
}
\description{
Plots HMC chains for a \code{\link{bayesnecfit}} or
\code{\link{bayesmanecfit}} model fit as returned by \code{\link{bnec}}.
}
\examples{
\dontrun{
library(bayesnec)
check_chains(manec_example)
nec4param <- pull_out(manec_example, model = "nec4param")
check_chains(nec4param)
}
}
