% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prebayesnecfit-class.R
\docType{class}
\name{prebayesnecfit-class}
\alias{prebayesnecfit-class}
\alias{prebayesnecfit}
\title{Class \code{prebayesnecfit} of models fitted with the \pkg{brms} package}
\description{
This is an intermediate class that was created to make both
\code{\link{bayesnecfit}} and \code{\link{bayesmanecfit}} objects lighter
to handle. It contains the original \code{\link[brms]{brmsfit}}
fitted object, name of non-linear model that was fitted, the list of
initialisation values applied, and the validated
\code{\link{bayesnecformula}}.
}
\details{
See \code{methods(class = "prebayesnecfit")} for an overview of
available methods.
}
\section{Slots}{

\describe{
\item{\code{fit}}{The fitted Bayesian model of class \code{\link[brms]{brmsfit}}.}

\item{\code{model}}{A \code{\link[base]{character}} string indicating the name of
the fitted model.}

\item{\code{init}}{A \code{\link[base]{list}} containing the initialisation values
for to fit the model.}

\item{\code{bayesnecformula}}{An object of class \code{\link{bayesnecformula}} and
\code{\link[stats]{formula}}.}
}}

\seealso{
\code{\link{bayesnec}},
\code{\link{bnec}},
\code{\link{bayesnecfit}},
\code{\link{bayesmanecfit}},
\code{\link{bayesnecformula}}
}
