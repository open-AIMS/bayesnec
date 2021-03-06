#' Example data of non-linear decay
#'
#' A simulated dataset containing a series of response
#' measurements as a function of a concentration axis.
#' Data simulated by Diego Barneche.
#'
#' @format A data frame with 100 rows and 2 variables:
#' \itemize{
#'    \item x: Concentration (predictor) axis.
#'    \item y: Response.
#' }
#' @name nec_data
#' @docType data
NULL

#' Custom beta-binomial family
#'
#' @format An object of class \code{\link[brms]{customfamily}}
#'
#' @name beta_binomial2
#' @docType data
NULL

#' Example bayesmanecfit object
#'
#' @format An object of class \code{\link{bayesmanecfit}}. This was created
#' to reduce run time in examples and tests, and to give the user an example
#' to toy with. This was fitted to \code{\link{bayesnec}} built-in mock dataset
#' (see ?\code{\link{nec_data}}), using models "nec4param" and "ecx4param".
#' The number of chains were set to 2 and number of iterations were 50 only
#' to make sure that package size was below 5 Mb. See help files for function
#' \code{\link{bnec}} and class \code{\link{bayesmanecfit}} for details.
#'
#' @source Code used to generate these models can be downloaded from \url{https://github.com/open-AIMS/bayesnec/blob/master/data-raw/manec_example.R}
#'
#' @name manec_example
#' @docType data
NULL
