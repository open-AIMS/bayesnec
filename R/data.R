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

#' Herbicide phytotoxicity data
#'
#' Herbicide phytotoxicity dataset from Jones & Kerswell (2003).
#'
#' The response data (Fv/Fm) Chlorophyll fluorescence measurements of symbiotic
#' dinoflagellates still in the host tissue of the coral
#' (in hospite or in vivo) were measured using a DIVING-PAM chlorophyll
#' fluorometer (Walz) on vertical planes of tissue 2 to 3 cm above the base of
#' the corals, using either a 6 mm (Acropora formosa) or 2 mm
#' (Seriatopora hystrix) fibre-optic probe. Parameters measured were the
#' maximum potential quantum yield (Fv/Fm).
#'
#' Additional information on each of the herbicides included is available from
#' the original publication Jones & Kerswell (2003).
#'
#' The columns are as follows:
#'
#' \describe{ 
#'\item{herbicide}{The herbicide (chr).}
#'\item{concentration}{The treatment concentration in µg / L (dbl).}
#'\item{fvfm}{Maximum effective quantum yield (dbl).}
#' }
#'
#' @name herbicide
#' @docType data
#' @format An object of class `data.frame` with 580 rows and 3 columns.
#' @keywords datasets
#' @examples
#' head(herbicide)
#'
#' @references
#' Jones RJ, Kerswell AP (2003) Phytotoxicity of Photosystem II (PSII)
#' herbicides to coral. Marine Ecology Progress Series, 261: 149-159.
#' doi: 10.3354/meps261149.
#'
NULL
