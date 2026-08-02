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
#' @name "beta_binomial"
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

#' Chronic snail toxicity data with mortality
#'
#' Growth and survival of the tropical marine snail \emph{Nassarius dorsatus}
#' in four chronic toxicity tests, reconstructed to one row per individual
#' snail that entered the experiment. The test method follows Trenfield et al.
#' (2016), although that publication does not describe the mortality scenario
#' these data are used to illustrate.
#'
#' This dataset exists to demonstrate the hurdle concentration-response models
#' (see \code{\link{bnec_hurdle}} and \code{vignette("example6")}). Exposure
#' both suppresses growth in the snails that live and kills others, so an
#' analysis of the survivors alone silently conditions on survival.
#'
#' \bold{Mortality is recorded as a zero growth value}, with \code{alive}
#' giving the same information as an explicit indicator. In the source records
#' deaths were not coded that way: they appeared as one of four traces --- a
#' sentinel value far below the real data, a blank replicate within a tank that
#' ran, a tank missing from a treatment that ran, or a treatment missing from
#' the file entirely because nothing in it survived. Reconstructing them is
#' part of the worked example.
#'
#' \bold{Growth is reported exactly as measured}, including four slightly
#' negative values among survivors. The assay is destructive, so growth is
#' obtained by referencing each individual to a baseline mean rather than to
#' its own starting size, and that referencing carries enough error to put a
#' strongly suppressed individual below zero. They are retained rather than
#' floored because flooring is the practice \code{vignette("example6")} argues
#' against; a Gamma fit requires handling them explicitly.
#'
#' \bold{Contaminant B is the most severely affected test.} Only 63 of the 284
#' snails exposed survived, and none at all above the third of its fourteen
#' doses; the treatments above that are absent from the source records rather
#' than recorded as total mortality, and are reinstated here on the dose ladder
#' shared with contaminant C. Because a dose at which nothing survived
#' contributes no rows, a survivors-only analysis of B would contain just four
#' of its fourteen doses and would not register that the contaminant is lethal
#' beyond them. It is the clearest case in these data for modelling the exposed
#' cohort rather than the survivors.
#'
#' The contaminants are anonymised and the dose units are not stated. Dose
#' values are the tested nominal concentrations on an undisclosed scale, so
#' their relative spacing --- and therefore every toxicity estimate derived
#' from them --- is preserved.
#'
#' The columns are as follows:
#'
#' \describe{
#' \item{contaminant}{The test, anonymised as A--D (fct).}
#' \item{dose}{Nominal exposure concentration, undisclosed units (dbl).}
#' \item{tank}{Replicate tank identifier, unique within contaminant (chr).}
#' \item{alive}{1 if the snail survived and was measured, 0 if it died (int).}
#' \item{growth}{Growth over the exposure, 0 for snails that died (dbl).}
#' \item{record}{How this snail's fate was established (fct), one of
#'   \code{"measured"}, \code{"sentinel_code"}, \code{"blank_rep"},
#'   \code{"absent_tank"} or \code{"absent_dose"}. The last two are snails that
#'   do not appear in the source records at all and were reinstated from the
#'   test design.}
#' }
#'
#' Between 79\% and 95\% of the deaths in each test fall into the two
#' \code{"absent_"} categories, so most of the mortality is reinstated rather
#' than read off the source file. This is a recording convention rather than a
#' defect: a treatment in which nothing survived was simply not written down,
#' and reinstating it recovers information rather than inventing it. Omitting
#' those rows biases survival thresholds low, because the last dose the records
#' do contain is one where some animals were still alive.
#'
#' \code{record} is retained so that the distinction stays visible and can be
#' tested --- \code{vignette("example6")} refits contaminant B without the
#' reinstated doses and shows which parts of the reconstruction change the
#' answer and which do not.
#'
#' @name nassarius
#' @docType data
#' @format An object of class `data.frame` with 1208 rows and 5 columns.
#' @keywords datasets
#' @examples
#' head(nassarius)
#' with(nassarius, table(contaminant, alive))
#'
#' @references
#' Trenfield MA, van Dam JW, Harford AJ, Parry D, Streten C, Gibb K,
#' van Dam RA (2016) A chronic toxicity test for the tropical marine snail
#' Nassarius dorsatus to assess the toxicity of copper, aluminium, gallium,
#' and molybdenum. Environmental Toxicology and Chemistry, 35(7): 1788-1795.
#' doi: 10.1002/etc.3331.
#'
NULL
