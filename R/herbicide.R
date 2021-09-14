#' Herbicide phytotoxicity data
#' 
#' Herbicide phytotoxicity dataset from \insertRef{Jones2003}{bayesnec}.
#' 
#' The response data (fvfm) Chlorophyll fluorescence measurements of symbiotic dinoflagellates
#' still in the host tissue of the coral (in hospite or in vivo) were measured using a DIVING-PAM chlorophyll
#' fluorometer (Walz) on vertical planes of tissue 2
#' to 3 cm above the base of the corals, using either a
#' 6 mm (Acropora formosa) or 2 mm (Seriatopora hystrix)
#' fibre-optic probe. Parameters measured were the
#' maximum potential quantum yield (Fv/Fm).
#' 
#' Additional information on each of the herbicides included is available from the original 
#' publication \insertRef{Jones2003}{bayesnec}
#' 
#' 
#' The columns are as follows:
#' 
#' \describe{ 
#'\item{name}{The herbicide (chr).}
#'\item{conc}{The treatment concentration in $\mu$g L^-1^ (dbl).}
#'\item{fvfm}{Maximum effective quantum yield (dbl).}
#' }
#' 
#' @name herbicide
#' @docType data
#' @format An object of class `data.frame` with 580 rows and 3 columns.
#' @keywords datasets
#' @examples
#' 
#' head(herbicide)
#' 
"herbicide"
