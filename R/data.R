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
#' @format An object of class `data.frame` with 1208 rows and 6 columns.
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

#' Marine microalgal growth inhibition data with censored cell counts
#'
#' Final cell density and average specific growth rate for two marine
#' microalgae exposed to each of two contaminants, consolidated from four
#' growth inhibition tests. The species are the symbiotic dinoflagellate
#' \emph{Cladocopium proliferum} (\code{"c_proliferum"}) and the cryptophyte
#' \emph{Rhodomonas salina} (\code{"r_salina"}). Each test ran a dilution
#' series against an unexposed control with five replicate cultures per
#' exposure level, and twice that at the control.
#'
#' This dataset exists to demonstrate two data-preparation problems that arise
#' before any model is fitted: what a response owes to the control mean it is
#' referenced against, and what to do with counts that fell below the
#' resolution the method could record.
#'
#' \bold{Cell density is counted to a resolution of 10.} A recorded density of
#' \code{0} therefore means "fewer than 10 cells", not "no cells". The growth
#' rate such a culture implies is bounded rather than undefined, and
#' left-censoring at the growth rate implied by a density of 10 is the
#' treatment the recording process actually justifies.
#'
#' \bold{Growth rate is reported exactly as supplied,} including substituted
#' values, with \code{sgr_source} marking them. Where density was recorded as
#' \code{0} the source set the growth rate to \code{0}. That substitution
#' repays a look: a growth rate of \code{0} means "no change", and it sits in
#' the middle of the observed range --- above every genuinely negative value in
#' the same test --- so a culture that was lost entirely is recorded as
#' \emph{less} affected than one that was merely declining. Correcting it in
#' the shipped data would remove the example, and substitution at a boundary is
#' the practice \code{vignette("example6")} argues against.
#'
#' \bold{The two species grow on very different scales,} which is why they were
#' run over different exposure durations --- seven days for
#' \emph{C. proliferum} against three for \emph{R. salina}. Symbiodiniaceae
#' divide slowly, and control cultures multiplied only about 2--3 fold over the
#' whole exposure, against about 17--73 fold for \emph{R. salina}; control
#' growth rates differ by roughly an order of magnitude between them. Only
#' \emph{R. salina} was driven below the counting resolution, in 16 of its 85
#' cultures under contaminant A and 4 of its 70 under contaminant B;
#' \emph{C. proliferum} never was.
#'
#' The contrast is useful in itself: a slow grower gives a small dynamic range
#' between control and complete inhibition, so the same absolute error in a
#' count is a much larger error in growth rate.
#'
#' Wherever a density was measured, the growth rate satisfies
#' \code{sgr = (log(density) - log(density_initial)) / days}. That holds
#' exactly for contaminant A, and to the three decimals the source rounded to
#' for contaminant B --- itself a milder instance of the same recording
#' problem. \code{density_initial} and \code{days} are supplied so that growth
#' rates can be recomputed and the censoring bound derived.
#'
#' The contaminants are anonymised and the exposure units are not stated. Dose
#' values are the tested nominal concentrations on an undisclosed scale, so
#' their relative spacing --- and therefore every toxicity estimate derived
#' from them --- is preserved. \bold{The two contaminants are on different
#' undisclosed scales and their dose values are not comparable with each
#' other.}
#'
#' The columns are as follows:
#'
#' \describe{
#' \item{species}{Test species (fct), \code{"c_proliferum"}
#'   (\emph{Cladocopium proliferum}) or \code{"r_salina"}
#'   (\emph{Rhodomonas salina}).}
#' \item{contaminant}{The contaminant, anonymised as A or B (fct).}
#' \item{dose}{Nominal exposure concentration, undisclosed units; 14 levels
#'   including the control for contaminant A, 13 for contaminant B (dbl).}
#' \item{dose_measured}{Measured exposure concentration, undisclosed units.
#'   Recorded for contaminant B only; \code{NA} throughout contaminant A
#'   (dbl).}
#' \item{density}{Final cell density, counted to a resolution of 10. A
#'   \code{0} means the count fell below that resolution (int).}
#' \item{sgr}{Average specific growth rate per day, exactly as supplied. A
#'   substituted \code{0} wherever \code{density} is \code{0} (dbl).}
#' \item{sgr_source}{How \code{sgr} arose (fct), either \code{"measured"} or
#'   \code{"substituted"}.}
#' \item{days}{Exposure duration in days (int).}
#' \item{density_initial}{Cell density at the start of the exposure (dbl).}
#' }
#'
#' @name alga
#' @docType data
#' @format An object of class `data.frame` with 310 rows and 9 columns.
#' @keywords datasets
#' @examples
#' head(alga)
#' with(alga, table(species, contaminant, sgr_source))
#'
NULL

#' Bioluminescent bacterial toxicity data with floored luminescence readings
#'
#' Raw luminescence of the marine bacterium Lum-31 exposed to dissolved copper
#' or zinc, from the acute toxicity tests of Luter et al. (2025). The assay
#' reads light output from a bacterial suspension in a microplate well; light
#' falls as the cells are inhibited, so luminescence is the response and no
#' separate endpoint is scored. The analysis code accompanying the paper is at
#' \url{https://github.com/open-AIMS/Lum31-tox-assays}.
#'
#' This dataset exists to demonstrate group-level terms (see
#' \code{vignette("example8")}). Its design supplies replication at three
#' nested scales --- four wells within a concentration (\code{conc_group}), 33
#' plates each spanning a whole dilution series (\code{plate}), and two
#' toxicants read at two exposure times --- so the same data support a
#' within-concentration group term, an across-concentration one, and a factor
#' covariate.
#'
#' The tests were run in five dated batches. A batch holds two to five plates,
#' each of which is a complete test: 11 measured concentrations, four replicate
#' wells at each, read at 15 and 30 minutes. Each row of the dataset is one
#' well at one exposure time, and \code{well} identifies the physical well, so
#' the two rows sharing a \code{well} level are the same suspension read twice.
#'
#' Concentrations are measured dissolved metal rather than nominal, and each
#' batch was dosed and assayed separately, so the concentration values differ
#' between batches. There are 53 distinct copper concentrations and 54 zinc
#' concentrations across the whole dataset but only 11 within any one plate,
#' and plates cannot be compared point by point.
#'
#' Readings were blank-corrected against wells of artificial seawater, so a
#' well emitting less light than the blank gives a negative value. Such a value
#' was replaced by zero in the source records, and 386 of the 2904 readings are
#' zero on that account; one further reading escaped the replacement and is
#' recorded as \code{-123}. \code{rlu} reports every reading as it stands in
#' the source, negative value included, because it is the direct evidence that
#' a zero here is a floored negative rather than an absence of light. The
#' generating process at the floor is measurement noise about a small positive
#' value, which makes these readings left-censored rather than the product of a
#' distinct zero-generating process.
#'
#' \code{censoring} and \code{rlu_cens} give that treatment in the form
#' \code{\link[brms]{brm}} expects, as \code{rlu_cens | cens(censoring)}. A
#' Gamma likelihood requires a strictly positive response and \code{brms}
#' refuses the fit at data validation otherwise, so the censoring bound cannot
#' be zero. The bound used is the smallest positive reading on the plate. It is
#' a plate property because the plate reader applies auto-scale gain
#' adjustment, and it is pooled over the two exposure times because the two
#' reads of one plate share a gain setting. No detection limit is recorded for
#' the instrument, so the bound is an inference from the readings rather than a
#' stated resolution, and estimates are more sensitive to it in the arms where
#' more readings reach the floor --- 28 per cent for copper at 30 minutes,
#' against 0.9 per cent for zinc at 15.
#'
#' Auto-scale gain also means absolute luminescence is not comparable between
#' plates. Median control-well luminescence differs by up to 2.8-fold between
#' plates read on the same day, and the October batch sits 4.6-fold below the
#' March batches. The published analysis removes this by dividing each plate by
#' the median of its four control wells; a group-level term on the control
#' parameter estimates the same per-plate factor and propagates its
#' uncertainty, which is what \code{vignette("example8")} compares.
#'
#' The columns are as follows:
#'
#' \describe{
#' \item{toxicant}{The metal tested (fct), \code{"Cu"} or \code{"Zn"}.}
#' \item{batch}{Date of the batch of assays (fct), five levels.}
#' \item{plate}{Plate identifier, unique across the dataset (fct), 16 copper
#'   plates and 17 zinc. Plate labels repeat between batches in the source
#'   workbooks, so the level combines toxicant, batch and plate label.}
#' \item{conc_group}{The four wells at one concentration on one plate (fct),
#'   numbered in ascending concentration within the plate. This is the
#'   within-concentration grouping factor: every level sits at a single
#'   concentration, so it cannot have a concentration-response curve of its
#'   own.}
#' \item{well}{Replicate well identifier, unique within plate (fct). Each level
#'   appears twice, once per exposure time. Within one toxicant-by-exposure-time
#'   subset a level has a single observation, so a group-level term on
#'   \code{well} there is an observation-level term rather than a grouping.}
#' \item{minutes}{Exposure time in minutes at which the plate was read (int),
#'   15 or 30. The source workbooks also hold a 5 minute read, which the
#'   published analysis did not use and which is not included here.}
#' \item{conc}{Measured dissolved metal concentration in mg/L (dbl).}
#' \item{rlu}{Raw luminescence in relative light units, exactly as recorded
#'   (dbl). Zero where a blank-corrected negative was replaced by zero, and
#'   \code{-123} in the one case where it was not.}
#' \item{censoring}{Censoring indicator (chr), \code{"left"} where \code{rlu}
#'   is zero or negative and \code{"none"} otherwise.}
#' \item{rlu_cens}{\code{rlu}, with every non-positive reading replaced by the
#'   smallest positive reading on the same plate (dbl). Strictly positive
#'   throughout, and the response to fit with \code{censoring}.}
#' }
#'
#' @name lum31
#' @docType data
#' @format An object of class `data.frame` with 2904 rows and 10 columns.
#' @keywords datasets
#' @examples
#' head(lum31)
#' with(lum31, table(toxicant, minutes, censoring))
#'
#' @references
#' Luter HM, Damjanovic K, Thomas MC, Fisher R, Hoj L, Negri AP (2025) A
#' bioluminescent bacterial toxicity assay for tropical marine environments.
#' Environmental Toxicology. doi: 10.1002/tox.70003.
#'
NULL
