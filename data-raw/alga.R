# Builds the `alga` dataset: growth inhibition tests on two marine microalgae
# exposed to two contaminants, consolidated from four source files into one
# frame. The species are Cladocopium proliferum (a symbiotic dinoflagellate,
# slow-growing, run over seven days) and Rhodomonas salina (a cryptophyte, run
# over three).
#
# The source files are not distributed with the package. They carry the real
# contaminant identities and exposure units, so committing them anywhere in the
# repository would undo the anonymisation this script performs. They live in
# ignore/, which is git-ignored.
#
# The contaminants are anonymised as A and B and the exposure units are
# deliberately not stated. Dose values are the tested nominal concentrations on
# an undisclosed scale, so relative spacing --- and therefore every toxicity
# estimate derived from them --- is preserved while the identity of the tests
# is not. The two contaminants are on different undisclosed scales and their
# dose values are not comparable with each other.
#
# This dataset exists for issues #173 (normalising to a control mean) and #181
# (censored responses). Two features make it useful for those, and both are
# preserved rather than cleaned away:
#
#   1. Cell density is counted to a resolution of 10, so a recorded density of
#      0 means "below the counting limit", not "no cells". Growth rate is
#      therefore left-censored at the value implied by a density of 10, not
#      undefined.
#   2. Where density was recorded as 0 the source set the growth rate to 0.
#      That is a substitution, and a badly placed one: a growth rate of 0 means
#      "no change", which sits in the *middle* of the observed range, above
#      every genuinely negative value. Total loss of the population is thereby
#      recorded as a less severe effect than a merely declining one.
#
# `sgr` is left exactly as supplied, including those substituted zeros, and
# `sgr_source` marks them. Correcting them here would push a data-preparation
# decision into the shipped data, which is the practice vignette("example6")
# argues against; the point is to have the problem available to demonstrate.

library(dplyr)
library(usethis)

SRC <- "ignore/alga_examples"

# Exposure duration in days, and the initial cell density each test started
# from. Both are recovered exactly from the source rather than supplied with
# it: where density > 0 the identity
#
#     sgr = (log(density) - log(density_initial)) / days
#
# holds, so regressing log(density) on sgr over the uncensored rows returns
# `days` as the slope and log(density_initial) as the intercept. For
# contaminant A that recovery is exact (residuals ~1e-9, every row implying the
# same initial density to the unit). For contaminant B the supplied growth
# rates are rounded to three decimals, so the implied initial density scatters
# by about 0.2% and the value below is the median.
DESIGN <- tibble::tribble(
  ~file,                ~species,        ~contaminant, ~days, ~density_initial,
  "c_proliferum.csv",   "c_proliferum",  "A",          7L,    7968,
  "r_salina.csv",       "r_salina",      "A",          3L,    3871,
  "c_proliferum2.csv",  "c_proliferum",  "B",          7L,    8681,
  "r_salina2.csv",      "r_salina",      "B",          3L,    3123
)

read_one <- function(file, species, contaminant, days, density_initial) {
  d <- read.csv(file.path(SRC, file), fileEncoding = "UTF-8-BOM")
  # The two contaminants were recorded under different column conventions:
  # contaminant A as a dilution with no measured value and no run column,
  # contaminant B with both nominal and measured concentrations. Normalise to
  # one shape here rather than carrying the difference into the dataset.
  if (contaminant == "A") {
    out <- data.frame(dose = d$percPW, dose_measured = NA_real_,
                      density = d$CellDensity, sgr = d$SGR)
  } else {
    density <- if ("CellDensity_Final" %in% names(d)) d$CellDensity_Final else d$CellDensity
    out <- data.frame(dose = as.numeric(d$mgL_nominal), dose_measured = d$mgL,
                      density = density, sgr = d$SGR)
  }
  # `Run` is dropped: it is constant within each species-by-contaminant test,
  # so it distinguishes nothing that `species` and `contaminant` do not, and it
  # is a laboratory identifier.
  out$species <- species
  out$contaminant <- contaminant
  out$days <- days
  out$density_initial <- density_initial
  out
}

alga <- DESIGN |>
  purrr::pmap(read_one) |>
  dplyr::bind_rows() |>
  dplyr::mutate(
    # A density of 0 is below the counting resolution, and the growth rate the
    # source reports for those rows is a substituted 0 rather than a
    # measurement. Everything else is the value computed from the count.
    sgr_source = factor(ifelse(density == 0, "substituted", "measured"),
                        levels = c("measured", "substituted")),
    species = factor(species),
    contaminant = factor(contaminant)
  ) |>
  dplyr::arrange(contaminant, species, dose) |>
  dplyr::select(species, contaminant, dose, dose_measured, density, sgr,
                sgr_source, days, density_initial) |>
  as.data.frame()

# --- checks -----------------------------------------------------------------

# The recorded growth rate must reproduce the count wherever it was measured.
# Contaminant A is exact; contaminant B is rounded to three decimals, so it is
# checked against the tolerance that rounding implies.
chk <- alga |>
  dplyr::filter(sgr_source == "measured") |>
  dplyr::mutate(sgr_implied = (log(density) - log(density_initial)) / days,
                err = abs(sgr - sgr_implied))
stopifnot(
  max(chk$err[chk$contaminant == "A"]) < 1e-6,
  max(chk$err[chk$contaminant == "B"]) < 5e-3
)

stopifnot(
  nrow(alga) == 310,
  !anyNA(alga$sgr), !anyNA(alga$density), !anyNA(alga$dose),
  # every substituted row is a zero density recorded as zero growth
  all(alga$density[alga$sgr_source == "substituted"] == 0),
  all(alga$sgr[alga$sgr_source == "substituted"] == 0),
  # dose_measured exists only for contaminant B
  all(is.na(alga$dose_measured[alga$contaminant == "A"])),
  !anyNA(alga$dose_measured[alga$contaminant == "B"]),
  # counting resolution
  all(alga$density %% 10 == 0)
)

print(with(alga, table(species, contaminant)))
print(with(alga, table(contaminant, species, sgr_source))[, , "substituted"])
cat("\ncontrol growth factor over the exposure:\n")
print(alga |>
        dplyr::filter(dose == 0) |>
        dplyr::group_by(species, contaminant) |>
        dplyr::summarise(fold = round(exp(mean(sgr) * dplyr::first(days)), 1),
                         .groups = "drop"))

usethis::use_data(alga, overwrite = TRUE)
