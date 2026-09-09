# Builds the `lum31` dataset: acute copper and zinc toxicity tests using the
# Lum-31 bioluminescent bacterial assay of Luter et al. (2025).
#
# Source workbooks are held in data-raw/lum31/ and are not distributed with
# the package (.gitignore). They are the AIMS data repository deposit
# accompanying the paper; the published analysis code is at
# https://github.com/open-AIMS/Lum31-tox-assays.
#
# One sheet per dated batch of assays. Column `x` is the measured dissolved
# metal concentration in mg/L; the remaining columns are `<plate>_y<minutes>`
# holding raw luminescence. Each sheet is 11 concentrations x 4 replicate
# wells = 44 rows, and the four wells of a row block are read again at each
# exposure time, so a row of a sheet is one physical well.
#
# The 5 minute readings are discarded: the workbook metadata sheet states that
# only the 15 and 30 minute data were used in the analyses.
#
# The chronic workbook and the Lum-31 versus Microtox comparison workbook are
# not read here. They have different designs and would have to be shipped as
# separate objects; no vignette section needs them yet.
#
# readxl is used only by this script. data-raw is not built or checked, so it
# is deliberately not added to DESCRIPTION.

library(dplyr)
library(tidyr)
library(usethis)

LUM31_DIR <- "data-raw/lum31"

ACUTE_FILES <- c(
  Cu = "Cu Acute tests_measured_AIMSrepository.xlsx",
  Zn = "Zn Acute tests_measured_AIMSrepository.xlsx"
)

# Exposure times retained, per the workbook metadata sheet.
RETAINED_MINUTES <- c(15L, 30L)

# --- 1. Read and reshape ----------------------------------------------------
# `well` is the row of the sheet, which is the physical well: the same well is
# read at 5, 15 and 30 minutes, so row identity has to survive the pivot. It is
# numbered within the plate rather than labelled with its concentration, which
# would make an unwieldy label out of values such as 15.1293414634146.

read_acute <- function(toxicant) {
  path <- file.path(LUM31_DIR, ACUTE_FILES[[toxicant]])
  sheets <- setdiff(readxl::excel_sheets(path), "file metadata")
  out <- lapply(sheets, function(sheet) {
    raw <- readxl::read_excel(path, sheet = sheet)
    raw |>
      dplyr::mutate(well_index = dplyr::row_number()) |>
      tidyr::pivot_longer(
        cols = tidyselect::matches("_y[0-9]+$"),
        names_to = c("plate_label", "minutes"),
        names_pattern = "^(.*)_y([0-9]+)$",
        values_to = "rlu"
      ) |>
      dplyr::transmute(
        toxicant = toxicant,
        batch = sheet,
        plate_label = plate_label,
        well_index = well_index,
        minutes = as.integer(minutes),
        # `conc` must not be integer: bnec() refuses an integer predictor
        # (R/set_distribution.R). These values are decimal, but reading a
        # whole-numbered column from a spreadsheet would give integer.
        conc = as.numeric(x),
        rlu = as.numeric(rlu)
      )
  })
  dplyr::bind_rows(out)
}

long <- dplyr::bind_rows(lapply(names(ACUTE_FILES), read_acute)) |>
  dplyr::filter(minutes %in% RETAINED_MINUTES)

# Plate labels repeat between sheets -- Rep1B is a copper plate on both 19Mar24
# and 28Mar24 -- so a plate identifier has to include the toxicant and batch.
long <- long |>
  dplyr::mutate(
    plate = paste(toxicant, batch, plate_label),
    well = paste0(plate, "-w", sprintf("%02d", well_index))
  )

# --- 2. Censoring -----------------------------------------------------------
# Readings were blank-corrected against seawater blanks, so a reading below the
# blank becomes negative and was replaced by zero in the source workbook. 386
# of the 2904 retained readings are exactly zero on that account, and one
# (Zn 5Apr24, Rep4B, 15 min, top concentration) is -123 and was not floored.
# `rlu` is left exactly as recorded, including that negative value, because it
# is the direct evidence that a zero here is a floored negative rather than an
# absence of light -- which is the argument for left-censoring rather than a
# hurdle.
#
# Gamma requires a strictly positive response and brms refuses the fit at data
# validation otherwise, so the censoring bound cannot be zero. It is the
# smallest positive reading on the plate: auto-scale gain is set per plate, so
# the value at which the instrument stops resolving is a plate property. The
# bound is pooled over the two exposure times because the two reads of a plate
# share a gain -- the 30 to 15 minute control-well ratio is 0.75 to 0.96 across
# all 33 plates, against a 2.8-fold spread in gain between plates read on the
# same day.
lum31 <- long |>
  dplyr::group_by(plate) |>
  dplyr::mutate(
    censoring = ifelse(rlu <= 0, "left", "none"),
    rlu_cens = ifelse(rlu <= 0, min(rlu[rlu > 0]), rlu)
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(toxicant, batch, plate, minutes, conc, well) |>
  dplyr::transmute(
    toxicant = factor(toxicant),
    batch = factor(batch, levels = c("19Mar24", "28Mar24", "5Apr24",
                                     "23Apr24", "1Oct24")),
    plate = factor(plate),
    well = factor(well),
    minutes = minutes,
    conc = conc,
    rlu = rlu,
    censoring = censoring,
    rlu_cens = rlu_cens
  ) |>
  as.data.frame()

# --- 3. Invariants ----------------------------------------------------------
stopifnot(
  !anyNA(lum31),
  # 11 concentrations x 4 wells on every plate, at each exposure time
  all(table(lum31$plate, lum31$minutes) == 44),
  all(table(lum31$plate, lum31$minutes, lum31$conc)[
    cbind(as.character(lum31$plate), as.character(lum31$minutes),
          as.character(lum31$conc))] == 4),
  # each physical well is read at both retained exposure times
  all(table(lum31$well) == 2),
  # the predictor must not be integer; bnec() refuses one
  is.numeric(lum31$conc), !is.integer(lum31$conc),
  # censoring flags exactly the readings that were floored or negative
  all((lum31$censoring == "left") == (lum31$rlu <= 0)),
  # the censored response is strictly positive, as Gamma requires
  all(lum31$rlu_cens > 0),
  # an uncensored reading is unchanged
  all(lum31$rlu_cens[lum31$censoring == "none"] ==
        lum31$rlu[lum31$censoring == "none"])
)

print(with(lum31, table(toxicant, minutes)))
print(with(lum31, table(toxicant, minutes, censoring == "left")[, , "TRUE"]))
print(with(lum31, tapply(conc, toxicant, function(x) length(unique(x)))))
print(nlevels(lum31$plate))

usethis::use_data(lum31, overwrite = TRUE)
