# Builds the `nassarius` dataset: four chronic toxicity tests on the tropical
# marine snail Nassarius dorsatus, reconstructed to one row per individual
# snail exposed.
#
# The source file is not distributed with the package. It holds one row per
# tank and one column per snail within that tank, with mortality recorded in
# four different ways rather than as zeros -- which is the whole reason this
# reconstruction is worth documenting. See vignette("example6").
#
# The contaminants are anonymised as A-D and the dose units are deliberately
# not stated; the dose values are the tested nominal concentrations on an
# undisclosed scale, so relative spacing (and therefore every toxicity
# estimate) is preserved while the identity of the tests is not.

library(dplyr)
library(tidyr)
library(usethis)

# Deliberately read from ignore/, which is git-ignored. The source file carries
# the real contaminant identities and dose units, so committing it anywhere in
# the repository would undo the anonymisation this script performs.
raw <- read.csv("ignore/collated_WET_data.csv", fileEncoding = "UTF-8-BOM")

# One row per cell of the tank x rep grid. A cell is a snail position, which
# may or may not have held a snail.
long <- raw |>
  tidyr::pivot_longer(tidyselect::starts_with("Rep"), names_to = "rep",
                      values_to = "y") |>
  dplyr::rename(round = Round, tank = ID, dose = PercentPW)

# --- 1. Sentinel death codes ------------------------------------------------
# Each round uses one constant far below the data to mean "died": -46.35,
# -34.61, -38.40. The cut is -30 because the lowest real measurement is -7.29,
# so the 23-unit gap makes the classification unambiguous.
SENTINEL_CUT <- -30

# Round 2's sentinels in its lowest three doses are a transcription error, not
# deaths (confirmed against the lab records). Its 0.10 dose sentinels are real.
r2_error <- with(long, round == 2 & dose <= 0.05 & !is.na(y) & y <= SENTINEL_CUT)
long <- long[!r2_error, ]

long <- long |>
  dplyr::mutate(
    dead_coded = !is.na(y) & y <= SENTINEL_CUT
  )

# --- 2. Blank reps in a present tank ----------------------------------------
# An empty cell means either that the tank held fewer snail positions than the
# widest tank in the file (no snail: drop) or that a snail present at the start
# gave no measurement (death: keep as a zero). The two are separated by the
# number of positions the round actually used, which is the modal filled count
# per tank within that round.
# A sentinel death code is itself a filled cell, so counting non-NA is enough;
# adding dead_coded on top would count those cells twice and inflate n_pos.
positions <- long |>
  dplyr::group_by(round, tank, dose) |>
  dplyr::summarise(filled = sum(!is.na(y)), .groups = "drop") |>
  dplyr::group_by(round) |>
  dplyr::summarise(n_pos = max(filled), .groups = "drop")

long <- long |>
  dplyr::left_join(positions, by = "round") |>
  dplyr::group_by(round, tank, dose) |>
  dplyr::mutate(
    rep_i = as.integer(sub("Rep", "", rep)),
    dead_blank = is.na(y) & rep_i <= n_pos
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(y) | dead_blank)

# --- 3 & 4. Absent tanks, and doses absent altogether -----------------------
# A treatment that ran with fewer tanks than the round's standard lost the
# missing tanks entirely; a treatment that does not appear at all was run and
# killed everything. Both are reinstated as fully dead tanks. Rounds are
# assumed to have been taken to a common top dose of 20, and round 2 is known
# to have had no survivors from 0.30 upward.
TOP_DOSE <- 20

tanks_per_dose <- long |>
  dplyr::group_by(round, dose) |>
  dplyr::summarise(n_tanks = dplyr::n_distinct(tank), .groups = "drop")

# The standard tank count is the modal count across the round's non-control
# doses; controls are replicated more heavily and would bias a mean.
standard_tanks <- tanks_per_dose |>
  dplyr::filter(dose > 0) |>
  dplyr::group_by(round) |>
  dplyr::summarise(n_std = as.integer(names(which.max(table(n_tanks)))),
                   .groups = "drop")

# Doses that were run but are absent from the file. Round 2 shares round 3's
# ladder exactly over the doses both recorded, so round 3's ladder is used to
# fill in round 2's missing rungs.
r3_ladder <- sort(unique(long$dose[long$round == 3]))
missing_doses <- dplyr::bind_rows(
  data.frame(round = 1, dose = TOP_DOSE),
  data.frame(round = 2, dose = c(r3_ladder[r3_ladder >= 0.30], TOP_DOSE)),
  data.frame(round = 3, dose = TOP_DOSE),
  data.frame(round = 4, dose = TOP_DOSE)
)

dead_rows <- dplyr::bind_rows(
  # tanks missing from a dose that is present
  tanks_per_dose |>
    dplyr::left_join(standard_tanks, by = "round") |>
    dplyr::filter(n_tanks < n_std) |>
    dplyr::mutate(n_dead_tanks = n_std - n_tanks, record = "absent_tank") |>
    dplyr::select(round, dose, n_dead_tanks, record),
  # doses missing altogether
  missing_doses |>
    dplyr::left_join(standard_tanks, by = "round") |>
    dplyr::mutate(n_dead_tanks = n_std, record = "absent_dose") |>
    dplyr::select(round, dose, n_dead_tanks, record)
) |>
  dplyr::left_join(positions, by = "round") |>
  dplyr::rowwise() |>
  dplyr::reframe(round = round, dose = dose,
                 tank = paste0("inferred", seq_len(n_dead_tanks)),
                 n_pos = n_pos, record = record) |>
  tidyr::uncount(n_pos) |>
  dplyr::mutate(y = NA_real_, dead_coded = FALSE, dead_blank = TRUE)

# --- assemble ---------------------------------------------------------------
nassarius <- dplyr::bind_rows(
  long |>
    dplyr::mutate(record = dplyr::case_when(
      dead_coded ~ "sentinel_code",
      dead_blank ~ "blank_rep",
      TRUE       ~ "measured")) |>
    dplyr::select(round, dose, tank, y, dead_coded, dead_blank, record),
  dead_rows |> dplyr::select(round, dose, tank, y, dead_coded, dead_blank, record)
) |>
  dplyr::mutate(
    alive = as.integer(!(dead_coded | dead_blank)),
    # How this row's fate was established. The two "absent_" levels are rows
    # that do not appear in the source records at all and were reinstated from
    # the test design; keeping the distinction lets a user see how much of the
    # mortality signal is inferred rather than observed.
    record = factor(record, levels = c("measured", "sentinel_code",
                                       "blank_rep", "absent_tank",
                                       "absent_dose")),
    # Growth is retained exactly as measured, including the four slightly
    # negative values. Flooring them here would push a data-preparation
    # decision into the shipped data, which is the practice vignette
    # "example6" argues against.
    growth = ifelse(alive == 1, y, 0),
    contaminant = factor(LETTERS[round])
  ) |>
  dplyr::arrange(contaminant, dose, tank) |>
  dplyr::mutate(tank = paste0(contaminant, "-", match(paste(contaminant, dose, tank),
                                                      unique(paste(contaminant, dose, tank))))) |>
  dplyr::select(contaminant, dose, tank, alive, growth, record) |>
  as.data.frame()

stopifnot(!anyNA(nassarius$growth), all(nassarius$growth[nassarius$alive == 0] == 0),
          !anyNA(nassarius$record),
          all(nassarius$record[nassarius$alive == 1] == "measured"))

print(with(nassarius, table(contaminant, alive)))
print(with(subset(nassarius, alive == 0), table(contaminant, droplevels(record))))

usethis::use_data(nassarius, overwrite = TRUE)
