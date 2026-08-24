# Builds the `coral` dataset: a 7-day flow-through exposure of the reef-building
# coral Acropora millepora to 1-methylnaphthalene, under two light regimes.
#
# Source: Brinkman DL, Flores F, Luter HM, Nordborg FM, Brooks M, Parkerton TF,
# Negri AP (2023) Sensitivity of the Indo-Pacific coral Acropora millepora to
# aromatic hydrocarbons. Environmental Pollution 332:121963.
#
# The source workbook is not distributed with the package. It carries three
# exposures -- toluene, naphthalene and 1-methylnaphthalene -- one per sheet;
# only the 1-MN sheet is taken, because it is the only one that crosses the
# concentration series with BOTH light regimes and the five coral colonies, and
# so is the only one that carries every grouping structure vignette("example8")
# needs from a single dataset.
#
# The sheet embeds its own 32-line data dictionary in an unnamed trailing
# column. Every column description in R/data.R is taken from it verbatim rather
# than inferred, and the dictionary is the reason `mort` and `surv` can be
# documented as the two different measurements they are.

library(readxl)
library(dplyr)
library(usethis)

# Deliberately read from ignore/, which is git-ignored.
raw <- readxl::read_excel("ignore/Toxicity Test Data and WQ_brinkman2022.xlsx",
                          sheet = "1-mn")

# Columns 31-33 are the embedded dictionary and two empty spacers, not data.
raw <- raw[, 1:30]

day_cols <- function(prefix, new) {
  stats::setNames(paste0("T", 1:7, prefix), paste0(new, "_d", 1:7))
}

coral <- raw |>
  dplyr::rename(
    chamber = Chamber,
    colony = TileColour,
    nominal = Nominal,
    light = Light,
    rep = Rep,
    growth_rate = `Growth rate`,
    colour_d0 = T0_ColourScore,
    colour_d7 = T7_ColourScore,
    recovery_d7 = T7_recovery,
    !!!day_cols("d_conc", "conc"),
    !!!day_cols("_mort", "mort"),
    !!!day_cols("_surv", "surv")
  ) |>
  dplyr::mutate(
    # chamber is an identifier, not a quantity: 32 chambers, each holding one
    # concentration x light combination. Kept as a factor so that a group-level
    # term over it is accepted -- check_formula() rejects a numeric grouping
    # variable outright.
    chamber = factor(chamber),
    # "TileColour" in the source. The tiles are coloured to tell the colonies
    # apart, so the variable IS the coral colony -- the paper's methods say the
    # coloured glass tiles "were used to differentiate the coral colonies" --
    # and it is named for what it identifies rather than for how it was marked.
    colony = factor(colony, levels = c("red", "green", "grey", "purple",
                                       "blue")),
    light = factor(light, levels = c("PAR", "UV")),
    rep = factor(rep)
  ) |>
  dplyr::select(chamber, colony, nominal, light, rep,
                dplyr::starts_with("conc_"),
                dplyr::starts_with("mort_"),
                dplyr::starts_with("surv_"),
                recovery_d7, growth_rate, colour_d0, colour_d7) |>
  as.data.frame()

# --- checks the documentation depends on ------------------------------------
# One row per coral fragment, and the design is complete: 8 nominal
# concentrations x 2 light regimes x 4 replicates x ... = 160.
stopifnot(nrow(coral) == 160, ncol(coral) == 30)
stopifnot(nlevels(coral$chamber) == 32, nlevels(coral$colony) == 5)
stopifnot(all(sort(unique(coral$nominal)) == c(0, 5, 8, 13, 22, 36, 60, 100)))

# A chamber holds exactly one concentration and one light regime, which is what
# makes it a WITHIN-concentration grouping and therefore an ogl() term. A
# colony spans the whole series, which is what makes it an ACROSS-concentration
# grouping. This distinction is the organising idea of vignette("example8"), so
# it is asserted here rather than left to the prose.
per_chamber <- coral |>
  dplyr::group_by(chamber) |>
  dplyr::summarise(n_conc = dplyr::n_distinct(nominal),
                   n_light = dplyr::n_distinct(light), .groups = "drop")
stopifnot(all(per_chamber$n_conc == 1), all(per_chamber$n_light == 1))
per_colony <- coral |>
  dplyr::group_by(colony) |>
  dplyr::summarise(n_conc = dplyr::n_distinct(nominal), .groups = "drop")
stopifnot(all(per_colony$n_conc == 8))

# surv is proportion of live tissue by image area; mort is a percent mortality
# score. They are two different measurements of the same thing and do NOT
# satisfy surv == 1 - mort/100, which is worth pinning so nobody derives one
# from the other.
stopifnot(!isTRUE(all.equal(coral$surv_d4, 1 - coral$mort_d4 / 100)))

# Growth is missing exactly where the coral died outright, and is zero for 22
# corals that lived without measurably growing. Those 22 are the reason
# vignette("example8") cannot treat every zero as the hurdle process.
stopifnot(sum(is.na(coral$growth_rate)) == 60,
          sum(coral$growth_rate == 0, na.rm = TRUE) == 22)

cat("interior surv observations by day (0 < surv < 1):\n")
print(vapply(paste0("surv_d", 1:7),
             function(v) sum(coral[[v]] > 0 & coral[[v]] < 1, na.rm = TRUE),
             integer(1)))
cat("\nmissing by day:\n")
print(vapply(paste0("surv_d", 1:7), function(v) sum(is.na(coral[[v]])),
             integer(1)))

usethis::use_data(coral, overwrite = TRUE)
