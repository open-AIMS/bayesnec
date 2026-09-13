# Builds `coral_colour` and `coral_pam`: the herbicide diuron crossed with three
# climate scenarios on the coral Acropora millepora, from Flores et al. (2021).
#
# Two endpoints from one 14-day experiment, shipped as two objects because the
# designs differ. Colour is scored once per fragment at the end of the exposure,
# four fragments per chamber. PAM effective quantum yield is read repeatedly per
# fragment, so its rows are not balanced.
#
# Source files are in data-raw/coral_diuron/ and are not distributed with the
# package.

library(dplyr)
library(usethis)

RAW <- "data-raw/coral_diuron"

# --- coral_colour -----------------------------------------------------------
# `Chamber` is already unique across the experiment here (1 to 45), unlike in the
# PAM file, so it needs no reconstruction.
cc <- read.csv(file.path(RAW, "CoralColour.csv"))

stopifnot(
  length(unique(cc$Chamber)) == 45,
  all(table(cc$Chamber) == 4),
  # `Proportion` is the complement of `Intensity`, exactly
  max(abs(cc$Proportion + cc$Intensity - 1)) < 1e-9
)

# `Diuron.` substitutes 0.1 for the zero control so that a log predictor can be
# taken. The recorded 0 is shipped instead: substituting at a boundary is the
# practice vignette("example6") argues against, and a user who wants the
# substitution can make it and say so.
stopifnot(all(cc$Diuron[cc$Diuron > 0] == cc$Diuron.[cc$Diuron > 0]),
          all(cc$Diuron.[cc$Diuron == 0] == 0.1))

coral_colour <- cc |>
  dplyr::transmute(
    climate = factor(Climate, levels = c(2018, 2050, 2100)),
    diuron = as.numeric(Diuron),
    chamber = factor(sprintf("c%02d", Chamber)),
    fragment = as.integer(Fragment),
    t0_pixel = as.numeric(T0.MeanPixel),
    t14_pixel = as.numeric(T14.MeanPixel),
    intensity = as.numeric(Intensity),
    proportion = as.numeric(Proportion)
  ) |>
  dplyr::arrange(climate, diuron, chamber, fragment) |>
  as.data.frame()

# --- coral_pam --------------------------------------------------------------
# `Chamber` runs 1 to 3 within each climate-by-concentration cell rather than
# across the experiment, so a unique identifier has to combine the three.
pd <- read.csv(file.path(RAW, "pamdat.csv"))

coral_pam <- pd |>
  dplyr::transmute(
    climate = factor(Climate, levels = c(2018, 2050, 2100)),
    diuron = as.numeric(Diuron),
    chamber = factor(paste0(Climate, "-", Diuron, "-", Chamber)),
    fragment = as.integer(Fragment),
    yield = as.numeric(Yield)
  ) |>
  dplyr::arrange(climate, diuron, chamber, fragment) |>
  as.data.frame()

stopifnot(
  nlevels(coral_pam$chamber) == 54,
  # every chamber sits at one climate and one concentration
  all(tapply(coral_pam$diuron, coral_pam$chamber, function(z) length(unique(z))) == 1),
  all(tapply(as.character(coral_pam$climate), coral_pam$chamber,
             function(z) length(unique(z))) == 1),
  !anyNA(coral_pam), !anyNA(coral_colour),
  # the predictor must not be integer; bnec() refuses one
  is.numeric(coral_colour$diuron), !is.integer(coral_colour$diuron),
  is.numeric(coral_pam$diuron), !is.integer(coral_pam$diuron)
)

print(with(coral_colour, table(climate, diuron)))
print(with(coral_pam, table(climate, diuron)))
cat("colour: chambers", nlevels(coral_colour$chamber),
    " proportion at 0:", sum(coral_colour$proportion == 0),
    " intensity at 1:", sum(coral_colour$intensity == 1), "\n")
cat("pam: chambers", nlevels(coral_pam$chamber),
    " readings per chamber:", paste(range(table(coral_pam$chamber)), collapse = "-"),
    " yield at 0:", sum(coral_pam$yield == 0), "\n")

usethis::use_data(coral_colour, overwrite = TRUE)
usethis::use_data(coral_pam, overwrite = TRUE)
