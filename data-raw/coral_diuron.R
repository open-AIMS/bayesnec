# Builds `coral_colour` and `coral_pam`: the herbicide diuron crossed with three
# climate scenarios on the coral Acropora millepora, from Flores et al. (2021).
#
# Two endpoints from one 14-day experiment, shipped as two objects because the
# designs differ. Colour is scored once per fragment at the end of the exposure,
# up to four fragments per chamber. PAM effective quantum yield is read
# repeatedly per fragment, so its rows are not balanced.
#
# Source files are in data-raw/coral_diuron/ and are not distributed with the
# package.

library(dplyr)
library(usethis)

RAW <- "data-raw/coral_diuron"

# --- coral_colour -----------------------------------------------------------
# `Chamber` is numbered across the experiment (1 to 54), unlike in the PAM file,
# so it needs no reconstruction. The 29 ug/L chambers are absent: those corals
# died before they were photographed.
cc <- read.csv(file.path(RAW, "Colour.with10.csv"))

cc$intensity <- (cc$T14.MeanPixel - min(cc$T0.MeanPixel, cc$T14.MeanPixel)) /
  diff(range(cc$T0.MeanPixel, cc$T14.MeanPixel))
rows <- with(cc, table(Climate, Diuron))
stopifnot(
  length(unique(cc$Chamber)) == 45,
  all(table(cc$Chamber) <= 4),
  !any(cc$Diuron == 29),
  all(rows[, colnames(rows) != "9.6"] == 12),
  all(rows[, "9.6"] == c(12, 10, 5)),
  # `converted` is `T14.MeanPixel` rescaled by the range of both pixel columns,
  # as a percentage
  max(abs(cc$converted / 100 - cc$intensity)) < 1e-8,
  # `Proportion` is its complement, except that the file records the one 0 as
  # 1e-05. The 0 is shipped.
  sum(abs(cc$Proportion - (1 - cc$intensity)) > 1e-8) == 1,
  max(abs(cc$Proportion - (1 - cc$intensity))) < 2e-5
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
    intensity = as.numeric(intensity),
    proportion = 1 - intensity
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
