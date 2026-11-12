# Download the CC BY-NC 3.0 AU Negri et al. (2021) workbook for local analysis.
# The workbook and extracted observations stay in ignore/; they are not bundled
# with the GPL-2 package. Run from the package source root.

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Install readxl to import the Negri et al. (2021) workbook.")
}
source_url <- paste0(
  "https://data.aims.gov.au/data-download/",
  "615e208d-f6c7-4b2f-b99e-d4fc50614e48/",
  "Negri_etal_2021_Experimental-data_Toxicity-thresholds-gas-condensate-oils.xlsx"
)
dir.create("ignore/hormesis_data", recursive = TRUE, showWarnings = FALSE)
workbook <- file.path("ignore/hormesis_data", "Negri_2021_condensate.xlsx")
if (!file.exists(workbook)) {
  utils::download.file(source_url, workbook, mode = "wb")
}
sheet <- readxl::read_excel(workbook, sheet = "Cgoreaui(zoox)")
observations <- data.frame(source_conc_ug_l_tah = sheet[["raw.x"]],
                           sgr = sheet[["SGR"]])
stopifnot(nrow(observations) == 65,
          sum(observations$source_conc_ug_l_tah == 1) == 10,
          !anyNA(observations))
utils::write.csv(observations,
                 "ignore/hormesis_data/hormesis_negri_2021.csv",
                 row.names = FALSE)
cat("Imported", nrow(observations), "individual observations to ignore/hormesis_data.\n")
