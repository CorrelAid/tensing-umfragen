rm(list = ls())
options(readr.show_col_types = FALSE)
source("R/load_libs.R")

years <- get_all_years()
message("Years detected: ", paste(years, collapse = ", "))

# --- pipeline --------------------------------------------------------------
for (YEAR in years) {
    message("=== Running pipeline for ", YEAR, " ===")

    # pipeline uses YEAR as constant
    assign("YEAR", YEAR, envir = .GlobalEnv)

    source("pipeline/setup.R")
    source("pipeline/00-get-metadata.R")
    source("pipeline/01-clean-og-data.R")
    source("pipeline/02-clean-tn-data.R")

    # optional year-specific scripts
    f_year <- sprintf("pipeline/03-year_specific-processing/process_%s.R", YEAR)
    if (file.exists(f_year)) {
        source(f_year)
    }

    source("pipeline/04-tn-og-processing.R")
}
