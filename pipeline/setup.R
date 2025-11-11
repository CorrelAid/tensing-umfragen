args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) stop("Please provide a year, e.g. Rscript pipeline/setup.R 2024")
YEAR <- args[1]

DIR_META <- file.path("data", "meta", YEAR)
DIR_RAW <- file.path("data", "raw", YEAR)
DIR_CLEANED <- file.path("data", "cleaned", YEAR)
DIR_CONFIG <- file.path("config", YEAR)

# ensure directories exist
dirs <- c(DIR_META, DIR_RAW, DIR_CLEANED, DIR_CONFIG)
for (d in dirs) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

source("pipeline/00-get-metadata.R")
source("pipeline/01-get-og-data.R")
source("pipeline/02-get-tn-data.R")

if (file.exists(sprintf("pipeline/year_specific/process_og_%s.R", YEAR))) {
  source(sprintf("pipeline/year_specific/process_og_%s.R", YEAR))
} 
if (file.exists(sprintf("pipeline/year_specific/process_tn_%s.R", YEAR))) {
  source(sprintf("pipeline/year_specific/process_tn_%s.R", YEAR))
}
source("pipeline/03-delete-duplicates.R")
