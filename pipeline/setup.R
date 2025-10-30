YEAR <- 2024
DIR_META <- file.path("data", "meta", YEAR)
DIR_RAW <- file.path("data", "raw", YEAR)
DIR_CLEANED <- file.path("data", "cleaned", YEAR)
DIR_CONFIG <- file.path("config", YEAR)

# ensure directories exist
dirs <- c(DIR_META, DIR_RAW, DIR_CLEANED)
for (d in dirs) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

source("pipeline/00-get-metadata.R")


source("config/og_config.R")
#source("pipeline/01-get-og-data.R")
#source("pipeline/02-get-tn-data.R")