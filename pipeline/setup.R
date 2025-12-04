DIR_META <- file.path("data", "meta", YEAR)
DIR_RAW <- file.path("data", "raw", YEAR)
DIR_CLEANED <- file.path("data", "cleaned", YEAR)
DIR_CONFIG <- file.path("config", YEAR)

# ensure directories exist
dirs <- c(DIR_META, DIR_RAW, DIR_CLEANED, DIR_CONFIG)
for (d in dirs) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

