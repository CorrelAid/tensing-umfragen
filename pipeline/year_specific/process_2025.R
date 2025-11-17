#' this script contains year specific processes for 2025:
#' TN: 
#' - consolidate og_name


# TN DATA ==========
# (1) consolidate og_name
# read in data
tn <- readr::read_rds(file.path(DIR_CLEANED, "tn.rds"))
og_recoding <- readr::read_csv(file.path(DIR_META, "og_recoding.csv"))
og_recoding <- og_recoding %>%
    rename(
        og_name_orig = `Schreibweise in den Daten`,
        og_name = Vereinheitlicht,
        og_region = Landesverband
    )

# join to og data
tn$data <- tn$data %>%
  rename(og_region_orig = og_region) %>%
  left_join(og_recoding, by = c("og_name_cleaned" = "og_name_orig"))

# Fill in regions where og_name is NA (but region isnt)
tn$data$og_region[is.na(tn$data$og_name)] <- tn$data$og_region_orig[is.na(tn$data$og_region)]

# Replace NA in og_name
tn$data$og_name_orig[tn$data$og_name_orig == ""] <- NA

print(tn$data[is.na(tn$data$og_name), c("og_name", "og_name_orig", "og_region_orig")], n=49)

# check that NAs are legit
sum(is.na(tn$data$og_name))
sum(is.na(tn$data$og_name_orig))

tn %>% readr::write_rds(file.path(DIR_CLEANED, "tn.rds"))