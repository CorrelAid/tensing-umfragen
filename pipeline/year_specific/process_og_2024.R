#' this script recodes open text entries to the question
#' "what is your local group"
#' this is only relevant for 2024, for 2025 ff. this question is a single select
#' TODO: integrate in 2024-specific data processing, doesn't need to be its own script going forward
library(googlesheets4)
library(tidyverse)
# KODIERUNGSDATEN ============

# og_recoding <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1kFo3lhPVslsJrXCmFvbWry8BN4V_9VIIptIQ32Y4sDE/edit?gid=0#gid=0")
# og_recoding %>% readr::write_csv(file.path(DIR_META,"og_recoding_gsheet.csv")
og_recoding <- readr::read_csv(file.path(DIR_META, "og_recoding_gsheet.csv"))
# rename variables
og_recoding <- og_recoding %>%
  rename(
    og_name_orig = `Schreibweise in den Daten`,
    og_name = Vereinheitlicht,
    og_region = Landesverband
  ) %>%
  select(-Kommentar)

# OG DATA ============
# read in data
og <- readr::read_rds(file.path(DIR_CLEANED, "og.rds"))
og_data <- og$data

# join to og data
og_data <- og_data %>%
  rename(og_region_orig = og_region) %>%
  left_join(og_recoding, by = "og_name_orig")

# TODO: how to deal with NA in og_name? and TODO: How to deal with duplicate entries for one OG? -> see 04_ for current approach
og$data <- og_data
og %>% readr::write_rds(file.path(DIR_CLEANED, "og.rds"))

# TN DATA ==========
# read in data
tn <- readr::read_rds(file.path(DIR_CLEANED, "tn.rds"))

# join to og data
tn$data <- tn$data %>%
  rename(og_region_orig = og_region) %>%
  left_join(og_recoding, by = c("og_name_cleaned" = "og_name_orig"))

# check that NAs are legit
sum(is.na(tn$data$og_name))
sum(is.na(tn$data$og_name_orig))

tn %>% readr::write_rds(file.path(DIR_CLEANED, "tn.rds"))
