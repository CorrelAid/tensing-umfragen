#' this script contains year specific processes for 2024: 
#' OG
#' -  recodes open text entries to the question "what is your local group"
#' -  delete duplicates in og data (some local groups answered twice)
#' TN:
#' -  recodes open text entries to the question "what is your local group"

library(googlesheets4)
library(tidyverse)
# (1) recode og_name
# KODIERUNGSDATEN ============

# og_recoding <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1kFo3lhPVslsJrXCmFvbWry8BN4V_9VIIptIQ32Y4sDE/edit?gid=0#gid=0")
# og_recoding %>% readr::write_csv(file.path(DIR_META,"og_recoding.csv")
og_recoding <- readr::read_csv(file.path(DIR_META, "og_recoding.csv"))
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
  
# add grouped regions 
og_data <- og_data |> select(-c("region_grouped"))
m_regions <- readr::read_csv("data/meta/region_mapping.csv")

og_data <- og_data |>
  left_join(
    m_regions |> select(og_region, region_grouped),
    by = "og_region"
  )


# (2) remove duplicates
#' we only take the first answer (TODO: to discuss with TEN SING whether that's a valid approach)
#' TODO: alternatively, look at data quality.

# we filter out NA
og_data <- og_data %>%
  filter(!is.na(og_name))

# we filter out duplictaes by taking only the first answer for a given og name
og_data <- og_data %>%
  group_by(og_name) %>%
  slice(1)

# now only keep those who were kept in the long data
og$long <- og$long %>%
  purrr::map(function(df) {
    df %>%
      filter(og_id %in% og_data$og_id)
  })


og$data <- og_data
og %>% readr::write_rds(file.path(DIR_CLEANED, "og.rds"))