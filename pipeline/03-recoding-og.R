library(googlesheets4)
library(tidyverse)
# KODIERUNGSDATEN ============
og_ref <- readr::read_csv("data/ref/og_names.csv")

# og_recoding <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1kFo3lhPVslsJrXCmFvbWry8BN4V_9VIIptIQ32Y4sDE/edit?gid=0#gid=0")
# og_recoding %>% readr::write_csv("data/meta/og_recoding_gsheet.csv")
og_recoding <- readr::read_csv("data/meta/og_recoding_gsheet.csv")
# rename variables 
og_recoding <- og_recoding %>% 
  rename(og_name_orig = `Schreibweise in den Daten`,
         og_name = Vereinheitlicht,
         og_region = Landesverband) %>% 
  select(-Kommentar)

# OG DATA ============
# read in data 
og <- readr::read_rds("data/cleaned/og.rds")
og_data <- og$data

# join to og data 
og_data <- og_data %>% 
  rename(og_region_orig = og_region) %>% 
  left_join(og_recoding, by = "og_name_orig")


og_data$og_name

# TODO: how to deal with NA in og_name? and TODO: How to deal with duplicate entries for one OG?
og$data <- og_data
og %>% readr::write_rds("data/cleaned/og.rds")

# TN DATA ==========
# read in data 
tn <- readr::read_rds("data/cleaned/tn.rds")
tn_data <- tn$data

# join to og data 
tn_data <- tn_data %>% 
  rename(og_region_orig = og_region) %>% 
  left_join(og_recoding, by = c("og_name_cleaned" = "og_name_orig"))

# check that NAs are legit 
sum(is.na(tn_data$og_name))
sum(is.na(tn_data$og_name_orig))
tn_data %>% filter(is.na(og_name)) %>% 
  select(og_name, og_name_orig)

tn$data <- tn_data
tn %>% readr::write_rds("data/cleaned/tn.rds")
