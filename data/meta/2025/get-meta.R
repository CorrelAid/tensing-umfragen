#' this script pulls metadata from the kobo api
#' for both the OG and the TN survey, it pulls down the survey/questions and the choices
#' (see https://xlsform.org/en/ for terminology)
#' we need the metadata for the config/config-[tn|og].R "config" files and also for the get-data scripts
library(kbtbr)
library(dotenv)
library(dplyr)
library(readr)
source("R/utils.R")

dotenv::load_dot_env(file = "data/meta/2025/.env") # loads .env
kobo <- kbtbr::Kobo$new(
    "https://eu.kobotoolbox.org/",
    kobo_token = Sys.getenv("KOBO_EU_TOKEN")
)

# OG SURVEY DATEN --------
og_id <- Sys.getenv("OG")
og_asset <- kobo$get(sprintf("assets/%s", og_id))
og_survey <- og_asset$content$survey
og_survey <- og_survey |>
    flatten_label() |> # utils.R
    rename(col_name = "$xpath")

# json format for reference only
og_survey |>
    survey_to_json() |>
    readr::write_lines("data/meta/2025/og_survey.json")
og_survey |> readr::write_csv("data/meta/2025/og_survey.csv")

# extract choices
og_choices <- og_asset$content$choices
og_choices$label <- unlist(og_choices$label)
og_choices |> readr::write_csv("data/meta/2025/og_choices.csv")

# TN SURVEY ---------
tn_id <- Sys.getenv("TN")
tn_asset <- kobo$get(sprintf("assets/%s", tn_id))
tn_survey <- tn_asset$content$survey
tn_survey <- tn_survey |>
    flatten_label() |>
    rename(col_name = "$xpath")

tn_survey |>
    survey_to_json() |>
    readr::write_lines("data/meta/2025/tn_survey.json")
tn_survey |> readr::write_csv("data/meta/2025/tn_survey.csv")

# extract coices
tn_choices <- tn_asset$content$choices
tn_choices$label <- unlist(tn_choices$label)
tn_choices |> readr::write_csv("data/meta/2025/tn_choices.csv")
