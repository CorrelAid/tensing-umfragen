library(kbtbr)
library(dotenv)
library(dplyr)
library(readr)
source("R/utils.R")

dotenv::load_dot_env()
kobo <- kbtbr::Kobo$new("https://eu.kobotoolbox.org/", kobo_token = Sys.getenv("KOBO_EU_TOKEN"))

# OG SURVEY DATEN --------
og_id <- Sys.getenv("2024_OG")
og_asset <- kobo$get(sprintf("assets/%s", og_id))
og_survey <- og_asset$content$survey
og_survey <- og_survey %>% 
    mutate(label = label %>% purrr::map_chr(function(x) {
        if(is.null(x)) x <- NA
        stopifnot(length(x) == 1) # only one label per question
        return(x)
    })) %>% 
    rename(col_name = "$xpath")

# for reference only
og_survey %>% 
    purrr::transpose() %>% 
    as.list() %>% 
    jsonlite::toJSON(auto_unbox = TRUE) %>% 
    readr::write_lines("data/meta/og_survey.json")
og_survey %>% readr::write_csv("data/meta/og_survey.csv")

og_choices <- og_asset$content$choices
og_choices$label <- unlist(og_choices$label)
og_choices %>% readr::write_csv("data/meta/og_choices.csv")

# TN Survey Daten ---------
tn_id <- Sys.getenv("2024_TN")
tn_asset <- kobo$get(sprintf("assets/%s", tn_id))
tn_survey <- tn_asset$content$survey
tn_survey <- tn_survey %>% 
    mutate(label = label %>% purrr::map_chr(function(x) {
        if(is.null(x)) x <- NA
        stopifnot(length(x) == 1) # only one label per question
        return(x)
    })) %>% 
    rename(col_name = "$xpath")

tn_survey %>% 
    purrr::transpose() %>% 
    as.list() %>% 
    jsonlite::toJSON(auto_unbox = TRUE) %>% 
    readr::write_lines("data/meta/tn_survey.json")
tn_survey %>% readr::write_csv("data/meta/tn_survey.csv")

tn_choices <- tn_asset$content$choices
tn_choices$label <- unlist(tn_choices$label)
tn_choices %>% readr::write_csv("data/meta/tn_choices.csv")

