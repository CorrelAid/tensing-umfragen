library(kbtbr)
library(dotenv)
library(dplyr)
source("R/utils.R")
dotenv::load_dot_env()

source("pipeline/00-get-metadata.R")

# METADATA ----
og_survey <- readr::read_csv("data/meta/og_survey.csv")
og_choices <- readr::read_csv("data/meta/og_choices.csv")


cfg <- list()

cfg$URL <- "https://ee-eu.kobotoolbox.org/single/cs5DElJT"

cfg$Q_TN_GEWINNUNG_PROB <- find_q(og_survey, "col_name", "[Gg]ewinnung.+?[Pp]robleme")
cfg$CN_TN_GEWINNUNG_PROB <- cfg$Q_TN_GEWINNUNG_PROB$col_name

cfg$QS_TN_GEWINNUNG_MASSNAHMEN <- find_qs(og_survey, "col_name", "[Gg]ewinnung.+?[Mm]assnahmen$")
cfg$CNS_TN_GEWINNUNG_MASSNAHMEN <- cfg$QS_TN_GEWINNUNG_MASSNAHMEN$col_name

cfg$Q_TN_GEWINNUNG_MASSNAHMEN_SONST <- find_q(og_survey, "col_name", "[Gg]ewinnung.+?[Mm]assnahmen_sons$")
cfg$CN_TN_GEWINNUNG_MASSNAHMEN_SONST <- cfg$Q_TN_GEWINNUNG_MASSNAHMEN_SONST$col_name

cfg$QS_TN_GEWINNUNG_MASSNAHMEN_ERFOLG <- find_qs(og_survey, "col_name", "Erfolg.+?[Mm]assnahm") %>% filter(type == "select_one")

cfg$Q_BEGIN_UNTERSTUETZUNGSBEDARFE <- find_qs(og_survey, "col_name", "Unterstuetzungsbedarfe") %>% 
    filter(type == "begin_group")

cfg$QS_UNTERSTUETZUNGSBEDARFE <- find_qs(og_survey, "col_name", "Unterstuetzungsbedarfe") %>% 
    filter(type == "select_one") %>% 
    filter(!str_detect(col_name, "header"))
cfg$CN_UNTERSTUETZUNGSBEDARFE <- cfg$QS_UNTERSTUETZUNGSBEDARFE$col_name


cfg %>% readr::write_rds("config/og_cfg.rds")
