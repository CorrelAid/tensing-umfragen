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

# question block "wie viele Personen waren bei euch... aktiv?"
cfg$Q_ANZAHL_BEGIN <- find_q(og_survey, "label", "Wie viele Personen.+aktiv?")

# TODO weird bug where col_name in metadata has 002 suffix but data has not :eyes:
cfg$CN_ANZAHL_TN <- "group_jx3lf36/Teilnehmende"
cfg$Q_ANZAHL_TN <- find_q(og_survey, "col_name", paste0(cfg$CN_ANZAHL_TN, "_002"))
cfg$Q_ANZAHL_TN$col_name <- cfg$CN_ANZAHL_TN

cfg$Q_TN_GEWINNUNG_PROB <- find_q(og_survey, "col_name", "[Gg]ewinnung.+?[Pp]robleme")
cfg$CN_TN_GEWINNUNG_PROB <- cfg$Q_TN_GEWINNUNG_PROB$col_name

cfg$Q_TN_GEWINNUNG_MASSNAHMEN <- find_q(og_survey, "col_name", "[Gg]ewinnung.+?[Mm]assnahmen$")
cfg$CN_TN_GEWINNUNG_MASSNAHMEN <- cfg$Q_TN_GEWINNUNG_MASSNAHMEN$col_name

cfg$Q_TN_GEWINNUNG_MASSNAHMEN_SONST <- find_q(og_survey, "col_name", "[Gg]ewinnung.+?[Mm]assnahmen_sons$")
cfg$CN_TN_GEWINNUNG_MASSNAHMEN_SONST <- cfg$Q_TN_GEWINNUNG_MASSNAHMEN_SONST$col_name

cfg$QS_TN_GEWINNUNG_MASSNAHMEN_ERFOLG <- find_qs(og_survey, "col_name", "Erfolg.+?[Mm]assnahm") %>% filter(type == "select_one")

cfg$Q_BEGIN_UNTERSTUETZUNGSBEDARFE <- find_qs(og_survey, "col_name", "Unterstuetzungsbedarfe") %>% 
    filter(type == "begin_group")

cfg$QS_UNTERSTUETZUNGSBEDARFE <- find_qs(og_survey, "col_name", "Unterstuetzungsbedarfe") %>% 
    filter(type == "select_one") %>% 
    filter(!str_detect(col_name, "header"))
cfg$CN_UNTERSTUETZUNGSBEDARFE <- cfg$QS_UNTERSTUETZUNGSBEDARFE$col_name

cfg$Q_OG_REGION <- find_q(og_survey, "col_name", "region")
cfg$CN_OG_REGION <- cfg$Q_OG_REGION$col_name

cfg$Q_OG_NAME <- find_q(og_survey, "col_name", "ortsgruppe_name")
cfg$CN_OG_NAME <- cfg$Q_OG_NAME$col_name

cfg %>% readr::write_rds("config/og_cfg.rds")
