#' contains "configuration" aka column finding code
#' to identify the column(s) for each question.
#' creates config/og_cfg.rds object which is used in get-og-data.R
#' see readme for background
#' this is not complete, for some questions this is also done
#' directly in get-og-data.R
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

cfg$URL <- "https://ee-eu.kobotoolbox.org/single/cs5DElJT" # TODO: 2024 specific -> put into year-specific .env file?!

# EURE AKTIVITÄTEN -------
# An welchen Tagen finden in einer normalen Schulwoche bei euch TEN SING Veranstaltungen statt?
cfg$Q_WOCHENTAG <- find_q(og_survey, "name", "Wochentag")
cfg$CN_WOCHENTAG <- cfg$Q_WOCHENTAG$col_name

# Wie viele Stunden trefft ihr euch ...?
cfg$Q_WOCHENTAG_STUNDEN <- find_qs(og_survey, "name", ".+?_[sS]tunden")
cfg$CN_WOCHENTAG_STUNDEN <- cfg$Q_WOCHENTAG_STUNDEN$col_name

# Wie viele Auftritte (z. B. Show, Auftritte in Gottesdiensten) hatte eure Gruppe im letzten Schuljahr?
cfg$Q_AUFTRITTE <- find_q(og_survey, "label", "Auftritte.+?Schuljahr")
cfg$CN_AUFTRITTE <- cfg$Q_AUFTRITTE$col_name

# Welche Workshops werden bei euch vor Ort angeboten?
cfg$Q_WORKSHOPS <- find_q(og_survey, "name", "angebote_vor_ort")
cfg$CN_WORKSHOPS <- cfg$Q_WORKSHOPS$col_name

# Gibt es bei euch vor Ort eine:n hauptberufliche:n Mitarbeiter:in, der für TEN SING zuständig ist?
cfg$Q_HAUPTAMT_JANEIN <- find_q(og_survey, "label", "hauptberuflich")
cfg$CN_HAUPTAMT_JANEIN <- cfg$Q_HAUPTAMT_JANEIN$col_name

# wenn ja: Wie viele Stunden hat (/haben) die Person(en) insgesamt pro Woche für eure TEN SING Gruppe?
cfg$Q_HAUPTAMT_STUNDEN <- find_q(og_survey, "name", "Hauptamt_Wochenstunden")
cfg$CN_HAUPTAMT_STUNDEN <- cfg$Q_HAUPTAMT_STUNDEN$col_name

# question block "wie viele Personen waren bei euch... aktiv?"
cfg$Q_ANZAHL_BEGIN <- find_q(og_survey, "label", "Wie viele Personen.+aktiv?")

# TODO matrix

# TODO weird bug where col_name in metadata has 002 suffix but data has not :eyes:
cfg$CN_ANZAHL_TN <- "group_jx3lf36/Teilnehmende"
cfg$Q_ANZAHL_TN <- find_q(
    og_survey,
    "col_name",
    paste0(cfg$CN_ANZAHL_TN, "_002")
)
cfg$Q_ANZAHL_TN$col_name <- cfg$CN_ANZAHL_TN

cfg$Q_TN_GEWINNUNG_PROB <- find_q(
    og_survey,
    "col_name",
    "[Gg]ewinnung.+?[Pp]robleme"
)
cfg$CN_TN_GEWINNUNG_PROB <- cfg$Q_TN_GEWINNUNG_PROB$col_name

cfg$Q_TN_GEWINNUNG_MASSNAHMEN <- find_q(
    og_survey,
    "col_name",
    "[Gg]ewinnung.+?[Mm]assnahmen$"
)
cfg$CN_TN_GEWINNUNG_MASSNAHMEN <- cfg$Q_TN_GEWINNUNG_MASSNAHMEN$col_name

cfg$Q_TN_GEWINNUNG_MASSNAHMEN_SONST <- find_q(
    og_survey,
    "col_name",
    "[Gg]ewinnung.+?[Mm]assnahmen_sons$"
)
cfg$CN_TN_GEWINNUNG_MASSNAHMEN_SONST <- cfg$Q_TN_GEWINNUNG_MASSNAHMEN_SONST$col_name

cfg$QS_TN_GEWINNUNG_MASSNAHMEN_ERFOLG <- find_qs(
    og_survey,
    "col_name",
    "Erfolg.+?[Mm]assnahm"
) %>%
    filter(type == "select_one")

cfg$Q_BEGIN_UNTERSTUETZUNGSBEDARFE <- find_qs(
    og_survey,
    "col_name",
    "Unterstuetzungsbedarfe"
) %>%
    filter(type == "begin_group")

cfg$QS_UNTERSTUETZUNGSBEDARFE <- find_qs(
    og_survey,
    "col_name",
    "Unterstuetzungsbedarfe"
) %>%
    filter(type == "select_one") %>%
    filter(!str_detect(col_name, "header"))
cfg$CN_UNTERSTUETZUNGSBEDARFE <- cfg$QS_UNTERSTUETZUNGSBEDARFE$col_name

cfg$Q_OG_REGION <- find_q(og_survey, "col_name", "region")
cfg$CN_OG_REGION <- cfg$Q_OG_REGION$col_name

cfg$Q_OG_NAME <- find_q(og_survey, "col_name", "ortsgruppe_name")
cfg$CN_OG_NAME <- cfg$Q_OG_NAME$col_name

cfg %>% readr::write_rds("config/og_cfg.rds")
