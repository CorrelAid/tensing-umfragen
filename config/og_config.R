#' contains "configuration" aka column finding code
#' to identify the column(s) for each question.
#' creates config/og_cfg.rds object which is used in 01-clean-og-data.R
#' see readme for background
#' this is not complete, for some questions this is also done
#' directly in 01-clean-og-data.R
library(kbtbr)
library(dotenv)
library(dplyr)
source("R/utils.R")
dotenv::load_dot_env()

# METADATA ----
og_survey <- readr::read_csv(file.path(DIR_META, "og_survey.csv"))
og_choices <- readr::read_csv(file.path(DIR_META, "og_choices.csv"))


cfg <- list()

cfg$URL <- Sys.getenv(paste0(YEAR, "_OG_URL"))

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

cfg$Q_ANZAHL_MA <- find_q(og_survey, "col_name", "Mitarbeitende_inkl_Leitung")
cfg$CN_ANZAHL_MA <- cfg$Q_ANZAHL_MA$col_name    
# TODO matrix

cfg$Q_GESCHLECHT <- find_q(og_survey, "label", "identifizieren")
cfg$CN_GESCHLECHT <- cfg$Q_GESCHLECHT$col_name

cfg$Q_GESCHLECHT_BEGIN<-find_qs(og_survey, "type", "begin_kobomatrix") %>%
     filter(str_detect(label, "Geschlecht"))

cfg$Q_ALTERSGRUPPE <- find_q(og_survey, "col_name", "Altersgruppe")
cfg$CN_ALTERSGRUPPE <- cfg$Q_ALTERSGRUPPE$col_name
cfg$Q_ALTER_BEGIN <- find_qs(og_survey, "type", "begin_kobomatrix") %>%
    filter(str_detect(label, "Alter"))


cfg$Q_ANZAHL_TN <- find_q(
    og_survey,
    "col_name",
    "Teilnehmende_002"
)
cfg$CN_ANZAHL_TN <- sub("_[0-9]+$", "", cfg$Q_ANZAHL_TN$col_name)

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
    "[Gg]ewinnung.+?[Mm]assnahmen_sons(t)*$"
)
cfg$CN_TN_GEWINNUNG_MASSNAHMEN_SONST <- cfg$Q_TN_GEWINNUNG_MASSNAHMEN_SONST$col_name

cfg$QS_TN_GEWINNUNG_MASSNAHMEN_ERFOLG_BEGIN <- find_qs(
    og_survey,
    "col_name",
    "(Erfolg.+?[Mm]assnahm)|block_erfolg_tngewinnung",
) %>%
    filter(str_detect(name, "header"))

cfg$QS_TN_GEWINNUNG_MASSNAHMEN_ERFOLG <- find_qs(
    og_survey,
    "col_name",
    "(Erfolg.+?[Mm]assnahm)|block_erfolg_tngewinnung",
    "select_one"
) %>%
    # header in survey design ist fälschlicherweise ein "select_one"
    filter(!str_detect(name, "header"))

cfg$Q_BEGIN_UNTERSTUETZUNGSBEDARFE <- find_qs(
    og_survey,
    "col_name",
    "[Uu]nterstuetzungsbedarfe",
    "begin_group"
)
cfg$QS_UNTERSTUETZUNGSBEDARFE <- find_qs(
    og_survey,
    "col_name",
    "[Uu]nterstuetzungsbedarfe",
    "select_one"
) %>%
    filter(!str_detect(col_name, "header"))
cfg$CN_UNTERSTUETZUNGSBEDARFE <- cfg$QS_UNTERSTUETZUNGSBEDARFE$col_name

cfg$Q_UNTERSTUETZUNGSBEDARFE_WEITERE <- find_q(og_survey, "col_name", "[wW]eitereUnters")
cfg$CN_UNTERSTUETZUNGSBEDARFE_WEITERE <- cfg$Q_UNTERSTUETZUNGSBEDARFE_WEITERE$col_name


cfg$Q_OG_REGION <- find_q(og_survey, "col_name", "region$")
cfg$CN_OG_REGION <- cfg$Q_OG_REGION$col_name

cfg$Q_OG_NAME <- find_qs(og_survey, "col_name", "ortsgruppe_name$")
cfg$CN_OG_NAME <- cfg$Q_OG_NAME$col_name


cfg %>% readr::write_rds(file.path(DIR_CONFIG, "og_cfg.rds"))
