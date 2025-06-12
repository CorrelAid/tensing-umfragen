library(kbtbr)
library(dotenv)
library(dplyr)
source("R/utils.R")
dotenv::load_dot_env()

source("pipeline/00-get-metadata.R")

# METADATA ----
tn_survey <- readr::read_csv("data/meta/tn_survey.csv")
tn_choices <- readr::read_csv("data/meta/tn_choices.csv")

cfg <- list()

cfg$URL <- "https://ee-eu.kobotoolbox.org/single/UgaA9dLH"

# IDENTIFY QUESTIONS --
cfg$Q_BEGIN_WORT <- find_qs(tn_survey, "label", "Wort") %>% filter(type == "begin_group")
cfg$QS_WORT <- find_qs(tn_survey, "col_name", "Wort")

cfg$Q_BEGIN_EIGENSCHAFTEN <- find_q(tn_survey, "label", "Eigenschaften")
stopifnot(cfg$Q_BEGIN_EIGENSCHAFTEN$type == "begin_group") # todo design function that can handle this as an additional constraint
cfg$QS_EIGENSCHAFTEN <- find_qs(tn_survey, "col_name", cfg$Q_BEGIN_EIGENSCHAFTEN$name) %>% filter(type == "select_one")

#eig_qs %>% write_csv("data/meta/tn/eig.csv")
#eig_choices <- tn_choices %>% filter(list_name %in% eig_qs$select_from_list_name)
#eig_choices %>% write_csv("data/meta/tn/eig_choices.csv")

cfg$Q_ZUGANGSWEGE <- find_qs(tn_survey, "col_name", "zugangsweg") %>% filter(type == "select_multiple")
cfg$LABEL_ZUGANGSWEGE <- c(
    "Mitgenommen von anderer Person",
    "Aufführung",
    "Seminar",
    "Schnupperprobe",
    "Kirchengemeinde",
    "Angebot von TEN SING in der Schule",
    "Nicht-kirchliche Veranstaltung",
    "Social Media",
    "Sonstiges"
)
cfg$CN_ZUGANGSWEGE <- cfg$Q_ZUGANGSWEGE$col_name

cfg$Q_ZUGANGSWEGE_SONST <- find_qs(tn_survey, "col_name", "zugangsweg") %>% filter(type == "text")
cfg$CN_ZUGANGSWEGE_SONST <- cfg$Q_ZUGANGSWEGE_SONST$col_name

cfg$Q_BEGIN_AUSSAGEN_LIKERT <- find_qs(tn_survey, "col_name", "aussage") %>% filter(str_detect(name, "header"))
cfg$QS_AUSSAGEN_LIKERT <- find_qs(tn_survey, "col_name", "aussage") %>% filter(type == "select_one") %>% filter(!str_detect(name, "header"))
cfg$CN_AUSSAGEN_LIKERT <- cfg$QS_AUSSAGEN_LIKERT$col_name

cfg$QS_AUSSAGEN_LIKERT %>% 
    arrange(name) %>% 
    write_csv("data/meta/tn/aussagen.csv")

tn_choices %>% 
    filter(list_name == cfg$QS_AUSSAGEN_LIKERT$select_from_list_name[1]) %>% 
    write_csv("data/meta/tn/aussagen_choices.csv")

cfg$Q_ANGEBOTE_VOR_ORT <- find_q(tn_survey, "label", "An welchen Angeboten")
cfg$CN_ANGEBOTE_VOR_ORT <- cfg$Q_ANGEBOTE_VOR_ORT$col_name
cfg$Q_ANGEBOTE_VOR_ORT_SONST <- find_q(tn_survey, "col_name", "andere_angebote")
cfg$CN_ANGEBOTE_VOR_ORT_SONST <- cfg$Q_ANGEBOTE_VOR_ORT_SONST$col_name

cfg$Q_OG_NAME <- find_q(tn_survey, "label", "heißt.+?Ortsgruppe")
cfg$CN_OG_NAME <- cfg$Q_OG_NAME$col_name

cfg$Q_VERANTWORTUNG_JANEIN <- find_q(tn_survey, "label", "Übernimmst.+?Verantwortung\\?$")
cfg$CN_VERANTWORTUNG_JANEIN <- cfg$Q_VERANTWORTUNG_JANEIN$col_name

cfg$Q_VERANTWORTUNG_SUPPORT <- find_q(tn_survey, "label", "helfen.+?Verantwortung")
cfg$CN_VERANTWORTUNG_SUPPORT <- cfg$Q_VERANTWORTUNG_SUPPORT$col_name

cfg$Q_VERANTWORTUNG_SUPPORT_SONST <- find_q(tn_survey, "col_name", "hilfe_fuer_verantw")
cfg$CN_VERANTWORTUNG_SUPPORT_SONST <- cfg$Q_VERANTWORTUNG_SUPPORT_SONST$col_name

cfg$Q_INFO_WEGE_AKTUELL <- find_q(tn_survey, "col_name", "informationswege_derzeit")
cfg$CN_INFO_WEGE_AKTUELL <- cfg$Q_INFO_WEGE_AKTUELL$col_name

cfg$Q_INFO_KOMPLETT <- find_q(tn_survey, "col_name", "informationen_komplett")
cfg$CN_INFO_KOMPLETT  <- cfg$Q_INFO_KOMPLETT$col_name


info_wege_qs <- find_qs(tn_survey, "label", "Wenn du es dir aussuchen.+?Informationen.+?")
cfg$Q_INFO_WEGE_AENDERUNG <- info_wege_qs %>% filter(type == "select_one")
cfg$CN_INFO_WEGE_AENDERUNG <- cfg$Q_INFO_WEGE_AENDERUNG$col_name

cfg$Q_INFO_WEGE_WUNSCH <- info_wege_qs %>% filter(type == "select_multiple")
cfg$CN_INFO_WEGE_WUNSCH <- cfg$Q_INFO_WEGE_WUNSCH$col_name


cfg$Q_EMPFEHLUNG <- find_q(tn_survey, "label", "empfiehlst")
cfg$CN_EMPFEHLUNG <- cfg$Q_EMPFEHLUNG$col_name

cfg$Q_DEMO_GENDER <- find_q(tn_survey, "label", "Geschlecht")
cfg$CN_DEMO_GENDER <- cfg$Q_DEMO_GENDER$col_name

cfg$Q_DEMO_ALTER <- find_q(tn_survey, "label", "Alter")
cfg$CN_DEMO_ALTER <- cfg$Q_DEMO_ALTER$col_name
# ab welchem Alter soll eine Eingabe als invalid angesehen werden?
# diese Eingabe wird dann gelöscht aus den Daten
cfg$INVALID_DEMO_ALTER <- 99

cfg$Q_DEMO_SCHULE <- find_q(tn_survey, "col_name", "demo.+?Schule")
cfg$CN_DEMO_SCHULE <- cfg$Q_DEMO_SCHULE$col_name

cfg$Q_CHR_GLAUBEN_KONTAKT <- find_q(tn_survey, "label", "Kontakt.+?christlichen")
cfg$CN_CHR_GLAUBEN_KONTAKT <- cfg$Q_CHR_GLAUBEN_KONTAKT$col_name

cfg$Q_CHR_GLAUBEN_KONTAKT_SONST <- find_q(tn_survey, "label", "andere Kontaktpunkte")
cfg$CN_CHR_GLAUBEN_KONTAKT_SONST <- cfg$Q_CHR_GLAUBEN_KONTAKT_SONST$col_name


cfg %>% readr::write_rds("config/tn_cfg.rds")



