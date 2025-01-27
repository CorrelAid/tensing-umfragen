library(kbtbr)
library(dotenv)
library(dplyr)
source("utils.R")
dotenv::load_dot_env()

kobo <- kbtbr::Kobo$new("https://eu.kobotoolbox.org/", kobo_token = Sys.getenv("KOBO_EU_TOKEN"))


# Ortsgruppe Survey Daten ---------
tn_id <- Sys.getenv("2024_TN")
tn_asset <- kobo$get(sprintf("assets/%s", tn_id))
tn_survey <- tn_asset$content$survey
tn_survey <- tn_survey %>% 
    mutate(label = label %>% purrr::map_chr(function(x) {
        if(is.null(x)) x <- NA
        stopifnot(length(x) == 1) # only one label per question
        return(x)
    }))  %>% 
    rename(col_name = "$xpath")

# IDENTIFY QUESTIONS --
QS_WORT <- find_qs(tn_survey, "col_name", "Wort")

Q_VERANTWORTUNG_JANEIN <- find_q(tn_survey, "label", "Übernimmst.+?Verantwortung\\?$")
CN_VERANTWORTUNG_JANEIN <- Q_VERANTWORTUNG_JANEIN$col_name

Q_VERANTWORTUNG_SUPPORT <- find_q(tn_survey, "label", "helfen.+?Verantwortung")
CN_VERANTWORTUNG_SUPPORT <- Q_VERANTWORTUNG_SUPPORT$col_name

Q_VERANTWORTUNG_SUPPORT_SONST <- find_q(tn_survey, "col_name", "hilfe_fuer_verantw")
CN_VERANTWORTUNG_SUPPORT_SONST <- Q_VERANTWORTUNG_SUPPORT_SONST$col_name

Q_INFO_WEGE_AKTUELL <- find_q(tn_survey, "col_name", "informationswege_derzeit")
CN_INFO_WEGE_AKTUELL <- Q_INFO_WEGE_AKTUELL$col_name

Q_INFO_KOMPLETT <- find_q(tn_survey, "col_name", "informationen_komplett")
CN_INFO_KOMPLETT  <- Q_INFO_KOMPLETT$col_name


info_wege_qs <- find_qs(tn_survey, "label", "Wenn du es dir aussuchen.+?Informationen.+?")
Q_INFO_WEGE_AENDERUNG <- info_wege_qs %>% filter(type == "select_one")
CN_INFO_WEGE_AENDERUNG <- Q_INFO_WEGE_AENDERUNG$col_name

Q_INFO_WEGE_WUNSCH <- info_wege_qs %>% filter(type == "select_multiple")
CN_INFO_WEGE_WUNSCH <- Q_INFO_WEGE_WUNSCH$col_name


Q_EMPFEHLUNG <- find_q(tn_survey, "label", "empfiehlst")
CN_EMPFEHLUNG <- Q_EMPFEHLUNG$col_name

Q_DEMO_GENDER <- find_q(tn_survey, "label", "Geschlecht")
CN_DEMO_GENDER <- Q_DEMO_GENDER$col_name

Q_DEMO_ALTER <- find_q(tn_survey, "label", "Alter")
CN_DEMO_ALTER <- Q_DEMO_ALTER$col_name
# ab welchem Alter soll eine Eingabe als invalid angesehen werden?
# diese Eingabe wird dann gelöscht aus den Daten
INVALID_DEMO_ALTER <- 99

Q_DEMO_SCHULE <- find_q(tn_survey, "col_name", "demo.+?Schule")
CN_DEMO_SCHULE <- Q_DEMO_SCHULE$col_name

Q_CHR_GLAUBEN_KONTAKT <- find_q(tn_survey, "label", "Kontakt.+?christlichen")
CN_CHR_GLAUBEN_KONTAKT <- Q_CHR_GLAUBEN_KONTAKT$col_name

Q_CHR_GLAUBEN_KONTAKT_SONST <- find_q(tn_survey, "label", "andere Kontaktpunkte")
CN_CHR_GLAUBEN_KONTAKT_SONST <- Q_CHR_GLAUBEN_KONTAKT_SONST$col_name