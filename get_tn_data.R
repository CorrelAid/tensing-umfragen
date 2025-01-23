library(kbtbr)
library(dotenv)
library(dplyr)
source("utils.R")
source("tn_config.R")

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
    })) 

tn_survey %>% 
    purrr::transpose() %>% 
    as.list() %>% 
    jsonlite::toJSON(auto_unbox = TRUE) %>% 
    readr::write_lines("data/meta/tn_survey.json")


tn_choices <- tn_asset$content$choices
tn_choices$label <- unlist(tn_choices$label)
tn_choices %>% readr::write_csv("data/meta/tn_choices.csv")

# TN Daten -----------
tn_df <- kobo$get_submissions(tn_id)
# drop system columns
tn_df <- tn_df %>% 
    rename(tn_id = `_id`) %>% 
    select(-starts_with("_")) 
    
tn_df %>% readr::write_csv(file = "data/raw/tn_api.csv")

TN_DATA <- list(
    long = list(),
    wide = list(), # list of tibbles - wide format
    data = tibble(tn_id = tn_df$tn_id) # tibble with the rest of the variables
)


# EIGENSCHAFTEN -----------
start_group <- find_q(tn_survey, "label", "Eigenschaften")
stopifnot(start_group$type == "begin_group")
eig_group_id <- start_group$name

eig_qs <- find_qs(tn_survey, "$xpath", eig_group_id)
eig_qs <- eig_qs %>% filter(type == "select_one")

eig_long <- pivot_cols_long(tn_df, eig_qs$`$xpath`, values_to = "value")
eig_long <- eig_long %>% 
    left_join(eig_qs %>% select(label, name, col_name = `$xpath`), by = "col_name") %>% 
    select(tn_id, eig = name, eig_label = label, value)

TN_DATA$long$tensing_eigenschaften <- eig_long

# WEG ZU TENSING -------
zugangsweg_qs <- find_qs(tn_survey, "$xpath", "zugangsweg")
zw_msq <- zugangsweg_qs %>% filter(type == "select_multiple")
stopifnot(nrow(zw_msq) == 1)
zw_msq_col_name <- zw_msq$`$xpath`

zugangsweg_long <- make_multiselect_long(tn_df, zw_msq_col_name) %>% 
    rename(zugangsweg = !!zw_msq_col_name)

# sonstige / open text feld 
zw_sonst <- zugangsweg_qs %>% filter(type == "text")
stopifnot(nrow(zw_sonst) == 1)
zw_sonst_col_name <- zw_sonst$`$xpath`

zugangsweg_long <- zugangsweg_long %>% 
    left_join(tn_df %>% 
                select(tn_id, zugangsweg_sonst = !!zw_sonst_col_name) %>% 
                mutate(zugangsweg = "sonstiges"), by = c("tn_id", "zugangsweg")) 
# use labels
m <- get_mapping(tn_choices, zw_msq$select_from_list_name)
zugangsweg_long <- zugangsweg_long %>% recode_values(m, "zugangsweg")

TN_DATA$long$zugangsweg <- zugangsweg_long


# AUSSAGEN LIKERT ---------------
auss <- find_qs(tn_survey, "$xpath", "aussage")
auss <- auss %>% filter(type == "select_one") %>% filter(!str_detect(name, "header"))
auss  <- auss %>% arrange(name)

# just to make sure we sort the variables alphabetically before renaming
TN_DATA$wide$aussagen <- tn_df %>% select(tn_id, auss$`$xpath`) %>% 
  select(tn_id, sort(tidyselect::peek_vars())) %>%   
  rename_with(~auss$name, all_of(starts_with("group")))


# ANGEBOTE VOR ORT ---------------
angebote_q <- find_q(tn_survey, "label", "An welchen Angeboten")
col_name_angebote <- angebote_q$`$xpath`
angebote_m <- get_mapping(tn_choices, angebote_q$select_from_list_name)

angebote_long <- make_multiselect_long(tn_df, col_name_angebote) %>% 
    rename(angebot = !!col_name_angebote)

# add information from sonstige question
q <- find_q(tn_survey, "$xpath", "andere_angebote")
col_name_angebote_sonst <- q$`$xpath`
angebote_sonst <- tn_df %>% 
    filter(!is.na(.data[[col_name_angebote_sonst]])) %>% 
    mutate(angebot = "andere_workshops") %>% 
    select(tn_id, angebot, angebot_sonst = !!col_name_angebote_sonst)  

angebote_long <- left_join(angebote_long, angebote_sonst, by = c("tn_id", "angebot")) 

# wenn eine antwortoption "/" hat, dann wird es durch ___ ersetzt und nicht nur durch _ im Spaltennamen
angebote_long <- angebote_long %>% 
    mutate(angebot = str_replace_all(angebot, "___", "_")) %>% 
    recode_values(m, "angebot")

TN_DATA$long$angebote_vor_ort <- angebote_long

# STUNDEN -----------
q <- find_q(tn_survey, "$xpath", "stunden")
col_name_stunden <- q$`$xpath`

TN_DATA$data$stunden_pro_woche <- as.numeric(tn_df[[col_name_stunden]])

# REGION ------------
q <- find_q(tn_survey, "label", "Region.+?Ortsgruppe")
col_name_region <- q$`$xpath`
# recode and assign
m <- get_mapping(tn_choices, q$select_from_list_name)
TN_DATA$data$og_region <- tn_df %>% 
    recode_values(m, col_name_region) %>% 
    pull(!!col_name_region)

# ORTSGRUPPE NAME --------
# TODO make conditional on whether select with sonst option or open text field
q <- find_q(tn_survey, "label", "heißt.+?Ortsgruppe")
col_name_ortsgruppe_name <- q$`$xpath`
TN_DATA$data$og_name <- tn_df %>% 
    pull(!!col_name_ortsgruppe_name)



# ORTSUEBERGREIFENDE ANGEBOTE ------------
q <- find_q(tn_survey, "label", "ortsgruppenübergreifenden")
col_name_ogueber <- q$`$xpath`
# recode and assign
m <- get_mapping(tn_choices, q$select_from_list_name)
TN_DATA$data$teilnahme_og_uebergreifend <- tn_df %>% 
    recode_values(m, col_name_ogueber) %>% 
    pull(!!col_name_ogueber)

# deutschlandweite angebote
q <- find_q(tn_survey, "label", "Warst du.+?deutschlandweit")
col_name_dtlweit <- q$`$xpath`
# recode and assign
m <- get_mapping(tn_choices, q$select_from_list_name)
TN_DATA$data$teilnahme_dtlweit <- tn_df %>% 
    recode_values(m, col_name_dtlweit) %>% 
    pull(!!col_name_dtlweit)


# gründe gegen teilnahme an deutschlandweite oder ortsübergreifende angebote
# deutschlandweite angebote
q <- find_q(tn_survey, "label", "deutschlandweit.+?gegen.+?Gründe")
col_name_gruende_gegen_tn <- q$`$xpath`
m <- get_mapping(tn_choices, q$select_from_list_name)
gruende_long <- make_multiselect_long(tn_df, col_name_gruende_gegen_tn) %>% 
    rename(grund = !!col_name_gruende_gegen_tn)

# andere gründe
q <- find_q(tn_survey, "$xpath", "gruende.+?andere")
col_name_gruende_andere <- q$`$xpath`
gruende_andere <- tn_df %>% 
    filter(!is.na(.data[[col_name_gruende_andere]])) %>% 
    mutate(grund = "andere_gr_nde") %>% 
    select(tn_id, grund, andere_gruende = !!col_name_gruende_andere)  

gruende_long <- left_join(gruende_long, gruende_andere, by = c("tn_id", "grund"))  %>% 
    recode_values(m, "grund")


TN_DATA$long$teilnahme_dtlweit_gruende_dagegen <- gruende_long

# DU UND DEINE TENSING GRUPPE -----------
# Verantwortung ja/nein
m <- get_mapping(tn_choices, Q_VERANTWORTUNG_JANEIN$select_from_list_name)
TN_DATA$data$verantwortung_janein <- tn_df %>% 
    recode_values(m, CN_VERANTWORTUNG_JANEIN) %>% 
    pull(!!CN_VERANTWORTUNG_JANEIN)

# Hilfe für mehr Verantwortungsübernahme
# sonstiges 
hilfe_sonst <- tn_df %>%
    filter(!is.na(!!CN_VERANTWORTUNG_SUPPORT_SONST)) %>% 
    select(tn_id, hilfsangebot_sonst = !!CN_VERANTWORTUNG_SUPPORT_SONST) %>% 
    mutate(hilfsangebot = "sonstiges")

# all other ones 
m_hilfe <- get_mapping(tn_choices, Q_VERANTWORTUNG_SUPPORT$select_from_list_name)
hilfe_long <- make_multiselect_long(tn_df, CN_VERANTWORTUNG_SUPPORT) %>% 
    rename(hilfsangebot = !!CN_VERANTWORTUNG_SUPPORT)

hilfe_long <- hilfe_long %>% 
    left_join(hilfe_sonst, by = c("tn_id", "hilfsangebot")) %>% 
    recode_values(m_hilfe, "hilfsangebot")
TN_DATA$long$verantwortung_hilfsangebote <- hilfe_long

# Informationen --------------------------
# Informationswege aktuell
m <- get_mapping(tn_choices, Q_INFO_WEGE_AKTUELL$select_from_list_name)
TN_DATA$long$info_wege_aktuell <- tn_df %>% 
    make_multiselect_long(CN_INFO_WEGE_AKTUELL) %>% 
    recode_values(m, CN_INFO_WEGE_AKTUELL) %>% 
    rename(info_weg = !!CN_INFO_WEGE_AKTUELL)

# Info komplett?
m <- get_mapping(tn_choices, Q_INFO_KOMPLETT$select_from_list_name)
TN_DATA$data$info_komplett <- tn_df %>% 
    recode_values(m, CN_INFO_KOMPLETT) %>% 
    pull(!!CN_INFO_KOMPLETT)

# Informationswege Änderungswunsch? 
m <- get_mapping(tn_choices, Q_INFO_WEGE_AENDERUNG$select_from_list_name)
TN_DATA$data$info_wege_aenderung <- tn_df %>% 
    recode_values(m, CN_INFO_WEGE_AENDERUNG) %>% 
    pull(!!CN_INFO_WEGE_AENDERUNG)

# Informationswege neu / wunsch
m <- get_mapping(tn_choices, Q_INFO_WEGE_WUNSCH$select_from_list_name)
TN_DATA$long$info_wege_wunsch <- tn_df %>% 
    make_multiselect_long(CN_INFO_WEGE_WUNSCH) %>% 
    recode_values(m, CN_INFO_WEGE_WUNSCH) %>% 
    rename(info_weg = !!CN_INFO_WEGE_WUNSCH)

# Weiterempfehlung
TN_DATA$data$wahrsch_empfehlung <- tn_df %>% 
    pull(!!CN_EMPFEHLUNG) %>% as.numeric() # we use the numeric values 


# DEMOGRAPHIE ------------

# Geschlecht
m <- get_mapping(tn_choices, Q_DEMO_GENDER$select_from_list_name)
gender <- tn_df %>% 
    recode_values(m, CN_DEMO_GENDER) %>% 
    pull(!!CN_DEMO_GENDER)
TN_DATA$demo$gender <- sample(gender, length(gender))

# Alter
alter <- tn_df %>% 
    pull(!!CN_DEMO_ALTER) %>% 
    as.integer()

# recode implausible values as NA
# TODO BUCKETS
alter[alter > INVALID_DEMO_ALTER] <- NA
TN_DATA$demo$alter <- sample(alter, length(alter))

# Schule
m <- get_mapping(tn_choices, Q_DEMO_SCHULE$select_from_list_name)

schule <- tn_df %>% 
    recode_values(m, CN_DEMO_SCHULE) %>% 
    pull(!!CN_DEMO_SCHULE)

TN_DATA$demo$schule <- sample(schule, length(schule))

# Kontakt christlicher glauben
# Hilfe für mehr Verantwortungsübernahme
# sonstiges 
kontakt_sonst <- tn_df %>%
    filter(!is.na(.data[[CN_CHR_GLAUBEN_KONTAKT_SONST]])) %>% 
    pull(!!CN_CHR_GLAUBEN_KONTAKT_SONST)
TN_DATA$demo$kontakt_chrgl_sonst <- sample(kontakt_sonst, length(kontakt_sonst))

# all other ones 
m_kontakt <- get_mapping(tn_choices, Q_CHR_GLAUBEN_KONTAKT$select_from_list_name)
kontaktpunkte <- make_multiselect_long(tn_df, CN_CHR_GLAUBEN_KONTAKT) %>% 
    recode_values(m_kontakt, CN_CHR_GLAUBEN_KONTAKT) %>% 
    rename(kontaktpunkt = !!CN_CHR_GLAUBEN_KONTAKT)  %>% 
    pull(kontaktpunkt)

TN_DATA$demo$kontakt_chrgl <- sample(kontaktpunkte, length(kontaktpunkte))


readr::write_rds(TN_DATA, "data/cleaned/tn.rds")
