#' get the response data for the tn survey
#' wrangle data
#' store in tn.rds list object which has:
#' - long: individual long-format data frames for multi-select or question matrix questions
#' - wide: wide data frames for certain topics, only used for aussagen -> should probably be changed.
#' - data: all questions that can be represented with one row per member
#' general workflow: for each question
#' 1. extract question columns using info from cfg object
#' 2. put into target format (e.g. long format data frame):
#' 3. store in list object
library(kbtbr)
library(dotenv)
library(dplyr)
library(readr)
source("R/utils.R")
source(file.path("config/tn_config.R"))

dotenv::load_dot_env()
kobo <- kbtbr::Kobo$new(
    "https://eu.kobotoolbox.org/",
    kobo_token = Sys.getenv("KOBO_EU_TOKEN")
)
tn_id <- Sys.getenv(paste0(YEAR, "_TN"))

# METADATA ----
tn_survey <- readr::read_csv(file.path(DIR_META, "tn_survey.csv"))
tn_choices <- readr::read_csv(file.path(DIR_META, "tn_choices.csv"))
tn_cfg <- readr::read_rds(file.path(DIR_CONFIG, "tn_cfg.rds"))

# TN Daten -----------
tn_df <- kobo$get_submissions(tn_id)

# drop system columns
tn_df <- tn_df %>%
    rename(tn_id = `_id`) %>%
    select(-starts_with("_"))
tn_df %>% readr::write_csv(file = file.path(DIR_RAW, "tn_api.csv")) # for reference

TN_DATA <- list(
    long = list(),
    wide = list(), # list of tibbles - wide format
    data = tibble(tn_id = tn_df$tn_id) # tibble with the rest of the variables
)

# WÖRTER ---------
TN_DATA$long$woerter <- pivot_cols_long(
    tn_df,
    tn_cfg$QS_WORT$col_name,
    "wort"
) %>%
    select(tn_id, wort)

# EIGENSCHAFTEN -----------
eig_long <- pivot_cols_long(
    tn_df,
    tn_cfg$QS_EIGENSCHAFTEN$col_name,
    values_to = "value"
)
eig_long <- eig_long %>%
    left_join(
        tn_cfg$QS_EIGENSCHAFTEN %>% select(label, name, col_name),
        by = "col_name"
    ) %>%
    select(tn_id, eig = name, eig_label = label, value)

TN_DATA$long$tensing_eigenschaften <- eig_long


# WEG ZU TENSING -------
zugangsweg_long <- make_multiselect_long(tn_df, tn_cfg$CN_ZUGANGSWEGE) %>%
    rename(zugangsweg = !!tn_cfg$CN_ZUGANGSWEGE)
# sonstige / open text feld
zugangsweg_long <- zugangsweg_long %>%
    left_join(
        tn_df %>%
            select(tn_id, zugangsweg_sonst = !!tn_cfg$CN_ZUGANGSWEGE_SONST) %>%
            mutate(zugangsweg = "sonstiges"),
        by = c("tn_id", "zugangsweg")
    )
# use labels
m <- get_mapping(tn_choices, tn_cfg$Q_ZUGANGSWEGE$select_from_list_name)
zugangsweg_long <- zugangsweg_long %>% recode_values(m, "zugangsweg")
TN_DATA$long$zugangsweg <- zugangsweg_long


# AUSSAGEN LIKERT ---------------

# just to make sure we sort the variables alphabetically before renaming
TN_DATA$wide$aussagen <- tn_df %>%
    select(tn_id, tn_cfg$CN_AUSSAGEN_LIKERT) %>%
    select(tn_id, sort(tidyselect::peek_vars())) %>%
    rename_with(~ tn_cfg$QS_AUSSAGEN_LIKERT$name, matches("^(group|block)"))

# aggregate for trends
TN_DATA$agg$aussagen <-TN_DATA$wide$aussagen %>% 
tidyr::pivot_longer(-tn_id,
                names_to = "aussage",
                values_to = "value") %>%
mutate(value = as.numeric(value)) %>%
group_by(aussage) %>%
summarise(
    total_non_na = sum(!is.na(value)),
    average      = mean(value, na.rm = TRUE),
    zustimmung    = sum(value > 3, na.rm = TRUE),
    zustimmung_perc = zustimmung/total_non_na,
    .groups = "drop")

# ANGEBOTE VOR ORT ---------------
angebote_m <- get_mapping(
    tn_choices,
    tn_cfg$Q_ANGEBOTE_VOR_ORT$select_from_list_name
)
angebote_long <- make_multiselect_long(tn_df, tn_cfg$CN_ANGEBOTE_VOR_ORT) %>%
    rename(angebot = !!tn_cfg$CN_ANGEBOTE_VOR_ORT)

# add information from sonstige question
angebote_sonst <- tn_df %>%
    filter(!is.na(.data[[tn_cfg$CN_ANGEBOTE_VOR_ORT_SONST]])) %>%
    mutate(angebot = "andere_workshops") %>%
    select(tn_id, angebot, angebot_sonst = !!tn_cfg$CN_ANGEBOTE_VOR_ORT_SONST)

angebote_long <- left_join(
    angebote_long,
    angebote_sonst,
    by = c("tn_id", "angebot")
)

# wenn eine antwortoption "/" hat, dann wird es durch ___ ersetzt und nicht nur durch _ im Spaltennamen
angebote_long <- angebote_long %>%
    mutate(angebot = str_replace_all(angebot, "___", "_")) %>%
    recode_values(angebote_m, "angebot")

TN_DATA$long$angebote_vor_ort <- angebote_long

# STUNDEN -----------
q <- find_q(tn_survey, "col_name", "stunden")
col_name_stunden <- q$`col_name`

TN_DATA$data$stunden_pro_woche <- as.numeric(tn_df[[col_name_stunden]])

# REGION ------------
q <- find_q(tn_survey, "label", "Region.+?Ortsgruppe")
col_name_region <- q$`col_name`
# recode and assign
m <- get_mapping(tn_choices, q$select_from_list_name)
TN_DATA$data$og_region <- tn_df %>%
    recode_values(m, col_name_region) %>%
    pull(!!col_name_region)

# ORTSGRUPPE NAME --------
# TODO make conditional on whether select with sonst option or open text field

TN_DATA$data$og_name_orig <- tn_df %>%
    pull(!!tn_cfg$CN_OG_NAME)
TN_DATA$data$og_name_orig[TN_DATA$data$og_name_orig == ""] <- NA

TN_DATA$data <- TN_DATA$data %>%
    dplyr::mutate(
        og_name_orig = if_else(!is.na(og_name_orig), og_name_orig, "")
    ) %>%
    mutate(
        og_name_cleaned = str_remove_all(
            og_name_orig,
            stringr::regex("tensing|ten sing|ten-sing", ignore_case = TRUE)
        ) %>%
            stringr::str_trim()
    )

# ORTSUEBERGREIFENDE ANGEBOTE ------------
q <- find_q(tn_survey, "label", "ortsgruppenübergreifenden")
col_name_ogueber <- q$`col_name`
# recode and assign
m <- get_mapping(tn_choices, q$select_from_list_name)
TN_DATA$data$teilnahme_og_uebergreifend <- tn_df %>%
    recode_values(m, col_name_ogueber) %>%
    pull(!!col_name_ogueber)

# deutschlandweite angebote
q <- find_q(tn_survey, "label", "Warst du.+?deutschlandweit")
col_name_dtlweit <- q$`col_name`
# recode and assign
m <- get_mapping(tn_choices, q$select_from_list_name)
TN_DATA$data$teilnahme_dtlweit <- tn_df %>%
    recode_values(m, col_name_dtlweit) %>%
    pull(!!col_name_dtlweit)


# gründe gegen teilnahme an deutschlandweite oder ortsübergreifende angebote
# deutschlandweite angebote
q <- find_q(tn_survey, "label", "deutschlandweit.+?gegen.+?Gründe")
col_name_gruende_gegen_tn <- q$`col_name`
m <- get_mapping(tn_choices, q$select_from_list_name)
gruende_long <- make_multiselect_long(tn_df, col_name_gruende_gegen_tn) %>%
    rename(grund = !!col_name_gruende_gegen_tn)

# andere gründe
q <- find_q(tn_survey, "col_name", "gruende.+?andere")
col_name_gruende_andere <- q$`col_name`
gruende_andere <- tn_df %>%
    filter(!is.na(.data[[col_name_gruende_andere]])) %>%
    mutate(grund = "andere_gr_nde") %>%
    select(tn_id, grund, andere_gruende = !!col_name_gruende_andere)

gruende_long <- left_join(
    gruende_long,
    gruende_andere,
    by = c("tn_id", "grund")
) %>%
    recode_values(m, "grund")


TN_DATA$long$teilnahme_dtlweit_gruende_dagegen <- gruende_long

# DU UND DEINE TENSING GRUPPE -----------
# Verantwortung ja/nein
m <- get_mapping(
    tn_choices,
    tn_cfg$Q_VERANTWORTUNG_JANEIN$select_from_list_name
)
TN_DATA$data$verantwortung_janein <- tn_df %>%
    recode_values(m, tn_cfg$CN_VERANTWORTUNG_JANEIN) %>%
    pull(!!tn_cfg$CN_VERANTWORTUNG_JANEIN)

# Hilfe für mehr Verantwortungsübernahme
# sonstiges
hilfe_sonst <- tn_df %>%
    filter(!is.na(!!tn_cfg$CN_VERANTWORTUNG_SUPPORT_SONST)) %>%
    select(
        tn_id,
        hilfsangebot_sonst = !!tn_cfg$CN_VERANTWORTUNG_SUPPORT_SONST
    ) %>%
    mutate(hilfsangebot = "sonstiges")

# all other ones
m_hilfe <- get_mapping(
    tn_choices,
    tn_cfg$Q_VERANTWORTUNG_SUPPORT$select_from_list_name
)
hilfe_long <- make_multiselect_long(tn_df, tn_cfg$CN_VERANTWORTUNG_SUPPORT) %>%
    rename(hilfsangebot = !!tn_cfg$CN_VERANTWORTUNG_SUPPORT)

hilfe_long <- hilfe_long %>%
    left_join(hilfe_sonst, by = c("tn_id", "hilfsangebot")) %>%
    recode_values(m_hilfe, "hilfsangebot")
TN_DATA$long$verantwortung_hilfsangebote <- hilfe_long

# Informationen --------------------------
# Informationswege aktuell
m <- get_mapping(tn_choices, tn_cfg$Q_INFO_WEGE_AKTUELL$select_from_list_name)
TN_DATA$long$info_wege_aktuell <- tn_df %>%
    make_multiselect_long(tn_cfg$CN_INFO_WEGE_AKTUELL) %>%
    recode_values(m, tn_cfg$CN_INFO_WEGE_AKTUELL) %>%
    rename(info_weg = !!tn_cfg$CN_INFO_WEGE_AKTUELL)

# Info komplett?
m <- get_mapping(tn_choices, tn_cfg$Q_INFO_KOMPLETT$select_from_list_name)
TN_DATA$data$info_komplett <- tn_df %>%
    recode_values(m, tn_cfg$CN_INFO_KOMPLETT) %>%
    pull(!!tn_cfg$CN_INFO_KOMPLETT)

# Informationswege Änderungswunsch?
m <- get_mapping(tn_choices, tn_cfg$Q_INFO_WEGE_AENDERUNG$select_from_list_name)
TN_DATA$data$info_wege_aenderung <- tn_df %>%
    recode_values(m, tn_cfg$CN_INFO_WEGE_AENDERUNG) %>%
    pull(!!tn_cfg$CN_INFO_WEGE_AENDERUNG)


# Informationswege neu / wunsch
m <- get_mapping(tn_choices, tn_cfg$Q_INFO_WEGE_WUNSCH$select_from_list_name)
TN_DATA$long$info_wege_wunsch <- tn_df %>%
    make_multiselect_long(tn_cfg$CN_INFO_WEGE_WUNSCH) %>%
    recode_values(m, tn_cfg$CN_INFO_WEGE_WUNSCH) %>%
    rename(info_weg = !!tn_cfg$CN_INFO_WEGE_WUNSCH)

# Weiterempfehlung
TN_DATA$data$wahrsch_empfehlung <- tn_df %>%
    pull(!!tn_cfg$CN_EMPFEHLUNG) %>%
    as.numeric() # we use the numeric values


# DEMOGRAPHIE ------------

# Geschlecht
m <- get_mapping(tn_choices, tn_cfg$Q_DEMO_GENDER$select_from_list_name)
gender <- tn_df %>%
    recode_values(m, tn_cfg$CN_DEMO_GENDER) %>%
    pull(!!tn_cfg$CN_DEMO_GENDER)
TN_DATA$demo$gender <- sample(gender, length(gender))

# Alter
alter <- tn_df %>%
    pull(!!tn_cfg$CN_DEMO_ALTER) %>%
    as.integer()

# recode implausible values as NA
# TODO BUCKETS
alter[alter > tn_cfg$INVALID_DEMO_ALTER] <- NA
TN_DATA$demo$alter <- sample(alter, length(alter))

# Schule
m <- get_mapping(tn_choices, tn_cfg$Q_DEMO_SCHULE$select_from_list_name)

schule <- tn_df %>%
    recode_values(m, tn_cfg$CN_DEMO_SCHULE) %>%
    pull(!!tn_cfg$CN_DEMO_SCHULE)

TN_DATA$demo$schule <- sample(schule, length(schule))

# Hilfe für mehr Verantwortungsübernahme
# KONTAKT CHRISTLICHER GLAUBEN -----
m_kontakt <- get_mapping(
    tn_choices,
    tn_cfg$Q_CHR_GLAUBEN_KONTAKT$select_from_list_name
)
kontakt_sonst <- tn_df %>% # sonstiges
    select(
        tn_id,
        kontakt_chrgl_sonst = !!tn_cfg$CN_CHR_GLAUBEN_KONTAKT_SONST
    ) |>
    filter(!is.na(kontakt_chrgl_sonst)) |>
    mutate(kontaktpunkt = "ja_andere")

TN_DATA$demo$kontakt_chrgl_sonst <- sample(
    kontakt_sonst$kontakt_chrgl_sonst,
    length(kontakt_sonst$kontakt_chrgl_sonst)
)

# all other ones
kontaktpunkte_long <- make_multiselect_long(
    tn_df,
    tn_cfg$CN_CHR_GLAUBEN_KONTAKT
) %>%
    rename(kontaktpunkt = !!tn_cfg$CN_CHR_GLAUBEN_KONTAKT) |>
    left_join(kontakt_sonst, by = c("tn_id", "kontaktpunkt")) |>
    recode_values(m_kontakt, "kontaktpunkt")


TN_DATA$demo$kontakt_chrgl <- sample(
    kontaktpunkte_long$kontaktpunkt,
    length(kontaktpunkte_long$kontaktpunkt)
)
TN_DATA$long$kontakt_chrgl <- kontaktpunkte_long

readr::write_rds(TN_DATA, file.path(DIR_CLEANED, "tn.rds"))
