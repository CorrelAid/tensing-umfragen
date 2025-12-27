#' get the response data for the og survey
#' wrangle data
#' store in og.rds list object which has:
#' - long: individual long-format data frames for multi-select or question matrix questions
#' - data: all questions that can be represented with one row per local group
#' general workflow: for each question
#' 1. extract question columns using info from cfg object
#' 2. put into target format (e.g. long format data frame):
#' 3. store in list object
library(kbtbr)
library(dotenv)
library(dplyr)
library(fuzzyjoin)

source("R/utils.R")
source("config/og_config.R")
dotenv::load_dot_env()

og_id <- Sys.getenv(paste0(YEAR, "_OG"))

# connect to kobo
kobo <- kbtbr::Kobo$new(
    "https://eu.kobotoolbox.org/",
    kobo_token = Sys.getenv("KOBO_EU_TOKEN")
)

# METADATA AND SETUP ----
og_survey <- readr::read_csv(file.path(DIR_META, "og_survey.csv"))
og_choices <- readr::read_csv(file.path(DIR_META, "og_choices.csv"))


# config
og_cfg <- readr::read_rds(file.path(DIR_CONFIG, "og_cfg.rds"))

# Ortsgruppe Daten -----------
og_df <- kobo$get_submissions(og_id)
# remove submissions after submission deadline (set as January of the following year)
og_df<- og_df %>%
    filter(end < as.Date(paste0(YEAR + 1, "-01-01")))

# drop system columns
og_df <- og_df %>%
    rename(og_id = `_id`) %>%
    drop_system_columns()
og_df %>% readr::write_csv(file = file.path(DIR_RAW, "og_api.csv")) # for reference

# filter out observations that are empty / only have NA except for id
og_df <- og_df %>%
    filter(if_any(-og_id, ~ !is.na(.x)))

OG_DATA <- list(
    long = list(),
    data = tibble(og_id = og_df$og_id) # tibble with the rest of the variables
)

# AKTIVITÄTEN ------
# Wochentage
#' for multiselect (checkbox) questions, selected values are in one column,
#' space separated. -> function make_multiselect_long pulls them into long format
wochentage <- og_df %>%
    make_multiselect_long(og_cfg$CN_WOCHENTAG) %>%
    rename(wochentag = !!og_cfg$CN_WOCHENTAG) %>%
    mutate(
        wochentag = str_to_title(str_replace_all(
            wochentag,
            ".+?/(.+?)_[sS]tunden",
            "\\1"
        ))
    )

# stunden questions
#' we have other questions that have multiple columns
#' either because they're question matrixes or because they
#' logically belong together.
#' for those cases, the pivot_cols_long function collects
#' the columns and puts them into long format.

#' Ensure all expected columns exist in og_df
missing_cols <- setdiff(og_cfg$CN_WOCHENTAG_STUNDEN, names(og_df))
if (length(missing_cols) > 0) {
    og_df[missing_cols] <- NA
}
wochentage_stunden <- pivot_cols_long(
    og_df,
    og_cfg$CN_WOCHENTAG_STUNDEN,
    values_to = "stunden"
)

wochentage_stunden <- wochentage_stunden %>%
    rename(wochentag = col_name) %>%
    mutate(stunden = as.integer(stunden)) %>%
    mutate(
        wochentag = str_to_title(str_replace_all(
            wochentag,
            ".+?/(.+?)_[sS]tunden",
            "\\1"
        ))
    )

# join
wochentage <- dplyr::left_join(
    wochentage,
    wochentage_stunden,
    by = c("og_id", "wochentag")
)
OG_DATA$long[["wochentage"]] <- wochentage

# Auftritte ---------------
OG_DATA$data$auftritte <- og_df[[og_cfg$CN_AUFTRITTE]] %>% as.integer()

# Workshops ---------------
angebote_m <- get_mapping(og_choices, og_cfg$Q_WORKSHOPS$select_from_list_name)

OG_DATA$long[["angebote_vor_ort"]] <- make_multiselect_long(
    og_df,
    og_cfg$CN_WORKSHOPS
) %>%
    rename(angebote_vor_ort = !!og_cfg$CN_WORKSHOPS) |>
    recode_values(angebote_m, "angebote_vor_ort")

# HAUPTBERUFLICH MA -------------
# gibt es hauptamtl. MA?
m <- get_mapping(og_choices, og_cfg$Q_HAUPTAMT_JANEIN$select_from_list_name)
OG_DATA$data$hat_hauptamt <- og_df %>%
    recode_values(m, og_cfg$CN_HAUPTAMT_JANEIN) %>%
    pull(!!og_cfg$CN_HAUPTAMT_JANEIN)


# wenn ja, wie viele Stunden?
OG_DATA$data$hauptamt_stunden <- og_df[[cfg$CN_HAUPTAMT_STUNDEN]] %>%
    as.numeric()


# AKTIVE PERSONEN --------------
# mitarbeitende + tn
OG_DATA$data$anzahl_ma_leitung <- og_df[[og_cfg$CN_ANZAHL_MA]] %>% as.integer()

OG_DATA$data$anzahl_tn <- og_df[[cfg$CN_ANZAHL_TN]] %>% as.integer()
# GESCHLECHT MATRIX -------------
# name of the matrix. this is in all columns related to the matrix, so we can regex the colnames
gender_mat_group_id <- og_cfg$Q_GESCHLECHT_BEGIN$name
gender_mat_cols <- colnames(og_df)[str_detect(
    colnames(og_df),
    og_cfg$Q_GESCHLECHT_BEGIN$name
)]

# we use fuzzyjoins to merge the correct labels for the gender and the person_type
gender_options <- get_mapping(og_choices, og_cfg$Q_GESCHLECHT_BEGIN$`kobo--matrix_list`)
person_type_options <- find_matrix_header_qs(
    og_survey,
    og_cfg$Q_GESCHLECHT_BEGIN$`$kuid`
) %>%
    select(name, label)

# gender long
gender_long <- pivot_cols_long(og_df, gender_mat_cols, values_to = "n")
gender_long <- gender_long %>%
    fuzzyjoin::regex_inner_join(gender_options, by = c("col_name" = "name")) %>%
    rename(gender = label) %>%
    fuzzyjoin::regex_inner_join(
        person_type_options,
        by = c("col_name" = "name")
    ) %>%
    rename(person_type = label) %>%
    mutate(n = as.integer(n)) %>%
    select(og_id, person_type, gender, n) %>%
    arrange(og_id, person_type, gender)

OG_DATA$long$gender_by_participant_type <- gender_long

# ALTER MATRIX --------------
# name of the matrix. this is in all columns related to the matrix, so we can regex the colnames
alter_mat_cols <- colnames(og_df)[str_detect(
    colnames(og_df),
    og_cfg$Q_ALTER_BEGIN$name
)]


# we use fuzzyjoins to merge the correct labels for the alter and the person_type
alter_options <- get_mapping(og_choices, og_cfg$Q_ALTER_BEGIN$`kobo--matrix_list`)
person_type_options <- find_matrix_header_qs(
    og_survey,
    og_cfg$Q_ALTER_BEGIN$`$kuid`
) %>%
    select(name, label)

# alter long
alter_long <- pivot_cols_long(og_df, alter_mat_cols, values_to = "n")

alter_long <- alter_long %>%
    fuzzyjoin::regex_inner_join(alter_options, by = c("col_name" = "name")) %>%
    rename(alter = label) %>%
    fuzzyjoin::regex_inner_join(
        person_type_options,
        by = c("col_name" = "name")
    ) %>%
    rename(person_type = label) %>%
    mutate(n = as.integer(n)) %>%
    select(og_id, person_type, alter, n) %>%
    arrange(og_id, person_type, alter)

OG_DATA$long$alter_by_participant_type <- alter_long

# GEWINNUNG VON TEILNEHMENDEN -------------------
# Probleme bei TN-Gewinnung --------

m <- get_mapping(og_choices, cfg$Q_TN_GEWINNUNG_PROB$select_from_list_name)
OG_DATA$data$hat_probleme_tn_gewinnung <- og_df %>%
    recode_values(m, cfg$CN_TN_GEWINNUNG_PROB) %>%
    pull(!!cfg$CN_TN_GEWINNUNG_PROB)

# Maßnahmen TN Gewinnung --------
massnahmen_long <- make_multiselect_long(
    og_df,
    cfg$CN_TN_GEWINNUNG_MASSNAHMEN
) %>%
    rename(massnahme = !!cfg$CN_TN_GEWINNUNG_MASSNAHMEN)

# add open text field sonstige as separate column
mass_sonst <- og_df %>%
    filter(!is.na(.data[[cfg$CN_TN_GEWINNUNG_MASSNAHMEN_SONST]])) %>%
    mutate(massnahme = "sonstige") %>%
    select(
        og_id,
        massnahme,
        massnahme_sonst = !!cfg$CN_TN_GEWINNUNG_MASSNAHMEN_SONST
    )

massnahmen_long <- left_join(
    massnahmen_long,
    mass_sonst,
    by = c("og_id", "massnahme")
)

# add Erfolg of Massnahmen
# the questionnaire always shows the "insgesamt" / overall evaluation of the
# massnahmen. we have to recode this.

mass_erfolg_cols <- cfg$QS_TN_GEWINNUNG_MASSNAHMEN_ERFOLG %>%
    mutate(massnahme = str_replace_all(relevant, ".+?\\'(.+?)\\'.$", "\\1")) %>%
    mutate(massnahme = if_else(is.na(massnahme), "insgesamt", massnahme))

massnahmen_erfolg <- pivot_cols_long(
    og_df,
    mass_erfolg_cols$col_name,
    values_to = "erfolg"
) %>%
    mutate(erfolg = as.integer(erfolg)) %>%
    left_join(
        mass_erfolg_cols %>% select("col_name", massnahme),
        by = c("col_name" = "col_name")
    ) %>%
    select(og_id, erfolg, massnahme)

massnahmen <- left_join(
    massnahmen_erfolg,
    massnahmen_long,
    by = c("og_id", "massnahme")
)

# finally recode values to use labels
# we get the labels from the multiple choice question
# we have to add label for insgesamt manually
m_massnahmen <- get_mapping(
    og_choices,
    cfg$Q_TN_GEWINNUNG_MASSNAHMEN$select_from_list_name
)
m_massnahmen <- bind_rows(
    m_massnahmen,
    tibble(name = "insgesamt", label = "Insgesamt")
)
massnahmen <- massnahmen %>% recode_values(m_massnahmen, "massnahme")
OG_DATA$long$tn_gewinnung_massnahmen <- massnahmen

# UNTERSTUETZUNGSBEDARFE -----------------
bedarfe_long <- pivot_cols_long(
    og_df,
    cfg$CN_UNTERSTUETZUNGSBEDARFE,
    values_to = "bedarf_value"
) %>%
    left_join(
        cfg$QS_UNTERSTUETZUNGSBEDARFE %>% select("col_name", bedarf = label),
        by = "col_name"
    ) %>%
    select(og_id, bedarf, bedarf_value)
m <- get_mapping(
    og_choices,
    cfg$QS_UNTERSTUETZUNGSBEDARFE$select_from_list_name %>% unique()
)
bedarfe_long <- bedarfe_long %>% recode_values(m, "bedarf_value")
OG_DATA$long$unterstuetzungsbedarfe <- bedarfe_long

# weitere
OG_DATA$data$unterstuetzungsbedarfe_weitere <- og_df[[og_cfg$CN_UNTERSTUETZUNGSBEDARFE_WEITERE]]

# ORTSGRUPPE --------------
m <- get_mapping(og_choices, cfg$Q_OG_REGION$select_from_list_name)
OG_DATA$data$og_region <- og_df %>%
    recode_values(m, cfg$CN_OG_REGION) %>%
    pull(!!cfg$CN_OG_REGION)

m <- get_mapping(og_choices, cfg$Q_OG_NAME$select_from_list_name)
if (nrow(m)) {
    OG_DATA$data$og_name <- og_df %>%
        recode_values(m, cfg$CN_OG_NAME) %>%
        pull(!!cfg$CN_OG_NAME)
} else {
    OG_DATA$data$og_name_orig <- og_df[[cfg$CN_OG_NAME]]
    OG_DATA$data$og_name_orig[OG_DATA$data$og_name_orig == ""] <- NA
}

# add grouped regions
m_regions <- readr::read_csv("data/meta/region_mapping.csv")

OG_DATA$data <- OG_DATA$data |>
    left_join(
        m_regions |> select(og_region, region_grouped),
        by = "og_region"
    )

# CALCULATE INSG AKTIVE
OG_DATA$data <- OG_DATA$data %>%
    dplyr::mutate(anzahl_insg = anzahl_tn + anzahl_ma_leitung)


OG_DATA %>% readr::write_rds(file.path(DIR_CLEANED, "og.rds"))
