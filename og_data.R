library(kbtbr)
library(dotenv)
library(dplyr)
library(fuzzyjoin)

source("utils.R")
dotenv::load_dot_env()
kobo <- kbtbr::Kobo$new("https://eu.kobotoolbox.org/", kobo_token = Sys.getenv("KOBO_EU_TOKEN"))

# Ortsgruppe Survey Daten ---------
og_id <- Sys.getenv("2024_OG")
og_asset <- kobo$get(sprintf("assets/%s", og_id))
og_survey <- og_asset$content$survey
og_survey <- og_survey %>% 
    mutate(label = label %>% purrr::map_chr(function(x) {
        if(is.null(x)) x <- NA
        stopifnot(length(x) == 1) # only one label per question
        return(x)
    })) 

# for reference only
og_survey %>% 
    purrr::transpose() %>% 
    as.list() %>% 
    jsonlite::toJSON(auto_unbox = TRUE) %>% 
    readr::write_lines("data/meta/og_survey.json")

og_choices <- og_asset$content$choices
og_choices$label <- unlist(og_choices$label)
og_choices %>% readr::write_csv("data/meta/og_choices.csv")

# Ortsgruppe Daten -----------
og_df <- kobo$get_submissions(og_id)

# drop system columns
og_df <- og_df %>% 
    rename(og_id = `_id`) %>% 
    drop_system_columns()
og_df %>% readr::write_csv(file = "data/raw/og_api.csv")


OG_DATA <- list(
    long = list(),
    data = tibble(og_id = og_df$og_id) # tibble with the rest of the variables
)

# AKTIVITÄTEN ------
# Wochentage
q <- find_q(og_survey, "name", "Wochentag")
stopifnot(q$type == "select_multiple")

col_name_wt <- q$`$xpath`
wochentage <- og_df %>% 
    make_multiselect_long(col_name_wt) %>% 
    rename(wochentag = !!col_name_wt) %>% 
    mutate(wochentag = str_to_title(str_replace_all(wochentag, ".+?/(.+?)_[sS]tunden", "\\1")))

# stunden questions
qs <- find_qs(og_survey, "name", ".+?_[sS]tunden")
wochentage_stunden <- pivot_cols_long(og_df, qs$`$xpath`, values_to = "stunden")
wochentage_stunden <- wochentage_stunden %>% 
    rename(wochentag = col_name) %>% 
    mutate(stunden = as.integer(stunden))  %>% 
    mutate(wochentag = str_to_title(str_replace_all(wochentag, ".+?/(.+?)_[sS]tunden", "\\1")))

# join
wochentage <- dplyr::left_join(wochentage, wochentage_stunden, by = c("og_id", "wochentag"))
OG_DATA$long[["wochentage"]] <- wochentage

# Auftritte ---------------
q <- find_q(og_survey, "label", "Auftritte.+?Schuljahr")
col_name_auf <- q$`$xpath`
OG_DATA$data$auftritte <- og_df[[col_name_auf]] %>% as.integer()

# Workshops ---------------
q <- find_q(og_survey, "name", "angebote_vor_ort")
col_name_avo <- q$`$xpath`
OG_DATA$long[["angebote_vor_ort"]] <- make_multiselect_long(og_df, col_name_avo) %>% 
    rename(angebote_vor_ort = !!col_name_avo)

# HAUPTBERUFLICH MA -------------
# gibt es hauptamtl. MA?
q <- find_q(og_survey, "label", "hauptberuflich")
col_name_hbma <- q$`$xpath`
m <- get_mapping(og_choices, q$select_from_list_name)
OG_DATA$data$hat_hauptamt <- og_df %>% 
    recode_values(m, col_name_hbma) %>%
    pull(!!col_name_hbma) 


# wenn ja, wie viele Stunden?
q <- find_q(og_survey, "name", "Hauptamt_Wochenstunden")
col_name_st <- q$`$xpath`
OG_DATA$data$hauptamt_stunden <- og_df[[col_name_st]] %>% as.numeric()


# AKTIVE PERSONEN --------------
# mitarbeitende + tn
q <- find_q(og_survey, "$xpath", "Mitarbeitende_inkl_Leitung")
col_name_ma <- q$`$xpath`
OG_DATA$data$anzahl_ma_leitung <- og_df[[col_name_ma]] %>% as.integer()

# TODO weird bug where xpath has 002 suffix but data has not :eyes:
col_name_tn <- "group_jx3lf36/Teilnehmende"
OG_DATA$data$anzahl_tn <- og_df[[col_name_tn]] %>% as.integer()

# GESCHLECHT MATRIX -------------
gender_mat_begin <- find_qs(og_survey, "type", "begin_kobomatrix") %>% 
    filter(str_detect(label, "Geschlecht"))
# name of the matrix. this is in all columns related to the matrix, so we can regex the colnames
gender_mat_group_id <- gender_mat_begin$name 
gender_mat_cols <- colnames(og_df)[str_detect(colnames(og_df), gender_mat_group_id)]


# we use fuzzyjoins to merge the correct labels for the gender and the person_type
gender_options <- get_mapping(og_choices, gender_mat_begin$`kobo--matrix_list`)
person_type_options <- find_matrix_header_qs(og_survey, gender_mat_begin$`$kuid`) %>% 
    select(name, label)

# gender long
gender_long <- pivot_cols_long(og_df, gender_mat_cols, values_to = "n") 
gender_long <- gender_long %>% 
    fuzzyjoin::regex_inner_join(gender_options, by = c("col_name" = "name")) %>% 
    rename(gender = label) %>% 
    fuzzyjoin::regex_inner_join(person_type_options, by = c("col_name" = "name")) %>% 
    rename(person_type = label) %>% 
    mutate(n = as.integer(n)) %>% 
    select(og_id, person_type, gender, n) %>% 
    arrange(og_id, person_type, gender)

OG_DATA$long$gender_by_participant_type <- gender_long

# ALTER MATRIX --------------
alter_mat_begin <- find_qs(og_survey, "type", "begin_kobomatrix") %>% 
    filter(str_detect(label, "Alter"))
# name of the matrix. this is in all columns related to the matrix, so we can regex the colnames
alter_mat_group_id <- alter_mat_begin$name 
alter_mat_cols <- colnames(og_df)[str_detect(colnames(og_df), alter_mat_group_id)]


# we use fuzzyjoins to merge the correct labels for the alter and the person_type
alter_options <- get_mapping(og_choices, alter_mat_begin$`kobo--matrix_list`)
person_type_options <- find_matrix_header_qs(og_survey, alter_mat_begin$`$kuid`) %>% 
    select(name, label)

# alter long
alter_long <- pivot_cols_long(og_df, alter_mat_cols, values_to = "n") 

alter_long <- alter_long %>% 
    fuzzyjoin::regex_inner_join(alter_options, by = c("col_name" = "name")) %>% 
    rename(alter = label) %>% 
    fuzzyjoin::regex_inner_join(person_type_options, by = c("col_name" = "name")) %>% 
    rename(person_type = label) %>% 
    mutate(n = as.integer(n)) %>% 
    select(og_id, person_type, alter, n) %>% 
    arrange(og_id, person_type, alter)

OG_DATA$long$alter_by_participant_type <- alter_long

# GEWINNUNG VON TEILNEHMENDEN -------------------
# Probleme bei TN-Gewinnung --------
q <- find_q(og_survey, "$xpath", "[Gg]ewinnung.+?[Pp]robleme")
col_name_prob <- q$`$xpath`
m <- get_mapping(og_choices, q$select_from_list_name)
OG_DATA$data$hat_probleme_tn_gewinnung <- og_df %>% 
    recode_values(m, col_name_prob) %>% 
    pull(!!col_name_prob)

# Maßnahmen TN Gewinnung --------
q_massnahmen <- find_qs(og_survey, "$xpath", "[Gg]ewinnung.+?[Mm]assnahmen$")
col_name_mass <- q_massnahmen$`$xpath`
massnahmen_long <- make_multiselect_long(og_df, col_name_mass) %>% 
    rename(massnahme = !!col_name_mass)

# add open text field sonstige as separate column
q <- find_q(og_survey, "$xpath", "[Gg]ewinnung.+?[Mm]assnahmen_sons$")
col_name_mass_sonst <- q$`$xpath`
mass_sonst <- og_df %>% 
    filter(!is.na(.data[[col_name_mass_sonst]])) %>% 
    mutate(massnahme = "sonstige") %>% 
    select(og_id, massnahme, massnahme_sonst = !!col_name_mass_sonst)  

massnahmen_long <- left_join(massnahmen_long, mass_sonst, by = c("og_id", "massnahme"))

# add Erfolg of Massnahmen
qs <- find_qs(og_survey, "$xpath", "Erfolg.+?[Mm]assnahm")
qs <- qs %>% filter(type == "select_one")

qs <- qs %>%
    mutate(massnahme = str_replace_all(relevant, ".+?\\'(.+?)\\'.$", "\\1")) %>% 
    mutate(massnahme = if_else(is.na(massnahme), "insgesamt", massnahme))

massnahmen_erfolg  <-  pivot_cols_long(og_df, qs$`$xpath`, values_to = "erfolg")  %>% 
    mutate(erfolg = as.integer(erfolg)) %>% 
    left_join(qs %>% select("$xpath", massnahme), by = c("col_name" = "$xpath")) %>% 
    select(og_id, erfolg, massnahme)

massnahmen <- left_join(massnahmen_erfolg, massnahmen_long, by = c("og_id", "massnahme"))

# finally recode values to use labels 
# we get the labels from the multiple choice question
# we have to add label for insgesamt manually
m_massnahmen <- get_mapping(og_choices, q_massnahmen$select_from_list_name)
m_massnahmen <- bind_rows(m_massnahmen, tibble(name = "insgesamt", label = "Insgesamt"))
massnahmen <- massnahmen %>% recode_values(m_massnahmen, "massnahme")
OG_DATA$long$tn_gewinnung_massnahmen <- massnahmen_long

# UNTERSTUETZUNGSBEDARFE -----------------
qs <- find_qs(og_survey, "$xpath", "Unterstuetzungsbedarfe")
qs_ms <- qs %>% 
    filter(type == "select_one") %>% 
    filter(`$xpath` %in% colnames(og_df))

bedarfe_long  <-  pivot_cols_long(og_df, qs_ms$`$xpath`, values_to = "bedarf_value")  %>% 
    left_join(qs_ms %>% select("$xpath", bedarf = label), by = c("col_name" = "$xpath")) %>% 
    select(og_id, bedarf, bedarf_value)
m <- get_mapping(og_choices, qs_ms$select_from_list_name %>% unique())
bedarfe_long <- bedarfe_long %>% recode_values(m, "bedarf_value")
OG_DATA$long$unterstuetzungsbedarfe <- bedarfe_long

# weitere
q <- find_q(og_survey, "$xpath", "[wW]eitereUnters")
col_name_weitere_ub <- q$`$xpath`
OG_DATA$data$unterstuetzungsbedarfe_weitere <- og_df[[col_name_weitere_ub]]

# ORTSGRUPPE --------------
q <- find_q(og_survey, "$xpath", "region")
col_name_og_region <- q$`$xpath`
m <- get_mapping(og_choices, q$select_from_list_name)
OG_DATA$data$og_region <- og_df %>% recode_values(m, col_name_og_region) %>% 
    pull(!!col_name_og_region)


q <- find_q(og_survey, "$xpath", "ortsgruppe_name")
OG_DATA$data$og_name <- og_df[[q$`$xpath`]]


OG_DATA %>% readr::write_rds("data/cleaned/og.rds")
