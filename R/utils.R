library(stringr)
library(purrr)
library(here)

#' Find columns for a matrix question.
#' @param meta_survey A data frame containing survey metadata.
#' @param col_to_regex A string naming the column in \code{meta_survey} to search.
#' sometimes, we want to regex the question text, sometimes the column name.
#' @param regex A regular expression pattern to match within \code{col_to_regex}.
#' @param type Optional. A string specifying the question type to filter by (e.g., "select_one").
#' If NULL, no type filtering is applied.
#' @return A filtered data frame of rows where the column matches the regex (and type, if specified).
#' @details meta_survey stores metadata for a kobo survey. This function helps us to find the columns
#' that are relevant for a given matrix question. Can also be used to find columns
#' for questions that are thematically related.
#' If no matches are found, the function stops with an error.
#' @seealso \code{\link[stringr]{str_detect}}, \code{\link[dplyr]{filter}}
#' @export
find_qs <- function(meta_survey, col_to_regex, regex, type_filter = NULL) {
    q_df <- meta_survey %>%
        dplyr::filter(stringr::str_detect(.data[[col_to_regex]], regex))

    if (!is.null(type_filter)) {
        q_df <- q_df %>%
            dplyr::filter(.data$type == type_filter)
    }

    if (nrow(q_df) == 0) {
        stop(sprintf(
            "Did not find hits for %s in %s%s",
            regex, col_to_regex,
            if (!is.null(type_filter)) paste0(" with type = ", type_filter) else ""
        ))
    }

    return(q_df)
}


#' Find column for a question. Use for simple text or numeric questions.
#' @param meta_survey A data frame containing survey metadata.
#' @param col_to_regex A string naming the column in \code{meta_survey} to search.
#' @param regex A regular expression pattern to match within \code{col_to_regex}.
#' @param type_filter Optional. A string specifying the question type to filter by (e.g., "text").
#'        If NULL, no type filtering is applied.
#' @return A single-row data frame corresponding to the matched question.
#' @details Calls \code{find_qs()} to locate matching rows and ensures exactly one match.
#' Throws an error if none or multiple matches are found.
#' @seealso \code{\link{find_qs}}, \code{\link[stringr]{str_detect}}, \code{\link[dplyr]{filter}}
#' @export
find_q <- function(meta_survey, col_to_regex, regex, type_filter = NULL) {
    q_df <- find_qs(meta_survey, col_to_regex, regex, type_filter = type_filter)

    if (nrow(q_df) != 1) {
        stop(sprintf(
            "Did not find one column for %s in %s%s but %i",
            regex,
            col_to_regex,
            if (!is.null(type_filter)) paste0(" with type = ", type_filter) else "",
            nrow(q_df)
        ))
    }

    return(q_df)
}


#' Drop System Columns from Survey Submissions
#' @param submissions_df A data frame of survey submissions, typically from KoboToolbox or ODK exports.
#' @return A data frame with system-generated columns removed.
#' @details Removes metadata and system columns such as those starting with \code{"_"} or \code{"meta"}, as well as \code{formhub/uuid}, \code{start}, and \code{end}.
#' @export
drop_system_columns <- function(submissions_df) {
    submissions_df %>%
        select(
            -starts_with("_"),
            -starts_with("meta"),
            -`formhub/uuid`,
            -start,
            -end
        )
}

#' Expand a Multiselect / checkbox Column into Long Format
#' @param submissions_df A data frame containing a multiselect question column.
#' @param msq_col_name character. The name of the multiselect column (as character) to split into multiple rows.
#' @return A long-format data frame with one row per selected option, keeping only id column and the multiselect/checkbox variable.
#' @details Splits space-separated values in the multiselect/checkbox column using \code{tidyr::separate_rows()} and removes missing values.
#' @seealso \code{\link[tidyr]{separate_rows}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{filter}}
#' @export
make_multiselect_long <- function(submissions_df, msq_col_name) {
    submissions_df %>%
        tidyr::separate_rows(!!msq_col_name, sep = " ") %>%
        select(ends_with("_id"), !!msq_col_name) %>% # gets og_id or tn_id depending on dataset
        filter(!is.na(.data[[msq_col_name]]))
}


# RECODING CHOICES  / ANSWER OPTIONS -------
# recode values for variables that use lists for choices
#' Get Value-Label Mapping from Choices data frame
#' @param choices_df A data frame of kobotoolbox choices (answer options), typically from an XLSForm choices sheet.
#' @param choice_list_name A character string naming the choice list (cf. select-from-list-name) to extract.
#' @return A data frame with columns \code{name} and \code{label} for the specified choice list.
#' @details Filters the choices data frame for the given list name and selects only the relevant mapping columns.
#' @seealso \code{\link[dplyr]{filter}}, \code{\link[dplyr]{select}}
#' @export
get_mapping <- function(choices_df, choice_list_name) {
    if (length(choice_list_name) != 1) {
        stop("choice_list_name must be character of length 1.")
    }
    choices <- choices_df %>%
        filter(list_name == choice_list_name) %>%
        select(name, label)
    return(choices)
}

#' Recode Column Values Using a Mapping Table
#' @param df A data frame containing the column to be recoded.
#' @param mapping A data frame with columns \code{name} and \code{label} defining the old and new values.
#' @param col_name The name of the column in \code{df} to recode.
#' @return A data frame with the specified column values replaced according to the mapping table.
#' @details KoboToolbox has labels and values -> mapping comes from the choices data frame. This function joins the input data with the mapping table by \code{name} and replaces the target column with corresponding \code{label} values.
#' @seealso \code{\link[dplyr]{left_join}}
#' @export
recode_values <- function(df, mapping, col_name) {
    df["name"] <- df[[col_name]]
    df <- df %>% dplyr::left_join(mapping, by = "name")
    df[col_name] <- df$label
    df$name <- NULL # TODO: not sure whether we should actually delete the original value --> retain it? However, then we would have two columns .. maybe this function should be used in the quarto directly on demand and not during preprocessing/data wrangling
    df$label <- NULL
    return(df)
}

#' Pivot specific columns to long format
#' @param df A data frame containing the columns to pivot.
#' @param col_names A character vector of column names to pivot into long format.
#' @param values_to The name of the new column that will contain the pivoted values.
#' @return A long-format data frame with ID columns retained and \code{NA} values removed.
#' @details Selects ID columns and specified columns, reshapes them with \code{tidyr::pivot_longer()}, and filters out missing values.
#' @seealso \code{\link[tidyr]{pivot_longer}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{filter}}
#' @export
pivot_cols_long <- function(df, col_names, values_to) {
    df_long <- df %>%
        select(ends_with("_id"), all_of(col_names)) %>%
        tidyr::pivot_longer(
            cols = all_of(col_names),
            names_to = "col_name", # TODO: is this needed? drop this later?
            values_to = values_to
        ) %>%
        filter(!is.na(.data[[values_to]]))
    df_long
}

#' Find columns that correspond to the header of a \link[question matrix](https://support.kobotoolbox.org/matrix_response.html)
#' @param survey_df metadata of survey as dataframe
#' @param begin_kuid The \code{$kuid} value marking the start of a \code{begin_kobomatrix} block.
#' @return A data frame containing the rows (columns in the submissions_df) between \code{begin_kobomatrix} and \code{end_kobomatrix}.
#' @details Identifies the row where a matrix question begins, then iterates through the metadata to find the corresponding end marker. Returns all intermediate rows.
#' @seealso \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}, \code{\link[tibble]{rownames_to_column}}
#' @export
#'
find_matrix_header_qs <- function(survey_df, begin_kuid) {
    # get row index of row that starts kobomatrix
    start_i <- survey_df %>%
        tibble::rownames_to_column(var = "rownum") %>%
        mutate(rownum = as.integer(rownum)) %>%
        filter(`$kuid` == begin_kuid) %>%
        pull(rownum)

    i <- start_i + 1
    while (survey_df$type[i] != "end_kobomatrix") {
        i <- i + 1
    }
    # all questions between begin_kobomatrix and end_kobomatrix are the headers
    qs_is <- c(start_i + 1, i - 1)
    survey_df[qs_is, ]
}

# for metadata: extract label which is stored as list column but each element has only one element
flatten_label <- function(data) {
    data |>
        dplyr::mutate(
            label = label %>%
                purrr::map_chr(function(x) {
                    if (is.null(x)) {
                        x <- NA
                    }
                    stopifnot(length(x) == 1) # only one label per question
                    return(x)
                })
        )
}

# convert survey df to json for reference purposes
survey_to_json <- function(survey_df) {
    survey_df |>
        purrr::transpose() %>%
        as.list() %>%
        jsonlite::toJSON(auto_unbox = TRUE)
}

# Gets all years based on existing folders in data/cleaned
get_all_years <- function(root = here("data/cleaned")) {
    folder_names <- dir(root)
    years <- as.integer(folder_names)
    years <- years[!is.na(years)]
    sort.int(years)
}

# Gets the current and previous year based on existing folders in data/cleaned
get_current_and_previous_year <- function(root = here("data/cleaned")) {
    years = get_all_years(root)
    current_year <- max(years)
    previous_year <- current_year - 1L
    c(current_year, previous_year)
}

# Loads all relevant data for a given year
load_year <- function(y) {
    list(
        year = y,
        tn = readr::read_rds(here("data/cleaned", y, "tn.rds")),
        og = readr::read_rds(here("data/cleaned", y, "og.rds")),
        cfg = list(
            tn_cfg = readr::read_rds(here("config", y, "tn_cfg.rds")),
            og_cfg = readr::read_rds(here("config", y, "og_cfg.rds"))
        ),
        meta = list(
            aussagen = readr::read_csv(here("data/meta", y, "tn_aussagen.csv")),
            aussagen_choices = readr::read_csv(here::here("data/meta", y, "tn_aussagen_choices.csv")) %>% mutate(name = as.character(name)),
            eig = readr::read_csv(here("data/meta", y, "tn_eig.csv")),
            eig_choices = readr::read_csv(here::here("data/meta", y, "tn_eig_choices.csv")),
            og_choices = readr::read_csv(here::here("data/meta", y, "og_choices.csv")),
            tn_choices = readr::read_csv(here::here("data/meta", y, "tn_choices.csv"))
        )
    )
}

load_all_years <- function() {
    years <- get_available_years() 
    data_list <- purrr::map(years, load_year)
    names(data_list) <- years
    data_list
}
