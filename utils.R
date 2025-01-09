library(stringr)


# FIND COLUMNS / ENTRIES IN METADATA
find_qs <- function(meta_survey, col_to_regex, regex) {
    q_df <- meta_survey %>% 
        filter(str_detect(.data[[col_to_regex]], regex))
    if (nrow(q_df) == 0) {
        stop(sprintf("Did not find hits for %s in %s", regex, col_to_regex))
    }
    return(q_df)
}

find_q <- function(meta_survey, col_to_regex, regex) {
    q_df <- find_qs(meta_survey, col_to_regex, regex)
    if (nrow(q_df) != 1) {
        stop(sprintf("Did not find one column for %s in %s but %i", regex, col_to_regex, nrow(q_df)))
    }
    return(q_df)
}

# BASIC CLEANING
drop_system_columns <- function(submissions_df) {
    submissions_df %>% 
        select(-starts_with("_"), -starts_with("meta"), -`formhub/uuid`, -start, -end)
}

make_multiselect_long <- function(df, msq_col_name)  {
    og_df %>% 
        tidyr::separate_rows(!!msq_col_name, sep = " ")  %>% 
        select(ends_with("_id"), !!msq_col_name) %>% 
        filter(!is.na(.data[[msq_col_name]]))
}

# RECODING CHOICES -------
# recode values for variables that use lists for choices
get_mapping <- function(choices_df, choice_list_name) {
    if (length(choice_list_name) != 1) {
        stop("choice_list_name must be character of length 1.")
    }
    choices <- choices_df %>% 
        filter(list_name == choice_list_name) %>% 
        select(name, label)
    return(choices)
}

# actual replacement TODO improve
recode_values <- function(df, mapping, col_name) {
    df['name'] <- df[[col_name]]
    df <- df %>% dplyr::left_join(mapping, by = "name")
    df[col_name] <- df$label
    df$name <- NULL
    df$label <- NULL
    return(df)
}

# COLLECT MULTIPLE COLUMNS AND PIVOT LONG 
# filter out na values
pivot_cols_long <- function(df, col_names, values_to) {
    df_long <- df %>% 
        select(ends_with("_id"), all_of(col_names)) %>% 
        tidyr::pivot_longer(cols = all_of(col_names), names_to = "col_name", values_to = values_to) %>% 
        filter(!is.na(.data[[values_to]]))
    df_long
}

# special function for kobomatrix (nested matrix)
# finds the columns of the matrix
find_matrix_header_qs <- function(survey_df, begin_kuid) {
    # get row index of row that starts kobomatrix
    start_i <- survey_df %>% 
        tibble::rownames_to_column(var = "rownum") %>% 
        mutate(rownum = as.integer(rownum)) %>% 
        filter(`$kuid` == begin_kuid) %>% 
        pull(rownum)

    i  <- start_i + 1
    while (survey_df$type[i] != "end_kobomatrix") {
        i  <- i + 1
    }    
    # all questions between begin_kobomatrix and end_kobomatrix are the headers    
    qs_is <- c(start_i + 1, i - 1)
    survey_df[qs_is, ]
}
