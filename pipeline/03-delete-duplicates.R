#' delete duplicates in og data
#' some local groups answered twice
#' we only take the first answer (TODO: to discuss with TEN SING whether that's a valid approach)
#' TODO: alternatively, look at data quality.
library(tidyverse)

og <- readr::read_rds(file.path(DIR_CLEANED, "og.rds"))
og_data <- og$data

# we filter out NA
og_data <- og_data %>%
  filter(!is.na(og_name))

# we filter out duplictaes by taking only the first answer for a given og name
og_data <- og_data %>%
  group_by(og_name) %>%
  slice(1)

# now only keep those who were kept in the long data
og$long <- og$long %>%
  purrr::map(function(df) {
    df %>%
      filter(og_id %in% og_data$og_id)
  })

og$data <- og_data

og %>% readr::write_rds(file.path(DIR_CLEANED, "og.rds"))
