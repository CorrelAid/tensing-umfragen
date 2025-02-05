# delete duplicates in og data
library(tidyverse)

og <- readr::read_rds("data/cleaned/og.rds")
og_data <- og$data

# we filter out NA
og_data <- og_data %>% 
  filter(!is.na(og_name))

# we filter out duplictaes by taking only the first answer for a given og name
og_data <- og_data %>% 
  group_by(og_name) %>% 
  slice(1)
  
# now only keep those who were kept by filtering in long data
og$long <- og$long %>% purrr::map(function(df) {
  df %>% 
    filter(og_id %in% og_data$og_id)
})

og$data <- og_data

og %>% readr::write_rds("data/cleaned/og.rds")
