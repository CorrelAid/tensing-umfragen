years <- get_current_and_previous_year()
current_year <- years[["current_year"]]
previous_year <- years[["previous_year"]]
has_previous_year <- !is.na(previous_year)

curr <- load_year(current_year)
prev <- if (has_previous_year) load_year(previous_year) else NULL
