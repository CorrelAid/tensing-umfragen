og <- readr::read_rds(file.path(DIR_CLEANED, "og.rds"))
tn <- readr::read_rds(file.path(DIR_CLEANED, "tn.rds"))
og_regions <- readr::read_csv("data/meta/region_mapping.csv")


og_tn_only <- tn$data %>%
    filter(!og_name %in% og$data$og_name) |>
    tidyr::replace_na(list(region_grouped = "keine Angabe", og_name = "keine Angabe")) %>%
    # we use the verantwortungs question to proxy whether a person is "mitarbeitend"
    mutate(verantwortung_ja = if_else(verantwortung_janein == "Ja", TRUE, FALSE, missing = FALSE)) |>
    group_by(region_grouped, og_name, og_region) |>
    summarize(
        anzahl_ma_leitung = sum(verantwortung_ja),
        anzahl_tn = sum(!verantwortung_ja),
        anzahl_insg = n(),
    )

og_region_tn <- og$data %>%
    select(region_grouped, og_region,og_name, anzahl_insg, anzahl_tn, anzahl_ma_leitung) |>
    bind_rows(og_tn_only)

og$agg$total_tn_by_og = og_region_tn


og %>% readr::write_rds(file.path(DIR_CLEANED, "og.rds"))
