# Architecture

## What lives here

- Two annual surveys are processed: **OG (Ortsgruppen)** with one response per local group and **TN (Teilnehmende)** with individual responses.
- A small R pipeline pulls raw data from the EU KoboToolbox instance, normalizes it, and writes per-year artifacts to `data/cleaned/<year>/`.
- A Quarto website reads those artifacts and renders one edition per year into `_site/<year>/`.

## Data flow at a glance

1) **Orchestration**: `pipeline/run_pipeline.R` loops over all years detected under `data/cleaned/` (folder names that look like `^[0-9]{4}$`), assigns `YEAR` globally, and runs the steps below. To process a new year, create an empty `data/cleaned/<year>/` folder first.
2) **Setup**: `pipeline/setup.R` ensures the year-specific directories exist (`data/meta`, `data/raw`, `data/cleaned`, `config`).
3) **Metadata pull**: `pipeline/00-get-metadata.R` loads `.env`, uses `kbtbr` against `https://eu.kobotoolbox.org/`, and writes survey/choice metadata for OG and TN to `data/meta/<year>/`.
4) **Column mapping**: `config/og_config.R` and `config/tn_config.R` derive column names and choice mappings from the metadata and save them to `config/<year>/og_cfg.rds` and `config/<year>/tn_cfg.rds`. OG uses `<YEAR>_OG_URL` from the environment for questionnaire deep links; the TN URL is currently hardcoded in `tn_config.R`.
5) **Cleaning – OG**: `pipeline/01-clean-og-data.R` downloads OG submissions, drops system columns, reshapes checkboxes/matrices, recodes answers, attaches grouped regions from `data/meta/region_mapping.csv`, and saves `data/cleaned/<year>/og.rds`.
6) **Cleaning – TN**: `pipeline/02-clean-tn-data.R` downloads TN submissions, reshapes multiselect/matrix answers, builds aggregates (Likert summaries, age by OG), applies OG recoding from `data/meta/<year>/og_recoding.csv`, scrambles demographic vectors for safer display, and saves `data/cleaned/<year>/tn.rds`.
7) **Optional year tweaks**: `pipeline/03-year_specific-processing/process_<year>.R` contains ad-hoc fixes (e.g., deduplicating OG responses, recoding OG names).
8) **Joint aggregation**: `pipeline/04-tn-og-processing.R` merges OG/TN outputs to produce region-level totals and writes back to `og.rds`.

## Data shapes

- `data/cleaned/<year>/og.rds` (list)
  - `data`: one row per OG with region, grouped region, counts, names, hours, and TN recruitment flags.
  - `long`: long-format tables for multiselect/matrix questions (e.g., `wochentage`, `gender_by_participant_type`, `tn_gewinnung_massnahmen`, `unterstuetzungsbedarfe`).
  - `agg`: derived aggregates such as `total_tn_by_og` from the TN join step.
- `data/cleaned/<year>/tn.rds` (list)
  - `data`: one row per TN response with cleaned OG names/regions, hours, participation flags, and recommendation scores.
  - `long`: long tables like `woerter`, `zugangsweg`, `angebote_vor_ort`, `verantwortung_hilfsangebote`, `info_wege_*`, `kontakt_chrgl`.
  - `wide`: currently `aussagen` (Likert block) for direct use in plots.
  - `agg`: summaries such as Likert averages and age per OG.
  - `demo`: sampled vectors (gender, age, school, faith-contact) to reduce re-identification risk when rendering widgets.

## Quarto rendering model

- Pages source `R/load_libs.R` (packages, palettes, helpers) and `R/load_data.R` (loads current and previous year's data via `get_current_and_previous_year()` from `R/utils.R`).
- The render year defaults to the highest folder in `data/cleaned`. Override with `RENDER_YEAR=<year>` when running Quarto.
- `_quarto.yml` sets the site structure. For preview it writes to `preview/`; `build.R` uses the `build` profile (`_quarto-build.yml`) so per-year editions render directly to `_site/<year>`.
- Templates: region pages include `_region-template.qmd` (with a Westbund variant for grouped regions), while top-level content lives in `index.qmd`, `ueberblick.qmd`, `themen.qmd`, `wirkung-pg.qmd`, `trends.qmd`, and `hilfe.qmd`.

## Supporting modules and data

- `R/utils.R`: question-finder helpers, recoding utilities, year detection, and convenience loaders.
- `R/quarto-utils.R`: formatting helpers, tabset renderer, callout generator, widget dependency registration.
- `R/viz.R`: shared ggplot/ggiraph chart builders and fallback “no data” plots.
- `config/viz_config.R`: TEN SING palettes, fonts, and the base theme.
- `data/meta/region_mapping.csv`: maps raw OG regions to grouped regions for aggregation.
- `data/meta/og_recoding.csv`: harmonizes OG name spellings and regions for TN data.
- `logos/`: assets included as resources during rendering.

## Adding a new survey year

1) Create `data/cleaned/<year>/` (empty) so `run_pipeline.R` can detect the year.
2) Add env vars in `.env`: `KOBO_EU_TOKEN`, `<YEAR>_OG`, `<YEAR>_TN`, and `<YEAR>_OG_URL` (plus update the TN URL in `config/tn_config.R` if it changes).
3) If this year's data needs any additional fixes, add a script  `process_<YEAR>.R `  in `03-year-süecific-processing`. If OG names need to be harmonized, add a mapping table to `data/meta/<YEAR>/og_recoding.csv`
4) Run `pipeline/run_pipeline.R` (or `build.R` to run the pipeline and render). Verify `config/<year>/` and `data/cleaned/<year>/` contain the new artifacts.
5) Render the site: `quarto render .` for a single year (uses `RENDER_YEAR` if set) or `Rscript build.R` to render every available year into `_site/<year>/`.

## Deployment notes

- Built sites live in `_site/<year>/`. The `preview/` folder is only for `quarto preview`.
- `.github/workflows/static.yml` is currently commented out; static hosting would upload `_site/` to GitHub Pages.
