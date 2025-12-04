# TEN SING Annual Survey – Data & Website

Code for the yearly TEN SING surveys and the public Quarto site that publishes the results. Two questionnaires run each autumn:

- **OG (Ortsgruppen)** – one submission per local group (headcounts, activities, staffing).
- **TN (Teilnehmende)** – individual submissions from participants and staff.

The repo contains both the data pipeline (Kobo pull, cleaning, year-specific tweaks) and the Quarto project that reads the cleaned data.

## Requirements

- R with `renv`
- Quarto CLI
- Access to the EU KoboToolbox instance
- `.env` with credentials and survey identifiers

### Environment

Create `.env` from `.env-template` and add one block per survey year:

```
KOBO_EU_TOKEN="apitoken from kobo instance"
2025_OG="asset id of OG survey"
2025_TN="asset id of TN survey"
2025_OG_URL="public link to OG questionnaire"
2025_TN_URL="public link to OG questionnaire"
```

Add further `<YEAR>_…` pairs for every year you process.

## Setup

```r
renv::restore()
```

## Running the pipeline

- `Rscript pipeline/run_pipeline.R` – runs metadata download, config generation, cleaning, optional year-specific fixes, and OG/TN merging for every year detected under `data/cleaned/<year>/`. Create a new empty `data/cleaned/<year>/` directory to include another year.
- Expected outputs: `config/<year>/og_cfg.rds`, `config/<year>/tn_cfg.rds`, `data/cleaned/<year>/og.rds`, `data/cleaned/<year>/tn.rds`, plus raw/metadata CSVs under `data/meta/<year>/` and `data/raw/<year>/`.

### Pipeline stages (per year)

1) `pipeline/setup.R` – ensures year-specific directories exist.
2) `pipeline/00-get-metadata.R` – pulls Kobo survey structure and choice lists.
3) `config/og_config.R` and `config/tn_config.R` – derive column mappings and URLs.
4) `pipeline/01-clean-og-data.R` / `02-clean-tn-data.R` – download, clean, reshape, and save `og.rds` / `tn.rds`.
5) `pipeline/03-year_specific-processing/process_<year>.R` – optional one-off fixes (deduplication, recoding).
6) `pipeline/04-tn-og-processing.R` – combines OG and TN outputs for region aggregates.

-> `pipeline/run_pipeline.R` runs the pipeline for all existing years

## Rendering the website

- **All years** (pipeline + render): `Rscript build.R`
  - Uses `get_all_years()` to loop through years found in `data/cleaned/<year>/`.
  - Renders each year with Quarto’s `build` profile into `_site/<year>/` (clearing any previous build).
- **Single year only**: `RENDER_YEAR=2025 quarto render . --profile build --output-dir _site/2025`
  - Without `RENDER_YEAR`, Quarto uses the most recent year in `data/cleaned`.
  - `quarto render .` without `--profile build` writes to `preview/` for local viewing.

## Data outputs (short)

- `og.rds`: `data` (one row per OG with regions, headcounts, hours), `long` (multiselect/matrix tables such as `wochentage`, `unterstuetzungsbedarfe`), `agg` (regional totals).
- `tn.rds`: `data` (cleaned OG names/regions, hours, participation flags), `long` (e.g., `woerter`, `zugangsweg`, `angebote_vor_ort`, `info_wege_*`, `kontakt_chrgl`), `wide` (`aussagen` Likert block), `agg` (Likert and age summaries), `demo` (scrambled vectors for safer display).

## Repository layout

- `pipeline/` – end-to-end scripts listed above.
- `config/` – metadata-driven question mappings, visual settings (`viz_config.R`), per-year cfg RDS files.
- `data/` – `meta/`, `raw/`, `cleaned/` per year; region and OG recoding CSVs.
- `R/` – shared helpers for both pipeline and Quarto (`load_libs.R`, `load_data.R`, `utils.R`, `quarto-utils.R`, `viz.R`).
- `*.qmd` – Quarto pages; `_region-template*.qmd` power the region subpages; `_quarto.yml` configures the site.

## License

See `LICENCE.md`.
