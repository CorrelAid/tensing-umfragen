# TEN SING Jahresumfrage – Daten & Website

Code für die jährlichen TEN SING Umfragen sowie die öffentliche Quarto-Website, die die Ergebnisse zeigt. Jedes Jahr gibt es zwei Fragebögen:

- **OG (Ortsgruppen)** – eine Abgabe pro Ortsgruppe (Zahlen, Aktivitäten, Personal).
- **TN (Teilnehmende)** – Einzelabgaben von Teilnehmenden und Mitarbeitenden.

Das Repo enthält sowohl die Daten-Pipeline (Kobo-Abruf, Bereinigung, jahresspezifische Anpassungen) als auch das Quarto-Projekt, das die bereinigten Daten einliest.

## Anforderungen

- R mit `renv`
- Quarto CLI
- Zugang zur EU-KoboToolbox
- `.env` mit Zugangsdaten und Survey-IDs

### Umgebung

Lege `.env` anhand von `.env-template` an und ergänze pro Jahr einen Block:

```
KOBO_EU_TOKEN="apitoken from kobo instance"
2025_OG="Asset-ID OG-Fragebogen"
2025_TN="Asset-ID TN-Fragebogen"
2025_OG_URL="Öffentlicher Link zum OG-Fragebogen"
2025_TN_URL="Öffentlicher Link zum TN-Fragebogen"

```

Füge weitere `<YEAR>_…`-Variablen für jedes Jahr hinzu indem die Umfragen durchgeführt wurden.

## Setup

```r
renv::restore()
```

## Pipeline ausführen

- `Rscript pipeline/run_pipeline.R` – lädt Metadaten, erzeugt Configs, bereinigt die Daten, führt optionale Jahresskripte aus und kombiniert OG/TN. Es werden alle Jahre verarbeitet, für die ein Ordner `data/cleaned/<year>/` existiert. Für ein neues Jahr also zuerst diesen Ordner anlegen.
- Erwartete Outputs: `config/<year>/og_cfg.rds`, `config/<year>/tn_cfg.rds`, `data/cleaned/<year>/og.rds`, `data/cleaned/<year>/tn.rds` sowie Roh- und Metadaten-CSV unter `data/meta/<year>/` und `data/raw/<year>/`.

### Pipeline-Schritte (pro Jahr)

1. `pipeline/setup.R` – legt die Jahres-Verzeichnisse an.
2. `pipeline/00-get-metadata.R` – zieht Fragebogenstruktur und Antwortoptionen von Kobo.
3. `config/og_config.R` und `config/tn_config.R` – leiten Spalten-Mappings und URLs ab.
4. `pipeline/01-clean-og-data.R` / `02-clean-tn-data.R` – _config Scripts ausführen, laden, bereinigen, umformen und speichern `og.rds` / `tn.rds`.
5. `pipeline/03-year_specific-processing/process_<year>.R` – optionale Einzelanpassungen (z. B. Deduplizieren).
6. `pipeline/04-tn-og-processing.R` – kombiniert OG- und TN-Outputs für Regions-Aggregate.

-> `pipeline/run_pipeline.R` führt die Pipeline für alle vorhandenen Jahre durch

## Website rendern

- **Alle Jahre (Pipeline + Render)**: `Rscript build.R`
  - Nutzt `get_all_years()` und rendert jedes Jahr mit dem Quarto-Profile `build` nach `_site/<year>/` (vorherige Builds werden gelöscht).
- **Nur ein Jahr**: `RENDER_YEAR=2025 quarto render . --profile build --output-dir _site/2025`
  - Ohne `RENDER_YEAR` nimmt Quarto das aktuellste Jahr in `data/cleaned`.
  - `quarto render .` ohne `--profile build` schreibt nach `preview/` für die lokale Ansicht.

## Daten-Outputs

- `og.rds`: `data` (eine Zeile pro OG mit Region, Gruppierung, Zahlen), `long` (Multiselect-/Matrix-Tabellen wie `wochentage`, `unterstuetzungsbedarfe`), `agg` (Regionssummen).
- `tn.rds`: `data` (bereinigte OG-Namen/-Regionen, Stunden, Teilnahmeflags), `long` (z. B. `woerter`, `zugangsweg`, `angebote_vor_ort`, `info_wege_*`, `kontakt_chrgl`), `wide` (`aussagen`-Likert-Block), `agg` (Likert- und Alters-Summen), `demo` (durchmischte Vektoren für sicherere Darstellung).

## Projektstruktur

- `pipeline/` – End-to-End-Skripte (siehe oben).
- `config/` – Fragen-Mappings, Visual-Einstellungen (`viz_config.R`), jahresspezifische cfg-RDS.
- `data/` – `meta/`, `raw/`, `cleaned/` pro Jahr; Region- und OG-Recoding-CSV.
- `R/` – gemeinsame Helfer für Pipeline und Quarto (`load_libs.R`, `load_data.R`, `utils.R`, `quarto-utils.R`, `viz.R`).
- `*.qmd` – Quarto-Seiten; `_region-template*.qmd` für Regionsseiten; `_quarto.yml` für die Site-Konfiguration.

## Lizenz

Siehe `LICENCE.md`.
