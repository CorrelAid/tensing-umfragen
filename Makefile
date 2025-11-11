.PHONY: data

YEARS = 2024 2025

data:
	for year in $(YEARS); do \
		echo "=== Running pipeline for $$year ==="; \
		Rscript pipeline/setup.R $$year; \
	done

website: index.qmd data/cleaned/og.rds data/cleaned/tn.rds
	quarto render .

preview: index.qmd data/cleaned/og.rds data/cleaned/tn.rds
	quarto preview
