.PHONY: data


data: 
	Rscript pipeline/01-get-og-data.R 
	Rscript pipeline/02-get_tn_data.R 
	Rscript pipeline/03-recoding-og.R 
	Rscript pipeline/04-delete-duplicates.R

website: index.qmd data/cleaned/og.rds data/cleaned/tn.rds
	quarto render .

preview: index.qmd data/cleaned/og.rds data/cleaned/tn.rds
	quarto preview