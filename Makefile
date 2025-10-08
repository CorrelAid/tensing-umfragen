.PHONY: data


# 00-get-metadata.R is executed via source'ing of config/config_[og|tn].R in the get-[og|tn]-data.R files
data: 
	Rscript pipeline/01-get-og-data.R 
	Rscript pipeline/02-get-tn-data.R 
	Rscript pipeline/03-recoding-og.R 
	Rscript pipeline/04-delete-duplicates.R

website: index.qmd data/cleaned/og.rds data/cleaned/tn.rds
	quarto render .

preview: index.qmd data/cleaned/og.rds data/cleaned/tn.rds
	quarto preview