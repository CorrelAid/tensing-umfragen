# TEN SING Annual Survey Report

This repository contains code for the annual TEN SING survey. 

## Background
Each autumn, TEN SING Germany conducts two surveys among its members:

- a survey among its local groups. Each local group answers once and reports data on the local level (e.g. number of events, participants) (de: Ortsgruppen -> prefix "og" in this repo). 
- a survey among its members. each  member can report on their experience, preferences, wishes, etc. (de: Teilnehmende -> prefix "tn" in this repo)

The surveys are conducted using the open source tool [KoboToolbox](https://www.kobotoolbox.org/), more specifically the [EU instance](https://eu.kobotoolbox.org/accounts/login/).

**So far, the survey has only been conducted in 2024, but the second wave is being rolled out in early October 2025.**

## General setup

Package management is done with [`renv`](https://cran.r-project.org/web/packages/renv/index.html). To restore necessary packages:

```
renv::restore()
```

To interact with KoboToolbox API for data processing, you need an API token and access to the surveys. Create an account on [the EU instance](https://eu.kobotoolbox.org/accounts/login/) and contact CorrelAid to get access.

## Repository structure 
This repository contains code for: 
- data processing and data wranglig 
- and the static HTML report/website


## Data processing & data wrangling
To understand the data processing, it is important to understand that KoboToolbox uses **the [xlsform](https://xlsform.org/en/) format** which represents metadata of surveys/forms in an excel sheet. Consequences of this: 

- one question can have multiple columns
- columns have very specific naming patterns, e.g. using various levels of prefixes to group columns together (e.g. for question matrixes). This is particularly relevant for question matrixes. 
- not all columns contain actual response data, some only contain introductory text or other metadata.

### Run data processing

- execute scripts in `pipeline` in order
- or using `make` (see `Makefile`)

```
make data
```

### Developer info: Data pipeline scripts

data processing is separated in **several R scripts** which are in the folder `pipeline`. Abstractly, the pipeline does the following:

**Get data from Kobo**

- get metadata for the two surveys
- get data for local group (`og`) survey
- get data for member (`tn`) survey

The purpose and working of each script is explained in more detail in its header. 


We use the [kbtbr](https://github.com/CorrelAid/kbtbr) package for interacting with the KoboToolbox API.

**cleanup**

- recoding of open answers to "what is your local group called?" question. this was only done in 2024, from 2025 onwards this will be a single-select question.
- deletion of duplicate answers to the local group survey

The purpose and working of each script is explained in more detail in its header. 


### Developer info: Other relevant files

**configuration** related to data processing:


- `config/og_config.R`
- `config/tn_config.R`

Those files contain "configuration" to identify the acutal column(s) for each question. This is done to avoid hardcoding those column names in the data processing pipeline. 

The purpose and workings of each script is explained in more detail in its header. 

- `R/utils.R` contains functions related to data processing. 
    - relevant for `config/*`: `find_q` and `find_qs` to identify the columns
    - relevant for `pipeline` scripts `get_[tn|og]_data`: `make_multiselect_long` and `pivot_cols_long`


## Quarto website
The report is a [quarto website](https://quarto.org/docs/websites/). It contains several pages. 

Tools:
- `ggplot`: plots
- `ggiraph`: make ggplots interactive
- `reactable`: for tables

### Develop and build

Development server: 

```
quarto preview
```

Build the website -> outputs to `_site`

```
quarto render
```

or using make (see `Makefile`):

```
make website
```

### Developer info: Relevant files

- configuration of website in `_quarto.yml`.
- `R/quarto-utils.R`: utility functions (e.g. extracting question text for a question, formatting text)
- `R/viz-utils.R`: functions for recurring plots

# 2024 -> 2025 survey changes

## Both surveys
- question on local group name is now a single select (previously open text)
- better metadata labels for blocks (now "speaking" question groups)
- some minor changes to options

## Local group (`og`)
- fixed type bug where number of people was text field instead of integer/numeric entry