# Quarto Dev container demo

## Setup

```
renv::restore()
```

## Develop

```
quarto preview
```

## Build

```
quarto render
```


# Anpassungen an Kobo Umfrage
## Ortstgruppen

Altermatrix -> integer / numeric entry

# issues
find_q in config.R auslagern 
make mapping recoding safe against _ __ issues -> align beforehand to only have single _ in value 
do not recode values in long format but add label as additional column. 

get_mapping can take question -> we always use select_from_list_name???

find_q multiple conditions, named vector / list of str_detect matches, mutliple matches str detect, construct in function

anonymity of age
funtion shuffle vector

# Fragen an TenSING

TODO: how to deal with NA in og_name? and TODO: How to deal with duplicate entries for one OG?

one table to show all the data for ortsgruppen??? -> aggreagte long answers again :eyes:

# next steps 

- CI/CD
- Datenschutz
- Farben
