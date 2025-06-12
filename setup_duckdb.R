library(duckdb)
library(DBI)

# in memory db
con <- dbConnect(duckdb(), dbdir = ":memory:")


og <- readr::read_rds("data/cleaned/og.rds")
tn <- readr::read_rds("data/cleaned/tn.rds")

dbWriteTable(con, "TN", tn$wide$aussagen)
