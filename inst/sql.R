rm(list = ls())
library(flightscanner)
library(dplyr)
library(microbenchmark)

devtools::document()
pkgdown::build_site()


# initialize SQLite database
con <- dbCreateDB(dbname = "inst/flight.db")
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "inst/flight.db")
# dbDisconnect(con)
dbListTables(con)

# remove all tables
sapply(dbListTables(con), dbRemoveTable, conn = con)

# write data into database
SetAPI(host = "skyscanner-skyscanner-flight-search-v1.p.rapidapi.com",
       key = "3e85a0e43cmshac6dba6fde57066p1c1145jsn1e6f8c3d0e33")
resp.get <- CreateSession(orig = "SEA", dest = "PVG", startDate = "2019-06-01", returnDate = "2019-07-01") %>%
  PollSession(respondPOST = .)
SaveData(con, x = resp.get)

# extract data
data <- GetData(resp.get)  # from API response
data <- GetData(con)  # from SQLite database
GetData(con, lazy = T)

# check duplicate Id
sapply(data, CheckDuplicate)  # Leg: 往返时会重复. carriers: Id = 0重复

# filter flight
FilterFlight(data, max_price = 1000, max_duration = 60 * 24,
             max_stops = 2, layover = c(60, 180),
             carrier_include = c("UA", "AA", "DL", "CX", "NH", "MU", "HU"),
             carrier_exclude = c("MH", "KE"))

