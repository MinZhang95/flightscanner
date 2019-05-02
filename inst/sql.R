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
DBI::dbListTables(con)

# remove all tables
sapply(DBI::dbListTables(con), DBI::dbRemoveTable, conn = con)

# write data into database
SetAPI(host = "skyscanner-skyscanner-flight-search-v1.p.rapidapi.com",
       key = "3e85a0e43cmshac6dba6fde57066p1c1145jsn1e6f8c3d0e33")
resp.get <- CreateSession(orig = "DSM", dest = "DTW",
                          startDate = "2019-06-01") %>%  # , returnDate = "2019-07-01"
  PollSession(respondPOST = .)
SaveData(con, x = resp.get)

# extract data
data <- GetData(resp.get)  # from API response
data <- GetData(con)  # from SQLite database
GetData(con, lazy = T)

# check duplicate Id
sapply(data, flightscanner:::CheckDuplicate)  # Leg: 往返时会重复. carriers: Id = 0重复

# filter flight
FilterFlight(data)
FilterFlight(data, max_price = 1000, max_duration = 60 * 24,
             max_stops = 0, layover = c(1L, 5L) * 60,
             carrier_include = c("UA", "AA", "DL", "CX", "NH", "MU", "HU"),
             carrier_exclude = c("MH", "KE"))


# DEBUG
a = FilterFlight(data) %>%
  select(-ends_with("LegSegments"), -ends_with("LegStops")) %>%
  tidyr::unnest(!!sym("PricingOptions")) %>%
  select(Price, everything(), -AgentId, -LinkURL)

FilterFlight(data)
FilterFlight(data, out_departure = c(0, 1)) %>%
  select(-ends_with("LegSegments"), -ends_with("LegStops")) %>%
  tidyr::unnest(PricingOptions)

FilterFlight(data, max_stops = 0, layover = c(1, Inf))






