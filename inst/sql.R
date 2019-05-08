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
sapply(dbListTables(con), DBI::dbRemoveTable, conn = con)

# write data into database
apiSetKey("3e85a0e43cmshac6dba6fde57066p1c1145jsn1e6f8c3d0e33")
resp.get <- apiCreateSession(orig = "DSM", dest = "DTW", startDate = "2019-06-01",
                             # returnDate = "2019-07-01"
                             ) %>% apiPollSession()

# extract data
data <- flightGet(resp.get)  # from API response
data <- flightGet(con)  # from SQLite database

# save data
dbSaveData(resp.get, con)  # from API response
dbSaveData(data, con)  # from list


# check duplicate Id
# Leg: could be replicated for round trip.
# Carriers: Id = 0 will be replicated.
mapply(flightscanner:::CheckDuplicateRow, data,
       rep(list(c("OutboundLegId", "InboundLegId"), "Id"), c(2, 5)))

# filter flight
flightFilter(data)
flightFilter(data, max_price = 1000, max_duration = 60 * 24,
             max_stops = 2, layover = c(1L, 5L) * 60,
             carrier_include = c("UA", "AA", "DL", "CX", "NH", "MU", "HU"),
             carrier_exclude = c("MH", "KE"))


# DEBUG
a = flightFilter(data) %>%
  select(-ends_with("LegSegments"), -ends_with("LegStops")) %>%
  tidyr::unnest(!!sym("PricingOptions")) %>%
  select(Price, everything(), -AgentId, -LinkURL)

flightFilter(data)
flightFilter(data, out_departure = c(0, 1)) %>%
  select(-ends_with("LegSegments"), -ends_with("LegStops")) %>%
  tidyr::unnest(PricingOptions)

flightFilter(data, max_stops = 0, layover = c(1, Inf))
