library(DBI)

con <- dbConnect(RSQLite::SQLite(), dbname = "inst/flight.db")
dbDisconnect(con)
dbListTables(con)

price <- GetPrice(resp.get)
itineraries <- GetItineraries(resp.get)
legs <- GetLegs(resp.get)
segments <- GetSegments(resp.get)

carriers <- GetCarriers(resp.get)
agents <- GetAgents(resp.get)
places <- GetPlaces(resp.get)

# remove all tables
sapply(dbListTables(con), dbRemoveTable, conn = con)

# initialize SQLite database
dbCreateDB(con, dbname = "inst/flight.db")

# write data into database
dbAppendTableNew(con, "carrier", carriers)
dbReadTable(con, "carrier") %>% head
dbAppendTableNew(con, "agent", agents)
dbReadTable(con, "agent") %>% head
dbAppendTableNew(con, "place", places)
dbReadTable(con, "place") %>% head

dbAppendTableNew(con, "segment", segments)
dbReadTable(con, "segment") %>% ListPack(mutate = T) %>% head
dbAppendTableNew(con, "leg", legs)
dbReadTable(con, "leg") %>% ListPack(mutate = T, vars = c("SegmentIds", "Stops")) %>% head
dbAppendTableNew(con, "itinerary", itineraries)
dbReadTable(con, "itinerary") %>% head
dbAppendTableNew(con, "price", price)
dbReadTable(con, "price") %>% ListPack(mutate = T, vars = "PricingOptions") %>% as.tbl %>% head







library(sqldf)
sqldf()


dcon <- src_sqlite("inst/flight.db")
a = tbl(con, from = "segment")
a
